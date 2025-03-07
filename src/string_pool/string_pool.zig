const std = @import("std");

pub const StringPool = struct {
    gpa: std.mem.Allocator,
    root: ?*Node,
    strings: std.ArrayList([]const u8),

    pub fn init(gpa: std.mem.Allocator) StringPool {
        var result: StringPool = .{
            .gpa = gpa,
            .root = null,
            .strings = std.ArrayList([]const u8).init(gpa),
        };

        _ = result.put("invalid") catch unreachable;
        return result;
    }

    pub fn put(self: *StringPool, str: []const u8) !usize {
        const len = str.len;
        if (self.root) |root| {
            // Search for existing node
            if (root.search(str)) |node| {
                return node.value;
            }

            // Insert new node
            var parent: ?*Node = null;
            var current: ?*Node = root;
            var order: std.math.Order = undefined;
            while (current) |node| {
                parent = node;
                const node_str = node.strOf();
                order = std.mem.order(u8, node_str, str);
                current = switch (order) {
                    .lt => node.left,
                    .gt => node.right,
                    .eq => unreachable, // Already checked above
                };
            }

            const neo = try Node.new(self.gpa, len);
            neo.value = self.strings.items.len;
            @memcpy(Node.strOf(neo), str);
            try self.strings.append(Node.strOf(neo));
            neo.parent = parent;

            if (order == .lt) {
                parent.?.left = neo;
            } else {
                parent.?.right = neo;
            }

            // Fixup红黑树属性
            Node.insertFixup(neo, &self.root);
            return neo.value;
        } else {
            // 树为空，直接创建新节点作为根
            const neo = try Node.new(self.gpa, len);
            neo.value = self.strings.items.len;
            @memcpy(Node.strOf(neo), str);
            try self.strings.append(Node.strOf(neo));
            neo.is_red = false; // 根节点必须是黑色
            self.root = neo;
            return neo.value;
        }
    }

    pub fn putMany(self: *StringPool, strs: anytype) !usize {
        var total_len: usize = 0;
        inline for (strs) |s| {
            const str = @as([]const u8, s);
            total_len += str.len;
        }

        const tmp = try self.gpa.alloc(u8, total_len);
        defer self.gpa.free(tmp);
        var offset: usize = 0;
        inline for (strs) |s| {
            const str = @as([]const u8, s);
            std.mem.copyForwards(u8, tmp[offset..], str);
            offset += str.len;
        }

        return self.put(tmp[0..]);
    }

    pub fn get(self: *StringPool, idx: usize) ?[]const u8 {
        if (idx >= self.strings.items.len) {
            return null;
        }
        return self.strings.items[idx];
    }

    pub fn contains(self: *StringPool, str: []const u8) bool {
        return self.root != null and self.root.?.search(str) != null;
    }

    pub fn deinit(self: *StringPool) void {
        for (self.strings.items) |s| {
            const base = s.ptr - @sizeOf(Node);
            self.gpa.free(base[0 .. s.len + @sizeOf(Node)]);
        }
        self.strings.deinit();
    }
    pub const Index = usize;
};

const Node = struct {
    is_red: bool,
    len: u56,
    left: ?*Node,
    right: ?*Node,
    parent: ?*Node,
    value: usize,

    pub fn new(gpa: std.mem.Allocator, len: usize) !*Node {
        // const mem = try gpa.alignedAlloc(u8, @alignOf(Node), @sizeOf(Node) + len);
        const mem = try gpa.alloc(u8, @sizeOf(Node) + len);
        const node = @as(*Node, @ptrCast(@alignCast(mem)));
        node.is_red = true;
        node.parent = null;
        node.left = null;
        node.right = null;
        node.len = @intCast(len);
        return node;
    }

    pub fn strOf(node: *Node) []u8 {
        return @as([*]u8, @ptrCast(@alignCast(node)))[@sizeOf(Node)..][0..node.len];
    }

    pub fn search(node: *Node, str: []const u8) ?*Node {
        var current: ?*Node = node;
        while (current) |n| {
            const node_str = n.strOf();
            switch (std.mem.order(u8, node_str, str)) {
                .lt => current = n.left,
                .gt => current = n.right,
                .eq => return n,
            }
        }
        return null;
    }

    pub fn insertFixup(neo: *Node, root: *?*Node) void {
        var current = neo;
        while (true) {
            const parent = current.parent orelse break;
            if (!parent.is_red) break;

            const gparent = parent.parent orelse break;
            const uncle = if (gparent.left == parent) gparent.right else gparent.left;

            if (isRed(uncle)) {
                // Case 1: Uncle is red
                setBlack(parent);
                setBlack(uncle);
                setRed(gparent);
                current = gparent;
            } else {
                // Case 2 & 3: Uncle is black
                if (parent == gparent.left) {
                    if (current == parent.right) {
                        // Case 2: Left-Right
                        rotateLeft(parent, root);
                        current = parent;
                    }
                    // Case 3: Left-Left
                    setBlack(parent);
                    setRed(gparent);
                    rotateRight(gparent, root);
                } else {
                    if (current == parent.left) {
                        // Case 2: Right-Left
                        rotateRight(parent, root);
                        current = parent;
                    }
                    // Case 3: Right-Right
                    setBlack(parent);
                    setRed(gparent);
                    rotateLeft(gparent, root);
                }
                break;
            }
        }
        // Ensure root is black
        if (current.parent == null) {
            setBlack(current);
            root.* = current;
        }
    }

    fn rotateLeft(node: *Node, root: *?*Node) void {
        const right_child = node.right orelse return;
        const parent = node.parent;

        node.right = right_child.left;
        if (right_child.left) |left| {
            left.parent = node;
        }

        right_child.left = node;
        node.parent = right_child;

        right_child.parent = parent;

        if (parent) |p| {
            if (p.left == node) {
                p.left = right_child;
            } else {
                p.right = right_child;
            }
        } else {
            root.* = right_child;
        }
    }

    fn rotateRight(node: *Node, root: *?*Node) void {
        const left_child = node.left orelse return;
        const parent = node.parent;

        node.left = left_child.right;
        if (left_child.right) |right| {
            right.parent = node;
        }

        left_child.right = node;
        node.parent = left_child;

        left_child.parent = parent;

        if (parent) |p| {
            if (p.left == node) {
                p.left = left_child;
            } else {
                p.right = left_child;
            }
        } else {
            root.* = left_child;
        }
    }

    inline fn isRed(node: ?*Node) bool {
        return if (node) |n| n.is_red else false;
    }

    inline fn setBlack(node: ?*Node) void {
        if (node) |n| {
            n.is_red = false;
        }
    }

    inline fn setRed(node: ?*Node) void {
        if (node) |n| {
            n.is_red = true;
        }
    }
};

test "basic usage" {
    var pool = StringPool.init(std.testing.allocator);
    defer pool.deinit();

    const str1 = try pool.put("hello");
    const str2 = try pool.put(" world");

    const str3 = try pool.put("hello world");
    std.debug.assert(std.mem.eql(u8, pool.get(str3).?, "hello world"));
    const str4 = try pool.putMany(.{ pool.get(str1).?, pool.get(str2).? });
    std.debug.assert(str3 == str4);
}
