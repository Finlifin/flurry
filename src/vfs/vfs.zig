const std = @import("std");
const lex = @import("../lex/lexer.zig");
const ast = @import("../parse/ast.zig");
const parse = @import("../parse/parser.zig");
const Err = @import("../common.zig").Err;

// a global span
pub const Span = struct {
    src: u32,
    lspan: LSpan,
};

// a file local span
pub const LSpan = struct {
    from: u32,
    to: u32,
};

// 定义 VFS 节点类型（文件或目录）
pub const NodeKind = enum {
    file,
    directory,
};

// VFS 节点结构
pub const Node = struct {
    src_id: u32,
    kind: NodeKind,
    name: []const u8, // 文件或目录名
    parent: ?*Node, // 指向父节点的指针（可选，方便向上遍历）
    children: std.ArrayList(*Node), // 子节点列表（仅目录有）

    // 渐进式加载内容
    content: ?[]const u8 = null,
    tokens: ?std.ArrayList(lex.Token) = null,
    ast: ?ast.Ast = null,

    // 创建新节点
    pub fn init(
        allocator: std.mem.Allocator,
        src_id: u32,
        kind: NodeKind,
        name: []const u8,
        parent: ?*Node,
    ) !*Node {
        const self = try allocator.create(Node);
        self.* = .{
            .src_id = src_id,
            .kind = kind,
            .name = try allocator.dupe(u8, name), //复制字符串
            .parent = parent,
            .children = std.ArrayList(*Node).init(allocator),
        };
        return self;
    }

    // 释放节点及其子节点
    pub fn deinit(self: *Node, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        if (self.kind == .directory) {
            for (self.children.items) |child| {
                child.deinit(allocator);
            }
            self.children.deinit();
        }
        if (self.content) |content|
            allocator.free(content);
        if (self.tokens) |tokens|
            tokens.deinit();
        if (self.ast) |*ast_|
            ast_.deinit();
        allocator.destroy(self);
    }

    // 添加子节点（仅用于目录）
    pub fn addChild(self: *Node, child: *Node) !void {
        if (self.kind != .directory) {
            return error.NotADirectory;
        }
        try self.children.append(child);
    }

    pub fn fullPath(self: *Node, allocator: std.mem.Allocator) ![]const u8 {
        var names = std.ArrayList([]const u8).init(allocator);
        defer names.deinit();
        var path = std.ArrayList(u8).init(allocator);

        var current_node: ?*Node = self;
        while (current_node) |node| {
            try names.append(node.name);
            current_node = node.parent;
        }

        var len = names.items.len - 1;
        while (len >= 0) {
            const name = names.items[len];
            try path.appendSlice(name);
            if (len != 0)
                try path.append('/');

            if (len == 0)
                break;

            len -= 1;
        }
        return path.toOwnedSlice();
    }

    // 读取文件内容
    pub fn loadModule(self: *Node, allocator: std.mem.Allocator) ![]const u8 {
        // dir nodes are not implemented
        std.debug.assert(self.kind == .file);

        if (self.content) |content| {
            return content;
        }

        const path = try self.fullPath(allocator);
        defer allocator.free(path);
        const file = try std.fs.openFileAbsolute(path, .{ .mode = .read_only });
        defer file.close();
        self.content = try file.readToEndAlloc(allocator, 1024 * 1024 * 1024 * 4);
        return self.content.?;
    }

    pub fn lexModule(self: *Node, allocator: std.mem.Allocator) !void {
        // dir nodes are not implemented
        std.debug.assert(self.kind == .file);

        if (self.tokens != null)
            return error.FileAlreadyLexed;

        const content = try self.loadModule(allocator);

        self.tokens = try lex.lex(allocator, content);
    }

    pub fn parseModule(self: *Node, allocator: std.mem.Allocator) !void {
        // dir nodes are not implemented
        std.debug.assert(self.kind == .file);

        if (self.ast != null)
            return error.FileAlreadyParsed;

        var parser = try parse.Parser.init(allocator, self, .{});
        try parser.parse();
    }

    pub fn srcContentT(self: *Node, token: lex.Token, allocator: std.mem.Allocator) ![]const u8 {
        const content = try self.loadModule(allocator);
        if (token.from > token.to or token.to > content.len) {
            return error.InvalidToken;
        }

        return content[token.from..token.to];
    }

    pub fn dumpTokens(self: *Node, allocator: std.mem.Allocator) !void {
        if (self.tokens) |tokens| {
            for (tokens.items) |token| {
                std.debug.print("| {s}:\t\t{s}\n", .{
                    @tagName(token.tag),
                    try self.srcContentT(token, allocator),
                });
            }
        }
    }

    pub fn dumpAst(self: *Node) !void {
        const dump_file = try std.fs.cwd().createFile("dump.lisp", .{ .read = true, .truncate = true });
        defer dump_file.close();

        if (self.ast) |ast_|
            try ast_.dump(ast_.root, dump_file.writer());
    }
};

pub const VfsOption = struct {
    mode: Mode = .debug,

    const Mode = enum {
        debug,
        release,
    };
};

// 完整的 VFS 结构
pub const Vfs = struct {
    allocator: std.mem.Allocator,
    root: *Node, // 根节点 (src 目录)
    next_src_id: u32, // 用于生成下一个 src_id
    absolute_path: []const u8, // 绝对路径
    ignore_list: std.StringHashMap(void),
    options: VfsOption,

    // 创建 VFS
    pub fn init(allocator: std.mem.Allocator, opt: VfsOption) !Vfs {
        var self = Vfs{
            .allocator = allocator,
            .absolute_path = try std.fs.cwd().realpathAlloc(allocator, "."),
            .root = undefined,
            .next_src_id = 1, // 从 1 开始
            .ignore_list = std.StringHashMap(void).init(allocator),
            .options = opt,
        };

        self.addIgnore(".");
        self.addIgnore("..");
        self.addIgnore(".git");
        self.addIgnore(".gitignore");
        self.addIgnore("README.md");
        self.addIgnore("LICENSE");
        self.addIgnore(".zig-cache");
        self.addIgnore("zig-out");

        return self;
    }

    // 释放VFS
    pub fn deinit(self: *Vfs) void {
        self.allocator.free(self.absolute_path);
        self.root.deinit(self.allocator);
        self.ignore_list.deinit();
    }

    pub fn addIgnore(self: *Vfs, path: []const u8) void {
        self.ignore_list.put(path, undefined) catch {
            return;
        };
    }

    pub fn isIgnored(self: *Vfs, path: []const u8) bool {
        return self.ignore_list.get(path) != null;
    }

    // 创建文件节点
    pub fn createFile(self: *Vfs, parent: *Node, name: []const u8) !*Node {
        if (parent.kind != .directory) {
            return error.NotADirectory;
        }
        const file = try Node.init(self.allocator, self.next_src_id, .file, name, parent);
        self.next_src_id += 1;
        try parent.addChild(file);
        return file;
    }

    // 创建目录节点
    pub fn createDirectory(self: *Vfs, parent: *Node, name: []const u8) !*Node {
        if (parent.kind != .directory) {
            return error.NotADirectory;
        }
        const dir = try Node.init(self.allocator, self.next_src_id, .directory, name, parent);
        self.next_src_id += 1;
        try parent.addChild(dir);
        return dir;
    }

    // 根据 src_id 查找节点 (递归)
    pub fn findNodeById(self: *Vfs, src_id: u32) ?*Node {
        return findNodeByIdRecursive(self.root, src_id);
    }

    fn findNodeByIdRecursive(node: *Node, src_id: u32) ?*Node {
        if (node.src_id == src_id) {
            return node;
        }
        if (node.kind == .directory) {
            for (node.children.items) |child| {
                if (child.src_id == src_id) {
                    return child;
                }
                if (child.kind == .directory) {
                    if (findNodeByIdRecursive(child, src_id)) |found_node| {
                        return found_node;
                    }
                }
            }
        }
        return null;
    }

    // 从磁盘构建 VFS
    pub fn buildVfsFromDisk(self: *Vfs) !void {
        // create root node
        self.root = try Node.init(self.allocator, self.next_src_id, .directory, self.absolute_path, null);
        self.next_src_id += 1;
        try self.buildVfsFromDiskRecursive(self.root, self.absolute_path);
    }

    fn buildVfsFromDiskRecursive(self: *Vfs, parent_node: *Node, path: []const u8) !void {
        // 打开目录
        var dir = try std.fs.openDirAbsolute(path, .{ .iterate = true });
        defer dir.close();

        // 遍历目录条目
        // refactor it with Dir.walk?
        var dir_iterator = dir.iterate();
        while (try dir_iterator.next()) |entry| {
            if (self.isIgnored(entry.name)) {
                continue; // 忽略被忽略的条目
            }
            if (self.options.mode == .debug)
                std.debug.print("DEBUG: entry {s} at {s}\n", .{ entry.name, path });

            // 构建条目的完整路径
            const entry_disk_path = try std.fs.path.join(self.allocator, &.{ path, entry.name });
            defer self.allocator.free(entry_disk_path);
            if (entry.kind == .file) {
                // 创建文件节点
                _ = try self.createFile(parent_node, entry.name);
            } else if (entry.kind == .directory) {
                // 创建目录节点
                const new_dir_node = try self.createDirectory(parent_node, entry.name);
                try self.buildVfsFromDiskRecursive(new_dir_node, entry_disk_path);
            }
        }
    }

    pub fn fullPath(self: *Vfs, src_id: u32) ?[]const u8 {
        const node = try self.findNodeById(src_id);
        var path = std.ArrayList(u8).init(self.allocator);
        while (node) |n| {
            try path.appendSlice(n.name);
            node = n.parent;
        }
        return path.toOwnedSlice();
    }

    // 根据路径查找节点
    pub fn findNodeByPath(self: *Vfs, path: []const u8) ?*Node {
        var current_node = self.root;

        // 使用 std.mem.tokenize 来分割路径
        var path_parts = std.mem.tokenize(u8, path, "/");

        while (path_parts.next()) |part| {
            // 检查当前节点是否为目录
            if (current_node.kind != .directory) {
                return null; // 如果当前节点不是目录，则路径无效
            }

            var found: bool = false;
            // 在当前目录的子节点中查找
            for (current_node.children.items) |child| {
                if (std.mem.eql(u8, child.name, part)) {
                    current_node = child; // 找到匹配的子节点，更新 current_node
                    found = true;
                    break;
                }
            }

            // 如果在当前目录中未找到匹配的子节点，则路径无效
            if (!found) {
                return null;
            }
        }

        // 循环结束后，current_node 指向路径的最后一个节点
        return current_node;
    }
};
