const std = @import("std");
const node_module = @import("node.zig");
const Err = @import("../common.zig").Err;

// Import the node struct and related definitions
pub const Span = node_module.Span;
pub const LSpan = node_module.LSpan;
pub const NodeKind = node_module.NodeKind;
pub const Node = node_module.Node;

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
