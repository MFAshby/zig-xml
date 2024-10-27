const std = @import("std");
const assert = std.debug.assert;

// hard-coded prefixes list
// TODO generate prefixes using an infinite sequence to avoid having a hard limit on the available namespace prefixes.
const prefixes_list = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

options: Options,

state: State,
indent_level: u32,

sink: Sink,

aa: *std.heap.ArenaAllocator,
a: std.mem.Allocator,

// Namespaces. See https://www.w3.org/TR/xml-names/
// elementStartNs and elementEndNs can be used to add namespaced elements to XML documents.
// The Writer will automatically handle prefixes.

// The Writer needs to retain some state for the namespaces in scope.
// We'll use a stack as is natural fit for a recursive data structure like XML
// Stack entry contains a list of tuples containing the currently-in-scope namespace/prefix mappings.
namespaceStack: std.ArrayListUnmanaged(std.ArrayListUnmanaged(NsStateEntry)),
// index into prefixes_list for namespace prefixes we've assigned
usedPfix: usize,

const NsStateEntry = struct {
    prefix: []const u8,
    namespace: []const u8,
};

const Writer = @This();

pub const Options = struct {
    indent: []const u8 = "",
};

pub const Sink = struct {
    context: *const anyopaque,
    writeFn: *const fn (context: *const anyopaque, data: []const u8) anyerror!void,

    pub fn write(sink: *Sink, data: []const u8) anyerror!void {
        return sink.writeFn(sink.context, data);
    }
};

const State = enum {
    start,
    after_bom,
    after_xml_declaration,
    element_start,
    after_structure_end,
    text,
    end,
};

pub fn init(a: std.mem.Allocator, sink: Sink, options: Options) !Writer {
    var res: Writer = undefined;
    // Arena allocator saves worrying too much about cleanup of complex data structure.
    res.aa = try a.create(std.heap.ArenaAllocator);
    res.aa.* = std.heap.ArenaAllocator.init(a);
    res.a = res.aa.allocator();
    res.options = options;
    res.state = .start;
    res.indent_level = 0;
    res.sink = sink;
    res.namespaceStack = .{};
    res.usedPfix = 0;
    return res;
}

pub fn deinit(self: *Writer) void {
    const allocator = self.aa.child_allocator;
    self.aa.deinit();
    allocator.destroy(self.aa);
}

pub const WriteError = error{};

pub fn bom(writer: *Writer) anyerror!void {
    assert(writer.state == .start);
    try writer.raw("\u{FEFF}");
    writer.state = .after_bom;
}

pub fn xmlDeclaration(writer: *Writer, encoding: ?[]const u8, standalone: ?bool) anyerror!void {
    assert(writer.state == .start or writer.state == .after_bom);
    try writer.raw("<?xml version=\"1.0\"");
    if (encoding) |e| {
        try writer.raw(" encoding=\"");
        try writer.attributeText(e);
        try writer.raw("\"");
    }
    if (standalone) |s| {
        if (s) {
            try writer.raw(" standalone=\"yes\"");
        } else {
            try writer.raw(" standalone=\"no\"");
        }
    }
    try writer.raw("?>");
    if (writer.options.indent.len > 0) try writer.newLineAndIndent();
    writer.state = .after_xml_declaration;
}

pub fn elementStart(writer: *Writer, name: []const u8) anyerror!void {
    return elementStartNs(writer, name, null, .{});
}

pub const ElementStartNsOpts = struct {
    defaultNs: ?[]const u8 = null,
    prefixedNs: ?[]const []const u8 = null,
};

// 'opts' allows an element to introduce new default or prefixed namespaces
pub fn elementStartNs(writer: *Writer, name: []const u8, namespace: ?[]const u8, opts: ElementStartNsOpts) anyerror!void {
    switch (writer.state) {
        .start, .after_bom, .after_xml_declaration, .text => {},
        .element_start => {
            try writer.raw(">");
            try writer.newLineAndIndent();
        },
        .after_structure_end => {
            try writer.newLineAndIndent();
        },
        .end => unreachable,
    }

    var attributesToAdd: std.ArrayListUnmanaged([2][]const u8) = .{};
    var newState: std.ArrayListUnmanaged(NsStateEntry) = .{};
    if (writer.namespaceStack.getLastOrNull()) |stackTop| {
        newState = try stackTop.clone(writer.a); // everything from parent element remains in scope by default
    }
    if (opts.defaultNs) |newDefaultNs| {
        for (newState.items, 0..) |*entry, i| {
            if (entry.prefix.len == 0) {
                _ = newState.swapRemove(i);
                break;
            }
        }
        try newState.append(writer.a, .{ .namespace = newDefaultNs, .prefix = "" });
        try attributesToAdd.append(writer.a, [_][]const u8{ "xmlns", newDefaultNs });
    }
    if (opts.prefixedNs) |prefixedNses| {
        lpns: for (prefixedNses) |pfns| {
            for (newState.items) |entry| {
                if (std.mem.eql(u8, entry.namespace, pfns)) {
                    continue :lpns; // Skip it, it's already in our state
                }
            }
            const prefix = prefixes_list[writer.usedPfix .. writer.usedPfix + 1];
            writer.usedPfix += 1;
            try newState.append(writer.a, .{ .namespace = pfns, .prefix = prefix });
            const key = try std.fmt.allocPrint(writer.a, "xmlns:{s}", .{prefix});
            try attributesToAdd.append(writer.a, [_][]const u8{ key, pfns });
        }
    }
    try writer.namespaceStack.append(writer.a, newState);

    try writer.raw("<");
    try writer.writeNs(namespace);
    try writer.raw(name);

    writer.state = .element_start;
    writer.indent_level += 1;

    for (attributesToAdd.items) |attr| {
        try writer.doAddAttribute(attr[0], attr[1]);
    }
}

fn writeNs(writer: *Writer, namespace: ?[]const u8) anyerror!void {
    if (namespace) |ns| {
        const nsState = writer.namespaceStack.getLast();
        for (nsState.items) |nsEntry| {
            if (std.mem.eql(u8, nsEntry.namespace, ns)) {
                if (nsEntry.prefix.len > 0) {
                    try writer.raw(nsEntry.prefix);
                    try writer.raw(":");
                }
                return;
            }
        } else {
            return error.InvalidNamespace;
        }
    }
}

pub fn elementEnd(writer: *Writer, name: []const u8) anyerror!void {
    return elementEndNs(writer, name, null);
}
pub fn elementEndNs(writer: *Writer, name: []const u8, namespace: ?[]const u8) anyerror!void {
    writer.indent_level -= 1;
    switch (writer.state) {
        .text => {},
        .element_start => {
            try writer.raw(">");
            try writer.newLineAndIndent();
        },
        .after_structure_end => {
            try writer.newLineAndIndent();
        },
        .start, .after_bom, .after_xml_declaration, .end => unreachable,
    }

    try writer.raw("</");
    try writer.writeNs(namespace);
    try writer.raw(name);
    try writer.raw(">");
    _ = writer.namespaceStack.pop();
    writer.state = if (writer.indent_level > 0) .after_structure_end else .end;
}

pub fn elementEndEmpty(writer: *Writer) anyerror!void {
    assert(writer.state == .element_start);
    try writer.raw("/>");
    writer.state = .after_structure_end;
    writer.indent_level -= 1;
}

pub fn attribute(writer: *Writer, name: []const u8, value: []const u8) anyerror!void {
    if (std.mem.eql(u8, name, "xmlns") or std.mem.startsWith(u8, name, "xmlns:")) {
        // Use elementStartNs instead.
        return error.AttributeError;
    }
    return doAddAttribute(writer, name, value);
}

fn doAddAttribute(writer: *Writer, name: []const u8, value: []const u8) anyerror!void {
    assert(writer.state == .element_start);
    try writer.raw(" ");
    try writer.raw(name);
    try writer.raw("=\"");
    try writer.attributeText(value);
    try writer.raw("\"");
}

fn attributeText(writer: *Writer, s: []const u8) anyerror!void {
    var pos: usize = 0;
    while (std.mem.indexOfAnyPos(u8, s, pos, "\r\n\t&<\"")) |esc_pos| {
        try writer.raw(s[pos..esc_pos]);
        try writer.raw(switch (s[esc_pos]) {
            '\r' => "&#xD;",
            '\n' => "&#xA;",
            '\t' => "&#x9;",
            '&' => "&amp;",
            '<' => "&lt;",
            '"' => "&quot;",
            else => unreachable,
        });
        pos = esc_pos + 1;
    }
    try writer.raw(s[pos..]);
}

pub fn pi(writer: *Writer, target: []const u8, data: []const u8) anyerror!void {
    switch (writer.state) {
        .start, .after_bom, .after_xml_declaration, .text, .end => {},
        .element_start => {
            try writer.raw(">");
            try writer.newLineAndIndent();
        },
        .after_structure_end => {
            try writer.newLineAndIndent();
        },
    }
    try writer.raw("<?");
    try writer.raw(target);
    try writer.raw(" ");
    try writer.raw(data);
    try writer.raw("?>");
    writer.state = .after_structure_end;
}

pub fn text(writer: *Writer, s: []const u8) anyerror!void {
    switch (writer.state) {
        .after_structure_end, .text => {},
        .element_start => try writer.raw(">"),
        .start, .after_bom, .after_xml_declaration, .end => unreachable,
    }
    var pos: usize = 0;
    while (std.mem.indexOfAnyPos(u8, s, pos, "\r&<")) |esc_pos| {
        try writer.raw(s[pos..esc_pos]);
        try writer.raw(switch (s[esc_pos]) {
            '\r' => "&#xD;",
            '&' => "&amp;",
            '<' => "&lt;",
            else => unreachable,
        });
        pos = esc_pos + 1;
    }
    try writer.raw(s[pos..]);
    writer.state = .text;
}

// insert some existing XML document without escaping anything
pub fn embed(writer: *Writer, s: []const u8) anyerror!void {
    switch (writer.state) {
        .start, .after_bom, .after_xml_declaration, .after_structure_end, .text, .end => {},
        .element_start => try writer.raw(">"),
    }
    try writer.raw(s);
    writer.state = switch (writer.state) {
        .start, .after_bom, .after_xml_declaration => .after_xml_declaration,
        .element_start, .after_structure_end, .text => .text,
        .end => .end,
    };
}

fn newLineAndIndent(writer: *Writer) anyerror!void {
    if (writer.options.indent.len == 0) return;

    try writer.raw("\n");
    var n: usize = 0;
    while (n < writer.indent_level) : (n += 1) {
        try writer.raw(writer.options.indent);
    }
}

fn raw(writer: *Writer, s: []const u8) anyerror!void {
    try writer.sink.write(s);
}

test {
    _ = T;
}
const T = struct {
    const Testbed = struct {
        a: std.mem.Allocator,
        buf: std.ArrayListUnmanaged(u8),
        fn init(a: std.mem.Allocator) Testbed {
            return .{
                .a = a,
                .buf = .{},
            };
        }
        fn writer(self: *Testbed, indent: []const u8) !Writer {
            return Writer.init(self.a, .{
                .context = self,
                .writeFn = write,
            }, .{ .indent = indent });
        }
        fn write(context: *const anyopaque, data: []const u8) anyerror!void {
            // TODO not sure why context is const.
            var self: *Testbed = @constCast(@alignCast(@ptrCast(context)));
            try self.buf.appendSlice(self.a, data);
        }
        fn output(self: *Testbed) []const u8 {
            return self.buf.items;
        }
        fn deinit(self: *Testbed) void {
            self.buf.deinit(self.a);
        }
    };
    test "embed" {
        var tb = Testbed.init(std.testing.allocator);
        defer tb.deinit();
        var wtr = try tb.writer("  ");
        defer wtr.deinit();
        try wtr.xmlDeclaration("UTF-8", null);
        try wtr.elementStart("foo");
        try wtr.embed("<bar>Baz!</bar>");
        try wtr.elementEnd("foo");
        try std.testing.expectEqualStrings(
            \\<?xml version="1.0" encoding="UTF-8"?>
            \\<foo><bar>Baz!</bar></foo>
        , tb.output());
    }

    test "namespace" {
        var tb = Testbed.init(std.testing.allocator);
        defer tb.deinit();
        var wtr = try tb.writer("  ");
        defer wtr.deinit();
        try wtr.elementStartNs("foo", "foospace", .{
            .defaultNs = "barspace",
            .prefixedNs = &.{ "foospace", "bazspace" },
        });
        // nested element changes the default namespace and repeats an existing prefixed namespace
        try wtr.elementStartNs("zap", "zapspace", .{
            .defaultNs = "zapspace",
            .prefixedNs = &.{ "bazspace", "zingspace" },
        });
        // sub-element inherits the new default
        try wtr.elementStartNs("zip", "zapspace", .{});
        try wtr.elementEndNs("zip", "zapspace");
        // sub-element can still access prefixes from outer scope
        try wtr.elementStartNs("fooooo", "foospace", .{});
        try wtr.elementEndNs("fooooo", "foospace");
        try wtr.elementEndNs("zap", "zapspace");
        // element in the default namespace; should have no prefix
        try wtr.elementStartNs("bar", "barspace", .{});
        try wtr.elementEndNs("bar", "barspace");
        // element in a prefixed namespace
        try wtr.elementStartNs("baz", "bazspace", .{});
        // nested element in default namespace
        try wtr.elementStartNs("fee", "barspace", .{});
        try wtr.elementEndNs("fee", "barspace");
        // nested element introduces another namespace
        try wtr.elementStartNs("fie", "fiespace", .{ .prefixedNs = &.{"fiespace"} });
        try wtr.elementEndNs("fie", "fiespace");
        try wtr.elementEndNs("baz", "bazspace");
        try wtr.elementEndNs("foo", "foospace");
        try std.testing.expectEqualStrings(
            \\<a:foo xmlns="barspace" xmlns:a="foospace" xmlns:b="bazspace">
            \\  <zap xmlns="zapspace" xmlns:c="zingspace">
            \\    <zip>
            \\    </zip>
            \\    <a:fooooo>
            \\    </a:fooooo>
            \\  </zap>
            \\  <bar>
            \\  </bar>
            \\  <b:baz>
            \\    <fee>
            \\    </fee>
            \\    <d:fie xmlns:d="fiespace">
            \\    </d:fie>
            \\  </b:baz>
            \\</a:foo>
        , tb.output());
    }
};
