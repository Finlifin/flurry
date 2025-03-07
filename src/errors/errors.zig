pub const Err = error{
    OutOfMemory,
    FileNotFound,

    InvalidStrLiteral,
    InvalidArbitraryId,
    InvalidIntegerLiteral,
    InvalidFloatLiteral,
    InvalidCharLiteral,
    InvalidMacroCallContent,

    InvalidExpr,
    InvalidPattern,
    InvalidStatement,

    UnexpectedToken,
};

pub const Kind = enum {
    err,
    warn,
    log,
    help,
    info,
};
