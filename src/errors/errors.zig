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
