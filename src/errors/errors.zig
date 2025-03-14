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
    InvalidModPath,
    InvalidDefinition,

    NoRecordCall,
    EndOfTerm,
    UnexpectedToken,

    UnknownError,
    TestingError,
    NoError,
};

pub const Kind = enum {
    err,
    warn,
    log,
    help,
    info,
};
