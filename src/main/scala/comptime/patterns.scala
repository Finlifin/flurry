// package comptime

// enum RangeKind:
//   case From
//   case To
//   case FromTo
//   case ToInclusive
//   case FromToInclusive

// case class LiteralPattern(value: Value) extends Pattern
// case class RangePattern(start: Value, end: Value, kind: RangeKind) extends Pattern
// case class TuplePattern(elements: List[Pattern]) extends Pattern
// case class RecordPattern(fields: Map[String, Pattern]) extends Pattern
