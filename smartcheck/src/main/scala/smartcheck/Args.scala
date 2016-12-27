package smartcheck

import scalaprops._

sealed abstract class Format
case object PrintTree extends Format
case object PrintString extends Format

case class ScArgs(
  format: Format = PrintTree,
  param: Param = Param.withCurrentTimeSeed(),
  scMaxSize: Int = 10,
  scMaxDepth: Option[Int] = None,
  scMaxReduce: Int = 100,
  runForall: Boolean = true,
  scMaxForall: Int = 20,
  scMinForall: Int = 10,
  runExists: Boolean = true,
  scMaxExists: Int = 20
)
