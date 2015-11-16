package ohnosequences.blast

import ohnosequences.datasets._
import api._

case object data {

  trait AnyBlastOutputType extends AnyDataType {

    type BlastExpressionType <: AnyBlastExpressionType
    val blastExpressionType: BlastExpressionType
  }

  abstract class BlastOutputType[BET <: AnyBlastExpressionType](val blastExpressionType: BET, val label: String)
  extends AnyBlastOutputType {

    type BlastExpressionType = BET
  }

  trait AnyBlastOutput extends AnyData {

    type DataType <: AnyBlastOutputType
    val dataType: DataType
  }

  abstract class BlastOutput[BOT <: AnyBlastOutputType](val dataType: BOT, val label: String) extends AnyBlastOutput {

    type DataType = BOT
  }
}
