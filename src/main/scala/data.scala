package ohnosequences.blast

import ohnosequences.datasets._, dataSets._
import api._

case object data {

  trait AnyBlastOutputType extends AnyDataType {

    // this is OK to be known statically
    type OutputRecord = BlastExpression#OutputRecord
    val outputRecord: OutputRecord

    type BlastExpression <: AnyBlastExpression
  }
  abstract class BlastOutputType[BE <: AnyBlastExpression](val outputRecord: BE#OutputRecord, val label: String)
  extends AnyBlastOutputType {

    type BlastExpression = BE
  }

  trait AnyBlastOutput extends AnyData {

    type DataType <: AnyBlastOutputType
    val dataType: DataType
  }

  abstract class BlastOutput[BOT <: AnyBlastOutputType](val dataType: BOT, val label: String) extends AnyBlastOutput {

    type DataType = BOT
  }
}
