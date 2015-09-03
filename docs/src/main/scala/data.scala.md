
```scala
package ohnosequences.blast

import ohnosequences.datasets._, dataSets._
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

```




[test/scala/CommandGeneration.scala]: ../../test/scala/CommandGeneration.scala.md
[test/scala/OutputParsing.scala]: ../../test/scala/OutputParsing.scala.md
[test/scala/OutputFieldsSpecification.scala]: ../../test/scala/OutputFieldsSpecification.scala.md
[main/scala/api.scala]: api.scala.md
[main/scala/data.scala]: data.scala.md