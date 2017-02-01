
```scala
package ohnosequences.blast.api

import ohnosequences.cosas._, types._, records._, fns._, klists._, typeUnions._
import java.io._

sealed trait AnyBlastOption extends AnyType {
```

The `label` is used for generating the command-line `String` representation of this option. For a BLAST option `-x_yz_abc` name your option here `case object x_yz_abc`.

```scala
  lazy val label: String = s"-${toString}"
```

this is used for serializing values to command-line args

```scala
  val valueToString: Raw => Seq[String]
}

abstract class BlastOption[V](val v: V => String) extends AnyBlastOption {

  type Raw = V

  val valueToString = { x: V => Seq(v(x)) }
}

case object optionValueToSeq extends DepFn1[AnyDenotation, Seq[String]] {

  implicit def default[FO <: AnyBlastOption, V <: FO#Raw](implicit
    option: FO with AnyBlastOption { type Raw = FO#Raw }
  )
  : AnyApp1At[optionValueToSeq.type, FO := V] { type Y = Seq[String] }=
    App1 { v: FO := V => Seq(option.label) ++ option.valueToString(v.value).filterNot(_.isEmpty) }

    implicit def forFlags[FO <: AnyBlastOption { type Raw = Boolean }](implicit
      option: FO
    )
    : AnyApp1At[optionValueToSeq.type, FO := Boolean] { type Y = Seq[String] }=
      App1 {
        v: FO := Boolean =>
          if(v.value)
            Seq(option.label) ++ option.valueToString(v.value).filterNot(_.isEmpty)
          else
            Seq()
      }
}
```

This works as a type class, which provides a way of serializing a list of AnyBlastOption's

```scala
trait BlastOptionsToSeq[L <: AnyKList.withBound[AnyDenotation]] {

  def apply(l: L): Seq[String]
}

case object BlastOptionsToSeq {

  implicit def default[L <: AnyKList.withBound[AnyDenotation], O <: AnyKList.withBound[Seq[String]]](implicit
    mapp: AnyApp2At[mapKList[optionValueToSeq.type, Seq[String]], optionValueToSeq.type, L] { type Y = O }
  ): BlastOptionsToSeq[L] = new BlastOptionsToSeq[L] {
      def apply(l: L): Seq[String] = mapp(optionValueToSeq, l).asList.flatten
  }
}
```


### Options

As the same options are valid for several commands, they are defined independently here.


```scala
case object db    extends BlastOption[Set[File]](f => f.toList.map(_.getCanonicalPath).mkString(" "))
case object query extends BlastOption[File](f => f.getCanonicalPath)
case object out   extends BlastOption[File](f => f.getCanonicalPath)

case object num_threads     extends BlastOption[Int](n => n.toString)
case object evalue          extends BlastOption[BigDecimal](n => n.toString)
case object max_target_seqs extends BlastOption[Int](n => n.toString)
case object show_gis        extends BlastOption[Boolean](t => "")

case object word_size extends BlastOption[Int](n => if(n < 4) 4.toString else n.toString)
```

penalty needs to be ≤ 0

```scala
case object penalty extends BlastOption[Int](n => if(n > 0) 0.toString else n.toString)
```

reward needs to be ≥ 0

```scala
case object reward extends BlastOption[Int](n => if(n < 0) 0.toString else n.toString)
case object ungapped extends BlastOption[Boolean](t => "")

case object strand extends BlastOption[Strands](_.toString)

sealed trait Strands
case object Strands {

  case object both  extends Strands
  case object minus extends Strands
  case object plus  extends Strands
}

// TODO the default values website says that this is an int?!
case object perc_identity extends BlastOption[Double](n => if(n > 100 || n < 0) 0.toString else n.toString)
```


#### `makeblastdb`-specific options


```scala
case object title extends BlastOption[String](x => x)
case object in extends BlastOption[File](f => f.getCanonicalPath)


case object input_type extends BlastOption[DBInputType](t => t.toString)

sealed trait DBInputType
case object DBInputType {

  case object asn1_bin  extends DBInputType
  case object asn1_txt  extends DBInputType
  case object blastdb   extends DBInputType
  case object fasta     extends DBInputType
}


case object dbtype extends BlastOption[BlastDBType](t => t.toString)

sealed trait BlastDBType
case object BlastDBType {
  case object nucl extends BlastDBType
  case object prot extends BlastDBType
}

```




[main/scala/api/commands/blastn.scala]: commands/blastn.scala.md
[main/scala/api/commands/blastp.scala]: commands/blastp.scala.md
[main/scala/api/commands/blastx.scala]: commands/blastx.scala.md
[main/scala/api/commands/makeblastdb.scala]: commands/makeblastdb.scala.md
[main/scala/api/commands/tblastn.scala]: commands/tblastn.scala.md
[main/scala/api/commands/tblastx.scala]: commands/tblastx.scala.md
[main/scala/api/expressions.scala]: expressions.scala.md
[main/scala/api/options.scala]: options.scala.md
[main/scala/api/outputFields.scala]: outputFields.scala.md
[main/scala/api/package.scala]: package.scala.md
[test/scala/CommandGeneration.scala]: ../../../test/scala/CommandGeneration.scala.md
[test/scala/OutputFieldsSpecification.scala]: ../../../test/scala/OutputFieldsSpecification.scala.md
[test/scala/OutputParsing.scala]: ../../../test/scala/OutputParsing.scala.md