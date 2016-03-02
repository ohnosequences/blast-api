package ohnosequences.blast.api

import ohnosequences.cosas._, types._, records._, fns._, klists._, typeUnions._
import better.files._

sealed trait AnyBlastOption extends AnyType {

  /* The `label` is used for generating the command-line `String` representation of this option. For a BLAST option `-x_yz_abc` name your option here `case object x_yz_abc`. */
  lazy val label: String = s"-${toString}"

  /* this is used for serializing values to command-line args */
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
}

/* This works as a type class, which provides a way of serializing a list of AnyBlastOption's */
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

/*
  ### Options

  As the same options are valid for several commands, they are defined independently here.
*/
case object db    extends BlastOption[File](f => f.path.toString)
case object query extends BlastOption[File](f => f.path.toString)
case object out   extends BlastOption[File](f => f.path.toString)

case object num_threads     extends BlastOption[Int](n => n.toString)
case object evalue          extends BlastOption[Double](n => n.toString)
case object max_target_seqs extends BlastOption[Int](n => n.toString)
case object show_gis        extends BlastOption[Boolean](t => "")

case object word_size extends BlastOption[Int](n => if(n < 4) 4.toString else n.toString)
/* penalty needs to be ≤ 0 */
case object penalty extends BlastOption[Int](n => if(n > 0) 0.toString else n.toString)
/* reward needs to be ≥ 0 */
case object reward extends BlastOption[Int](n => if(n < 0) 0.toString else n.toString)
case object ungapped extends BlastOption[Boolean](t => "")

case object num_alignments extends BlastOption[Int](n => if(n < 0) 250.toString else n.toString)

case object strand extends BlastOption[Strands](_.toString)

sealed trait Strands
case object Strands {

  case object both  extends Strands
  case object minus extends Strands
  case object plus  extends Strands
}

/*
  #### `makeblastdb`-specific options
*/
case object title extends BlastOption[String](x => x)
case object in extends BlastOption[File](f => f.path.toString)


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
