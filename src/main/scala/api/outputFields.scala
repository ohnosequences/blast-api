package ohnosequences.blast.api

import ohnosequences.cosas._, types._, records._, fns._, klists._, typeUnions._
import java.math.BigDecimal

/*
  ### BLAST output formats and fields

  A lot of different outputs, plus the possibility of choosing fields for CSV/TSV output.
*/
// TODO: add parsing. use the label for parsing the key afterwards
sealed trait AnyOutputField extends AnyType

trait OutputField[V] extends AnyOutputField {

  type Raw = V
  lazy val label: String = toString
}

/* Inside this object you have all the possible fields that you can specify as output */
case object outputFields {

  // parsers
  val intParser: String => Option[Int] = str => {
    import scala.util.control.Exception._
    catching(classOf[NumberFormatException]) opt str.toInt
  }

  val doubleParser: String => Option[Double] = str => {
    import scala.util.control.Exception._
    catching(classOf[NumberFormatException]) opt str.toDouble
  }
  val longParser: String => Option[Long] = str => {
    import scala.util.control.Exception._
    catching(classOf[NumberFormatException]) opt str.toLong
  }

  val bigDecimalParser: String => Option[BigDecimal] = str => {
    import scala.util.control.Exception._
    catching(classOf[NumberFormatException]) opt new BigDecimal(str)
  }

  /* Query Seq-id */
  type qseqid = qseqid.type
  case object qseqid    extends OutputField[String]
  implicit val qseqidParser: DenotationParser[qseqid, String, String] =
    new DenotationParser(qseqid, qseqid.label)({ s: String => Some(s) })
  implicit val qseqidSerializer: DenotationSerializer[qseqid, String, String] =
    new DenotationSerializer(qseqid, qseqid.label)({ v: String => Some(v) })

  /* Query GI */
  case object qgi       extends OutputField[String]
  // means Query accesion
  case object qacc      extends OutputField[String]
  // means Query accesion.version
  // case object qaccver   extends OutputField[Int]

  /* Query sequence length */
  type qlen = qlen.type
  case object qlen      extends OutputField[Int]
  implicit val qlenParser: DenotationParser[qlen,Int,String] =
    new DenotationParser(qlen, qlen.label)({ intParser })
  implicit val qlenSerializer: DenotationSerializer[qlen,Int,String] =
    new DenotationSerializer(qlen, qlen.label)({ v => Some(v.toString) })

  /* Subject Seq-id */
  type sseqid = sseqid.type
  case object sseqid    extends OutputField[String]
  implicit val sseqidParser: DenotationParser[sseqid,String,String] =
    new DenotationParser(sseqid, sseqid.label)({ s: String => Some(s) })
  implicit val sseqidSerializer: DenotationSerializer[sseqid,String,String] =
    new DenotationSerializer(sseqid, sseqid.label)({ v: String => Some(v) })


  // means All subject Seq-id(s), separated by a ';'
  // case object sallseqid extends OutputField[List[String]]

  /* Subject GI */
  type sgi = sgi.type
  case object sgi       extends OutputField[String]
  implicit val sgiParser: DenotationParser[sgi,String,String] =
    new DenotationParser(sgi, sgi.label)({ s: String => Some(s) })
  implicit val sgiSerializer: DenotationSerializer[sgi,String,String] =
    new DenotationSerializer(sgi, sgi.label)({ v: String => Some(v) })

  // means All subject GIs
  // case object sallgi    extends OutputField[List[String]]
  // means Subject accession
  case object sacc      extends OutputField[String]
  // means Subject accession.version
  case object saccver   extends OutputField[String]
  // means All subject accessions
  case object sallacc   extends OutputField[String]

  /* Subject sequence length */
  type slen = slen.type
  case object slen      extends OutputField[Int]
  implicit val slenParser: DenotationParser[slen,Int,String] =
    new DenotationParser(slen, slen.label)(intParser)
  implicit val slenSerializer: DenotationSerializer[slen,Int,String] =
    new DenotationSerializer(slen, slen.label)({ v => Some(v.toString) })


  /* Start of alignment in query */
  type qstart = qstart.type
  case object qstart    extends OutputField[Int]
  implicit val qstartParser: DenotationParser[qstart,Int,String] =
    new DenotationParser(qstart, qstart.label)(intParser)
  implicit val qstartSerializer: DenotationSerializer[qstart,Int,String] =
    new DenotationSerializer(qstart, qstart.label)({ v => Some(v.toString) })

  /* End of alignment in query */
  type qend = qend.type
  case object qend      extends OutputField[Int]
  implicit val qendParser: DenotationParser[qend,Int,String] =
    new DenotationParser(qend, qend.label)(intParser)
  implicit val qendSerializer: DenotationSerializer[qend,Int,String] =
    new DenotationSerializer(qend, qend.label)({ v => Some(v.toString) })

  /* Start of alignment in subject */
  type sstart = sstart.type
  case object sstart    extends OutputField[Int]
  implicit val sstartParser: DenotationParser[sstart,Int,String] =
    new DenotationParser(sstart, sstart.label)(intParser)
  implicit val sstartSerializer: DenotationSerializer[sstart,Int,String] =
    new DenotationSerializer(sstart, sstart.label)({ v => Some(v.toString) })

  /* End of alignment in subject */
  type send = send.type
  case object send      extends OutputField[Int]
  implicit val sendParser: DenotationParser[send,Int,String] =
    new DenotationParser(send, send.label)(intParser)
  implicit val sendSerializer: DenotationSerializer[send,Int,String] =
    new DenotationSerializer(send, send.label)({ v => Some(v.toString) })

  // means Aligned part of query sequence
  type qseq = qseq.type
  case object qseq      extends OutputField[String]

  // means Aligned part of subject sequence
  type sseq = sseq.type
  case object sseq      extends OutputField[String]

  // means Expect value
  // TODO make it BigDecimal
  case object evalue    extends OutputField[BigDecimal]
  // TODO this does not seem to work as expected
  implicit val evalueParser: DenotationParser[evalue.type,BigDecimal,String] =
    new DenotationParser(evalue, evalue.label)(bigDecimalParser)
  implicit val evalueSerializer: DenotationSerializer[evalue.type,BigDecimal,String] =
    new DenotationSerializer(evalue, evalue.label)({ v => Some(v.toString) })

  // means Bit score
  type bitscore = bitscore.type
  case object bitscore  extends OutputField[Long]
  implicit val bitscoreParser: DenotationParser[bitscore,Long,String] =
    new DenotationParser(bitscore, bitscore.label)(longParser)
  implicit val bitscoreSerializer: DenotationSerializer[bitscore,Long,String] =
    new DenotationSerializer(bitscore, bitscore.label)({ v => Some(v.toString) })

  // means Raw score
  type score = score.type
  case object score     extends OutputField[Long]
  implicit val scoreParser: DenotationParser[score,Long,String] =
    new DenotationParser(score, score.label)(longParser)
  implicit val scoreSerializer: DenotationSerializer[score,Long,String] =
    new DenotationSerializer(score, score.label)({ v => Some(v.toString) })

  // means Alignment length
  type length = length.type
  case object length    extends OutputField[Int]
  implicit val lengthParser: DenotationParser[length,Int,String] =
    new DenotationParser(length, length.label)(intParser)
  implicit val lengthSerializer: DenotationSerializer[length,Int,String] =
    new DenotationSerializer(length, length.label)({ v => Some(v.toString) })

  // means Percentage of identical matches
  type pident = pident.type
  case object pident    extends OutputField[Double]
  implicit val pidentParser: DenotationParser[pident,Double,String] =
    new DenotationParser(pident, pident.label)(doubleParser)
  implicit val pidentSerializer: DenotationSerializer[pident,Double,String] =
    new DenotationSerializer(pident, pident.label)({ v => Some(v.toString) })

  // means Number of mismatches
  type mismatch = mismatch.type
  case object mismatch  extends OutputField[Int]
  implicit val mismatchParser: DenotationParser[mismatch,Int,String] =
    new DenotationParser(mismatch, mismatch.label)(intParser)
  implicit val mismatchSerializer: DenotationSerializer[mismatch,Int,String] =
    new DenotationSerializer(mismatch, mismatch.label)({ v => Some(v.toString) })

  // means Number of positive-scoring matches
  type positive = positive.type
  case object positive  extends OutputField[Int]
  implicit val positiveParser: DenotationParser[positive,Int,String] =
    new DenotationParser(positive, positive.label)(intParser)
  implicit val positiveSerializer: DenotationSerializer[positive,Int,String] =
    new DenotationSerializer(positive, positive.label)({ v => Some(v.toString) })

  // means Number of gap openings
  type gapopen = gapopen.type
  case object gapopen   extends OutputField[Int]
  implicit val gapopenParser: DenotationParser[gapopen,Int,String] =
    new DenotationParser(gapopen, gapopen.label)(intParser)
  implicit val gapopenSerializer: DenotationSerializer[gapopen,Int,String] =
    new DenotationSerializer(gapopen, gapopen.label)({ v => Some(v.toString) })

  // means Total number of gaps
  type gaps = gaps.type
  case object gaps      extends OutputField[Int]
  implicit val gapsParser: DenotationParser[gaps,Int,String] =
    new DenotationParser(gaps, gaps.label)(intParser)
  implicit val gapsSerializer: DenotationSerializer[gaps,Int,String] =
    new DenotationSerializer(gaps, gaps.label)({ v => Some(v.toString) })

  // means Query frame
  case object qframe    extends OutputField[String]
  // means Subject frame
  case object sframe    extends OutputField[String]

  // query coverage per subject. See https://www.biostars.org/p/121972/#122201
  type qcovs = qcovs.type
  case object qcovs extends OutputField[Double]
  implicit val qcovsParser: DenotationParser[qcovs,Double,String] =
    new DenotationParser(qcovs, qcovs.label)(doubleParser)
  implicit val qcovsSerializer: DenotationSerializer[qcovs,Double,String] =
  new DenotationSerializer(qcovs, qcovs.label)({ v => Some(v.toString) })

  // means Number of identical matches
  type nident = nident.type
  case object nident extends OutputField[Int]
  implicit val nidentParser: DenotationParser[nident,Int,String] =
    new DenotationParser(nident, nident.label)(intParser)
  implicit val nidentSerializer: DenotationSerializer[nident,Int,String] =
    new DenotationSerializer(nident, nident.label)({ v => Some(v.toString) })

  // means Percentage of positive-scoring matches
  type ppos = ppos.type
  case object ppos extends OutputField[Double]
  implicit val pposParser: DenotationParser[ppos,Double,String] =
    new DenotationParser(ppos, ppos.label)(doubleParser)
  implicit val pposSerializer: DenotationSerializer[ppos,Double,String] =
  new DenotationSerializer(ppos, ppos.label)({ v => Some(v.toString) })


  // TODO sort these out
  // case object frames extends OutputField[String]  { // means Query and subject frames separated by a '/'
  // }
  // case object btop extends OutputField[String]  { // means Blast traceback operations (BTOP)
  // }
  // case object staxids extends OutputField[String]  { // means unique Subject Taxonomy ID(s), separated by a ';' (in numerical order)
  // }
  // case object sscinames extends OutputField[String]  { // means unique Subject Scientific Name(s), separated by a ';'
  // }
  // case object scomnames extends OutputField[String]  { // means unique Subject Common Name(s), separated by a ';'
  // }
  // case object sblastnames extends OutputField[String]  { // means unique Subject Blast Name(s), separated by a ';' (in alphabetical order)
  // }
  // case object sskingdoms extends OutputField[String]  { // means unique Subject Super Kingdom(s), separated by a ';' (in alphabetical order)
  // }
  // case object stitle extends OutputField[String]  { // means Subject Title
  // }
  // case object salltitles extends OutputField[String]  { // means All Subject Title(s), separated by a '<>'
  // }
  // case object sstrand extends OutputField[String]  { // means Subject Strand
  // }
  // case object qcovhsp extends OutputField[String]  { // means Query Coverage Per HSP
  // }
}
