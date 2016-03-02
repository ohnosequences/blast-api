
```scala
package ohnosequences.blast.api

import ohnosequences.cosas._, types._, records._, fns._, klists._, typeUnions._
```


### BLAST output formats and fields

A lot of different outputs, plus the possibility of choosing fields for CSV/TSV output.


```scala
// TODO: add parsing. use the label for parsing the key afterwards
sealed trait AnyOutputField extends AnyType

trait OutputField[V] extends AnyOutputField {

  type Raw = V
  lazy val label: String = toString
}
```

Inside this object you have all the possible fields that you can specify as output

```scala
case object outputFields {

  // parsers
  val intParser: String => Option[Int] = str => {
    import scala.util.control.Exception._
    catching(classOf[NumberFormatException]) opt str.toInt
  }

  val longParser: String => Option[Long] = str => {
    import scala.util.control.Exception._
    catching(classOf[NumberFormatException]) opt str.toLong
  }

  val doubleFromScientificNotation: String => Option[Double] = str => {
    import java.math.BigDecimal
    // funny blast output
    Some( (new BigDecimal( str.replace("e", "E") )).doubleValue )
  }
```

Query Seq-id

```scala
  type qseqid = qseqid.type
  case object qseqid    extends OutputField[String]
  implicit val qseqidParser: DenotationParser[qseqid, String, String] =
    new DenotationParser(qseqid, qseqid.label)({ s: String => Some(s) })
  implicit val qseqidSerializer: DenotationSerializer[qseqid, String, String] =
    new DenotationSerializer(qseqid, qseqid.label)({ v: String => Some(v) })
```

Query GI

```scala
  case object qgi       extends OutputField[String]
  // means Query accesion
  case object qacc      extends OutputField[String]
  // means Query accesion.version
  case object qaccver   extends OutputField[Int]
```

Query sequence length

```scala
  type qlen = qlen.type
  case object qlen      extends OutputField[Int]
  implicit val qlenParser: DenotationParser[qlen,Int,String] =
    new DenotationParser(qlen, qlen.label)({ intParser })
  implicit val qlenSerializer: DenotationSerializer[qlen,Int,String] =
    new DenotationSerializer(qlen, qlen.label)({ v => Some(v.toString) })
```

Subject Seq-id

```scala
  type sseqid = sseqid.type
  case object sseqid    extends OutputField[String]
  implicit val sseqidParser: DenotationParser[sseqid,String,String] =
    new DenotationParser(sseqid, sseqid.label)({ s: String => Some(s) })
  implicit val sseqidSerializer: DenotationSerializer[sseqid,String,String] =
    new DenotationSerializer(sseqid, sseqid.label)({ v: String => Some(v) })


  // means All subject Seq-id(s), separated by a ';'
  case object sallseqid extends OutputField[List[String]]
```

Subject GI

```scala
  type sgi = sgi.type
  case object sgi       extends OutputField[String]
  implicit val sgiParser: DenotationParser[sgi,String,String] =
    new DenotationParser(sgi, sgi.label)({ s: String => Some(s) })
  implicit val sgiSerializer: DenotationSerializer[sgi,String,String] =
    new DenotationSerializer(sgi, sgi.label)({ v: String => Some(v) })

  // means All subject GIs
  case object sallgi    extends OutputField[List[String]]
  // means Subject accession
  case object sacc      extends OutputField[String]
  // means Subject accession.version
  case object saccver   extends OutputField[String]
  // means All subject accessions
  case object sallacc   extends OutputField[String]
```

Subject sequence length

```scala
  type slen = slen.type
  case object slen      extends OutputField[Int]
  implicit val slenParser: DenotationParser[slen,Int,String] =
    new DenotationParser(slen, slen.label)(intParser)
  implicit val slenSerializer: DenotationSerializer[slen,Int,String] =
    new DenotationSerializer(slen, slen.label)({ v => Some(v.toString) })
```

Start of alignment in query

```scala
  type qstart = qstart.type
  case object qstart    extends OutputField[Int]
  implicit val qstartParser: DenotationParser[qstart,Int,String] =
    new DenotationParser(qstart, qstart.label)(intParser)
  implicit val qstartSerializer: DenotationSerializer[qstart,Int,String] =
    new DenotationSerializer(qstart, qstart.label)({ v => Some(v.toString) })
```

End of alignment in query

```scala
  type qend = qend.type
  case object qend      extends OutputField[Int]
  implicit val qendParser: DenotationParser[qend,Int,String] =
    new DenotationParser(qend, qend.label)(intParser)
  implicit val qendSerializer: DenotationSerializer[qend,Int,String] =
    new DenotationSerializer(qend, qend.label)({ v => Some(v.toString) })
```

Start of alignment in subject

```scala
  type sstart = sstart.type
  case object sstart    extends OutputField[Int]
  implicit val sstartParser: DenotationParser[sstart,Int,String] =
  new DenotationParser(sstart, sstart.label)(intParser)
  implicit val sstartSerializer: DenotationSerializer[sstart,Int,String] =
    new DenotationSerializer(sstart, sstart.label)({ v => Some(v.toString) })
```

End of alignment in subject

```scala
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
  case object evalue    extends OutputField[Double]
  // TODO this does not seem to work as expected
  implicit val evalueParser: DenotationParser[evalue.type,Double,String] =
    new DenotationParser(evalue, evalue.label)(doubleFromScientificNotation)
  implicit val evalueSerializer: DenotationSerializer[evalue.type,Double,String] =
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
  case object length    extends OutputField[Int]
  // means Percentage of identical matches
  case object pident    extends OutputField[Double]
  // means Number of mismatches
  case object mismatch  extends OutputField[Int]
  // means Number of positive-scoring matches
  case object positive  extends OutputField[Int]
  // means Number of gap openings
  case object gapopen   extends OutputField[Int]
  // means Total number of gaps
  case object gaps      extends OutputField[Int]
  // means Query frame
  case object qframe    extends OutputField[String]
  // means Subject frame
  case object sframe    extends OutputField[String]

  // TODO sort these out
  // means Number of identical matches
  // case object nident extends OutputField[String]  {
  // }
  // case object ppos extends OutputField[String]  { // means Percentage of positive-scoring matches
  // }
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
  // case object qcovs extends OutputField[String]  { // means Query Coverage Per Subject
  // }
  // case object qcovhsp extends OutputField[String]  { // means Query Coverage Per HSP
  // }
}

```




[test/scala/CommandGeneration.scala]: ../../../test/scala/CommandGeneration.scala.md
[test/scala/OutputParsing.scala]: ../../../test/scala/OutputParsing.scala.md
[test/scala/OutputFieldsSpecification.scala]: ../../../test/scala/OutputFieldsSpecification.scala.md
[main/scala/api/outputFields.scala]: outputFields.scala.md
[main/scala/api/options.scala]: options.scala.md
[main/scala/api/package.scala]: package.scala.md
[main/scala/api/expressions.scala]: expressions.scala.md
[main/scala/api/commands/blastn.scala]: commands/blastn.scala.md
[main/scala/api/commands/blastp.scala]: commands/blastp.scala.md
[main/scala/api/commands/tblastx.scala]: commands/tblastx.scala.md
[main/scala/api/commands/tblastn.scala]: commands/tblastn.scala.md
[main/scala/api/commands/blastx.scala]: commands/blastx.scala.md
[main/scala/api/commands/makeblastdb.scala]: commands/makeblastdb.scala.md