package ohnosequences.blast.test

import ohnosequences.blast.api._
import ohnosequences.cosas._, types._, klists._

class IgBLASTCommands extends org.scalatest.FunSuite {

  test("igblastn") {

    val nullFile =
      new java.io.File("/dev/null")

    val expr1 =
      igblastn (
        (query         := nullFile              ) ::
        (germline_db_V := nullFile              ) ::
        (germline_db_D := nullFile              ) ::
        (germline_db_J := nullFile              ) ::
        (ig_seqtype    := IgBLASTSeqType.TCR    ) ::
        (organism      := IgBLASTOrganism.human ) ::
        (clonotype_out := nullFile              ) ::
        (out           := nullFile              ) ::
        *[AnyDenotation],

        (igblastn.defaults update auxiliary_data(Some(nullFile): Option[java.io.File]) :: *[AnyDenotation]).value
      )

    assert { expr1.toSeq.contains("-auxiliary_data") }

    val expr2 =
      igblastn (
        (query         := nullFile              ) ::
        (germline_db_V := nullFile              ) ::
        (germline_db_D := nullFile              ) ::
        (germline_db_J := nullFile              ) ::
        (ig_seqtype    := IgBLASTSeqType.TCR    ) ::
        (organism      := IgBLASTOrganism.human ) ::
        (clonotype_out := nullFile              ) ::
        (out           := nullFile              ) ::
        *[AnyDenotation],

        igblastn.defaults.value
      )

    assert { ! expr2.toSeq.contains("-auxiliary_data") }

  }
}

class TCRAOutput extends org.scalatest.FunSuite {

  lazy val lines =
    io.Source.fromFile("data/igblastn/tcra.out").getLines.toList

  def allRight[X,Y](s: Seq[Either[X,Y]]) = ! s.exists(_.isLeft)

  test("VJ Rearrangement summary") {

    assert { allRight(parse.igblastn.TCRA parseVJRearrangementSummary lines) }
  }

  test("VJ junction details") {

    assert { allRight(parse.igblastn.TCRA parseVJJunctionDetails lines) }
  }

  test("parse CDR3 sequence") {

    assert { allRight(parse.igblastn.TCRA parseCDR3Sequence lines) }
  }

  test("parse V Region annotations") {

    assert { allRight(parse.igblastn.TCRA parseVRegionAnnotations lines) }
  }
}

/////////////////////////////////////////////////////////////////////////////////////////

class TCRBOutput extends org.scalatest.FunSuite {

  lazy val lines =
    io.Source.fromFile("data/igblastn/tcrb.out").getLines.toList

  def allRight[X,Y](s: Seq[Either[X,Y]]) = ! s.exists(_.isLeft)

  test("V(D)J Rearrangement summary") {

    assert { allRight(parse.igblastn.TCRB parseVDJRearrangementSummary lines) }
  }

  test("V(D)J junction details") {

    assert { allRight(parse.igblastn.TCRB parseVDJJunctionDetails lines) }
  }

  test("parse CDR3 sequence") {

    assert { allRight(parse.igblastn.TCRB parseCDR3Sequence lines) }
  }

  test("parse V Region annotations") {

    assert { allRight(parse.igblastn.TCRB parseVRegionAnnotations lines) }
  }

  test("parse hit table") {

    assert { allRight(parse.igblastn.TCRB parseHitTable lines) }
  }
}
