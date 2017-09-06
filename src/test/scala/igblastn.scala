package ohnosequences.blast.test

import ohnosequences.blast.api.igblastn.output
import ohnosequences.blast.api._
import ohnosequences.cosas._, types._, klists._, records._

class TCRAOutput extends org.scalatest.FunSuite {

  import output._, TCRA._

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

  import output._, TCRB._

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
