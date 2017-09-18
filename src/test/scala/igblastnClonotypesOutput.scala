package ohnosequences.blast.api.test

import ohnosequences.blast.api.parse.igblastn._, clonotypes._
import java.io.File
import java.nio.file._
import scala.collection.JavaConverters._

class IgBLASTOutput extends org.scalatest.FunSuite {

  def outputLines: Iterator[String] =
    Files.lines(new File("data/in/clonotype.out").toPath).iterator.asScala

  def totals: Option[Totals] =
    Totals parseFromLines outputLines

  def summary: Iterator[Option[ClonotypeSummary]] =
    ClonotypeSummary parseFromLines outputLines

  def clonotypes: Iterator[Option[Clonotype]] =
    Clonotype parseFromLines outputLines

  test("Totals summary") {

    assert { totals.isDefined }
  }

  test("Clonotypes summary") {

    summary foreach { opt => assert(opt.isDefined) }
  }

  test("Clonotypes detail") {

    clonotypes foreach { opt => assert(opt.isDefined) }
  }
}
