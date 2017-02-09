package ohnosequences.blast.api.parse

import ohnosequences.blast.api.{ BlastOutputRecord, AnyOutputField, OutputField }
import ohnosequences.blast.api.outputFields._
import ohnosequences.cosas._, types._, klists._, records._

/*
  # IgBLAST output structure

  The only parseable IgBLAST output corresponds to `outfmt 7`; let's describe it.

  > **WARNING** this version works with the results of **one** query sequence

  ## Header

  Something like

  ```
  # IGBLASTN 2.6.0+
  # Query: JQ778271.1 Homo sapiens clone 71 TCR alpha chain mRNA, complete cds
  # Database: IG_DB/imgt.TR.Homo_sapiens.V.f.orf.p IG_DB/imgt.TR.Homo_sapiens.D.f.orf IG_DB/imgt.TR.Homo_sapiens.J.f.orf.p
  # Domain classification requested: imgt
  ```

  The essential bit here is the query ID: the line which starts with `# Query: `. If we take the query ID from the hit table (see below) we could forget about this header; that would be good.
*/
case object igblastn {

  case object linePrefixes {

    val query                 = "# Query:"
    val domainClassification  = "# Domain classification requested:"
  }
}
