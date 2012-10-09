import sbt._
import Keys._

object ScalaGPBuild extends Build {
  lazy val scalaGP = Project(id = "scalagp", 
                             base = file("."))

  lazy val examples = Project(id = "scalagp-examples",
                              base = file("scalagp-examples")) dependsOn(scalaGP)
}
