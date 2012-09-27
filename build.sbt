name := "scalagp"

version := "0.0.1"

scalaVersion := "2.9.1"

libraryDependencies += "org.specs2" %% "specs2" % "1.12.1" % "test"

scalacOptions += "-deprecation"

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                  "releases"  at "http://oss.sonatype.org/content/repositories/releases")
