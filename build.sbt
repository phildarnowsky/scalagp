name := "scalagp"

version := "0.0.1"

scalaVersion := "2.10.1"

libraryDependencies += "org.specs2" %% "specs2" % "1.14" % "test"

libraryDependencies += "org.mockito" % "mockito-all" % "1.9.0" % "test"

scalacOptions += "-deprecation"

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                  "releases"  at "http://oss.sonatype.org/content/repositories/releases")
