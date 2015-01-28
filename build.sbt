name := "reasonably-priced-monad"

scalaVersion := "2.10.4"

unmanagedSourceDirectories in Compile <<= (scalaSource in Compile) (Seq(_))

unmanagedSourceDirectories in Test := Seq()

libraryDependencies := Seq(
  "org.scalaz" %% "scalaz-score" % "7.1.0"
)
