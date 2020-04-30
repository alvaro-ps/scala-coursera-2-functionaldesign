course := "progfun2"
assignment := "quickcheck"

scalaVersion := "2.13.0"
scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")
libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0"
libraryDependencies += "com.lihaoyi" % "ammonite" % "2.1.1" % "test" cross CrossVersion.full

testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")

sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
  Seq(file)
}.taskValue
