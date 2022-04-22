organization := "dev.zio"
name := "zio-deriving"

// there's nothing stopping 2.10 being supported except the need to rewrite all
// the macros using the 2.10 API... and capping the codegen to an arity of 22.
ThisBuild / crossScalaVersions := List("3.1.1", "2.13.8", "2.12.15", "2.11.12")
ThisBuild / scalaVersion := "2.13.8"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-language:higherKinds"
)

Compile / console / scalacOptions -= "-Ywarn-unused"

Compile / unmanagedSourceDirectories ++= {
  val dir = (Compile / scalaSource).value.getPath
  val Some((major, minor)) = CrossVersion.partialVersion(scalaVersion.value)
  val specific = (major, minor) match {
    case (3, _) => file(s"$dir-2.13+3.x") :: Nil
    case (2, 13) => file(s"$dir-2.11+2.12+2.13") :: file(s"$dir-2.13+3.x") :: Nil
    case (2, 12) => file(s"$dir-2.11+2.12+2.13") :: file(s"$dir-2.11+2.12") :: Nil
    case (2, 11) => file(s"$dir-2.11+2.12+2.13") :: file(s"$dir-2.11+2.12") :: Nil
    case (2, 10) => Nil
  }

  file(s"$dir-$major") :: file(s"$dir-$major.$minor") :: specific
}

libraryDependencies ++= Seq(
  "com.novocode" % "junit-interface" % "0.11"   % Test,
  "junit"        % "junit"           % "4.13.2" % Test
)
crossPaths := false // https://github.com/sbt/junit-interface/issues/35
testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")
fork := true

libraryDependencies ++= {
  if (scalaVersion.value.startsWith("3.")) Seq(
    "org.scala-lang" % "scala3-compiler_3" % scalaVersion.value % "provided"
  ) else Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
  )
}

Compile / sourceManaged := {
  val dir = (Compile / sourceManaged).value
  val Some((major, minor)) = CrossVersion.partialVersion(scalaVersion.value)
  dir / s"scala-${major}.${minor}"
}

Compile / sourceGenerators += Def.task {
  val dir = (Compile / sourceManaged).value
  val data = dir / "shapely" / "data.scala"
  IO.write(data, ShapelyCodeGen.data)
  val derivable = dir / "shapely" / "derivable.scala"
  IO.write(derivable, ShapelyCodeGen.derivable)
  Seq(data, derivable)
}.taskValue

Compile / sourceGenerators += Def.task {
  val Some((major, _)) = CrossVersion.partialVersion(scalaVersion.value)
  if (major < 3) Nil
  else {
    val dir = (Compile / sourceManaged).value
    val file = dir / "shapely" / "compat.scala"
    IO.write(file, ShapelyCodeGen.compat)
    Seq(file)
  }
}.taskValue

Test / sourceGenerators += Def.task {
  val dir = (Compile / sourceManaged).value
  val enums = dir / "wheels" / "enums" / "GeneratedEnums.scala"
  IO.write(enums, ExamplesCodeGen.enums)
  Seq(enums)
}.taskValue
