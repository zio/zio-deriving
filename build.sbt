import BuildHelper._

inThisBuild(
  List(
    organization := "dev.zio",
    homepage     := Some(url("https://zio.github.io/zio-prelude/")),
    licenses     := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers   := List(
      Developer(
        "jdegoes",
        "John De Goes",
        "john@degoes.net",
        url("http://degoes.net")
      )
    )
  )
)

addCommandAlias("fix", "; all compile:scalafix test:scalafix; all scalafmtSbt scalafmtAll")
addCommandAlias("fmt", "; all scalafmtSbt scalafmtAll")
addCommandAlias(
  "check",
  "; scalafmtSbtCheck; scalafmtCheckAll; Test/compile; compile:scalafix --check; test:scalafix --check"
)

addCommandAlias(
  "testJVM",
  ";derivingJVM/test"
)
addCommandAlias(
  "testJS",
  ";derivingJS/test"
)
addCommandAlias(
  "testNative",
  ";derivingNative/test" // `test` currently executes only compilation, see `nativeSettings` in `BuildHelper`
)

val zioVersion             = "2.0.0-RC5"
val scalaCollectionsCompat = "2.7.0"

// there's nothing stopping 2.10 being supported except the need to rewrite all
// the macros using the 2.10 API... and capping the codegen to an arity of 22.
// ThisBuild / crossScalaVersions := List("3.1.1", "2.13.8", "2.12.15", "2.11.12")
// ThisBuild / scalaVersion       := "2.13.8"
lazy val root = project
  .in(file("."))
  .settings(
    name           := "zio-deriving-root",
    publish / skip := true,
    unusedCompileDependenciesFilter -= moduleFilter("org.scala-js", "scalajs-library")
  )
  .aggregate(
    derivingJS,
    derivingJVM,
    derivingNative
  )

lazy val deriving = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("deriving"))
  .settings(stdSettings("zio-deriving"))
  .settings(crossProjectSettings)
  .settings(macroDefinitionSettings)
  .settings(buildInfoSettings("zio.deriving"))
  .settings(
    Compile / sourceGenerators += Def.task {
      val dir       = (Compile / sourceManaged).value
      val data      = dir / "zio" / "deriving" / "data.scala"
      IO.write(data, ShapelyCodeGen.data)
      val derivable = dir / "zio" / "deriving" / "derivable.scala"
      IO.write(derivable, ShapelyCodeGen.derivable)
      Seq(data, derivable)
    }.taskValue,
    Compile / sourceGenerators += Def.task {
      val Some((major, _)) = CrossVersion.partialVersion(scalaVersion.value)
      if (major < 3) Nil
      else {
        val dir  = (Compile / sourceManaged).value
        val file = dir / "zio" / "deriving" / "compat.scala"
        IO.write(file, ShapelyCodeGen.compat)
        Seq(file)
      }
    }.taskValue,
    Test / sourceGenerators += Def.task {
      val dir   = (Compile / sourceManaged).value
      val enums = dir / "wheels" / "enums" / "GeneratedEnums.scala"
      IO.write(enums, ExamplesCodeGen.enums)
      Seq(enums)
    }.taskValue
  )
  .settings(testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")))
  .enablePlugins(BuildInfoPlugin)

lazy val derivingJS = deriving.js
  .settings(jsSettings)
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)

lazy val derivingJVM = deriving.jvm
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)
  .settings(scalaReflectTestSettings)

lazy val derivingNative = deriving.native
  .settings(nativeSettings)

// scalacOptions ++= Seq(
//   "-deprecation",
//   "-feature",
//   "-language:higherKinds"
// )

// Compile / console / scalacOptions -= "-Ywarn-unused"

// Compile / unmanagedSourceDirectories ++= {
//   val dir                  = (Compile / scalaSource).value.getPath
//   val Some((major, minor)) = CrossVersion.partialVersion(scalaVersion.value)
//   val specific             = (major, minor) match {
//     case (3, _)  => file(s"$dir-2.13+3.x") :: Nil
//     case (2, 13) => file(s"$dir-2.11+2.12+2.13") :: file(s"$dir-2.13+3.x") :: Nil
//     case (2, 12) => file(s"$dir-2.11+2.12+2.13") :: file(s"$dir-2.11+2.12") :: Nil
//     case (2, 11) => file(s"$dir-2.11+2.12+2.13") :: file(s"$dir-2.11+2.12") :: Nil
//     case (2, 10) => Nil
//   }

//   file(s"$dir-$major") :: file(s"$dir-$major.$minor") :: specific
// }

// libraryDependencies ++= Seq(
//   "com.novocode" % "junit-interface" % "0.11"   % Test,
//   "junit"        % "junit"           % "4.13.2" % Test
// )
// crossPaths := false // https://github.com/sbt/junit-interface/issues/35
// testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")
// fork       := true

// libraryDependencies ++= {
//   if (scalaVersion.value.startsWith("3."))
//     Seq(
//       "org.scala-lang" % "scala3-compiler_3" % scalaVersion.value % "provided"
//     )
//   else
//     Seq(
//       "org.scala-lang" % "scala-reflect"     % scalaVersion.value % "provided"
//     )
// }

// Compile / sourceManaged := {
//   val dir                  = (Compile / sourceManaged).value
//   val Some((major, minor)) = CrossVersion.partialVersion(scalaVersion.value)
//   dir / s"scala-${major}.${minor}"
// }
