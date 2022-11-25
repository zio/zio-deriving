addSbtPlugin("com.github.sbt" % "sbt-unidoc"      % "0.5.0")
addSbtPlugin("com.eed3si9n"   % "sbt-buildinfo"   % "0.11.0")
addSbtPlugin("com.github.sbt" % "sbt-ci-release"  % "1.5.10")
addSbtPlugin("ch.epfl.scala"  % "sbt-scalafix"    % "0.10.1")
addSbtPlugin("org.scalameta"  % "sbt-scalafmt"    % "2.4.6")
addSbtPlugin("dev.zio"        % "zio-sbt-website" % "0.0.0+84-6fd7d64e-SNAPSHOT")

resolvers += Resolver.sonatypeRepo("public")
