import sbt._
import Keys._

object SubstitutionsBuild extends Build {
  
  object Projects {
    lazy val scalaUtils = RootProject(uri("git://github.com/kchaloux/scala-utils.git"))
  }

  lazy val substitutions = Project("substitutions", file("."))
    .dependsOn(Projects.scalaUtils)
}

