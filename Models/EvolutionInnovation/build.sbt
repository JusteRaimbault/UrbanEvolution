scalaVersion := "2.13.1"

name := "urbanevolution"

version := "0.1-SNAPSHOT"

//mainClass in (Compile, run) := Some("urbanevolution.UrbanEvolution")

enablePlugins(SbtOsgi)
OsgiKeys.exportPackage := Seq("urbanevolution.*")
OsgiKeys.importPackage := Seq("*;resolution:=optional")
OsgiKeys.privatePackage := Seq("!scala.*,*")
OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))""""


resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("staging"),
  Resolver.mavenCentral,
  ("icm" at "http://maven.icm.edu.pl/artifactory/repo").withAllowInsecureProtocol(true)
)


libraryDependencies ++= Seq(
  "org.openmole.library" %% "spatialdata" % "0.4-SNAPSHOT"
)
