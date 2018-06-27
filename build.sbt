organization := "net.turambar"

name := "palimpsest"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.12"

fork in Compile := true

javaOptions in Compile ++= Seq("-Xmx2G")


testOptions in Test ++= Seq(Tests.Filter(s => !s.endsWith("Props")))

//Seq(bintrayResolverSettings:_*)

libraryDependencies ++= Seq(
	"org.scala-lang" % "scala-reflect" % "2.11.12",
//	"com.chuusai" %% "shapeless" % "2.2.5",
	"org.scalatest" %% "scalatest" % "2.2.4" % "test",
	"org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
)


scalacOptions ++= Seq(
	"-optimise",
	"-Yinline-warnings",
//	"-Ylog-classpath",
//	"-Xlog-implicits",
	"-Xexperimental",
	"-feature",
	"-deprecation",
	"-language:postfixOps",
	"-language:implicitConversions",
	"-language:higherKinds",
	"-language:reflectiveCalls",
	"-language:existentials"
)



