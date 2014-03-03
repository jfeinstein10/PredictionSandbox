name := "scalding-sandbox"

version := "1.0"

libraryDependencies  ++= Seq(
  "org.scalanlp" % "breeze_2.10" % "0.5.2",
  "org.scalanlp" % "nak" % "1.2.0",
  "org.apache.hadoop" % "hadoop-core" % "1.0.4",
  "com.twitter" % "scalding-core_2.10" % "0.8.11",
  "com.twitter" % "scalding-args_2.10" % "0.8.11",
  "cascading" % "cascading-core" % "2.5.2",
  "cascading" % "cascading-hadoop" % "2.5.2",
  "cascading" % "cascading-local" % "2.5.2",
  "org.slf4j" % "slf4j-simple" % "1.7.6"
)

resolvers ++= Seq(
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Concurrent Maven Repo" at "http://conjars.org/repo"
)