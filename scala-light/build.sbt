name := "scala-light"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "0.3.4"
)

onlyScalaSource(in = Compile)

onlyScalaSource(in = Test)

def onlyScalaSource(in: Configuration) = unmanagedSourceDirectories in in := Seq((scalaSource in in).value)
