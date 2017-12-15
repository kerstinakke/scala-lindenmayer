name := "scala-lindenmayer"

version := "0.1"

scalaVersion := "2.12.4"
enablePlugins(ScalaJSPlugin)

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.2"
libraryDependencies += "com.lihaoyi"  %%% "scalatags" % "0.6.7"
libraryDependencies += "com.github.japgolly.scalajs-react" %%% "core" % "1.1.1"

scalaJSUseMainModuleInitializer := true
mainClass in Compile := Some("Main")

jsDependencies ++= Seq(

  "org.webjars.bower" % "react" % "15.6.1"
    /        "react-with-addons.js"
    minified "react-with-addons.min.js"
    commonJSName "React",

  "org.webjars.bower" % "react" % "15.6.1"
    /         "react-dom.js"
    minified  "react-dom.min.js"
    dependsOn "react-with-addons.js"
    commonJSName "ReactDOM"
)
