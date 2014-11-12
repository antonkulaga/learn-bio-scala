import bintray.Plugin.bintraySettings


import bintray.Opts

import bintray.Keys._

name := "learn-bio"

version := "0.1"

scalaVersion := "2.11.4"

bintraySettings

resolvers += Opts.resolver.repo("denigma", "denigma-releases")

resolvers += "BioJava repository" at "http://www.biojava.org/download/maven/"

libraryDependencies += "me.shadaj" %% "genalgo" % "0.1.3"

libraryDependencies += "org.scalanlp" %% "breeze" % "0.8.1"

val biojavaVersion = "3.1.0"

libraryDependencies += "org.biojava" % "biojava3-core" % biojavaVersion


libraryDependencies += "org.biojava" % "biojava3-genome" % biojavaVersion



libraryDependencies += "org.scala-saddle" %% "saddle-core" % "1.3.3"

//libraryDependencies += "com.nativelibs4java" %% "scalaxy-streams" % "0.3.3"

libraryDependencies += "com.github.scala-blitz" % "scala-blitz_2.11" % "1.2"