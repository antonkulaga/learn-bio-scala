import bintray.Plugin.bintraySettings


import bintray.Opts

import bintray.Keys._

name := "learn-bio"

version := "0.1"

scalaVersion := "2.11.2"

bintraySettings

resolvers += Opts.resolver.repo("denigma", "denigma-releases")

libraryDependencies += "me.shadaj" %% "genalgo" % "0.1.3"

libraryDependencies += "org.scalanlp" %% "breeze" % "0.8.1"

libraryDependencies += "org.biojava" % "biojava3-core" % "3.0"

libraryDependencies += "org.biojava" % "sequencing" % "1.9.1"

libraryDependencies += "org.scala-saddle" %% "saddle-core" % "1.3.3"