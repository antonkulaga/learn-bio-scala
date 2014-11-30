import bintray.Plugin.bintraySettings

import bintray.Opts

import bintray.Keys._

name := "learn-bio"

version := "0.1"

scalaVersion := "2.11.4"

bintraySettings

// Scalaxy snapshots are published on the Sonatype repository.
resolvers += Resolver.sonatypeRepo("snapshots")

resolvers += Opts.resolver.repo("denigma", "denigma-releases")

resolvers += Opts.resolver.repo("denigma", "bio")

resolvers += "Pellucid Bintray" at "http://dl.bintray.com/pellucid/maven"

resolvers += "BioJava repository" at "http://www.biojava.org/download/maven/"

libraryDependencies += "me.shadaj" %% "genalgo" % "0.1.3.1"

libraryDependencies += "org.scalanlp" %% "breeze" % "0.8.1"

val biojavaVersion = "3.1.0"

libraryDependencies += "org.biojava" % "biojava3-core" % biojavaVersion


libraryDependencies += "org.biojava" % "biojava3-genome" % biojavaVersion

libraryDependencies += "com.github.benhutchison" %% "microjson" % "1.0"

libraryDependencies += "org.scala-saddle" %% "saddle-core" % "1.3.3"

//val scalaxyVersion = "0.4-SNAPSHOT"

//libraryDependencies += "com.nativelibs4java" %% "scalaxy-loops" % scalaxyVersion

//libraryDependencies += "com.nativelibs4java" %% "scalaxy-streams" % scalaxyVersion


libraryDependencies += "com.github.scala-blitz" %% "scala-blitz" % "1.2"

libraryDependencies += "com.softwaremill.macwire" %% "macros" % "0.7.3"

libraryDependencies += "com.pellucid" %% "framian" % "0.3.3"
