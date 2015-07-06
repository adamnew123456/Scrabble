scalaVersion := "2.11.6"
ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

libraryDependencies += "junit" % "junit" % "4.11" % "test"
libraryDependencies += "com.novocode" % "junit-interface" % "0.8" % "test->default"

// For Twitter's LRU cache
libraryDependencies += "com.twitter" % "util-collection_2.11" % "6.25.0"
