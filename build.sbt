name := "WhileyParser"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.0.0"

ratsScalaRepetitionType := Some (VectorType)

ratsUseScalaOptions := true

ratsUseScalaPositions := true

ratsDefineASTClasses := true

ratsDefinePrettyPrinter := true

ratsUseKiama := 2