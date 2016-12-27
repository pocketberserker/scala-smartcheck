import SmartCheckBuild._

lazy val smartcheck = Project(
  id = "smartcheck",
  base = file("smartcheck"),
  settings = buildSettings
)

lazy val examples = Project(
  id = "examples",
  base = file("examples"),
  settings = exampleSettings
)
  .dependsOn(smartcheck)

publish := {}
publishLocal := {}
publishArtifact := false
