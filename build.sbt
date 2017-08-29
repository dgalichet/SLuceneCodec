name := "fields-codec"

scalaVersion := "2.12.3"

val scalaTestVersion = "3.0.1"
val scalacheckVersion = "1.13.4"
val scalacheckShapelessVersion = "1.1.5"
val luceneVersion = "6.4.1"
val catsVersion = "0.9.0"
val shapelessVersion = "2.3.2"

libraryDependencies ++= Seq(
  "org.typelevel"         %% "cats-core"                          % catsVersion,
  "org.apache.lucene"     %  "lucene-core"                        % luceneVersion,
  "org.apache.lucene"     %  "lucene-queryparser"                 % luceneVersion,
  "org.apache.lucene"     %  "lucene-analyzers-common"            % luceneVersion,
  "com.chuusai"           %% "shapeless"                          % shapelessVersion,
  "org.scalatest"               %% "scalatest"                    % scalaTestVersion % "test",
  "org.scalacheck"              %% "scalacheck"                   % scalacheckVersion % "test",
  "com.github.alexarchambault"  %% "scalacheck-shapeless_1.13"    % scalacheckShapelessVersion % "test"
)
