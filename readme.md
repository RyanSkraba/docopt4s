docopt4s - A scala library for using docopt
==============================================================================

[![Java CI with Maven](https://github.com/RyanSkraba/docopt4s/actions/workflows/maven.yml/badge.svg)](https://github.com/RyanSkraba/docopt4s/actions/workflows/maven.yml)

[docopt]: http://docopt.org/ "The original docopt home"
[docopt-github]: https://github.com/docopt/ "The original docopt source"
[docopt-readthedocs]: https://docopt.readthedocs.io/en/latest/ "Read the docs"

Using docopt4s
------------------------------------------------------------------------------

You can import the library into your project from [maven central](https://central.sonatype.com/artifact/com.tinfoiled/docopt4s_2.13):

```xml
<dependency>
  <groupId>com.tinfoiled</groupId>
  <artifactId>docopt4s_2.13</artifactId>
  <version>0.0.5</version>
</dependency>
```

Building
------------------------------------------------------------------------------

```sh
# Build, format and run all tests
mvn spotless:apply clean verify

# Using the uber jar from the command line
alias docopt_go="java -jar $(find ~+ -name find ~+ -name docopt4s-testkit*.jar | sort | head -n1 | sort | head -n1)"
skrync_go --help
```

The project comes with an example CLI in the [testkit](testkit/) module, which you can run:

```sh
# From inside the root directory
__cwd=$PWD
function ExampleGo() {
  mvn -q -f $__cwd/testkit/pom.xml exec:java \
    -Dexec.classpathScope=test \
    -Dexec.mainClass=com.tinfoiled.docopt4s.testkit.example.ExampleGo -Dexec.args="$*"
}

# Now you can run the examples in the example CLI
ExampleGo naval_fate ship NOSTRADAMUS move 10 20 --speed=23
```
