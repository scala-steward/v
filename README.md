# v

![Build Status](https://img.shields.io/github/workflow/status/NthPortal/v/Continuous%20Integration?logo=github&style=for-the-badge)
[![Coverage Status](https://img.shields.io/coveralls/github/NthPortal/v/master?logo=coveralls&style=for-the-badge)](https://coveralls.io/github/NthPortal/v?branch=master)
[![Maven Central](https://img.shields.io/maven-central/v/lgbt.princess/v_2.13?logo=apache-maven&style=for-the-badge)](https://mvnrepository.com/artifact/lgbt.princess/v)
[![Versioning](https://img.shields.io/badge/versioning-semver%202.0.0-blue.svg?style=for-the-badge)](http://semver.org/spec/v2.0.0.html)
[![Docs](https://www.javadoc.io/badge2/lgbt.princess/v_2.13/docs.svg?color=blue&style=for-the-badge)](https://www.javadoc.io/doc/lgbt.princess/v_2.13)

A library for manipulating and comparing versions

## Add to Your sbt Build

**Scala 2.13**

```sbtshell
libraryDependencies += "lgbt.princess" %% "v-core"   % "0.5.0"  // the core library supporting basic version types
libraryDependencies += "lgbt.princess" %% "v-semver" % "0.5.0"  // the SemVer portion of the library (includes "v-core")
libraryDependencies += "lgbt.princess" %% "v"        % "0.5.0"  // all parts of the library
```
