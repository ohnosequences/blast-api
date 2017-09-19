### Typesafe BLAST API

[![](https://travis-ci.org/ohnosequences/blast-api.svg)](https://travis-ci.org/ohnosequences/blast-api)
[![](https://img.shields.io/codacy/4565f7fae2d241f9a77a25a1bfbebe82.svg)](https://www.codacy.com/app/ohnosequences/fastarious)
[![](https://img.shields.io/github/release/ohnosequences/blast-api.svg)](https://github.com/ohnosequences/blast-api/releases/latest)
[![](https://img.shields.io/badge/license-AGPLv3-blue.svg)](https://tldrlegal.com/license/gnu-affero-general-public-license-v3-%28agpl-3.0%29)
[![](https://img.shields.io/badge/contact-gitter_chat-dd1054.svg)](https://gitter.im/ohnosequences/blast-api)


This library provides a typesafe API for the [BLAST](https://en.wikipedia.org/wiki/BLAST) bioinformatics tool based on the [ohnosequences/cosas](https://github.com/ohnosequences/cosas) library.

For documentation see the code annotations in [docs/](docs/src/main/scala/api) and the [API reference](http://ohnosequences.com/blast-api/docs/api/latest).


#### SBT dependency

```scala
resolvers += "Era7 maven releases" at "https://s3-eu-west-1.amazonaws.com/releases.era7.com"

libraryDependencies += "ohnosequences" %% "blast-api" % "<version>"
```
