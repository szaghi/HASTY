<a name="top"></a>

# HASTY [![GitHub tag](https://img.shields.io/github/tag/szaghi/HASTY.svg)]() [![Join the chat at https://gitter.im/szaghi/HASTY](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/szaghi/HASTY?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[![License](https://img.shields.io/badge/license-GNU%20GeneraL%20Public%20License%20v3,%20GPLv3-blue.svg)]()
[![License](https://img.shields.io/badge/license-BSD2-red.svg)]()
[![License](https://img.shields.io/badge/license-BSD3-red.svg)]()
[![License](https://img.shields.io/badge/license-MIT-red.svg)]()

[![Status](https://img.shields.io/badge/status-stable-green.svg)]()
[![Build Status](https://travis-ci.org/szaghi/HASTY.svg?branch=master)](https://travis-ci.org/szaghi/HASTY)
[![Build Status](https://api.shippable.com/projects/57dabe9156a1350f001d8ffc/badge?branch=master)]()
[![Coverage Status](https://img.shields.io/codecov/c/github/szaghi/HASTY.svg)](http://codecov.io/github/szaghi/HASTY?branch=master)

### HASTY, HASh Table fortran container exploiting coarraY

> A KISS pure Fortran Library implementing a flexible hash table exploting coarrays for massively parralel support:

- HASTY is a pure Fortran (KISS) library implementing a flexible hash table;
- HASTY supports massively parallelism by means of coarray;
- HASTY is Fortran 2008+ standard compliant;
- HASTY is OOP designed;
- HASTY is a Free, Open Source Project.

#### Issues

[![GitHub issues](https://img.shields.io/github/issues/szaghi/HASTY.svg)]()
[![Ready in backlog](https://badge.waffle.io/szaghi/HASTY.png?label=ready&title=Ready)](https://waffle.io/szaghi/HASTY)
[![In Progress](https://badge.waffle.io/szaghi/HASTY.png?label=in%20progress&title=In%20Progress)](https://waffle.io/szaghi/HASTY)
[![Open bugs](https://badge.waffle.io/szaghi/HASTY.png?label=bug&title=Open%20Bugs)](https://waffle.io/szaghi/HASTY)

#### Compiler Support

[![Compiler](https://img.shields.io/badge/GNU-v6.1.1+-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/Intel-v16.1+-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/IBM%20XL-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/g95-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/NAG-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/PGI-not%20tested-yellow.svg)]()

---

[What is HASTY?](#what-is-hasty) | [Main features](#main-features) | [Copyrights](#copyrights) | [Download](#download) | [Compilation](#compilation) | [Documentation](#documentation) | [References](#references)

---

## What is HASTY?

To be written.

Go to [Top](#top)

## Main features

To be completed.

* [ ] Test Driven Developed (TDD);
* [ ] collaborative developed;
* [ ] well documented;
* [ ] free!

Any feature request is welcome.

Go to [Top](#top)

## Copyrights

HASTY is a Free and Open Source Software (FOSS), it is distributed under a **very permissive** multi-licensing system: selectable licenses are [GPLv3](http://www.gnu.org/licenses/gpl-3.0.html), [BSD2-Clause](http://opensource.org/licenses/BSD-2-Clause), [BSD3-Clause](http://opensource.org/licenses/BSD-3-Clause) and [MIT](http://opensource.org/licenses/MIT), feel free to select the license that best matches your workflow.

> Anyone is interest to use, to develop or to contribute to HASTY is welcome.

More details can be found on [wiki](https://github.com/szaghi/HASTY/wiki/Copyrights).

Go to [Top](#top)

## Download

HASTY home is at [https://github.com/szaghi/HASTY](https://github.com/szaghi/HASTY). To download all the source files you can:

+ clone recursively this repository: `git clone --recursive https://github.com/szaghi/HASTY`
+ download the latest master-branch archive at [https://github.com/szaghi/HASTY/archive/master.zip](https://github.com/szaghi/HASTY/archive/master.zip)
+ download a release archive at [https://github.com/szaghi/HASTY/releases](https://github.com/szaghi/HASTY/releases)

Go to [Top](#top)

## Compilation

HASTY is a modern Fortran project thus a modern Fortran compiler is need to compile the project.

The library is modular, namely it exploits Fortran modules. As a consequence, there is compilation-cascade hierarchy to build the library. To correctly build the library the following approaches are supported

+ [Build by means of FoBiS](#build-by-means-of-fobis): full support;
+ [Build by means of GNU Make](#build-by-means-of-gnu-make): to be implemented.
+ [Build by means of CMake](#build-by-means-of-cmake): to be implemented.

The FoBiS building support is the most complete, as it is the one used for the developing HASTY.

### Build by means of FoBiS

A `fobos` file is provided to build the library by means of the Fortran Building System [FoBiS](https://github.com/szaghi/FoBiS).

To be completed.

### Build by means of GNU Make

To be implemented.

### Build by means of CMake

To be implemented.

Go to [Top](#top)

---

## Documentation

Besides this README file the HASTY documentation is contained into its own [wiki](https://github.com/szaghi/HASTY/wiki). Detailed documentation of the API is contained into the [GitHub Pages](http://szaghi.github.io/HASTY/index.html) that can also be created locally by means of [ford tool](https://github.com/cmacmackin/ford).

To be completed.

Go to [Top](#top)

### References

To be written.

Go to [Top](#top)
