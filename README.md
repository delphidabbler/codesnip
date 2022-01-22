# CodeSnip

A code bank designed with Pascal in mind.

* [Overview](#overview)
* [Installation](#installation)
* [Support](#support)
* [Source Code](#source-code)
* [Change Log](#change-log)
* [License](#license)
* [Bug Reports and Features](#bug-reports-and-features)

## Overview

CodeSnip is an open source code bank for storing and viewing your code snippets. While it can manage snippets in any source language, it is focused mainly on Pascal and Delphi code for which additional features are available.

CodeSnip can import code from the DelphiDabbler [Code Snippets Database](https://github.com/delphidabbler/code-snippets).

The program is available in both standard and portable editions.

CodeSnip requires Windows 2000 or later and Internet Explorer 6 or later, although XP and IE 8 and later are preferred.

## Installation

The standard edition of CodeSnip is installed and removed using a standard Windows installer. Administrator privileges are required for installation.

The portable edition has no installer. Simply follow the instructions in the [read me file](https://github.com/delphidabbler/codesnip/blob/master/Docs/ReadMe.txt) that is included in the download zip file.

## Support

The following support is available CodeSnip users:

* A comprehensive help file.
* A [read-me file](https://raw.githubusercontent.com/delphidabbler/codesnip/master/Docs/ReadMe.txt)<sup> *</sup> that discusses installation, configuration, updating and known issues.
* A [Using CodeSnip FAQ](https://github.com/delphidabbler/codesnip-faq/blob/master/UsingCodeSnip.md).
* A [Blog](https://codesnip-app.blogspot.co.uk/).

There's also plenty of info available on how to compile CodeSnip from source - see below.

<sup>*</sup> This link takes you to the most recent version of the read-me file -- it can change from release to release.

## Source Code

CodeSnip's source code is maintained in the [`delphidabbler/codesnip`](https://github.com/delphidabbler/codesnip) Git repository on GitHub†.

The [Git Flow](https://nvie.com/posts/a-successful-git-branching-model/) methodology has been adopted, with the exception of some branches that have been used in various attempts to start work on CodeSnip 5.

The following branches existed as of 2022/01/01:

* `master`: Always reflects the state of the source code as of the latest release.‡
* `develop`: Main development branch. The head of this branch contains the latest v4 development code.
* `belvedere`: The latest attempt to develop CodeSnip 5. See the [Belvedere Readme file](https://github.com/delphidabbler/codesnip/blob/belvedere/README.md) for a full explanation.
* `pagoda`: An abortive attempt at developing CodeSnip 5. Work on this branch has halted. It does not follow GitFlow methodology. ***Do not use this branch: it may be pruned.***
* `pavilion`: Another attempt at working on CodeSnip 5. It branched off `pagoda` and work on it has halted. Again it does not follow GitFlow methodology. ***Do not use this branch: it may be pruned.***

New features and most bug fixes are worked on in `feature/xxxx` branches that are branched off `develop` locally. They are merged into `develop` as they are completed and the branches are deleted.

Note that the default branch on GitHub is `master`, which contains the state of the project as of the latest release. If you want to see the current state of play with new developments switch to `develop`.

> † Up to and including v4.13.1 the source code was kept in a Subversion repository on SourceForge. It was converted to Git in October 2015 and imported into GitHub. All releases from v3.0.0 are marked by tags in the form `version-x.x.x` where `x.x.x` is the version number. None of the Subversion branches made it through the conversion to Git, so to see a full history look at the old [SourceForge repository](https://sourceforge.net/p/codesnip/code/).

> ‡ All the converted Subversion code was committed to `master`, making it a copy of the old Subversion `trunk`. As such `master` contains various development commits along with numerous commits related to management of Subversion. After release 4.13.1, and the the first commit of this read-me file, `master` contains only commits relating to actual releases.

### Contributions

#### CodeSnip 4

To contribute to CodeSnip 4 development please fork the repository on GitHub. Create a feature branch off the `develop` branch. Make your changes to your feature branch then submit a pull request via GitHub.

> **Do not create branches off `master`, always branch from `develop`.**

#### CodeSnip 5 Belvedere

Proceed as for CodeSnip 4 except create your feature branch off the `belvedere` branch instead of `develop`.

> **Do not submit changes to the earlier `pagoda` or `pavilion` branches because they will not be accepted.**

### Compiling

`master` has a file in the root directory named [`Build.html`](https://htmlpreview.github.io/?https://github.com/delphidabbler/codesnip/blob/master/Build.html) that gives detailed information about how to compile the current release of CodeSnip.

There is also a [Compiling & Source Code FAQ](https://github.com/delphidabbler/codesnip-faq/blob/master/SourceCode.md).

## Change Log

The program's current change log can be found in the file `CHANGELOG.md` in the root of the `master` branch.

> Note that CodeSnip v4.15.1 and earlier did not have `CHANGELOG.md`. Instead, some versions maintained a separate change log for each major version in the `Docs/ChangeLogs` directory.

## License

The program's EULA, which gives full details of the license applying to the latest release, can be found in the file [`Docs\License.html`](https://htmlpreview.github.io/?https://github.com/delphidabbler/codesnip/blob/master/Docs/License.html) in the `master` branch. The license has changed between releases, so if you need to see an older one, select the appropriate `version-x.x.x` tag and read the older version of the file.

Most of the original code is made available under the [Mozilla Public License v2](https://www.mozilla.org/MPL/2.0/).

The [CodeSnip Compiling & Source Code FAQ](https://github.com/delphidabbler/codesnip-faq/blob/master/SourceCode.md) may be useful if you have any queries about re-using CodeSnip source in other projects.

## Bug Reports and Features

You can report bugs or request new features using the [Issues section](https://github.com/delphidabbler/codesnip/issues) of the CodeSnip GitHub project. You will need a GitHub account to do this.

Please do not report bugs unless you have checked whether the bug exists in the latest version of the program.
