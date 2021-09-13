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
* A [Blog](http://codesnip-app.blogspot.co.uk/).

There's also plenty of info available on how to compile CodeSnip from source - see below.

<sup>*</sup> These links take you to the most recent version of the documents -- they can change from release to release.

## Source Code

CodeSnip's source code is maintained in the [`delphidabbler/codesnip`](https://github.com/delphidabbler/codesnip) Git repository on GitHub†.

[Git Flow](http://nvie.com/posts/a-successful-git-branching-model/) methodology has been adopted, with the exception of some branches that have been used in abortive attempts to start work on CodeSnip 5.

The following branches existed at the time when CodeSnip v4.16.0 was released:

* `master`: Always reflects the state of the source code as of the latest release.‡
* `develop`: Main development branch. The head of this branch contains the latest development code.
* `pagoda`: An abortive attempt at developing CodeSnip 5. Work on this branch has halted. It does not follow GitFlow methodology. ***Do not use this branch: it may be pruned.***
* `pavilion`: Another attempt at working on CodeSnip 5. It branched off `pagoda` and it's future is uncertain. Again it does not follow GitFlow methodology.

New features and most bug fixes are worked on in `feature/xxxx` branches locally. They are merged into `develop` as they are completed and the branches are deleted.

**Note** that the default branch on GitHub is `develop` rather than `master`. This is because that's where all the work takes place. If you want to see the state of play at the last release make sure to switch to `master`.

> † Up to and including v4.13.1 the source code was kept in a Subversion repository on SourceForge. It was converted to Git in October 2015 and imported into GitHub. All releases from v3.0.0 are marked by tags in the form `version-x.x.x` where `x.x.x` is the version number. None of the Subversion branches made it through the conversion to Git, so to see a full history look at the old [SourceForge repository](https://sourceforge.net/p/codesnip/code/).

> ‡ All the converted Subversion code was committed to `master`, making it a copy of the old Subversion `trunk`. As such `master` contains various development commits along with numerous commits related to management of Subversion. After release 4.13.1, and the the first commit of this read-me file, `master` contains only commits relating to actual releases.

### Contributions

To contribute to the project please fork the repository on GitHub. Create a feature branch off the `develop` branch. Make your changes to the feature branch then submit a pull request via GitHub.

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
