# CodeSnip

A code bank designed with Pascal in mind.

* [Overview](#overview)
* [Installation](#installation)
* [Support](#support)
* [Source Code](#source-code)
* [Compiling](#compiling)
* [Contributing](#contributing)
* [Change Log](#change-log)
* [License](#license)
* [Bug Reports and Feature Requests](#bug-reports-and-feature-requests)

## Overview

CodeSnip is an open source code bank for storing and viewing your code snippets. While it can manage snippets in any source language, it is focused mainly on Pascal and Delphi code for which additional features are available.

The program is available in both standard and portable editions.

CodeSnip can import code from the DelphiDabbler [Code Snippets Database](https://github.com/delphidabbler/code-snippets) and the [SWAG Pascal Code Collection](https://github.com/delphidabbler/swag).

## Installation

The standard edition of CodeSnip is installed and removed using a Windows installer. Administrator privileges are required for installation.

The portable edition has no installer. Simply follow the instructions in the [read me file](https://raw.githubusercontent.com/delphidabbler/codesnip/master/Docs/ReadMe-portable.txt) that is included in the download.

The program _should_ run on Windows 2000, with Internet Explorer 6 or later, although XP and IE 8 and later are recommended. _But_ note that recent releases of CodeSnip have only been tested on Windows 10 & 11.

## Support

The following support is available to CodeSnip users:

* A comprehensive help file.
* A read-me file that discusses installation, configuration, updating and known issues. There are different versions of this file for each edition of CodeSnip: one for the [standard edition](https://raw.githubusercontent.com/delphidabbler/codesnip/master/Docs/ReadMe-standard.txt) and another for the [portable edition](https://raw.githubusercontent.com/delphidabbler/codesnip/master/Docs/ReadMe-portable.txt). [^1]
* The [Using CodeSnip FAQ](https://github.com/delphidabbler/codesnip-faq/blob/master/UsingCodeSnip.md).
* The [CodeSnip Blog](https://codesnip-app.blogspot.co.uk/).
* CodeSnip's own [Web Page](https://delphidabbler.com/software/codesnip).

There's also plenty of info available on how to compile CodeSnip from source - see below.

> [^1]: The linked read-me file is the most recent version. It can change from release to release. 

## Source Code

CodeSnip's source code is maintained in the [`delphidabbler/codesnip`](https://github.com/delphidabbler/codesnip) Git repository on GitHub. [^2]

The [Git Flow](https://nvie.com/posts/a-successful-git-branching-model/) methodology has been adopted for CodeSnip 4 development. The following branches are used:

* [`master`](https://github.com/delphidabbler/codesnip/tree/master): Always reflects the state of the source code as of the latest release. [^3]
* [`develop`](https://github.com/delphidabbler/codesnip/tree/develop): The head of this branch contains the latest v4 development code. Normal development of CodeSnip 4 takes place in feature branches that are then merged into `develop`.
* Feature branches, with names of the form `feature/<feature-name>`. Normally such branches are only used locally, but occasionally some feature branches may be pushed to the main repository.

You will find other branches in the repository. These are either experimental or abandoned. To find out more about them switch to the required branch and read its `README.md` file.

> [^2]: Up to and including v4.13.1 the source code was kept in a Subversion repository on SourceForge. It was converted to Git in October 2015 and imported into GitHub. All releases from v3.0.0 are marked by tags in the form `version-x.x.x` where `x.x.x` is the version number. None of the Subversion branches made it through the conversion to Git, so to see a full history look at the old [SourceForge repository](https://sourceforge.net/p/codesnip/code/).

> [^3]: All the converted Subversion code was committed to `master`, making it a copy of the old Subversion `trunk`. As such `master` contains various development commits along with numerous commits related to management of Subversion. After release 4.13.1, and the the first commit of this read-me file, `master` contains only commits relating to actual releases.

## Compiling

If you want to compile CodeSnip 4 from source code you will need the rather long-in-the-tooth Delphi XE. See [this FAQ](https://github.com/delphidabbler/codesnip-faq/blob/master/SourceCode.md#faq-11) to find out why.

Full instructions on setting up the build environment are provided in [`Build.html`](https://htmlpreview.github.io/?https://github.com/delphidabbler/codesnip/blob/develop/Build.html).

## Contributing

Please see [`CONTRIBUTING.md`](https://github.com/delphidabbler/codesnip/blob/develop/CONTRIBUTING.md) for details of how to contribute to the CodeSnip project.

⛔ Contributions to experimental and abandoned branches are not accepted.

## Change Log

The change log can be found in the file [`CHANGELOG.md`](https://github.com/delphidabbler/codesnip/blob/master/CHANGELOG.md). [^4]

> [^4]: CodeSnip v4.15.1 and earlier did not have `CHANGELOG.md`. Instead, some versions maintained a separate change log for each major version in the `Docs/ChangeLogs` directory.

## License

A summary of CodeSnip's license can be found in [`LICENSE.md`](https://github.com/delphidabbler/codesnip/blob/master/LICENSE.md) and the complete license text is in [`Docs\License.html`](https://htmlpreview.github.io/?https://github.com/delphidabbler/codesnip/blob/master/Docs/License.html). [^5]

> [^5]: The linked license files relate to the latest release. However, the license file names and content can change between releases, so if you need to see an older version, select the relevant `version-x.x.x` tag to find the appropriate file.

The [CodeSnip Compiling & Source Code FAQ](https://github.com/delphidabbler/codesnip-faq/blob/master/SourceCode.md) may be useful if you have any queries about re-using the CodeSnip source code in other projects.

## Bug Reports and Feature Requests

Report bugs and requests for new features are welcome. Please see the [Issues section of `CONTRIBUTING.md`](https://github.com/delphidabbler/codesnip/blob/develop/CONTRIBUTING.md#issues) for information about how to proceed.
