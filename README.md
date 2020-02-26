# CodeSnip

A code bank designed with Pascal in mind.

* [Overview](#overview)
* [Installation](#installation)
* [Support](#support)
* [Source Code](#source-code)
* [Change Log](#change-log)
* [CodeSnip 5 Development](#codesnip-5-development) -- **frozen**

## Overview

> **CodeSnip 4 Is No Longer Being Supported**.

> Bugs can no longer be reported and requests for new features in CodeSnip 4 are no longer being accepted.

> It is hoped to produce one final version that removes the program's dependency on the delphidabbler.com webserver that is due to close 15th June 2020, but no guarantees. Work started late Jan 2020.

CodeSnip is an open source code bank for storing and viewing your code snippets. While it can manage snippets in any source language, it is focussed mainly on Pascal and Delphi code for which additional features are available.

CodeSnip also provides offline access to the DelphiDabbler [Code Snippets Database](https://github.com/delphidabbler/code-snippets).

The program is available in both standard and portable editions.

CodeSnip requires Windows 2000 or later and Internet Explorer 6 or later, although XP and IE 8 and later are preferred.

## Installation

CodeSnip is installed and removed using a standard Windows installer. Administrator privileges are required for installation.

## Support

The following support is available for CodeSnip:

* A comprehensive help file.
* A [read-me file](https://raw.githubusercontent.com/delphidabbler/codesnip/master/Docs/ReadMe.txt)<sup> *</sup> that discusses installation, configuration, updating and known issues.
* An [FAQ](https://github.com/delphidabbler/codesnip-faq/blob/master/UsingCodeSnip.md)
* A [Blog](http://codesnip-app.blogspot.co.uk/).
* A [privacy statement](https://raw.githubusercontent.com/delphidabbler/codesnip/master/Docs/Privacy.txt)<sup>*</sup> that provides information about any personal information stored by the program and what info is transmitted over the net.

There's also plenty of info available on how to compile CodeSnip from source - see below.

<sup>*</sup> These links take you to the most recent version of the documents -- they can change from release to release.


## Source Code

Up to and including release 4.13.1 the project's source code was maintained in a Subversion repository on [SourceForge](https://sourceforge.net/p/codesnip/code/). The Subversion repo was converted to Git on 21 5 October 2015 and imported to GitHub. All releases from v3.0.0 are marked by tags in the form `version-x.x.x` where `x.x.x` is the version number.

> Note that any branches that were created and deleted in the Subversion repo are missing from the Git history. So, for full details of the the project's history from v3.0.0 to v4.13.1 please refer to the old Subversion repo on SourceForge.

Following tag `version-4.13.1` the [Git Flow](http://nvie.com/posts/a-successful-git-branching-model/) methodology was adopted. The exception is the `pagoda` branch that sits outside Git Flow. This branch was originally planned to be the CodeSnip v5 development branch (see [below](#codesnip-5-development) for details).

Up to and including release 4.13.1, `master` is simply a copy of the old Subversion `trunk` and, as such, contains various development commits along with numerous commits related to management of Subversion. After release 4.13.1, and the addition of this read-me file, `master` contains only commits relating to an actual release.

### Contributions

Contributions are no longer accepted for this repo. If you wish to take over development, please fork the repo and take it where you wish.

### Compiling

`master` and each branch will have a file in the root directory named `Build.html` that gives detailed information about how to compile CodeSnip.

There is also an [FAQ](https://github.com/delphidabbler/codesnip-faq) -- (see [Support](#support) above).

## Change Log

The program's current change log can be found in the file `Docs/ChangeLogs/ChangeLog-v4.txt` on the `master` branch.

## License

The program's EULA which gives full details of the license applying to the latest release can be found in the file `Docs\License.html` in the `master` branch. The license has changed between releases, so if you need to see an older one, select the appropriate `version-x.x.x` tag and read the older version of the file.

Most of the original code is made available under the [Mozilla Public License v2](https://www.mozilla.org/MPL/2.0/).

The [CodeSnip Compiling & Source Code FAQ](https://github.com/delphidabbler/codesnip-faq/blob/master/SourceCode.md) may be useful if you have any queries about re-using CodeSnip source in other projects.

## CodeSnip 5 Development

Following a false start back in 2013/4 in the `parsnip` branch, development of CodeSnip 5 restarted in the `pagoda` branch, which picked up where `parsnip` left off. (Don't ask where those names came from!)

Unfortunately `pagoda` has also been abandoned now. The branch has been left in place in case anyone else wants to continue development.

The idea behind v5 was to:

1. Generalise CodeSnip to be a code bank for several different languages instead of just Pascal, while still providing some additional support for test-compiling Pascal code.
2. Increase the focus on the user's own code while downplaying the importance of code downloaded from the DelphiDabbler [Code Snippets database](https://github.com/delphidabbler/code-snippets), like [SWAG](https://github.com/delphidabbler/swag) is at present.
