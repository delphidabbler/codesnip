================================================================================

DELPHIDABBLER CODESNIP v4 PORTABLE EDITION README

================================================================================


What is CodeSnip?
================================================================================

DelphiDabbler CodeSnip 4 is a code snippets repository targetted at the Pascal /
Delphi programming languages. It can download and display code snippets from the
online DelphiDabbler Code Snippets database as well as maintain a database of
user-defined snippets.

It displays details of each snippet in the database and can test-compile them
with each installed Win32 version of Delphi from Delphi 2 to Delphi 12.x and
Free Pascal.

Compilable Pascal units can be created that contain selected snippets.


CodeSnip Editions
================================================================================

This document relates to the PORTABLE edition of CodeSnip. This edition can be
run from any writeable removable storage medium (e.g. a USB memory stick) or
from any folder on the computer's hard disk. It makes no changes to the host
computer.

There is also a standard edition of the program. This edition is installed on
the user's computer using an installer. It records its presence in the registry
and stores data in the system's application and user data directories. You can
get the standard edition from the same place you downloaded the this edition.

You can run both the standard and portable editions together on the same
computer and even run them at the same time. However, each edition maintains its
own settings and keeps its own copies of the snippets databases. To share user
defined snippets you must export them from one edition and import into the
other. CodeSnip provides no mechanism for keeping them synchronised.


Installation
================================================================================

CodeSnip requires Windows 2000 or later. It also requires MS Internet Explorer 6
or later, although IE 8, 9 or 10 are strongly recommended. Note that recent
releases have only been tested on Windows 11.

The portable edition of CodeSnip 4 is distributed in a zip file that contains
the program executable, the help file and various documentation files.

Install the program using the following steps:

1) Mount any storage medium on which you want to install CodeSnip.

2) Create a folder on the storage medium or on your computer's internal disk in
   which to copy the required files.

3) Copy the files CodeSnip-p.exe (the executable program) and CodeSnip.chm
   (the help file) into the folder you created.

   CodeSnip does not need the other files included in the zip file in order to
   run, but you may find them useful. Copy them if you wish.

Run the program by double clicking it. When it first runs it will create two
sub-directories within the folder where you installed the program. These will
be named AppData and UserData. Do not remove these directories or alter any of
the contents because CodeSnip uses them to store configuration data along with
your code snippets.

No files are written outside the folder where you copied the files and the
registry is not modified.

** WARNING: When updating an existing portable installation with a new version
of CodeSnip it is important that you do not change or delete the AppData and
UserData folders. If you do this you risk loosing your settings and/or database.


Uninstallation
================================================================================

Simply delete the folder where you installed the portable edition of CodeSnip
along with all its contents.

Be aware that any snippets you have created will be lost. If you want to keep
them for use in another CodeSnip installation, either export them or back up the
user database before deleting the folder. See the help file for details of how
to do this.


Downloading & Updating the Code Snippets Database
================================================================================

The online DelphiDabbler Code Snippets database is not installed with the
program.

CodeSnip's start-up screen shows details of any installed databases. If there is
no copy of the online database then a link is displayed that enables the
database to be installed. This link opens the "Install or Update DelphiDabbler
Snippets Database" wizard dialogue box. The dialogue box explains how to
download and install the database.

You can download or update the database later by opening the same dialogue box
using the "Database | Install or Update DelphiDabbler Snippets Database" menu
option.


Configuring CodeSnip to Work With Your Compilers
================================================================================

A feature of CodeSnip is its ability to test compile snippets with any installed
Windows 32 version of Delphi (from Delphi 2 to Delphi.x) and FreePascal,
providing some simple rules are followed.

When CodeSnip is first installed it knows nothing about the available compilers
and so test compilations cannot be performed. If any supported Delphi compiler
is detected when the program is first run you will be given the option of
registering it. This does not work for Free Pascal.

You can also tell CodeSnip about the available compilers by using the "Tools |
Configure Compilers" menu option. The resulting dialogue can automatically
detect all installed versions of supported Delphi compilers at the click of a
button. Free Pascal, where installed, must be set up manually. The Welcome page
displays a list of compilers it has been configured to work with.

Compilers that do not use English as their output language will need further
configuration. See the help file for information (look up "configure compilers
dialogue" in the help file index).

Each user can configure compilers differently.

Delphi XE2 and later may need to be configured to search for required units in
the correct namespaces. This is explained in the Add/Edit Snippet Dialogue Box
help topic and in the FAQ at
https://github.com/delphidabbler/codesnip-faq/blob/master/UsingCodeSnip.md#faq-7

Any type of snippet other than "freeform" can be test compiled.


Updating the Program
================================================================================

Updates are published on GitHub. See
https://github.com/delphidabbler/codesnip/releases

News of new updates is published on the CodeSnip Blog:
https://codesnip-app.blogspot.com/.


Known Installation and Upgrading Issues
================================================================================

+ If you have updated to CodeSnip v4.2.0 or later from any earlier v4 release,
  and then run the earlier version of the program again, its saved main window
  state, size, position and layout will have been lost and the program will
  display in its default size.

+ If you have updated to CodeSnip v4.3.0 or later from v4.2.x or earlier any -NS
  command line options you have specified on the "Switches" (aka "Command Line")
  tab of the Configure Compilers dialogue box for Delphi XE2 or later will be
  removed and equivalent entries will have been made on the "Namespaces" tab.

+ CodeSnip v4.16.0 and later cannot be registered. Any previous registration
  information may be lost.


License & Disclaimer
================================================================================

CodeSnip is made available under the terms of the Mozilla Public License v2.0.
The license is explained in full in the file License.html that is installed with
CodeSnip. A summary of the license can be viewed from the "Help | License" menu
option.

CodeSnip is supplied on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either
express or implied. See License.html for details.

The source code of any snippet managed by CodeSnip, whether from the
DelphiDabbler Code Snippets Database or the user database, is used WITHOUT
WARRANTY OF ANY KIND, either express or implied. The code is used entirely at
the user's own risk.

The snippets from the DelphiDabbler Code Snippets Database are open source. See
the "About The Database" tab of the About dialogue box for details of the
applicable license. (You can display the About box from the "Help" menu.)

The user is responsible to ensure that any code snippets managed by CodeSnip are
used in accordance with any applicable license.


Source Code
================================================================================

CodeSnip's source code is freely available. For details of how to obtain the
source see the FAQ at
https://github.com/delphidabbler/codesnip-faq/blob/master/SourceCode.md#faq-1

The portable edition of CodeSnip shares the same source code base with the
standard edition.

The original source code of v4 is released under the Mozilla Public license
v2.0 (see https://www.mozilla.org/MPL/) and other open source licenses. See the
file "License.html" in the "Docs" directory of the repository for full licensing
information.


Bugs & Feature Requests
================================================================================

Please do report any bugs you find. Suggestions for new features are also
welcomed.

Both bug reports and feature requests are made using the GitHub issue tracker
(GitHub account required). For details about using the issue tracker see
https://github.com/delphidabbler/codesnip/blob/master/CONTRIBUTING.md#issues.


FAQs
================================================================================

There are Frequently Asked Questions pages for CodeSnip on the web, at
https://github.com/delphidabbler/codesnip-faq/blob/master/README.md


Privacy
================================================================================

From v4.16.0 CodeSnip neither stores nor transmits any personally identifiable
data.

Do note though that CodeSnip can display web pages via your default web browser,
but only in response to user input. No guarantee is made about any personal data
collected by such web pages.


Thanks
================================================================================

Thanks to:

+ David Mustard and Bill Miller for providing information that enabled me to add
  Delphi 2007 and Delphi 2009 support, respectively, to the program.

+ geoffsmith82 and an anonymous contributor for information about getting
  CodeSnip to work with Delphi XE2.

+ The authors of the third party source code and images used by the program. See
  the program's about box or License.html for details.

+ Various contributors to the DelphiDabbler Code Snippets database. Names of
  contributors are listed in the program's About Box (use the "Help | About"
  menu option then select the "About the Database" tab). The list will be empty
  if the Code Snippets Database has not been installed.


================================================================================
