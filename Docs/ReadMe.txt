================================================================================

DELPHIDABBLER CODESNIP v4 README

================================================================================


What is CodeSnip?
================================================================================

DelphiDabbler CodeSnip 4 is a code snippets repository targetted at the Pascal /
Delphi programming languages. It can download and display code snippets from the
online DelphiDabbler Code Snippets database as well as maintain a database of
user-defined snippets.

It displays details of each snippet in the database and can test-compile them
with each installed Win32 version of Delphi from Delphi 2 to Delphi 11
Alexandria and Free Pascal.

Compilable Pascal units can be created that contain selected snippets.

Features new to CodeSnip 4 are listed in the "What's New In CodeSnip 4" topic
in the program's help file.


CodeSnip Editions
================================================================================

There are two different editions of CodeSnip 4 available:

+ The standard edition, which is installed on the user's computer using an
  installer and which records its presence in the registry and stores data in
  the system's application and user data directories.

+ The portable edition that can be run from any writeable removable storage
  medium (e.g. a USB memory stick) and that makes no changes to the host
  computer. This edition has no installer and is simply copied onto the required
  medium.

You can run both the standard and portable editions together on the same
computer and even run them at the same time. However, each edition maintains its
own settings and keeps its own copies of the snippets databases. To share user
defined snippets you must export them from one edition and import into the
other. CodeSnip provides no mechanism for keeping them synchronised.


Installation
================================================================================

CodeSnip requires Windows 2000 or later. It also requires MS Internet Explorer 6
or later, but IE 8, 9 or 10 are strongly recommended.

Installing the Standard Edition
-------------------------------

You will need administrator privileges to run the setup program for the standard
edition. If you are using a non-admin user account on Windows 2000 or XP you
should run setup as administrator. By default Windows Vista to Windows 10 will
require an admin password if running as a standard user and setup will attempt
to elevate the process. If UAC prompts are disabled you must run setup as
administrator.

CodeSnip v4 will install alongside any v3 or earlier release that may already be
installed. If you want to replace the earlier version simply uninstall it in the
usual way. Uninstalling v3 or earlier after installing v4 will have no adverse
affect on v4.

CodeSnip's installation program is named codesnip-setup-4.x.x.exe, where x.x
is the program's minor version number. The install program may be distributed in
a zip file.

Close any running instance of CodeSnip, run the install program then follow the
on-screen instructions.

The installer makes the following changes to your system:

+ The main program's executable file and documentation are installed into the
  chosen install folder (%ProgramFiles%\DelphiDabbler\CodeSnip-4 by default).

+ Files required by the uninstaller are stored in the main installation's Uninst
  sub-folder.

+ The program's uninstall information is registered with the "Apps and Features"
  (a.k.a. "Programs and Features", a.k.a. "Add / Remove Programs") control panel
  applet.

+ A program group may be created in the start menu (optional).

+ A %ProgramData%\DelphiDabbler\CodeSnip.4 folder is created. A configuration
  file is stored in the folder. If the online database is installed, it will be
  copied to the "Database" sub-folder.

+ An %AppData%\DelphiDabbler\CodeSnip.4 folder is also created. This is used to
  hold a file that stores per-user configuration data and, sometimes, another
  file that records any favourite snippets. A "UserDatabase" sub-folder is used
  to store any user defined snippets. These folders are created when CodeSnip is
  first run.

  Users can move the user defined snippets data from the "UserDatabase"
  sub-folder to another location, in which case "UserDatabase" will not be
  present. You might want to do this to place the snippets data in a folder that
  will be backed up, e.g. a Dropbox or GoogleDrive sub-directory.

If you are updating to CodeSnip 4 from version 3 or earlier, CodeSnip will give
you the option of bringing forward your old settings and / or user defined
database. This happens the first time v4 is run for each user.

Installing the Portable Edition
-------------------------------

The portable edition of CodeSnip 4 is distributed in a zip file that contains
the program executable, the help file and various documentation files.

Install the program using the following steps:

1) Mount the storage medium on which you want to install CodeSnip.

2) Create a folder on the storage medium in which to copy the required files.

3) Copy the files CodeSnip-p.exe (the executable program) and CodeSnip.chm
   (the help file) into the folder you created.

   CodeSnip does not need the other files included in the zip file in order to
   run, but you may find them useful. Copy them if you wish.

Run the program by double clicking it. When it first runs it will create two
sub-directories within the folder where you installed the program. These will
be named AppData and UserData. Do not remove these directories or alter any of
the contents. CodeSnip uses them to store configuration data along with your
code snippets.

No files are written outside the folder where you copied the files and the
registry is not modified.

** WARNING: When updating an existing portable installation with a new version
of CodeSnip it is important that you do not change or delete the AppData and
UserData folders. If you do this you risk loosing your settings and/or database.


Uninstallation
================================================================================

Uninstalling the Standard Edition
---------------------------------

CodeSnip can be uninstalled via "Apps and Features" (a.k.a. "Programs and
Features", a.k.a. "Add / Remove Programs") from the Windows Control Panel or by
choosing "Uninstall DelphiDabbler CodeSnip" from the program's start menu group.

Administrator privileges will be required to uninstall CodeSnip. Windows Vista
to Windows 10 with UAC prompts enabled will prompt for an admin password if
necessary.

The uninstall program will delete any local copy of the online Code Snippets
database but will leave any user defined database, configuration data and
favourites intact. To remove user defined databases and configuration data,
delete the %AppData%\DelphiDabbler\CodeSnip.4 directory and all its contents for
each user who ran CodeSnip. If any user has moved the user database directory
those directories also need to be deleted.

Uninstalling the Portable Edition
---------------------------------

Simply delete the folder where you installed CodeSnip, with all its contents.

Be aware that any snippets you have created will be lost. If you want to keep
them for use in another CodeSnip installation either export them or back up the
user database before deleting the folder. See the help file for details of how
to do this.


Downloading & Updating the Code Snippets Database
================================================================================

The online DelphiDabbler Code Snippets database is not installed with the
program.

CodeSnip's start-up screen shows details of any installed databases. If there is
no copy of the online database a link is displayed that enables the database to
be installed. This link opens the "Install or Update DelphiDabbler Snippets
Database" wizard style dialogue box. The dialogue box explains how to download
and install the database.

You can download or update the database later by opening the same dialogue box
using the "Database | Install or Update DelphiDabbler Snippets Database" menu
option.

Standard Edition Only
---------------------

When installing the standard edition, the setup program will detect if an older
database installation is present and will give the option to carry it forward.
When setup completes it checks for the presence of the database and puts up a
message if it is not present.

Database updates will apply to all users of the computer the next time they
start CodeSnip.


Configuring CodeSnip to Work With Your Compilers
================================================================================

A feature of CodeSnip is its ability to test compile snippets with any installed
Windows 32 version of Delphi (from Delphi 2 to Delphi 11 Alexandria) and
FreePascal, providing some simple rules are followed.

When CodeSnip is first installed it knows nothing about the available compilers
and so test compilations cannot be performed. You must tell CodeSnip about the
available compilers by using the "Tools | Configure Compilers" menu option. The
resulting dialogue can automatically detect all installed versions of supported
Delphi compilers at the click of a button. Free Pascal, where installed, must be
set up manually. The Welcome page displays a list of compilers it has been
configured to work with.

Compilers that do not use English as their output language will need further
configuration. See the help file for information (look up "configure compilers
dialogue" in the help file index).

Each user can configure compilers differently.

Delphi XE2 and later may need to be configured to search for required units in
the correct namespaces. This is explained in the Add/Edit Snippet Dialogue Box
help topic and in the FAQ at
https://github.com/delphidabbler/codesnip-faq/blob/master/UsingCodeSnip.md#faq-1

Any type of snippet other than "freeform" can be test compiled.


Updating the Program
================================================================================

Updates are published on:

+ GitHub: https://github.com/delphidabbler/codesnip/releases

+ SourceForge: https://sourceforge.net/projects/codesnip/files/

News of new updates is published on the CodeSnip Blog:
https://codesnip-app.blogspot.com/.


Known Installation and Upgrading Issues
================================================================================

+ Any syntax highlighter customisation you have made will be lost if you are
  updating from any v2 or earlier.

  You will need to redo any customisation using the "Syntax Highlighter" page of
  the Preferences dialogue box displayed from the "Tools | Preferences" menu
  option.

+ Your source code formatting preferences will have been lost if you are
  updating from v1.7.4 or earlier.

  You will need to reconfigure them using the "Code Formatting" page of the
  Preferences dialogue box displayed from the "Tools | Preferences" menu option.

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

The snippets from the DelphiDabbler Code Snippets Database is open source. See
the "About The Database" tab of the About dialogue box for details of the
applicable license. (You can display the About box from the "Help" menu.)

The user is responsible to ensure that any code snippets managed by CodeSnip are
used in accordance with any applicable license.


Source Code
================================================================================

CodeSnip's source code is freely available. For details of how to obtain the
source see the FAQ at
https://github.com/delphidabbler/codesnip-faq/blob/master/SourceCode.md#faq-1

The standard and portable editions of CodeSnip share the same source code.

The original source code of v4 is released under the Mozilla Public license
v2.0 (see https://www.mozilla.org/MPL/) and other open source licenses. See the
file "License.html" in the "Docs" directory of the repository for full licensing
information.


Bugs
================================================================================

Please do report any bugs you find.

Bugs are recorded in tracker software. View the reported and fixed bugs via
https://github.com/delphidabbler/codesnip/issues (GitHub account required).

You can also access the bug tracker from CodeSnip by using the "Tools | Report
Bug Online" menu option then following the link that appears in the resulting
dialogue box.

If you wish to report a bug, please check the current reports on the bug
tracker. If your bug hasn't already been reported or fixed please add a report
using the "Add new" link on Tracker.

Please note that version 4.15.1 and earlier are no longer supported, so don't
report bugs for those versions. You should update the program first and only
report the bug if it is still present.


Feedback
================================================================================

If you want to suggest new features please use the feature request tracker
accessed from https://github.com/delphidabbler/codesnip/issues (GitHub account
required). Please check whether anyone else has requested something similar and
add a comment to their request if so.

Always check the latest version of CodeSnip before requesting a feature just in
case it has already been implemented!


FAQs
================================================================================

There are Frequently Asked Questions pages for CodeSnip on the web, at
https://github.com/delphidabbler/codesnip-faq/blob/master/README.md


Privacy
================================================================================

As of v4.16.0 CodeSnip no longer stores or transmits any personally identifiable
data.

Because of this change the privacy statement that used to be provided with the
program has been removed.

Do note though that CodeSnip can display web pages via your default web
browser, but only in response to user input. No guarantee is made about any
personal data collected by such web pages.


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
  menu option then select the "About the Database" tab). If the list is empty
  then updating the Code Snippets Database will download the details.


================================================================================
