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
with each installed Win32 version of Delphi from Delphi 2 to Delphi 10.1 Berlin
along with Free Pascal.

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
  medium (e.g. a USB memory stick) that makes no changes to the host computer.
  This edition has no installer and is simply copied onto the required medium.

You can run both the standard and portable editions together on the same
computer and even run them at the same time. However, each edition maintains its
own settings and keeps its own copies of the snippets databases. To share user
defined snippets you must export them from one edition and import into the
other. CodeSnip provides no mechanism for keeping them synchronised.


Installation
================================================================================

CodeSnip requires Windows 2000 or later. It also requires MS Internet Explorer
V6 or later, but IE 8, 9 or 10 are strongly recommended.

Installing the Standard Edition
-------------------------------

You will need administrator privileges to run the setup program for the standard
edition. If you are using a non-admin user account on Windows 2000 or XP you
should run setup as administrator. By default Windows Vista and Windows 7 will
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

+ The program's uninstall information is registered with the "Programs and
  Features" (a.k.a. "Add / Remove Programs") control panel applet.

+ A program group may be created in the start menu (optional).

+ A %ProgramData%\DelphiDabbler\CodeSnip.4 folder is created. A configuration
  file is stored in the folder. If the online database is downloaded, it will be
  stored in a "Database" sub-folder, unless you have changed the database
  location (see below).

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

2) Create a folder on the storage medium in which to "install" CodeSnip.

3) Copy the files CodeSnip-p.exe (the executable program) and CodeSnip.chm
   (the help file) into the folder you created.

   CodeSnip does not need the other files included in the zip file in order to
   run, but you may find them useful. Copy them if you wish.

Run the program by double clicking it. When it first runs it will created two
sub-directories within the folder where you installed the program. These will
be named AppData and UserData. Do not remove these directories or alter any of
the contents. CodeSnip uses them to store configuration data along with your
code snippets.

No files are written to the host computer and the registry is not modified.


Uninstallation
================================================================================

Uninstalling the Standard Edition
---------------------------------

CodeSnip can be uninstalled via "Programs and Features" (a.k.a. "Add/Remove
Programs") from the Windows Control Panel or by choosing "Uninstall
DelphiDabbler CodeSnip" from the program's start menu group.

Administrator privileges will be required to uninstall CodeSnip. Windows Vista
and Windows 7 with UAC prompts enabled will prompt for an admin password if
necessary.

The uninstall program will delete any local copy of the online Code Snippets
database but will leave any user defined database, configuration data and
favourites intact. To remove user defined databases and configuration data,
delete the %AppData%\DelphiDabbler\CodeSnip.4 directory and all its contents for
each user who ran CodeSnip. If any user has moved the user database directory
those directories also need to be deleted.

Uninstalling the Portable Edition
---------------------------------

Simply delete the folder where you installed CodeSnip and all its contents.

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
be installed.

By default CodeSnip checks online periodically to find out whether the database
has been updated. The checking process runs behind the scenes unless an update
is available when CodeSnip will display a notification window at the bottom
right of the main window. From the window you can display the "Update From Web"
dialogue which you can use to download the updated database.

You can also check for updates manually by using the program's "Database |
Update From Web" menu option. This displays the same "Update From Web" dialogue
box mentioned above. You can get to know about what updates are available by
subscribing to the CodeSnip RSS feed (see below).

If you don't want CodeSnip to check for database updates automatically, or if
you want to change the frequency with with it checks, you can do that from the
"Updates" tab of the "Preferences" dialogue box. This dialogue box is accessed
via the "Tools | Preferences" menu option.

NOTE: You may need to configure your firewall to permit CodeSnip to access the
internet otherwise the automatic updating may not work.

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
Windows 32 version of Delphi and FreePascal. User defined snippets can also be
test compiled providing some simple rules are followed.

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

Delphi XE2 to XE8 and Delphi 10.1 Berlin may need to be configured to search for
required units in the correct namespaces. This is explained in the Add/Edit
Snippet Dialogue Box help topic and in the FAQ at
http://delphidabbler.com/url/codesnip-and-xe2.


Registration
================================================================================

Registration of CodeSnip is not required, but the author would be grateful if
you do register the program, just so he knows it is being used.

To register click the "Tools | Register CodeSnip" menu item and follow the
wizard. If this menu option is not displayed then the program has already been
registered.

On systems with multiple users using the standard edition, only one user needs
to register. Once this is done the program will show as registered regardless of
which user is logged on.

When using the portable edition, uninstalling the program looses registration
data. If the portable edition is installed on more than one device then each
copy must be registered separately.


Updating the Program
================================================================================

CodeSnip automatically checks online for available program updates. If it finds
that an update is available it displays a notification window at the bottom
right of the main window. That window has a button you can click that opens your
default browser at a web page that downloads the new version of the program.

Once the updated version is downloaded you must close CodeSnip install the
program in the usual way.

You can change the frequency with which CodeSnip checks for updates, or turn off
the feature, from the "Updates" tab of the "Preferences" dialogue box. You can
display this dialogue box from the "Tools | Preferences" menu option.

You can also check for program updates manually at any time from the "Tools |
Check For Program Updates" menu option. Once again you will be directed to a web
page from where the updated program can be downloaded.

Neither of the above methods will detect beta and preview releases of the
program. To find out about those you should subscribe to the CodeSnip news feed
(see below). Alternatively you can view the latest news from the feed from the
"Help | CodeSnip News" menu option.

NOTE: You may need to configure your firewall to permit CodeSnip to access the
internet otherwise the automatic updating may not work.


Known Installation and Upgrading Issues
================================================================================

+ Any syntax highlighter customisation you have made will be lost if you are
  updating from any v2 or earlier release.

  You will need to redo any customisation using the Syntax Highlighter tab of
  the Preferences dialogue box displayed from the "Tools | Preferences" menu
  option.

+ If you are updating from v3.6.0 or earlier and have set up a password
  protected proxy server for internet access your password will have been lost.
  This is because the format for storing passwords changed at v3.6.1.

  To re-enter your proxy password use the Proxy Server Configuration dialogue
  box displayed from the "Tools | Proxy Server" menu option.

+ Your source code formatting preferences will have been lost if you are
  updating from v1.7.4 or earlier.

  You will need to reconfigure them using the Code Formatting tab of the
  Preferences dialogue box displayed from the "Tools | Preferences" menu option.

+ If you are updating from v1.8.11 or earlier and have registered CodeSnip your
  registration information will have been lost.

  You can check this by displaying the About dialogue box. If it displays a
  Register CodeSnip button the program is not registered. You can (re)register
  if you wish by clicking the button.

+ If you have updated to CodeSnip v4.2.0 or later from any earlier v4 release,
  and then run the earlier version of the program again, its saved main window
  state, size, position and layout will have been lost and the program will
  display in its default size.

+ If you have updated to CodeSnip v4.3.0 or later from v4.2.x or earlier any -NS
  command line options you have specified on the "Switches" (aka "Command Line")
  tab of the Configure Compilers dialogue box for Delphi XE2 or later will be
  removed and equivalent entries will have been made on the "Namespaces" tab.


RSS News Feed
================================================================================

You can get notified of all updates to the CodeSnip program and to the database
by subscribing to the Code Snippets RSS Feed at
http://delphidabbler.com/feeds/site-news-feed?id=codesnip.

The latest news from this feed can also be displayed from the program's "Help |
CodeSnip News" menu option.


License & Disclaimer
================================================================================

CodeSnip is made available under the terms of the Mozilla Public License v2.0.
The license is explained in full in the file License.html that is installed with
CodeSnip. A summary of the license can be viewed from the "Help | License" menu
option.

CodeSnip is supplied on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either
express or implied. See License.html for details.

The source code contained in the database, or in any units or snippets generated
by this program, is made available on an "AS IS" basis, WITHOUT WARRANTY OF ANY
KIND, either express or implied. The code is used entirely at you own risk.


Source Code
================================================================================

CodeSnip's source code is freely available. For details of how to obtain the
source see the FAQ at
http://wiki.delphidabbler.com/index.php/FAQs/CodeSnipAppSource#FAQ1

The standard and portable editions of CodeSnip share the same source code.

The original source code of v4 is released under the Mozilla Public license
v2.0 (see http://www.mozilla.org/MPL/) and other open source licenses. See the
file "License.html" in the "Docs" directory of the repository for full licensing
information.


Bugs
================================================================================

Please do report any bugs you find.

Bugs are recorded in tracker software. View the reported and fixed bugs via
https://github.com/delphidabbler/codesnip/issues

You can also access the bug tracker from CodeSnip by using the "Tools | Report
Bug Online" menu option then following the link that appears in the resulting
dialogue box.

If you wish to report a bug, please check the current reports on the bug
tracker. If your bug hasn't already been reported or fixed please add a report
using the "Add new" link on Tracker.

Please note that versions 1 to 3 of CodeSnip are no longer supported, so don't
report bugs for those versions. You should update the program first and only
report the bug if it is still present.


Make a Donation
================================================================================

CodeSnip is free to use and there is no requirement to pay anything for it. You
get a fully working version of the program whether you make a donation or not.

Having said that, it takes time and money to maintain CodeSnip and the online
database. So if you wish to make a contribution it will be most welcome.

Payment in pounds sterling can be made via this address -
http://delphidabbler.com/url/donate-cs - which redirects to a secure PayPal
page.


Feedback
================================================================================

If you want to suggest new features please use the feature request tracker
accessed from  http://delphidabbler.com/url/codesnip-featurereq.

Any other comments can be sent using the contact page at
http://delphidabbler.com/contact.


FAQs
================================================================================

There are Frequently Asked Questions pages for CodeSnip on the web, at
http://delphidabbler.com/url/codesnip-faq.


Contribute to the Database
================================================================================

Please do contribute Pascal snippets to the on-line Code Snippets database.

You can submit routines from your user-defined snippets database using the
"Snippets | Submit Routines" menu option. Otherwise please send your code via
the DelphiDabbler contact page at http://delphidabbler.com/contact.


Thanks
================================================================================

Thanks to:

+ David Mustard and Bill Miller for providing information that enabled me to add
  Delphi 2007 and Delphi 2009 support respectively to the program.

+ geoffsmith82 and an anonymous contributor for information about getting
  CodeSnip to work with Delphi XE2.

+ The authors of the third party source code and images used by the program. See
  the program's about box or License.html for details.

+ Various contributors to the Code Snippets database. Names of contributors are
  listed in the program's About Box (use the "Help | About" menu option then
  select the "About the Database" tab). If the list is empty then updating the
  Code Snippets Database will download the details.


================================================================================
$Rev$
$Date$
================================================================================
