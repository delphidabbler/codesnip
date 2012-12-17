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
with each installed Win32 version of Delphi from v2 to XE3 along with Free
Pascal.

Compilable Pascal units can be created that contain selected snippets.

Features new to CodeSnip 4 are listed in the "What's New In CodeSnip 4" topic
in the program's help file.


Installation
================================================================================

IMPORTANT NOTES:

1) CodeSnip requires Windows 2000 or later. It also requires MS Internet
   Explorer V6 or later, but IE 8, 9 or 10 are strongly recommended.

2) You will need administrator privileges to run the setup program. If you are
   using a non-admin user account on Windows 2000 or XP you should run setup as
   administrator. By default Windows Vista and Windows 7 will require an admin
   password if running as a standard user and setup will attempt to elevate the
   process. If UAC prompts are disabled you must run setup as administrator.

3) CodeSnip v4 will install alongside any v3 or earlier release that may already
   be installed. If you want to replace the earlier version simply uninstall it
   in the usual way. Uninstalling v3 or earlier after installing v4 will have no
   adverse affect on v4.

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
  stored in a "Database" sub-folder (see below).

+ An %AppData%\DelphiDabbler\CodeSnip.4 folder is also created. This is used to
  hold a file that stores per-user configuration data. A "UserDatabase" sub-
  folder is used to store any user defined snippets. These folders are created
  when CodeSnip is first run.

If you are updating to CodeSnip 4 from version 3 or earlier, CodeSnip will give
you the option of bringing forward your old settings and / or user defined
database. This happens the first time v4 is run for each user.


Uninstallation
================================================================================

CodeSnip can be uninstalled via "Programs and Features" (a.k.a. "Add/Remove
Programs") from the Windows Control Panel or by choosing "Uninstall
DelphiDabbler CodeSnip" from the program's start menu group.

Administrator privileges will be required to uninstall CodeSnip. Windows Vista
and Windows 7 with UAC prompts enabled will prompt for an admin password if
necessary.

The uninstall program will delete any local copy of the online Code Snippets
database but will leave any user defined database and configuration data intact.
To remove user defined databases and configuration data, delete the
%AppData%\DelphiDabbler\CodeSnip.4 directory and all its contents for each user
who ran CodeSnip.


Downloading & Updating the Code Snippets Database
================================================================================

The online DelphiDabbler Code Snippets database is not installed with the
program. However, if an older installation is present setup will give the option
to carry it forward if possible. When setup completes it checks for the presence
of the database and puts up a message if it is not present.

CodeSnip's start-up screen shows details of any installed databases. If there is
no copy of the online database a link is displayed that enables the database to
be installed.

From time to time you should check for updates to the online code snippets
database. This is done by selecting the program's "Database | Update From Web"
menu option.

You can get to know about updates by subscribing to the CodeSnip RSS feed (see
below).

Updates will apply to all users of the computer.


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

Delphi XE2 and XE3 may need to be configured to search for required units in the
correct namespaces. This is explaned in the Add/Edit Snippet Dialogue Box help
topic and in the FAQ at http://delphidabbler.com/url/codesnip-and-xe2.


Registration
================================================================================

Registration of CodeSnip is not required, but the author would be grateful if
you do register the program, just so he knows it is being used.

To register click the "Tools | Register CodeSnip" menu item and follow the
wizard. If this menu option is not displayed then the program has already been
registered.

On systems with multiple users, only one user needs to register. Once this is
done the program will show as registered regardless of which user is logged on.


Updating the Program
================================================================================

You can use the "Tools | Check For Program Updates" menu option to find out if
an updated version of CodeSnip is available. If so you will be directed to a
page on delphidabbler.com from where the updated program can be downloaded.

Beta and preview releases are not normally notified using this method. You will
need to keep an eye of the CodeSnip news feed to find out when these programs
are released. Use the "Help | CodeSnip News" menu option to see the news, or
subscribe to the feed in your news reader - see below for details.


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

The source code of the latest version of CodeSnip can be downloaded from
http://delphidabbler.com/url/codesnip-download.

The current development source tree and snapshots of all releases from v3.0
onwards can be viewed and downloaded from SourgeForge at
http://codesnip.svn.sourceforge.net/viewvc/codesnip/.

Subversion users can checkout code from
https://codesnip.svn.sourceforge.net/svnroot/codesnip.

You will usually checkout the "trunk" branch (development branch) or one of the
stable releases listed in the "tags" branch. There are also maintenances
branches for v3 and v4 releases in the "branches" branch.

Ready zipped source code archives of the current release and all earlier
versions back to v3.0.0 can be downloaded from the CodeSnip Files page on
SourceForge.net at https://sourceforge.net/projects/codesnip/files/

NOTE: Some SourceForge URLs are likely to change in due course. If you find the
given URLs don't work please let the author know using the contact page at
http://delphidabbler.com/contact so that you can be sent the revised URLS.

The original source code of v4 is released under the Mozilla Public license
v2.0 (see http://www.mozilla.org/MPL/) and other open source licenses. See the
file "License.html" in the "Docs" directory of the repository for full licensing
information.


Bugs
================================================================================

Please do report any bugs you find.

Bugs are recorded in Tracker on SourceForge. View the reported and fixed bugs
via http://delphidabbler.com/url/codesnip-bugs which redirects to SourceForge.
You can also access the bug tracker from CodeSnip by using the "Tools | Report
Bug Online" menu option then following the link that appears in the resulting
dialogue box.

If you wish to report a bug, please check the current reports on Tracker. If
your bug hasn't already been reported or fixed please add a report using the
"Add new" link on Tracker.

Please note that versions 1 and 2 of CodeSnip are no longer supported, so don't
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

If you want to suggest new features please use the feature request tracker at
http://delphidabbler.com/url/codesnip-featurereq which redirects to SourceForge.

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
