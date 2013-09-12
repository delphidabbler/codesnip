================================================================================

DELPHIDABBLER CODESNIP README

================================================================================


What is CodeSnip?
================================================================================

DelphiDabbler CodeSnip is a Delphi code snippets repository. It can download and
display snippets from the online DelphiDabbler CodeSnip database as well as
maintain a database of user-defined snippets.

It displays details of each snippet in the database and can test-compile them
with each installed Win32 version of Delphi from v2 to XE5 and Free Pascal.

Compilable Pascal units containing selected snippets can be created.


Installation
================================================================================

IMPORTANT NOTES:

1) CodeSnip requires Windows 2000 or later. It cannot be installed on Windows
   95, 98, Me, NT3.51 or NT4. It also requires MS Internet Explorer V6 or later,
   but IE 7, 8 or 9 are strongly recommended.

2) You will need administrator privileges to run the setup program. If you are
   using a non-admin user account on Windows 2000 or XP you should run setup as
   administrator. By default Windows Vista and Windows 7 will require an admin
   password if running as a standard user and setup will attempt to elevate the
   process. If UAC prompts are disabled you must run setup as administrator.

CodeSnip's installation program is named codesnip-setup-3.x.x.exe, where x.x
is the program's minor version number. The install program may be distributed in
a zip file. If this is the case then extract the install program.

Close any running instance of CodeSnip, double click the install program then
follow the on-screen instructions.

The installer makes the following changes to your system:

+ The main program's executable file and documentation are installed into the
  chosen install folder (the %ProgramFiles%\DelphiDabbler\CodeSnip folder by
  default).

+ Files required by the uninstaller are stored in the main installation's Uninst
  sub-folder.

+ The program's uninstall information is registered with the "Add / Remove
  Programs" (a.k.a "Programs and Features") control panel applet.

+ A program group may be created in the start menu (optional).

+ A %ProgramData%\DelphiDabbler\CodeSnip folder is created. A configuration
  file is stored in the folder. Once the database is downloaded, it will be
  stored in a "Data" sub-folder (see below).

+ A %AppData%\DelphiDabbler\CodeSnip folder is also created. This is used to
  hold a file that stores per-user configuration data. A "UserData.3" sub-folder
  is used to store any user defined snippets.

+ Setup offers to copy any relevant files from installations earlier than v3.0
  into the correct locations for v3.


Downloading the Database
================================================================================

The CodeSnip database is not installed with the program. However, a previous
installation may be present. Setup will try to use an older version of the
database if present. When setup completes it checks for a database and puts
up a message if none is present.

When CodeSnip is first run it detects if there is no database and displays
message to that effect in its main window. A link is displayed that can be used
to download the database from the DelphiDabbler website. Once this is done the
required files are stored in the %ProgramData%\DelphiDabbler\CodeSnip\Data
folder.


Configuring CodeSnip to Work With Your Compilers
================================================================================

A feature of CodeSnip is the ability to test compile snippets in its database
with any version of Delphi that can compile 32 bit targets (i.e. Delphi 2 to
XE5) and FreePascal. User defined snippets can also be test compiled providing
some simple rules are followed.

When CodeSnip is first installed it knows nothing about the available compilers
and so test compilations can not be performed. You must tell CodeSnip about the
available compilers by using the "Tools | Configure Compilers" menu option. The
resulting dialog can automatically detect all installed versions of supported
Delphi compilers at the click of a button. Free Pascal, where installed, must be
set up manually.

Compilers that do not use English as their output language will need further
configuration. See the help file for information (look up "configure compilers
dialog" in the help file index).

Each user can configure compilers differently.


Registration
================================================================================

Registration of CodeSnip is not required, but the author would be grateful if
you do register the program, just so he knows it is being used.

To register click the "Tools | Register CodeSnip" menu item and follow the
wizard.

Only one user needs to register. Once this is done the program will show as
registered regardless of which user is logged on.


Uninstallation
================================================================================

CodeSnip can be uninstalled via "Add/Remove Programs" (a.k.a "Programs and
Features") from the Windows Control Panel or by choosing "Uninstall
DelphiDabbler CodeSnip" from the program's start menu group.

Administrator privileges will be required to uninstall CodeSnip. Windows Vista
and Windows 7 with UAC prompts enabled will prompt for an admin password if
necessary.


Updating the Database
================================================================================

From time to time you should check for updates to the CodeSnip database. This is
done by selecting the program's "Database | Update From Web" menu option.

It should be sufficient to check for updates about once per month. You can get
to know about updates by subscribing to the CodeSnip RSS feed (see below).

Updates will apply to all users of the computer.


Known Installation and Upgrading Issues
================================================================================

1) Users of v1.0.3 or earlier will loose any source code formatting preferences
   when upgrading to the latest version. If you experience this problem you need
   to reset your preferences via the "Tools | Preferences" menu option.

2) Syntax highlighting preferences will be lost when upgrading from any v1.x or
   v2.x release and the new v3 default highlighting style will be used. This
   can be changed from the Syntax Highlighter tab of the Preferences dialog box,
   accessed from the "Tools | Preferences" menu option.

3) Users who have configured CodeSnip to access the internet via a proxy server
   will loose any stored passwords required by the proxy server when updgrading
   from v3.6.0 and earlier to v3.6.1 and later. The installer will warn of this.
   Passwords have to be re-entered using the "Tools | Proxy Server" menu option.


RSS News Feed
================================================================================

You can get notified of all updates to the CodeSnip program and to the database
by subscribing to the Code Snippets RSS Feed at
http://www.delphidabbler.com/feeds/site-news-feed?id=codesnip.


License & Disclaimer
================================================================================

The executable program's End User License Agreement is displayed by the install
program and must be accepted in order to proceed with installation. A copy of
the license is installed with the program - see License.txt. The license can be
viewed from the "Help | License" menu option or from the About Box by clicking
the "End User License Agreement" link.

CodeSnip is supplied on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either
express or implied. See License.txt for details.

The source code contained in the database, or in any units or snippets generated
by this program, is made available on an "AS IS" basis, WITHOUT WARRANTY OF ANY
KIND, either express or implied. The code is used entirely at you own risk.


Source Code
================================================================================

CodeSnip's source code is freely available. For details of how to obtain the
source see the FAQ at
http://wiki.delphidabbler.com/index.php/FAQs/CodeSnipAppSource#FAQ1

Available source code is released under the Mozilla Public license (see
http://www.mozilla.org/MPL/MPL-1.1) and other open source licenses. See the file
SourceCodeLicenses.txt in the "Docs" directory of the repository for full
source code licensing information.


Bugs
================================================================================

Please do report any bugs you find.

Bugs are recorded in Tracker on SourceForge. View the reported and fixed bugs
via http://www.delphidabbler.com/url/codesnip-bugs which redirects to
SourceForge. You can also access the bug tracker from CodeSnip by using the
"Tools | Report Bug Online" menu option then following the link that appears in
the resulting dialog box.

If you wish to report a bug, please check the current reports on Tracker AND
the historic list of fixed bugs at
http://www.delphidabbler.com/software/codesnip/bugs.

If your bug hasn't been reported or fixed please add a report using the
"Add new" link on Tracker.


Make a Donation
================================================================================

CodeSnip is free to use and there is no requirement to pay anything for it. You
get a fully working version of the program whether you make a donation or not.

Having said that, it takes time and money to maintain CodeSnip and the online
database. So if you wish to make a contribution it will be most welcome.

Payment in pounds sterling can be made via this address -
http://www.delphidabbler.com/url/donate-cs which redirects to a secure PayPal
page.


Feedback
================================================================================

If you want to suggest new features please use the feature request tracker at
http://www.delphidabbler.com/url/codesnip-featurereq

Any other comments can be sent using the contact page at
http://www.delphidabbler.com/contact


FAQs
================================================================================

There are Frequently Asked Questions pages for CodeSnip on the web, at
http://www.delphidabbler.com/url/codesnip-faq


Contribute to the Database
================================================================================

Please do contribute procedures, functions and type or constant definitions to
the on-line Code Snippets database.

You can submit routines from your user-defined database using the "Database |
Submit Routines" menu option. Otherwise please send your code via the
DelphiDabbler contact page at http://www.delphidabbler.com/contact


Thanks
================================================================================

Thanks to:

+ David Mustard and Bill Miller for providing information that enabled me to add
  Delphi 2007 and Delphi 2009 support respectively to the program.

+ The authors of various pieces of source code and images used by the program.
  See the program's about box (use the "Help | About" menu option and see the
  "About the Program" tab) for details.

+ Various contributors to the Code Snippets database. Names of contributors are
  listed in the program's About Box (use the "Help | About" menu option then
  select the "About the Database" tab). If they don't appear then update the
  database.


================================================================================
$Rev$
$Date$
================================================================================
