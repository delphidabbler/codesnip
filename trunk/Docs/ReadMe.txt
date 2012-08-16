================================================================================

DELPHIDABBLER CODESNIP v4 README

================================================================================


What is CodeSnip?
================================================================================

DelphiDabbler CodeSnip is a Delphi code snippets repository. It can download and
display snippets from the online DelphiDabbler CodeSnip database as well as
maintain a database of user-defined snippets.

It displays details of each snippet in the database and can test-compile them
with each installed Win32 version of Delphi from v2 to XE2 and Free Pascal.

Compilable Pascal units containing selected snippets can be created.


About this v4 beta release
================================================================================

Overview
--------

This is a beta release of CodeSnip v4. It is pretty much feature-complete, but
the look and feel may change by the final release.

If you accept the default install path, or specify a unique install path it will
install alongside any v3 release and will not affect the v3 release in any way.

If you have not created a user database using any v4 preview or beta releases
the program will offer to copy over any user defined snippets from a
pre-existing v3 and can preserve most if not all settings. Any v4 preview or
earlier beta release will be overwritten.

Any user defined snippets you edit will not be reflected in any v3 installation
and any settings you change won't affect the v3 install.

Nearly all the file formats have been changed from v3, so export and backup
files are not compatible with v3 and earlier. Don't expect any backups or
export files you create to work with v3. However files created by v3 can be read
by v4.

If you have been using one of the CodeSnip 4 previews this beta will attempt to
bring forward all settings from that. If you don't want to do that, follow the
instructions the installer displays when it detects that a preview release was
installed.

Copies of the online database are stored in a different location to v3, so
updating the database from v4 will not update the database used by v3 and vice
versa.

If any file formats change before the final release of CodeSnip 4, that program
WILL be able to read files created by this beta.

DO NOT try to return to a v4.0 preview release after installing this beta.

New Features
------------

New features of v4 to date worth trying out are:

* New "unit" and "class" snippet types that can include complete units and
  classes (and advanced records) in the database. Both can be test compiled
  and classes and advanced records can also be included in generated units. Read
  up on these in the help file - see the Snippet Kinds help topic.

* Snippets from both the main and user databases can now be duplicated. This is
  very useful if you have created a snippet and want to create another one that
  shares a lot of the source code, dependencies etc. Just duplicate the first
  one with a new name and edit it as required. It saves a lot of time. There's a
  second use where you can duplicate a snippet from the main database under the
  same name and make any tweaks you like to the new version: duplicate snippets
  are always editable.

* The new multi-tab display can show more than one snippet, category etc. in the
  main display. Control click items in the overview pane to display them in a
  new tab in the detail pane. Alternatively select View | New Tab (Ctrl+T) to
  display a new empty tab that can display selected items. Press Ctrl+F4 to
  close a tab.

* Results of test compilations (F9) now appear in a dialogue box.

* You can specify the paths to be searched by the Delphi compilers when looking
  for used units. This lets you compile snippets that use units other than those
  provided in the VCL and RTL. For example you can specify a path to the Indy
  components to compile them. Use the Compilers dialogue box to set the paths on
  a per-compiler basis.

* You can use Unicode source code.

* The structure of snippet pages in the details pane is now customisable:
  various page elements can be omitted and the order of elements can be changed.
  Each snippet type has its own page customisation. The colours used for Snippet
  and other headings can now be customised.

* Snippets can now have a "display name" that can contain any characters and
  does not need to be unique. Snippet descriptions can now be formatted and
  contain multiple paragraphs.

* The number of compilers that appear in the compiler results table in the
  display pane can now be determined by the user, so you can omit those that are
  of no interest to you.

* There is a new option on the Tools menu that checks availability of new
  versions of CodeSnip. Note though that this does not report new betas, only
  final releases.

For other features of v4 please read the change log for this release and all
earlier betas and previews at http://delphidabbler.com/software/codesnip/log?v=4

Bugs and Feature Requests
-------------------------

Please report bugs at http://www.delphidabbler.com/url/codesnip-bugs mentioning
the program version you are using (see about box).

Request new features at http://www.delphidabbler.com/url/codesnip-featurereq.
No more new features will be implemented in release 4.0, but I will consider
them for other v4.x point releases.


Installation
================================================================================

IMPORTANT NOTES:

1) CodeSnip requires Windows 2000 or later. It cannot be installed on Windows
   95, 98, Me, NT3.51 or NT4. It also requires MS Internet Explorer V6 or later,
   but IE 8, 9 or 10 are strongly recommended.

2) You will need administrator privileges to run the setup program. If you are
   using a non-admin user account on Windows 2000 or XP you should run setup as
   administrator. By default Windows Vista and Windows 7 will require an admin
   password if running as a standard user and setup will attempt to elevate the
   process. If UAC prompts are disabled you must run setup as administrator.

3) CodeSnip v4 will install alongside any v3 or earlier release that may already
   be installed. If you want to replace the earlier version simply uninstall it
   in the usual way. Uninstalling v3 will not delete any existing main database
   used by v4.

CodeSnip's installation program is named codesnip-setup-4.x.x.exe, where x.x
is the program's minor version number. The install program may be distributed in
a zip file. If this is the case then extract the install program.

*** NOTE. The beta versions of CodeSnip 4 have slightly different setup file
          names: codesnip-setup-4.0-beta.x where x is the beta version.

Close any running instance of CodeSnip, double click the install program then
follow the on-screen instructions.

The installer makes the following changes to your system:

+ The main program's executable file and documentation are installed into the
  chosen install folder (the %ProgramFiles%\DelphiDabbler\CodeSnip-4 folder by
  default).

+ Files required by the uninstaller are stored in the main installation's Uninst
  sub-folder.

+ The program's uninstall information is registered with the "Add / Remove
  Programs" (a.k.a "Programs and Features") control panel applet.

+ A program group may be created in the start menu (optional).

+ A %ProgramData%\DelphiDabbler\CodeSnip.4 folder is created. A configuration
  file is stored in the folder. Once the database is downloaded, it will be
  stored in a "Database" sub-folder (see below).

+ A %AppData%\DelphiDabbler\CodeSnip.4 folder is also created. This is used to
  hold a file that stores per-user configuration data. A "UserDatabase" sub-
  folder is used to store any user defined snippets. This happens only for the
  user installing the program. Other users will have different default folders
  and config files created when they first run CodeSnip.


Downloading the Database
================================================================================

The main CodeSnip database is not installed with the program. However, an older
installation may be present. Setup will try to use any older installation's
database if possible. When setup completes it checks for a database and puts up
a message if none is present.

When CodeSnip is first run it detects if there is no database and displays
message to that effect in its main window. A link is displayed that can be used
to download the database from the DelphiDabbler website. Once this is done the
required files are stored in the %ProgramData%\DelphiDabbler\CodeSnip.4\Database
folder.


Configuring CodeSnip to Work With Your Compilers
================================================================================

A feature of CodeSnip is the ability to test compile snippets in its main
database with any installed Windows 32 version of Delphi (i.e. Delphi 2 to 7 and
2005, 2006, 2007, 2009, 2010, XE and XE2) and FreePascal. User defined snippets
can also be test compiled providing some simple rules are followed.

When CodeSnip is first installed it knows nothing about the available compilers
and so test compilations cannot be performed. You must tell CodeSnip about the
available compilers by using the "Tools | Configure Compilers" menu option. The
resulting dialogue can automatically detect all installed versions of supported
Delphi compilers at the click of a button. Free Pascal, where installed, must be
set up manually.

Compilers that do not use English as their output language will need further
configuration. See the help file for information (look up "configure compilers
dialogue" in the help file index).

Each user can configure compilers differently.

Delphi XE2 may need to be configured to search the namespaces containing any
required units. This is explaned in the Add/Edit Snippet Dialogue Box help
topic. Alternatively see the FAQ at http://wiki.delphidabbler.com/index.php/FAQs/CodeSnipAppUsing#FAQ7


Registration
================================================================================

Registration of CodeSnip is not required, but the author would be grateful if
you do register the program, just so he knows it is being used.

To register click the "Tools | Register CodeSnip" menu item and follow the
wizard. If this menu option is not displayed then the program has already been
registered.

On systems with multiple users, only one user needs to register. Once this is
done the program will show as registered regardless of which user is logged on.


Uninstallation
================================================================================

CodeSnip can be uninstalled via "Add/Remove Programs" (a.k.a "Programs and
Features") from the Windows Control Panel or by choosing "Uninstall
DelphiDabbler CodeSnip" from the program's start menu group.

Administrator privileges will be required to uninstall CodeSnip. Windows Vista
and Windows 7 with UAC prompts enabled will prompt for an admin password if
necessary.

The uninstall program will delete the main database but will leave the user
defined database intact.


Updating the Database
================================================================================

From time to time you should check for updates to the online code snippets
database. This is done by selecting the program's "Database | Update From Web"
menu option.

You can get to know about updates by subscribing to the CodeSnip RSS feed (see
below).

Updates will apply to all users of the computer.


Updating the Program
================================================================================

You can use the "Tools | Check For Program Updates" menu option to find out if
an updated version of CodeSnip is available. If so you will be directed to a
page on delphidabbler.com from where the updated program can be downloaded.

Note that new betas and previews are not notified using this method. You will
need to keep an eye of the CodeSnip news feed to find out when these programs
are released. Use the "Help | CodeSnip News" menu option to see the news, or
subscribe to the feed in your news reader - see below for details.


Known Installation and Upgrading Issues
================================================================================

+ Any syntax highlighter customisation you have made will be lost if you are
  updating from any v2 or earlier release.

  You will need to redo any customisation using the Syntax Highlighter tab of
  the Preferences dialogue box.

+ If you are updating from v3.6.0 or earlier and have set up a password
  protected proxy server for internet access you will need to re-enter the
  password since it will have been lost. This is because the format for storing
  passwords changed at v3.6.1.

  To re-enter your proxy password use the Proxy Server Configuration dialogue
  box.

+ Your source code formatting preferences will have been lost if you are
  updating from v1.7.4 or earlier.

  You will need to reconfigure them using the Code Formatting tab of the
  Preferences dialogue box.

+ If you are updating from v1.8.11 or earlier and have registered CodeSnip your
  registration information will have been lost.

  You can check this by displaying the About dialogue box and checking the About
  The Program tab. If it displays a Register CodeSnip button the program is not
  registered. You can (re)register if you wish by clicking the button.


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

The source code of the latest version of CodeSnip can be downloaded from
http://www.delphidabbler.com/software/codesnip/download.

The current development source tree and snapshots of all releases from v3.0 can
be viewed and downloaded from SourgeForge at
http://codesnip.svn.sourceforge.net/viewvc/codesnip/.

Subversion users can checkout code from
https://codesnip.svn.sourceforge.net/svnroot/codesnip. You will usually checkout
the "trunk" branch (development branch) or one of the stable releases listed in
the "tags" branch. There is also a maintenance branch for the v3.x releases in
the "branches/3.x" branch.

Ready zipped source code archives of the current release and all earlier
versions back to v3.0.0 can be downloaded from the CodeSnip Files page on
SourceForge.net at https://sourceforge.net/projects/codesnip/files/

Available source code is released under the Mozilla Public license v1.1 or v2.0
(see http://www.mozilla.org/MPL/) and other open source licenses. See the file
SourceCodeLicenses.txt in the "Docs" directory of the repository for full
source code licensing information.


Bugs
================================================================================

Please do report any bugs you find.

Bugs are recorded in Tracker on SourceForge. View the reported and fixed bugs
via http://www.delphidabbler.com/url/codesnip-bugs which redirects to
SourceForge. You can also access the bug tracker from CodeSnip by using the
"Tools | Report Bug Online" menu option then following the link that appears in
the resulting dialogue box.

If you wish to report a bug, please check the current reports on Tracker AND
the historic list of fixed bugs at
http://www.delphidabbler.com/software/codesnip/bugs. If your bug hasn't already
been reported or fixed please add a report using the "Add new" link on Tracker.

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
http://www.delphidabbler.com/url/donate-cs - which redirects to a secure PayPal
page.


Feedback
================================================================================

If you want to suggest new features please use the feature request tracker at
http://www.delphidabbler.com/url/codesnip-featurereq which redirects to
SourceForge.

Any other comments can be sent using the contact page at
http://www.delphidabbler.com/contact


FAQs
================================================================================

There are Frequently Asked Questions pages for CodeSnip on the web, at
http://www.delphidabbler.com/url/codesnip-faq


Contribute to the Database
================================================================================

Please do contribute Pascal snippets to the on-line Code Snippets database.

You can submit routines from your user-defined snippets database using the
"Snippets | Submit Routines" menu option. Otherwise please send your code via
the DelphiDabbler contact page at http://www.delphidabbler.com/contact


Thanks
================================================================================

Thanks to:

+ David Mustard and Bill Miller for providing information that enabled me to add
  Delphi 2007 and Delphi 2009 support respectively to the program.

+ geoffsmith82 and an anonymous contributor for information about getting
  CodeSnip to work with Delphi XE2.

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
