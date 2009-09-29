
¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
DELPHIDABBLER CODESNIP README
________________________________________________________________________________


¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
What is CodeSnip?
________________________________________________________________________________

DelphiDabbler CodeSnip is a Delphi code snippets repository. It can download and
display snippets from the online DelphiDabbler CodeSnip database as well as
maintain a database of user-defined snippets.

It displays details of each snippet in the database and can test-compile them
with each installed version of Delphi from 2 to 7, Win32 personalities of Delphi
2005, 2006 & 2009, Delphi 2007 and Free Pascal.

Compilable Pascal units containing selected snippets can be created.

¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
Installation
________________________________________________________________________________

IMPORTANT NOTES: 

1) CodeSnip can only be run on the Windows NT platform. It cannot be installed
   on Windows 95, 98 or Me. It also requires MS Internet Explorer V6 or later.

2) You will need administrator privileges to run the setup program. If you are
   using a non-admin user account on Windows NT, 2000 or XP you should run setup
   as administrator. By default Windows Vista will require an admin password if
   running as a standard user and it will attempt to elevate the process. If UAC
   prompts are disabled you must run setup as administrator.

CodeSnip's installation program is named codesnip-setup-3.x.x.exe, where x.x
is the program's minor version number. The install program may be distributed in
a zip file. If this is the case then extract the install program from the zip
file.

Close any running instance of CodeSnip, double click the install program then
follow the on-screen instructions.

The installer makes the following changes to your system:

+ The main program's executable file and documentation are installed into the
  chosen install folder (\Progam Files\DelphiDabbler\CodeSnip by default).
  
+ Files required by the uninstaller are stored in the main installation's Uninst
  sub-folder.
  
+ The program's uninstall information is registered with the Add / Remove
  Programs control panel applet.
  
+ A program group may be created in the start menu (optional).

+ A CodeSnip folder is created inside the DelphiDabbler sub-folder of the
  common application data folder. A configuration file is stored in the
  folder. Once the database is downloaded, it will be stored in a Data
  subfolder (see below). 
  
+ Another CodeSnip folder is created in the DelphiDabbler sub-folder of the
  user's application data folder. This is used to hold a file that stores per-
  user configuration data. A UserData.3 sub folder of this folder is used to
  store any user defined snippets.
  
+ Setup offers to copy any relevant files from installations earlier than v3.0
  into the correct locations for v3.


¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
Downloading the Database
________________________________________________________________________________

The CodeSnip database is not installed with the program. However, a previous
installation may be present. Setup will try to use an older version of the
database if present. When setup completes it checks for a database and puts
up a message if no database is present.

When CodeSnip is first run it detects if there is no database and displays
message to that effect in its main window. A link is displayed that can be used
to download the database from the DelphiDabbler website. Once this is done the
required files are stored in the DelphiDabbler\CodeSnip\Data sub folder of the 
common application data folder.


¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
Configuring CodeSnip to Work With Your Compilers
________________________________________________________________________________

A feature of CodeSnip is the ability to test compile routines in its database
with any installed Windows 32 version of Delphi (i.e. Delphi 2 to 7 and 2005,
2006, 2007, 2009 and 2010) and FreePascal. User defined snippets can also be
test compiled providing some simple rules are followed.

When CodeSnip is first installed it knows nothing about these compilers and so
test compilations can not be performed. You must tell CodeSnip about the
available compilers by using the "Tools | Configure Compilers" menu option. The
resulting dialog can automatically detect all installed versions of supported
Delphi compilers at the click of a button. Free Pascal, where installed, must be
set up manually.

Compilers that do not use English as their output language will need further
configuration. See the help file for information (look up "configure compilers
dialog" in the help file index).

Each user can configure compilers differently.


¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
Registration
________________________________________________________________________________

Registration of CodeSnip is not required, but the author would be grateful if
you do register the program, just so he knows it is being used.

To register click the "Tools | Register CodeSnip" menu item and follow the
wizard.

Only one user needs to register. Once this is done the program will show as
registered regardless of which user is logged on.


¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
Uninstallation
________________________________________________________________________________

CodeSnip can be uninstalled via Add/Remove Programs from the Windows Control
Panel or by choosing "Uninstall DelphiDabbler CodeSnip" from the program's start
menu group.

Administrator privileges will be required to Uninstall CodeSnip. Windows Vista
with UAC prompts enabled will prompt for an admin password if necessary.


¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
Updating the Database
________________________________________________________________________________

From time to time you should check for updates to the CodeSnip database. This is
done by selecting the program's "Database | Update From Web" menu option. 

It should be sufficient to check for updates about once per month. Mailing list
members are notified of updates (see below).

Updates will apply to all users of the computer.


¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
Known Installation Issues
________________________________________________________________________________

1) Any source code formatting preferences you may have set when using CodeSnip
   v1.0.3 or earlier will be lost when upgrading to CodeSnip v1.1 or later. This
   is because the storage format for source code formatting changed at v1.1. If
   you experience this problem you need to reset your preferences via the
   Tools | Preferences menu option.
   
2) When upgrading from v1.x or v2.x to v3.0 and later any custom syntax
   highlighting is ignored and the new default highlighting is restored. This
   can be changed from the Syntax Highlighter tab of the Preferences dialog box,
   accessed from the Tools | Preferences menu option.


¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
Mailing List
________________________________________________________________________________

Information about updates to CodeSnip or to the database is sent by email to
members of the CodeSnip mailing list.

You can subscribe to the list in several ways:

+ By visiting http://www.delphidabbler.com/subsid/maillist?list=codesnip,
  entering your email address and clicking the subscribe button. 
+ By providing the relevant information when you register CodeSnip,
+ By selecting CodeSnip's "Tools | Join Mailing List" menu option and filling
  your details in the dialog box.

In all cases you will need to respond to a confirmation email to confirm your
membership of the list.

Unsubscribing is easy. Each mailing contains a link that can be visited to
unsubscribe from the mailing list.

Your privacy is protected. See the Privacy.txt file in the program's install
folder or the program's help file for details (look up "privacy statement" in
the help file index).


¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
License & Disclaimer
________________________________________________________________________________

The executable program's End User License Agreement is displayed by the install
program and must be accepted in order to proceed with installation. A copy of
the license is installed with the program - see License.txt. The license can be
viewed from the "Help | License" menu option or from the About Box by clicking
the "End User License Agreement" link.

CodeSnip is supplied on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either
express or implied. See "License.txt" for details.

The source code contained in the database, or in any units or snippets generated
by this program, is made available on an "AS IS" basis, WITHOUT WARRANTY OF ANY
KIND, either express or implied. The code is used entirely at you own risk.


¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
Source Code
________________________________________________________________________________

The source code of the latest version of CodeSnip can be downloaded from
http://www.delphidabbler.com/software/codesnip/download.

The current development source tree and all v3.x versions can be viewed and
downloaded from SourgeForce.net at
http://codesnip.svn.sourceforge.net/viewvc/codesnip/.

Subversion users can checkout code from
https://codesnip.svn.sourceforge.net/svnroot/codesnip. You will usually checkout
the "trunk" branch (development branch) or one of the stable releases listed in
the "tags" branch.

Ready zipped source code archives of the current release and earlier versions
can be downloaded from the CodeSnip Files page on SourceForge.net at:
https://sourceforge.net/projects/codesnip/files/

Source code older than one year is unlikely to be available.

Available source code is released under the Mozilla Public license (see
http://www.mozilla.org/MPL/) and other open source licenses.


¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
Bugs
________________________________________________________________________________

Please do report any bugs you find. 

From 28th September CodeSnip bugs are recorded in Tracker on SourceForge. View
the bug list here: https://sourceforge.net/tracker/?group_id=272192&atid=1157220

If you wish to report a bug, please check the current reports on Tracker AND
the historic list of fixed bugs at
http://www.delphidabbler.com/software/codesnip/bugs

If your bug hasn't been reported on fixed please add a report using the
"Add new" link on tracker.

Bugs can still be reported direct from CodeSnip, but this is no longer the
preferred option - I just wasn't getting enough info to fix them from the
reports. If you want to use this route, you'll find the bug report wizard on
the Tools menu.


¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
Make a Donation
________________________________________________________________________________

CodeSnip is free to use and there is no requirement to pay anything for it. You
get a fully working version of the program whether you make a donation or not.

Having said that, it takes time and money to maintain both CodeSnip and the
online database. So if you wish to make a contribution it will be most welcome.

Payment in pounds sterling can be made via this address - 
http://www.delphidabbler.com/url/donate-cs which redirects to a secure PayPal
page.


¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
Feedback
________________________________________________________________________________

If you want to suggest new features or comment on the program please contact the
author using the contact page at http://www.delphidabbler.com/contact or email
delphidabber [AT] yahoo [DOT] co [DOT] uk.


¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
Contribute to the Database
________________________________________________________________________________

Please do contribute procedures, functions and type or constant definitions to
the on-line Code Snippets database.

You can submit routines from your user-defined database using the Database |
Submit Routines menu option. Otherwise please send your code via the
DelphiDabbler contact page at http://www.delphidabbler.com/contact or by email
to delphidabber [AT] yahoo [DOT] co [DOT] uk.


¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
Thanks
________________________________________________________________________________

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


¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯