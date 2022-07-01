# Changelog

This is the change log for _DelphiDabbler CodeSnip_.

All notable changes to this project are documented in this file.

This change log begins with the first ever pre-release version of _CodeSnip_. Releases are listed in reverse version number order.

> Note that _CodeSnip_ v4 was developed in parallel with v3 for a while. As a consequence some v3 releases have later release dates than early v4 releases.

From v4.1.0 the version numbering has attempted to adhere to the principles of [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Release v4.20.1 of 01 July 2022

* Operating system detection code was updated to (a) fix bugs and (b) detect some Dev, Beta and Release Preview builds of Windows 11 22H2.
* Fixed copyright date in `Docs/License.html`.

## Release v4.20.0 of 15 May 2022

* Added an option to delete the user defined database.
* Fixed bug that enabled the user to attempt to move, or back up, the user database when it doesn't exist. These options are now disabled when there is no user database.
* Added facility to customise the size of font used in the details pane for all items except the source code font (which could already be modified separately). A new preference was added to the Display pane of the Preferences dialogue box to be used to set the font size.
* Rearranged the controls on the Preferences dialogue box's Display pane.
* Changed the default font used for the overview pane from a fixed value to the default size for the underlying operating system.
* Changed the description of "Delphi 11 Alexandria" to "Delphi 11.x Alexandria" to reflect the fact the Delphi 11 updates have different minor version numbers, but can't be installed alongside each other.
* Widened the compiler list box in the Configure Compilers dialogue box to accommodate the longer name used for Delphi 11.x compilers.
* Refactored some font handling code.
* Operating system detection code was updated to (a) fix some bugs and (b) detect some Dev channel builds of Windows 11.
* Bumped the version of the per-user config file to 18 following the addition of a new preference.
* Help file updated re the changes in this release.
* Documentation updated to reflect changes in this release.
* Updated `README.md` and `Build.html`

## Release v4.19.0 of 31 December 2021

* Improved user-friendliness of Preferences dialogue box:
  * Removed multi-line tab sets and replacing navigation pane on left hand side of window.
  * Hid warning on Printing preferences page that changes will not be made until after program is restarted, if and only if page is displayed from Print dialogue box.
  * Last preferences page displayed is now remembered and restored the next time the dialogue box is displayed.
* Added facility to customise size of font used in Overview pane's tree view. A new preference added to Display pane of Preferences dialogue box is used to set the font size.
* Fixed obscure bug in code that reads legacy ANSI Code Snippets Database files that was potentially using the incorrect ANSI code page.
* Updated help file re changes
* Fixed errors in custom installer dialogue boxes.
* Documentation corrected, expanded and updated, with some file format documentation having a major overhaul.
* Some tidying up:
  * Fixed some broken web links in source code comments and elsewhere.
  * Replaced `http` protocol in URLS with `https` wherever supported - mainly in source code comments & documentation.
  * Removed some orphaned files long since removed from project.
  * Added missing header comments to source file.
  * Updated copyright dates in files modified during year to include 2021.
  * Change log was overhauled to fix linting errors.
* Bumped version of per-user config file to 17 following addition of new preferences.
* Small amount of refactoring

## Release v4.18.1 of 29 November 2021

* Improved handling of control and whitespace characters in generated HTML: revised which characters were converted to HTML character attributes / entities.
* Fixed error in title of _Save Annotated Source_ dialogue box.
* Replaced use 3rd party `GIFImage` unit with similar `GIFImg` unit from Delphi XE VCL.
* Corrected help topic for _Dependencies_ dialogue box to describe _Save & Close_ button.
* Operating system detection code was updated to correctly detect Windows 11 and Windows 10 version 21H2.
* Some refactoring.
* Updated license document (`License.html`) following removal of dependency on GIFImage unit.

## Release v4.18.0 of 13 September 2021

* Added support for test compilation with, and detection of, Delphi 11 Alexandria.
* Updated various dialogue boxes to widen lists of compilers to accommodate length of new compiler name.
* Operating system detection code was updated to correctly detect Windows 10 version 20H2.
* Updated documentation re changes.
* Updated help file re changes.
* Minor documentation corrections.

## Release v4.17.2 of 12 September 2020

Hotfix release.

* Updated version of jQuery used by program easter egg from v1.8.0 to v1.12.4 to fix a known vulnerability in v1.8.0. Also updated jQuery Cycle Lite that depends on jQuery from v1.6 to 1.7.

## Release v4.17.1 of 31 July 2020

Hotfix release.

* Corrected "What's New" dialogue box content that is displayed _only_ when updating from v4.15.1 and earlier. The correction is to ensure the text makes sense when release 4.16.0 has been skipped. This change should have been made in v4.17.0.
* Removed a redundant resource.

## Release v4.17.0 of 31 July 2020

* Added support for test compilation with, and detection of, Delphi 10.2 Tokyo, Delphi 10.3 Rio and Delphi 10.4 Sydney compilers.
* Updated documentation re changes.
* Updated help file re changes.

## Release v4.16.0 of 31 May 2020

This is a significant update. It's purpose is to remove CodeSnip's dependencies on the delphidabbler.com website and associated web services. This was done because of the expected June 2020 closure or reduced functionality of delphidabbler.com. Some affected features were removed and others replaced with alternatives.

* Removed all dependencies on web services. The following changes were made as a consequence of this:
  * Replaced the option to update the main DelphiDabbler Code Snippets database from the web with an option to update it from locally stored data:
    * Replaced the _Update from Web_ dialogue box with a new wizard. Menu options on the _Snippets_ menu were renamed accordingly and the old tool bar button was removed.
    * Changed the database update code to use data that has been manually downloaded from the `delphidabbler/code-snippets` GitHub project.
    * Modified the database reading code to accept both the new Code Snippets database v2 format _and_ the legacy v1 format.
  * Replaced the option to import SWAG snippets from an on-line REST service with an option to import snippets from locally stored data.
    * Revised the SWAG import wizard re the changes to the import method.
    * Modified the SWAG import code to use data that has been manually downloaded from the `delphidabbler/swag` GitHub project.
  * The option to register the program was removed. No registration key is now generated or stored.
  * Replaced the option to read and display the CodeSnip RSS news feed with one to display the CodeSnip Blog.
  * Removed the menu option used to check for program updates.
  * Removed the background task that automatically checked for program and database updates.
  * Removed the option to submit snippets for addition to the DelphiDabbler Code Snippets database.
  * Removed support for a proxy web server - now unnecessary.
  * Removed support for the `--test-server` command line option that enabled use of a different server to test web services.
  * Updated install program so it no longer displays a page stating that CodeSnip will go automatically go on-line to check for updates.
* Removed references and links to delphidabbler.com from the program, the installer, the help file and documentation. Some references were deleted while others were replaced with alternatives, including:
  * Changed the URL of the FAQs to refer to the `codesnip-faq` GitHub project.
  * References to swag.delphidabbler.com were replaced with references to the `delphidabbler/swag` project on GitHub.
  * URLs that were redirected via a service on delphidabbler.com were replaced by hard coded URLs.
* The export file format was changed to exclude personal user information. The original format can still be read but any user information is ignored and discarded.
* Config file processing changes:
  * Removed support for reading or writing data relating to removed features.
  * When CodeSnip is first run after updating from an earlier version, any pre-existing config files are purged of any information that is no longer relevant.
  * The common config file is no longer used by the portable edition. Any pre-existing file is deleted the first time the portable edition is run.
  * The common and per-user config file versions were bumped to 7 and 16 respectively.
* Welcome page changes:
  * Removed the _Update Checks_ and _Donate_ sections and related links.
  * Removed links used to check for program and database updates.
  * Replaced the link used to display the news feed with one that displays the CodeSnip blog.
* Added a "What's New" type of dialogue box that can be selectively displayed when a new version of CodeSnip is run for the first time. v4.16.0 _always_ displays the dialogue box when first run.
* The operating system detection code was updated to correctly detect all Windows and Windows Server releases as of March 2020.
* Revised the _About_ dialogue box:
  * To display version and licensing information extracted from Code Snippets Database v2 meta data.
  * To remove credits for 3rd party code that is no longer used.
* The bug tracker dialogue boxes were updated re the change of issue tracker from SourceForge to GitHub.
* Removed redundant pages and controls from the _Preferences_ dialogue box.
* Removed the _Donate_ dialogue box and associated menu options.
* Revised and re-ordered some menu options.
* The program no longer generates and saves an application identifier key.
* Bugs fixed:
  * Corrected license information stored in the _Extra_ information section of imported SWAG packages.
  * Fixed a text formatting error in the SWAG import wizard ([issue #4](https://github.com/delphidabbler/codesnip/issues/4)).
  * Fixed broken help topic links in some dialogue boxes ([issue #3](https://github.com/delphidabbler/codesnip/issues/3))
  * Fixed a bug in the portable edition's startup processing of its config file.
  * Fixed the dialogue box displayed when updating from CodeSnip v3 or earlier to display an icon in the Windows task bar.
  * Corrected the license details included in comments of generated source code that includes snippets from the main database.
  * Corrected typos and errors in the UI.
* Some source code refactoring and clarifications.
* Removed redundant library code:
  * Encryption library.
  * Indy Internet components.
* Help file overhauled: new topics added, redundant topics removed and many errors corrected. Some restyling.
* Updated documentation, including:
  * Major changes to `./README.md` and `./Docs/ReadMe.txt`.
  * Merged all the major version specific changelogs into a single `./CHANGELOG.md` file and deleted the old files.
  * File format documentation was overhauled re changes introduced in this release.
  * Edited `./Docs/License.html` to remove license information and acknowledgements for 3rd party code that is no longer used.
  * Fixed errors in `./Build.html` concerning the source code repository and made some other minor changes.
  * Removed the privacy statement document, `./Docs/Privacy.txt` since CodeSnip no longer stores or transmits any personal information. (Also removed privacy help topic and menu item.)
  * Removed `./Docs/Design/WebServices.txt` file that described the web services used by CodeSnip.

## Release v4.15.1 of 22 September 2016

* Updated OS detection code to detect Windows 10 Version 1607 (Anniversary update) and all technical previews of Windows 2016 Server to date.

## Release v4.15.0 of 13 July 2016

* Added support for test compilation with, and detection of, Delphi 10.1 Berlin compiler.
* Tweaked size of compiler list in Configure Compilers dialogue box to accommodate length of Delphi 10.1 Berline compiler name!
* Updated documentation re changes.
* Updated help file re changes.

## Release v4.14.0 of 19 March 2016

* Changes to About Box's "Paths" tab:
  * Added new buttons to display the contents of the system and per-user config files.
  * Renamed tab as "Paths & Files".
* Implemented [SourceForge] feature request #83 to enable the name and port of any web service test server to be passed on command line by using the new "--test-server" command line option. This replaces hard-wired test server name & port that was activated using the now removed "-localhost" command line switch.
* Fixed [SourceForge] bug #96: "Some open / save dialogues too small". The height of customised dialogue boxes was increased.
* Updated operating system detection code to detect Windows 10 TH2.
* New photo of Sophie the dog added to the Easter egg slide show!
* Updated help file re About Box changes.

## Release v4.13.2 of 20 February 2016

* Updated and corrected hints displayed in main window.
* Changed some menu options and associated dialogue box captions.
* Tweaked some button captions in Select Snippets dialogue box.
* Updated help file re the menu and caption changes.
* Updated copyright date in program license as displayed in help, about box, installer and documentation.

## Release v4.13.1 of 29 September 2015

* Improved operating system detection to detect Windows 10.
* Modified program's manifest to declare it compatible with Windows 8 to 10.
* Code that determines which system font to use no longer depends on OS version but simply on font availability.
* Updated copyright date in program license as displayed in help, about box, installer and documentation.

## Release v4.13.0 of 5 September 2015

* Added support for test compilation with, and detection of, Delphi 10 Seattle compiler.
* Made some minor changes to method used to build required type library to remove dependency on the MS MIDL compiler, greatly simplifying build process.
* Updated documentation re changes.
* Updated help file re changes.

## Release v4.12.0 of 6 May 2015

* Added support for test compilation with, and detection of, Delphi XE8 compiler.
* Updated documentation re changes.
* Updated help file re changes and fixed some spelling mistakes.

## Release v4.11.1 of 26 October 2014

* Corrected an erroneous error message that is displayed when circular snippet references are detected in the snippets editor (no bug report filed).
* Corrected some spelling errors in UI.
* Some documentation corrections.

## Release v4.11.0 of 25 September 2014

* Changes re licensing of snippets from online Code Snippets Database under MIT license:
  * Generated code now carries a reference to the MIT license where relevant.
  * Submit Snippets wizard has a new page where user must confirm that any submitted snippets may be MIT licensed.
  * "About the Database" of the About Box now refers to the license.
  * Documentation and help file updated accordingly.
* Now uses version 6 of the Code Snippets Database update web service which supports the downloading of category and source code files larger than 32Kb.
* The undocumented -localhost command line option now causes CodeSnip to expect web services to be on localhost port 8080 instead of port 80. This change has no effect on normal operation and is used only in testing.
* Minor layout tweaks in in Display tab of Preferences dialogue box.
* Minor changes to help file and documentation.

## Release v4.10.0 of 12 September 2014

* Added support for test compilation with, and detection of, Delphi XE7 compiler.
* Updated documentation re changes.
* Updated help file re changes.

## Release v4.9.0 of 30 April 2014

* Added support for test compilation with, and detection of, Delphi XE6 compiler.
* Updated documentation re changes.
* Updated help file re changes.

## Release v4.8.7 of 06 March 2014

* Fixed automatic update checker so that it correctly records last update date. Fixes [SourceForge] bug #93.
* Updated to use v2 of the DelphiDabbler CodeSnip update web service when checking for availability of program updates.
* Minor corrections to help file.

## Release v4.8.6 of 28 February 2014

* Improved operating system detection to handle Windows 8.1.
* Added compatibility section to application manifest that declares the program has been tested with Windows Vista and Windows 7.

## Release v4.8.5 of 13 January 2014

* Fixed [SourceForge] bug #91: "Generated units won't compile on Delphi XE5". Compiler directives that are used to change compiler warnings now includes a conditionally compiled $LEGACYIFEND ON directive.
* Fixed potential bug when checking for the existence of files. It had been possible that a "sym-link" to a file could give misleading results.
* Updated program copyright date in license file, about box, help file and installer.

## Release v4.8.4 of 28 November 2013

* Improved user interface of SWAG Import Wizard.
* Renamed "Save Snippet" and "Copy Snippet" menu options to "Save Annotated Source" and "Copy Annotated Source". This fixes [SourceForge] bug #90: " Wrong caption on menu option for copying category to clipboard".
* Revised and corrected numerous main menu and pop-up menu hints.
* Updated help file re changes to menu options and SWAG Import Wizards.

## Release v4.8.3 of 06 November 2013

* Fixed registry access code so that the 64 bit view of the registry is used when CodeSnip runs on a Windows 64 bit operating system.
* Changed to avoid use of a deprecated API call when using the Windows Browse for Folder dialogue box.
* Updated documentation.

## Release v4.8.2 of 30 October 2013

* Modified Syntax Highlighter tab of Preferences dialogue box so that "vertical" fonts (whose names begin with "@") no longer appear in list of available fonts.
* Fixed potential bug in operating system detection code that may fail on Windows 2000.

## Release v4.8.1 of 18 September 2013

* Removed "File | Page Setup" menu option because some settings made there were being ignored when a file was printed. This is a fix for [SourceForge] bug #89: "Setup selections not being remembered".
* Updated help file re changes.

## Release v4.8.0 of 12 September 2013

* Added support for Delphi XE5 compiler.
* Fixed bug in code that reads or imports user database files written using CodeSnip 3 where Delphi XE4 compile results would be lost.
* Updated documentation re changes.
* Updated help file re changes.

## Release v4.7.2 of 27 August 2013

* Fixed [SourceForge] bug #88: "SWAG Import Wizard display bug" where duplicate snippets could be displayed on the "Ready to import" page in certain circumstances.

## Release v4.7.1 of 18 August 2013

* Fixed bug where right clicking a tab in the detail pane sometimes caused the contents of the pane to be temporarily blanked out while the context menu was displayed.
* The above fix also, as a side effect, fixed [SourceForge] bug #87: "Tab headings and contents don't match after a tab is closed."

## Release v4.7.0 of 31 July 2013

* Implemented [SourceForge] feature request #71: "Support importing of one or more snippets from the SWAG database":
  * Uses DelphiDabbler SWAG web service to get SWAG data.
  * New wizard to permit user to select required SWAG snippets. This is accessible from the new "Snippets | Import Snippets From SWAG" menu option.
  * Snippets are imported into a new "SWAG Imports" category.
* Implemented [SourceForge] feature request #80: "Enable detail pane tabs to be re-ordered".
* In detail pane source code and compiler table now display horizontal scroll bars if they do not fit within the width of the pane. This implements [SourceForge] feature requests #60 and #61.
* Minor changes to dialogue box that appears during long operations.
* Fixed [SourceForge] bug #86: "Snippets are sorted by snippet name in snippet table listings in detail pane".
* Fixed a few code errors that could have surfaced as bugs.
* Modified how HTML based detail pane display is generated and displayed.
* Some refactoring.
* Updated some 3rd party code to latest available versions.
* Updated help file re changes.
* Updated privacy statement.

## Release v4.6.4 of 24 July 2013

* Fix for IE 9 related browser control script bugs introduced in v4.6.3 when IE 10 bugs were fixed:
  * [SourceForge] Bug #84: "Script errors on startup"
  * [SourceForge] Bug #85: "Check For Updates link"

## Release v4.6.3 of 14 July 2013

* Further fix for IE 10 related [SourceForge] bug #75 "Floating point error in 4.4.1". Re-implemented method used to display content in main window's detail pane using the IE web browser control.

## Release v4.6.2 of 09 July 2013

* Tentative fix for [SourceForge] bug #83: "Error when the main form is shown" that has been reported on Windows 8. The fix is tentative because the bug hasn't been reproduced.

## Release v4.6.1 of 01 July 2013

* Provided fix for reported [SourceForge] bug #75: "Floating point error in 4.4.1" that apparently affects Windows 8, probably with IE 10 installed.
* Fixed unreported bug where IE 10 browser was being reported as IE 9.
* Fixed potential bug in code that processes class / advanced record snippet types ready for test compilation and inclusion in generated units.

## Release v4.6.0 of 02 June 2013

* Added new options to "Find Cross References" dialogue box to allow snippets that either cross reference or depend upon the selected snippet to be included in the search. This implements [SourceForge] feature request #30.
* Added a new "Select and Close" button to the Dependencies dialogue box that causes the snippets displayed on the current tab to be selected in the main display. This implements [SourceForge] feature request #77.
* The background colour of source code displayed in the main display can now be customised via a new option on the Display tab of the Preferences dialogue box. This implements [SourceForge] feature request #36.
* CodeSnip now compiled with Delphi XE.
* Per-user configuration file format changed to v15 which is not entirely compatible with previous versions of CodeSnip.
* Updated help file re changes.

## Release v4.5.1 of 15 May 2013

* Added progress bars or marquees to several database operations that can take a long time on slower storage devices, i.e.:
  * When local files are being updated after downloading an updated database in the Update From Web dialogue box. This fixes [SourceForge] bug #79:
  * When the local database is being saved.
  * When the local database is being backed up or restored.
  * When the local database is being moved to a new location.
* The user database can now be relocated to a network drive. This fixes [SourceForge] issue #81 "Move database to a network drive".
* Fixed [SourceForge] issue #80 "HTML output bug".
* Fixed minor alignment bug that occurred when displaying a wait dialogue box over the main window.
* Some refactoring.
* Updated help file re changes.

## Release v4.5.0 of 02 May 2013

* Added support for Delphi XE4 compiler. Implements [SourceForge] feature request #78.
* Updated documentation re changes.
* Updated help file re changes.

## Release v4.4.2 of 26 April 2013

* Fixed [SourceForge] bugs:
  * #76: An advanced record snippet with a method name that clashes with a directive is not test compiling correctly.
  * #77: Syntax highlighter highlights "contains", "requires" and "package" directives when used in method names.
  * #78: CodeSnip doesn't restore window in correct position when task bar on left or top of screen.

## Release v4.4.1 of 09 April 2013

* Fixed [SourceForge] bug #73: "Attempting to check for program updates returns a 404 'Not found' error" - this error happened only when using remote server, not localhost test server.

## Release v4.4.0 of 08 April 2013

* Implemented [SourceForge] feature request #75 "Check for updates on start-up":
  * CodeSnip checks for both program and Code Snippets Database updates in low priority background threads that run when the program is first started.
  * Update checking takes place at intervals between once per day and once per month.
  * A new "Updates" tab was added to the "Preferences" dialogue box where update frequencies can be chosen, or the auto-update feature switched off. Program and database update checking can be configured individually.
  * Updates are notified via a new slide-in, slide-out notification window that is displayed for a fixed amount of time or until closed by the user. The notification window contains a button that can be used to initiate the appropriate update. For database updates the "Update From Web" dialogue box is opened while for program updates a suitable download web page is displayed in the default browser.
  * Program checking is edition specific, i.e. the standard edition checks for standard edition updates and the portable edition behaves similarly.
* A new "Update Checks" section was added the welcome screen that gives information about the current auto-update settings and provides a link to change them. Some other text on the screen was tweaked.
* The "Check For Program Updates" dialogue box now opens the correct version and edition specific web page to download the latest version of CodeSnip instead of simply opening a general download page.
* A new CodeSnip specific program update web service on the codesnip.delphidabbler.com sub-domain is now used to get information about CodeSnip updates instead of the generic update service on delphidabbler.com.
* Additional usage information is now sent to the DelphiDabbler Code Snippets database update web service.
* Some refactoring and code clean-up.
* The installer may now display an information page that describes the new automatic update checking feature. This page is displayed only when updating from v4.3.0 or earlier to v4.4.0 (or later).
* Updated help file:
  * Updated and added help topics for all new features of the release.
  * Updated "What's New" topic re new features.
* Updated documentation, including privacy statement, with information about automatic update checking.
* Per-user configuration file format changed to v13 which is incompatible with previous versions of CodeSnip.

## Release v4.3.0 of 27 February 2013

* Implemented [SourceForge] feature request #40: "Add 'Namespaces' tab to Configure Compilers dialogue box". The new tab appears only for Delphi XE2 and later and obviates the need to manually create -NS commands for passing to the compilers. Suitable default namespaces are provided if none have been configured.
* Implemented [SourceForge] feature request #70: "Let user specify location of user database". This feature is accessed from the new "Move Use Database" option on the Database menu. NOTE: The feature is not available in the portable edition which is designed to keep the user database together with the program.
* Implemented [SourceForge] feature request #69: "Enable custom syntax highlighter styles to be saved". The Syntax Highlighter tab of the Preferences dialogue box has been modified to enable custom syntax highlighter attributes to be saved under a given name and existing named styles to be used or deleted.
* Changed name of "Delphi 2006" predefined syntax highlighter to "RAD Studio". This remains the default highlighter.
* A little refactoring.
* Enlarged Configure Compilers dialogue box.
* Updated help file:
  * Updated and added help topics re all new features in the release.
  * Updated "What's New" topic re new features.
  * Removed help topic for "Browse For Folders" dialogue box accessed from Search Paths tab of Configure Compilers dialogue.
* Per-user configuration file format changed to v12 which is not fully compatible with previous versions of CodeSnip.
* Updated documentation.

## Release v4.2.1 of 14 February 2013

* Bug fix: changed Favourites dialogue to display snippet display names instead of unique names. Fixes [SourceForge] bug #72.
* Updated program copyright date in About box.

## Release v4.2.0 of 07 February 2013

* Added support for "favourite" snippets. Implements [SourceForge] feature request #37:
  * Any displayed snippet can be flagged as a favourite via a menu option or toolbar button.
  * A new non-modal dialogue box can now be displayed alongside the CodeSnip window for easy selection and management of favourite snippets.
* Changes to Duplicate Snippets dialogue box:
  * Display name of duplicate snippet can be edited. Implements [SourceForge] feature request #64.
  * Snippets Editor can be opened immediately the Duplicate Snippet dialogue box closes to edit the duplicated snippet. Implements [SourceForge] feature request #65.
* Status bar changed: first panel now displays no category information, but displays both total number of snippets and number of snippets in each database.
* Fixed unreported bug in save dialogue boxes where overwrite permission requests could be displayed erroneously.
* Program closes gracefully if run on unsupported versions.
* Updated some 3rd party code to latest available versions.
* Some refactoring.
* Per-user configuration file format changed to v11 which is not fully compatible with older versions of CodeSnip.
* Updated help file re changes in Duplicate Snippets dialogue box and addition of support for Favourites.
* Updated documentation.

## Release v4.1.1 of 30 January 2013

* Fixed [SourceForge] bugs:
  * #68: Comments missing in unit / code generation for some types.
  * #70: Changing syntax highlighter font has no effect in main display.
  * #71: Option to select monochrome printing not working properly.
  * Unreported: Changing syntax highlighter font has no effect when printing or when copying text to clipboard as RTF (related to bug #70).
* Updated help file re syntax highlighter changes.

## Release v4.1.0 of 06 January 2013

* This is the first non-beta release to made available in both standard and portable editions, compiled from a common code base. The portable edition differs from the standard as follows (as per release v4.0.1 portable beta 1):
  * Executable file name is CodeSnip-p.exe.
  * Program caption identifies program as portable version.
  * Data directories are sub-directories of executable program directory.
  * Common file dialogue boxes default to program's working directory.
  * First run processing does not give the option to import old settings or existing user databases.
  * Different version information and program identifier.
  * There is no set up program.
* Changes to snippets editor:
  * Added context menus to cross-references and dependencies check box lists. Both have menu item to clear list. Dependencies list has item to view dependencies. Implements [SourceForge] feature request #3560960.
  * Deleted "View Dependencies" button now that its functionality is now on context menu.
  * Enlarged various controls on all except "Code" tab.
  * Units listed on "References" tab is now persistent. Units can be removed, defaults restored and selection cleared via a new context menu. Implements [SourceForge] feature request #3560962.
* New source code formatting option added to only use first paragraph of a snippet description as snippet comment in generated code. This is configured via "Code Formatting" tab of "Preferences" dialogue box and / or from relevant "Save" dialogue boxes. Implements [SourceForge] feature request #3560647.
* Changed mini-toolbar in overview pane to expand / collapse all overview tree view instead of just selected node. Implements [SourceForge] feature request #3560646.
* Reimplemented database search engine.
* Some external links modified that will seamlessly accommodate future changes in destination URLs.
* Changed some glyphs used in menus.
* Per-user configuration file format changed: bumped file version to 10.
* Some refactoring.
* Help file updated re changes & added privacy statement to TOC.
* Updated documentation:
  * Re changes to source code repository and bug / feature request trackers.
  * Re portable version.

## Release v4.0.2 of 17 December 2012

* Improvements to keyboard handling:
  * Fixed some keyboard focus bugs.
  * Fixed broken, missing and duplicate Alt-key short-cuts in several dialogue boxes.
  * Fixed broken keyboard access to list view in Code Generation tab of Preferences dialogue box.
* Corrected an incorrect font in Compilers dialogue box.
* Added title to "View Link" dialogue box displayed from mark-up editor.
* Corrected error in "Submit Code to the Database" task help topic.

## Release v4.0.1 portable edition, beta 1 of 12 December 2012

Internal CodeSnip version 4.0.1.213

* Modified version of Release v4.0.1 that can run from a writeable removable medium without writing files or registry on host computer. This implements [SourceForge] feature request #3577431.
* Changes that apply only to portable version:
  * Changed executable file name to CodeSnip-p.exe.
  * Program caption changed to identify as portable version.
  * Data directories changed to be sub-directories of executable program directory.
  * Changed common file dialogue boxes to working directory by default.
  * First run processing no longer gives option to import old settings or existing user databases.
  * Different version information and program identifier format.
  * No set up program.
* Code base modified to conditionally compile either portable or standard edition.
* Updated documentation, including privacy statement.

## Release v4.0.1 of 08 December 2012

Internal CodeSnip version 4.0.1.212

* Fixed [SourceForge] bug #3578652: "Pre-processor directive errors in main db ini files" by removing support for problematic directives.
* Rolling mouse over links in detail pane no longer displays a hint in the status bar. This change fixes [SourceForge] bug #3577407: Clicking detail pane snippet link leaves hint in status bar.
* Windows no longer scale automatically when screen DPI differs from that on design system. This fixes [SourceForge] bug #3591818: "Strange window behaviour in Windows 7" and [SourceForge] bug #3591820: "Incorrect font size used for some bold text".
* Update operating system detection code to detect Windows 8 & 2012 server.
* Some refactoring and some redundant code removed.
* Updated documentation.
* Updated help topic that describes main display.

## Release v4.0.0 of 12 October 2012

_Final v4 release._

See also changes from alpha, beta and release candidates below for details of changes since v3.9.3.

* New glyphs that describes level of testing applied to snippets from online Code Snippets Database now appear in top right of detail pane.
* Changed main window caption and task bar entry to include version number "4" after program name.
* Fixed bugs:
  * [SourceForge] bug #3572382 Automatic conversion of blank lines to paragraphs in REML mark-up editor gets confused if block level tags are already present.
  * Unreported: Controls in News dialogue box do not use correct font.
* A little refactoring.
* Updated help file:
  * Modified re recent changes
  * Added new "What's new" topic giving details of changes in v4. Implements [SourceForge] feature request.
  * Renamed "Welcome" page as "Overview".
* Updated documentation, including read-me file.

## Release v4.0 RC 3 of 18 September 2012

Internal CodeSnip version 3.999.3

* Fixed serious [SourceForge] bug #3568628: "CodeSnip faulting at startup after fresh install with no previous v3 installation".

## Release v4.0 RC 2 of 17 September 2012

Internal CodeSnip version 3.999.2

* Fixed serious [SourceForge] bug #3568515: Duplicating a snippet with a display name causes crash.
* Minor update to licensing information and about box credits.

## Release v4.0 RC 1 of 14 September 2012

Internal CodeSnip version 3.999.1

* UI changes:
  * Welcome page completely revised. Instead of a program overview the page now describes the state of the databases and available compilers and displays help links and a donation request. There are also some links to the about box, news and program updates.
  * Many changes to glyphs used in menus and toolbar.
  * Removed some images that relate to trade marks, i.e. PayPal Donate button and Delphi compiler icons.
  * No clicking noise is now issued by UI in response to user interaction with Details pane.
  * Links to external commands and to other snippets re-styled.
  * Revised and updated program's main icon.
* Fixed [SourceForge] bug #3566426: About Box Paths Page displays wrongly when themes not available.
* Added support for Delphi XE3 compiler. Implements [SourceForge] feature request #3566346.
* Completely new Easter Egg.
* Refactoring and internal code changes, including a revision of the "external" object that communicates with JavaScript in browser controls.
* Changed License:
  * EULA for executable code changed to Mozilla Public License v2.0.
  * Most original source code changed to Mozilla Public License v2.0 from v1.1.
  * Only an abbreviated version of the license is now displayed by the installer and in the help file.
  * License information has been consolidated into a new file: License.html.
* About box changed re new license and changes in required and voluntary acknowledgements and credits.
* Help file updated.
* Documentation updated.

## Release v4.0 beta 2 of 25 August 2012

Internal CodeSnip version 3.99.2

* [SourceForge] Bug fixes:
  * #3556620: Serious flaw in generating units containing class types when the classes contain method types other than procedure or function.
  * #3556713: Context menus are not displayed when pressing Alt+F10.
  * #3556715: Deleting a category then returning to it via the history list causes a GPF.
  * #3556718: Inconsistent context menus for edit controls in Snippets Editor.
  * #3557107: New snippets and categories are not added to the history list.
  * #3558649: Closing the Preferences dialogue always refreshes the main display even if the dialogue box was cancelled or if nothing was changed.
  * #3559156: "Previews" giving examples of the effect of changes made in the Preferences dialogue box sometimes disappear when the tab key is pressed.
  * #3559239: Snippet names and display names are used inconsistently in the UI.
  * #3559257: Compile Results displayed from the main menu can get out of sync with the actual compile results of snippets that have been edited since they were last compiled.
  * #3559265: Viewing dependencies for an unnamed snippet or a snippet not in the database causes a GPF.
  * #3559266: When include files are generated for a snippet that depends on a class type, the required class is not listed in the file's header comments.
  * #3560317: The caption of the Active Text preview dialogue box refers to "Extra" text when it is used to preview a snippet's description.
  * #3560521: The state of the Overview tree often doesn't restore correctly after a database update.
  * #3560958: Snippets are not sorted correctly (i.e. on display name) in the Overview pane.
  * #3561014: The current view in the Display pane is not cleared, even though all tabs are closed, when the database is re-loading.
  * #3561047: The Category view in the Overview pane sometimes appears fully expanded when it is expected to be fully collapsed.
  * Untracked: Removed "(v4 preview)" text that had been left in main window title bar.
  * Untracked: Minor accelerator key related problems in the Preferences dialogue box.
* Main UI changes:
  * The Detail pane tab-set now has a context menu that can be used to close the current tab or all but the current tab. The Close tab option is also added to current view's context menu.
  * Right-clicking a tab in the Overview or Detail pane now selects the tab.
  * Middle-clicking an item in the Overview pane now selects it. Previously middle clicks were ignored.
  * Many more parts of the main display and dialogue boxes now display display names for snippets rather than unique names.
  * Ctrl clicking snippet and category links in the Detail pane and History UI controls now opens the chosen item in a new tab. Implements [SourceForge] feature requests #3559377 and #3559378.
  * Different link styles are now used for the different types of link in the Detail pane. Implements [SourceForge] feature request #3559464.
  * Pressing Ctrl+Return on an active snippet or category link in the Details pane now opens the item in a new tab instead of the current tab. Pressing Return now opens the item in the current tab.
  * The Detail pane's context menu now displays more options when text selections and links are right clicked. Implements [SourceForge] feature request #3559375 with some minor differences.
  * New button added to the Display tab of the Preferences dialogue box that resets default snippet heading colours. Implements [SourceForge] feature request #3559140.
  * The "Types" unit is now displayed by default in the Snippets Editor Reference Tab's predefined units list.
* When a category is printed, any URLs in snippet descriptions are output and styled.
* Names of units referenced by snippets may now contain dots.
* Some refactoring and internal code changes, including a major reworking of the "first run" program configuration and a revision to the "external" object that communicates with JavaScript in browser controls.
* Help file updated:
  * Re UI changes.
  * With a description of the need to configure namespaces for Delphi XE2 for each unit referenced by the code it is test compiling - addresses [SourceForge] bug.
* Updated documentation.

## Release v4.0 beta 1 of 11 August 2012

Internal CodeSnip version 3.99.1

* New features:
  * Structure of snippet pages in details pane is now customisable: various page elements can be omitted and order of elements can be specified. Each snippet type has its own page customisation. Implements [SourceForge] feature request #3519456.
  * Snippets can now have a "display name" that can contain any characters and does not need to be unique. When provided the display name is displayed in preference to the normal name. Implements [SourceForge] feature request #3519460.
  * Snippet descriptions can now be formatted and contain multiple paragraphs. This implements [SourceForge] feature requests #3411890 and #3520405.
  * Snippets can now be configured so that their source code is not syntax highlighted. This change allows snippets in other languages not to be highlighted as if they are Pascal. Implements [SourceForge] feature request #3519935.
  * Colour of headings for snippets and categories from main and user databases are now user configurable. This implements [SourceForge] feature request #3519463.
  * User can now limit the number of compilers that appear in the compiler results table in the display pane. This is done via the Configure Compilers dialogue box. Implements [SourceForge] feature request #3519459.
  * New option on Tools menu that checks availability of new versions of CodeSnip.
* User interface changes:
  * "Test Compile" link removed from snippet display in details pane.
  * "Test Compile" dialogue box changed so that only the installed compilers that CodeSnip uses for test compilation are displayed, instead of all known compilers.
  * Welcome page revised.
* Changes to snippets editor:
  * New field on Code tab to enter optional snippet display name.
  * New check box on Code tab to specify if Pascal syntax highlighter is to be used for source code.
  * New tabbed "mark-up editor" lets user enter multi-paragraph snippet descriptions and extra information either as plain text or as REML mark-up.
  * Controls on Code tab re-ordered.
  * Extra Information tab revised with much larger edit control and deletion of explanatory text.
  * Editor enlarged.
* Changes to Preferences dialogue box:
  * New "Snippet Layout" tab added where composition and layout of snippet pages can be customised.
  * "General" preferences tab split into two: "Misc." that contains only measurement units and "Display" that contains display related options.
  * Added controls to "Display" tab to set main and user database heading colours.
  * Changes that affect appearance of content of details pane are now reflected in the display as soon as the Preferences dialogue box closes, rather than on program restart.
* Changes to REML mark-up handling:
  * Any REML text not embedded in block level tags is now automatically wrapped in `<p>`...`</p>` tags.
  * Nested REML block level tags are no longer allowed.
  * Changed handling of multiple spaces in REML code to be the same as in HTML.
  * Formatting of REML code improved when re-displayed.
* Bug fixes:
  * [SourceForge] bug #3536331 fixed: words at the end of some paragraphs in a snippet's extra information were not being found in "whole word only" searches.
  * Fixed unreported file parsing bug that occurred when loading a saved snippet selection from disk.
* Changed Delphi compiler detection so that compilers can be detected by examining current user registry key in addition to local machine registry key. This enables Delphis that were installed for a given user only to be detected.
* Improved error handling when reading and writing snippet selection files.
* User database and export file formats updated to v6.
* Per-user configuration file format changes: bumped file version to v9.
* Major changes to installer:
  * Now always brings forward any earlier common configuration files if needed.
  * Per user configuration files are now ignored: they are handled by main program (see below).
  * Only main database, not user database, is now imported from earlier versions on user request. User databases are now handled by main program (see below).
  * There is no longer an option to delete old databases or configuration files.
  * Updating from a v4 preview (alpha) release causes an extra page to be displayed that gives instructions relating to updating from preview to current release.
* Program now detects if it is running for first time since updating:
  * If this is first run since updating from v3 or earlier a "first run" wizard guides user through importing any old preferences or user databases.
  * For point v4.x point updates user configuration file is silently updated as necessary.
* Significant refactoring.
* Updated help file in line with changes and new features.
* Updated documentation, including minor changes to privacy statement and license.

## Release v4.0 alpha 3 (preview) of 18 June 2012

Internal CodeSnip version 3.98.3

* New features:
  * Compiler warnings can now be switched on as well as off in generated code.
  * Names and descriptions of snippets in a category can now be printed.
  * Text and compiler searches can now be nested so that the later search refines the earlier one.
  * Current selection (i.e. search result set) can be saved to disk and loaded again later.
* User interface changes:
  * Overview pane now displays buttons that can be used to collapse or expand non-empty section headings.
  * Ctrl+arrow keys can now be used to scroll overview pane tree view vertically and horizontally without changing selection in overview pane.
  * Main window is now refreshed whenever changes that affect it are made in the Preferences dialogue box.
  * Some main menu short-cut keys changed.
  * Dependencies dialogue box now has two tabs: the first displays the snippets required to compile the selected snippet while the second tab displays snippets that depend upon the selected snippet.
  * Code Generation tab of Preferences dialogue box updated to enable warnings to be switched on or off. In addition default warnings can be restored, list view columns can be sorted and Alt key short-cuts tweaked.
  * Code Import dialogue box improved: now sorts imported units in list view and scrolls to make renamed snippets visible.
  * Snippet selection and cross-reference search dialogues now report if existing search results will be overwritten.
  * Text and compiler search dialogues now ask if any current search results are to be refined.
  * Tree views in Snippet Selection, Snippets export and Snippets submission dialogue boxes can now be expanded and collapsed.
  * Appearance of message boxes tweaked.
  * Program tab of About box updated with credits for new third party code.
* Bug fixes:
  * Error in logic of code that generates program ID was fixed.
  * [SourceForge] Bug #2868708 fixed: edited snippets are no longer lost from manual snippet selections unless snippet names are changed.
  * [SourceForge] Bug #3534138 fixed: details pane display is now cleared when last tab is closed: previously content of last closed tab remained on screen.
* Info about user's OS and IE version is now sent to web server during online database updates.
* Some refactoring.
* Help file updated in line with changes and new features. Some US English spellings changed to UK English for consistency.
* Updated documentation, including:
  * Privacy statement updated re changes in data recorded via database update log-ons.
  * Licensing docs updated re introduction of some MPL 2.0 files.

## Release v4.0 alpha 2 (preview) of 21 April 2012

Internal CodeSnip version 3.98.2

* New features:
  * New "unit" snippet type that enables complete units to be stored in database and to be test compiled.
  * New "classes" snippet type that enables a single Object Pascal class or advanced record-with-methods to be stored in database, test compiled and included in generated units.
  * Snippets from both the user and main databases can now be duplicated. Duplicates are editable and are stored in the user database.
  * Online CodeSnip FAQs can now be displayed in the default browser from a new option on the "Help" menu.
* User interface changes:
  * New "Snippets" and "Categories" top level menus have been added. They are populated with items previously on the "Database" menu. "Snippets" menu also has new "Duplicate Snippet" item.
  * "Help" menu re-arranged: items from the former "On The Web" sub-menu are now placed directly on "Help" menu.
  * Numerous new and updated glyphs on toolbar, menu and in main display.
  * Minor tweaks to controls in the Code tab of the Snippets Editor.
  * Minor changes to the style of version info displayed on the splash screen.
* Bug fixes:
  * Fixed potential source of a bug in code that edits user-defined categories.
  * Fixed unreported minor bug in dialogue boxes that display tabbed page controls: clicking a tab did not always give it the keyboard focus.
  * Fixed [SourceForge] bug #3519784 where multi-line "type" or "constant" snippets that start on the same line as the type or const keyword were corrupted when included in units using the "comments after snippet header" comment style.
* Characters used to introduced switches on the command line were changed: '/' replaces '\'. '-' is still permitted.
* User and main database formats modified. User databases saved with this version may not be readable with release 4.0 alpha 1.
* Some refactoring.
* Help file updated in line with changes and some errors fixed.
* Updated documentation.

## Release v4.0 alpha 1 (preview) of 31 December 2011

Internal CodeSnip version 3.98.1

**Changes relate to v3.9.3:** CodeSnip 4 development branched off CodeSnip 3.9.3. CodeSnip v3 continued to be developed in parallel.

* User interface changes:
  * New multi-tab detail pane can now show more than one snippet, category etc.
  * Results of test compiles now appear in a dialogue box instead of in details pane.
  * New code import wizard for cleaner control over import process.
  * New "Compile" top level menu that groups all actions relating to test compilation.
  * Empty section headings can now be displayed in overview pane if required.
  * New display options relating to multi-tab display.
  * New view displayed instead of welcome screen when database updated.
  * Some additions and changes to main window navigation keys.
  * Main window and task bar captions changed.
  * Some dialogue boxes tweaked.
  * Compiler configuration dialogue box heavily revised to support default compiler paths.
  * "About" dialogue box paths tab display improved.
  * Compile error dialogue box display standardised.
  * Splash screen updated.
* Improved Delphi code syntax highlighter:
  * Recognises Delphi 2010 keywords
  * Correctly handles context sensitive directives within "property", "exports" and "external" statements.
  * Recognises `&` prefix that causes keywords to be treated as identifiers.
* Compiler search paths can now be specified for included units permitting non-VCL units to be used by snippets.
* Database:
  * Non-empty categories can no longer be deleted.
  * File format of both user-defined and main databases changed.
  * Database locations changed: updates to main database and edits to user database do not affect databases used by v3 and earlier.
  * Location and file format of both user defined and main databases changed.
  * Database now supports Unicode Delphi source code.
  * Unicode Delphi identifiers can now be used for snippet names.
  * Export and backup file formats updated: new formats are not backward compatible but older versions can still be imported.
  * Code submission service now supports Unicode source code.
  * Database updates now use v5 of delphidabbler.com web update service with revised checksum handling.
* Unicode support:
  * Program now fully supports Unicode internally.
  * Test units now use UTF-8 format if source code contains non-ANSI characters. ANSI format is used otherwise to permit compilation on older Delphi compilers.
  * Many export file formats now support Unicode and UTF8 formats. User may specify file types from Save dialogue box.
  * Configuration files are now in Unicode format.
  * Some Unicode support added to database (see above).
* Web service data handling code improved: now includes ability to send raw bytes and can detect and adapt to character encoding used in responses.
* Common and per-user configuration file names and locations changed. Bumped file version numbers to 6 and 8 respectively.
* Fixed some bugs:
  * Various Unicode and code page related problems in RTF code generation.
  * Memory leaks.
  * Version detection in backup file restoration.
* Cascading style sheet handling improved.
* Any errors in scripts run in browser control now trapped and reported as exceptions instead of via browser control's own error dialogue box.
* Revised external object that communicates with JavaScript in browser controls.
* Hyper-links used in snippets now support the https:// protocol.
* A default title now used in print spooler if none specified.
* Source code heavily refactored.
* Help file updated in line with changes.
* Installer:
  * Changed so that v3 and v4 installs can co-exist - default install locations are different and v4 does not overwrite v3.
  * Converts v3 configuration files to v4 Unicode format and copies to new locations. File version stamps are updated.
  * Installer is now compiled with Unicode version of Inno Setup instead of ANSI version.
  * Scripts updated and refactored.
* Updated documentation, including changes to privacy statement and new file format documentation.

## Release v3.13.2 of 31 October 2013

* Modified Syntax Highlighter tab of Preferences dialogue box so that "vertical" fonts (whose names begin with "@") no longer appear in list of available fonts.
* Fixed potential bug in operating system detection code that may fail on Windows 2000.
* Fixed registry access code so that the 64 bit view of the registry is used when CodeSnip runs on a Windows 64 bit operating system.

## Release v3.13.1 of 18 September 2013

* Removed File | Page Setup menu option because some settings made there were being ignored when a file was printed. This is a fix for [SourceForge] bug #89 "Setup selections not being remembered".
* Updated help file re changes.

## Release v3.13.0 of 12 September 2013

* Added support for Delphi XE5 compiler.
* Updated documentation re changes.
* Updated help file re changes.

## Release v3.12.1 of 01 July 2013

* Fixed [SourceForge] bug #82 "Fatal divide by zero exception on start-up" that affected all v3.x versions when the IE 10 browser was installed.
* Fixed unreported bug where IE 10 browser was being reported as IE 9.
* Updated all third party DelphiDabbler code to latest available versions.
* Updated documentation re changes.

## Release v3.12.0 of 02 May 2013

* Added support for Delphi XE4 compiler. Implements [SourceForge] feature request #78.
* Fixed [SourceForge] bug #78: CodeSnip doesn't restore window in correct position when task bar on left or top of screen.
* Updated documentation re changes.
* Updated help file re changes.

## Release v3.11.1 of 08 December 2012

* Fixed [SourceForge] bug #3578654: "Pre-processor directive errors in main db ini files" by removing support for problematic directives.
* Hints are no longer displayed in status bar when user rolls mouse over a link in the display pane. This fixes [SourceForge] bug #3577408: "Clicking detail pane snippet link leaves hint in status bar".
* Windows no longer scale automatically when screen DPI differs from that on design system. This fixes [SourceForge] bug #3591818: "Strange window behaviour in Windows 7" and [SourceForge] bug #3591820: "Incorrect font size used for some bold text".
* Updated operating system detection code to detect Windows 8 & 2012 server.
* Updated documentation

## Release v3.11.0 of 17 September 2012

* Added support for Delphi XE3 compiler. Implements [SourceForge] feature request #3566345.
* [SourceForge] Bug fixes:
  * #3561713: The Category view in the Overview pane sometimes appears fully expanded when it is expected to be fully collapsed.
  * #3566430: About Box Paths Page displays wrongly when themes not available.
* Updated documentation re changes.
* Updated help file re changes.

## Release v3.10.5 of 21 August 2012

* Fixed [SourceForge] bugs:
  * #3559257: Compile Results accessed from menu can get out of sync.
  * #3559156: "Previews" disappearing in Preferences dialogue box

## Release v3.10.4 of 16 August 2012

* Added support for displaying pop-up menus over appropriate control when Alt+F10 is pressed. Fixes [SourceForge] bug #3556713.
* Changes to snippets editor:
  * Added missing edit context menu to "add unit" edit control on References tab. Fixes [SourceForge] bug #3556718 as it relates to v3.
  * Predefined list of units in Units list on References tab now includes the "Types" unit.
  * Referenced unit names may now contain dots.
  * Snippets Editor help topic now explains need to configure Delphi XE2 compiler to search namespaces containing referenced units. This provides a solution to [SourceForge] bug #3536531.

## Release v3.10.3 of 25 July 2012

* Changed so that Delphi compilers can be detected by examining current user registry key in addition to local machine registry key. This enables Delphis that were installed for a given user only to be detected.
* Fixed bug in Compiler tab of Configure Compilers dialogue box that failed to flag selected compiler as unavailable after button was pressed.

## Release v3.10.2 of 19 June 2012

* Fixed [SourceForge] bug #3536331 where some distinct words in a snippet's Extra text where not being found in text searches.
* Info about user's OS and IE version is now sent to web server during online database updates.
* Updated privacy statement re changes in information sent by update web service.

## Release v3.10.1 of 20 April 2012

* Fixed [SourceForge] bug #3519784 where multi-line type or constant snippets that start on same line as type or const keyword were corrupted when included in units using the "comments after snippet header" comment style.
* Also fixed potential source of a bug in code that edits user-defined categories.

## Release v3.10.0 of 17 April 2012

* Added new Help | On The Web | FAQs menu option to display CodeSnip FAQs in default browser.
* Fixed unreported minor bug in dialogue boxes that display tabbed page controls: clicking a tab did not always give it the keyboard focus.
* Characters used to introduced switches on command line were changed: '/' replaces non-standard '\'. '-' is still permitted.
* Updated help file: added topic for new menu option and minor change to FAQ help topic.

## Release v3.9.3 of 23 November 2011

**Note:** Development of CodeSnip 4 branched off this release.

* Fixed some bugs in main window:
  * Toolbar was truncated when window is too narrow to display it all. It now wraps.
  * Treeview state in Overview pane was not restoring correctly after navigating away from and then returning to a tab.
  * Pressing Ctrl+Tab or Shift+Ctrl+Tab did not necessarily change the tab in the expected tab set in either the Overview or Detail panes.
* Fixed a broken URL in about box.
* Bumped installer program helper build number re Delphi 2010 compilation (should have been done at v3.5.1).

## Release v3.9.2 of 28 October 2011

* Fixed [SourceForge] bug #3427741 where details pane tabs didn't change in response to key presses.
* Fixed [SourceForge] bug #3427866 where selection in overview was not always same as item displayed in details pane.
* Fixed [SourceForge] bug #3427889 where there was the possibility of a GPF in overview pane.

## Release v3.9.1 of 18 September 2011

* Fixed [SourceForge] bug #3369422 in Pascal highlighter that was causing an assertion failure when parsing malformed Pascal general format floating point numbers.

## Release v3.9.0 of 07 September 2011

* Added support for Delphi XE2 Windows 32 bit compiler:
  * Can now test compile and display results with Delphi XE2 32 bit.
  * Delphi XE2 compiler version 23.0 has been added to the drop down menu in the Code Generation tab of the preference dialogue box.
  * Updated help file re Delphi XE2 support.
  * Updated documentation.
* Limited user name edit control to 48 chars in registration wizard because this is limit in online registration database.

## Release v3.8.11 of 02 July 2011

* Fixed display problem in about box and compiler error dialogue boxes on systems using Internet Explorer v9 web browser control. This fixes [SourceForge] issue #3349186.
* Updated read-me file re support for IE9 browser control.

## Release v3.8.10 of 20 May 2011

* Reverted checked tree views and list boxes to standard Windows behaviour. Clicking item text no longer toggles associated check boxes. This behaviour was more problematic then helpful.
* Updated documentation, including new info about CodeSnip FAQ.
* Added FAQs topic and TOC entry to help file that links to online FAQ.

## Release v3.8.9 of 10 May 2011

* Fixed [SourceForge] bug #3299870 that was allowing imported snippets with duplicate names to be renamed with invalid names.
* Improved UI used to edit imported snippet names.
* Any "warning" compile results in main database are now treated and displayed as "success" results per [SourceForge] feature request #3290359.
* Fixed unreported potential bug in code that sets window class names.
* Updated documentation.

## Release v3.8.8 of 19 January 2011

* Added facility for user to specify maximum age of news items displayed in news dialogue box. New preferences tab added where the maximum age can be customised.
* Preferences dialogue box now displays multi-line tabs when necessary.
* Refactored some code used to align controls on forms.
* Updated license. License HTML help file is no longer MPLd and may not be altered by third parties.
* Updated help file re changes.
* Updated documentation.

## Release v3.8.7 of 16 December 2010

* Delphi XE compiler version 22.0 has been added to the drop down menu in the Code Generation tab of the preference dialogue box.
* Bug fix: compiler results are no longer listed when free-form snippets are printed or copied to the clipboard using the "Edit | Copy Information" menu item.

## Release v3.8.6 of 06 December 2010

* Bug fix release (none reported in bug tracker):
  * Corrected XML file validation so that it does not reject XML processing instructions that contain an "encoding" attribute.
  * Fixed long standing bug that was crashing CodeSnip when the database was updated or restored after editing, adding or deleting any user defined snippet.
  * Attempting to restore a database backup with an unknown (later) file format now raises an exception. Previously CodeSnip tried, unsuccessfully, to read the file.

## Release v3.8.5 of 28 November 2010

* Fixed bug where user was able to create snippets with valid names that would crash the alphabetic overview. Snippet names are now limited to letters from English alphabet and the underscore. Fixes [SourceForge] bug #3120958.
* Fixed bug where snippets that have names beginning with a lower case letter were being omitted from from the associated list of snippets shown in the detail pane. Fixes [SourceForge] bug #3120962.
* Updated Snippets Editor topic in help file.

## Release v3.8.4 of 26 November 2010

* User can now opt to terminate the application when an unexpected exception is trapped. This implements [SourceForge] feature request #3074914.
* Wording of bug report dialogue boxes changed.
* Snippets selection dialogue box now displays wait cursor while waiting for it to be displayed.
* Some corrections and clarifications made to comments that appear in generated "include" files.
* Custom message boxes can now display custom title and icon.
* Imported some updates from "new-backend" development tree:
  * Some source code re-organisation and renaming.
  * Updated some sorted list management code.

## Release v3.8.3 of 24 November 2010

* Added button to "Compile" tab of Snippets Editor to display unit used to test compile snippets. This implements [SourceForge] feature request #3108008.
* Fixed unreported bugs in handling of exceptions raised in threads.
* Simplified method used to load database on start up. No longer uses a separate thread.
* Overhauled and simplified code used to display "wait" dialogues during test compilations and database reloading.
* Refactorings:
  * Increased use of generics in lists and enumerators.
  * Reorganised source code tree by moving some code to more relevant units, renaming some units and increasing use of namespaces.
  * Removed some redundant code.
* Updated help file re changes to snippets editor.

## Release v3.8.2 of 16 November 2010

* The position of the caret in the Snippets Editor's Extra Information control is now displayed. Implements [SourceForge] feature request #3105288.
* Code that displays caret positions was refactored and improved.
* Display of errors in the Snippets Editor's text edit controls has been improved in most cases either by positioning the caret near the error or selecting the erroneous text. This implements [SourceForge] feature request #3107042.
* Made significant changes to code that parses REML mark-up:
  * Rationalised error reporting and added support for reporting the position of errors.
  * Fixed unreported bug that produced wrong error message when empty tags are encountered.
  * Fixed [SourceForge] bug #3107982 that failed to report some unclosed tags as errors.
  * Refactored and reorganised much of the code.
* All encoding and decoding of URIs is now RFC 3986 compliant.
* Refactored character detection and string encoding support code.
* Renamed some units and classes.
* Updated documentation.

## Release v3.8.1 of 08 November 2010

* Fixed [SourceForge] bug #3015589 where some user syntax highlighter settings were being ignored in main display.
* Changed Test Unit view dialogue box to use user syntax highlighter settings.
* Revised credits in About Box program tab.
* Updated third party units: PJMD5 to v0.3, PJSysInfo to v3.3, PJVersionInfo to v3.3.
* Modified version info code to use new features of new PJVersionInfo 3rd party unit.
* Refactored code that parses XHTML-style code.
* Updated compiler warnings used in project and made command line and IDE options the same.
* Updated documentation.

## Release v3.8.0 of 23 October 2010

* Added support for Delphi XE to program. Can now test compile and display results with Delphi XE.
* Updated help file re Delphi XE support.
* Some refactoring.
* Standardised bug-trap and assertion failure exception messages.

## Release v3.7.0 of 23 September 2010

* Added new "Help | CodeSnip News" menu option that displays latest news about CodeSnip and the online database in a dialogue box. The news comes from the CodeSnip RSS news feed.
* Removed news pane from "Update from Web" dialogue box and replaced with button that displays new "CodeSnip News" dialogue box.
* Removed mailing list subscription facility:
  * Removed subscription dialogue box and associated menu option.
  * Removed code that accessed mailing list web service.
  * Removed subscription option from program registration dialogue box.
* Fixed a memory leak.
* Added code that downloads XML document and reads and parses RSS feeds.
* Refactored and improved HTTP request handling code.
* Some further refactoring.
* Updated help file re changes in this release.
* Updated privacy statement.

## Release v3.6.3 of 22 July 2010

* Completely overhauled code that interacts with web services.
  * Character encodings are now correctly handled per information in HTTP header and several different encodings are supported.
  * MD5 checksums in HTTP headers are now supported.
* Updated and corrected the contents of the About Box's "About The Program" Tab.
* Some refactorings, mainly to code that uses MD5 message digests.
* Attempts to compile source with Delphi 2009 and earlier are now prevented.
* Updated documentation.

## Release v3.6.2 of 18 June 2010

* Fixed source code formatting problem in code generator where "forward" declarations were sometimes preceded with an unwanted blank line.
* Fixed potential bug in code that parses mark-up used for a snippet's Extra information. Symbolic entities were not case sensitive.
* Fixed a memory leak.
* Some refactorings that increase use of generics and some others.
* Read-me file updated re v3.6.1 password changes.

## Release v3.6.1 of 01 June 2010

* Proxy server passwords can now contain any Unicode character, not just those included in the Windows-1252 code page.
* Password format in per user ini file changed. Existing passwords have to be re-entered. Ini file format updated to v7.
* Installer updated:
  * It deletes any passwords from v6 and earlier per user ini files.
  * Per-user ini file now stamped as v7.
* Some potential Unicode-ANSI string conversion problems fixed.
* Updated documentation.

## Release v3.6.0 of 26 May 2010

* Added support for emitting compiler directives to switch off specified warnings. This implement [SourceForge] feature request #2994485.
* Preferences dialogue box updated:
  * New "Code Generation" tab used to configure which if any warnings are to be inhibited.
  * Renamed "Source Code" tab to "Code Formatting".
* Added new tab to About Box that displays and enables exploration of some key directories used by CodeSnip.
* Snippets editor now displays row and column occupied by text cursor.
* Per user ini file format changed. It now supports code generation preferences. Ini file version updated to v6.
* Installer updated:
  * Ini files are stamped with correct program and ini file version information.
  * Older versions (v1..v5) of per-user ini file are updated with default code generation preferences.
  * Per-user ini file now stamped as v6.
* Fixed numerous memory leaks.
* Fixed some other potential and unreported minor bugs.
* Some refactoring.
* Updated help file re changes.

## Release v3.5.5 of 24 March 2010

* Fixes download stream read [SourceForge] bug #2976048.

## Release v3.5.4 of 18 March 2010

* Temporary fix for download error checking [SourceForge] bug #2970055.
* Fixed https protocol [SourceForge] bug #2970896.

## Release v3.5.3 of 08 March 2010

* Fixed database download error checking [SourceForge] bug #2964767.
* Updated PayPal donations narrative on welcome page.

## Release v3.5.2 of 22 February 2010

* Changed database downloader to:
  * Use web service's revised download file format
  * Validate download data before updating local database.
  * Provide better download error messages.
* Fixed [SourceForge] bug #2947794 in view link dialogue box.
* Refactored some exception handling code.

## Release v3.5.1 of 09 February 2010

* New Unicode build of the program compiled with Delphi 2010. File I/O remains ANSI.
* Windows NT is no longer supported. Windows 2000 is now the minimum OS. Set-up program changed to enforce this.
* More rigorous enforcement of rules for REML tag attributes used in a snippet's Extra information.
* Fixed a couple of minor UI problems in the Proxy Server and Trapped Bug Report dialogue boxes.
* Minor changes to HTML and embedded browser code.
* Some refactoring.
* Updated documentation.

## Release v3.5.0 of 16 January 2010

* Overview pane can now be configured using Preferences dialogue box to start up with all sections collapsed.
* Reference to ability to donate by credit / debit card removed from Donate dialogue box: now PayPal only.
* Help file updated re above changes.
* Minor refactoring of code that provides information about and renders source code comments.

## Release v3.4.8 of 10 January 2010

* Made some changes to key presses responded to by overview pane and fixed bug where Alt+F4 was not closing program when pane had focus.
* Made some changes to hints displayed when rolling over links in compiler check pane. Also removed pop-up windows describing compiler errors.
* Updated help file: noted Delphi 2010 compiler support and added new information about overview pane keyboard short-cuts.

## Release v3.4.7 of 31 December 2009

* Added IE version number to OS information submitted when program is registered.
* Program now displays "[localhost]" in main window caption when started with -localhost switch.
* All text edit controls in snippets editor now have custom pop-up menus and short-cuts for "cut", "copy", "paste", "select all" and "undo" now work.
* Refactored code that supports use of fonts.
* Updated privacy statement re registration changes.

## Release v3.4.6 of 18 November 2009

* Changed code that takes a security backup of main database during updates to store backup in a single file rather as separate files in a temporary folder. This should fix [SourceForge] bug #2898687.
* Slightly modified user database backup file format to match that now used for main database backup.
* Fixed potential bugs:
  * Code that performs busy waits could have caused program to freeze.
  * Negative numbers written to backup files were not being written correctly.
  * A garbled error message was corrected.

## Release v3.4.5 of 09 November 2009

* Home, Ctrl+Home, End and Ctrl+End keys now work in overview pane and go to first and last item in tree view respectively per [SourceForge] feature request #2888880.
* State of tree view in overview pane is now maintained after editing the user database: the tree is no longer always fully expanded after each edit.
* Removed "Properties" button from print dialogue box along with associated dialogue boxes. This option has always been buggy. This "fixes" [SourceForge] bug #2868706.
* Fixed unreported makefile bug.

## Release v3.4.4 of 21 October 2009

* Changed bug reporting mechanism. Bugs are now reported via the on-line bug tracker. Bug report dialogues changed accordingly. Access to the old bug report web service was removed.
* Added two new default syntax highlighter styles: "Visual Studio" and "No Highlighter". The latter switches off syntax highlighting.
* Fixed [SourceForge] bug #2882331. This was a bug in the syntax highlighter that occurred when an unexpected character was encountered.
* Updated help file re changes.
* Some minor source code corrections.

## Release v3.4.3 of 19 October 2009

* User's OS is now reported and recorded when program is registered.
* Text displayed in preview dialogue boxes can now be scrolled horizontally.
* Added support for building source against later releases of Indy 10 components.
* Help file and privacy statement updated.
* Further updated third party GIF image handling code to latest release.
* Some changes to source code project options.

## Release v3.4.2 of 10 October 2009

* Fixed [SourceForge] bugs #2868706 and #2875857.
* Updated GIF image handling code.

## Release v3.4.1 of 29 September 2009

* All dialogue boxes that request a user's name and / or email address now remember the information last entered, to save retyping the same data.
* Changed to use Indy Internet Components v10 instead of v9 for net access.
* Refactored:
  * Code that stores information about a user.
  * Code that gets details of system folders on local system and other file system related code.
* Updated privacy statement (text file and in help file).

## Release v3.4 of 24 September 2009

* Added support for Delphi 2010 to program. Can now test compile and display results with Delphi 2010.
* Bug fixes:
  * "Invalid cast" error that occasionally appears when a snippet is updated.
  * Bug that kept backup files locked open after restoring a database backup.
  * Current selection is now displayed in Alphabetic and Snippet Kind tabs of overview pane: previously all the database was shown, regardless of search.

## Release v3.3 of 21 September 2009

* Added support for user defined categories which can now be added, renamed or deleted.
* Made changes to snippets editor:
  * On the "Compile Results" Tab, a single simplified list box is now used to both display and change compile results. This replaces two linked controls.
  * The text case of a snippet name can now be changed without causing a duplicate name error.
  * Some controls resized.
* Fixed bug where attempting to overwrite files that are in use caused the bug report dialogue box to appear instead of simply reporting the problem.
* Improved validity checking of snippets that are included in generated source code.
* Help file updated.
* Refactored:
  * UI handling code in snippets editor.
  * Some Snippets database and validation code.

## Release v3.2.3 of 14 September 2009

* Fixed bug in "update from web" dialogue box where most up to date news item was not being displayed.
* Dialogue boxes that that enable selection of categories and snippets by means of tree views and associated check boxes now sort categories by description.
* Categories and snippet kinds displayed in the snippets editor are now sorted by description.
* Refactored:
  * Code that displays tree views in overview pane and snippet selection dialogues.
  * Some list management code.
  * Some snippets editor code.

## Release v3.2.2 of 08 September 2009

* Fixed bug in check list boxes where moving selection using keyboard causes check state to be toggled.
* Custom colours used in colour dialogue, on syntax highlighter page of preferences dialogue box, are now persistent.
* Re-implemented code that displays pop-up menus in detail pane, and fixed a minor glitch as a side effect.
* Simplified code that manages help system.
* Refactored code that manages and customises common dialogues.

## Release v3.2.1 of 24 August 2009

* Appearance of comments that appear at the top of generated source code was changed.
* Slightly modified "license" that appears at the top of some generated units.
* Information about contributor of imported code is now appended to snippet's "extra" information.
* Added a garbage collector.
* Changed size of About box - now wider and credits scroll boxes are now taller. Added credit for encryption code.
* Fixed minor bug that could display a JavaScript error dialogue if an exception occurred in an action initiated by clicking a link in the main display.
* Numerous refactorings.

## Release v3.2 of 17 August 2009

* Added facility for CodeSnip to use a proxy server when accessing the Internet.
* Provided a new dialogue box to configure any proxy server.
* Updated help file re proxy server support and configuration.
* UI is no longer frozen while web services are executing requests. "Update from Web" dialogue box changed to indicate if cancel button pressed when a web request is executing.
* Product version reported in generated source code header comments, splash screen and about box now includes any special build information.
* Some minor code tweaks and refactoring.

## Release v3.1.1 of 15 August 2009

* Check list boxes throughout program changed so that clicking anywhere on an item toggles check state.
* Button used to render and display extra information in snippets editor is now disabled when there is no extra information to display.
* Made minor changes to layout of some dialogue boxes: replaced missing text in bug report dialogue box.
* Some refactorings.

## Release v3.1 of 11 August 2009

* Added a button to the snippets editor to preview an HTML rendering of the mark-up entered as extra information. Includes facility to check any links in the mark-up.

## Release v3.0.5 of 21 July 2009

* Default font is now dependent on underlying OS: Vista - Segoe UI, XP/2000 - Tahoma, NT - MS Sans Serif.
* Some dialogues and splash screen modified to accommodate OS font, in particular larger Vista font. Some also given a light makeover.

## Release v3.0.4 of 13 July 2009

* Added a snippet's category description to main display and to snippet information copied to clipboard or printed. Category description in main display can be clicked to display the category.
* Refactored code that displays clicked routines and code that displays a snippet for editing.

## Unreleased v3.0.3 of 12 July 2009

* Refactored code:
  * Rationalised some JavaScript code.
  * Rationalised some dialogue alignment code.
  * Changed some object types and class hierarchies.
  * Added some automatic object lifetime management logic.
  * Removed some duplicate code and merged some units.
* Fixed an obscure bug in category code snippet generation as a side effect of refactoring.

## Release v3.0.2 of 08 July 2009

* Fixed broken link to CodeSnip database in welcome page.
* Fixed bug in selection search that was selecting both user defined and main database snippets with same name if only one was selected.
* Fixed bug where units required by constants and type definitions were not being added to generated units.

## Release v3.0.1 of 06 July 2009

* Added support for file:// protocol in links in a snippet's extra information.
* Updated help file re changes to extra info.
* Updated exported code and user database file formats to v4 to accommodate revised extra information, although we now save data in v3 format if possible.

## Release v3.0 of 29 June 2009

* Added support for constants and type definition snippets: there are now four types of snippets - routines, constants, types and free-form (which don't conform to any format). Free-form snippets cannot be included in generated units.
* Further formatting instructions added to the active text used in database's Extra information field. Also added a contributors field to database.
* Three predefined syntax highlighters are now provided, with default changing to Delphi 2006 default style. Syntax highlighting used in main display is now customisable. Highlighter keyword list updated.
* Main display changed:
  * Test unit is no longer displayed in compiler check pane: it's now displayed in a dialogue box.
  * Compiler check pane's font changed to true type, with face depending on OS.
  * Information pane now hides compiler table when a free-form snippet is displayed.
  * Compiler check pane now displays special "not available" pages when no compilers installed or a free-form snippet or a section header is selected.
  * "Uncategorised" tab removed from overview pane and replaced with new "Alphabetical" tab that groups snippets by initial letter and "Snippet Kind" tab that groups snippets by kind.
  * "Section" nodes in overview pane can now be expanded and collapsed: pane now has toolbar to perform these actions.
  * "Edit snippet" links displayed in information pane are now also displayed in compiler check pane.
  * Information about snippet type added to information pane.
  * Context menu added to overview pane.
  * Some changes to menu glyphs and short-cut keys.
  * Welcome display modified and now has a link to the donate dialogue box.
* Added option to copy an snippet's source code to clipboard in text and RTF formats.
* Exporting and copying of snippets complete with descriptions and cross references is restricted to routines: not supported for free-form, types and constants.
* Improved detection of invalid dependencies in snippets, including circular dependencies, and provided option to view all dependencies for any snippet from main window and snippets editor.
* Revised content of many dialogue boxes etc to refer to "snippets" instead of "routines" where necessary.
* Changed format and location of user-defined database and format of exported and submitted files.
* Added new "Imported Snippets" category that receives imported routines: they were formerly imported into the "User Defined" category
* Modified code that reads main database to deal with revised file format for new snippets types and introduction of pre-processor instructions to enable retrofitting of new snippets without breaking earlier versions of CodeSnip.
* Changed name and location of user preference configuration file.
* Revised external object that communicates with JavaScript in browser controls.
* Updated program credits in about box, restyled and widened it.
* Changed size of preferences dialogue box and revised syntax highlighter tab.
* Changed captions in preview dialogue box.
* Changed appearance of splash screen.
* Modified snippets editor to work with new snippet types, improved error checking code and prevented test compilation of free-form snippets.
* Speeded up loading of main database.
* Added an Easter egg!
* A few refactorings.
* Fixed several bugs:
  * Bug in backup files including database files larger than 32Kb was fixed.
  * Bug in history list following editing user defined snippets fixed by clearing list after snippets have been edited.
  * Imported user defined routines no longer forget any dependencies on main database snippets.
  * Occasional bug in displaying test unit fixed by displaying test unit in dialogue box instead of main display.
* Modified installer re new folder structure and copying over data from previous versions.
* Revised help file to reflect changes. Added new main contents "chapter" about the various snippet types.

## Unreleased v2.4.1 of 13 May 2009

* Refactored code that provides information about the program and web URLs and services it accesses.
* Changed URL accessed by donations dialogue box.

## Release v2.4 of 11 May 2009

* Added donate menu option and dialogue that accesses a PayPal donation web page.
* Removed support for the Windows 9x platform since CodeSnip now generates fatal errors on that platform:
  * Removed Windows 9x specific code.
  * Changed installer to prevent installation on Windows 9x.
* Updated help file re changes.

## Release v2.3.7 of 26 April 2009

* Made user name and email address entered in Code Submission Wizard persistent on a per-user basis.
* Updated Code Submission Wizard and Privacy Statement help topics re the changes.

## Unreleased v2.3.6 of 26 January 2009

* Changed method that is used to get locale information to be compatible with Vista as well as earlier OSs.

## Release v2.3.5 of 25 January 2009

* Changed method used to generate HTML displayed in main program window to avoid dynamic updating of documents in attempt to counter a reported JavaScript bug.
* Refactored generation of HTML tags in all parts of program that use HTML in display.
* Corrected method naming error.

## Unreleased v2.3.4 of 16 January 2009

* Copy Source Code menu item now places a copy of selected snippet on clipboard in syntax highlighted rich text in addition to plain text.
* Updated help file accordingly.

## Unreleased v2.3.3 of 14 January 2009

* Browser controls and snippets tree-views are now selected when containing frame is entered.
* Discrepancy in way highlighting works in snippets tree-views fixed.
* "&" characters are now rendered correctly in TMessageBox dialogues.
* Code that executes compilers now uses one thread instead of two.
* Refactorings:
  * Some constants relocated.
  * Rationalised some routine and method calls.
  * Replaced some control character literals with constants.
  * Updated IntfUIHandlers unit with IE6/7 related constants.

## Unreleased v2.3.2 of 10 January 2009

* Revised compilers object. Singleton instance removed. Local instances of object are created where needed.
* Added new method to compiler objects to detect errors and warnings
* Fixed incorrect caption in compiler error dialogue.
* Added new object that manages test compilations, compiler set-up and viewing compile errors. Used by main form and snippets editor.
* Added "View Compile Errors" menu option to Database menu.
* Added Alt+V hot key to view compile errors in Snippets editor.
* Updated help file for database menu to add "View Compile Errors" and missing entries for Submission, import and export of user database.

## Unreleased v2.3.1 of 06 January 2009

* Fixed test compilation bug in snippets editor that could corrupt compiler errors or warnings displayed from main display.
* Added support for tab switching in compiler errors dialogue box using Ctrl+Tab and Shift+Ctrl+Tab.

## Release v2.3 of 05 January 2009

* Changed name of Copy Snippet menu item to Copy Source Code.
* Added new Copy Information menu item to Copy menu - copies all snippet information to clipboard in text and RTF.
* Added Save Database button to toolbar.
* Changed status bar to display a count of user defined routines and an indicator that shows when user database has been modified.
* Refactored and extended clipboard management code.
* Added new buttons to selection search dialogue box to select user defined or main database routines.
* Added facility to test compile routines to user defined snippets editor dialogue box.
* Modified compiler errors dialogue box to be able to display results of compilation with more than one compiler.
* Updated help file re changes.

## Release v2.2.5 of 31 December 2008

* Replaced routine's credits and comments properties with new Extra information property that can store formatted text.
* Added parser for mark-up language used by new Extra property.
* Modified snippets edit dialogue box to use new Extra property.
* Changed main database, user database and export file format to support new Extra property. User database and export files generated by this version can't be read by earlier versions of the program.
* Modified and refactored print document generation code to use new Extra property.
* Refactored some HTML generation code.
* Fixed a bug that occasionally causes a GPF when updating a user defined routine.
* Removed redundant topic from help file.

## Release v2.2.4 of 19 December 2008

* Fixed bug in text and RTF preview dialogue boxes that was setting margins incorrectly and clipping displayed text.

## Unreleased v2.2.3 of 17 December 2008

* Refactored code that handles XML files (user database and import / export). Pulled out common code and further extended XML document object.

## Unreleased v2.2.2 of 16 December 2008

* Added glyphs to printers in print dialogue box.
* Various refactorings of print and highlighting related code.
* Printing now uses user-defined highlighters. Current highlighting is now previewed in print preferences.
* Bug fixes:
  * Help button now displays in page set-up dialogue on Vista.
  * Page set-up dialogue now makes use of custom margin settings.

## Unreleased v2.2.1 of 16 December 2008

* Several refactorings:
  * Rationalised email validation code.
  * Rationalised exceptions raised when validating entry into dialogue box.
  * Rationalised code that momentarily pauses execution of a thread.
  * Made wide use of extended TRect structure.
  * Changed various loops to use enumerators.
  * Removed some unused code.
* Fixed minor bug in open and save dialogues that occasionally failed to detect existence of a file.

## Unreleased v2.2 of 15 December 2008

* Added facility to submit user defined snippets via Internet for inclusion in main database.
* Added facility to export user defined routines to file and to import exported files.
* Made minor changes to wizard dialogue boxes.
* Rewrote message dialogue box code.
* Made minor changes to open and save dialogue boxes.
* Updated help file for the new code import, export and submission features.

## Release v2.1 of 11 October 2008

* Added support for Delphi 2009 Win 32 personality.
* Added a button to set all compiler results to success to snippets edit dialogue box.
* Refactored some code.
* Updated help file re Delphi 2009 support.

## Unreleased v2.0.7 of 05 October 2008

* Fixed residual bug in alt key bug work-around (CodeGear Quality Central bug report #374030). The bug was manifesting itself only for the first dialogue box displayed after the program started.

## Unreleased v2.0.6 of 05 October 2008

* Refactoring:
  * Added class methods to instantiate and use various classes that have only one public method to save caller having to create, execute and destroy object. Public constructors of these classes were changed to cause assertion failure if directly called.
  * Made static classes derive from new base class that causes assertion failure if constructor called.
  * Combined some action update handlers in main form.
  * Updated assertions and raising of EBug exceptions to programatically get name of class triggering error.
  * Made some class' protected and private sections strict.

## Unreleased v2.0.5 of 03 October 2008

* Refactoring: changed custom save source dialogue to descend from extended save dialogue box.

## Release v2.0.4 of 21 September 2008

* Improved speed of looking up routines in database.
* Prevented any user defined routine from referencing itself.
* User defined routines now always reference routines from user database in preference to main database when there is a name conflict.

## Unreleased v2.0.3 of 20 September 2008

* Fixed bug that caused an assertion failure when an attempt was made to display the Select Routines dialogue box when an empty category was present in database.

## Unreleased v2.0.2 of 19 September 2008

* Now gives option to save changed user defined database before updated main database.
* When a routine is updated or deleted references to it in other routines are updated or removed.
* Corrected reference in installer to menu item used to update database (this changed from v2).

## Release v2.0.1 of 18 September 2008

* Fixed bug that fails to load user database and deletes it if a category is added to main database during on-line update.
* Fixed bug that ignores any user defined snippets that have same name as snippets in main database.
* Ensured main form is disabled when database is loading.
* Ensured splash form is hidden if an exception occurs while splash form is displayed.

## Release v2.0 of 15 September 2008

* Added support for user defined snippets:
  * User database can be edited, saved, backed-up and restored.
  * User database can reference code in main database.
  * Names of user defined snippets are coloured blue to distinguish them from main database.
  * User database is stored as a mix of XML and source files in a sub-folder of the per-user application data folder.
  * Queries can now be refreshed when content of user database changes.
* Modified extended external object that communicates between browser controls and application.
* Main database engine heavily modified.
* Greater use of DHTML to manipulate main display.
* Made browser pop-up menu display glyphs for items menu items that replicate links in browser control.
* Modified welcome page to appear differently depending on state of main and user defined databases.
* Disclaimers, copyright and other headers of saved, printed and copied documents changed.
* Commenting of exported code changed slightly to allow for user snippets that may not support all commenting styles.
* Fixed status bar display bug.
* Category headers in overview pane are now in bold.
* Added enumerators to several list objects to support for..in construct.
* Tweaked exception handling.
* Added support for converting GIF resources into bitmaps for use in image lists.
* Changed URL used to access program's home page.
* Updated help file to reflect changes.
* Added credits for use of Anders Melander's GIFImage unit to about box.

## Release v1.9.4 of 01 September 2008

* Improved handling of errors encountered when running compilers.
* Provided checks for invalid compiler executable files in Config Compilers dialogue box.
* Added enumerator to Compilers object.
* Made ECodeSnip exceptions and descendants clonable when copying between threads.

## Unreleased v1.9.3 of 24 August 2008

* Fixed bug in the database updater which could cause a deleted local file not to be noticed and not replaced.

## Unreleased v1.9.2 of 24 August 2008

* Changed method used to generate program key. No longer uses MAC Address, since code to find this fails on Windows Vista.
* Refactored to remove knowledge of how contributor information and database are stored from TAppInfo class.
* Revised code that manages contributors so that storage details are private to the classes.

## Unreleased v1.9.1 of 24 August 2008

* Rebuilt CodeSnip and install helper program with Delphi 2006:
* Modified CodeSnip source to compile without warnings.
* Replaced deprecated library calls with alternatives.

## Release v1.9 of 14 August 2008

* Changed so that all user accounts use the same database rather than having their own copy. Database now stored in common application data folder, along with registration information. Per-user configuration information remains in per-user application data folder in renamed file.
* Installer can now optionally preserve data stored in database and configuration file used by earlier versions of the program. This involves creating new configuration files and moving the database.
* Updated help file re these changes.

## Unreleased v1.8.11 of 11 August 2008

* Removed duplicate compiler glyph resources and modified compiler handling code accordingly.

## Unreleased v1.8.10 of 11 August 2008

* Refactored various units to use extended theme support.
* Fixed redraw bug in tree views that use check boxes: check boxes were redrawing in wrong state when themes changed.
* Improved support for theme changes. Theme manager now gets notified of changes directly from Windows.
* Suppressed unnecessary compiler warnings.

## Unreleased v1.8.9 of 10 August 2008

* Modified Select Compiler dialogue box (opened from Configure Compiler dialogue) and Choose Element Colour dialogue (opened from Preferences dialogue) to be aligned correctly over dialogues, work correctly with Vista task bar and support help keywords.
* Select Compiler file open dialogue now defaults to display any current compiler executable.
* Choose Element Colour dialogue box now uses UK English and has custom title.
* Added help topics for Select Compiler and Choose Element Colour dialogues.

## Release v1.8.8 of 16 June 2008

* Changed to make application minimisation, task bar preview window, and appearance in "Flip 3D" task switching display correctly on Windows Vista.
* Provided work-around for Delphi's Alt key bug on XP and Vista (CodeGear Quality Central bug report #374030).

## Unreleased v1.8.7 of 05 June 2008

* Made selected tabs in information and detail pane persistent.
* Fixed bug in build script.

## Unreleased v1.8.6 of 02 June 2008

* Fixed lock-up that could occur when displaying wait dialogue box while background tasks execute. Previous attempt to fix this problem failed.
* Changed "marquee" that is displayed in wait dialogue box to appear correctly on Vista.

## Release v1.8.5 of 30 May 2008

* Fixed bug that was causing Save Snippet and Save Unit dialogue boxes to ignore file type
 selected by user, always outputting default file type.
* Deleted some unused source code.
* Removed option to install a desktop icon from installer. Also refactored install script to conform to current Inno Setup standards.

## Release v1.8.4 of 22 April 2008

* Added manifest resource to ensure compatibility with Windows Vista and to use Vista themes.
* Fixed border problem in web update dialogue box and about box when displayed under Vista / IE7 browser control.
* Prevented selection of text in previews displayed in preferences dialogue box.
* Updated set-up script to use macros.
* Modified Build batch file to work with Windows SDK 2008.

## Unreleased v1.8.3 of 05 November 2007

* Refactored dynamic CSS generation code.

## Unreleased v1.8.2 of 04 November 2007

* Refactored assignable interfaced objects.

## Release v1.8.1 of 04 November 2007

* Made changes to browser control and URL handling.

## Unreleased v1.8 of 04 November 2007

* Added pop-up context menus to main display's detail pane.

## Unreleased v1.7.7 of 29 October 2007

* Modified code of compiler wait dialogue box and splash screen to try to prevent bug that occasionally prevent the dialogue from closing, locking up application.

## Unreleased v1.7.6 of 18 October 2007

* Shift-clicking links in the main display and some dialogue boxes was starting Internet Explorer. Fixed so that Internet Explorer is no longer started and shift-clicking external links now starts default browser.

## Unreleased v1.7.5 of 17 October 2007

* Modified Preferences dialogue box:
  * Refactored code that displays measurement units.
  * Preview on Source Code tab now takes on appearance of source code highlighter defined on Syntax Highlighter tab.
* Changed format of ini file that stores persistent settings so that source code highlighter preferences are now stored in Prefs section of ini file rather than own section.
* Customised installer to update existing ini files to revised version.

## Release v1.7.4 of 14 October 2007

* Fixed display bug when selecting routines following a text search.
* Improved text search algorithm to permit search strings containing punctuation characters.
* Fixed typo in the "About The Database" section of the About box.

## Release v1.7.3 of 27 September 2007

* Improved alignment of dialogue boxes and splash screen over owning forms. Alignment code substantially refactored.
* Added support for multiple monitors.

## Release v1.7.2 of 24 September 2007

* Fixed bug that was preventing wait dialogue box from displaying during long compilations.

## Unreleased v1.7.1 of 22 September 2007

* Added list of testers to credits section of Database tab in About box.
* Added new help menu item that displays privacy statement.
* Rearranged help menu items.
* Updated help file re changes to help menu.

## Release v1.7 of 08 September 2007

* Added new facility to print information about selected routines, with page set-up and printer configuration.
* Added new "general" tab (sets default measurement units) and "printing" tab (to set printing defaults) to preferences dialogue box.
* Changed format of ini file that stores persistent settings.
* Updated help file to reflect changes.
* Customised installer to update existing ini files to revised version.

## Unreleased v1.6.4 of 02 July 2007

* Corrected typos in generated source code header comments.
* Added support for embedding titles in generated documents where document supports title meta data.
* Added suggested file name to save unit and save snippets dialogue boxes.
* Refactored code in syntax highlighter that generates XHTML.

## Unreleased v1.6.3 of 13 May 2007

* Added support for selecting and copying text displayed in preview dialogue.
* Changed so that each document type displayed in preview dialogue box has same margins.
* Updated help file re changes to preview dialogue box.

## Unreleased v1.6.2 of 12 May 2007

* Updated to use revised news data format provided by web service.
* Update from Web dialogue box now displays number of news items along with page number of currently displayed item.

## Release v1.6.1 of 09 May 2007

* Fixed bug that allowed user to select a different routine while compiling another causing display to get out sync.

## Release v1.6 of 08 May 2007

* Added support for Delphi 2007 compiler.
* Updated help file re new compiler support.

## Release v1.5.13 of 04 March 2007

* Fixed bug from v1.5.11 where Tools | Register CodeSnip and View | Show/Hide Test Unit menu options were permanently disabled.
* Fixed bug from v1.5.9 where showing and hiding test units from menus was out of sync with links in compiler check pane.

## Release v1.5.12 of 01 March 2007

* Made long operations (loading database and compiling test units) execute in threads.
* Changed to display wait dialogue while updated database is being loaded.
* Made progress meters displayed in wait dialogues update more smoothly.

## Release v1.5.11 of 25 February 2007

* Added splash screen displayed when program is loading.
* Main window, menu and toolbar is now disabled when program is initialising and when updated database is loading.
* Program window is now centred on screen first time it is run and program is now never started minimized.

## Unreleased v1.5.10 of 17 February 2007

* Refactored code that handles web browser controls. Moved various pieces of code that manipulates and queries browser controls into central UI and IO manager classes. Also added helper classes to manipulate HTML documents and browser controls.
* Some code made redundant by above changes was removed.
* Lightened and centralised colours used to highlight text search results.

## Unreleased v1.5.9 of 16 February 2007

* Refactoring update. Revised code that manages the main display, i.e overview and details panes.

## Release v1.5.8 of 16 February 2007

* Fixed bug in view history where selecting an item from the history list could cause a crash after database has been updated. This was fixed by clearing the history list after updating the database.
* Now clears the main display before re-displaying an updated database to prevent an item from the old version of the database being selected.

## Unreleased v1.5.7 of 12 February 2007

* Refactored, relocated and extended use of some utility routines, resulting in some other minor changes:
  * All error and information message boxes now have properly terminated sentences.
  * Generated XHTML less likely to contain illegal characters.

## Unreleased v1.5.6 of 11 February 2007

* Modified about dialogue box to display information about the Code Snippets Database in addition to the program. The two kinds of information are displayed in two tabs.
* Added code to get list of database contributors from a file downloaded with database updates.

## Unreleased v1.5.5 of 11 February 2007

* Made keyboard interaction with application more consistent:
  * Made browser controls activate and focus properly when user tabs into them.
  * Fixed tab order problems in main display and about dialogue box so that only controls that may need to receive user input are now activated by tabbing.
  * Links displayed in browser controls are always now included in tab sequence and can be activated by Ctrl+Return when focused.
  * Fixed inconsistency in tab sets in overview and details pane responded inconsistently to Ctrl+Tab and Shift+Ctrl+Tab.
* Changed browser control respond to activation via the mouse to be the same as for the keyboard.

## Release v1.5.4 of 09 February 2007

* Added disclaimers re database code to generated units and snippets and to program's welcome page.
* Made slight modifications to source code generation code.

## Release v1.5.3 of 08 February 2007

* Refactored and rationalised code in main form and moved some code into help classes.
* Revised code that performs customisation, auto-sizing and alignment of forms and dialogue boxes.
* Standardised execution method of dialogue boxes.

## Unreleased v1.5.2 of 04 February 2007

* Refactored help manager system to make it easier to swap in new help systems in future.
* Modified help handlers in forms to remove redundant code.
* Modified how help menu items call help topics.

## Unreleased v1.5.1 of 04 February 2007

* Refactored handling of database searches by creating new global query object to store information about current query on database.
* Changed relevant code to use the new object and deleted resulting redundant code.
* Made some other minor code improvements and modifications.

## Unreleased v1.5 of 03 February 2007

* Made status bar display database and search information along with other prompts in addition to displaying hints.

## Unreleased v1.4.6 of 17 December 2006

* Made minor changes to appearance:
  * Changed some colours to system colours from hard-wired colours.
  * Changed help links in main display from blue to green.
  * Removed text highlighting from welcome page.

## Unreleased v1.4.5 of 04 December 2006

* Refactored code that generates test units. As a consequence names of test units displayed in Compiler Check pane have been corrected to the actual names used in test compilations.

## Release v1.4.4 of 04 December 2006

* Added new menu item to View menu that toggles visibility of test units in the compiler check tab.
* Changed glyph used for link that performs same action in compiler check tab and made image change depending on visibility of test unit.

## Unreleased v1.4.3 of 03 December 2006

* Changed information pane to load routines dynamically via DHTML rather than reloading document each time.
* Refactored DHTML code and detail frames that support DHTML.
* Refactored routine HTML generation code.
* Rationalised some dynamic CSS generating code.
* Revised information pane's underlying HTML code.

## Unreleased v1.4.2 of 03 December 2006

* Corrected alignment of About and Compiler Errors dialogue boxes over main form.

## Unreleased v1.4.1 of 03 December 2006

* Fixed bug where Test Compile menu option and tool button were always enabled and could cause an assertion failure when no routine was selected or no compilers were available.

## Unreleased v1.4 of 03 December 2006

* Revised display in compiler check pane. Now lists database and test results side by side.
* Changed routine compiler check page to be updated dynamically (using JavaScript) when routine selection changes rather than always reloading page.
* Modified some JavaScript support code.
* Fixed potential bug in compiler code.
* Updated help file re changes to Compiler Check tab.
* Fixed a typo and index error in help file.

## Unreleased v1.3.5 of 01 December 2006

* Changed to display a border-less message dialogue during long test compilations. The dialogue is not displayed for shorter compilations.
* Updated help file re above and fixed an error in the search menu topic.

## Unreleased v1.3.4 of 26 November 2006

* Refactored JavaScript used to interface between main program and HTML display.
* Centralised generation of JavaScript in main code.

## Unreleased v1.3.3 of 25 November 2006

* Refactored handling of CSS and XHTML:
  * Changed way CSS is provided to enable use of system font and colours.
  * Tidied source HTML documents to remove illegal XHTML strict attributes and to remove hard-wired colours.

## Release v1.3.2 of 24 November 2006

* Made program remember whether test units are displayed or hidden until end of session.

## Unreleased v1.3.1 of 21 November 2006

* Made minor modification to appearance of Configure Compilers dialogue box.

## Unreleased v1.3 of 18 November 2006

* Added facility to sign up to CodeSnip mailing list on-line.
* Corrected further typos in registration wizard.
* Updated help file re mailing list sign-up, changed privacy statement and added license to contents page.

## Release v1.2.5 of 16 November 2006

* Corrected and modified text displayed on last page of Registration Wizard when user elects to join mailing list.

## Unreleased v1.2.4 of 14 November 2006

* Changed about box and help menu to display end user license agreement in help file rather than separate text file.
* Added license topic and made related changes to help file.

## Unreleased v1.2.3 of 12 November 2006

* Fixed incorrect glyph used for Show All search menu item and tool button.
* Moved Tools | Preferences menu option to top of Tools menu.

## Unreleased v1.2.2 of 12 November 2006

* Added hot tracking to tree view check boxes used in Select Routines dialogue box when Windows XP themes are enabled.

## Unreleased v1.2.1 of 11 November 2006

* Refactoring release:
  * Method used to construct and use help file changed.
  * Moved code that detects HTML and RTF files to appropriate utility units.
  * Streamlined code in preview dialogue box.

## Release v1.2 of 11 November 2006

* Changed syntax highlighter used to format units and code snippets to be able to read custom settings from persistent storage.
* Added Syntax Highlighter tab to preferences dialogue box to enable users to customise the font, style and colours used by the syntax highlighter.
* Modified preferences dialogue box's Source Code tab to display a preview of routines using various comment styles.
* Updated help file re revised preferences dialogue box.

## Unreleased v1.1.2 of 07 November 2006

* Refactoring release:
  * Added code to generate CSS properties.
  * Added new classes to generate RTF code.
  * Re-implemented RTF highlighted code.
* Now generates much smaller RTF export files.

## Unreleased v1.1.1 of 31 October 2006

* Changed Select Routines dialogue to use XP style check boxes when XP themes active and custom check boxes when XP themes inactive.

## Release v1.1 of 30 October 2006

* Added ability to generate and save whole Pascal unit containing currently selected routines.
* Added new search that can find all routines cross-referenced by a given routine.
* Added ability to manually select routines that are displayed in overview pane.
* Added short-cut key and changed glyph for File | Save Snippet option / tool button.
* Updated way source code preferences are stored. Broke backwards compatibility with previous storage method, so upgraders may loose settings.
* Made minor changes to preferences dialogue box.
* Word-wrapped long uses lists in generated units.
* Refactored code that determines type of exported files.
* Refactored and expanded code that deals with source code exporting.
* Fixed some minor bugs:
  * Previews of large rich text documents were displaying RTF source instead of rendering document.
  * Assertion failure could (rarely) happen when displaying message boxes without specifying parent form.
  * Saving snippets in a file without supplying a file extension could silently overwrite existing files.
  * Comment style was being ignored when generating a unit.
* Updated help file:
  * Added new topics, index entries and TOC entries for new features.
  * Updated some existing topics to refer to new features.
  * Revised and corrected several existing help topics.

## Unreleased v1.0.3 of 26 October 2006

* Refactored various parts of source code. No changes to program's functionality. Details are:
  * Standardised all singleton objects on interface based implementation.
  * Centralised code that gets location of license file.
  * Standardised links that trigger JavaScript in some HTML resources.
  * Changed bug report dialogue box to descend from common wizard dialogue box.

## Release v1.0.2 of 25 October 2006

* Changed so that links from program to external web pages display in default browser rather than IE.
* Refactored code that displays license text file in external application.
* Reworded some of welcome screen and added links to on-line database.
* Refactored some JavaScript code that works with main display HTML and web browser code.
* Made minor changes to hints displayed in status bar when cursor is over links.

## Release v1.0.1 of 14 October 2006

* Fixed problem in web update that caused program to crash on Windows 9x platforms.

## Release v1.0 of 09 June 2006

* Revised About Box text and appearance and added link that displays license file.
* Refactored and renamed some code.
* Made minor changes to appearance and effect of Configure Compilers dialogue box.
* Fixed potential bug displaying JavaScript error dialogue if help called from links in main display fail.
* Made some literal strings resource strings.
* Made calls to help system fail gracefully on machines without HTML Help installed.
* Modified code that reads program's version information.
* Added important compiler directives.
* Standardised appearance of all groups of action links in main display.
* Added Help menu item to display license and to access CodeSnip web page.
* Moved bug report and registration menu options from Help to Tools menu.
* Updated and help file re new commands, corrected some errors and re-styled menu help sections.
* Created installer using Inno Setup.
* Added new batch file to build program.
* Fully commented code.
* Changed to new end user license agreement for the executable program. The program remains open source.

## Release v1.0 RC 3 of 01 May 2006

Internal CodeSnip version 0.12.0

* 3rd release candidate for the v1.0 release.
* Updated to use v4 of update web service that uses completely new update protocol. Significant changes to code were needed to achieve this.
* Redesigned update dialogue box accordingly.
* Added ability to update dialogue to display latest CodeSnip news delivered as part update process.
* Updated help file re changes to update dialogue box.

## Release v1.0 RC 2 of 16 April 2006

Internal CodeSnip version 0.11.3

* 2nd release candidate for the v1.0 release.
* Fixed bug where user could drag and drop files onto web browser controls and file contents would overwrite the display.

## Release v1.0 RC 1 of 11 April 2006

Internal CodeSnip version 0.11.2

* 1st release candidate for the v1.0 release.
* Updated help file:
  * Ensured that external links display in a web browser window rather than in the help window.
  * Added additional internal links to some help topics.
* Fixed compiler warnings.
* Removed some redundant code.

## Unreleased v0.11.1 Beta of 10 April 2006

* Improved and fixed interaction with database update web service:
  * Download manager now sends program's key and registration code to web service instead of place-holder strings.
  * Handling for HTTP error messages improved. Short HTTP error descriptions are displayed rather than full content of error pages.

## Release v0.11.0 Beta of 07 April 2006

* Added ability to register CodeSnip at DelphiDabbler.com:
  * Registration is performed via a new wizard that gathers registration information and interacts with web server.
  * Wizard is accessed via a Help menu option and About box button that appear only when application is unregistered.
  * Application is identified by a unique key.
  * Registration information is stored in persistent storage.
* Reworked and added classes to centralise access to system and application information.
* Updated help file with details of registration dialogue and CodeSnip mailing list.

## Unreleased v0.10.12 Beta of 04 April 2006

* Improved code that stores global application settings. Prepared way for having per-user and global settings rather than just per-user settings as at present.

## Unreleased v0.10.11 Beta of 03 April 2006

* Revised to work with v3.1 of CodeSnip database update web service.

## Unreleased v0.10.10 Beta of 02 April 2006

* Added program icon (16x16, 32x32 and 48x48 versions).

## Release v0.10.9 Beta of 02 April 2006

* Fixed bug where browser controls displayed JavaScript error dialogue when exceptions were raised by database updates initiated from browser control's "external" object.
* Refactored some code in main form and main snippets object as a result of above fix.
* Also refactored some of search code in main form.

## Unreleased v0.10.8 Beta of 02 April 2006

* Removed bug in database update manager that was causing database to be restored unnecessarily.
* Heavily refactored update manager code as part of bug fix.

## Release v0.10.7 Beta of 28 January 2006

* Fixed display problems in details pane when running on Windows 2000.
* Changed style of scroll bars from flat to normal when running in Windows XP classic style or on earlier Windows version.
* Made compiler check pane update itself when compilers are added or removed.

## Release v0.10.6 Beta of 20 January 2006

* Fixed bug where backup directory was not being deleted after database updates.

## Release v0.10.5 Beta of 14 January 2006

* Added credits for third party code to about box.
* Completed help file.

## Release v0.10.4 Beta of 12 January 2006

* Added checking of checksums of downloaded files to increase security. Exceptions now raised when a file's checksum is incorrect.
* Fixed small alignment problem in update dialogue.

## Unreleased v0.10.3 Beta of 11 January 2006

* Changed so that compiler output is now captured directly rather than via temporary log file.
* Compiler execution is now time-sliced and time-limited rather than being allowed infinite processing time.

## Unreleased v0.10.2 Beta of 10 January 2006

* Reverted to Delphi 7 to avoid Delphi 2006 bug that was enabling dialogues to be minimized and maximized.
* Reordered controls in Find Compiler dialogues.
* Restored title bar close button to web update dialogue.
* Reverted to Indy 9 Internet controls (from Indy 10) and made relevant adjustments to code.
* Completed help topics for Find Text and Find Compiler dialogues.

## Release v0.10.1 Beta of 09 January 2006

* Removed debug code (message box) mistakenly left in compiler execution code.
* Refactored compiler support classes.

## Release v0.10.0 Beta of 08 January 2006

* Added support for Delphi 2005/6 Win32 compilers.
* Refactored some compiler support code.
* Added support for user-configurable compiler switches.
* Used new tabbed layout for Configure Compilers dialogue box and added tab for configuring compiler switches.
* Updated help file to reflect redesign of Configure Compilers dialogue box.

## Release v0.9.0 Beta of 06 January 2006

* Added facility to copy code snippets to clipboard.
* Added new preferences dialogue box to enable configuration of default format for code snippets.
* Added new preferences class to persist data entered in the preferences dialogue.
* Refactored main snippets class to simplify addition of new copy snippet facility.
* Updated help file re new additions.

## Unreleased v0.8.3 Beta of 04 January 2006

* Modified to compile with Indy Components v10 and Delphi 2006 for Win 32.

## Unreleased v0.8.2 Beta of 04 January 2006

* Created static class to interpret command line and changed other code to work with the new class.

## Unreleased v0.8.1 Beta of 04 January 2006

* Fixed minor display bug in web update dialogue box.
* Fixed about box's problem in displaying help in response to F1 key press.

## Release v0.8.0 Beta of 30 November 2005

* Changed help file from WinHelp (.hlp) format to HTML Help (.chm) format.
* Changed program to use new format help file.

## Release v0.7.7 Beta of 22 November 2005

* Refactored and revised code that accesses DelphiDabbler web services.
* Updated to use v2 of the database update web service.
* Added topics for Bug Report dialogue and Web Update dialogue to help file.

## Release v0.7.6 Beta of 04 June 2005

* Fixed Delphi compiler auto-detection bug.
* Fixed bug that caused endless loop of exceptions when "database" was corrupt.
* Syntax highlighter now generates correct XHTML for multi-line comments and generates correct CSS for mono-spaced fonts.
* Occasional failure to create compiler log files now reported as error rather than bug.

## Release v0.7.5 Beta of 03 June 2005

* Fixes bugs that surface when user has disabled scripts in IE's Internet zone. Program no longer runs in Internet zone.

## Release v0.7.4 Beta of 09 May 2005

* Made user defined settings in Save Snippets dialogue box persistent.

## Unreleased v0.7.3 Beta of 25 April 2005

* Disabled test compile button on compiler checks pane along with associated menu and toolbar button when no compilers installed.
* Rewrote main welcome page, adding links to compiler check dialogue. Made same welcome page appear in both detail panes.
* Updated compiler check pages by adding links to compiler check dialogue and new "about compiler checks" help topic.
* Updated help file with new "about compiler checks" topic and completed "QuickStart" topic.
* Added new features to DOM's external object to support above changes.

## Unreleased v0.7.2 Beta of 21 April 2005

* Refactored code that maintains persistent application data.
* Refactored syntax highlighter code and moved interfaces and enumerated types to own unit.
* Renamed unit generation unit now that it generates source code other than complete units.
* Carried out minor refactoring of Pascal analyser unit.

## Unreleased v0.7.1 Beta of 20 April 2005

* Disabled F1 key press handling in dialogues with no help button. Was triggering bad topic errors in WinHelp.
* Added "Compile" prefix to compiler check page's "Test Compile" button.
* Fixed errors in "do nothing" doc host handler used by web browser control.

## Release v0.7.0 Beta of 17 March 2005

* Added new facility to save a routine, or a whole category of routines, to file.
* Reworked syntax highlighter implementation.
* Updated help file with details of new routine saving feature.

## Release v0.6.0 Beta of 10 March 2005

* Added syntax highlighting for source code displayed in detail panes.

## Release v0.5.0 Beta of 05 March 2005

* Added support for Free Pascal compiler by totally reworking the compiler support engine.
* Added new dialogue box to configure compilers. Compiler detection ability retained but now only works in response to user request.
* Revised about box to include "powered by Delphi" logo.
* Updated help file:
  * Added incomplete topics for each of the main menus.
  * Added complete new topic for the compiler configuration dialogue box.
  * Fixed K-keyword errors and added extra keywords for dialogue boxes.

## Release v0.4.0 Beta of 28 February 2005

* Text search results are now highlighted when routines are displayed in the information pane.

## Unreleased v0.3.4 Beta of 26 February 2005

* Separated back end database code from Snippets object.
* New back end code designed to make it easy to change the data provider in future versions. Current version accesses data in .ini and .dat files.

## Unreleased v0.3.3 Beta of 25 February 2005

* Fixed bug that was preventing Ctrl+F from activating Find Text dialogue box.
* Realigned controls in bug report dialogue and fixed tab order problems.
* Fixed email address validation error in bug report dialogue

## Unreleased v0.3.2 Beta of 24 February 2005

* Refactored code that provides compiler names and introduced global Compilers object.

## Unreleased v0.3.1 Beta of 24 February 2005

* Centralised code that displays message dialogues and standardised their appearance.

## Unreleased v0.3.0 Beta of 23 February 2005

* Updated welcome pages to provide more help on using CodeSnip.
* Removed dialogues that appeared on start up when database was empty. Welcome page now provides this information along with an option to download database.
* Improved handling of welcome page.

## Unreleased v0.2.4 Beta of 23 February 2005

* Refactored and simplified access to dialogue boxes.
* Improved search code.

## Unreleased v0.2.3 Beta of 23 February 2005

* Created a class hierarchy for all frames that display HTML in a web browser control.

## Unreleased v0.2.2 Beta of 22 February 2005

* Localised various literal strings and moved some constant values to a common location.

## Unreleased v0.2.1 Beta of 22 February 2005

* Overhauled web browser external object extender that communicates browser events to application.
* Added new notifier object that centralises handling of GUI user interaction.

## Unreleased v0.2.0 Beta of 21 February 2005

* Made minor changes to appearance of main display.
* Refactored the HTML generation engine, added several HTML templates to resources and localised all strings used in generated HTML.

## Unreleased v0.1.4 Beta of 19 February 2005

* Refactored some code.

## Unreleased v0.1.3 Beta of 18 February 2005

* Removed redundant code.

## Unreleased v0.1.2 Beta of 18 February 2005

* Removed debug code.

## Unreleased v0.1.1 Beta of 18 February 2005

* Fixed minor bugs.

## Release v0.1.0 Beta of 30 January 2005

* Original beta release.
