{
 * FmMain.pas
 *
 * Application's main form. Handles the program's user initiated actions,
 * output, control layout and navigation history.
 *
 * v0.1 of 30 Jan 2005  - Original version.
 * v0.2 of 19 Feb 2005  - Refactoring: changed to work with renamed
 *                        IOverviewActionMgr, IDetailActionMgr,
 *                        ICompCheckActionMgr and IInfoActionMgr interfaces.
 * v0.3 of 20 Feb 2005  - Stopped welcome page menu option being disabled when
 *                        no database available.
 *                      - Added actions to compiler check frame to handle
 *                        database download and quick start help.
 * v0.4 of 22 Feb 2005  - Added a notifier object that is passed to frames an
 *                        web browser external object extender to use to trigger
 *                        actions in response to user input.
 *                      - Added new browser external object extender object that
 *                        it provided to frame objects that contain web browser
 *                        controls.
 * v0.5 of 22 Feb 2005  - Localised string literals.
 * v0.6 of 23 Feb 2005  - Changed new include new versions of information and
 *                        compiler check frames with new control names.
 * v0.7 of 23 Feb 2005  - Changed all code that displays subsidiary dialogs to
 *                        call static Execute methods rather than instantiating
 *                        the dialog classes in this code.
 *                      - Search dialogs now return search objects rather than
 *                        criteria: changed code that uses them accordingly.
 * v0.8 of 23 Feb 2005  - Changed startup code and access to welcome page:
 *                      - Removed dialog boxes that display on start up when
 *                        database is emtpy: the welcome page now informs user
 *                        when the database is empty.
 *                      - Made code that displays welcome page on startup
 *                        independent of whether welcome page menu option is
 *                        enabled.
 * v0.9 of 24 Feb 2005  - Now displays dialog boxes via new TMessageBox class.
 *                        Removed TPJWinMsgDlg component as a consequence.
 * v0.10 of 25 Feb 2005 - Fixed bug: Ctrl+F was not displaying Find Dialog
 *                        (Ctrl+F had also been assigned to Search menu item).
 * v0.11 of 05 Mar 2005 - Added new top level "Tools" menu item with single menu
 *                        item "Configure Compilers" that displays "Configure
 *                        Compilers" dialog box.
 *                      - Added missing ellipsis to about box action.
 * v0.12 of 17 Mar 2005 - Added new menu item / tool button to save selected
 *                        routine or category to disk along with custom save
 *                        dialog.
 * v0.13 of 21 Apr 2005 - Changed code that persists main form settings to use
 *                        revised UAppInfo code.
 * v0.14 of 25 Apr 2005 - Added new instance of custom action and handlers to
 *                        access help topic by ALink keyword.
 *                      - Passed action used to display configure compilers
 *                        dialog box to notifier object.
 *                      - Removed code that sets obsolete quick start help
 *                        action in notifier object.
 *                      - Added new "About Compiler Checks" help menu item and
 *                        associated action.
 *                      - Indirected quick start and compiler check help actions
 *                        via new custom help topic action.
 *                      - Ensured "Test Compile Routine" action disabled when no
 *                        compilers installed with program.
 * v0.15 of 30 Nov 2005 - Deleted setting of Application.HelpFile.
 *                      - Changed help contents action to call HelpMgr and set
 *                        Help | Contents menu item and Help tool button to call
 *                        new action.
 *                      - Deleted OnExecute event for fActHelpTopic.
 * v0.16 of 06 Jan 2006 - Added new copy Snippet menu option / action that
 *                        selected code snippet to clipboard.
 *                      - Added new Preferences menu option / action that
 *                        displays preferences dialog box.
 *                      - Changed to use TSaveSnippetMgr to determine whether a
 *                        view item is savable.
 * v0.17 of 10 Jan 2006 - Removed unsupported properties from form on reversion
 *                        to Delphi 7.
 * v0.18 of 28 Jan 2006 - Modified to refresh display after configuring
 *                        compilers.
 * v0.19 of 02 Apr 2006 - Made explicitly load Snippets object when form is
 *                        shown.
 *                      - Changed to use renamed Snippets.Load method to load
 *                        and re-load Snippets object.
 *                      - Made to display and swallow exceptions during
 *                        Snippets.Load to enable
 *                        (a) display to be correctly initialised and
 *                        (b) to prevent exceptions propagating to WebBrowser
 *                            control if action initiated there.
 *                      - Extracted common code shared by search actions into
 *                        single method.
 * v0.20 of 04 Apr 2006 - Removed old AppInfo object - now use TWindowSettings
 *                        object to persist main window information.
 *                      - Removed TPJWdwState component and used new dynamically
 *                        create TWindowSettings custom component.
 *                      - Used TWindowSettings to store splitter position
 *                        instead of dedicated code.
 *                      - Removed unused resourcestrings.
 * v0.21 of 07 Apr 2006 - Added new registration action and menu item to Help
 *                        menu and code to support it to display Registration
 *                        dialog. Menu item only visible if app is not
 *                        registered. Code also added to detect if registration
 *                        performed via About box.
 * v0.22 of 16 Apr 2006 - Added object that prevents drag drop on web browser
 *                        controls and passed to information and compiler check
 *                        frames.
 *                      - Changed to use new IWBCustomiser interface to perform
 *                        all web browser customisation.
 * v1.0 of 09 Jun 2006  - Improved and corrected comments.
 *                      - Removed unused unit reference.
 *                      - Replaced use of Application.OnException with
 *                        appEvents.OnException.
 *                      - Added several new glyphs to actions / menu items.
 *                      - No longer refreshes display after Configure Compilers
 *                        dialog is cancelled.
 *                      - Deleted duplicate glyph from image list.
 *                      - Added new actions to display license and to access
 *                        CodeSnip home page on web.
 *                      - Moved bug report and registration commands from Help
 *                        to Tools menu.
 *                      - Reworked Help menu, adding License display option and
 *                        making web access options part of new sub menu.
 * v1.1 of 25 Oct 2006  - Renamed some menu items to confirm with naming
 *                        standards.
 *                      - Changed to use renamed UShellExecAction unit and
 *                        renamed TShellExecAction class and its ResourceName
 *                        property.
 * v1.2 of 26 Oct 2006  - Changed to use TAppInfo.LicenseFileName to get full
 *                        file path of licence file rather than hard-wiring it.
 * v1.3 of 29 Oct 2006  - Added new Select Routines action, menu item, tool
 *                        button and code to display Select Routines dialog box.
 *                      - Added new Find Cross References action, menu item and
 *                        code to display Find Cross References dialog box.
 *                      - Added new Save Unit action, menu item, tool button and
 *                        code to display Save Unit dialog box.
 *                      - Changed to use renamed USaveSourceDlg unit and
 *                        TSaveSourceDlg class.
 *                      - Changed glyph used for Save Snippets action.
 *                      - Changed to use new UGlobals.cFullProgName constant for
 *                        application title.
 *                      - Added Ctrl+S shortuct to Save Snippets action.
 * v1.4 of 12 Nov 2006  - Corrected glyph associated with Show All search
 *                        action.
 *                      - Moved Tools | Preferences menu item to top of Tools
 *                        menu.
 * v1.5 of 14 Nov 2006  - Changed license action to display license topic in
 *                        help file rather than license text file in external
 *                        text editor.
 * v1.6 of 18 Nov 2006  - Added new action that displays Join Mailing List
 *                        dialog box. Also added corresponding tools menu item.
 * v1.7 of 23 Nov 2006  - Added new action that shows / hides test units in
 *                        Compiler Check pane.
 *                      - Changed calls to .Free to calls to FreeAndNil.
 * v1.8 of 01 Dec 2006  - Changed so that a wait dialog box may be displayed
 *                        during relatively lengthy test compilations.
 * v1.9 of 03 Dec 2006  - Fixed bug where Test Compile actions were always
 *                        enabled, permitting action to be triggered when no
 *                        routine selected or no available compilers. This was
 *                        causing an assertion failure.
 * v1.10 of 04 Dec 2006 - Added View / Hide Test unit menu item and added glyphs
 *                        to existing action.
 * v1.11 of 03 Feb 2007 - Added support for displaying prompts and database and
 *                        search related information in status bar.
 *                      - Added event handlers to intercept hints to make them
 *                        work with revised status bar code.
 *                      - Now uses new TStatusBarMgr object to assist in
 *                        managing status bar.
 * v1.12 of 04 Feb 2007 - Changed to use Query object for filtering database
 *                        rather than calling display manager.
 *                      - Now uses global query object to find details of
 *                        current search query rather than use TDetailView
 *                        object.
 *                      - Changed to use renamed CurrentView property of main
 *                        display manager.
 * v1.13 of 04 Feb 2007 - Changed QuickStart, License and Compiler Checks help
 *                        actions to directly call inherited DisplayHelp method
 *                        rather than triggering special help action.
 * v1.14 of 08 Feb 2007 - Tidied up code and reduced number of required units:
 *                      - Removed custom action and custom menu fields and
 *                        constructors and created them on the fly. Used new
 *                        action factory class to create actions.
 *                      - Deleted erroneous code that loaded non-existing
 *                        resource bitmap from FormCreate event.
 *                      - Removed fWBExternal and fWBNulDropTarget fields and
 *                        replaced them with local variables inside FormCreate
 *                        event.
 *                      - Replaced FormKeyDown event handler used to generated
 *                        test bug exception with action that does same thing.
 *                      - Changed actTestCompileIndirectExecute to use new
 *                        static Run method of TWaitForActionUI class.
 *                      - Changed to use TDialogMgr to display main dialog
 *                        boxes.
 * v1.15 of 11 Feb 2007 - Changed info and detail frames so they are not tab
 *                        stops - now only tab controls, routine tree view and
 *                        current browser control are tab stops.
 *                      - Removed appEvents OnMessage handler. This had been
 *                        used to active browser control. This is now handled
 *                        automatically in host frames.
 *                      - Added new actions to trap Ctrl+Tab and Shift+Ctrl+Tab
 *                        and switch between tabs in currently active tab set.
 *                        This means that both tab sets have same response to
 *                        key presses.
 * v1.16 of 16 Feb 2007 - Fixed history list bug by clearing history list after
 *                        updating database.
 *                      - Also cleared main display while loading updated
 *                        database to prevent a pre-update routine being
 *                        selected while display was being redrawn.
 * v1.17 of 16 Feb 2007 - Changed to work with revised Main Display Manager and
 *                        Details frames. Removed all references Detail frame's
 *                        child frames.
 *                      - Removed all references to browser controls.
 *                      - Slightly modified conditions under which Select All
 *                        and Copy actions are made available.
 * v1.18 of 25 Feb 2007 - Moved most of code from FormCreate and all of code
 *                        FormShow to overriden InitForm method.
 *                      - Added code to request closing splash screen that was
 *                        opened in main project file.
 *                      - Form is now disabled when initialising and when
 *                        updated database is being loaded. Added code to
 *                        disable main actions when the form is disabled.
 * v1.19 of 01 Mar 2007 - Removed actTestCompile and its events and renamed
 *                        actTestCompileIndirect back to actTestCompile.
 *                      - Changed to run compilation in a separate thread via
 *                        TWaitForActionUI instead of executing action via the
 *                        class.
 *                      - Added Update database action that enables database to
 *                        be loaded while a waiy dialpg box is displayed.
 *                      - No longer disable form when database re-loading after
 *                        updating. We display modal dialog instead.
 *                      - Modified LoadSnippets to load database via a thread.
 * v1.20 of 04 Mar 2007 - Fixed bug where Register CodeSnip and Show/Hide Test
 *                        Unit menu options were always disabled.
 * v1.21 of 09 May 2007 - Fixed bug that permitted new routine to be selected
 *                        while compiling routines by disabling form during
 *                        compilation.
 * v1.22 of 07 Sep 2007 - Added new print and page setup actions and menu items
 *                        and added a print tool button.
 * v1.23 of 22 Sep 2007 - Moved Help | License menu option up the menu to help
 *                        section.
 *                      - Added Help | Privacy Statement menu option and
 *                        associated action that displays help message.
 * v1.24 of 24 Sep 2007 - Lengthened time before wait dialog appears when test-
 *                        compiling.
 * v1.25 of 31 Oct 2007 - Added support for pop-up menus in detail pane.
 * v1.26 of 04 Nov 2007 - Removed call to INotifier.SetHelpTopicAction and
 *                        associated creation of THelpTopicAction instance.
 * v1.27 of 21 Apr 2008 - Removed XPMan unit reference: functionality replaced
 *                        by new manifest resource.
 * v1.28 of 05 Jun 2008 - Changed to persist selected tabs in overview and
 *                        detail panes.
 *                      - Modified to work with revised interface to main
 *                        display manager for working with pane tabs.
 *                      - Added update event handlers for tab selection actions
 *                        that check selected tabs.
 * v1.29 of 12 Jun 2008 - Modified to change task bar handling. Application
 *                        object's hidden window no longer used for task bar.
 *                        Main form window now used for task bar. This change
 *                        required for compatibility with Vista.
 * v1.30 of 25 Aug 2008 - Code that performs test compile in a thread while
 *                        displaying a wait dialog was extracted into the
 *                        UTestCompileUI unit. This new unit improves handling
 *                        of cases when compiler fails to run.
 * v1.31 of 14 Sep 2008 - Changed to work with redefined methods in Snippets
 *                        object to support user defined database.
 *                      - Added event handler for new Snippets change event.
 *                      - Added database menu and other actions to support
 *                        adding, deleting and updating routines and for backing
 *                        up and restoring user defined database. Moved Database
 *                        update and Test Compile from File to Database menu.
 *                      - Added new Save Database and New, Edit and Delete
 *                        Snippet glyph and revised Save Snippet and Save unit
 *                        gylphs.
 *                      - Fixed display bug in status bar by switching AutoHint
 *                        off.
 * v1.32 of 18 Sep 2008 - Added code to hide splash form and enable form if
 *                        exception raised during initialisation.
 *                      - Now disable form while database is reloading.
 * v1.33 of 19 Sep 2008 - Now detects if user database has changed and offers
 *                        user choice of whether to save before main database is
 *                        updated.
 * v1.34 of 04 Oct 2008 - Changed to work with revised static methods of
 *                        TCopySnippetMgr, TSaveSnippetMgr, TSaveUnitMgr,
 *                        TPrintMgr and TThreadWrapper.
 *                      - Now use TPrintMgr.CanPrint method to check if a view
 *                        item can be printed.
 *                      - Added assertion that a routine is in standard format
 *                        to TestCompile action execution.
 *                      - Corrected routine and class names in assertions.
 *                      - Made all action update events refer to Sender rather
 *                        than explicit actions. Grouped some common updates to
 *                        share same event handlers.
 *                      - Made all resource strings local to methods.
 *                      - Now use ClassName method in all assert and raise EBug
 *                        statements.
 * v1.35 of 07 Dec 2008 - Added facility to submit user-defined routines to the
 *                        online code snippets database.
 *                      - Added facility to export and import user-defined
 *                        routines.
 * v1.36 of 04 Jan 2009 - Changed name of Copy Snippet menu item to Copy Source
 *                        Code.
 *                      - Added new Copy Information menu item to Copy menu and
 *                        detail pane context menu.
 *                      - Added Save Database button to toolbar.
 *                      - Modified to work with revised toolbar.
 *                      - Removed code that sets control state when form's
 *                        enabled state changes. Support now provided in FmBase.
 * v1.37 of 06 Jan 2009 - Changed to use revised TTestCompileUI.Execute
 *                        parameter signature.
 * v1.38 of 10 Jan 2009 - Changed to use a manager object to manage compilations
 *                        triggered from main program. No longer uses global
 *                        Compilers singleton. New manager also manages compile
 *                        related UI and dialogs.
 *                      - Added Database menu option to show last compile errors
 *                        and warnings for selected routine if available.
 * v1.39 of 14 Jan 2009 - Replaced control char literals with constants.
 * v1.40 of 10 May 2009 - Added new Donate action, menu item and glyph.
 * v1.41 of 13 May 2009 - Full program name now obtained from TAppInfo instead
 *                        of UGlobals unit.
 *                      - Browse actions that access website now have URLs set
 *                        using information from TWebInfo. Hints were modified.
 * v1.42 of 23 Jun 2009 - Added new View | Dependencies menu option and action.
 *                      - View Test Unit action now displays a dialog box rather
 *                        than unit in compiler check pane. Hide Test Unit
 *                        action now removed.
 *                      - Actions and menu items added to expand/collapse tree
 *                        nodes in Overview pane.
 *                      - Made changes to support new and changed overview pane
 *                        tabs.
 *                      - Tab select actions now identify tab via action's Tag
 *                        property.
 *                      - Revised text, hot keys and/or glyphs of some actions /
 *                        menu items.
 *                      - Code added to dynamically define toolbar and popup
 *                        menu in overview frame.
 *                      - Altered to work with revised Notifier object.
 *                      - Retitled Copy Source Code action as Copy Snippet and
 *                        added new Copy Source Code action to copy a snippet's
 *                        plain source code, without annotations, to clipboard.
 *                      - Replaced use of "routine" with "snippet" in text where
 *                        appropriate.
 *                      - Made private and protected sections strict.
 *                      - Now deletes history if a snippet is changed: could
 *                        cause GPF in history's dependency references.
 * v1.43 of 11 Jul 2009 - Removed reference to deleted UCopySnippetMgr unit.
 * v1.44 of 13 Jul 2009 - Added a new dynamically created category display
 *                        action that is passed to notifier.
 *                      - Removed OnExecute handler for dynamically created
 *                        TRoutineAction: action now displays snippet in its
 *                        Execute method. Removed URoutineAction unit reference.
 *                      - Changed actEditSnippetExecute to call notifier rather
 *                        than call user database manager directly.
 *                      - Changed a menu item name.
 *
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is FmMain.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FmMain;


interface


uses
  // Delphi
  Menus, ExtActns, StdActns, Classes, ActnList, ImgList, Controls, Forms,
  ExtCtrls, ComCtrls, ToolWin, Messages, AppEvnts,
  // Project
  FmHelpAware, FrDetail, FrOverview, FrTitled, IntfNotifier, UCompileMgr,
  UDialogMgr, UHistory, UMainDisplayMgr, USearch, UStatusBarMgr,
  UWindowSettings;


type

  {
  TMainForm:
    Implements the application's main form. Handles the program's user initiated
    actions, output, control layout and navigation history.
  }
  TMainForm = class(THelpAwareForm)
    actAbout: TAction;
    actAddSnippet: TAction;
    actBackupDatabase: TAction;
    actBugReport: TAction;
    actCollapseNode: TAction;
    actCollapseTree: TAction;
    actCompilers: TAction;
    actCopy: TAction;
    actCopyInfo: TAction;
    actCopySnippet: TAction;
    actCopySource: TAction;
    actDeleteSnippet: TAction;
    actDonate: TAction;
    actEditSnippet: TAction;
    actExit: TFileExit;
    actExpandNode: TAction;
    actExpandTree: TAction;
    actExportCode: TAction;
    actFindClear: TAction;
    actFindCompiler: TAction;
    actFindText: TAction;
    actFindXRefs: TAction;
    actGoBack: TAction;
    actGoForward: TAction;
    actHelpCompChecks: TAction;
    actHelpContents: TAction;
    actHelpQuickStart: TAction;
    actHomePage: TBrowseURL;
    actImportCode: TAction;
    actLicense: TAction;
    actLoadDatabase: TAction;
    actMailingList: TAction;
    actNextTab: TAction;
    actPageSetup: TAction;
    actPreferences: TAction;
    actPreviousTab: TAction;
    actPrint: TAction;
    actPrivacy: TAction;
    actRegister: TAction;
    actRestoreDatabase: TAction;
    actSaveDatabase: TAction;
    actSaveSnippet: TAction;
    actSaveUnit: TAction;
    actSelectAll: TAction;
    actSelectRoutines: TAction;
    actSubmit: TAction;
    actTestBug: TAction;
    actTestCompile: TAction;
    actUpdateDbase: TAction;
    actViewAlphabetical: TAction;
    actViewCategorised: TAction;
    actViewCompErrs: TAction;
    actViewCompCheck: TAction;
    actViewDependencies: TAction;
    actViewInfo: TAction;
    actViewSnippetKinds: TAction;
    actViewTestUnit: TAction;
    actWebSite: TBrowseURL;
    actWelcome: TAction;
    alMain: TActionList;
    appEvents: TApplicationEvents;
    frmDetail: TDetailFrame;
    frmOverview: TOverviewFrame;
    ilMain: TImageList;
    miAbout: TMenuItem;
    miAddSnippet: TMenuItem;
    miBackupDatabase: TMenuItem;
    miCollapseNode: TMenuItem;
    miCollapseTree: TMenuItem;
    miCompilers: TMenuItem;
    miCopy: TMenuItem;
    miCopyInfo: TMenuItem;
    miCopySnippet: TMenuItem;
    miDatabase: TMenuItem;
    miDeleteSnippet: TMenuItem;
    miDonate: TMenuItem;
    miEdit: TMenuItem;
    miEditSnippet: TMenuItem;
    miExit: TMenuItem;
    miExpandNode: TMenuItem;
    miExpandTree: TMenuItem;
    miExportCode: TMenuItem;
    miFile: TMenuItem;
    miFindClear: TMenuItem;
    miFindCompiler: TMenuItem;
    miFindText: TMenuItem;
    miFindXRefs: TMenuItem;
    miGoBack: TMenuItem;
    miGoForward: TMenuItem;
    miHelp: TMenuItem;
    miHelpCompChecks: TMenuItem;
    miHelpContents: TMenuItem;
    miHelpQuickStart: TMenuItem;
    miHomePage: TMenuItem;
    miImportCode: TMenuItem;
    miLicense: TMenuItem;
    miMailingList: TMenuItem;
    miPageSetup: TMenuItem;
    miPreferences: TMenuItem;
    miPrint: TMenuItem;
    miPrivacy: TMenuItem;
    miRegister: TMenuItem;
    miReportBug: TMenuItem;
    miRestoreDatabase: TMenuItem;
    miSaveSnippet: TMenuItem;
    miSaveDatabase: TMenuItem;
    miSaveUnit: TMenuItem;
    miSearch: TMenuItem;
    miSelectRoutines: TMenuItem;
    miSelectAll: TMenuItem;
    miSourceCode: TMenuItem;
    miSpacer1: TMenuItem;
    miSpacer2: TMenuItem;
    miSpacer3: TMenuItem;
    miSpacer4: TMenuItem;
    miSpacer5: TMenuItem;
    miSpacer6: TMenuItem;
    miSpacer7: TMenuItem;
    miSpacer8: TMenuItem;
    miSpacer9: TMenuItem;
    miSpacer10: TMenuItem;
    miSpacer11: TMenuItem;
    miSpacer12: TMenuItem;
    miSpacer13: TMenuItem;
    miSpacer14: TMenuItem;
    miSpacer15: TMenuItem;
    miSpacer16: TMenuItem;
    miSubmit: TMenuItem;
    miTestCompile: TMenuItem;
    miTools: TMenuItem;
    miUpdateDbase: TMenuItem;
    miView: TMenuItem;
    miViewCategorised: TMenuItem;
    miViewCompCheck: TMenuItem;
    miViewCompErrs: TMenuItem;
    miViewDependencies: TMenuItem;
    miViewInfo: TMenuItem;
    miViewSnippetKinds: TMenuItem;
    miViewTestUnit: TMenuItem;
    miViewAlphabetical: TMenuItem;
    miWeb: TMenuItem;
    miWebSite: TMenuItem;
    miWelcome: TMenuItem;
    mnuMain: TMainMenu;
    pnlBody: TPanel;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    sbStatusBar: TStatusBar;
    splitVert: TSplitter;
    tbarMain: TToolBar;
    tbAddSnippet: TToolButton;
    tbCopy: TToolButton;
    tbDeleteSnippet: TToolButton;
    tbEditSnippet: TToolButton;
    tbFindClear: TToolButton;
    tbFindCompiler: TToolButton;
    tbFindText: TToolButton;
    tbGoBack: TToolButton;
    tbGoForward: TToolButton;
    tbHelpContents: TToolButton;
    tbPrint: TToolButton;
    tbSaveDatabase: TToolButton;
    tbSaveSnippet: TToolButton;
    tbSaveUnit: TToolButton;
    tbSelectRoutines: TToolButton;
    tbSpacer1: TToolButton;
    tbSpacer2: TToolButton;
    tbSpacer3: TToolButton;
    tbSpacer4: TToolButton;
    tbSpacer5: TToolButton;
    tbSpacer6: TToolButton;
    tbSpacer7: TToolButton;
    tbSpacer8: TToolButton;
    tbTestCompile: TToolButton;
    tbUpdateDbase: TToolButton;
    actProxyServer: TAction;
    miProxyServer: TMenuItem;
    procedure actAboutExecute(Sender: TObject);
    procedure actAddSnippetExecute(Sender: TObject);
    procedure actBackupDatabaseExecute(Sender: TObject);
    procedure actBugReportExecute(Sender: TObject);
    procedure actCompilersExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCopyInfoUpdate(Sender: TObject);
    procedure actCopyInfoExecute(Sender: TObject);
    procedure actCopySnippetExecute(Sender: TObject);
    procedure actCopySnippetUpdate(Sender: TObject);
    procedure actCopySourceExecute(Sender: TObject);
    procedure actCopySourceUpdate(Sender: TObject);
    procedure actCopyUpdate(Sender: TObject);
    procedure actDeleteSnippetExecute(Sender: TObject);
    procedure ActDetailTabExecute(Sender: TObject);
    procedure ActDetailTabUpdate(Sender: TObject);
    procedure actDonateExecute(Sender: TObject);
    procedure ActEditDeleteSnippetUpdate(Sender: TObject);
    procedure actExportCodeExecute(Sender: TObject);
    procedure actEditSnippetExecute(Sender: TObject);
    procedure actFindClearExecute(Sender: TObject);
    procedure actFindClearUpdate(Sender: TObject);
    procedure actFindCompilerExecute(Sender: TObject);
    procedure actFindTextExecute(Sender: TObject);
    procedure actFindXRefsExecute(Sender: TObject);
    procedure actFindXRefsUpdate(Sender: TObject);
    procedure actGoBackExecute(Sender: TObject);
    procedure actGoBackUpdate(Sender: TObject);
    procedure actGoForwardExecute(Sender: TObject);
    procedure actGoForwardUpdate(Sender: TObject);
    procedure actHelpCompChecksExecute(Sender: TObject);
    procedure actHelpContentsExecute(Sender: TObject);
    procedure actHelpQuickStartExecute(Sender: TObject);
    procedure actImportCodeExecute(Sender: TObject);
    procedure actLicenseExecute(Sender: TObject);
    procedure actLoadDatabaseExecute(Sender: TObject);
    procedure actMailingListExecute(Sender: TObject);
    procedure actNextTabExecute(Sender: TObject);
    procedure ActNonEmptyDBUpdate(Sender: TObject);
    procedure ActOverviewTabExecute(Sender: TObject);
    procedure ActOverviewTabUpdate(Sender: TObject);
    procedure actPageSetupExecute(Sender: TObject);
    procedure actPreferencesExecute(Sender: TObject);
    procedure actPreviousTabExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure actPrintUpdate(Sender: TObject);
    procedure actPrivacyExecute(Sender: TObject);
    procedure actRegisterExecute(Sender: TObject);
    procedure actRegisterUpdate(Sender: TObject);
    procedure actRestoreDatabaseExecute(Sender: TObject);
    procedure actSaveDatabaseExecute(Sender: TObject);
    procedure actSaveDatabaseUpdate(Sender: TObject);
    procedure actSaveSnippetExecute(Sender: TObject);
    procedure actSaveSnippetUpdate(Sender: TObject);
    procedure actSaveUnitExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actSelectAllUpdate(Sender: TObject);
    procedure actSelectRoutinesExecute(Sender: TObject);
    procedure actSubmitExecute(Sender: TObject);
    procedure ActSubmitOrExportUpdate(Sender: TObject);
    procedure actTestBugExecute(Sender: TObject);
    procedure actTestCompileExecute(Sender: TObject);
    procedure actTestCompileUpdate(Sender: TObject);
    procedure ActTreeStateChangeExecute(Sender: TObject);
    procedure ActTreeStateChangeUpdate(Sender: TObject);
    procedure actUpdateDbaseExecute(Sender: TObject);
    procedure actViewCompErrsExecute(Sender: TObject);
    procedure actViewCompErrsUpdate(Sender: TObject);
    procedure actViewDependenciesExecute(Sender: TObject);
    procedure actViewDependenciesUpdate(Sender: TObject);
    procedure actViewTestUnitExecute(Sender: TObject);
    procedure actViewTestUnitUpdate(Sender: TObject);
    procedure actWelcomeExecute(Sender: TObject);
    procedure appEventsHint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure splitVertCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure actProxyServerExecute(Sender: TObject);
  strict private
    fIsAppRegistered: Boolean;        // Flag noting if app is registered
    fNotifier: INotifier;             // Notififies app of user-initiated events
    fHistory: THistory;               // Maintains a history of the items viewed
    fWindowSettings: TWindowSettings; // Saves and restores main window settings
    fMainDisplayMgr: TMainDisplayMgr; // Manages the main display output
    fStatusBarMgr: TStatusBarMgr;     // Manages status bar display
    fDialogMgr: TDialogMgr;           // Manages display of dialog boxes
    fCompileMgr: TMainCompileMgr;     // Manage test compilations
    procedure WMSyscommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
      {Handles system command messages. Overrides default processing of
      minimizing and restoration of main window. This is required now we have
      inhibited application object's default processing of these messages.
        @param Msg [in/out] Details of system command. Result field set to 0 if
          we handle message to prevent default processing.
      }
    procedure ActViewItemExecute(Sender: TObject);
      {Displays a requested view item and records in history.
        @param Sender [in] Action triggering this event. Must be a
          TViewItemAction.
      }
    procedure ActViewHistoryItemExecute(Sender: TObject);
      {Displays requested history item and selects it in the history list.
        @param Sender [in] Action triggering this event. Must be a
          TViewItemAction.
      }
    procedure ActEditRoutineExecute(Sender: TObject);
      {Edits a named user defined snippet.
        @param Sender [in] Action triggering this event. Must be a
          TEditRoutineAction.
      }
    procedure ActViewCompLogExecute(Sender: TObject);
      {Displays compiler warning or error log for last compile by a specified
      compiler.
        @param Sender [in] Action triggering this event. Must be a
          TCompLogAction.
        @except Raised if last compiler result was not an error or a warning.
      }
    procedure ActBrowserHintExecute(Sender: TObject);
      {Displays hint from browser hint action in status bar.
        @param Sender [in] Not used.
      }
    procedure SnippetsChangeHandler(Sender: TObject; const EvtInfo: IInterface);
      {Handles Snippets change event handler that is trigerred when a user
      defined entry in the database changes.
        @param Sender [in] Not used.
        @para EvtInfo [in] Object providing information about the event.
      }
    procedure DisplayWelcomePage;
      {Displays welcome page in currently active detail pane.
      }
    procedure DisplayHint(const Hint: string);
      {Displays hint in status bar using status bar manager.
        @param Hint [in] Hint to be displayed.
      }
    procedure LoadSnippets;
      {Loads Snippets object from database and re-intitialises display.
      }
    procedure ReloadDatabase;
      {Reloads the whole database in a thread.
      }
    procedure DoSearchFilter(const Search: ISearch);
      {Filters main display using search object and displays message if no
      snippets found.
        @param Search [in] Search object to filter by.
      }
  strict protected
    procedure CreateParams(var Params: TCreateParams); override;
      {Updates style of window to ensure this main window appears on task bar.
        @params Params [in/out] In: current parameters. Out: adjusted
          parameters: we update ExStyle field with new window styles.
      }
    procedure InitForm; override;
      {Initialises form and creates and configures owned objects. Once
      initialisation is complete splash window is canclled and form enabled.
      }
  end;


var
  MainForm: TMainForm;


implementation


uses
  // Delphi
  SysUtils, Windows,
  // Project
  FmSplash, FmWaitDlg, IntfFrameMgrs, IntfWBPopupMenus,
  UActionFactory, UAppInfo, UCodeShareMgr, UCommandBars, UCompLogAction,
  UConsts, UCopyInfoMgr, UCopySourceMgr, UDatabaseLoader, UEditRoutineAction,
  UExceptions, UHelpMgr, UHistoryMenus, UMessageBox, UNotifier, UPrintMgr,
  UQuery, USaveSnippetMgr, USaveUnitMgr, USnippets, UThreadWrapper, UUserDBMgr,
  UView, UViewItemAction, UWaitForActionUI, UWBExternal, UWBNulDropTarget,
  UWebInfo;


{$R *.dfm}


const
  // Minimum sizes of various display parts: used when resizing
  cMinLeftPanelWidth  = 160;  // min width of left panel
  cMinRightPanelWidth = 300;  // min width of right panel
  // Default values of various display parts
  cDefLeftPanelWidth  = 186;


{ TMainForm }

procedure TMainForm.actAboutExecute(Sender: TObject);
  {Displays about box.
    @param Sender [in] Not used.
  }
begin
  fDialogMgr.ShowAboutDlg;
  if not fIsAppRegistered then
    fIsAppRegistered := TAppInfo.IsRegistered;
end;

procedure TMainForm.actAddSnippetExecute(Sender: TObject);
  {Gets a new user snippet from user.
    @param Sender [in] Not used.
  }
begin
  TUserDBMgr.Add;
end;

procedure TMainForm.actBackupDatabaseExecute(Sender: TObject);
  {Makes a backup of the user database.
    @param Sender [in] Not used.
  }
begin
  TUserDBMgr.BackupDatabase;
end;

procedure TMainForm.ActBrowserHintExecute(Sender: TObject);
  {Displays hint from browser hint action in status bar.
    @param Sender [in] Action triggering this event.
  }
begin
  DisplayHint((Sender as THintAction).Hint);
end;

procedure TMainForm.actBugReportExecute(Sender: TObject);
  {Displays bug report dialog box.
    @param Sender [in] Not used.
  }
begin
  fDialogMgr.ShowBugReportDlg;
end;

procedure TMainForm.actCompilersExecute(Sender: TObject);
  {Displays Configure Compilers dialog box.
    @param Sender [in] Not used.
  }
begin
  if fCompileMgr.ConfigCompilers then
    fMainDisplayMgr.Refresh;
end;

procedure TMainForm.actCopyExecute(Sender: TObject);
  {Copies selected text / html to clipboard from current control.
    @param Sender [in] Not used.
  }
begin
  fMainDisplayMgr.CopyToClipboard;
end;

procedure TMainForm.actCopyInfoExecute(Sender: TObject);
  {Copies information about selected snippet to clipboard.
    @param Sender [in] Not used.
  }
begin
  TCopyInfoMgr.Execute(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actCopyInfoUpdate(Sender: TObject);
  {Enables / disables copy information action according to whether a snippet is
  selected.
    @param Sender [in] Action triggering the event.
  }
begin
  (Sender as TAction).Enabled :=
    TCopyInfoMgr.CanHandleView(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actCopySnippetExecute(Sender: TObject);
  {Copies annotated source of selected routine or category to clipboard.
    @param Sender [in] Not used.
  }
begin
  TCopySnippetMgr.Execute(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actCopySnippetUpdate(Sender: TObject);
  {Enables / disables copy snippet action according to whether copy manager
  supports selected view.
    @param Sender [in] Action triggering the event.
  }
begin
  (Sender as TAction).Enabled :=
    TCopySnippetMgr.CanHandleView(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actCopySourceExecute(Sender: TObject);
  {Copies plain source code of selected snippet.
    @param Sender [in] Not used.
  }
begin
  TCopySourceMgr.Execute(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actCopySourceUpdate(Sender: TObject);
  {Enables / disables copy source code action according to whether copy manager
  supports selected view.
    @param Sender [in] Action triggering the event.
  }
begin
  (Sender as TAction).Enabled :=
    TCopySourceMgr.CanHandleView(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actCopyUpdate(Sender: TObject);
  {Enables / disables Copy action according to whether relevant controls are
  able to copy to clipboard.
    @param Sender [in] Action triggering the event.
  }
begin
  (Sender as TAction).Enabled := fMainDisplayMgr.CanCopy;
end;

procedure TMainForm.actDeleteSnippetExecute(Sender: TObject);
  {Deletes currently selected user defined snippet.
    @param Sender [in] Not used.
  }
begin
  Assert(TUserDBMgr.CanEdit(fMainDisplayMgr.CurrentView),
    ClassName + '.actDeleteSnippetExecute: Can''t delete current view item');
  TUserDBMgr.Delete(fMainDisplayMgr.CurrentView);
  // display updated is handled by snippets change event handler
end;

procedure TMainForm.ActDetailTabExecute(Sender: TObject);
  {Selects a tab in the detail pane.
    @param Sender [in] Action triggering this event
  }
begin
  // Action's Tag property specifies index of tab being selected
  fMainDisplayMgr.SelectedDetailTab := (Sender as TAction).Tag;
end;

procedure TMainForm.ActDetailTabUpdate(Sender: TObject);
  {Updates checked state of detail pane tab selection action according to if
  associated tab is selected.
    @param Sender [in] Action triggering this event.
  }
begin
  // Action's Tag property specifies index of tab being updated
  with Sender as TAction do
  begin
    Checked := fMainDisplayMgr.SelectedDetailTab = Tag;
    Enabled := True;
  end;
end;

procedure TMainForm.actDonateExecute(Sender: TObject);
  {Displays the Donate dialog box.
    @param Sender [in] Not used.
  }
begin
  fDialogMgr.ShowDonateDlg
end;

procedure TMainForm.ActEditDeleteSnippetUpdate(Sender: TObject);
  {Determines whether the Delete Snippet action is enabled: a user defined
  snippet must be selected.
    @param Sender [in] Action triggering this event.
  }
begin
  (Sender as TAction).Enabled :=
    TUserDBMgr.CanEdit(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.ActEditRoutineExecute(Sender: TObject);
  {Edits a named user defined snippet.
    @param Sender [in] Action triggering this event. Must be a
      TEditRoutineAction.
  }
begin
  TUserDBMgr.Edit((Sender as TEditRoutineAction).RoutineName);
end;

procedure TMainForm.actEditSnippetExecute(Sender: TObject);
  {Displays selected user defined snippet in edit dialog box to enable user to
  make changes.
    @param Sender [in] Not used.
  }
begin
  Assert(TUserDBMgr.CanEdit(fMainDisplayMgr.CurrentView),
    ClassName + '.actEditSnippetExecute: Can''t edit current view item');
  fNotifier.EditRoutine(fMainDisplayMgr.CurrentView.Routine.Name);
  // display of updated snippet is handled by snippets change event handler
end;

procedure TMainForm.actExportCodeExecute(Sender: TObject);
  {Exports one or more selected user-defined snippets to a file.
    @param Sender [in] Not used.
  }
begin
  TCodeShareMgr.ExportCode(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actFindClearExecute(Sender: TObject);
  {Clears current search and restores all snippets in database.
    @param Sender [in] Not used.
  }
begin
  Query.Reset;
  fMainDisplayMgr.QueryUpdated;
  fStatusBarMgr.Update;
end;

procedure TMainForm.actFindClearUpdate(Sender: TObject);
  {Enables / disables Show All action according to if there's a current search.
    @param Sender [in] Action triggering the event.
  }
begin
  // We have an active search if current search's criteria is not nul
  (Sender as TAction).Enabled := not Query.CurrentSearch.IsNul;
end;

procedure TMainForm.actFindCompilerExecute(Sender: TObject);
  {Performs compiler search. Gets search from user via Find Compiler dialog box
  then displays all snippets that match the search criteria.
    @param Sender [in] Not used.
  }
var
  Search: USearch.ISearch;  // text search object
begin
  // Display Find Compiler dialog box to enable user to enter search criteria
  // (dialog box creates and returns search object from entered criteria)
  if fDialogMgr.ExecFindCompilerDlg(Search) then
    DoSearchFilter(Search);
end;

procedure TMainForm.actFindTextExecute(Sender: TObject);
  {Performs text search. Gets search from user via Find Text dialog box then
  displays all snippets that match the search criteria.
    @param Sender [in] Not used.
  }
var
  Search: USearch.ISearch;  // text search object
begin
  // Display Find Text dialog box to enable user to enter search criteria
  // (dialog box creates and returns search object from entered criteria)
  if fDialogMgr.ExecFindTextDlg(Search) then
    DoSearchFilter(Search);
end;

procedure TMainForm.actFindXRefsExecute(Sender: TObject);
  {Performs cross reference search. Gets search from user via Find Cross Refs
  dialog box then displays all snippets that match the search criteria.
    @param Sender [in] Not used.
  }
var
  Search: USearch.ISearch;  // cross reference search object
begin
  Assert(fMainDisplayMgr.CurrentView.Kind = vkRoutine,
    ClassName + '.actFindXRefsExecute: Current view kind is not vkRoutine');
  // Display Find Cross Refs dialog box to enable user to enter search criteria
  // (dialog box creates and returns search object from entered criteria)
  if fDialogMgr.ExecFindXRefsDlg(
    fMainDisplayMgr.CurrentView.Routine, Search
  ) then
    DoSearchFilter(Search);
end;

procedure TMainForm.actFindXRefsUpdate(Sender: TObject);
  {Enables / disables Find Cross Refs action according to whether a snippet is
  currently selected.
    @param Sender [in] Action triggering the event.
  }
begin
  (Sender as TAction).Enabled := fMainDisplayMgr.CurrentView.Kind = vkRoutine;
end;

procedure TMainForm.actGoBackExecute(Sender: TObject);
  {Displays previous item in history list.
    @param Sender [in] Not used.
  }
var
  ViewItem: TViewItem;  // previous view item in history list
const
  // Bug error message
  cHistoryError = '%s.actGoBackExecute: '
    + 'There are no items before current one in history list';
begin
  // Get previous view item from history list and check it is assigned
  ViewItem := fHistory.GoBack;
  if not Assigned(ViewItem) then
    raise EBug.CreateFmt(cHistoryError, [ClassName]);
  // Display item, but don't record in history list
  fMainDisplayMgr.DisplayViewItem(ViewItem);
end;

procedure TMainForm.actGoBackUpdate(Sender: TObject);
  {Enables / disables Go Back action according to whether there are items before
  current location in history.
    @param Sender [in] Action triggering the event.
  }
begin
  (Sender as TAction).Enabled := fHistory.BackListCount > 0;
end;

procedure TMainForm.actGoForwardExecute(Sender: TObject);
  {Displays next item in history list.
    @param Sender [in] Not used.
  }
var
  ViewItem: TViewItem;  // next view item in history list
const
  // Bug error message
  cHistoryError = '%s.actGoForwardExecute: '
    + 'There are no items after current one in history list';
begin
  // Get next view item from history list and check it is assigned
  ViewItem := fHistory.GoForward;
  if not Assigned(ViewItem) then
    raise EBug.CreateFmt(cHistoryError, [ClassName]);
  // Display item, but don't record in history list
  fMainDisplayMgr.DisplayViewItem(ViewItem);
end;

procedure TMainForm.actGoForwardUpdate(Sender: TObject);
  {Enables / disables Go Forward action according to whether there are items
  after current location in history.
    @param Sender [in] Action triggering the event.
  }
begin
  (Sender as TAction).Enabled := fHistory.ForwardListCount > 0;
end;

procedure TMainForm.actHelpCompChecksExecute(Sender: TObject);
  {Displays "About Compiler Checks" help topic.
    @param Sender [in] Not used.
  }
begin
  // Displays help topic indirected via custom help topic action
  DisplayHelp('CompChecks');
end;

procedure TMainForm.actHelpContentsExecute(Sender: TObject);
  {Displays help contents (TOC) at default page.
    @param Sender [in] Not used.
  }
begin
  HelpMgr.ShowContents;
end;

procedure TMainForm.actHelpQuickStartExecute(Sender: TObject);
  {Displays "Quick Start" help topic.
    @param Sender [in] Not used.
  }
begin
  // Displays help topic indirected via custom help topic action
  DisplayHelp('QuickStart');                               
end;

procedure TMainForm.actImportCodeExecute(Sender: TObject);
  {Exports one or more user-defined routines from a file.
    @param Sender [in] Not used.
  }
begin
  TCodeShareMgr.ImportCode;
end;

procedure TMainForm.actLicenseExecute(Sender: TObject);
  {Display program's license in help file.
    @param Sender [in] Not used.
  }
begin
  DisplayHelp('License');                                  
end;

procedure TMainForm.actLoadDatabaseExecute(Sender: TObject);
  {Loads snippets database.
    @param Sender [in] Not used.
  }
begin
  LoadSnippets;
end;

procedure TMainForm.actMailingListExecute(Sender: TObject);
  {Displays mailing list wizard.
    @param Sender [in] Not used.
  }
begin
  fDialogMgr.ShowMailingListDlg;
end;

procedure TMainForm.actNextTabExecute(Sender: TObject);
  {Displays next tab in either overview or details pane depending which is
  active.
    @param Sender [in] Not used.
  }
begin
  fMainDisplayMgr.SelectNextActiveTab;
end;

procedure TMainForm.ActNonEmptyDBUpdate(Sender: TObject);
  {Enables / disables an action according to whether there are routines in
  database.
    @param Sender [in] Action triggering this event.
  }
begin
  (Sender as TAction).Enabled := Snippets.Routines.Count > 0;
end;

procedure TMainForm.ActOverviewTabExecute(Sender: TObject);
  {Selects a tab in the overview pane.
    @param Sender [in] Action triggering this event
  }
begin
  // Action's Tag property specifies index of tab being selected
  fMainDisplayMgr.SelectedOverviewTab := (Sender as TAction).Tag;
end;

procedure TMainForm.ActOverviewTabUpdate(Sender: TObject);
  {Updates checked state of overview pane tab selection action according to if
  associated tab is selected.
    @param Sender [in] Action triggering this event.
  }
begin
  // Action's Tag property specifies index of tab being updated
  with Sender as TAction do
  begin
    Checked := fMainDisplayMgr.SelectedOverviewTab = Tag;
    Enabled := True;
  end;
end;

procedure TMainForm.actPageSetupExecute(Sender: TObject);
  {Displays the page setup dialog box.
    @param Sender [in] Not used.
  }
begin
  fDialogMgr.ExecPageSetupDlg;
end;

procedure TMainForm.actPreferencesExecute(Sender: TObject);
  {Displays Preferences dialog box.
    @param Sender [in] Not used.
  }
begin
  fDialogMgr.ExecPreferencesDlg;
end;

procedure TMainForm.actPreviousTabExecute(Sender: TObject);
  {Displays previous tab in either overview or details pane depending which is
  active.
    @param Sender [in] Not used.
  }
begin
  fMainDisplayMgr.SelectPreviousActiveTab;
end;

procedure TMainForm.actPrintExecute(Sender: TObject);
  {Displays the print dialog that user can use to print current snippet.
    @param Sender [in] Not used.
  }
begin
  Assert(TPrintMgr.CanPrint(fMainDisplayMgr.CurrentView),
    ClassName + '.actPrintExecute: View kind not vkRoutine');
  // Display print dialog
  if fDialogMgr.ExecPrintDlg then
    // User OKd: print document
    TPrintMgr.Print(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actPrintUpdate(Sender: TObject);
  {Enables / disables Print action according to whether a snippet is selected.
    @param Sender [in] Action that triggered the event.
  }
begin
  (Sender as TAction).Enabled :=
    TPrintMgr.CanPrint(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actPrivacyExecute(Sender: TObject);
  {Displays privacy statement in help file.
    @param Sender [in] Not used.
  }
begin
  DisplayHelp('PrivacyStatement');
end;

procedure TMainForm.actProxyServerExecute(Sender: TObject);
  {Displays proxy server configuration dialog box.
    @param Sender [in] Not used.
  }
begin
  fDialogMgr.ExecProxyServerDlg;
end;

procedure TMainForm.actRegisterExecute(Sender: TObject);
  {Displays registration dialog box.
    @param Sender [in] Not used.
  }
begin
  if fDialogMgr.ExecRegistrationDlg then
    fIsAppRegistered := True;
end;

procedure TMainForm.actRegisterUpdate(Sender: TObject);
  {Hides registration action (and hence menu item) if application is registered.
    @param Sender [in] Action triggering the event.
  }
begin
  with Sender as TAction do
  begin
    Visible := not fIsAppRegistered;
    Enabled := True;
  end;
end;

procedure TMainForm.actRestoreDatabaseExecute(Sender: TObject);
  {Restores user database from a backup.
    @param Sender [in] Not used.
  }
begin
  if TUserDBMgr.RestoreDatabase then
    ReloadDatabase;
end;

procedure TMainForm.actSaveDatabaseExecute(Sender: TObject);
  {Saves updated user database to disk.
    @param Sender [in] Not used.
  }
begin
  TUserDBMgr.Save;
  fStatusBarMgr.Update;
end;

procedure TMainForm.actSaveDatabaseUpdate(Sender: TObject);
  {Enables / disables Save Database action depending on if database has been
  changed.
    @param Sender [in] Action triggering the event.
  }
begin
  (Sender as TAction).Enabled := TUserDBMgr.CanSave;
end;

procedure TMainForm.actSaveSnippetExecute(Sender: TObject);
  {Saves selected routine or category to disk.
    @param Sender [in] Not used.
  }
begin
  TSaveSnippetMgr.Execute(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actSaveSnippetUpdate(Sender: TObject);
  {Enables / disables Save Snippet action according to whether a routine or
  category is selected.
    @param Sender [in] Action triggering the event.
  }
begin
  (Sender as TAction).Enabled :=
    TSaveSnippetMgr.CanHandleView(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actSaveUnitExecute(Sender: TObject);
  {Saves a unit containing currently selected snippets to disk.
    @param Sender [in] Not used.
  }
begin
  TSaveUnitMgr.Execute(Query.Selection);
end;

procedure TMainForm.actSelectAllExecute(Sender: TObject);
  {Selects all text in currently displayed web browser control.
    @param Sender [in] Not used.
  }
begin
  fMainDisplayMgr.SelectAll;
end;

procedure TMainForm.actSelectAllUpdate(Sender: TObject);
  {Enables / disables Select All action according to whether relevant controls
  are able to select text.
    @param Sender [in] Action triggering the event.
  }
begin
  (Sender as TAction).Enabled := fMainDisplayMgr.CanSelectAll;
end;

procedure TMainForm.actSelectRoutinesExecute(Sender: TObject);
  {Permits user to select snippets to be displayed. Gets selection from user via
  Select Snippets dialog box then displays all selected snippets.
    @param Sender [in] Not used.
  }
var
  Search: USearch.ISearch;  // selection search object
begin
  // Display Select Snippets dialog box to enable user to specify required
  // snippets (dialog box creates and returns search object for required
  // snippets)
  if fDialogMgr.ExecSelectionSearchDlg(Query.Selection, Search) then
    DoSearchFilter(Search);
end;

procedure TMainForm.actSubmitExecute(Sender: TObject);
  {Attempts to submit a code snippet to submission web service.
    @param Sender [in] Not used.
  }
begin
  TCodeShareMgr.Submit(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.ActSubmitOrExportUpdate(Sender: TObject);
  {Updates Submit and Export actions depending on whether there are entries in
  the user database.
    @param Sender [in] Action that triggered the event.
  }
begin
  (Sender as TAction).Enabled := TCodeShareMgr.CanShare;
end;

procedure TMainForm.actTestBugExecute(Sender: TObject);
  {Generates a test bug exception.
    @param Sender [in] Not used.
    @except Raises EBug exception.
  }
const
  // Bug error message
  cFakeError = 'Bug check: raised by pressing Shift+Ctrl+Alt+B. '
    + 'This is a program generated bug used for testing purposes only. '
    + 'PLEASE DO NOT REPORT!';
begin
  raise EBug.Create(cFakeError);
end;

procedure TMainForm.actTestCompileExecute(Sender: TObject);
  {Test compiles currently selected snippet via further action in background
  thread and displays dialog box if compilation takes a long time.
    @param Sender [in] Not used.
  }
begin
  Assert(
    fCompileMgr.CanCompile(fMainDisplayMgr.CurrentView),
    ClassName + '.actTestCompileExecute: Can''t compile current view');
  // Disable form to prevent other snippetss being selected while compiling
  Enabled := False;
  try
    // Do test compile, show a window if it takes a long time, and show results
    fCompileMgr.Compile(
      frmDetail,
      fMainDisplayMgr.CurrentView.Routine,
      fMainDisplayMgr.DisplayCompileResults
    );
  finally
    // Re-enable form before displaying results: tab not changed if disabled
    Enabled := True;
  end;
end;

procedure TMainForm.actTestCompileUpdate(Sender: TObject);
  {Enables / disables actTestCompile according to whether current selection is a
  snippet and if any compilers are installed with CodeSnip.
    @param Sender [in] Action triggering this event.
  }
begin
  (Sender as TAction).Enabled :=
    fCompileMgr.CanCompile(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.ActTreeStateChangeExecute(Sender: TObject);
  {Expands or collapses tree nodes in overview pane.
    @param Sender [in] Action triggering this event. Required operation on tree
      is defined by action's tag property.
  }
begin
  fMainDisplayMgr.UpdateOverviewTreeState(
    TTreeNodeAction((Sender as TAction).Tag)
  );
end;

procedure TMainForm.ActTreeStateChangeUpdate(Sender: TObject);
  {Enables / disables various tree node expand or collapse actions according to
  whether an expand or collapse operation is permitted.
    @param Sender [in] Action triggering this event. Queried operaation is
      defined by action's tag property.
  }
var
  Action: TAction;  // action triggering this event
begin
  Action := Sender as TAction;
  Action.Enabled := fMainDisplayMgr.CanUpdateOverviewTreeState(
    TTreeNodeAction(Action.Tag)
  )
end;

procedure TMainForm.actUpdateDbaseExecute(Sender: TObject);
  {Attempts to update code snippets database from internet and reloads Snippets
  object if update succeeds.
    @param Sender [in] Not used.
  }
resourcestring
  sConfirmSave = 'The user database has been changed. Do you wish to save it '
    + 'before updating the database?' + EOL2 + 'Clicking No will cause all '
    + 'recent changes to be lost.';
begin
  if fDialogMgr.ExecUpdateDlg then
  begin
    // Database was updated: check if user database needs saving
    if (Snippets as ISnippetsEdit).Updated
      and TMessageBox.Confirm(Self, sConfirmSave) then
      (Snippets as ISnippetsEdit).Save;
    // Reload the databases
    ReloadDatabase;
  end;
end;

procedure TMainForm.actViewCompErrsExecute(Sender: TObject);
  {Displays compiler errors for last compiled snippet.
    @param Sender [in] Not used.
  }
begin
  fCompileMgr.ShowErrors;
end;

procedure TMainForm.actViewCompErrsUpdate(Sender: TObject);
  {Updates view compiler errors action. Action enabled only if last compiled
  view is current one and their were compile errors.
    @param Sender [in] Action to be udpated.
  }
begin
  (Sender as TAction).Enabled :=
    fCompileMgr.IsLastCompiledView(fMainDisplayMgr.CurrentView)
    and fCompileMgr.HaveErrors;
end;

procedure TMainForm.ActViewCompLogExecute(Sender: TObject);
  {Displays compiler warning or error log for last compile by a specified
  compiler.
    @param Sender [in] Action triggering this event. Must be a TCompLogAction.
    @except Raised if last compiler result was not an error or a warning.
  }
begin
  // Display log in compile error dialog box
  fCompileMgr.ShowError((Sender as TCompLogAction).CompilerID);
end;

procedure TMainForm.actViewDependenciesExecute(Sender: TObject);
  {Displays dependency tree for selected snippet.
    @param Sender [in] Not used.
  }
begin
  Assert(fMainDisplayMgr.CurrentView.Kind = vkRoutine,
    ClassName + '.actViewDependenciesExecute: View kind vkRoutine expected');
  fDialogMgr.ShowDependencyTree(fMainDisplayMgr.CurrentView.Routine);
end;

procedure TMainForm.actViewDependenciesUpdate(Sender: TObject);
  {Enables / disables view dependencies action depending on whether current
  selection is a code snippet.
    @param Sender [in] Action triggering this event.
  }
begin
  (Sender as TAction).Enabled := fMainDisplayMgr.CurrentView.Kind = vkRoutine;
end;

procedure TMainForm.ActViewHistoryItemExecute(Sender: TObject);
  {Displays requested history item and selects it in the history list.
    @param Sender [in] Action triggering this event. Must be a TViewItemAction.
  }
begin
  fMainDisplayMgr.DisplayViewItem((Sender as TViewItemAction).ViewItem);
  fHistory.SelectItem((Sender as TViewItemAction).ViewItem);
end;

procedure TMainForm.ActViewItemExecute(Sender: TObject);
  {Displays a requested view item and records in history.
    @param Sender [in] Action triggering this event. Must be a TViewItemAction.
  }
begin
  fMainDisplayMgr.DisplayViewItem((Sender as TViewItemAction).ViewItem);
  fHistory.NewItem((Sender as TViewItemAction).ViewItem);
end;

procedure TMainForm.actViewTestUnitExecute(Sender: TObject);
  {Displays test unit for currently selected snippet in a dialog box.
    @param Sender [in] Not used.
  }
begin
  Assert(fMainDisplayMgr.CurrentView.Kind = vkRoutine,
    ClassName + '.actViewTestUnitExecute: View kind vkRoutine expected');
  Assert(fMainDisplayMgr.CurrentView.Routine.CanCompile,
    ClassName + '.actViewTestUnitExecute: Snippet is not compilable');
  fDialogMgr.ShowTestUnit(fMainDisplayMgr.CurrentView.Routine);
end;

procedure TMainForm.actViewTestUnitUpdate(Sender: TObject);
  {Enables /disables action depending on whether current view is a compilable
  snippet.
    @param Sender [in] Action triggering this event.
  }
begin
  (Sender as TAction).Enabled := (fMainDisplayMgr.CurrentView.Kind = vkRoutine)
    and fMainDisplayMgr.CurrentView.Routine.CanCompile;
end;

procedure TMainForm.actWelcomeExecute(Sender: TObject);
  {Displays welcome page.
    @param Sender [in] Not used.
  }
begin
  DisplayWelcomePage;
end;

procedure TMainForm.appEventsHint(Sender: TObject);
  {Handles hint events triggered when a control issues a hint. The hint is
  displayed in the status bar.
    @param Sender [in] Not used.
  }
begin
  DisplayHint(Application.Hint);
end;

procedure TMainForm.CreateParams(var Params: TCreateParams);
  {Updates style of window to ensure this main window appears on task bar.
    @params Params [in/out] In: current parameters. Out: adjusted parameters: we
      update ExStyle field with new window styles.
  }
begin
  inherited;
  Params.ExStyle := Params.ExStyle and not WS_EX_TOOLWINDOW or WS_EX_APPWINDOW;
end;

procedure TMainForm.DisplayHint(const Hint: string);
  {Displays hint in status bar using status bar manager.
    @param Hint [in] Hint to be displayed.
  }
begin
  if Assigned(fStatusBarMgr) then
    fStatusBarMgr.ShowHint(Hint);
end;

procedure TMainForm.DisplayWelcomePage;
  {Displays welcome page in currently active detail pane.
  }
var
  Welcome: TViewItem; // welcome page view item
begin
  // Get notifier to display welcome page
  Welcome := TViewItem.Create(vkWelcome);
  try
    fNotifier.ShowViewItem(Welcome);
  finally
    FreeAndNil(Welcome);
  end;
end;

procedure TMainForm.DoSearchFilter(const Search: USearch.ISearch);
  {Filters main display using search object, displays message if no snippets
  found and updates status bar as required.
    @param Search [in] Search object to filter by.
  }
resourcestring
  sNoRoutines = 'No snippets found.'; // dialog box messages
begin
  if Query.ApplySearch(Search) then
  begin
    fMainDisplayMgr.QueryUpdated;
    fStatusBarMgr.Update;
  end
  else
    TMessageBox.Information(Self, sNoRoutines);
end;

procedure TMainForm.FormCreate(Sender: TObject);
  {Removes application object's hidden window from task bar, disables main form
  and sets up application event handler.
    @param Sender [in] Not used.
  }
begin
  try
    inherited;
    // Remove hidden application window from task bar: this form is now use on
    // task bar. This required so task bar button conforms to Vista
    // requirements.
    ShowWindow(Application.Handle, SW_HIDE);
    SetWindowLong(
      Application.Handle,
      GWL_EXSTYLE,
      GetWindowLong(Application.Handle, GWL_EXSTYLE)
        and not WS_EX_APPWINDOW or WS_EX_TOOLWINDOW
    );
    ShowWindow(Application.Handle, SW_SHOW);
    // Disable form
    Enabled := False;
    // Set up application events
    appEvents.OnException := TExceptionHandler.Handler;
  except
    // Make sure form is enabled and splash form hidden on exception
    Enabled := True;
    SplashForm.RequestClose;
    raise;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
  {Tidy up application. Writes persistent data and frees objects.
    @param Sender [in] Not used.
  }
begin
  inherited;
  // Save any changes to user database
  with Snippets as ISnippetsEdit do
  begin
    if Updated then
      Save;
  end;
  // Unhook snippets event handler
  Snippets.RemoveChangeEventHandler(SnippetsChangeHandler);
  // Save window state
  fWindowSettings.SplitterPos := pnlLeft.Width;
  fWindowSettings.OverviewTab := fMainDisplayMgr.SelectedOverviewTab;
  fWindowSettings.DetailTab := fMainDisplayMgr.SelectedDetailTab;
  fWindowSettings.Save;
  // Free owned objects
  FreeAndNil(fHistory);
  FreeAndNil(fMainDisplayMgr);
  FreeAndNil(fStatusBarMgr);
end;

procedure TMainForm.FormResize(Sender: TObject);
  {Ensure splitter bar is in a sensible position when form size changes.
    @param Sender [in] Not used.
  }
begin
  inherited;
  if pnlLeft.Width > ClientWidth - cMinRightPanelWidth - splitVert.Width then
    pnlLeft.Width := ClientWidth - cMinRightPanelWidth - splitVert.Width;
end;

procedure TMainForm.InitForm;
  {Initialises form and creates and configures owned objects. Once
  initialisation is complete splash window is canclled and form enabled.
  }
var
  WBExternal: IDispatch;        // external object of browser control
begin
  try
    inherited;
    // Set window caption
    Application.Title := TAppInfo.FullProgramName;
    Caption := Application.Title;

    // Restore window settings
    fWindowSettings := TWindowSettings.CreateStandAlone(Self);     // auto-freed
    fWindowSettings.SplitterPos := cDefLeftPanelWidth;       // default position
    fWindowSettings.Restore;                                // sizes main window
    pnlLeft.Width := fWindowSettings.SplitterPos;

    // Initialise actions
    // Browse actions have to have URLs set dynamically
    actHomePage.URL := TWebInfo.ProgramHomeURL;
    actWebSite.URL := TWebInfo.DelphiDabblerHomeURL;
    // Tree control actions need shortcuts adding dynamically, and state stored
    // in Tag property
    actExpandNode.ShortCut := ShortCut(VK_ADD, [ssCtrl]);
    actCollapseNode.ShortCut := ShortCut(VK_SUBTRACT, [ssCtrl]);
    actExpandTree.ShortCut := ShortCut(VK_ADD, [ssCtrl, ssShift]);
    actCollapseTree.ShortCut := ShortCut(VK_SUBTRACT, [ssCtrl, ssShift]);
    actExpandTree.Tag := Ord(taExpandAll);
    actExpandNode.Tag := Ord(taExpandNode);
    actCollapseTree.Tag := Ord(taCollapseAll);
    actCollapseNode.Tag := Ord(taCollapseNode);
    // Overview tab actions have tab id in tags
    actViewCategorised.Tag := cCategorisedTab;
    actViewAlphabetical.Tag := cAlphabeticTab;
    actViewSnippetKinds.Tag := cKindTab;
    actViewInfo.Tag := cInfoTab;
    actViewCompCheck.Tag := cCompCheckTab;

    // Create notifier object and assign actions triggered by its methods
    // note that actions created on fly are automatically freed
    fNotifier := TNotifier.Create;
    with fNotifier as ISetActions do
    begin
      SetUpdateDbaseAction(actUpdateDbase);
      SetDisplayRoutineAction(TActionFactory.CreateRoutineAction(Self));
      SetDisplayCategoryAction(TActionFactory.CreateCategoryAction(Self));
      SetCompileRoutineAction(actTestCompile);
      SetViewCompilerLogAction(
        TActionFactory.CreateCompLogAction(Self, ActViewCompLogExecute)
      );
      SetShowHintAction(
        TActionFactory.CreateHintAction(Self, ActBrowserHintExecute)
      );
      SetConfigCompilersAction(actCompilers);
      SetShowViewItemAction(
        TActionFactory.CreateViewItemAction(Self, ActViewItemExecute)
      );
      SetOverviewStyleChangeActions([
        actViewCategorised, actViewAlphabetical, actViewSnippetKinds
      ]);
      SetDetailPaneChangeActions([actViewInfo, actViewCompCheck]);
      SetShowTestUnitAction(actViewTestUnit);
      SetEditRoutineAction(
        TActionFactory.CreateEditRoutineAction(Self, ActEditRoutineExecute)
      );
      SetDonateAction(actDonate);
    end;

    // Customise web browser controls in Details pane
    WBExternal := TWBExternal.Create;
    with frmDetail as IWBCustomiser do
    begin
      SetExternalObj(WBExternal);
      SetDragDropHandler(TWBNulDropTarget.Create);
    end;

    // Set notifier for objects that trigger notifications
    (WBExternal as ISetNotifier).SetNotifier(fNotifier);
    (frmOverview as ISetNotifier).SetNotifier(fNotifier);
    (frmDetail as ISetNotifier).SetNotifier(fNotifier);

    // Create dialog box manager
    fDialogMgr := TDialogMgr.Create(Self);  // automatically freed

    // Create display manager
    fMainDisplayMgr := TMainDisplayMgr.Create(frmOverview, frmDetail);
    // select active tabs
    fMainDisplayMgr.SelectedOverviewTab := fWindowSettings.OverviewTab;
    fMainDisplayMgr.SelectedDetailTab := fWindowSettings.DetailTab;

    // Create status bar manager
    fStatusBarMgr := TStatusBarMgr.Create(sbStatusBar);

    // Create navigation history and menus
    // history manager
    fHistory := THistory.Create;
    // history menus: note menu and action objects are automatically freed
    tbGoBack.DropdownMenu := TBackHistoryMenu.Create(
      Self,
      fHistory,
      TActionFactory.CreateViewItemAction(
        Self, ActViewHistoryItemExecute
      ) as TViewItemAction
    );
    tbGoForward.DropdownMenu := TForwardHistoryMenu.Create(
      Self,
      fHistory,
      TActionFactory.CreateViewItemAction(
        Self, ActViewHistoryItemExecute
      ) as TViewItemAction
    );

    // Set up detail pane's popup menus
    with frmDetail as IWBPopupMenuConfig do
    begin
      // set images to use
      SetImages(ilMain);
      // default menu
      AddAction(actViewDependencies, pmkDefault);
      AddSpacer(pmkDefault);
      AddAction(actCopyInfo, pmkDefault);
      AddAction(actCopySnippet, pmkDefault);
      AddAction(actCopySource, pmkDefault);
      AddSpacer(pmkDefault);
      AddAction(actSaveSnippet, pmkDefault);
      AddAction(actPrint, pmkDefault);
      AddSpacer(pmkDefault);
      AddAction(actSelectAll, pmkDefault);
      // selected text menu
      AddAction(actCopy, pmkTextSelect);
      AddAction(actSelectAll, pmkTextSelect);
      // anchor menu
      AddAction(TActionFactory.CreateLinkAction(Self), pmkAnchor);
      // image menu
      AddAction(TActionFactory.CreateLinkAction(Self), pmkImage);
    end;

    // Set up overview pane's toolbar and popup menu
    with frmOverview as ICommandBarConfig do
    begin
      SetImages(ilMain);
      // add toolbar actions (in reverse order we want them!)
      AddAction(actCollapseNode, cOverviewToolBar);
      AddAction(actExpandNode, cOverviewToolBar);
      // add popup menu actions
      AddAction(actViewDependencies, cOverviewPopupMenu);
      AddSpacer(cOverviewPopupMenu);
      AddAction(actCopyInfo, cOverviewPopupMenu);
      AddAction(actCopySnippet, cOverviewPopupMenu);
      AddAction(actCopySource, cOverviewPopupMenu);
      AddSpacer(cOverviewPopupMenu);
      AddAction(actSaveSnippet, cOverviewPopupMenu);
      AddAction(actPrint, cOverviewPopupMenu);
      AddSpacer(cOverviewPopupMenu);
      AddAction(actEditSnippet, cOverviewPopupMenu);
      AddSpacer(cOverviewPopupMenu);
      AddAction(actCollapseNode, cOverviewPopupMenu);
      AddAction(actExpandNode, cOverviewPopupMenu);
    end;

    // Create object to handle compilation and assoicated UI and dialogs
    fCompileMgr := TMainCompileMgr.Create(Self);  // auto-freed

    // Record if app is registered
    fIsAppRegistered := TAppInfo.IsRegistered;

    // Set event handler for snippets database
    Snippets.AddChangeEventHandler(SnippetsChangeHandler);

    // Load snippets database(s) and re-initialise display
    LoadSnippets;
  finally
    // Ready to start using app: request splash form closes and enable form
    SplashForm.RequestClose;
    Enabled := True;
  end;
end;

procedure TMainForm.LoadSnippets;
  {Loads Snippets object from database and re-intitialises display.
  }
resourcestring
  sLoadingDatabase  = 'Loading database...';  // status bar message
begin
  // Inform that database is being loaded via status bar
  fStatusBarMgr.ShowSimpleMessage(sLoadingDatabase);
  fHistory.Clear;
  fMainDisplayMgr.Clear;
  // Load the database in a separate thread
  try
    TThreadWrapper.Execute(TDatabaseLoader.Create, True);
  except
    on E: ECodeSnip do
      Application.HandleException(E);
  end;
  // Re-initialise display
  fMainDisplayMgr.Initialise;
  DisplayWelcomePage;
  // Display updated database stats and search results in status bar
  fStatusBarMgr.Update;
end;

procedure TMainForm.ReloadDatabase;
  {Reloads the whole database in a thread.
  }
var
  WaitDlg: TWaitDlg;  // dialog to be displayed while database loading
resourcestring
  sWaitMsg = 'Loading database...'; // caption displayed in wait form
begin
  // Load database, displaying a dialog box while it's loading
  WaitDlg := TWaitDlg.Create(Self);
  try
    WaitDlg.Caption := sWaitMsg;
    // always display wait dialog, for no longer than necessary
    TWaitForActionUI.Run(actLoadDatabase, WaitDlg, 0, 0);
  finally
    FreeAndNil(WaitDlg);
  end;
end;

procedure TMainForm.SnippetsChangeHandler(Sender: TObject;
  const EvtInfo: IInterface);
  {Handles Snippets change event handler that is trigerred when a user defined
  entry in the database changes.
    @param Sender [in] Not used.
    @para EvtInfo [in] Object providing information about the event.
  }

  // ---------------------------------------------------------------------------
  procedure ReInitialise;
    {Re-initialises display, resetting any queries if necessary.
    }
  begin
    if not Query.Refresh then
      Query.Reset;
    fMainDisplayMgr.Initialise;
  end;
  // ---------------------------------------------------------------------------

var
  Info: ISnippetChangeEventInfo;  // information about the event
begin
  Info := EvtInfo as ISnippetChangeEventInfo;
  case Info.Kind of
    evChangeBegin:        // database about to change: disable form
      Enabled := False;
    evChangeEnd:          // database change has completed: re-enable form
      Enabled := True;
    evRoutineAdded:       // snippet added: display new routine
    begin
      ReInitialise;
      fNotifier.DisplayRoutine(Info.Routine.Name, Info.Routine.UserDefined);
    end;
    evRoutineChanged:     // snippet edited: display changes
    begin
      fHistory.Clear;
      ReInitialise;
      fNotifier.DisplayRoutine(Info.Routine.Name, Info.Routine.UserDefined);
    end;
    evRoutineDeleted:     // snippet deleted: display welcome page
    begin
      fHistory.Clear;
      ReInitialise;
      DisplayWelcomePage;
    end;
  end;
  // Display updated database stats and search results in status bar
  fStatusBarMgr.Update;
end;

procedure TMainForm.splitVertCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
  {Determines if splitter can resize the controls it governs.
    @param Sender [in] Not used.
    @param NewSize [in] New size of governed control.
    @param Accept [in/out] Set false if splitter cannot resize the control.
  }
begin
  if (NewSize < cMinLeftPanelWidth)
    or (NewSize > ClientWidth - cMinRightPanelWidth) then
    Accept := False;
end;

procedure TMainForm.WMSyscommand(var Msg: TWMSysCommand);
  {Handles system command messages. Overrides default processing of minimizing
  and restoration of main window. This is required now we have inhibited
  application object's default processing of these messages.
    @param Msg [in/out] Details of system command. Result field set to 0 if we
      handle message to prevent default processing.
  }
begin
  // Note: according to Win API low order four bits of Msg.CmdType are reserved
  // for use by windows. We therefore mask out those bits before processing.
  case (Msg.CmdType and $FFF0) of
    SC_MINIMIZE:
    begin
      ShowWindow(Handle, SW_MINIMIZE);
      Msg.Result := 0;
    end;
    SC_RESTORE:
    begin
      ShowWindow(Handle, SW_RESTORE);
      Msg.Result := 0;
    end;
    else
      inherited;
  end;
end;

end.

