{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2016, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Application's main form. Handles the program's main window display and user
 * interaction.
}


unit FmMain;


interface


uses
  // Delphi
  SysUtils,
  Menus,
  ExtActns,
  StdActns,
  Classes,
  ActnList,
  ImgList,
  Controls,
  Forms,
  ExtCtrls,
  ComCtrls,
  ToolWin,
  Messages,
  AppEvnts,
  // Project
  FmHelpAware,
  FrDetail,
  FrOverview,
  FrTitled,
  IntfNotifier,
  UCompileMgr,
  UDialogMgr,
  UHistory,
  UMainDisplayMgr,
  USearch,
  UStatusBarMgr,
  UUpdateCheckers,
  UWindowSettings;


type
  ///  <summary>Form that provides the application's main window.</summary>
  TMainForm = class(THelpAwareForm)
    actAbout: TAction;
    actToggleFavourite: TAction;
    actAddSnippet: TAction;
    actBackupDatabase: TAction;
    actBugReport: TAction;
    actCloseAllDetailsTabs: TAction;
    actCloseDetailsTab: TAction;
    actCloseUnselectedDetailsTabs: TAction;
    actCollapseNode: TAction;
    actCollapseTree: TAction;
    actCompilers: TAction;
    actCopy: TAction;
    actCopyInfo: TAction;
    actCopySnippet: TAction;
    actCopySource: TAction;
    actDeleteSnippet: TAction;
    actDuplicateSnippet: TAction;
    actEditSnippet: TAction;
    actExit: TFileExit;
    actExpandNode: TAction;
    actExpandTree: TAction;
    actExportCode: TAction;
    actFavourites: TAction;
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
    actLoadSelection: TAction;
    actMoveDatabase: TAction;
    actNextTab: TAction;
    actNewDetailsTab: TAction;
    actPreferences: TAction;
    actPreviousTab: TAction;
    actPrint: TAction;
    actPrivacy: TAction;
    actProgramUpdates: TAction;
    actProxyServer: TAction;
    actRestoreDatabase: TAction;
    actSaveDatabase: TAction;
    actSaveSelection: TAction;
    actSaveSnippet: TAction;
    actSaveUnit: TAction;
    actSelectAll: TAction;
    actSelectSnippets: TAction;
    actSubmit: TAction;
    actSWAGImport: TAction;
    actTestBug: TAction;
    actTestCompile: TAction;
    actUpdateDbase: TAction;
    actViewGroupAlphabetically: TAction;
    actViewGroupByTags: TAction;
    actViewCompErrs: TAction;
    actViewDependencies: TAction;
    actViewGroupBySnippetKinds: TAction;
    actViewTestUnit: TAction;
    actWelcome: TAction;
    alMain: TActionList;
    appEvents: TApplicationEvents;
    frmDetail: TDetailFrame;
    frmOverview: TOverviewFrame;
    ilMain: TImageList;
    miAbout: TMenuItem;
    miToggleFavourite: TMenuItem;
    miAddSnippet: TMenuItem;
    miBackupDatabase: TMenuItem;
    miCheckUpdates: TMenuItem;
    miCloseAllDetailsTabs: TMenuItem;
    miCloseDetailsTab: TMenuItem;
    miCollapseNode: TMenuItem;
    miCollapseTree: TMenuItem;
    miCompile: TMenuItem;
    miCompilers: TMenuItem;
    miCopy: TMenuItem;
    miCopyInfo: TMenuItem;
    miCopySnippet: TMenuItem;
    miDatabase: TMenuItem;
    miDeleteSnippet: TMenuItem;
    miDuplicateSnippet: TMenuItem;
    miEdit: TMenuItem;
    miEditSnippet: TMenuItem;
    miExit: TMenuItem;
    miExpandNode: TMenuItem;
    miExpandTree: TMenuItem;
    miExportCode: TMenuItem;
    miFavourites: TMenuItem;
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
    miLoadSelection: TMenuItem;
    miMoveDatabase: TMenuItem;
    miNewDetailsTab: TMenuItem;
    miPreferences: TMenuItem;
    miPrint: TMenuItem;
    miPrivacy: TMenuItem;
    miProxyServer: TMenuItem;
    miReportBug: TMenuItem;
    miRestoreDatabase: TMenuItem;
    miSaveDatabase: TMenuItem;
    miSaveSelection: TMenuItem;
    miSaveSnippet: TMenuItem;
    miSaveUnit: TMenuItem;
    miSearch: TMenuItem;
    miSelectSnippets: TMenuItem;
    miSelectAll: TMenuItem;
    miSnippets: TMenuItem;
    miSourceCode: TMenuItem;
    miSpacer1: TMenuItem;
    miSpacer2: TMenuItem;
    miSpacer3: TMenuItem;
    miSpacer4: TMenuItem;
    miSpacer5: TMenuItem;
    miSpacer6: TMenuItem;
    miSpacer7: TMenuItem;
    miSpacer8: TMenuItem;
    miSpacer10: TMenuItem;
    miSpacer11: TMenuItem;
    miSpacer12: TMenuItem;
    miSpacer13: TMenuItem;
    miSpacer14: TMenuItem;
    miSpacer15: TMenuItem;
    miSpacer16: TMenuItem;
    miSpacer17: TMenuItem;
    miSpacer18: TMenuItem;
    miSpacer19: TMenuItem;
    miSpacer20: TMenuItem;
    miSubmit: TMenuItem;
    miSWAGImport: TMenuItem;
    miTestCompile: TMenuItem;
    miTools: TMenuItem;
    miUpdateDbase: TMenuItem;
    miView: TMenuItem;
    miViewGroupByTags: TMenuItem;
    miViewCompErrs: TMenuItem;
    miViewDependencies: TMenuItem;
    miViewGroupBySnippetKinds: TMenuItem;
    miViewTestUnit: TMenuItem;
    miViewGroupAlphabetically: TMenuItem;
    miWelcome: TMenuItem;
    mnuMain: TMainMenu;
    pnlBody: TPanel;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    sbStatusBar: TStatusBar;
    splitVert: TSplitter;
    tbarMain: TToolBar;
    tbAddSnippet: TToolButton;
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
    tbSelectSnippets: TToolButton;
    tbSpacer1: TToolButton;
    tbSpacer3: TToolButton;
    tbSpacer4: TToolButton;
    tbSpacer5: TToolButton;
    tbSpacer6: TToolButton;
    tbSpacer7: TToolButton;
    tbSpacer8: TToolButton;
    tbTestCompile: TToolButton;
    tbUpdateDbase: TToolButton;
    actConfigDiffProg: TAction;
    miConfigDiffProg: TMenuItem;
    actViewGroupByLanguage: TAction;
    miViewGroupByLanguage: TMenuItem;
    actFindTags: TAction;
    miFindTags: TMenuItem;
    tbFindTags: TToolButton;
    ///  <summary>Displays About Box.</summary>
    procedure actAboutExecute(Sender: TObject);
    ///  <summary>Adds current snippet to favourites.</summary>
    procedure actToggleFavouriteExecute(Sender: TObject);
    ///  <summary>Determines whether AddFavourites action can be enabled.
    ///  </summary>
    procedure actToggleFavouriteUpdate(Sender: TObject);
    ///  <summary>Opens Snippets Editor to add a new snippet to the database.
    ///  </summary>
    procedure actAddSnippetExecute(Sender: TObject);
    ///  <summary>Makes a backup of the snippets database.</summary>
    procedure actBackupDatabaseExecute(Sender: TObject);
    ///  <summary>Displays Bug Report dialogue box.</summary>
    procedure actBugReportExecute(Sender: TObject);
    ///  <summary>Closes all open tabs in details pane.</summary>
    procedure actCloseAllDetailsTabsExecute(Sender: TObject);
    ///  <summary>Closes current tab in details pane.</summary>
    procedure actCloseDetailsTabExecute(Sender: TObject);
    ///  <summary>Determines whether CloseDetailsTab action can be enabled.
    ///  </summary>
    procedure actCloseDetailsTabsUpdate(Sender: TObject);
    ///  <summary>Closes all open tabs in details pane except for the current
    ///  tab.</summary>
    procedure actCloseUnselectedDetailsTabsExecute(Sender: TObject);
    ///  <summary>Displays Configure Compilers dialogue box.</summary>
    procedure actCompilersExecute(Sender: TObject);
    ///  <summary>Displays the Configure Diff Viewer Program dialogue box.
    ///  </summary>
    procedure actConfigDiffProgExecute(Sender: TObject);
    ///  <summary>Copies selected text from details pane to clipboard.</summary>
    procedure actCopyExecute(Sender: TObject);
    ///  <summary>Copies information about current snippet to clipboard.
    ///  </summary>
    procedure actCopyInfoExecute(Sender: TObject);
    ///  <summary>Determines whether CopyInfo action can be enabled.</summary>
    procedure actCopyInfoUpdate(Sender: TObject);
    ///  <summary>Copies annotated source code of selected snippet or selected
    ///  tag to clipboard.</summary>
    procedure actCopySnippetExecute(Sender: TObject);
    ///  <summary>Determines whether CopySnippet action can be enabled.
    ///  </summary>
    procedure actCopySnippetUpdate(Sender: TObject);
    ///  <summary>Copies current snippet's source code to clipboard.</summary>
    procedure actCopySourceExecute(Sender: TObject);
    ///  <summary>Determines whether CopySource action can be enabled.</summary>
    procedure actCopySourceUpdate(Sender: TObject);
    ///  <summary>Determines whether Copy action can be enabled.</summary>
    procedure actCopyUpdate(Sender: TObject);
    ///  <summary>Attempts to delete the current snippet from the database.
    ///  </summary>
    procedure actDeleteSnippetExecute(Sender: TObject);
    ///  <summary>Displays a dialogue box that can be used to duplicate the
    ///  selected snippet.</summary>
    procedure actDuplicateSnippetExecute(Sender: TObject);
    ///  <summary>Determines whether DuplicateSnippet action can be enabled.
    ///  </summary>
    procedure actDuplicateSnippetUpdate(Sender: TObject);
    ///  <summary>Determines whether the DeleteSnippet or EditSnippet actions
    ///  can be enabled.</summary>
    procedure ActEditDeleteSnippetUpdate(Sender: TObject);
    ///  <summary>Displays a dialogue box that can be used to export one or more
    ///  snippets.</summary>
    procedure actExportCodeExecute(Sender: TObject);
    ///  <summary>Opens currently selected snippet in Snippets Editor for
    ///  editing.</summary>
    procedure actEditSnippetExecute(Sender: TObject);
    ///  <summary>Displays the Favourites non-modal dialogue box.</summary>
    procedure actFavouritesExecute(Sender: TObject);
    ///  <summary>Clears any current query and displays all snippets in the
    ///  database.</summary>
    procedure actFindClearExecute(Sender: TObject);
    ///  <summary>Determines whether FindClear action can be enabled.</summary>
    procedure actFindClearUpdate(Sender: TObject);
    ///  <summary>Performs compiler search. Displays Find Compiler dialogue box
    ///  to get search critria then executes the search.</summary>
    procedure actFindCompilerExecute(Sender: TObject);
    ///  <summary>Performs tags search. Displays Find Tags dialogue box
    ///  to get search critria then executes the search.</summary>
    procedure actFindTagsExecute(Sender: TObject);
    ///  <summary>Performs plain text search. Displays Find Text dialogue box
    ///  to get search critria then executes the search.</summary>
    procedure actFindTextExecute(Sender: TObject);
    ///  <summary>Performs cross-reference search. Displays Find Cross Refs
    ///  dialogue box to get search critria then executes the search.</summary>
    procedure actFindXRefsExecute(Sender: TObject);
    ///  <summary>Determines whether FindXRefs action can be enabled.</summary>
    procedure actFindXRefsUpdate(Sender: TObject);
    ///  <summary>Displays previous view item in history list.</summary>
    procedure actGoBackExecute(Sender: TObject);
    ///  <summary>Determines whether GoBack action can be enabled.</summary>
    procedure actGoBackUpdate(Sender: TObject);
    ///  <summary>Displays next view item in history list.</summary>
    procedure actGoForwardExecute(Sender: TObject);
    ///  <summary>Determines whether GoForward action can be enabled.</summary>
    procedure actGoForwardUpdate(Sender: TObject);
    ///  <summary>Displays About Compiler Checks help topic.</summary>
    procedure actHelpCompChecksExecute(Sender: TObject);
    ///  <summary>Displays help table of contents with default help topic.
    ///  </summary>
    procedure actHelpContentsExecute(Sender: TObject);
    ///  <summary>Displays Quick Start help topic.</summary>
    procedure actHelpQuickStartExecute(Sender: TObject);
    ///  <summary>Displays Import Wizard dialogue box from where code snippets
    ///  can be imported.</summary>
    procedure actImportCodeExecute(Sender: TObject);
    ///  <summary>Displays License help topic.</summary>
    procedure actLicenseExecute(Sender: TObject);
    ///  <summary>Displays Load Selection dialogue box from where user can
    ///  load a snippet selection from file and display the selection.</summary>
    procedure actLoadSelectionExecute(Sender: TObject);
    ///  <summary>Displays a dialogue box that can be used to move the snippets
    ///  database to a user defined directory.</summary>
    ///  <remarks>This action must be hidden when running in portable mode.
    ///  </remarks>
    procedure actMoveDatabaseExecute(Sender: TObject);
    ///  <summary>Creates a new empty tab in details pane.</summary>
    procedure actNewDetailsTabExecute(Sender: TObject);
    ///  <summary>Displays next tab in either overview or details pane depending
    ///  which pane is active.</summary>
    procedure actNextTabExecute(Sender: TObject);
    ///  <summary>Determines whether actions that require a non-empty database
    ///  can be enabled.</summary>
    procedure ActNonEmptyDBUpdate(Sender: TObject);
    ///  <summary>Selects a tab in the overview pane.</summary>
    ///  <remarks>The index of the tab to be selected is stored in the sending
    ///  action's Tag property.</remarks>
    procedure ActOverviewGroupingExecute(Sender: TObject);
    ///  <summary>Updates checked state of OverviewTab action referenced by
    ///  Sender.</summary>
    ///  <remarks>Action is checked iff its index, stored in the Tag property,
    ///  is the currently selected overview tab index.</remarks>
    procedure ActOverviewGroupingUpdate(Sender: TObject);
    ///  <summary>Displays the Preferences dialogue box.</summary>
    procedure actPreferencesExecute(Sender: TObject);
    ///  <summary>Displays previous tab in either the overview or details pane
    ///  depending on which pane is active.</summary>
    procedure actPreviousTabExecute(Sender: TObject);
    ///  <summary>Displays the Print dialogue box that can be used to print the
    ///  selected snippet or tag.</summary>
    procedure actPrintExecute(Sender: TObject);
    ///  <summary>Determines whether the Print action can be enabled.</summary>
    procedure actPrintUpdate(Sender: TObject);
    ///  <summary>Displays the Privacy Statement help topic.</summary>
    procedure actPrivacyExecute(Sender: TObject);
    ///  <summary>Displays the Check For Program Updates dialogue box that
    ///  displays the availability of any program updates.</summary>
    procedure actProgramUpdatesExecute(Sender: TObject);
    ///  <summary>Displays the Proxy Server Configuration dialogue box that can
    ///  be used to specify a proxy server to use for internet access.</summary>
    procedure actProxyServerExecute(Sender: TObject);
    ///  <summary>Displays a dialogue box from which a backup file can be
    ///  selected and used to restore the snippets database.</summary>
    procedure actRestoreDatabaseExecute(Sender: TObject);
    ///  <summary>Save any changes in the snippets database to disk.</summary>
    procedure actSaveDatabaseExecute(Sender: TObject);
    ///  <summary>Determines whether the SaveDatabase action can be enabled.
    ///  </summary>
    procedure actSaveDatabaseUpdate(Sender: TObject);
    ///  <summary>Save the current snippet selection to disk in a user-specified
    ///  file.</summary>
    procedure actSaveSelectionExecute(Sender: TObject);
    ///  <summary>Determines whether the SaveSelection action can be enabled.
    ///  </summary>
    procedure actSaveSelectionUpdate(Sender: TObject);
    ///  <summary>Saves annotated source code of the currently selected snippet
    ///  or tag to disk in a user-specified file.</summary>
    procedure actSaveSnippetExecute(Sender: TObject);
    ///  <summary>Determines whether the SaveSnippet action can be enabled.
    ///  </summary>
    procedure actSaveSnippetUpdate(Sender: TObject);
    ///  <summary>Saves the current snippet selection to disk in a
    ///  user-specified file.</summary>
    procedure actSaveUnitExecute(Sender: TObject);
    ///  <summary>Selects all text selected in the current detail pane.
    ///  </summary>
    procedure actSelectAllExecute(Sender: TObject);
    ///  <summary>Determines whether the SelectAll action can be enabled.
    ///  </summary>
    procedure actSelectAllUpdate(Sender: TObject);
    ///  <summary>Displays the Select Snippets dialogue box where the snippets
    ///  to be displayed can be chosen.</summary>
    procedure actSelectSnippetsExecute(Sender: TObject);
    ///  <summary>Displays the Code Submission Wizard that enables snippets to
    ///  be submitted for inclusion in the main database.</summary>
    procedure actSubmitExecute(Sender: TObject);
    ///  <summary>Determines whether the Submit or ExportCode actions can be
    ///  enabled.</summary>
    procedure ActSubmitOrExportUpdate(Sender: TObject);
    ///  <summary>Displays the SWAG import dialogue box that enables the user to
    ///  import snippets from the SWAG database into the snippets database.
    ///  </summary>
    procedure actSWAGImportExecute(Sender: TObject);
    ///  <summary>Generates an exception that simulates an uncaught exception.
    ///  </summary>
    ///  <exception>EBug exception always raised.</exception>
    ///  <remarks>This simulation is included to permit exception handling code
    ///  to be tested.</remarks>
    procedure actTestBugExecute(Sender: TObject);
    ///  <summary>Test compiles the selected snippet in all available compilers.
    ///  </summary>
    procedure actTestCompileExecute(Sender: TObject);
    ///  <summary>Determines whether TestCompile action can be enabled.
    ///  </summary>
    procedure actTestCompileUpdate(Sender: TObject);
    ///  <summary>Expands or collapses tree is overview pane.</summary>
    ///  <remarks>The required expand or collapse operation is specified by the
    ///  value of the sending action's Tag property.</remarks>
    procedure ActTreeStateChangeExecute(Sender: TObject);
    ///  <summary>Determines whether tree expand or collapse action referenced
    ///  by Sender can be enabled.</summary>
    ///  <remarks>The Tag property of the Action specifies the actual expand /
    ///  collapse operation.</remarks>
    procedure ActTreeStateChangeUpdate(Sender: TObject);
    ///  <summary>Displays the Update From Web dialogue box that enables the
    ///  latest version of the online database to be downloaded. If the database
    ///  was changed it is reloaded.</summary>
    procedure actUpdateDbaseExecute(Sender: TObject);
    ///  <summary>Displays a dialogue box giving details of any compiler errors
    ///  and warnings for the last compiled snippet.</summary>
    procedure actViewCompErrsExecute(Sender: TObject);
    ///  <summary>Determines whether the ViewCompErrs action can be enabled.
    ///  </summary>
    procedure actViewCompErrsUpdate(Sender: TObject);
    ///  <summary>Displays a dialogue box that shows any dependencies of the
    ///  current snippet.</summary>
    procedure actViewDependenciesExecute(Sender: TObject);
    ///  <summary>Determines whether the ViewDependencies action can be enabled.
    ///  </summary>
    procedure actViewDependenciesUpdate(Sender: TObject);
    ///  <summary>Displays the source code unit that would be used to test
    ///  compile the selected snippet.</summary>
    procedure actViewTestUnitExecute(Sender: TObject);
    ///  <summary>Determines whether the ViewTestUnit action can be enabled.
    ///  </summary>
    procedure actViewTestUnitUpdate(Sender: TObject);
    ///  <summary>Displays the Welcome page in the details pane.</summary>
    procedure actWelcomeExecute(Sender: TObject);
    ///  <summary>Handles events triggered when a control issues a hint. The
    ///  hint is displayed in the form's status bar.</summary>
    procedure appEventsHint(Sender: TObject);
    ///  <summary>Main form initialisation. Sets exception handle and handles
    ///  clearing of splash screen and disabling form's UI until it is
    ///  initialised.</summary>
    procedure FormCreate(Sender: TObject);
    ///  <summary>Main form's finalisation code. Saves unsaved snippets and
    ///  stores main window state.</summary>
    procedure FormDestroy(Sender: TObject);
    ///  <summary>Handles form resizing event by ensuring vertical splitter is
    ///  constrained to a sensible position.</summary>
    procedure FormResize(Sender: TObject);
    ///  <summary>Called when splitter is moved. Determines if the new splitter
    ///  position is permitted and blocks the move if not.</summary>
    procedure splitVertCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
  strict private
    var
      ///  <summary>Object that notifies user-initiated events by triggering
      ///  actions.</summary>
      fNotifier: INotifier;
      ///  <summary>Object that maintains a history of viewed item.</summary>
      fHistory: THistory;
      ///  <summary>Component that saves and restores main window settings.
      ///  </summary>
      fWindowSettings: TMainWindowSettings;
      ///  <summary>Object that manages the main display.</summary>
      fMainDisplayMgr: TMainDisplayMgr;
      ///  <summary>Object that manages the status bar.</summary>
      fStatusBarMgr: TStatusBarMgr;
      ///  <summary>Object that manages display of dialogue boxes.</summary>
      fDialogMgr: TDialogMgr;
      ///  <summary>Object that manages test compilations.</summary>
      fCompileMgr: TMainCompileMgr;
      ///  <summary>Object that checks for program and database updates in a
      ///  background thread.</summary>
      fUpdateChecker: TUpdateCheckerMgr;

    ///  <summary>Displays view item given by TViewItemAction instance
    ///  referenced by Sender and adds to history list.</summary>
    procedure ActViewItemExecute(Sender: TObject);
    ///  <summary>Displays view item from history list given by TViewItemAction
    ///  instance referenced by Sender.</summary>
    procedure ActViewHistoryItemExecute(Sender: TObject);
    ///  <summary>Opens a named snippet in Snippets Editor for editing. The
    ///  snippet name is provided by the TEditSnippetAction instance referenced
    ///  by Sender.</summary>
    procedure ActEditSnippetByNameExecute(Sender: TObject);
    ///  <summary>Selects a tab in the details pane where the tab is provided by
    ///  the TDetailTabAction instance referenced by Sender.</summary>
    procedure ActSelectDetailTabExecute(Sender: TObject);
    ///  <summary>Displays Preferences dialogue box containing the single page
    ///  specified by the TShowPrefsPageAction instance referenced by Sender.
    ///  </summary>
    procedure ActShowPrefsPageExecute(Sender: TObject);
    ///  <summary>Removes a tag from a snippet where both the tag and snippet ID
    ///  are specified by the TRemoveTagAction instance referenced by Sender.
    ///  </summary>
    procedure ActRemoveTagExecute(Sender: TObject);
    ///  <summary>Handles events that inform of changes to the database. The
    ///  EvtInfo object provides information about the change.</summary>
    procedure DBChangeHandler(Sender: TObject; const EvtInfo: IInterface);
    ///  <summary>Handles the given un-trapped application level exception E.
    ///  </summary>
    ///  <remarks>Display an error message for expected exceptions or a bug
    ///  report dialogue box for unexpected exceptions.</remarks>
    procedure HandleExceptions(Sender: TObject; E: Exception);
    ///  <summary>Provides a wrapper round code that (re)loads the snippets
    ///  database. Wrapper code prepares and re-initialises the UI.</summary>
    ///  <param name="Loader">TProc [in] Closure that actually loads the
    ///  database.</param>
    procedure LoadSnippets(const Loader: TProc);
    ///  <summary>Reloads the whole database in a separate thread.</summary>
    procedure ReloadDatabase;
    ///  <summary>Filters snippets shown in main display according to search
    ///  criteria.</summary>
    ///  <param name="Search">ISearch [in] Search object to filter by.</param>
    ///  <param name="Refine">Boolean [in] Flag indicating whether any exiting
    ///  search should be refined (True) or search should be applied to whole
    ///  database (False).</param>
    ///  <remarks>A message box is dislayed if search returns no snippets.
    ///  </remarks>
    procedure DoSearchFilter(const Search: ISearch;
      const Refine: Boolean = False);
  strict protected
    ///  <summary>Initialises form.</summary>
    ///  <remarks>Once initialisation is complete the splash window is cancelled
    ///  and the form is enabled.</remarks>
    procedure InitForm; override;
    ///  <summary>Performs actions that need to run after the form is visible on
    ///  screen.</summary>
    procedure AfterShowForm; override;
  end;


var
  // Instance of main form
  MainForm: TMainForm;


implementation


uses
  // Delphi
  Windows,
  Graphics,
  // Project
  CS.Actions.RemoveTag,
  CS.Database.Types,
  CS.Init.CommandLineOpts,
  DB.UMain,
  FmSplash,
  FmTrappedBugReportDlg,
  FmWaitDlg,
  IntfFrameMgrs,
  Notifications.UDisplayMgr,
  UActionFactory,
  UAppInfo,
  UClassHelpers,
  UCodeShareMgr,
  UCommandBars,
  UConsts,
  UCopyInfoMgr,
  UCopySourceMgr,
  UDatabaseLoader,
  UDatabaseLoaderUI,
  UDetailTabAction,
  UEditSnippetAction,
  UExceptions,
  UHelpMgr,
  UHistoryMenus,
  UKeysHelper,
  UMessageBox,
  UNotifier,
  UNulDropTarget,
  UPrintMgr,
  UQuery,
  USaveSnippetMgr,
  USaveUnitMgr,
  USelectionIOMgr,
  UShowPrefsPageAction,
  UUserDBMgr,
  UView,
  UViewItemAction,
  UWBExternal,
  Web.UInfo;


{$R *.dfm}


{ TMainForm }

procedure TMainForm.actAboutExecute(Sender: TObject);
begin
  fDialogMgr.ShowAboutDlg;
end;

procedure TMainForm.actAddSnippetExecute(Sender: TObject);
begin
  TDBModificationMgr.AddSnippet;
end;

procedure TMainForm.actBackupDatabaseExecute(Sender: TObject);
begin
  TDBModificationMgr.BackupDatabase(Self);
end;

procedure TMainForm.actBugReportExecute(Sender: TObject);
begin
  fDialogMgr.ShowBugReportDlg;
end;

procedure TMainForm.actCloseAllDetailsTabsExecute(Sender: TObject);
begin
  fMainDisplayMgr.CloseDetailsTabs(dtcAll);
end;

procedure TMainForm.actCloseDetailsTabExecute(Sender: TObject);
begin
  fMainDisplayMgr.CloseDetailsTabs(dtcSelected);
end;

procedure TMainForm.actCloseDetailsTabsUpdate(Sender: TObject);
begin
  // Enables one or more details tabs to be closed.
  // Used by actCloseDetailsTab, actCloseUnselectedDetailsTabs and
  // actCloseAllDetailsTabs.
  (Sender as TAction).Enabled := fMainDisplayMgr.CanCloseDetailsTab;
end;

procedure TMainForm.actCloseUnselectedDetailsTabsExecute(Sender: TObject);
begin
  fMainDisplayMgr.CloseDetailsTabs(dtcAllExceptSelected);
end;

procedure TMainForm.actCompilersExecute(Sender: TObject);
begin
  if fCompileMgr.ConfigCompilers then
    fMainDisplayMgr.Refresh;
end;

procedure TMainForm.actConfigDiffProgExecute(Sender: TObject);
begin
  fDialogMgr.ExecConfigDiffProgDlg;
end;

procedure TMainForm.actCopyExecute(Sender: TObject);
begin
  fMainDisplayMgr.CopyToClipboard;
end;

procedure TMainForm.actCopyInfoExecute(Sender: TObject);
begin
  TCopyInfoMgr.Execute(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actCopyInfoUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    TCopyInfoMgr.CanHandleView(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actCopySnippetExecute(Sender: TObject);
begin
  TCopySnippetMgr.Execute(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actCopySnippetUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    TCopySnippetMgr.CanHandleView(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actCopySourceExecute(Sender: TObject);
begin
  TCopySourceMgr.Execute(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actCopySourceUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    TCopySourceMgr.CanHandleView(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actCopyUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := fMainDisplayMgr.CanCopy;
end;

procedure TMainForm.actDeleteSnippetExecute(Sender: TObject);
begin
  Assert(TDBModificationMgr.CanEdit(fMainDisplayMgr.CurrentView),
    ClassName + '.actDeleteSnippetExecute: Can''t delete current view item');
  TDBModificationMgr.DeleteSnippet(fMainDisplayMgr.CurrentView);
  // display update is handled by snippets change event handler
end;

procedure TMainForm.actDuplicateSnippetExecute(Sender: TObject);
begin
  TDBModificationMgr.DuplicateSnippet(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actDuplicateSnippetUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    TDBModificationMgr.CanDuplicate(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.ActEditDeleteSnippetUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    TDBModificationMgr.CanEdit(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.ActEditSnippetByNameExecute(Sender: TObject);
begin
  TDBModificationMgr.EditSnippet((Sender as TEditSnippetAction).SnippetID);
end;

procedure TMainForm.actEditSnippetExecute(Sender: TObject);
begin
  Assert(TDBModificationMgr.CanEdit(fMainDisplayMgr.CurrentView),
    ClassName + '.actEditSnippetExecute: Can''t edit current view item');
  fNotifier.EditSnippet(
    (fMainDisplayMgr.CurrentView as ISnippetView).SnippetID
  );
  // display of updated snippet is handled by snippets change event handler
end;

procedure TMainForm.actExportCodeExecute(Sender: TObject);
begin
  TCodeShareMgr.ExportCode(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actFavouritesExecute(Sender: TObject);
begin
  fDialogMgr.ShowFavouritesDlg(fNotifier);
end;

procedure TMainForm.actFindClearExecute(Sender: TObject);
begin
  Query.Reset;
  fMainDisplayMgr.UpdateDisplayedQuery;
  fStatusBarMgr.Update;
end;

procedure TMainForm.actFindClearUpdate(Sender: TObject);
begin
  // We have an active search if current search's criteria is not null
  (Sender as TAction).Enabled := Query.IsSearchActive;
end;

procedure TMainForm.actFindCompilerExecute(Sender: TObject);
var
  Search: USearch.ISearch;  // compiler search object
  Refine: Boolean;          // flag indicating if existing search to be refined
begin
  // Display Find Compiler dialogue box to enable user to enter search criteria
  // (dialogue box creates and returns search object from entered criteria)
  if fDialogMgr.ExecFindCompilerDlg(Search, Refine) then
    DoSearchFilter(Search, Refine);
end;

procedure TMainForm.actFindTagsExecute(Sender: TObject);
var
  Search: USearch.ISearch;  // text search object
  Refine: Boolean;          // flag indicating if existing search to be refined
begin
  // Display Find Tags dialogue box to enable user to enter search criteria
  // (dialogue box creates and returns search object from entered criteria)
  if fDialogMgr.ExecFindTagsDlg(Search, Refine) then
    DoSearchFilter(Search, Refine);
end;

procedure TMainForm.actFindTextExecute(Sender: TObject);
var
  Search: USearch.ISearch;  // text search object
  Refine: Boolean;          // flag indicating if existing search to be refined
begin
  // Display Find Text dialogue box to enable user to enter search criteria
  // (dialogue box creates and returns search object from entered criteria)
  if fDialogMgr.ExecFindTextDlg(Search, Refine) then
    DoSearchFilter(Search, Refine);
end;

procedure TMainForm.actFindXRefsExecute(Sender: TObject);
var
  Search: USearch.ISearch;  // cross reference search object
begin
  Assert(Supports(fMainDisplayMgr.CurrentView, ISnippetView),
    ClassName + '.actFindXRefsExecute: Current view is not a snippet');
  // Display Find Cross Refs dialogue box to enable user to enter search
  // criteria (dialogue box creates and returns search object from entered
  // criteria)
  if fDialogMgr.ExecFindXRefsDlg(
    Database.LookupSnippet(
      (fMainDisplayMgr.CurrentView as ISnippetView).SnippetID
    ),
    Search
  ) then
    DoSearchFilter(Search);
end;

procedure TMainForm.actFindXRefsUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    Supports(fMainDisplayMgr.CurrentView, ISnippetView);
end;

procedure TMainForm.actGoBackExecute(Sender: TObject);
var
  ViewItem: IView;  // previous view item in history list
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
  fMainDisplayMgr.DisplayViewItem(ViewItem, ssCtrl in ShiftKeysPressed);
end;

procedure TMainForm.actGoBackUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := fHistory.BackListCount > 0;
end;

procedure TMainForm.actGoForwardExecute(Sender: TObject);
var
  ViewItem: IView;  // next view item in history list
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
  fMainDisplayMgr.DisplayViewItem(ViewItem, ssCtrl in ShiftKeysPressed);
end;

procedure TMainForm.actGoForwardUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := fHistory.ForwardListCount > 0;
end;

procedure TMainForm.actHelpCompChecksExecute(Sender: TObject);
begin
  // Displays help topic indirected via custom help topic action
  DisplayHelp('CompChecks');
end;

procedure TMainForm.actHelpContentsExecute(Sender: TObject);
begin
  HelpMgr.ShowContents;
end;

procedure TMainForm.actHelpQuickStartExecute(Sender: TObject);
begin
  // Displays help topic indirected via custom help topic action
  DisplayHelp('QuickStart');
end;

procedure TMainForm.actImportCodeExecute(Sender: TObject);
begin
  TCodeShareMgr.ImportCode;
end;

procedure TMainForm.actLicenseExecute(Sender: TObject);
begin
  DisplayHelp('License');
end;

procedure TMainForm.actLoadSelectionExecute(Sender: TObject);
var
  Search: ISearch;
begin
  if TSelectionIOMgr.LoadSelectionSearch(Search) then
    DoSearchFilter(Search);
end;

procedure TMainForm.actMoveDatabaseExecute(Sender: TObject);
begin
  TDBModificationMgr.MoveDatabase;
end;

procedure TMainForm.actNewDetailsTabExecute(Sender: TObject);
begin
  fMainDisplayMgr.CreateNewDetailsTab;
end;

procedure TMainForm.actNextTabExecute(Sender: TObject);
begin
  fMainDisplayMgr.SelectNextDetailsTab;
end;

procedure TMainForm.ActNonEmptyDBUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not Database.IsEmpty;
end;

procedure TMainForm.ActOverviewGroupingExecute(Sender: TObject);
begin
  // Action's Tag property specifies index of grouping being selected
  fMainDisplayMgr.SelectOverviewGrouping((Sender as TAction).Tag);
end;

procedure TMainForm.ActOverviewGroupingUpdate(Sender: TObject);
begin
  // Action's Tag property specifies index of grouping being updated
  with Sender as TAction do
  begin
    Checked := fMainDisplayMgr.SelectedOverviewGrouping = Tag;
    Enabled := True;
  end;
end;

procedure TMainForm.actPreferencesExecute(Sender: TObject);
var
  UpdateUI: Boolean;  // flag true if preference changes affect main window UI
begin
  fDialogMgr.ExecPreferencesDlg(UpdateUI);
  if UpdateUI then
    fMainDisplayMgr.CompleteRefresh;
end;

procedure TMainForm.actPreviousTabExecute(Sender: TObject);
begin
  fMainDisplayMgr.SelectPreviousDetailsTab;
end;

procedure TMainForm.actPrintExecute(Sender: TObject);
begin
  Assert(TPrintMgr.CanPrint(fMainDisplayMgr.CurrentView),
    ClassName + '.actPrintExecute: View kind not vkRoutine');
  // Display print dialogue box
  if fDialogMgr.ExecPrintDlg then
    // User OKd: print document
    TPrintMgr.Print(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actPrintUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    TPrintMgr.CanPrint(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actPrivacyExecute(Sender: TObject);
begin
  DisplayHelp('PrivacyStatement');
end;

procedure TMainForm.actProgramUpdatesExecute(Sender: TObject);
begin
  fDialogMgr.ShowProgramUpdatesDlg;
end;

procedure TMainForm.actProxyServerExecute(Sender: TObject);
begin
  fDialogMgr.ExecProxyServerDlg;
end;

procedure TMainForm.ActRemoveTagExecute(Sender: TObject);
begin
  TDBModificationMgr.RemoveTagFromSnippet(
    (Sender as TRemoveTagAction).SnippetID, (Sender as TRemoveTagAction).Tag
  );
end;

procedure TMainForm.actRestoreDatabaseExecute(Sender: TObject);
begin
  if TDBModificationMgr.RestoreDatabase(Self) then
    ReloadDatabase;
end;

procedure TMainForm.actSaveDatabaseExecute(Sender: TObject);
begin
  TDBModificationMgr.Save(Self);
  fStatusBarMgr.Update;
end;

procedure TMainForm.actSaveDatabaseUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := TDBModificationMgr.CanSave;
end;

procedure TMainForm.actSaveSelectionExecute(Sender: TObject);
begin
  TSelectionIOMgr.SaveCurrentSelection;
end;

procedure TMainForm.actSaveSelectionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := TSelectionIOMgr.CanSaveCurrentSelection;
end;

procedure TMainForm.actSaveSnippetExecute(Sender: TObject);
begin
  TSaveSnippetMgr.Execute(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actSaveSnippetUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    TSaveSnippetMgr.CanHandleView(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actSaveUnitExecute(Sender: TObject);
begin
  TSaveUnitMgr.Execute(Query.Selection);
end;

procedure TMainForm.actSelectAllExecute(Sender: TObject);
begin
  fMainDisplayMgr.SelectAll;
end;

procedure TMainForm.actSelectAllUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := fMainDisplayMgr.CanSelectAll;
end;

procedure TMainForm.ActSelectDetailTabExecute(Sender: TObject);
begin
  fMainDisplayMgr.SelectDetailTab((Sender as TDetailTabAction).TabIndex);
end;

procedure TMainForm.actSelectSnippetsExecute(Sender: TObject);
var
  Search: USearch.ISearch;  // selection search object
begin
  // Display Select Snippets dialogue box to enable user to specify required
  // snippets (dialogue box creates and returns search object for required
  // snippets)
  if fDialogMgr.ExecSelectionSearchDlg(Query.Selection, Search) then
    DoSearchFilter(Search);
end;

procedure TMainForm.ActShowPrefsPageExecute(Sender: TObject);
var
  UpdateUI: Boolean;  // flag true if preference changes affect main window UI
begin
  fDialogMgr.ExecPreferencesDlg(
    (Sender as TShowPrefsPageAction).FrameClassName, UpdateUI
  );
  if UpdateUI then
    fMainDisplayMgr.CompleteRefresh;
end;

procedure TMainForm.actSubmitExecute(Sender: TObject);
begin
  TCodeShareMgr.Submit(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.ActSubmitOrExportUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := TCodeShareMgr.CanShare;
end;

procedure TMainForm.actSWAGImportExecute(Sender: TObject);
begin
  fDialogMgr.ShowSWAGImportDlg;
end;

procedure TMainForm.actTestBugExecute(Sender: TObject);
const
  // Bug error message
  cFakeError = 'Bug check: raised by pressing Shift+Ctrl+Alt+B. '
    + 'This is a program generated bug used for testing purposes only. '
    + 'PLEASE DO NOT REPORT!';
begin
  raise EBug.Create(cFakeError);
end;

procedure TMainForm.actTestCompileExecute(Sender: TObject);
begin
  Assert(
    fCompileMgr.CanCompile(fMainDisplayMgr.CurrentView),
    ClassName + '.actTestCompileExecute: Can''t compile current view');
  fDialogMgr.ShowTestCompileDlg(
    fCompileMgr,
    Database.LookupSnippet(
      (fMainDisplayMgr.CurrentView as ISnippetView).SnippetID
    )
  );
end;

procedure TMainForm.actTestCompileUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    fCompileMgr.CanCompile(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actToggleFavouriteExecute(Sender: TObject);
begin
  actToggleFavourite.Checked := not actToggleFavourite.Checked;
  fNotifier.ChangeSnippetStar(
    (fMainDisplayMgr.CurrentView as ISnippetView).SnippetID,
    actToggleFavourite.Checked
  );
end;

procedure TMainForm.actToggleFavouriteUpdate(Sender: TObject);
begin
  actToggleFavourite.Enabled := TDBModificationMgr.CanEdit(
    fMainDisplayMgr.CurrentView
  );
  if actToggleFavourite.Enabled then
    actToggleFavourite.Checked :=
      TDBModificationMgr.IsSnippetStarred(
        (fMainDisplayMgr.CurrentView as ISnippetView).SnippetID
      )
  else
    actToggleFavourite.Checked := False;
end;

procedure TMainForm.ActTreeStateChangeExecute(Sender: TObject);
begin
  fMainDisplayMgr.UpdateOverviewTreeState(
    TTreeNodeAction((Sender as TAction).Tag)
  );
end;

procedure TMainForm.ActTreeStateChangeUpdate(Sender: TObject);
var
  Action: TAction;  // action triggering this event
begin
  Action := Sender as TAction;
  Action.Enabled := fMainDisplayMgr.CanUpdateOverviewTreeState(
    TTreeNodeAction(Action.Tag)
  )
end;

procedure TMainForm.actUpdateDbaseExecute(Sender: TObject);
{ TODO -cCommented Out: Reinstate this action or similar to check for updates to
                        linked spaces.
                        NOTE: associated action has been made invisible. }
//resourcestring
//  sConfirmSave = 'The database has been changed. Do you wish to save it '
//    + 'before updating the database?' + EOL2 + 'Clicking No will cause all '
//    + 'recent changes to be lost.';
begin
//  if fDialogMgr.ExecDBUpdateDlg then
//  begin
//    // Database was updated: check if database needs saving
//    if (Database as IDatabaseEdit).Updated
//      and TMessageBox.Confirm(Self, sConfirmSave) then
//      TUserDBMgr.Save(Self);
//    // Reload the databases
//    ReloadDatabase;
//  end;
  TMessageBox.Error(Self, 'actUpdateDbase temporarily disabled');
end;

procedure TMainForm.actViewCompErrsExecute(Sender: TObject);
begin
  fCompileMgr.ShowErrors;
end;

procedure TMainForm.actViewCompErrsUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    fCompileMgr.IsLastCompiledView(fMainDisplayMgr.CurrentView)
    and fCompileMgr.HaveErrors;
end;

procedure TMainForm.actViewDependenciesExecute(Sender: TObject);
var
  Search: ISearch;
begin
  Assert(Supports(fMainDisplayMgr.CurrentView, ISnippetView),
    ClassName + '.actViewDependenciesExecute: Snippet view expected');
  Search := fDialogMgr.ShowDependenciesDlg(
    Database.LookupSnippet(
      (fMainDisplayMgr.CurrentView as ISnippetView).SnippetID
    ),
    'DependenciesDlg'
  );
  if Assigned(Search) then
    DoSearchFilter(Search);
end;

procedure TMainForm.actViewDependenciesUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    Supports(fMainDisplayMgr.CurrentView, ISnippetView);
end;

procedure TMainForm.ActViewHistoryItemExecute(Sender: TObject);
begin
  fMainDisplayMgr.DisplayViewItem(
    (Sender as TViewItemAction).ViewItem, (Sender as TViewItemAction).NewTab
  );
end;

procedure TMainForm.ActViewItemExecute(Sender: TObject);
begin
  fMainDisplayMgr.DisplayViewItem(
    (Sender as TViewItemAction).ViewItem, (Sender as TViewItemAction).NewTab
  );
  fHistory.NewItem((Sender as TViewItemAction).ViewItem);
end;

procedure TMainForm.actViewTestUnitExecute(Sender: TObject);
var
  SelectedSnippet: ISnippet;  // currently selected snippet
begin
  Assert(Supports(fMainDisplayMgr.CurrentView, ISnippetView),
    ClassName + '.actViewTestUnitExecute: Snippet view expected');
  SelectedSnippet := Database.LookupSnippet(
    (fMainDisplayMgr.CurrentView as ISnippetView).SnippetID
  );
  Assert(SelectedSnippet.CanCompile,
    ClassName + '.actViewTestUnitExecute: Snippet is not compilable');
  fDialogMgr.ShowTestUnitDlg(SelectedSnippet);
end;

procedure TMainForm.actViewTestUnitUpdate(Sender: TObject);
var
  SnippetView: ISnippetView;  // current view as snippet view if supported
begin
  (Sender as TAction).Enabled :=
    Supports(fMainDisplayMgr.CurrentView, ISnippetView, SnippetView)
    and Database.LookupSnippet(SnippetView.SnippetID).CanCompile;
end;

procedure TMainForm.actWelcomeExecute(Sender: TObject);
begin
  fMainDisplayMgr.ShowWelcomePage;
end;

procedure TMainForm.AfterShowForm;
begin
  inherited;
  // initialise display
  fMainDisplayMgr.Initialise(fWindowSettings.OverviewGrouping);
  fMainDisplayMgr.ShowWelcomePage;
end;

procedure TMainForm.appEventsHint(Sender: TObject);
begin
  if Assigned(fStatusBarMgr) then
    fStatusBarMgr.ShowHint(Application.Hint);
end;

procedure TMainForm.DBChangeHandler(Sender: TObject; const EvtInfo: IInterface);
begin
  case (EvtInfo as IDatabaseChangeEventInfo).Kind of
    evChangeBegin:
      Enabled := False;
    evChangeEnd:
      Enabled := True;
  end;
  // Display updated database stats and search results in status bar
  fStatusBarMgr.Update;
end;

procedure TMainForm.DoSearchFilter(const Search: USearch.ISearch;
  const Refine: Boolean);
var
  PrevSearchExists: Boolean;  // flags if a search was active before this search

  ///  Returns a suitable error message depending on if a search was active
  ///  before this search was run and what action was taken on failure.
  function FailureMessage: string;
  resourcestring
    // dialogue box message
    sFailureMsg = 'The search did not find any snippets.';
    sSearchNotChanged = 'No changes were made to the existing search results.';
    sDatabaseSelected = 'The previous search results have been cleared.';
  begin
    Result := sFailureMsg;
    if PrevSearchExists then
      if Refine then
        Result := Result + EOL2 + sSearchNotChanged
      else
        Result := Result + EOL2 + sDatabaseSelected;
  end;

begin
  // Check if a search is already active: needed to create error message
  PrevSearchExists := Query.IsSearchActive;
  if not Query.ApplySearch(Search, Refine) then
    TMessageBox.Information(Self, FailureMessage);
  fMainDisplayMgr.UpdateDisplayedQuery;
  fStatusBarMgr.Update;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  try
    inherited;
    // Disable form
    Enabled := False;
    // Set up application events
    appEvents.OnException := HandleExceptions;
  except
    // Make sure form is enabled and splash form hidden on exception
    Enabled := True;
    SplashForm.RequestClose;
    raise;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  inherited;
  // Stop update checking threads
  fUpdateChecker.StopThreads;
  fUpdateChecker.Free;

  // Stop notification display sub-system
  TNotificationDisplayMgr.Stop;

  // Save any changes to snippets database
  if Database.IsDirty then
    Database.Save;

  // Unhook snippets event handler
  Database.RemoveChangeEventHandler(DBChangeHandler);
  // Save window state
  fWindowSettings.SplitterPos := pnlLeft.Width;
  fWindowSettings.OverviewGrouping := fMainDisplayMgr.SelectedOverviewGrouping;
  fWindowSettings.Save;

  // Free owned objects
  fHistory.Free;
  fMainDisplayMgr.Free;
  // fStatusBarMgr MUST be nilled: otherwise it can be called after status bar
  // control has been freed and so cause AV when trying to use the control
  FreeAndNil(fStatusBarMgr);

end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  inherited;
  if pnlLeft.Width > ClientWidth - TMainWindowSettings.MinRightPanelWidth
    - splitVert.Width then
    pnlLeft.Width := ClientWidth - TMainWindowSettings.MinRightPanelWidth
      - splitVert.Width;
end;

procedure TMainForm.HandleExceptions(Sender: TObject; E: Exception);
begin
  if (E is ECodeSnip) or (E is EFileStreamError) then
    TMessageBox.Error(nil, E.Message)
  else
    TTrappedBugReportDlg.Execute(nil, E);
end;

procedure TMainForm.InitForm;
var
  WBExternal: IDispatch;        // external object of browser control
begin
  try
    inherited;
    // Set up action image list
    ilMain.LoadFromResource(RT_RCDATA, 'ACTIONIMAGES', 16, clFuchsia);
    RefreshActions;

    // Set window caption
    Application.Title := TAppInfo.ProgramCaption;
    Caption := TAppInfo.ProgramCaption;

    // Restore window settings
    fWindowSettings := TMainWindowSettings.CreateStandAlone(Self); // auto-freed
    fWindowSettings.Restore;                                // sizes main window
    pnlLeft.Width := fWindowSettings.SplitterPos;

    // Initialise actions
    // Browse actions have to have URLs set dynamically
    actHomePage.URL := TWebInfo.ProgramHomeURL;
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
    actViewGroupByTags.Tag := cTagsGrouping;
    actViewGroupAlphabetically.Tag := cAlphabeticGrouping;
    actViewGroupBySnippetKinds.Tag := cKindGrouping;
    actViewGroupByLanguage.Tag := cSourceCodeLanguageGrouping;
    // Move database option not available in portable mode
    actMoveDatabase.Visible := not TCommandLineOpts.IsPortable;

    // Create notifier object and assign actions triggered by its methods note
    // that actions created on fly are automatically freed
    fNotifier := TNotifier.Create;
    with fNotifier as ISetActions do
    begin
      SetUpdateDbaseAction(actUpdateDbase);
      SetDisplaySnippetAction(TActionFactory.CreateSnippetAction(Self));
      SetConfigCompilersAction(actCompilers);
      SetShowViewItemAction(
        TActionFactory.CreateViewItemAction(Self, ActViewItemExecute)
      );
      SetOverviewStyleChangeActions(
        [
          actViewGroupByTags,
          actViewGroupAlphabetically,
          actViewGroupBySnippetKinds,
          actViewGroupByLanguage
        ]
      );
      SetDetailPaneChangeAction(
        TActionFactory.CreateDetailTabAction(Self, ActSelectDetailTabExecute)
      );
      SetEditSnippetAction(
        TActionFactory.CreateEditSnippetAction(
          Self, ActEditSnippetByNameExecute
        )
      );
      SetNewSnippetAction(actAddSnippet);
      SetCheckForUpdatesAction(actProgramUpdates);
      SetAboutBoxAction(actAbout);
      SetShowPrefsPageAction(
        TActionFactory.CreateShowPrefsPageAction(Self, ActShowPrefsPageExecute)
      );
      SetDisplayTagAction(TActionFactory.CreateDisplayTagAction(Self));
      SetRemoveTagAction(
        TActionFactory.CreateRemoveTagAction(Self, ActRemoveTagExecute)
      );
      SetDisplayLanguageAction(
        TActionFactory.CreateDisplayLanguageAction(Self)
      );
      SetChangeSnippetStarAction(
        TActionFactory.CreateChangeSnippetStarAction(Self)
      );
    end;

    // Customise web browser controls in Details pane
    WBExternal := TWBExternal.Create;
    with frmDetail as IWBCustomiser do
    begin
      SetExternalObj(WBExternal);
      SetDragDropHandler(TNulDropTarget.Create);
    end;

    // Set notifier for objects that trigger notifications
    (WBExternal as ISetNotifier).SetNotifier(fNotifier);
    (frmOverview as ISetNotifier).SetNotifier(fNotifier);
    (frmDetail as ISetNotifier).SetNotifier(fNotifier);

    // Create dialogue box manager
    fDialogMgr := TDialogMgr.Create(Self);  // automatically freed

    // Create display manager
    // NOTE: Don't display anything until after window has displayed (see
    // AfterShowForm). This is to prevent any problems with IE 10 browser ctrl.
    fMainDisplayMgr := TMainDisplayMgr.Create(frmOverview, frmDetail);

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
    with frmDetail as ICommandBarConfig do
    begin
      // set images to use
      SetImages(ilMain);
      // detail view menus
      AddAction(
        TActionFactory.CreateLinkAction(Self),
        [cDetailPopupMenuAnchor, cDetailPopupMenuImage]
      );
      AddSpacer([cDetailPopupMenuAnchor, cDetailPopupMenuImage]);
      AddAction(actEditSnippet, cDetailPopupMenuIDs);
      AddSpacer(cDetailPopupMenuIDs);
      AddAction(actViewDependencies, cDetailPopupMenuIDs);
      AddSpacer(cDetailPopupMenuIDs);
      AddAction(actCopyInfo, cDetailPopupMenuIDs);
      AddAction(actCopySnippet, cDetailPopupMenuIDs);
      AddAction(actCopySource, cDetailPopupMenuIDs);
      AddSpacer(cDetailPopupMenuIDs);
      AddAction(actTestCompile, cDetailPopupMenuIDs);
      AddSpacer(cDetailPopupMenuIDs);
      AddAction(actSaveSnippet, cDetailPopupMenuIDs);
      AddAction(actPrint, cDetailPopupMenuIDs);
      AddSpacer(cDetailPopupMenuIDs);
      AddAction(actCopy, cDetailPopupMenuTextSelect);
      AddAction(actSelectAll, cDetailPopupMenuIDs);
      AddSpacer(cDetailPopupMenuIDs);
      AddAction(actCloseDetailsTab, cDetailPopupMenuIDs);
      // tab set menu
      AddAction(actCloseDetailsTab, cDetailTabSetPopupMenu);
      AddAction(actCloseUnselectedDetailsTabs, cDetailTabSetPopupMenu);
    end;

    // Set up overview pane's toolbar and popup menu
    with frmOverview as ICommandBarConfig do
    begin
      SetImages(ilMain);
      // add toolbar actions (in reverse order we want them!)
      AddAction(actCollapseTree, cOverviewToolBar);
      AddAction(actExpandTree, cOverviewToolBar);
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

    // Create object to handle compilation and assoicated UI and dialogues
    fCompileMgr := TMainCompileMgr.Create(Self);  // auto-freed

    // Set event handler for snippets database
    Database.AddChangeEventHandler(DBChangeHandler);

    // Load snippets database
    LoadSnippets(
      procedure
      begin
        try
          TDatabaseLoader.Load;
        except
          on E: ECodeSnip do
            Application.HandleException(E);
        end;
      end
    );

    // Start notification display sub-system
    TNotificationDisplayMgr.Start(Self);

    // Start update checking manager
    // *** Should be done after notification window listener starts
    fUpdateChecker := TUpdateCheckerMgr.Create;
    fUpdateChecker.StartThreads;
  finally
    // Ready to start using app: request splash form closes and enable form
    SplashForm.RequestClose;
    Enabled := True;
  end;
end;

procedure TMainForm.LoadSnippets(const Loader: TProc);
resourcestring
  sLoadingDatabase  = 'Loading database...';  // status bar message
begin
  // Inform that database is being loaded via status bar
  fStatusBarMgr.ShowSimpleMessage(sLoadingDatabase);
  fHistory.Clear;
  // Load the database
  try
    Loader;
  except
    on E: ECodeSnip do
      Application.HandleException(E);
  end;
  // Re-initialise display
  fMainDisplayMgr.ReStart;
  // Display updated database stats and search results in status bar
  fStatusBarMgr.Update;
end;

procedure TMainForm.ReloadDatabase;
begin
  fMainDisplayMgr.PrepareForDBReload;
  LoadSnippets(
    procedure
    begin
      TDatabaseLoaderUI.Execute(Self);
    end
  );
  fMainDisplayMgr.ShowDBUpdatedPage;
end;

procedure TMainForm.splitVertCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  if (NewSize < TMainWindowSettings.MinLeftPanelWidth)
    or (NewSize > ClientWidth - TMainWindowSettings.MinRightPanelWidth) then
    Accept := False;
end;

end.

