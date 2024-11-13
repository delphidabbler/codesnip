{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2024, Peter Johnson (gravatar.com/delphidabbler).
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
  Favourites.UManager,
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
  UWindowSettings;


type
  ///  <summary>Form that provides the application's main window.</summary>
  TMainForm = class(THelpAwareForm)
    actAbout: TAction;
    actAddCategory: TAction;
    actAddFavourite: TAction;
    actAddSnippet: TAction;
    actBackupDatabase: TAction;
    actBlog: TBrowseURL;
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
    actDeleteCategory: TAction;
    actDeleteSnippet: TAction;
    actDeleteUserDatabase: TAction;
    actDuplicateSnippet: TAction;
    actEditSnippet: TAction;
    actExit: TFileExit;
    actExpandNode: TAction;
    actExpandTree: TAction;
    actExportCode: TAction;
    actFavourites: TAction;
    actFAQs: TBrowseURL;
    actFindClear: TAction;
    actFindCompiler: TAction;
    actFindText: TAction;
    actFindXRefs: TAction;
    actGoBack: TAction;
    actGoForward: TAction;
    actHelpCompChecks: TAction;
    actHelpContents: TAction;
    actHelpQuickStart: TAction;
    actGitHubHome: TBrowseURL;
    actImportCode: TAction;
    actLicense: TAction;
    actLoadSelection: TAction;
    actMoveUserDatabase: TAction;
    actNextTab: TAction;
    actNewDetailsTab: TAction;
    actPreferences: TAction;
    actPreviousTab: TAction;
    actPrint: TAction;
    actRenameCategory: TAction;
    actRestoreDatabase: TAction;
    actSaveDatabase: TAction;
    actSaveSelection: TAction;
    actSaveSnippet: TAction;
    actSaveUnit: TAction;
    actSelectAll: TAction;
    actSelectSnippets: TAction;
    actSWAGImport: TAction;
    actTestBug: TAction;
    actTestCompile: TAction;
    actUpdateDbase: TAction;
    actViewAlphabetical: TAction;
    actViewCategorised: TAction;
    actViewCompErrs: TAction;
    actViewDependencies: TAction;
    actViewSnippetKinds: TAction;
    actViewTestUnit: TAction;
    actWelcome: TAction;
    alMain: TActionList;
    appEvents: TApplicationEvents;
    frmDetail: TDetailFrame;
    frmOverview: TOverviewFrame;
    ilMain: TImageList;
    miAbout: TMenuItem;
    miAddCategory: TMenuItem;
    miAddFavourite: TMenuItem;
    miAddSnippet: TMenuItem;
    miBackupDatabase: TMenuItem;
    miBlog: TMenuItem;
    miCategories: TMenuItem;
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
    miDeleteCategory: TMenuItem;
    miDeleteSnippet: TMenuItem;
    miDeleteUserDatabase: TMenuItem;
    miDuplicateSnippet: TMenuItem;
    miEdit: TMenuItem;
    miEditSnippet: TMenuItem;
    miExit: TMenuItem;
    miExpandNode: TMenuItem;
    miExpandTree: TMenuItem;
    miExportCode: TMenuItem;
    miFavourites: TMenuItem;
    miFAQs: TMenuItem;
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
    miMoveUserDatabase: TMenuItem;
    miNewDetailsTab: TMenuItem;
    miPreferences: TMenuItem;
    miPrint: TMenuItem;
    miRenameCategory: TMenuItem;
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
    miSpacer20: TMenuItem;
    miSpacer21: TMenuItem;
    miSWAGImport: TMenuItem;
    miTestCompile: TMenuItem;
    miTools: TMenuItem;
    miUpdateDbase: TMenuItem;
    miView: TMenuItem;
    miViewCategorised: TMenuItem;
    miViewCompErrs: TMenuItem;
    miViewDependencies: TMenuItem;
    miViewSnippetKinds: TMenuItem;
    miViewTestUnit: TMenuItem;
    miViewAlphabetical: TMenuItem;
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
    tbFavourites: TToolButton;
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
    tbSpacer5: TToolButton;
    tbSpacer6: TToolButton;
    tbSpacer7: TToolButton;
    tbSpacer8: TToolButton;
    tbTestCompile: TToolButton;
    ///  <summary>Displays About Box.</summary>
    procedure actAboutExecute(Sender: TObject);
    ///  <summary>Gets a new category from user and adds to database.</summary>
    procedure actAddCategoryExecute(Sender: TObject);
    ///  <summary>Adds current snippet to favourites.</summary>
    procedure actAddFavouriteExecute(Sender: TObject);
    ///  <summary>Determines whether AddFavourites action can be enabled.
    ///  </summary>
    procedure actAddFavouriteUpdate(Sender: TObject);
    ///  <summary>Opens Snippets Editor to add a new user defined snippet to the
    ///  database.</summary>
    procedure actAddSnippetExecute(Sender: TObject);
    ///  <summary>Makes a backup of the user database.</summary>
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
    ///  <summary>Copies selected text from details pane to clipboard.</summary>
    procedure actCopyExecute(Sender: TObject);
    ///  <summary>Copies information about current snippet to clipboard.
    ///  </summary>
    procedure actCopyInfoExecute(Sender: TObject);
    ///  <summary>Determines whether CopyInfo action can be enabled.</summary>
    procedure actCopyInfoUpdate(Sender: TObject);
    ///  <summary>Copies annotated source code of selected snippet or selected
    ///  category to clipboard.</summary>
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
    ///  <summary>Displays a dialogue box that can be used to delete a user
    ///  defined category.</summary>
    procedure actDeleteCategoryExecute(Sender: TObject);
    ///  <summary>Determines whether DeleteCategory action can be enabled.
    ///  </summary>
    procedure actDeleteCategoryUpdate(Sender: TObject);
    ///  <summary>Attempts to delete the current user defined snippet from the
    ///  database.</summary>
    procedure actDeleteSnippetExecute(Sender: TObject);
    ///  <summary>Requests permission then attempts to delete the user defined
    ///  snippets database.</summary>
    procedure actDeleteUserDatabaseExecute(Sender: TObject);
    ///  <summary>Displays a dialogue box that can be used to duplicate the
    ///  selected snippet.</summary>
    procedure actDuplicateSnippetExecute(Sender: TObject);
    ///  <summary>Determines whether DuplicateSnippet action can be enabled.
    ///  </summary>
    procedure actDuplicateSnippetUpdate(Sender: TObject);
    ///  <summary>Determines whether the DeleteSnippet or EditSnippet actions
    ///  can be enabled.</summary>
    procedure ActEditDeleteSnippetUpdate(Sender: TObject);
    ///  <summary>Displays a dialogue box that can be used to export one or
    ///  more user defined snippets.</summary>
    procedure actExportCodeExecute(Sender: TObject);
    ///  <summary>Opens current user defined snippet in Snippets Editor for
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
    ///  <summary>Displays a dialogue box that can be used to move the user
    ///  database to a user defined directory.</summary>
    ///  <remarks>This action must be hidden in the portable edition.</remarks>
    procedure actMoveUserDatabaseExecute(Sender: TObject);
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
    procedure ActOverviewTabExecute(Sender: TObject);
    ///  <summary>Updates checked state of OverviewTab action referenced by
    ///  Sender.</summary>
    ///  <remarks>Action is checked iff its index, stored in the Tag property,
    ///  is the currently selected overview tab index.</remarks>
    procedure ActOverviewTabUpdate(Sender: TObject);
    ///  <summary>Displays the Preferences dialogue box.</summary>
    procedure actPreferencesExecute(Sender: TObject);
    ///  <summary>Displays previous tab in either the overview or details pane
    ///  depending on which pane is active.</summary>
    procedure actPreviousTabExecute(Sender: TObject);
    ///  <summary>Displays the Print dialogue box that can be used to print the
    ///  selected snippet or category.</summary>
    procedure actPrintExecute(Sender: TObject);
    ///  <summary>Determines whether the Print action can be enabled.</summary>
    procedure actPrintUpdate(Sender: TObject);
    ///  <summary>Displays a dialogue box that can be used to rename a user
    ///  defined category.</summary>
    procedure actRenameCategoryExecute(Sender: TObject);
    ///  <summary>Determines whether the RenameCategory action can be enabled.
    ///  </summary>
    procedure actRenameCategoryUpdate(Sender: TObject);
    ///  <summary>Displays a dialogue box from which a backup file can be
    ///  selected and used to restore the user defined database.</summary>
    procedure actRestoreDatabaseExecute(Sender: TObject);
    ///  <summary>Save any changes in the user defined database to disk.
    ///  </summary>
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
    ///  or category to disk in a user-specified file.</summary>
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
    ///  <summary>Determines whether the Submit or ExportCode actions can be
    ///  enabled.</summary>
    procedure ActSubmitOrExportUpdate(Sender: TObject);
    ///  <summary>Displays the SWAG import dialogue box that enables the user to
    ///  import snippets from the SWAG database into the user-defined database.
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
    ///  <summary>Handles events triggered when help system is invoked. Prevents
    ///  exception being raised when F1 key is pressed when a menu is dropped
    ///  down.</summary>
    function appEventsHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
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
    procedure ActNonEmptyUserDBUpdate(Sender: TObject);
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
      ///  <summary>Object that manages favourites.</summary>
      fFavouritesMgr: TFavouritesManager;

    ///  <summary>Displays view item given by TViewItemAction instance
    ///  referenced by Sender and adds to history list.</summary>
    procedure ActViewItemExecute(Sender: TObject);
    ///  <summary>Displays view item from history list given by TViewItemAction
    ///  instance referenced by Sender.</summary>
    procedure ActViewHistoryItemExecute(Sender: TObject);
    ///  <summary>Opens snippet in Snippets Editor for editing. The snippet ID 
    ///  is provided by the TEditSnippetAction instance referenced by Sender.
    ///  </summary>
    procedure ActEditSnippetByIDExecute(Sender: TObject);
    ///  <summary>Selects a tab in the details pane where the tab is provided by
    ///  the TDetailTabAction instance referenced by Sender.</summary>
    procedure ActSelectDetailTabExecute(Sender: TObject);
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
  Windows, Graphics,
  // Project
  ClassHelpers.UControls,
  ClassHelpers.UGraphics,
  DB.UCategory,
  DB.UCollections,
  DB.UMain, DB.USnippet, FmSplash, FmTrappedBugReportDlg,
  FmWaitDlg, IntfFrameMgrs, UActionFactory, UAppInfo,
  UCodeShareMgr, UCommandBars, UConsts, UCopyInfoMgr,
  UCopySourceMgr, UDatabaseLoader, UDatabaseLoaderUI, UDetailTabAction,
  UEditSnippetAction, UExceptions, UHelpMgr, UHistoryMenus, UKeysHelper,
  UMessageBox, UNotifier, UNulDropTarget, UPrintMgr, UQuery, USaveSnippetMgr,
  USaveUnitMgr, USelectionIOMgr, UUrl, UUserDBMgr, UView, UViewItemAction,
  UWBExternal;


{$R *.dfm}


{ TMainForm }

procedure TMainForm.actAboutExecute(Sender: TObject);
begin
  fDialogMgr.ShowAboutDlg;
end;

procedure TMainForm.actAddCategoryExecute(Sender: TObject);
begin
  TUserDBMgr.AddCategory;
end;

procedure TMainForm.actAddFavouriteExecute(Sender: TObject);
begin
  fFavouritesMgr.AddFavourite(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actAddFavouriteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := fFavouritesMgr.CanAddFavourite(
    fMainDisplayMgr.CurrentView
  );
end;

procedure TMainForm.actAddSnippetExecute(Sender: TObject);
begin
  TUserDBMgr.AddSnippet;
end;

procedure TMainForm.actBackupDatabaseExecute(Sender: TObject);
begin
  if (Database as IDatabaseEdit).Updated then
    TUserDBMgr.Save(Self);
  TUserDBMgr.BackupDatabase(Self);
  fStatusBarMgr.Update;
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

procedure TMainForm.actDeleteCategoryExecute(Sender: TObject);
begin
  TUserDBMgr.DeleteACategory;
end;

procedure TMainForm.actDeleteCategoryUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := TUserDBMgr.CanDeleteACategory;
end;

procedure TMainForm.actDeleteSnippetExecute(Sender: TObject);
begin
  Assert(TUserDBMgr.CanEdit(fMainDisplayMgr.CurrentView),
    ClassName + '.actDeleteSnippetExecute: Can''t delete current view item');
  TUserDBMgr.DeleteSnippet(fMainDisplayMgr.CurrentView);
  // display update is handled by snippets change event handler
end;

procedure TMainForm.actDeleteUserDatabaseExecute(Sender: TObject);
begin
  if (Database as IDatabaseEdit).Updated then
    TUserDBMgr.Save(Self);
  if TUserDBMgr.DeleteDatabase then
  begin
    ReloadDatabase;
    fStatusBarMgr.Update;
  end;
end;

procedure TMainForm.actDuplicateSnippetExecute(Sender: TObject);
begin
  TUserDBMgr.DuplicateSnippet(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actDuplicateSnippetUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    TUserDBMgr.CanDuplicate(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.ActEditDeleteSnippetUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    TUserDBMgr.CanEdit(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.ActEditSnippetByIDExecute(Sender: TObject);
begin
  TUserDBMgr.EditSnippet((Sender as TEditSnippetAction).ID);
end;

procedure TMainForm.actEditSnippetExecute(Sender: TObject);
var
  Snippet: TSnippet;
begin
  Assert(TUserDBMgr.CanEdit(fMainDisplayMgr.CurrentView),
    ClassName + '.actEditSnippetExecute: Can''t edit current view item');
  Snippet := (fMainDisplayMgr.CurrentView as ISnippetView).Snippet;
  fNotifier.EditSnippet(Snippet.Key, Snippet.CollectionID);
  // display of updated snippet is handled by snippets change event handler
end;

procedure TMainForm.actExportCodeExecute(Sender: TObject);
begin
  TCodeShareMgr.ExportCode(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actFavouritesExecute(Sender: TObject);
begin
  fFavouritesMgr.ShowDialogue;
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
    (fMainDisplayMgr.CurrentView as ISnippetView).Snippet, Search
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

procedure TMainForm.actMoveUserDatabaseExecute(Sender: TObject);
begin
  if (Database as IDatabaseEdit).Updated then
    TUserDBMgr.Save(Self);
  TUserDBMgr.MoveDatabase;
end;

procedure TMainForm.actNewDetailsTabExecute(Sender: TObject);
begin
  fMainDisplayMgr.CreateNewDetailsTab;
end;

procedure TMainForm.actNextTabExecute(Sender: TObject);
begin
  fMainDisplayMgr.SelectNextActiveTab;
end;

procedure TMainForm.ActNonEmptyDBUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not Database.Snippets.IsEmpty;
end;

procedure TMainForm.ActNonEmptyUserDBUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not Database.Snippets.IsEmpty(
    TCollectionID.__TMP__UserDBCollectionID
  );
end;

procedure TMainForm.ActOverviewTabExecute(Sender: TObject);
begin
  // Action's Tag property specifies index of tab being selected
  fMainDisplayMgr.SelectOverviewTab((Sender as TAction).Tag);
end;

procedure TMainForm.ActOverviewTabUpdate(Sender: TObject);
var
  Action: TAction;
begin
  // Action's Tag property specifies index of tab being updated
  Action := (Sender as TAction);
  Action.Checked := fMainDisplayMgr.SelectedOverviewTab = Tag;
  Action.Enabled := True;
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
  fMainDisplayMgr.SelectPreviousActiveTab;
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

procedure TMainForm.actRenameCategoryExecute(Sender: TObject);
begin
  TUserDBMgr.RenameACategory;
end;

procedure TMainForm.actRenameCategoryUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := TUserDBMgr.CanRenameACategory;
end;

procedure TMainForm.actRestoreDatabaseExecute(Sender: TObject);
begin
  if TUserDBMgr.RestoreDatabase(Self) then
  begin
    ReloadDatabase;
    fStatusBarMgr.Update;
  end;
end;

procedure TMainForm.actSaveDatabaseExecute(Sender: TObject);
begin
  TUserDBMgr.Save(Self);
  fStatusBarMgr.Update;
end;

procedure TMainForm.actSaveDatabaseUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := TUserDBMgr.CanSave;
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
    (fMainDisplayMgr.CurrentView as ISnippetView).Snippet
  );
end;

procedure TMainForm.actTestCompileUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    fCompileMgr.CanCompile(fMainDisplayMgr.CurrentView);
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
resourcestring
  sConfirmSave = 'The user database has been changed. Do you wish to save it '
    + 'before updating the database?' + EOL2 + 'Clicking No will cause all '
    + 'recent changes to be lost.';
begin
  if fDialogMgr.ExecDBUpdateDlg then
  begin
    // Database was updated: check if user database needs saving
    if (Database as IDatabaseEdit).Updated
      and TMessageBox.Confirm(Self, sConfirmSave) then
      TUserDBMgr.Save(Self);
    // Reload the databases
    ReloadDatabase;
  end;
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
    (fMainDisplayMgr.CurrentView as ISnippetView).Snippet, 'DependenciesDlg'
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
  SelectedSnippet: TSnippet;  // currently selected snippet
begin
  Assert(Supports(fMainDisplayMgr.CurrentView, ISnippetView),
    ClassName + '.actViewTestUnitExecute: Snippet view expected');
  SelectedSnippet := (fMainDisplayMgr.CurrentView as ISnippetView).Snippet;
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
    and SnippetView.Snippet.CanCompile;
end;

procedure TMainForm.actWelcomeExecute(Sender: TObject);
begin
  fMainDisplayMgr.ShowWelcomePage;
end;

procedure TMainForm.AfterShowForm;
begin
  inherited;
  // initialise display
  fMainDisplayMgr.Initialise(fWindowSettings.OverviewTab);
  fMainDisplayMgr.ShowWelcomePage;
  // check for registerable Delphi compiler installations
  if fCompileMgr.CheckForNewCompilerInstalls then
    fMainDisplayMgr.Refresh;
end;

function TMainForm.appEventsHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  // Prevent Delphi Help system from interfering!
  // This prevents exception being raised when F1 is pressed over menu items
  // while still allowing our custom help manager to operate.
  CallHelp := False;
  Result := True;
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
var
  EditableDB: IDatabaseEdit;
begin
  inherited;
  // Save any changes to user database
  EditableDB := Database as IDatabaseEdit;
  if EditableDB.Updated then
    EditableDB.Save;

  // Unhook snippets event handler
  Database.RemoveChangeEventHandler(DBChangeHandler);
  // Save window state
  fWindowSettings.SplitterPos := pnlLeft.Width;
  fWindowSettings.OverviewTab := fMainDisplayMgr.SelectedOverviewTab;
  fWindowSettings.Save;

  // Free owned objects
  fHistory.Free;
  fMainDisplayMgr.Free;
  fFavouritesMgr.Free;
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
  ActionSetter: ISetActions;
  DetailCmdBarCfg, OverviewCmdBarCfg: ICommandBarConfig;
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
    actGitHubHome.URL := TURL.CodeSnipRepo;
    actFAQs.URL := TURL.CodeSnipFAQReadMe;
    actBlog.URL := TURL.CodeSnipBlog;
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
    {$IFDEF PORTABLE}
    // Move user database option not available in portable edition
    actMoveUserDatabase.Visible := False;
    {$ENDIF}

    // Create notifier object and assign actions triggered by its methods
    // note that actions created on fly are automatically freed
    fNotifier := TNotifier.Create;
    ActionSetter := fNotifier as ISetActions;
    ActionSetter.SetDisplaySnippetAction(
      TActionFactory.CreateSnippetAction(Self)
    );
    ActionSetter.SetDisplayCategoryAction(
      TActionFactory.CreateCategoryAction(Self)
    );
    ActionSetter.SetConfigCompilersAction(actCompilers);
    ActionSetter.SetShowViewItemAction(
      TActionFactory.CreateViewItemAction(Self, ActViewItemExecute)
    );
    ActionSetter.SetOverviewStyleChangeActions(
      [actViewCategorised, actViewAlphabetical, actViewSnippetKinds]
    );
    ActionSetter.SetDetailPaneChangeAction(
      TActionFactory.CreateDetailTabAction(Self, ActSelectDetailTabExecute)
    );
    ActionSetter.SetEditSnippetAction(
      TActionFactory.CreateEditSnippetAction(Self, ActEditSnippetByIDExecute)
    );
    ActionSetter.SetNewsAction(actBlog);
    ActionSetter.SetAboutBoxAction(actAbout);

    // Customise web browser controls in Details pane
    WBExternal := TWBExternal.Create;
    (frmDetail as IWBCustomiser).SetExternalObj(WBExternal);
    (frmDetail as IWBCustomiser).SetDragDropHandler(TNulDropTarget.Create);

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
    DetailCmdBarCfg := frmDetail as ICommandBarConfig;
      // set images to use
    DetailCmdBarCfg.SetImages(ilMain);
      // detail view menus
    DetailCmdBarCfg.AddAction(
      TActionFactory.CreateLinkAction(Self),
      [cDetailPopupMenuAnchor, cDetailPopupMenuImage]
    );
    DetailCmdBarCfg.AddSpacer([cDetailPopupMenuAnchor, cDetailPopupMenuImage]);
    DetailCmdBarCfg.AddAction(actViewDependencies, cDetailPopupMenuIDs);
    DetailCmdBarCfg.AddSpacer(cDetailPopupMenuIDs);
    DetailCmdBarCfg.AddAction(actCopyInfo, cDetailPopupMenuIDs);
    DetailCmdBarCfg.AddAction(actCopySnippet, cDetailPopupMenuIDs);
    DetailCmdBarCfg.AddAction(actCopySource, cDetailPopupMenuIDs);
    DetailCmdBarCfg.AddSpacer(cDetailPopupMenuIDs);
    DetailCmdBarCfg.AddAction(actTestCompile, cDetailPopupMenuIDs);
    DetailCmdBarCfg.AddSpacer(cDetailPopupMenuIDs);
    DetailCmdBarCfg.AddAction(actSaveSnippet, cDetailPopupMenuIDs);
    DetailCmdBarCfg.AddAction(actPrint, cDetailPopupMenuIDs);
    DetailCmdBarCfg.AddSpacer(cDetailPopupMenuIDs);
    DetailCmdBarCfg.AddAction(actCopy, cDetailPopupMenuTextSelect);
    DetailCmdBarCfg.AddAction(actSelectAll, cDetailPopupMenuIDs);
    DetailCmdBarCfg.AddSpacer(cDetailPopupMenuIDs);
    DetailCmdBarCfg.AddAction(actCloseDetailsTab, cDetailPopupMenuIDs);
    // tab set menu
    DetailCmdBarCfg.AddAction(actCloseDetailsTab, cDetailTabSetPopupMenu);
    DetailCmdBarCfg.AddAction(
      actCloseUnselectedDetailsTabs, cDetailTabSetPopupMenu
    );

    // Set up overview pane's toolbar and popup menu
    OverviewCmdBarCfg := frmOverview as ICommandBarConfig;
    OverviewCmdBarCfg.SetImages(ilMain);
    // add toolbar actions (in reverse order we want them!)
    OverviewCmdBarCfg.AddAction(actCollapseTree, cOverviewToolBar);
    OverviewCmdBarCfg.AddAction(actExpandTree, cOverviewToolBar);
    // add popup menu actions
    OverviewCmdBarCfg.AddAction(actViewDependencies, cOverviewPopupMenu);
    OverviewCmdBarCfg.AddSpacer(cOverviewPopupMenu);
    OverviewCmdBarCfg.AddAction(actCopyInfo, cOverviewPopupMenu);
    OverviewCmdBarCfg.AddAction(actCopySnippet, cOverviewPopupMenu);
    OverviewCmdBarCfg.AddAction(actCopySource, cOverviewPopupMenu);
    OverviewCmdBarCfg.AddSpacer(cOverviewPopupMenu);
    OverviewCmdBarCfg.AddAction(actSaveSnippet, cOverviewPopupMenu);
    OverviewCmdBarCfg.AddAction(actPrint, cOverviewPopupMenu);
    OverviewCmdBarCfg.AddSpacer(cOverviewPopupMenu);
    OverviewCmdBarCfg.AddAction(actEditSnippet, cOverviewPopupMenu);
    OverviewCmdBarCfg.AddSpacer(cOverviewPopupMenu);
    OverviewCmdBarCfg.AddAction(actCollapseNode, cOverviewPopupMenu);
    OverviewCmdBarCfg.AddAction(actExpandNode, cOverviewPopupMenu);

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

    // Create favourites manager
    // *** Must be done AFTER database has loaded ***
    fFavouritesMgr := TFavouritesManager.Create(fNotifier);

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

