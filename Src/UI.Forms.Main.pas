unit UI.Forms.Main;

interface

{ TODO: Fixup uses clauses - move to impl & remove unused. }

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  Vcl.ActnCtrls, Vcl.ToolWin, Vcl.ActnMan, Vcl.ActnMenus,
  Vcl.PlatformDefaultStyleActnCtrls, System.Actions, Vcl.ActnList, Vcl.StdActns,
  Vcl.BaseImageCollection, Vcl.ImageCollection, Vcl.ExtActns, Vcl.ExtCtrls,
  System.ImageList, Vcl.ImgList, Vcl.VirtualImageList, Vcl.Menus, Vcl.AppEvnts,
  // Project
  Favourites.UManager,
  UI.Forms.Root,
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
  TMainForm = class(TRootForm)
    ApplicationEvents: TApplicationEvents;
    MainMenuBar: TActionMainMenuBar;
    StatusBar: TStatusBar;
    ActionList: TActionList;
    MainImageList: TImageList;
    actSaveSnippet: TAction;
    actAddSnippet: TAction;
    actDeleteSnippet: TAction;
    actSaveUnit: TAction;
    actWelcome: TAction;
    actEditSnippet: TAction;
    actExit: TFileExit;
    actAbout: TAction;
    actCopy: TAction;
    actBugReport: TAction;
    actFindCompiler: TAction;
    actFindText: TAction;
    actGoBack: TAction;
    actGoForward: TAction;
    actCopySnippet: TAction;
    actSelectAll: TAction;
    actViewCategorised: TAction;
    actViewAlphabetical: TAction;
    actViewSnippetKinds: TAction;
    actHelpQuickStart: TAction;
    actCompilers: TAction;
    actHelpCompChecks: TAction;
    actHelpContents: TAction;
    actPreferences: TAction;
    actLicense: TAction;
    actGitHubHome: TBrowseURL;
    actFindXRefs: TAction;
    actSelectSnippets: TAction;
    actFindClear: TAction;
    actTestCompile: TAction;
    actViewCompErrs: TAction;
    actViewTestUnit: TAction;
    actTestBug: TAction;
    actNextTab: TAction;
    actPreviousTab: TAction;
    actPrint: TAction;
    actBackupDatabase: TAction;
    actRestoreDatabase: TAction;
    actSaveDatabase: TAction;
    actUpdateDbase: TAction;
    actExportCode: TAction;
    actImportCode: TAction;
    actCopyInfo: TAction;
    actViewDependencies: TAction;
    actCollapseTree: TAction;
    actExpandTree: TAction;
    actExpandNode: TAction;
    actCollapseNode: TAction;
    actCopySource: TAction;
    actAddCategory: TAction;
    actRenameCategory: TAction;
    actDeleteCategory: TAction;
    actNewDetailsTab: TAction;
    actCloseDetailsTab: TAction;
    actFAQs: TBrowseURL;
    actDuplicateSnippet: TAction;
    actSaveSelection: TAction;
    actLoadSelection: TAction;
    actCloseUnselectedDetailsTabs: TAction;
    actCloseAllDetailsTabs: TAction;
    actFavourites: TAction;
    actAddFavourite: TAction;
    actMoveUserDatabase: TAction;
    actSWAGImport: TAction;
    actBlog: TBrowseURL;
    actDeleteUserDatabase: TAction;
    ActionManager: TActionManager;
    tbarMain: TToolBar;
    tbSaveSnippet: TToolButton;
    tbSaveUnit: TToolButton;
    tbSpacer7: TToolButton;
    tbPrint: TToolButton;
    tbSpacer1: TToolButton;
    tbFindText: TToolButton;
    tbFindCompiler: TToolButton;
    tbSelectSnippets: TToolButton;
    tbFindClear: TToolButton;
    tbSpacer3: TToolButton;
    tbGoBack: TToolButton;
    tbGoForward: TToolButton;
    tbSpacer8: TToolButton;
    tbAddSnippet: TToolButton;
    tbEditSnippet: TToolButton;
    tbDeleteSnippet: TToolButton;
    tbSaveDatabase: TToolButton;
    tbFavourites: TToolButton;
    tbSpacer5: TToolButton;
    tbTestCompile: TToolButton;
    tbSpacer6: TToolButton;
    tbHelpContents: TToolButton;
    pnlBody: TPanel;
    splitVert: TSplitter;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    frmOverview: TOverviewFrame;
    frmDetail: TDetailFrame;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actLicenseExecute(Sender: TObject);
    function ApplicationEventsHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
    procedure actHelpContentsExecute(Sender: TObject);
    procedure actHelpQuickStartExecute(Sender: TObject);
    procedure actHelpCompChecksExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
  strict private
      ///  <summary>Object that notifies user-initiated events by triggering
      ///  actions.</summary>
      fNotifier: INotifier;
      ///  <summary>Object that maintains a history of viewed item.</summary>
      fHistory: THistory;
      ///  <summary>Component that saves and restores main window settings.
      ///  </summary>
      ///  <remarks>There is no need to explicitly free this component: it is
      ///  freed automatically when main form destroys its components.</remarks>
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
    ///  <summary>Opens a named user defined snippet in Snippets Editor for
    ///  editing. The snippet name is provided by the TEditSnippetAction
    ///  instance referenced by Sender.</summary>
    procedure ActEditSnippetByNameExecute(Sender: TObject);
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
    procedure InitialiseControls; override;
    ///  <summary>Performs actions that need to run after the form is visible on
    ///  screen.</summary>
    procedure AfterShowForm; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  // Delphi
  // SEE INTF: WinApi.Windows,
  // SEE INTF: Vcl.Graphics,
  // Project
  DB.UCategory, DB.UMain, DB.USnippet, FmSplash, FmTrappedBugReportDlg,
  FmWaitDlg, IntfFrameMgrs, UActionFactory, UAppInfo,
  UClassHelpers, UCodeShareMgr, UCommandBars, UConsts, UCopyInfoMgr,
  UCopySourceMgr, UDatabaseLoader, UDatabaseLoaderUI, UDetailTabAction,
  UEditSnippetAction, UExceptions, UHelpMgr, UHistoryMenus, UKeysHelper,
  UMessageBox, UNotifier, UNulDropTarget, UPrintMgr, UQuery, USaveSnippetMgr,
  USaveUnitMgr, USelectionIOMgr, UUrl, UUserDBMgr, UView, UViewItemAction,
  UWBExternal;


{ TMainForm }

procedure TMainForm.actAboutExecute(Sender: TObject);
begin
  fDialogMgr.ShowAboutDlg;
end;

procedure TMainForm.ActEditSnippetByNameExecute(Sender: TObject);
begin
  TUserDBMgr.EditSnippet((Sender as TEditSnippetAction).SnippetName);
end;

procedure TMainForm.actHelpCompChecksExecute(Sender: TObject);
begin
  DisplayHelp('CompChecks');
end;

procedure TMainForm.actHelpContentsExecute(Sender: TObject);
begin
  HelpMgr.ShowContents;
end;

procedure TMainForm.actHelpQuickStartExecute(Sender: TObject);
begin
  DisplayHelp('QuickStart');
end;

procedure TMainForm.actLicenseExecute(Sender: TObject);
begin
  DisplayHelp('License');
end;

procedure TMainForm.ActSelectDetailTabExecute(Sender: TObject);
begin
  fMainDisplayMgr.SelectDetailTab((Sender as TDetailTabAction).TabIndex);
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

function TMainForm.ApplicationEventsHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
begin
  // Prevent Delphi Help system from interfering!
  // This prevents exception being raised when F1 is pressed over menu items
  // while still allowing our custom help manager to operate.
  CallHelp := False;
  Result := True;
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

procedure TMainForm.DoSearchFilter(const Search: ISearch;
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
    ApplicationEvents.OnException := HandleExceptions;
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
  // Save any changes to user database
  with Database as IDatabaseEdit do
  begin
    if Updated then
      Save;
  end;
  // Unhook snippets event handler
  Database.RemoveChangeEventHandler(DBChangeHandler);
  // Save window state
  fWindowSettings.SplitterPos := pnlLeft.Width;
  fWindowSettings.OverviewTab := fMainDisplayMgr.SelectedOverviewTab;
  fWindowSettings.Save;

  fMainDisplayMgr.CloseDetailsTabs(dtcAll);

  // Free owned objects
  fHistory.Free;
  fMainDisplayMgr.Free;
  fFavouritesMgr.Free;
  // fStatusBarMgr MUST be nilled: otherwise it can be called after status bar
  // control has been freed and so cause AV when trying to use the control
  FreeAndNil(fStatusBarMgr);

  frmDetail.Free;

end;

procedure TMainForm.HandleExceptions(Sender: TObject; E: Exception);
begin
  if (E is ECodeSnip) or (E is EFileStreamError) then
    TMessageBox.Error(nil, E.Message)
  else
    TTrappedBugReportDlg.Execute(nil, E);
end;

procedure TMainForm.InitialiseControls;
var
  WBExternal: IDispatch;        // external object of browser control
begin
  try
    inherited;
    // Set up action image list
    MainImageList.LoadFromResource(RT_RCDATA, 'ACTIONIMAGES', 16, clFuchsia);
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
    with fNotifier as ISetActions do
    begin
      SetUpdateDbaseAction(actUpdateDbase);
      SetDisplaySnippetAction(TActionFactory.CreateSnippetAction(Self));
      SetDisplayCategoryAction(TActionFactory.CreateCategoryAction(Self));
      SetConfigCompilersAction(actCompilers);
      SetShowViewItemAction(
        TActionFactory.CreateViewItemAction(Self, ActViewItemExecute)
      );
      SetOverviewStyleChangeActions(
        [actViewCategorised, actViewAlphabetical, actViewSnippetKinds]
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
      SetNewsAction(actBlog);
      SetAboutBoxAction(actAbout);
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
    fStatusBarMgr := TStatusBarMgr.Create(StatusBar);

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
      SetImages(MainImageList);
      // detail view menus
      AddAction(
        TActionFactory.CreateLinkAction(Self),
        [cDetailPopupMenuAnchor, cDetailPopupMenuImage]
      );
      AddSpacer([cDetailPopupMenuAnchor, cDetailPopupMenuImage]);
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
      SetImages(MainImageList);
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

end.

