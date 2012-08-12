{
 * FmMain.pas
 *
 * Application's main form. Handles the program's user initiated actions,
 * output, control layout and navigation history.
 *
 * $Rev$
 * $Date$
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
 * Portions created by the Initial Developer are Copyright (C) 2005-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmMain;


interface


uses
  // Delphi
  SysUtils, Menus, ExtActns, StdActns, Classes, ActnList, ImgList, Controls,
  Forms, ExtCtrls, ComCtrls, ToolWin, Messages, AppEvnts,
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
    actAddCategory: TAction;
    actAddSnippet: TAction;
    actBackupDatabase: TAction;
    actBugReport: TAction;
    actCloseDetailsTab: TAction;
    actCollapseNode: TAction;
    actCollapseTree: TAction;
    actCompilers: TAction;
    actCopy: TAction;
    actCopyInfo: TAction;
    actCopySnippet: TAction;
    actCopySource: TAction;
    actDeleteCategory: TAction;
    actDeleteSnippet: TAction;
    actDonate: TAction;
    actEditSnippet: TAction;
    actExit: TFileExit;
    actExpandNode: TAction;
    actExpandTree: TAction;
    actExportCode: TAction;
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
    actHomePage: TBrowseURL;
    actImportCode: TAction;
    actLicense: TAction;
    actNextTab: TAction;
    actNewDetailsTab: TAction;
    actNews: TAction;
    actPageSetup: TAction;
    actPreferences: TAction;
    actPreviousTab: TAction;
    actPrint: TAction;
    actPrivacy: TAction;
    actProxyServer: TAction;
    actRegister: TAction;
    actRenameCategory: TAction;
    actRestoreDatabase: TAction;
    actSaveDatabase: TAction;
    actSaveSnippet: TAction;
    actSaveUnit: TAction;
    actSelectAll: TAction;
    actSelectSnippets: TAction;
    actSubmit: TAction;
    actTestBug: TAction;
    actTestCompile: TAction;
    actUpdateDbase: TAction;
    actViewAlphabetical: TAction;
    actViewCategorised: TAction;
    actViewCompErrs: TAction;
    actViewDependencies: TAction;
    actSelectDetailTab: TAction;
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
    miAddCategory: TMenuItem;
    miAddSnippet: TMenuItem;
    miBackupDatabase: TMenuItem;
    miCategories: TMenuItem;
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
    miDonate: TMenuItem;
    miEdit: TMenuItem;
    miEditSnippet: TMenuItem;
    miExit: TMenuItem;
    miExpandNode: TMenuItem;
    miExpandTree: TMenuItem;
    miExportCode: TMenuItem;
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
    miNewDetailsTab: TMenuItem;
    miNews: TMenuItem;
    miPageSetup: TMenuItem;
    miPreferences: TMenuItem;
    miPrint: TMenuItem;
    miPrivacy: TMenuItem;
    miProxyServer: TMenuItem;
    miRegister: TMenuItem;
    miRenameCategory: TMenuItem;
    miReportBug: TMenuItem;
    miRestoreDatabase: TMenuItem;
    miSaveSnippet: TMenuItem;
    miSaveDatabase: TMenuItem;
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
    miViewCompErrs: TMenuItem;
    miViewDependencies: TMenuItem;
    miViewSnippetKinds: TMenuItem;
    miViewTestUnit: TMenuItem;
    miViewAlphabetical: TMenuItem;
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
    tbSelectSnippets: TToolButton;
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
    actDuplicateSnippet: TAction;
    miDuplicateSnippet: TMenuItem;
    actSaveSelection: TAction;
    actLoadSelection: TAction;
    miSpacer17: TMenuItem;
    miLoadSelection: TMenuItem;
    miSaveSelection: TMenuItem;
    actProgramUpdates: TAction;
    miSpacer18: TMenuItem;
    miCheckUpdates: TMenuItem;
    actCloseUnselectedDetailsTabs: TAction;
    actCloseAllDetailsTabs: TAction;
    miCloseAllDetailsTabs: TMenuItem;
    procedure actAboutExecute(Sender: TObject);
    procedure actAddCategoryExecute(Sender: TObject);
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
    procedure actDeleteCategoryExecute(Sender: TObject);
    procedure actDeleteCategoryUpdate(Sender: TObject);
    procedure actDeleteSnippetExecute(Sender: TObject);
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
    procedure actNewsExecute(Sender: TObject);
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
    procedure actProxyServerExecute(Sender: TObject);
    procedure actRegisterExecute(Sender: TObject);
    procedure actRegisterUpdate(Sender: TObject);
    procedure actRenameCategoryExecute(Sender: TObject);
    procedure actRenameCategoryUpdate(Sender: TObject);
    procedure actRestoreDatabaseExecute(Sender: TObject);
    procedure actSaveDatabaseExecute(Sender: TObject);
    procedure actSaveDatabaseUpdate(Sender: TObject);
    procedure actSaveSnippetExecute(Sender: TObject);
    procedure actSaveSnippetUpdate(Sender: TObject);
    procedure actSaveUnitExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actSelectAllUpdate(Sender: TObject);
    procedure actSelectSnippetsExecute(Sender: TObject);
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
    procedure actNewDetailsTabExecute(Sender: TObject);
    procedure actCloseDetailsTabExecute(Sender: TObject);
    procedure actCloseDetailsTabsUpdate(Sender: TObject);
    procedure actSelectDetailTabExecute(Sender: TObject);
    procedure actDuplicateSnippetExecute(Sender: TObject);
    procedure actDuplicateSnippetUpdate(Sender: TObject);
    procedure actSaveSelectionExecute(Sender: TObject);
    procedure actSaveSelectionUpdate(Sender: TObject);
    procedure actLoadSelectionExecute(Sender: TObject);
    procedure actProgramUpdatesExecute(Sender: TObject);
    procedure actCloseUnselectedDetailsTabsExecute(Sender: TObject);
    procedure actCloseAllDetailsTabsExecute(Sender: TObject);
  strict private
    fIsAppRegistered: Boolean;        // Flag noting if app is registered
    fNotifier: INotifier;             // Notififies app of user-initiated events
    fHistory: THistory;               // Maintains a history of the items viewed
    fWindowSettings: TWindowSettings; // Saves and restores main window settings
    fMainDisplayMgr: TMainDisplayMgr; // Manages the main display output
    fStatusBarMgr: TStatusBarMgr;     // Manages status bar display
    fDialogMgr: TDialogMgr;           // Manages display of dialog boxes
    fCompileMgr: TMainCompileMgr;     // Manages test compilations
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
    procedure ActEditSnippetByNameExecute(Sender: TObject);
      {Edits a named user defined snippet.
        @param Sender [in] Action triggering this event. Must be a
          TEditSnippetAction.
      }
    procedure ActBrowserHintExecute(Sender: TObject);
      {Displays hint from browser hint action in status bar.
        @param Sender [in] Not used.
      }
    procedure DBChangeHandler(Sender: TObject; const EvtInfo: IInterface);
      {Handles events that inform of changes to database.
        @param Sender [in] Not used.
        @para EvtInfo [in] Object providing information about the event.
      }
    procedure DisplayHint(const Hint: string);
      {Displays hint in status bar using status bar manager.
        @param Hint [in] Hint to be displayed.
      }
    procedure HandleExceptions(Sender: TObject; E: Exception);
      {Handles untrapped application-level exceptions.
        @param Sender [in] Not used.
        @param E [in] Exception to be handled.
      }
    procedure LoadSnippets(const Loader: TProc);
      {Loads Snippets object from database and re-intitialises display.
        @param Loader [in] Closure that performs actual loading of database.
      }
    procedure ReloadDatabase;
      {Reloads the whole database in a thread.
      }
    procedure DoSearchFilter(const Search: ISearch;
      const Refine: Boolean = False);
      {Filters main display using search object and displays message if no
      snippets found.
        @param Search [in] Search object to filter by.
        @param Refine [in] Flag indicating whether any existing search should
          be refined (True) or search should be of whole databas (False).
      }
  strict protected
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
  Windows,
  // Project
  DB.UCategory, DB.UMain, DB.USnippet, FmSplash, FmTrappedBugReportDlg,
  FmWaitDlg, IntfFrameMgrs, UActionFactory, UAppInfo, UCodeShareMgr,
  UCommandBars, UConsts, UCopyInfoMgr, UCopySourceMgr, UDatabaseLoader,
  UDatabaseLoaderUI, UEditSnippetAction, UExceptions, UHelpMgr, UHistoryMenus,
  UMessageBox, UNotifier, UNulDropTarget, UPrintMgr, UQuery, USaveSnippetMgr,
  USaveUnitMgr, USelectionIOMgr, UUserDBMgr, UView, UViewItemAction,
  UWBExternal, Web.UInfo;


{$R *.dfm}


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

procedure TMainForm.actAddCategoryExecute(Sender: TObject);
  {Gets a new user defined category from user and adds to database.
    @param Sender [in] Not used.
  }
begin
  TUserDBMgr.AddCategory;
end;

procedure TMainForm.actAddSnippetExecute(Sender: TObject);
  {Gets a new user defined snippet from user and adds to database.
    @param Sender [in] Not used.
  }
begin
  TUserDBMgr.AddSnippet;
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
  {Copies annotated source of selected snippet or category to clipboard.
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

procedure TMainForm.actDeleteCategoryExecute(Sender: TObject);
  {Deletes a user defined category.
    @param Sender [in] Not used.
  }
begin
  TUserDBMgr.DeleteACategory;
end;

procedure TMainForm.actDeleteCategoryUpdate(Sender: TObject);
  {Enables or disables delete category action depending on whether categories
  are available for deleting.
    @param Sender [in] Action triggering the event.
  }
begin
  (Sender as TAction).Enabled := TUserDBMgr.CanDeleteACategory;
end;

procedure TMainForm.actDeleteSnippetExecute(Sender: TObject);
  {Deletes currently selected user defined snippet.
    @param Sender [in] Not used.
  }
begin
  Assert(TUserDBMgr.CanEdit(fMainDisplayMgr.CurrentView),
    ClassName + '.actDeleteSnippetExecute: Can''t delete current view item');
  TUserDBMgr.DeleteSnippet(fMainDisplayMgr.CurrentView);
  // display update is handled by snippets change event handler
end;

procedure TMainForm.actDonateExecute(Sender: TObject);
  {Displays the Donate dialog box.
    @param Sender [in] Not used.
  }
begin
  fDialogMgr.ShowDonateDlg
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
  {Determines whether the Delete Snippet action is enabled: a user defined
  snippet must be selected.
    @param Sender [in] Action triggering this event.
  }
begin
  (Sender as TAction).Enabled :=
    TUserDBMgr.CanEdit(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.ActEditSnippetByNameExecute(Sender: TObject);
  {Edits a named user defined snippet.
    @param Sender [in] Action triggering this event. Must be a
      TEditSnippetAction.
  }
begin
  TUserDBMgr.EditSnippet((Sender as TEditSnippetAction).SnippetName);
end;

procedure TMainForm.actEditSnippetExecute(Sender: TObject);
  {Displays selected user defined snippet in edit dialog box to enable user to
  make changes.
    @param Sender [in] Not used.
  }
begin
  Assert(TUserDBMgr.CanEdit(fMainDisplayMgr.CurrentView),
    ClassName + '.actEditSnippetExecute: Can''t edit current view item');
  fNotifier.EditSnippet(
    (fMainDisplayMgr.CurrentView as ISnippetView).Snippet.Name
  );
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
  fMainDisplayMgr.UpdateDisplayedQuery;
  fStatusBarMgr.Update;
end;

procedure TMainForm.actFindClearUpdate(Sender: TObject);
  {Enables / disables Show All action according to if there's a current search.
    @param Sender [in] Action triggering the event.
  }
begin
  // We have an active search if current search's criteria is not nul
  (Sender as TAction).Enabled := Query.IsSearchActive;
end;

procedure TMainForm.actFindCompilerExecute(Sender: TObject);
  {Performs compiler search. Gets search from user via Find Compiler dialog box
  then displays all snippets that match the search criteria.
    @param Sender [in] Not used.
  }
var
  Search: USearch.ISearch;  // compiler search object
  Refine: Boolean;          // flag indicating if existing search to be refined
begin
  // Display Find Compiler dialog box to enable user to enter search criteria
  // (dialog box creates and returns search object from entered criteria)
  if fDialogMgr.ExecFindCompilerDlg(Search, Refine) then
    DoSearchFilter(Search, Refine);
end;

procedure TMainForm.actFindTextExecute(Sender: TObject);
  {Performs text search. Gets search from user via Find Text dialog box then
  displays all snippets that match the search criteria.
    @param Sender [in] Not used.
  }
var
  Search: USearch.ISearch;  // text search object
  Refine: Boolean;          // flag indicating if existing search to be refined
begin
  // Display Find Text dialog box to enable user to enter search criteria
  // (dialog box creates and returns search object from entered criteria)
  if fDialogMgr.ExecFindTextDlg(Search, Refine) then
    DoSearchFilter(Search, Refine);
end;

procedure TMainForm.actFindXRefsExecute(Sender: TObject);
  {Performs cross reference search. Gets search from user via Find Cross Refs
  dialog box then displays all snippets that match the search criteria.
    @param Sender [in] Not used.
  }
var
  Search: USearch.ISearch;  // cross reference search object
begin
  Assert(Supports(fMainDisplayMgr.CurrentView, ISnippetView),
    ClassName + '.actFindXRefsExecute: Current view is not a snippet');
  // Display Find Cross Refs dialog box to enable user to enter search criteria
  // (dialog box creates and returns search object from entered criteria)
  if fDialogMgr.ExecFindXRefsDlg(
    (fMainDisplayMgr.CurrentView as ISnippetView).Snippet, Search
  ) then
    DoSearchFilter(Search);
end;

procedure TMainForm.actFindXRefsUpdate(Sender: TObject);
  {Enables / disables Find Cross Refs action according to whether a snippet is
  currently selected.
    @param Sender [in] Action triggering the event.
  }
begin
  (Sender as TAction).Enabled :=
    Supports(fMainDisplayMgr.CurrentView, ISnippetView);
end;

procedure TMainForm.actGoBackExecute(Sender: TObject);
  {Displays previous item in history list.
    @param Sender [in] Not used.
  }
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
  // TODO: decide if to have user option to decide how history items are shown
  fMainDisplayMgr.DisplayViewItem(ViewItem, ddmOverwrite);
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
  // TODO: decide if to have user option to decide how history items are shown
  fMainDisplayMgr.DisplayViewItem(ViewItem, ddmOverwrite);
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
  {Exports one or more user-defined snippets from a file.
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

procedure TMainForm.actLoadSelectionExecute(Sender: TObject);
var
  Search: ISearch;
begin
  if TSelectionIOMgr.LoadSelectionSearch(Search) then
    DoSearchFilter(Search);
end;

procedure TMainForm.actNewDetailsTabExecute(Sender: TObject);
begin
  fMainDisplayMgr.CreateNewDetailsTab;
end;

procedure TMainForm.actNewsExecute(Sender: TObject);
  {Displays latest news about CodeSnip and database.
    @param Sender [in] Not used.
  }
begin
  fDialogMgr.ShowNewsDlg;
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
  {Enables / disables an action according to whether there are snippets in
  database.
    @param Sender [in] Action triggering this event.
  }
begin
  (Sender as TAction).Enabled := not Database.Snippets.IsEmpty;
end;

procedure TMainForm.ActOverviewTabExecute(Sender: TObject);
  {Selects a tab in the overview pane.
    @param Sender [in] Action triggering this event
  }
begin
  // Action's Tag property specifies index of tab being selected
  fMainDisplayMgr.SelectOverviewTab((Sender as TAction).Tag);
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
var
  UpdateUI: Boolean;  // flag true if preference changes affect main window UI
begin
  fDialogMgr.ExecPreferencesDlg(UpdateUI);
  if UpdateUI then
    fMainDisplayMgr.CompleteRefresh;
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

procedure TMainForm.actProgramUpdatesExecute(Sender: TObject);
  {Displays dialogue box that checks for program updates.
    @param Sender [in] Not used.
  }
begin
  fDialogMgr.ShowProgramUpdatesDlg;
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

procedure TMainForm.actRenameCategoryExecute(Sender: TObject);
  {Renames a user defined category.
    @param Sender [in] Not used.
  }
begin
  TUserDBMgr.RenameACategory;
end;

procedure TMainForm.actRenameCategoryUpdate(Sender: TObject);
  {Enables or disables category rename action depending on whether categories
  are available for renaming.
    @param Sender [in] Action triggering the event.
  }
begin
  (Sender as TAction).Enabled := TUserDBMgr.CanRenameACategory;
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

procedure TMainForm.actSaveSelectionExecute(Sender: TObject);
begin
  TSelectionIOMgr.SaveCurrentSelection;
end;

procedure TMainForm.actSaveSelectionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := TSelectionIOMgr.CanSaveCurrentSelection;
end;

procedure TMainForm.actSaveSnippetExecute(Sender: TObject);
  {Saves selected snippet to disk.
    @param Sender [in] Not used.
  }
begin
  TSaveSnippetMgr.Execute(fMainDisplayMgr.CurrentView);
end;

procedure TMainForm.actSaveSnippetUpdate(Sender: TObject);
  {Enables / disables Save Snippet action according to whether a summary is
  selected.
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

procedure TMainForm.actSelectDetailTabExecute(Sender: TObject);
  {Selects a tab in the detail pane.
    @param Sender [in] Action triggering this event
  }
begin
  // Action's Tag property specifies index of tab being selected
  fMainDisplayMgr.SelectDetailTab((Sender as TAction).Tag);
end;

procedure TMainForm.actSelectSnippetsExecute(Sender: TObject);
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
  fDialogMgr.ShowTestCompileDlg(
    fCompileMgr,
    (fMainDisplayMgr.CurrentView as ISnippetView).Snippet
  );
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
    if (Database as IDatabaseEdit).Updated
      and TMessageBox.Confirm(Self, sConfirmSave) then
      (Database as IDatabaseEdit).Save;
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

procedure TMainForm.actViewDependenciesExecute(Sender: TObject);
  {Displays dependency tree for selected snippet.
    @param Sender [in] Not used.
  }
begin
  Assert(Supports(fMainDisplayMgr.CurrentView, ISnippetView),
    ClassName + '.actViewDependenciesExecute: Snippet view expected');
  fDialogMgr.ShowDependencyTree(
    (fMainDisplayMgr.CurrentView as ISnippetView).Snippet
  );
end;

procedure TMainForm.actViewDependenciesUpdate(Sender: TObject);
  {Enables / disables view dependencies action depending on whether current
  selection is a code snippet.
    @param Sender [in] Action triggering this event.
  }
begin
  (Sender as TAction).Enabled :=
    Supports(fMainDisplayMgr.CurrentView, ISnippetView);
end;

procedure TMainForm.ActViewHistoryItemExecute(Sender: TObject);
  {Displays requested history item and selects it in the history list.
    @param Sender [in] Action triggering this event. Must be a TViewItemAction.
  }
begin
  // TODO: decide if to have user option to decide how history items are shown
  fMainDisplayMgr.DisplayViewItem(
    (Sender as TViewItemAction).ViewItem, ddmOverwrite
  );
  fHistory.SelectItem((Sender as TViewItemAction).ViewItem);
end;

procedure TMainForm.ActViewItemExecute(Sender: TObject);
  {Displays a requested view item and records in history.
    @param Sender [in] Action triggering this event. Must be a TViewItemAction.
  }
const
  TabDisplayMap: array[Boolean] of TDetailPageDisplayMode = (
    ddmOverwrite, ddmRequestNewTab
  );
begin
  fMainDisplayMgr.DisplayViewItem(
    (Sender as TViewItemAction).ViewItem,
    TabDisplayMap[(Sender as TViewItemAction).NewTab]
  );
  fHistory.NewItem((Sender as TViewItemAction).ViewItem);
end;

procedure TMainForm.actViewTestUnitExecute(Sender: TObject);
  {Displays test unit for currently selected snippet in a dialog box.
    @param Sender [in] Not used.
  }
var
  SelectedSnippet: TSnippet;  // currently selected snippet
begin
  Assert(Supports(fMainDisplayMgr.CurrentView, ISnippetView),
    ClassName + '.actViewTestUnitExecute: Snippet view expected');
  SelectedSnippet := (fMainDisplayMgr.CurrentView as ISnippetView).Snippet;
  Assert(SelectedSnippet.CanCompile,
    ClassName + '.actViewTestUnitExecute: Snippet is not compilable');
  fDialogMgr.ShowTestUnit(SelectedSnippet);
end;

procedure TMainForm.actViewTestUnitUpdate(Sender: TObject);
  {Enables /disables action depending on whether current view is a compilable
  snippet.
    @param Sender [in] Action triggering this event.
  }
var
  SnippetView: ISnippetView;  // current view as snippet view if supported
begin
  (Sender as TAction).Enabled :=
    Supports(fMainDisplayMgr.CurrentView, ISnippetView, SnippetView)
    and SnippetView.Snippet.CanCompile;
end;

procedure TMainForm.actWelcomeExecute(Sender: TObject);
  {Displays welcome page.
    @param Sender [in] Not used.
  }
begin
  fMainDisplayMgr.ShowWelcomePage;
end;

procedure TMainForm.appEventsHint(Sender: TObject);
  {Handles hint events triggered when a control issues a hint. The hint is
  displayed in the status bar.
    @param Sender [in] Not used.
  }
begin
  DisplayHint(Application.Hint);
end;

procedure TMainForm.DBChangeHandler(Sender: TObject; const EvtInfo: IInterface);
  {Handles events that inform of changes to database.
    @param Sender [in] Not used.
    @para EvtInfo [in] Object providing information about the event.
  }
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

procedure TMainForm.DisplayHint(const Hint: string);
  {Displays hint in status bar using status bar manager.
    @param Hint [in] Hint to be displayed.
  }
begin
  if Assigned(fStatusBarMgr) then
    fStatusBarMgr.ShowHint(Hint);
end;

procedure TMainForm.DoSearchFilter(const Search: USearch.ISearch;
  const Refine: Boolean);
  {Filters main display using search object, displays message if no snippets
  found and updates status bar as required.
    @param Search [in] Search object to filter by.
    @param Refine [in] Flag indicating whether any existing search should
      be refined (True) or search should be of whole databas (False).
  }
var
  PrevSearchExists: Boolean;  // flags if a search was active before this search

  ///  Returns a suitable error message depending on if a search was active
  ///  before this search was run and what action was taken on failure.
  function FailureMessage: string;
  resourcestring
    // dialog box message
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
  {Removes application object's hidden window from task bar, disables main form
  and sets up application event handler.
    @param Sender [in] Not used.
  }
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
  {Tidy up application. Writes persistent data and frees objects.
    @param Sender [in] Not used.
  }
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
  // Free owned objects
  fHistory.Free;
  fMainDisplayMgr.Free;
  // fStatusBarMgr MUST be nilled: otherwise it can be called after status bar
  // control has been freed and so cause AV when trying to use the control
  FreeAndNil(fStatusBarMgr);
end;

procedure TMainForm.FormResize(Sender: TObject);
  {Ensure splitter bar is in a sensible position when form size changes.
    @param Sender [in] Not used.
  }
begin
  inherited;
  if pnlLeft.Width > ClientWidth - TWindowSettings.MinRightPanelWidth
    - splitVert.Width then
    pnlLeft.Width := ClientWidth - TWindowSettings.MinRightPanelWidth
      - splitVert.Width;
end;

procedure TMainForm.HandleExceptions(Sender: TObject; E: Exception);
  {Handles untrapped application-level exceptions.
    @param Sender [in] Not used.
    @param E [in] Exception to be handled.
  }
begin
  if (E is ECodeSnip) or (E is EFileStreamError) then
    TMessageBox.Error(nil, E.Message)
  else
    TTrappedBugReportDlg.Execute(nil, E);
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
    Application.Title := TAppInfo.ProgramCaption;
    Caption := TAppInfo.ProgramCaption;
    if TWebInfo.UsingLocalHost then
      Caption := Caption + ' [' + TWebInfo.LocalHost + ']';

    // Restore window settings
    fWindowSettings := TWindowSettings.CreateStandAlone(Self);     // auto-freed
    fWindowSettings.Restore;                                // sizes main window
    pnlLeft.Width := fWindowSettings.SplitterPos;

    // Initialise actions
    // Browse actions have to have URLs set dynamically
    actHomePage.URL := TWebInfo.ProgramHomeURL;
    actWebSite.URL := TWebInfo.DelphiDabblerHomeURL;
    actFAQs.URL := TWebInfo.FAQsURL;
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
    // Detail pane tab actions have index placed in tag dynamically. We use 0 as
    // default
    actSelectDetailTab.Tag := 0;

    // Create notifier object and assign actions triggered by its methods
    // note that actions created on fly are automatically freed
    fNotifier := TNotifier.Create;
    with fNotifier as ISetActions do
    begin
      SetUpdateDbaseAction(actUpdateDbase);
      SetDisplaySnippetAction(TActionFactory.CreateSnippetAction(Self));
      SetDisplayCategoryAction(TActionFactory.CreateCategoryAction(Self));
      SetShowHintAction(
        TActionFactory.CreateHintAction(Self, ActBrowserHintExecute)
      );
      SetConfigCompilersAction(actCompilers);
      SetShowViewItemAction(
        TActionFactory.CreateViewItemAction(Self, ActViewItemExecute)
      );
      SetOverviewStyleChangeActions(
        [actViewCategorised, actViewAlphabetical, actViewSnippetKinds]
      );
      SetDetailPaneChangeAction(actSelectDetailTab);
      SetEditSnippetAction(
        TActionFactory.CreateEditSnippetAction(
          Self, ActEditSnippetByNameExecute
        )
      );
      SetDonateAction(actDonate);
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

    // Create dialog box manager
    fDialogMgr := TDialogMgr.Create(Self);  // automatically freed

    // Create display manager
    fMainDisplayMgr := TMainDisplayMgr.Create(frmOverview, frmDetail);
    // select active tabs
    fMainDisplayMgr.SelectOverviewTab(fWindowSettings.OverviewTab);

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
      // detail view: default menu
      AddAction(actViewDependencies, cDetailPopupMenuDefault);
      AddSpacer(cDetailPopupMenuDefault);
      AddAction(actCopyInfo, cDetailPopupMenuDefault);
      AddAction(actCopySnippet, cDetailPopupMenuDefault);
      AddAction(actCopySource, cDetailPopupMenuDefault);
      AddSpacer(cDetailPopupMenuDefault);
      AddAction(actTestCompile, cDetailPopupMenuDefault);
      AddSpacer(cDetailPopupMenuDefault);
      AddAction(actSaveSnippet, cDetailPopupMenuDefault);
      AddAction(actPrint, cDetailPopupMenuDefault);
      AddSpacer(cDetailPopupMenuDefault);
      AddAction(actSelectAll, cDetailPopupMenuDefault);
      AddSpacer(cDetailPopupMenuDefault);
      AddAction(actCloseDetailsTab, cDetailPopupMenuDefault);
      // detail view: selected text menu
      AddAction(actCopy, cDetailPopupMenuTextSelect);
      AddAction(actSelectAll, cDetailPopupMenuTextSelect);
      // detail view: anchor menu
      AddAction(TActionFactory.CreateLinkAction(Self), cDetailPopupMenuAnchor);
      // detail view: image menu
      AddAction(TActionFactory.CreateLinkAction(Self), cDetailPopupMenuImage);
      // tab set menu
      AddAction(actCloseDetailsTab, cDetailTabSetPopupMenu);
      AddAction(actCloseUnselectedDetailsTabs, cDetailTabSetPopupMenu);
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
    fMainDisplayMgr.ShowWelcomePage;
  finally
    // Ready to start using app: request splash form closes and enable form
    SplashForm.RequestClose;
    Enabled := True;
  end;
end;

procedure TMainForm.LoadSnippets(const Loader: TProc);
  {Loads Snippets object from database and re-intitialises display.
    @param Loader [in] Closure that performs actual loading of database.
  }
resourcestring
  sLoadingDatabase  = 'Loading database...';  // status bar message
begin
  // Inform that database is being loaded via status bar
  fStatusBarMgr.ShowSimpleMessage(sLoadingDatabase);
  fHistory.Clear;
  fMainDisplayMgr.ClearAll;
  // Load the database
  try
    Loader;
  except
    on E: ECodeSnip do
      Application.HandleException(E);
  end;
  // Re-initialise display
  fMainDisplayMgr.Initialise;
  // Display updated database stats and search results in status bar
  fStatusBarMgr.Update;
end;

procedure TMainForm.ReloadDatabase;
  {Reloads the whole database in a thread.
  }
begin
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
  {Determines if splitter can resize the controls it governs.
    @param Sender [in] Not used.
    @param NewSize [in] New size of governed control.
    @param Accept [in/out] Set false if splitter cannot resize the control.
  }
begin
  if (NewSize < TWindowSettings.MinLeftPanelWidth)
    or (NewSize > ClientWidth - TWindowSettings.MinRightPanelWidth) then
    Accept := False;
end;

end.

