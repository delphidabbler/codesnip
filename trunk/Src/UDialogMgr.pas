{
 * UDialogMgr.pas
 *
 * Implements class that manages the display of dialog boxes.
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
 * The Original Code is UDialogMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UDialogMgr;


interface


uses
  // Delphi
  Classes,
  // Project
  DB.USnippet, UCompileMgr, USearch;


type

  {
  TDialogMgr:
    Class that manages the display of dialog boxes. Has Owner property that is
    supplied to each dialog box as it is displayed.
  }
  TDialogMgr = class(TComponent)
  public
    procedure ShowAboutDlg;
      {Displays About box.
      }
    procedure ShowBugReportDlg;
      {Displays Bug Report dialog box.
      }
    function ExecFindCompilerDlg(out ASearch: ISearch;
      out RefineExisting: Boolean): Boolean;
      {Displays Find Compiler dialog box.
        @param ASearch [out] Set to object recording search details if user OKs.
        @param RefineExisting [out] Set to flag indicating if any existing
          search is to be refined (True) or if compiler search is to apply to
          whole database (False).
        @return True if user OKs or false if user cancels.
      }
    function ExecFindTextDlg(out ASearch: ISearch; out RefineExisting: Boolean):
      Boolean;
      {Displays Find Text dialog box.
        @param ASearch [out] Set to object recording search details if user OKs.
        @param RefineExisting [out] Set to flag indicating if any existing
          search is to be refined (True) or if text search is to apply to whole
          database (False).
        @return True if user OKs or false if user cancels.
      }
    function ExecFindXRefsDlg(const ASnippet: TSnippet;
      out ASearch: ISearch): Boolean;
      {Displays Find Cross References dialog box.
        @param ASnippet [in] Snippet for which Cross-references are required.
        @param ASearch [out] Set to object recording search details if user OKs.
        @return True if user OKs or false if user cancels.
      }
    function ExecPreferencesDlg(out UpdateUI: Boolean): Boolean;
      {Display Preferences dialog box.
        @param UpdateUI [out] Flag indicates if UI needs to be updated as a
          result of changes to preferences.
        @return True if user OKs and preferences are updated or False if
          cancelled.
      }
    function ExecRegistrationDlg: Boolean;
      {Displays Registration dialog box.
        @return True if program was registered and False if not.
      }
    function ExecSelectionSearchDlg(const SelectedSnippets: TSnippetList;
      out ASearch: ISearch): Boolean;
      {Displays Select Snippets dialog box.
        @param SelectedSnippets [in] Default list of selected snippets.
        @param ASearch [out] Search to be performed if user OKs.
        @return True if user OKs or false if user cancels.
      }
    function ExecUpdateDlg: Boolean;
      {Displays Update From Web dialog box.
        @return True if database updated succesfully or false if database up to
          date, update failed or user cancelled.
      }
    function ExecPrintDlg: Boolean;
      {Displays Print dialog box.
        @return True if user OKs, False if cancelled.
      }
    function ExecPageSetupDlg: Boolean;
      {Displays Page Setup dialog box. Records page setup in persistent storage
      if user OKs.
        @return True if user OKs, False if cancelled.
      }
    procedure ShowDonateDlg;
      {Displays Donate dialog box.
      }
    procedure ShowTestUnit(const Snippet: TSnippet);
      {Displays test unit used to test compile a snippet.
        @param Snippet [in] Snippet to be test compiled.
      }
    procedure ShowDependencyTree(const Snippet: TSnippet);
      {Displays dependency tree for a snippet.
        @param Snippet [in] Snippet for which dependency tree is required.
      }
    function ExecProxyServerDlg: Boolean;
      {Displays Proxy Server configuration dialog box.
        @return True if user OKs or False if user cancels.
      }
    procedure ShowNewsDlg;
      {Displays latest news about CodeSnip and database in a dialog box.
      }
    procedure ShowTestCompileDlg(const CompileMgr: TCompileMgr;
      const Snippet: TSnippet);
      {Displays test compile dialog box that performs a test compilation.
        @param CompileMgr [in] Object used to manage compilation and retain
          results.
        @param Snippet [in] Snippet to be compiled.
      }
    procedure ShowProgramUpdatesDlg;
      {Displays dialogue box used to check for database and program updates.
      }
  end;


implementation


uses
  // Delphi
  Forms,
  // Project
  FmAboutDlg, FmDependenciesDlg, FmDonateDlg, FmFindCompilerDlg, FmFindTextDlg,
  FmFindXRefsDlg, FmNewsDlg, FmPreferencesDlg, FmPrintDlg, FmProgramUpdatesDlg,
  FmProxyServerDlg, FmRegistrationDlg, FmSelectionSearchDlg, FmTestCompileDlg,
  FmUpdateDlg, FmUserBugReportDlg, UPageSetupDlgMgr, UTestUnitDlgMgr;


{ TDialogMgr }

function TDialogMgr.ExecFindCompilerDlg(out ASearch: ISearch;
  out RefineExisting: Boolean): Boolean;
  {Displays Find Compiler dialog box.
    @param ASearch [out] Set to object recording search details if user OKs.
    @param RefineExisting [out] Set to flag indicating if any existing search is
      to be refined (True) or if compiler search is to apply to whole database
      (False).
    @return True if user OKs or false if user cancels.
  }
begin
  Result := TFindCompilerDlg.Execute(Owner, ASearch, RefineExisting);
end;

function TDialogMgr.ExecFindTextDlg(out ASearch: ISearch;
  out RefineExisting: Boolean): Boolean;
  {Displays Find Text dialog box.
    @param ASearch [out] Set to object recording search details if user OKs.
    @param RefineExisting [out] Set to flag indicating if any existing search is
      to be refined (True) or if text search is to apply to whole database
      (False).
    @return True if user OKs or false if user cancels.
  }
begin
  Result := TFindTextDlg.Execute(Owner, ASearch, RefineExisting);
end;

function TDialogMgr.ExecFindXRefsDlg(const ASnippet: TSnippet;
  out ASearch: ISearch): Boolean;
  {Displays Find Cross References dialog box.
    @param ASnippet [in] Snippet for which Cross-references are required.
    @param ASearch [out] Set to object recording search details if user OKs.
    @return True if user OKs or false if user cancels.
  }
begin
  Result := TFindXRefsDlg.Execute(Owner, ASnippet, ASearch);
end;

function TDialogMgr.ExecPageSetupDlg: Boolean;
  {Displays Page Setup dialog box. Records page setup in persistent storage if
  user OKs.
    @return True if user OKs, False if cancelled.
  }
begin
  Result := TPageSetupDlgMgr.Execute(Owner);
end;

function TDialogMgr.ExecPreferencesDlg(out UpdateUI: Boolean): Boolean;
  {Display Preferences dialog box.
    @param UpdateUI [out] Flag indicates if UI needs to be updated as a result
      of changes to preferences.
    @return True if user OKs and preferences are updated or False if cancelled.
  }
begin
  Result := TPreferencesDlg.Execute(Owner, UpdateUI);
end;

function TDialogMgr.ExecPrintDlg: Boolean;
  {Displays Print dialog box.
    @return True if user OKs, False if cancelled.
  }
begin
  Result := TPrintDlg.Execute(Owner);
end;

function TDialogMgr.ExecProxyServerDlg: Boolean;
  {Displays Proxy Server configuration dialog box.
    @return True if user OKs or False if user cancels.
  }
begin
  Result := TProxyServerDlg.Execute(Owner);
end;

function TDialogMgr.ExecRegistrationDlg: Boolean;
  {Displays Registration dialog box.
    @return True if program was registered and False if not.
  }
begin
  Result := TRegistrationDlg.Execute(Owner);
end;

function TDialogMgr.ExecSelectionSearchDlg(
  const SelectedSnippets: TSnippetList; out ASearch: ISearch): Boolean;
  {Displays Select Snippets dialog box.
    @param SelectedSnippets [in] Default list of selected snippets.
    @param ASearch [out] Search to be performed if user OKs.
    @return True if user OKs or false if user cancels.
  }
begin
  Result := TSelectionSearchDlg.Execute(Owner, SelectedSnippets, ASearch);
end;

function TDialogMgr.ExecUpdateDlg: Boolean;
  {Displays Update From Web dialog box.
    @return True if database updated succesfully or false if database up to
      date, update failed or user cancelled.
  }
begin
  Result := TUpdateDlg.Execute(Owner);
end;

procedure TDialogMgr.ShowAboutDlg;
  {Displays About box.
  }
begin
  TAboutDlg.Execute(Owner);
end;

procedure TDialogMgr.ShowBugReportDlg;
  {Displays Bug Report dialog box.
  }
begin
  TUserBugReportDlg.Execute(Owner);
end;

procedure TDialogMgr.ShowDependencyTree(const Snippet: TSnippet);
  {Displays dependency tree for a snippet.
    @param Snippet [in] Snippet for which dependency tree is required.
  }
begin
  TDependenciesDlg.Execute(Owner, Snippet, [tiDependsUpon, tiRequiredBy]);
end;

procedure TDialogMgr.ShowDonateDlg;
  {Displays Donate dialog box.
  }
begin
  TDonateDlg.Execute(Owner);
end;

procedure TDialogMgr.ShowNewsDlg;
  {Displays latest news about CodeSnip and database in a dialog box.
  }
begin
  TNewsDlg.Execute(Owner);
end;

procedure TDialogMgr.ShowProgramUpdatesDlg;
  {Displays dialogue box used to check for database and program updates.
  }
begin
  TProgramUpdatesDlg.Execute(Owner);
end;

procedure TDialogMgr.ShowTestCompileDlg(const CompileMgr: TCompileMgr;
  const Snippet: TSnippet);
  {Displays test compile dialog box that performs a test compilation.
    @param CompileMgr [in] Object used to manage compilation and retain
      results.
    @param Snippet [in] Snippet to be compiled.
  }
begin
  TTestCompileDlg.Execute(Owner, CompileMgr, Snippet);
end;

procedure TDialogMgr.ShowTestUnit(const Snippet: TSnippet);
  {Displays test unit used to test compile a snippet.
    @param Snippet [in] Snippet to be test compiled.
  }
begin
  TTestUnitDlgMgr.DisplayTestUnit(Owner, Snippet);
end;

end.

