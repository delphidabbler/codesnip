{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements class that manages the display of dialog boxes.
}


unit UDialogMgr;


interface


uses
  // Delphi
  Classes,
  // Project
  DB.USnippet, UCompileMgr, USearch;


type

  ///  <summary>Manages the display of various dialogue boxes.</summary>
  ///  <remarks>Any window handle associated with the Owner component passed to
  ///  the inherited constructor is used as the parent window of the dialogue
  ///  boxes.</remarks>
  TDialogMgr = class(TComponent)
  public

    ///  <summary>Displays the About dialogue box.</summary>
    procedure ShowAboutDlg;

    ///  <summary>Displays the Bug Report dialogue box.</summary>
    procedure ShowBugReportDlg;

    ///  <summary>Displays the Find Compiler dialogue box.</summary>
    ///  <param name="ASearch">ISearch [out] Set to object containing search
    ///  details. Undefined if user cancelled dialogue.</param>
    ///  <param name="RefineExisting">Boolean [out] Set to flag indicating if
    ///  any existing search is to be refined (True) or if this search is to
    ///  apply to whole database. Undefined if user cancelled dialogue.</param>
    ///  <returns>Boolean. True if user OKd dialogue or False if user cancelled.
    ///  </returns>
    function ExecFindCompilerDlg(out ASearch: ISearch;
      out RefineExisting: Boolean): Boolean;

    ///  <summary>Displays the Find Text dialogue box.</summary>
    ///  <param name="ASearch">ISearch [out] Set to object containing search
    ///  details. Undefined if user cancelled dialogue.</param>
    ///  <param name="RefineExisting">Boolean [out] Set to flag indicating if
    ///  any existing search is to be refined (True) or if this search is to
    ///  apply to whole database. Undefined if user cancelled dialogue.</param>
    ///  <returns>Boolean. True if user OKd dialogue or False if user cancelled.
    ///  </returns>
    function ExecFindTextDlg(out ASearch: ISearch; out RefineExisting: Boolean):
      Boolean;

    ///  <summary>Displays Find Cross References dialogue box.</summary>
    ///  <param name="ASnippet">TSnippet [in] Snippet for which cross-references
    ///  are required.</param>
    ///  <param name="ASearch">ISearch [out] Set to object containing search
    ///  details. Undefined if user cancelled dialogue.</param>
    ///  <returns>Boolean. True if user OKd dialogue or False if user cancelled.
    ///  </returns>
    function ExecFindXRefsDlg(const ASnippet: TSnippet;
      out ASearch: ISearch): Boolean;

    ///  <summary>Displays Preferences dialogue box showing all tabs.</summary>
    ///  <param name="UpdateUI">Boolean [out] Flag that indicates if the UI
    ///  needs to be updated as a result of changes to preferences.</param>
    ///  <returns>Boolean. True if user OKd dialogue or False if user cancelled.
    ///  </returns>
    function ExecPreferencesDlg(out UpdateUI: Boolean): Boolean; overload;

    ///  <summary>Displays Preference dialogue box showing a single tab.
    ///  </summary>
    ///  <param name="PageClassName">string [in] Class name of frame that
    ///  implements the content of the tab this is to be displayed.</param>
    ///  <param name="UpdateUI">Boolean [out] Flag that indicates if the UI
    ///  needs to be updated as a result of changes to preferences.</param>
    ///  <returns>Boolean. True if user OKd dialogue or False if user cancelled.
    ///  </returns>
    function ExecPreferencesDlg(const PageClassName: string;
      out UpdateUI: Boolean): Boolean; overload;

    ///  <summary>Displays Select Snippets dialogue box.</summary>
    ///  <param name="SelectedSnippets">TSnippetList [in] List of pre-selected
    ///  snippets.</param>
    ///  <param name="ASearch">ISearch [out] Set to object containing search
    ///  details. Undefined if user cancelled dialogue.</param>
    ///  <returns>Boolean. True if user OKd dialogue or False if user cancelled.
    ///  </returns>
    function ExecSelectionSearchDlg(const SelectedSnippets: TSnippetList;
      out ASearch: ISearch): Boolean;

    ///  <summary>Displays Update From Web dialogue box used to update the local
    ///  copy of the online database.</summary>
    ///  <returns>Boolean. True if the database was updated successfully or
    ///  False if the local database is up to date, if the update failed or if
    ///  the user cancelled.</returns>
    function ExecDBUpdateDlg: Boolean;

    ///  <summary>Displays the Print dialogue box.</summary>
    ///  <returns>Boolean. True if user OKd dialogue or False if user cancelled.
    ///  </returns>
    function ExecPrintDlg: Boolean;

    ///  <summary>Displays the Donate dialogue box.</summary>
    procedure ShowDonateDlg;

    ///  <summary>Displays the Test Unit dialogue box.</summary>
    ///  <param name="Snippet">TSnippet [in] Snippet for which test unit is to
    ///  be generated.</param>
    procedure ShowTestUnitDlg(const Snippet: TSnippet);

    ///  <summary>Shows Dependencies dialogue box.</summary>
    ///  <param name="Snippet">TSnippets [in] Snippet for which dependencies are
    ///  to be displayed.</param>
    ///  <param name="HelpKeyword">string [in] Keyword of help topic to be
    ///  associated with the dialogue box.</param>
    ///  <returns>ISearch. Search containing details of any snippets to be
    ///  selected when the dialogue box closes. May be nil if no search is to be
    ///  performed.</returns>
    function ShowDependenciesDlg(const Snippet: TSnippet;
      const HelpKeyword: string): ISearch;

    ///  <summary>Displays the Proxy Server configuration dialogue box.
    ///  </summary>
    ///  <returns>Boolean. True if user OKd dialogue or False if user cancelled.
    ///  </returns>
    function ExecProxyServerDlg: Boolean;

    ///  <summary>Displays the Test Compile dialogue box.</summary>
    ///  <param name="CompileMgr">TCompileMgr [in] Object used to manage test
    ///  compilation and to retain results.</param>
    ///  <param name="Snippet">TSnippet [in] Snippet to be test compiled.
    ///  </param>
    procedure ShowTestCompileDlg(const CompileMgr: TCompileMgr;
      const Snippet: TSnippet);

    ///  <summary>Displays the SWAG Import Wizard dialogue box.</summary>
    procedure ShowSWAGImportDlg;
  end;


implementation


uses
  // Delphi
  Forms,
  // Project
  FmAboutDlg, FmDBUpdateDlg, FmDependenciesDlg, FmDonateDlg, FmFindCompilerDlg,
  FmFindTextDlg, FmFindXRefsDlg, FmPreferencesDlg, FmPrintDlg,
  FmProxyServerDlg,
  FmSelectionSearchDlg, FmSWAGImportDlg, FmTestCompileDlg, FmUserBugReportDlg,
  UTestUnitDlgMgr;


{ TDialogMgr }

function TDialogMgr.ExecDBUpdateDlg: Boolean;
begin
  Result := TDBUpdateDlg.Execute(Owner);
end;

function TDialogMgr.ExecFindCompilerDlg(out ASearch: ISearch;
  out RefineExisting: Boolean): Boolean;
begin
  Result := TFindCompilerDlg.Execute(Owner, ASearch, RefineExisting);
end;

function TDialogMgr.ExecFindTextDlg(out ASearch: ISearch;
  out RefineExisting: Boolean): Boolean;
begin
  Result := TFindTextDlg.Execute(Owner, ASearch, RefineExisting);
end;

function TDialogMgr.ExecFindXRefsDlg(const ASnippet: TSnippet;
  out ASearch: ISearch): Boolean;
begin
  Result := TFindXRefsDlg.Execute(Owner, ASnippet, ASearch);
end;

function TDialogMgr.ExecPreferencesDlg(out UpdateUI: Boolean): Boolean;
begin
  Result := TPreferencesDlg.Execute(Owner, UpdateUI);
end;

function TDialogMgr.ExecPreferencesDlg(const PageClassName: string;
  out UpdateUI: Boolean): Boolean;
begin
  Result := TPreferencesDlg.Execute(Owner, PageClassName, UpdateUI);
end;

function TDialogMgr.ExecPrintDlg: Boolean;
begin
  Result := TPrintDlg.Execute(Owner);
end;

function TDialogMgr.ExecProxyServerDlg: Boolean;
begin
  Result := TProxyServerDlg.Execute(Owner);
end;

function TDialogMgr.ExecSelectionSearchDlg(
  const SelectedSnippets: TSnippetList; out ASearch: ISearch): Boolean;
begin
  Result := TSelectionSearchDlg.Execute(Owner, SelectedSnippets, ASearch);
end;

procedure TDialogMgr.ShowAboutDlg;
begin
  TAboutDlg.Execute(Owner);
end;

procedure TDialogMgr.ShowBugReportDlg;
begin
  TUserBugReportDlg.Execute(Owner);
end;

function TDialogMgr.ShowDependenciesDlg(const Snippet: TSnippet;
  const HelpKeyword: string): ISearch;
begin
  Result := TDependenciesDlg.Execute(
    Owner, Snippet, [tiDependsUpon, tiRequiredBy], True, HelpKeyword
  );
end;

procedure TDialogMgr.ShowDonateDlg;
begin
  TDonateDlg.Execute(Owner);
end;

procedure TDialogMgr.ShowSWAGImportDlg;
begin
  TSWAGImportDlg.Execute(Owner);
end;

procedure TDialogMgr.ShowTestCompileDlg(const CompileMgr: TCompileMgr;
  const Snippet: TSnippet);
begin
  TTestCompileDlg.Execute(Owner, CompileMgr, Snippet);
end;

procedure TDialogMgr.ShowTestUnitDlg(const Snippet: TSnippet);
begin
  TTestUnitDlgMgr.DisplayTestUnit(Owner, Snippet);
end;

end.

