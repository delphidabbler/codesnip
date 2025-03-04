{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a static class that manages sharing of user defined snippets.
 * Provides support for exporting and importing snippets.
}


unit UCodeShareMgr;


interface


uses
  // Project
  DB.USnippet,
  UBaseObjects,
  UView;


type

  ///  <summary>Manages sharing snippets. Provides support for exporting and
  /// importing snippets.</summary>
  TCodeShareMgr = class sealed(TNoConstructObject)
  strict private
    ///  <summary>Gets reference to any snippet represented by a view item.
    ///  </summary>
    ///  <param name="ViewItem"><c>IView</c> [in] View item fromm which snippet
    ///  is to be extracted, if present.</param>
    ///  <returns><c>TSnippet</c>. Reference to the required snippet or nil if
    ///  the view item does not represent a snippet.</returns>
    class function GetSnippetFromView(ViewItem: IView): TSnippet;
  public
    ///  <summary>Checks if there are any snippets that can be shared (i.e.
    ///  exported).</summary>
    ///  <returns><c>Boolean</c>. <c>True</c> if database is not empty,
    ///  <c>False</c> otherwise.</returns>
    class function CanShare: Boolean;
    ///  <summary>Exports snippets to an export file.</summary>
    ///  <param name="ViewItem"><c>IView</c> [in] View item that may contain a
    ///  snippet. If so the snippet is included in the export file by default.
    ///  </param>
    class procedure ExportCode(ViewItem: IView);
    ///  <summary>Imports snippets from an export file.</summary>
    class procedure ImportCode;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  DB.UMain,
  DB.Vaults,
  FmCodeExportDlg,
  FmCodeImportDlg,
  UCodeImportMgr;


{ TCodeShareMgr }

class function TCodeShareMgr.CanShare: Boolean;
begin
  Result := not Database.Snippets.IsEmpty;
end;

class procedure TCodeShareMgr.ExportCode(ViewItem: IView);
begin
  TCodeExportDlg.Execute(nil, GetSnippetFromView(ViewItem));
end;

class function TCodeShareMgr.GetSnippetFromView(
  ViewItem: IView): TSnippet;
var
  SnippetView: ISnippetView;  // ViewItem as snippet view if supported
begin
  if Supports(ViewItem, ISnippetView, SnippetView) then
    Result := SnippetView.Snippet
  else
    Result := nil;
end;

class procedure TCodeShareMgr.ImportCode;
var
  ImportMgr: TCodeImportMgr;  // manages import of code
begin
  ImportMgr := TCodeImportMgr.Create;
  try
    TCodeImportDlg.Execute(nil, ImportMgr);
  finally
    ImportMgr.Free;
  end;
end;

end.


