{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a static class that manages sharing of user defined snippets.
 * Provides support for exporting snippets, importing snippets and submitting
 * snippets to the online database.
}


unit UCodeShareMgr;


interface


uses
  // Project
  DB.USnippet, UBaseObjects, UView;


type

  {
  TCodeShareMgr:
    Sealed static class that manages sharing of user defined snippets. Provides
    support for exporting snippets, importing snippets and submitting snippets
    to the online database.
  }
  TCodeShareMgr = class sealed(TNoConstructObject)
  strict private
    class function GetSnippetFromView(ViewItem: IView): TSnippet;
      {Gets reference to any user defined snippet represented by a view item.
        @param ViewItem [in] View item for which snippet is required.
        @return Reference to required snippet or nil if view item does not
          represent a snippet or if snippet is not user defined.
      }
  public
    class function CanShare: Boolean;
      {Checks if there are any user defined snippets that can be shared (i.e.
      exported or submitted.
        @return True if user defined snippets exist in database.
      }
    class procedure ExportCode(ViewItem: IView);
      {Exports user defined code to an export file.
        @param ViewItem [in] View item that may contain a user defined snippet.
          If so the snippet is included in the export file by default.
      }
    class procedure ImportCode;
      {Imports user defined code from an export file.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  DB.UMain, FmCodeExportDlg, FmCodeImportDlg, UCodeImportMgr;


{ TCodeShareMgr }

class function TCodeShareMgr.CanShare: Boolean;
  {Checks if there are any user defined snippets that can be shared (i.e.
  exported or submitted.
    @return True if user defined snippets exist in database.
  }
begin
  Result := Database.Snippets.Count(True) > 0;
end;

class procedure TCodeShareMgr.ExportCode(ViewItem: IView);
  {Exports user defined code to an export file.
    @param ViewItem [in] View item that may contain a user defined snippet. If
      so the snippet is included in the export file by default.
  }
begin
  TCodeExportDlg.Execute(nil, GetSnippetFromView(ViewItem));
end;

class function TCodeShareMgr.GetSnippetFromView(
  ViewItem: IView): TSnippet;
  {Gets reference to any user defined snippet represented by a view item.
    @param ViewItem [in] View item for which snippet is required.
    @return Reference to required snippet or nil if view item does not represent
      a snippet or if snippet is not user defined.
  }
var
  SnippetView: ISnippetView;  // ViewItem as snippet view if supported
begin
  if Supports(ViewItem, ISnippetView, SnippetView)
    and (SnippetView.Snippet.UserDefined) then
    Result := SnippetView.Snippet
  else
    Result := nil;
end;

class procedure TCodeShareMgr.ImportCode;
  {Imports user defined code from an export file.
  }
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

