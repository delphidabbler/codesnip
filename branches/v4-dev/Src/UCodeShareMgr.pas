{
 * UCodeShareMgr.pas
 *
 * Implements a static class that manages sharing of user defined snippets.
 * Provides support for exporting snippets, importing snippets and submitting
 * snippets to the online database.
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
 * The Original Code is UCodeShareMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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
    class procedure Submit(ViewItem: IView);
      {Submits code for consideration to be included in main database.
        @param ViewItem [in] View item that may contain a user defined snippet.
          If so the snippet is included in code for submission by default.
      }
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
  FmCodeExportDlg, FmCodeSubmitDlg, UCodeImportMgr, USnippets;


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
begin
  TCodeImportMgr.Execute;
end;

class procedure TCodeShareMgr.Submit(ViewItem: IView);
  {Submits code for consideration to be included in main database.
    @param ViewItem [in] View item that may contain a user defined snippet. If
      so the snippet is included in code for submission by default.
  }
begin
  TCodeSubmitDlg.Execute(nil, GetSnippetFromView(ViewItem));
end;

end.

