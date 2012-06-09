{
 * UPrintMgr.pas
 *
 * Implements a class that manages printing of a document providing information
 * about a snippet.
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
 * The Original Code is UPrintMgr.pas
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


unit UPrintMgr;


interface


uses
  // Delphi
  Classes,
  // Project
  DB.USnippet, UBaseObjects, UPrintDocuments, UPrintInfo, URTFUtils, UView;


type

  {
  TPrintMgr:
    Class that manages printing of a document providing information about a
    snippet. It generates a RTF formatted document and passes it to the print
    engine for printing.
  }
  TPrintMgr = class(TNoPublicConstructObject)
  strict private
    var
      fViewItem: IView;
    function GetDocGenerator: IPrintDocument;
  strict protected
    constructor InternalCreate(ViewItem: IView);
      {Class constructor. Sets up object to print a view item.
        @param ViewItem [in] View item to be printed. Must be a snippet.
      }
    procedure DoPrint;
      {Prints document containing information about a snippet. Uses print
      engine.
      }
  public
    class procedure Print(ViewItem: IView);
      {Prints details of a view item using print engine.
        @param ViewItem [in] View item to be printed. Must be a snippet.
      }
    class function CanPrint(ViewItem: IView): Boolean;
      {Checks if a view item can be printed.
        @param ViewItem [in] View item to be checked.
        @return True if view item can be printed, False if not.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UPrintEngine;


{ TPrintMgr }

class function TPrintMgr.CanPrint(ViewItem: IView): Boolean;
  {Checks if a view item can be printed.
    @param ViewItem [in] View item to be checked.
    @return True if view item can be printed, False if not.
  }
begin
  // Can print snippets or non-empty categories
  Result := Supports(ViewItem, ISnippetView)
    or
  (
    Supports(ViewItem, ICategoryView)
      and
    ((ViewItem as ICategoryView).Category.Snippets.Count > 0)
  );
end;

procedure TPrintMgr.DoPrint;
  {Prints document containing information about a snippet. Uses print engine.
  }
var
  PrintEngine: TPrintEngine;  // object that prints the print document
  Document: TRTF;             // generated print document
begin
  PrintEngine := TPrintEngine.Create;
  try
    PrintEngine.Title := fViewItem.Description;
    Document := GetDocGenerator.Generate;
    PrintEngine.Print(Document);
  finally
    PrintEngine.Free;
  end;
end;

function TPrintMgr.GetDocGenerator: IPrintDocument;
var
  SnippetView: ISnippetView;
  CategoryView: ICategoryView;
begin
  Result := nil;
  if Supports(fViewItem, ISnippetView, SnippetView) then
    Result := TSnippetPrintDocument.Create(SnippetView.Snippet)
  else if Supports(fViewItem, ICategoryView, CategoryView) then
    Result := TCategoryPrintDocument.Create(CategoryView.Category);
  Assert(Assigned(Result), ClassName + '.GetPrintDocument: Invalid view');
end;

constructor TPrintMgr.InternalCreate(ViewItem: IView);
  {Class constructor. Sets up object to print a view item.
    @param ViewItem [in] View item to be printed. Must be a snippet.
  }
begin
  inherited InternalCreate;
  fViewItem := ViewItem;
end;

class procedure TPrintMgr.Print(ViewItem: IView);
  {Prints details of a view item using print engine.
    @param ViewItem [in] View item to be printed. Must be a snippet.
  }
begin
  Assert(Assigned(ViewItem), ClassName + '.Print: ViewItem is nil');
  Assert(CanPrint(ViewItem), ClassName + '.Print: ViewItem can''t be printed');
  with InternalCreate(ViewItem) do
    try
      DoPrint;
    finally
      Free;
    end;
end;

end.

