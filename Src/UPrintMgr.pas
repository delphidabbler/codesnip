{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that manages printing of a document providing information
 * about certain view items.
}


unit UPrintMgr;


interface


uses
  // Delphi
  Classes,
  // Project
  DB.USnippet, UBaseObjects, UPrintDocuments, UPrintInfo, URTFUtils, UView;


type
  ///  <summary>Class that manages printing of a document providing information
  ///  about certain view items.</summary>
  ///  <remarks>
  ///  <para>Currently supports printing of snippets and non-empty categories.
  ///  </para>
  ///  <para>Generates an RTF formatted document and passes it to the print
  ///  engine for printing.</para>
  ///  </remarks>
  TPrintMgr = class(TNoPublicConstructObject)
  strict private
    var
      ///  <summary>View item to be printed.</summary>
      fViewItem: IView;
    ///  <summary>Returns interface to an object that can generate a print
    ///  document for the current view item.</summary>
    function GetDocGenerator: IPrintDocument;
  strict protected
    ///  <summary>Contructs object for use in printing given view item.
    ///  </summary>
    constructor InternalCreate(ViewItem: IView);
    ///  <summary>Performs printing of document containing information about
    ///  view item.</summary>
    procedure DoPrint;
  public
    ///  <summary>Prints details of given view item using print engine.
    ///  </summary>
    class procedure Print(ViewItem: IView);
    ///  <summary>Checks if given view item can be printed.</summary>
    ///  <remarks>View item must either represent a snippet or a non-empty
    ///  category to be able to be printed.</remarks>
    class function CanPrint(ViewItem: IView): Boolean;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UPrintEngine;


{ TPrintMgr }

class function TPrintMgr.CanPrint(ViewItem: IView): Boolean;
begin
  // Can print snippets or non-empty categories
  Result := Supports(ViewItem, ISnippetView)
    or
  (
    Supports(ViewItem, ICategoryView)
      and
    not (ViewItem as ICategoryView).Category.Snippets.IsEmpty
  );
end;

procedure TPrintMgr.DoPrint;
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
begin
  inherited InternalCreate;
  fViewItem := ViewItem;
end;

class procedure TPrintMgr.Print(ViewItem: IView);
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

