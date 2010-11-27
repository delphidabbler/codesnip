{
 * UView.pas
 *
 * Classes that encapsulate and provide information about "view items" that are
 * displayed in the user interface, e.g.. routines, categories and the welcome
 * page.
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
 * The Original Code is UView.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UView;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  UAlphabet, UBaseObjects, USnippetKindInfo, USnippets;


type

  {
  TViewItem:
    Abstract base class for the different kind of "view items" that are
    displayed in the user interface.
  }
  TViewItem = class abstract(TObject)
  strict protected
    function GetDescription: string; virtual; abstract;
      {Read accessor for Description property.
        @return Description.
      }
  public
    function IsEqual(const ViewItem: TViewItem): Boolean; virtual; abstract;
      {Checks if this view item is the same as another view item.
        @param ViewItem [in] View item being testing for equality.
        @return True if view items are same, false otherwise.
      }
    property Description: string read GetDescription;
      {Description of view item}
  end;

  TNulViewItem = class sealed(TViewItem)
  strict protected
    function GetDescription: string; override;
  public
    function IsEqual(const ViewItem: TViewItem): Boolean; override;
  end;

  TStartPageViewItem = class sealed(TViewItem)
  strict protected
    function GetDescription: string; override;
  public
    function IsEqual(const ViewItem: TViewItem): Boolean; override;
  end;

  TSnippetViewItem = class sealed(TViewItem)
  strict private
    fSnippet: TRoutine;
  strict protected
    function GetDescription: string; override;
  public
    constructor Create(const Snippet: TRoutine);
    function IsEqual(const ViewItem: TViewItem): Boolean; override;
    property Snippet: TRoutine read fSnippet;
  end;

  TCategoryViewItem = class sealed(TViewItem)
  strict private
    fCategory: TCategory;
  strict protected
    function GetDescription: string; override;
  public
    constructor Create(const Category: TCategory);
    function IsEqual(const ViewItem: TViewItem): Boolean; override;
    property Category: TCategory read fCategory;
  end;

  TSnippetKindViewItem = class sealed(TViewItem)
  strict private
    fKindInfo: TSnippetKindInfo;
  strict protected
    function GetDescription: string; override;
  public
    constructor Create(const KindInfo: TSnippetKindInfo);
    function IsEqual(const ViewItem: TViewItem): Boolean; override;
    property KindInfo: TSnippetKindInfo read fKindInfo;
  end;

  TInitialLetterViewItem = class sealed(TViewItem)
  strict private
    fLetter: TLetter;
  strict protected
    function GetDescription: string; override;
  public
    constructor Create(const Letter: TLetter);
    function IsEqual(const ViewItem: TViewItem): Boolean; override;
    property Letter: TLetter read fLetter;
  end;

  TViewItemFactory = class sealed(TNoConstructObject)
  public
    class procedure ReplaceView(var OldViewItem: TViewItem;
      const NewViewItem: TViewItem);
    class function CreateCopy(const ViewItem: TViewItem): TViewItem;
    class function CreateNulView: TNulViewItem;
    class function CreateStartPageView: TStartPageViewItem;
    class function CreateSnippetView(const Snippet: TRoutine): TSnippetViewItem;
    class function CreateCategoryView(const Category: TCategory):
      TCategoryViewItem;
    class function CreateSnippetKindView(const KindInfo: TSnippetKindInfo):
      TSnippetKindViewItem;
    class function CreateInitialLetterView(const Letter: TLetter):
      TInitialLetterViewItem;
  end;

  {
  TViewItemList:
    Implements a list of TViewItem objects.
  }
  TViewItemList = class sealed(TObjectList<TViewItem>)
  public
    constructor Create;
      {Object constructor. Sets up list.
      }
  end;


implementation


uses
  UExceptions;


{ TNulViewItem }

function TNulViewItem.GetDescription: string;
begin
  Result := '';
end;

function TNulViewItem.IsEqual(const ViewItem: TViewItem): Boolean;
begin
  Result := ViewItem is ClassType;
end;

{ TStartPageViewItem }

function TStartPageViewItem.GetDescription: string;
begin
  Result := 'Welcome';
end;

function TStartPageViewItem.IsEqual(const ViewItem: TViewItem): Boolean;
begin
  Result := ViewItem is ClassType;
end;

{ TSnippetViewItem }

constructor TSnippetViewItem.Create(const Snippet: TRoutine);
begin
  inherited Create;
  fSnippet := Snippet;
end;

function TSnippetViewItem.GetDescription: string;
begin
  Result := fSnippet.Name;
end;

function TSnippetViewItem.IsEqual(const ViewItem: TViewItem): Boolean;
begin
  if not (ViewItem is TSnippetViewItem) then
    Exit(False);
  Result := fSnippet.IsEqual((ViewItem as TSnippetViewItem).fSnippet);
end;

{ TCategoryViewItem }

constructor TCategoryViewItem.Create(const Category: TCategory);
begin
  inherited Create;
  fCategory := Category;
end;

function TCategoryViewItem.GetDescription: string;
begin
  Result := fCategory.Description;
end;

function TCategoryViewItem.IsEqual(const ViewItem: TViewItem): Boolean;
begin
  if not (ViewItem is ClassType) then
    Exit(False);
  Result := fCategory.IsEqual((ViewItem as TCategoryViewItem).fCategory);
end;

{ TSnippetKindViewItem }

constructor TSnippetKindViewItem.Create(const KindInfo: TSnippetKindInfo);
begin
  inherited Create;
  fKindInfo := KindInfo;
end;

function TSnippetKindViewItem.GetDescription: string;
begin
  Result := fKindInfo.Description;
end;

function TSnippetKindViewItem.IsEqual(const ViewItem: TViewItem): Boolean;
begin
  if not (ViewItem is TSnippetKindViewItem) then
    Exit(False);
  Result := fKindInfo.Kind = (ViewItem as TSnippetKindViewItem).fKindInfo.Kind;
end;

{ TInitialLetterViewItem }

constructor TInitialLetterViewItem.Create(const Letter: TLetter);
begin
  inherited Create;
  fLetter := Letter;
end;

function TInitialLetterViewItem.GetDescription: string;
begin
  Result := fLetter.Letter;
end;

function TInitialLetterViewItem.IsEqual(const ViewItem: TViewItem): Boolean;
begin
  if not (ViewItem is TInitialLetterViewItem) then
    Exit(False);
  Result := fLetter.Letter =
    (ViewItem as TInitialLetterViewItem).fLetter.Letter;
end;

{ TViewItemFactory }

class function TViewItemFactory.CreateCategoryView(
  const Category: TCategory): TCategoryViewItem;
begin
  Result := TCategoryViewItem.Create(Category);
end;

class function TViewItemFactory.CreateCopy(
  const ViewItem: TViewItem): TViewItem;
begin
  if not Assigned(ViewItem) or (ViewItem is TNulViewItem) then
    Result := CreateNulView
  else if ViewItem is TStartPageViewItem then
    Result := CreateStartPageView
  else if ViewItem is TSnippetViewItem then
    Result := CreateSnippetView((ViewItem as TSnippetViewItem).Snippet)
  else if ViewItem is TCategoryViewItem then
    Result := CreateCategoryView((ViewItem as TCategoryViewItem).Category)
  else if ViewItem is TSnippetKindViewItem then
    Result := CreateSnippetKindView((ViewItem as TSnippetKindViewItem).KindInfo)
  else if ViewItem is TInitialLetterViewItem then
    Result := CreateInitialLetterView(
      (ViewItem as TInitialLetterViewItem).Letter
    )
  else //if ViewItem <> nil then
    raise EBug.CreateFmt(
      '%0.s.CreateCopy: Unknown ViewItem class "%1:s"',
      [ClassName, ViewItem.ClassName]
    );
end;

class function TViewItemFactory.CreateInitialLetterView(
  const Letter: TLetter): TInitialLetterViewItem;
begin
  Result := TInitialLetterViewItem.Create(Letter);
end;

class function TViewItemFactory.CreateNulView: TNulViewItem;
begin
  Result := TNulViewItem.Create;
end;

class function TViewItemFactory.CreateSnippetKindView(
  const KindInfo: TSnippetKindInfo): TSnippetKindViewItem;
begin
  Result := TSnippetKindViewItem.Create(KindInfo);
end;

class function TViewItemFactory.CreateSnippetView(
  const Snippet: TRoutine): TSnippetViewItem;
begin
  Result := TSnippetViewItem.Create(Snippet);
end;

class function TViewItemFactory.CreateStartPageView: TStartPageViewItem;
begin
  Result := TStartPageViewItem.Create;
end;

class procedure TViewItemFactory.ReplaceView(var OldViewItem: TViewItem;
  const NewViewItem: TViewItem);
var
  Temp: TViewItem;
begin
  Temp := OldViewItem;
  OldViewItem := NewViewItem;
  Temp.Free;
end;

{ TViewItemList }

constructor TViewItemList.Create;
  {Object constructor. Sets up list.
  }
begin
  // List does not own the objects stored in it
  inherited Create(False);
end;

end.

