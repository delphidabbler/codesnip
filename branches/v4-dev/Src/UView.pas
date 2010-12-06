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
  UBaseObjects, UInitialLetter, USnippetKindInfo, USnippets;

{ TODO: see if we can use generics in any way to simplify IViewKey
        implementations - there's a lot of duplication }
{ TODO: add key records to all items related to views (categories, snippets,
        etc) and define equals operator on them }
{ TODO: see if we can removed duplication between IView.IsEqual and
        IViewKey.IsEqual implementations. }

type

  { TODO: consider renaming this as IViewPersist }
  ///  <summary>
  ///  Interface supported by object that can compare key values associated with
  ///  a view for equality. The values stored in this object must persist even
  ///  if any object stored in the related IView object has been destroyed.
  ///  </summary>
  IViewKey = interface(IInterface)
    ['{E6C39F3C-63E3-4A3E-A4E7-B176DA121D9E}']
    ///  Checks if two view keys are equal.
    function IsEqual(const Key: IViewKey): Boolean;
  end;

  ///  <summary>
  ///  Interface supported by all view objects.
  ///  </summary>
  IView = interface(IInterface)
    ['{707ED3DC-1738-4B79-B1BE-1C554572097E}']
    ///  Checks if two view items are equal, i.e. they have the same "type" and
    ///  any properties have the the same values.
    function IsEqual(View: IView): Boolean;
    ///  View description.
    function GetDescription: string;
    property Description: string read GetDescription;
    ///  View key. Encapsulates the data that uniquely identifies the view item
    ///  without having have an instance of any object wrapped by the view item.
    function GetKey: IViewKey;
  end;

  ///  <summary>
  ///  Interface supported by nul view.
  ///  </summary>
  INulView = interface(IView)
    ['{18D06867-2E6D-4B62-A0A5-A269C91D18D6}']
  end;

  ///  <summary>
  ///  Interface supported by start page view.
  ///  </summary>
  IStartPageView = interface(IView)
    ['{9D2208C4-8FFC-4532-8C3F-07514EDFAB9D}']
  end;

  ///  <summary>
  ///  Interface supported by snippet views.
  ///  </summary>
  ISnippetView = interface(IView)
    ['{E0BD3AB7-2CB8-4B07-84D9-D7625DD020B0}']
    ///  Snippet associated with view.
    function GetSnippet: TRoutine;
    property Snippet: TRoutine read GetSnippet;
  end;

  ///  <summary>
  ///  Interface supported by category views.
  ///  </summary>
  ICategoryView = interface(IView)
    ['{C8BB04FD-B0B7-42F9-84AA-2F7FDD3A6FE8}']
    ///  Category associated with view.
    function GetCategory: TCategory;
    property Category: TCategory read GetCategory;
  end;

  ///  <summary>
  ///  Interface supported by snippet kind views.
  ///  </summary>
  ISnippetKindView = interface(IView)
    ['{FE074E72-857F-4D43-A17D-A951830B78E4}']
    ///  Info about snippet kind associated with view
    function GetKindInfo: TSnippetKindInfo;
    property KindInfo: TSnippetKindInfo read GetKindInfo;
  end;

  ///  <summary>
  ///  Interface supported by initial letter views.
  ///  </summary>
  IInitialLetterView = interface(IView)
    ['{E589E3E9-7178-4FF8-BDEF-D6F7DC9FEB85}']
    ///  Info about initial letter associated with view
    function GetInitialLetter: TInitialLetter;
    property InitialLetter: TInitialLetter read GetInitialLetter;
  end;

  ///  <summary>
  ///  List of view items.
  ///  </summary>
  TViewItemList = class sealed(TList<IView>);

  ///  <summary>
  ///  Static factory class used to create view instances of different types.
  ///  </summary>
  TViewItemFactory = class sealed(TNoConstructObject)
  public
    ///  Creates a copy of the given view. Copy has same type and properties.
    class function Clone(View: IView): IView;
    ///  Creates a nul view instance.
    class function CreateNulView: IView;
    ///  Creates a start page view instance.
    class function CreateStartPageView: IView;
    ///  Creates a snippet view instance associated with a given snippet.
    class function CreateSnippetView(const Snippet: TRoutine): IView;
    ///  Creates a category view instance associated with a given category
    class function CreateCategoryView(const Category: TCategory): IView;
    ///  Creates a snippet kind view instance associated with a given snippet
    ///  kind.
    class function CreateSnippetKindView(const KindInfo: TSnippetKindInfo):
      IView;
    ///  Creates an initial letter view instance associated with a given letter.
    class function CreateInitialLetterView(const Letter: TInitialLetter): IView;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UExceptions, USnippetIDs;


type

  ///  Nul view
  TNulViewItem = class sealed(TInterfacedObject,
    IView, INulView
  )
  strict private
    type
      TKey = class(TInterfacedObject, IViewKey)
      public
        function IsEqual(const Key: IViewKey): Boolean;
      end;
  public
    { IView methods }
    function GetDescription: string;
    function IsEqual(View: IView): Boolean;
    function GetKey: IViewKey;
  end;

  ///  View associated with start page
  TStartPageViewItem = class sealed(TInterfacedObject,
    IView, IStartPageView
  )
  strict private
    type
      TKey = class(TInterfacedObject, IViewKey)
      public
        function IsEqual(const Key: IViewKey): Boolean;
      end;
  public
    { IView methods }
    function GetDescription: string;
    function IsEqual(View: IView): Boolean;
    function GetKey: IViewKey;
  end;

  ///  View associated with a snippet.
  TSnippetViewItem = class sealed(TInterfacedObject,
    IView, ISnippetView
  )
  strict private
    ///  Associated snippet.
    var fSnippet: TRoutine;
    type
      TKey = class(TInterfacedObject, IViewKey)
      strict private
        fID: TSnippetID;
      public
        constructor Create(const ID: TSnippetID);
        function IsEqual(const Key: IViewKey): Boolean;
      end;
  public
    ///  Constructs view for a specified snippet.
    constructor Create(const Snippet: TRoutine);
    { IView methods }
    function GetDescription: string;
    function IsEqual(View: IView): Boolean;
    function GetKey: IViewKey;
    { ISnippetView methods }
    function GetSnippet: TRoutine;
  end;

  ///  View associated with a category.
  TCategoryViewItem = class sealed(TInterfacedObject,
    IView, ICategoryView
  )
  strict private
    ///  Associated category.
    var fCategory: TCategory;
    type
      TKey = class(TInterfacedObject, IViewKey)
      strict private
        fID: string;
      public
        constructor Create(const ID: string);
        function IsEqual(const Key: IViewKey): Boolean;
      end;
  public
    //  Constructs view for a specified category.
    constructor Create(const Category: TCategory);
    { IView methods }
    function GetDescription: string;
    function IsEqual(View: IView): Boolean;
    function GetKey: IViewKey;
    { ICategoryView methods }
    function GetCategory: TCategory;
  end;

  ///  View associated with a snippet kind grouping.
  TSnippetKindViewItem = class sealed(TInterfacedObject,
    IView, ISnippetKindView
  )
  strict private
    ///  Associated snippet kind.
    var fKindInfo: TSnippetKindInfo;
    type
      TKey = class(TInterfacedObject, IViewKey)
      strict private
        var fID: TSnippetKind;
      public
        constructor Create(const ID: TSnippetKind);
        function IsEqual(const Key: IViewKey): Boolean;
      end;
  public
    ///  Constructs view for a specified snippet kind.
    constructor Create(const KindInfo: TSnippetKindInfo);
    { IView methods }
    function GetDescription: string;
    function IsEqual(View: IView): Boolean;
    function GetKey: IViewKey;
    { ISnippetKindView methods }
    function GetKindInfo: TSnippetKindInfo;
  end;

  ///  View associated with an initial letter grouping.
  TInitialLetterViewItem = class sealed(TInterfacedObject,
    IView, IInitialLetterView
  )
  strict private
    ///  Associated initial letter.
    var fLetter: TInitialLetter;
    type
      TKey = class(TInterfacedObject, IViewKey)
      strict private
        var fID: TInitialLetter;
      public
        constructor Create(const ID: TInitialLetter);
        function IsEqual(const Key: IViewKey): Boolean;
      end;
  public
    ///  Constructs view for a specified initial letter.
    constructor Create(const Letter: TInitialLetter);
    { IView methods }
    function GetDescription: string;
    function IsEqual(View: IView): Boolean;
    function GetKey: IViewKey;
    { IInitialLetterView methods }
    function GetInitialLetter: TInitialLetter;
  end;


{ TNulViewItem }

function TNulViewItem.GetDescription: string;
begin
  Result := '';
end;

function TNulViewItem.GetKey: IViewKey;
begin
  Result := TKey.Create;
end;

function TNulViewItem.IsEqual(View: IView): Boolean;
begin
  Result := Supports(View, INulView);
end;

{ TNulViewItem.TKey }

function TNulViewItem.TKey.IsEqual(const Key: IViewKey): Boolean;
begin
  Result := Key is TKey;
end;

{ TStartPageViewItem }

function TStartPageViewItem.GetDescription: string;
resourcestring
  sDesc = 'Welcome';
begin
  Result := sDesc;
end;

function TStartPageViewItem.GetKey: IViewKey;
begin
  Result := TKey.Create;
end;

function TStartPageViewItem.IsEqual(View: IView): Boolean;
begin
  Result := Supports(View, IStartPageView);
end;

{ TStartPageViewItem.TKey }

function TStartPageViewItem.TKey.IsEqual(const Key: IViewKey): Boolean;
begin
  Result := Key is TKey;
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

function TSnippetViewItem.GetKey: IViewKey;
begin
  Result := TKey.Create(fSnippet.ID);
end;

function TSnippetViewItem.GetSnippet: TRoutine;
begin
  Result := fSnippet;
end;

function TSnippetViewItem.IsEqual(View: IView): Boolean;
var
  SnippetView: ISnippetView;
begin
  if not Supports(View, ISnippetView, SnippetView) then
    Exit(False);
  Result := fSnippet.IsEqual(SnippetView.Snippet);
end;

{ TSnippetViewItem.TKey }

constructor TSnippetViewItem.TKey.Create(const ID: TSnippetID);
begin
  inherited Create;
  fID := ID;
end;

function TSnippetViewItem.TKey.IsEqual(const Key: IViewKey): Boolean;
begin
  if not (Key is TSnippetViewItem.TKey) then
    Exit(False);
  Result := (Key as TSnippetViewItem.TKey).fID = fID;
end;

{ TCategoryViewItem }

constructor TCategoryViewItem.Create(const Category: TCategory);
begin
  inherited Create;
  fCategory := Category;
end;

function TCategoryViewItem.GetCategory: TCategory;
begin
  Result := fCategory;
end;

function TCategoryViewItem.GetDescription: string;
begin
  Result := fCategory.Description;
end;

function TCategoryViewItem.GetKey: IViewKey;
begin
  Result := TKey.Create(fCategory.Category);
end;

function TCategoryViewItem.IsEqual(View: IView): Boolean;
var
  CatView: ICategoryView;
begin
  if not Supports(View, ICategoryView, CatView) then
    Exit(False);
  Result := fCategory.IsEqual(CatView.Category);
end;

{ TCategoryViewItem.TKey }

constructor TCategoryViewItem.TKey.Create(const ID: string);
begin
  inherited Create;
  fID := ID;
end;

function TCategoryViewItem.TKey.IsEqual(const Key: IViewKey): Boolean;
begin
  if not (Key is TKey) then
    Exit(False);
  Result := AnsiSameText((Key as TKey).fID, fID);
end;

{ TSnippetKindViewItem }

constructor TSnippetKindViewItem.Create(const KindInfo: TSnippetKindInfo);
begin
  inherited Create;
  fKindInfo := KindInfo;
end;

function TSnippetKindViewItem.GetDescription: string;
begin
  Result := fKindInfo.DisplayName;
end;

function TSnippetKindViewItem.GetKey: IViewKey;
begin
  Result := TKey.Create(fKindInfo.Kind);
end;

function TSnippetKindViewItem.GetKindInfo: TSnippetKindInfo;
begin
  Result := fKindInfo;
end;

function TSnippetKindViewItem.IsEqual(View: IView): Boolean;
var
  SnipKindView: ISnippetKindView;
begin
  if not Supports(View, ISnippetKindView, SnipKindView) then
    Exit(False);
  Result := fKindInfo.Kind = SnipKindView.KindInfo.Kind;
end;

{ TSnippetKindViewItem.TKey }

constructor TSnippetKindViewItem.TKey.Create(const ID: TSnippetKind);
begin
  inherited Create;
  fID := ID;
end;

function TSnippetKindViewItem.TKey.IsEqual(const Key: IViewKey): Boolean;
begin
  if not (Key is TKey) then
    Exit(False);
  Result := (Key as TKey).fID = fID;
end;

{ TInitialLetterViewItem }

constructor TInitialLetterViewItem.Create(const Letter: TInitialLetter);
begin
  inherited Create;
  fLetter := Letter;
end;

function TInitialLetterViewItem.GetDescription: string;
begin
  Result := fLetter.Letter;
end;

function TInitialLetterViewItem.GetInitialLetter: TInitialLetter;
begin
  Result := fLetter;
end;

function TInitialLetterViewItem.GetKey: IViewKey;
begin
  Result := TKey.Create(fLetter);
end;

function TInitialLetterViewItem.IsEqual(View: IView): Boolean;
var
  LetterView: IInitialLetterView;
begin
  if not Supports(View, IInitialLetterView, LetterView) then
    Exit(False);
  // todo: change following to do comparison on record rather than char.
  Result := fLetter.Letter =
    LetterView.InitialLetter.Letter;
end;

{ TInitialLetterViewItem.TKey }

constructor TInitialLetterViewItem.TKey.Create(const ID: TInitialLetter);
begin
  inherited Create;
  fID := ID;
end;

function TInitialLetterViewItem.TKey.IsEqual(const Key: IViewKey): Boolean;
begin
  if not (Key is TKey) then
    Exit(False);
  Result := (Key as TKey).fID = fID;
end;

{ TViewItemFactory }

class function TViewItemFactory.Clone(View: IView): IView;
begin
  if not Assigned(View) or Supports(View, INulView) then
    Result := CreateNulView // also created if View = nil
  else if Supports(View, IStartPageView) then
    Result := CreateStartPageView
  else if Supports(View, ISnippetView) then
    Result := CreateSnippetView((View as ISnippetView).Snippet)
  else if Supports(View, ICategoryView) then
    Result := CreateCategoryView((View as ICategoryView).Category)
  else if Supports(View, ISnippetKindView) then
    Result := CreateSnippetKindView((View as ISnippetKindView).KindInfo)
  else if Supports(View, IInitialLetterView) then
    Result := CreateInitialLetterView(
      (View as IInitialLetterView).InitialLetter
    )
  else
    raise EBug.CreateFmt(
      '%s.CreateCopy: View does not support a valid interface', [ClassName]
    );
end;

class function TViewItemFactory.CreateCategoryView(const Category: TCategory):
  IView;
begin
  Result := TCategoryViewItem.Create(Category);
end;

class function TViewItemFactory.CreateInitialLetterView(
  const Letter: TInitialLetter): IView;
begin
  Result := TInitialLetterViewItem.Create(Letter);
end;

class function TViewItemFactory.CreateNulView: IView;
begin
  Result := TNulViewItem.Create;
end;

class function TViewItemFactory.CreateSnippetKindView(
  const KindInfo: TSnippetKindInfo): IView;
begin
  Result := TSnippetKindViewItem.Create(KindInfo);
end;

class function TViewItemFactory.CreateSnippetView(
  const Snippet: TRoutine): IView;
begin
  Result := TSnippetViewItem.Create(Snippet);
end;

class function TViewItemFactory.CreateStartPageView: IView;
begin
  Result := TStartPageViewItem.Create;
end;

end.

