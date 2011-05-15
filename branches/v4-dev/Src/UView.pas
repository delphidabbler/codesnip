{
 * UView.pas
 *
 * Classes that encapsulate and provide information about "view items" that are
 * displayed in the user interface.
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
 * Portions created by the Initial Developer are Copyright (C) 2005-2011 Peter
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


type
  ///  <summary>
  ///  Interface supported by object that can compare key values associated with
  ///  a view for equality.
  ///  </summary>
  ///  <remarks>
  ///  The values stored in this object must persist even if any object stored
  ///  in the related IView object has been destroyed.
  ///  </remarks>
  IViewKey = interface(IInterface)
    ['{E6C39F3C-63E3-4A3E-A4E7-B176DA121D9E}']
    ///  <summary>Checks if two view keys are equal.</summary>
    function IsEqual(const Key: IViewKey): Boolean;
  end;

type
  ///  <summary>
  ///  Interface supported by all view objects.
  ///  </summary>
  IView = interface(IInterface)
    ['{707ED3DC-1738-4B79-B1BE-1C554572097E}']
    ///  <summary>Checks if two view items are equal, i.e. they have the same
    ///  "type" and any properties have the the same values.</summary>
    function IsEqual(View: IView): Boolean;
    ///  <summary>Gets description of view.</summary>
    function GetDescription: string;
    ///  <summary>Description of view.</summary>
    property Description: string read GetDescription;
    ///  <summary>Gets object containing view's unique key.</summary>
    ///  <remarks>Encapsulates the data that uniquely identifies the view item
    ///  without having have an instance of any object wrapped by the view item.
    ///  </remarks>
    function GetKey: IViewKey;
    ///  <summary>Checks if view is user-defined.</summary>
    function IsUserDefined: Boolean;
  end;

type
  ///  <summary>
  ///  Interface supported by nul view.
  ///  </summary>
  INulView = interface(IView)
    ['{18D06867-2E6D-4B62-A0A5-A269C91D18D6}']
  end;

type
  ///  <summary>
  ///  Interface supported by start page view.
  ///  </summary>
  IStartPageView = interface(IView)
    ['{9D2208C4-8FFC-4532-8C3F-07514EDFAB9D}']
  end;

type
  ///  <summary>
  ///  Interface supported by snippet views.
  ///  </summary>
  ISnippetView = interface(IView)
    ['{E0BD3AB7-2CB8-4B07-84D9-D7625DD020B0}']
    ///  <summary>Gets reference to snippet associated with view.</summary>
    function GetSnippet: TSnippet;
    ///  <summary>Snippet associated with view.</summary>
    property Snippet: TSnippet read GetSnippet;
  end;

type
  ///  <summary>
  ///  Interface supported by category views.
  ///  </summary>
  ICategoryView = interface(IView)
    ['{C8BB04FD-B0B7-42F9-84AA-2F7FDD3A6FE8}']
    ///  <summary>Gets reference to category associated with view.</summary>
    function GetCategory: TCategory;
    ///  <summary>Category associated with view.</summary>
    property Category: TCategory read GetCategory;
  end;

type
  ///  <summary>
  ///  Interface supported by snippet kind views.
  ///  </summary>
  ISnippetKindView = interface(IView)
    ['{FE074E72-857F-4D43-A17D-A951830B78E4}']
    ///  <summary>Gets info about snippet kind associated with view.</summary>
    function GetKindInfo: TSnippetKindInfo;
    ///  <summary>Info about snippet kind associated with view.</summary>
    property KindInfo: TSnippetKindInfo read GetKindInfo;
  end;

type
  ///  <summary>
  ///  Interface supported by initial letter views.
  ///  </summary>
  IInitialLetterView = interface(IView)
    ['{E589E3E9-7178-4FF8-BDEF-D6F7DC9FEB85}']
    ///  <summary>Gets info about initial letter associated with view.</summary>
    function GetInitialLetter: TInitialLetter;
    ///  <summary>Info about initial letter associated with view.</summary>
    property InitialLetter: TInitialLetter read GetInitialLetter;
  end;

type
  ///  <summary>
  ///  List of view items.
  ///  </summary>
  TViewItemList = class sealed(TList<IView>);

type
  ///  <summary>
  ///  Static factory class used to create view instances of different types.
  ///  </summary>
  TViewItemFactory = class sealed(TNoConstructObject)
  public
    ///  <summary>Creates a copy of the given view. Copy has same type and
    ///  properties.</summary>
    class function Clone(View: IView): IView;
    ///  <summary>Creates a nul view instance.</summary>
    class function CreateNulView: IView;
    ///  <summary>Creates a start page view instance.</summary>
    class function CreateStartPageView: IView;
    ///  <summary>Creates a snippet view instance associated with a given
    ///  snippet.</summary>
    class function CreateSnippetView(const Snippet: TSnippet): IView;
    ///  <summary>Creates a category view instance associated with a given
    ///  category.</summary>
    class function CreateCategoryView(const Category: TCategory): IView;
    ///  <summary>Creates a snippet kind view instance associated with a given
    ///  snippet kind.</summary>
    class function CreateSnippetKindView(const KindInfo: TSnippetKindInfo):
      IView;
    ///  <summary>Creates an initial letter view instance associated with a
    ///  given letter.</summary>
    class function CreateInitialLetterView(const Letter: TInitialLetter): IView;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UExceptions, USnippetIDs;


type
  ///  <summary>Nul view.</summary>
  TNulViewItem = class sealed(TInterfacedObject,
    IView, INulView
  )
  strict private
    type
      ///  <summary>Implementation of IViewKey for nul view.</summary>
      TKey = class(TInterfacedObject, IViewKey)
      public
        ///  <summary>Checks if this key is equal to one passed as a parameter.
        ///  </summary>
        function IsEqual(const Key: IViewKey): Boolean;
      end;
  public
    { IView methods }
    ///  <summary>Checks if this view item is equal to the one passed as a
    ///  parameter.</summary>
    function IsEqual(View: IView): Boolean;
    ///  <summary>Gets description of view.</summary>
    function GetDescription: string;
    ///  <summary>Gets object containing view's unique key.</summary>
    function GetKey: IViewKey;
    ///  <summary>Checks if view is user-defined.</summary>
    function IsUserDefined: Boolean;
  end;

type
  ///  <summary>View associated with start page.</summary>
  TStartPageViewItem = class sealed(TInterfacedObject,
    IView, IStartPageView
  )
  strict private
    type
      ///  <summary>Implementation of IViewKey for start page view.</summary>
      TKey = class(TInterfacedObject, IViewKey)
      public
        ///  <summary>Checks if this key is equal to one passed as a parameter.
        ///  </summary>
        function IsEqual(const Key: IViewKey): Boolean;
      end;
  public
    { IView methods }
    ///  <summary>Checks if this view item is equal to the one passed as a
    ///  parameter.</summary>
    function IsEqual(View: IView): Boolean;
    ///  <summary>Gets description of view.</summary>
    function GetDescription: string;
    ///  <summary>Gets object containing view's unique key.</summary>
    function GetKey: IViewKey;
    ///  <summary>Checks if view is user-defined.</summary>
    function IsUserDefined: Boolean;
  end;

type
  ///  <summary>View associated with a snippet.</summary>
  TSnippetViewItem = class sealed(TInterfacedObject,
    IView, ISnippetView
  )
  strict private
    var
      ///  <summary>Snippet associated with view.</summary>
      fSnippet: TSnippet;
    type
      ///  <summary>Implementation of IViewKey for snippet view.</summary>
      TKey = class(TInterfacedObject, IViewKey)
      strict private
        var
          ///  <summary>Snippet id used as key.</summary>
          fID: TSnippetID;
      public
        ///  <summary>Constructs object with given snippet id as key.</summary>
        constructor Create(const ID: TSnippetID);
        ///  <summary>Checks if this key is equal to one passed as a parameter.
        ///  </summary>
        function IsEqual(const Key: IViewKey): Boolean;
      end;
  public
    ///  <summary>Constructs view for a specified snippet.</summary>
    constructor Create(const Snippet: TSnippet);
    { IView methods }
    ///  <summary>Checks if this view item is equal to the one passed as a
    ///  parameter.</summary>
    function IsEqual(View: IView): Boolean;
    ///  <summary>Gets description of view.</summary>
    function GetDescription: string;
    ///  <summary>Gets object containing view's unique key.</summary>
    function GetKey: IViewKey;
    ///  <summary>Checks if view is user-defined.</summary>
    function IsUserDefined: Boolean;
    { ISnippetView methods }
    ///  <summary>Gets reference to snippet associated with view.</summary>
    function GetSnippet: TSnippet;
  end;

type
  ///  <summary>View associated with a category.</summary>
  TCategoryViewItem = class sealed(TInterfacedObject,
    IView, ICategoryView
  )
  strict private
    var
      ///  <summary>Category associated with view.</summary>
      fCategory: TCategory;
    type
      ///  <summary>Implementation of IViewKey for category view item.</summary>
      TKey = class(TInterfacedObject, IViewKey)
      strict private
        var
          ///  <summary>Category id used as key.</summary>
          fID: string;
      public
        ///  <summary>Constructs object with given category id as key.</summary>
        constructor Create(const ID: string);
        ///  <summary>Checks if this key is equal to one passed as a parameter.
        ///  </summary>
        function IsEqual(const Key: IViewKey): Boolean;
      end;
  public
    ///  <summary>Constructs view for a specified category.</summary>
    constructor Create(const Category: TCategory);
    { IView methods }
    ///  <summary>Checks if this view item is equal to the one passed as a
    ///  parameter.</summary>
    function IsEqual(View: IView): Boolean;
    ///  <summary>Gets description of view.</summary>
    function GetDescription: string;
    ///  <summary>Gets object containing view's unique key.</summary>
    function GetKey: IViewKey;
    ///  <summary>Checks if view is user-defined.</summary>
    function IsUserDefined: Boolean;
    { ICategoryView methods }
    ///  <summary>Gets reference to category associated with view.</summary>
    function GetCategory: TCategory;
  end;

type
  ///  <summary>View associated with a snippet kind grouping.</summary>
  TSnippetKindViewItem = class sealed(TInterfacedObject,
    IView, ISnippetKindView
  )
  strict private
    var
      ///  <summary>Snippet kind associated with view.</summary>
      fKindInfo: TSnippetKindInfo;
    type
      ///  <summary>Implementation of IViewKey for snippet kind view item.
      ///  </summary>
      TKey = class(TInterfacedObject, IViewKey)
      strict private
        var
          ///  <summary>Snippet kind used as key.</summary>
          fID: TSnippetKind;
      public
        ///  <summary>Constructs object with given snippet kind as key.
        ///  </summary>
        constructor Create(const ID: TSnippetKind);
        ///  <summary>Checks if this key is equal to one passed as a parameter.
        ///  </summary>
        function IsEqual(const Key: IViewKey): Boolean;
      end;
  public
    ///  <summary>Constructs view for a specified snippet kind.</summary>
    constructor Create(const KindInfo: TSnippetKindInfo);
    { IView methods }
    ///  <summary>Checks if this view item is equal to the one passed as a
    ///  parameter.</summary>
    function IsEqual(View: IView): Boolean;
    ///  <summary>Gets description of view.</summary>
    function GetDescription: string;
    ///  <summary>Gets object containing view's unique key.</summary>
    function GetKey: IViewKey;
    ///  <summary>Checks if view is user-defined.</summary>
    function IsUserDefined: Boolean;
    { ISnippetKindView methods }
    ///  <summary>Gets info about snippet kind associated with view.</summary>
    function GetKindInfo: TSnippetKindInfo;
  end;

type
  ///  <summary>View associated with an initial letter grouping.</summary>
  TInitialLetterViewItem = class sealed(TInterfacedObject,
    IView, IInitialLetterView
  )
  strict private
    var
      ///  <summary>Initial letter associated with view.</summary>
      fInitialLetter: TInitialLetter;
    type
      ///  <summary>Implementation of IViewKey for initial letter view item.
      ///  </summary>
      TKey = class(TInterfacedObject, IViewKey)
      strict private
        var
          ///  <summary>Initial letter used as key.</summary>
          fID: TInitialLetter;
      public
        ///  <summary>Constructs object with given initial letter as key.
        ///  </summary>
        constructor Create(const ID: TInitialLetter);
        ///  <summary>Checks if this key is equal to one passed as a parameter.
        ///  </summary>
        function IsEqual(const Key: IViewKey): Boolean;
      end;
  public
    ///  <summary>Constructs view for a specified initial letter.</summary>
    constructor Create(const Letter: TInitialLetter);
    { IView methods }
    ///  <summary>Checks if this view item is equal to the one passed as a
    ///  parameter.</summary>
    function IsEqual(View: IView): Boolean;
    ///  <summary>Gets description of view.</summary>
    function GetDescription: string;
    ///  <summary>Gets object containing view's unique key.</summary>
    function GetKey: IViewKey;
    ///  <summary>Checks if view is user-defined.</summary>
    function IsUserDefined: Boolean;
    { IInitialLetterView methods }
    ///  <summary>Gets unfo about initial letter associated with view.</summary>
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

function TNulViewItem.IsUserDefined: Boolean;
begin
  Result := False;
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

function TStartPageViewItem.IsUserDefined: Boolean;
begin
  Result := False;
end;

{ TStartPageViewItem.TKey }

function TStartPageViewItem.TKey.IsEqual(const Key: IViewKey): Boolean;
begin
  Result := Key is TKey;
end;

{ TSnippetViewItem }

constructor TSnippetViewItem.Create(const Snippet: TSnippet);
begin
  inherited Create;
  fSnippet := Snippet;
end;

function TSnippetViewItem.GetDescription: string;
begin
  Result := GetSnippet.Name;
end;

function TSnippetViewItem.GetKey: IViewKey;
begin
  Result := TKey.Create(GetSnippet.ID);
end;

function TSnippetViewItem.GetSnippet: TSnippet;
begin
  Result := fSnippet;
end;

function TSnippetViewItem.IsEqual(View: IView): Boolean;
var
  SnippetView: ISnippetView;
begin
  if not Supports(View, ISnippetView, SnippetView) then
    Exit(False);
  Result := GetSnippet.IsEqual(SnippetView.Snippet);
end;

function TSnippetViewItem.IsUserDefined: Boolean;
begin
  Result := GetSnippet.UserDefined;
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
  Result := GetCategory.Description;
end;

function TCategoryViewItem.GetKey: IViewKey;
begin
  Result := TKey.Create(GetCategory.Category);
end;

function TCategoryViewItem.IsEqual(View: IView): Boolean;
var
  CatView: ICategoryView;
begin
  if not Supports(View, ICategoryView, CatView) then
    Exit(False);
  Result := GetCategory.IsEqual(CatView.Category);
end;

function TCategoryViewItem.IsUserDefined: Boolean;
begin
  Result := GetCategory.UserDefined;
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
  Result := GetKindInfo.DisplayName;
end;

function TSnippetKindViewItem.GetKey: IViewKey;
begin
  Result := TKey.Create(GetKindInfo.Kind);
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
  Result := GetKindInfo.Kind = SnipKindView.KindInfo.Kind;
end;

function TSnippetKindViewItem.IsUserDefined: Boolean;
begin
  Result := False;
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
  fInitialLetter := Letter;
end;

function TInitialLetterViewItem.GetDescription: string;
begin
  Result := GetInitialLetter.Letter;
end;

function TInitialLetterViewItem.GetInitialLetter: TInitialLetter;
begin
  Result := fInitialLetter;
end;

function TInitialLetterViewItem.GetKey: IViewKey;
begin
  Result := TKey.Create(GetInitialLetter);
end;

function TInitialLetterViewItem.IsEqual(View: IView): Boolean;
var
  LetterView: IInitialLetterView;
begin
  if not Supports(View, IInitialLetterView, LetterView) then
    Exit(False);
  Result := GetInitialLetter = LetterView.InitialLetter;
end;

function TInitialLetterViewItem.IsUserDefined: Boolean;
begin
  Result := False;
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
  const Snippet: TSnippet): IView;
begin
  Result := TSnippetViewItem.Create(Snippet);
end;

class function TViewItemFactory.CreateStartPageView: IView;
begin
  Result := TStartPageViewItem.Create;
end;

end.

