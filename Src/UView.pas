{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Classes that encapsulate and provide information about "views" that are
 * displayed in the user interface.
}


unit UView;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  DB.Categories,
  DB.SnippetKind,
  DB.Snippets,
  UBaseObjects,
  UInitialLetter;


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
    ///  <summary>Checks if two viewsare equal, i.e. they have the same "type"
    ///  and any properties have the the same values.</summary>
    function IsEqual(View: IView): Boolean;
    ///  <summary>Gets description of view.</summary>
    function GetDescription: string;
    ///  <summary>Description of view.</summary>
    property Description: string read GetDescription;
    ///  <summary>Gets object containing view's unique key.</summary>
    ///  <remarks>Encapsulates the data that uniquely identifies the view
    ///  without having have an instance of any object wrapped by the view.
    ///  </remarks>
    function GetKey: IViewKey;
    ///  <summary>Checks if view is a grouping.</summary>
    ///  <remarks>A grouping is a view that groups views together.
    ///  </remarks>
    function IsGrouping: Boolean;
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
  ///  Interface supported by new tab page view.
  ///  </summary>
  INewTabView = interface(IView)
    ['{EF760E9C-2DEE-4CCA-AB85-E6C59AA100E7}']
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
  ///  Interface supported by view that provides info about a database update.
  ///  </summary>
  IDBUpdateInfoView = interface(IView)
    ['{0E5220F3-33EE-4832-9533-B97B3F3E519B}']
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
  ///  List of views.
  ///  </summary>
  TViewList = class sealed(TList<IView>);

type
  ///  <summary>
  ///  Static factory class used to create view instances of different types.
  ///  </summary>
  TViewFactory = class sealed(TNoConstructObject)
  public
    ///  <summary>Creates a copy of the given view. Copy has same type and
    ///  properties.</summary>
    class function Clone(View: IView): IView;
    ///  <summary>Creates a nul view instance.</summary>
    class function CreateNulView: IView;
    ///  <summary>Creates a new tab view instance.</summary>
    class function CreateNewTabView: IView;
    ///  <summary>Creates a start page view instance.</summary>
    class function CreateStartPageView: IView;
    ///  <summary>Creates a database update information view instance.
    ///  </summary>
    class function CreateDBUpdateInfoView: IView;
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
    ///  <summary>Creates a view of given database object. View type depends on
    ///  the object's type.</summary>
    ///  <remarks>Database object type must be valid, i.e. either TSnippet or
    ///  TCategory, or nil.</remarks>
    class function CreateDBItemView(const DBObj: TObject): IView;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  DB.Main,
  DB.SnippetIDs,
  UExceptions,
  UStrUtils;

type
  ///  <summary>
  ///  Base class for views that contain no other object references and are used
  ///  simply for display in the details pane.
  ///  </summary>
  TSimpleView = class abstract(TInterfacedObject)
  strict private
    type
      ///  <summary>Implementation of IViewKey for descendants.</summary>
      TKey = class(TInterfacedObject, IViewKey)
      strict private
        ///  <summary>Class of object that owns this key.</summary>
        fOwnerClass: TClass;
      public
        ///  <summary>Constructs object with given owner class.</summary>
        constructor Create(OwnerClass: TClass);
        ///  <summary>Checks if this key is equal to one passed as a parameter.
        ///  </summary>
        ///  <remarks>Method of IViewKey.</remarks>
        function IsEqual(const Key: IViewKey): Boolean;
      end;
  public
    ///  <summary>Checks if this view is equal to the one passed as a parameter.
    ///  </summary>
    ///  <remarks>Method of IView.</remarks>
    function IsEqual(View: IView): Boolean;
    ///  <summary>Gets description of view.</summary>
    ///  <remarks>Method of IView.</remarks>
    function GetDescription: string; virtual; abstract;
    ///  <summary>Gets object containing view's unique key.</summary>
    ///  <remarks>Method of IView.</remarks>
    function GetKey: IViewKey;
    ///  <summary>Checks if view is a grouping.</summary>
    ///  <remarks>Method of IView.</remarks>
    function IsGrouping: Boolean;
  end;

type
  ///  <summary>Nul view.</summary>
  TNulView = class sealed(TSimpleView,
    IView, INulView
  )
  public
    ///  <summary>Gets description of view.</summary>
    ///  <remarks>Method of IView.</remarks>
    function GetDescription: string; override;
  end;

type
  ///  <summary>View associated with blank new tab pages.</summary>
  TNewTabView = class sealed(TSimpleView,
    IView, INewTabView
  )
  public
    ///  <summary>Gets description of view.</summary>
    ///  <remarks>Method of IView.</remarks>
    function GetDescription: string; override;
  end;

type
  ///  <summary>View associated with start page.</summary>
  TStartPageView = class sealed(TSimpleView,
    IView, IStartPageView
  )
  public
    ///  <summary>Gets description of view.</summary>
    ///  <remarks>Method of IView.</remarks>
    function GetDescription: string; override;
  end;

type
  ///  <summary>View associated with information about a database update.
  ///  </summary>
  TDBUpdateInfoView = class sealed(TSimpleView,
    IView, IDBUpdateInfoView
  )
  public
    ///  <summary>Gets description of view.</summary>
    ///  <remarks>Method of IView.</remarks>
    function GetDescription: string; override;
  end;

type
  ///  <summary>View associated with a snippet.</summary>
  TSnippetView = class sealed(TInterfacedObject,
    IView, ISnippetView
  )
  strict private
    var
      ///  <summary>ID of snippet associated with view.</summary>
      fSnippetID: TSnippetID;
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
        ///  <remarks>Method of IViewKey.</remarks>
        function IsEqual(const Key: IViewKey): Boolean;
      end;
  public
    ///  <summary>Constructs view for a specified snippet.</summary>
    constructor Create(const Snippet: TSnippet);
    ///  <summary>Checks if this view is equal to the one passed as a parameter.
    ///  </summary>
    ///  <remarks>Method of IView.</remarks>
    function IsEqual(View: IView): Boolean;
    ///  <summary>Gets description of view.</summary>
    ///  <remarks>Method of IView.</remarks>
    function GetDescription: string;
    ///  <summary>Gets object containing view's unique key.</summary>
    ///  <remarks>Method of IView.</remarks>
    function GetKey: IViewKey;
    ///  <summary>Checks if view is a grouping.</summary>
    ///  <remarks>Method of IView.</remarks>
    function IsGrouping: Boolean;
    ///  <summary>Gets reference to snippet associated with view.</summary>
    ///  <remarks>Method of ISnippetView.</remarks>
    function GetSnippet: TSnippet;
  end;

type
  ///  <summary>View associated with a category.</summary>
  TCategoryView = class sealed(TInterfacedObject,
    IView, ICategoryView
  )
  strict private
    var
      ///  <summary>ID of category associated with view.</summary>
      fCategoryID: string;
    type
      ///  <summary>Implementation of IViewKey for category view.</summary>
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
        ///  <remarks>Method of IViewKey.</remarks>
        function IsEqual(const Key: IViewKey): Boolean;
      end;
  public
    ///  <summary>Constructs view for a specified category.</summary>
    constructor Create(const Category: TCategory);
    ///  <summary>Checks if this view is equal to the one passed as a parameter.
    ///  </summary>
    ///  <remarks>Method of IView.</remarks>
    function IsEqual(View: IView): Boolean;
    ///  <summary>Gets description of view.</summary>
    ///  <remarks>Method of IView.</remarks>
    function GetDescription: string;
    ///  <summary>Gets object containing view's unique key.</summary>
    ///  <remarks>Method of IView.</remarks>
    function GetKey: IViewKey;
    ///  <summary>Checks if view is a grouping.</summary>
    ///  <remarks>Method of IView.</remarks>
    function IsGrouping: Boolean;
    ///  <summary>Gets reference to category associated with view.</summary>
    ///  <remarks>Method of ICategoryView.</remarks>
    function GetCategory: TCategory;
  end;

type
  ///  <summary>View associated with a snippet kind grouping.</summary>
  TSnippetKindView = class sealed(TInterfacedObject,
    IView, ISnippetKindView
  )
  strict private
    var
      ///  <summary>Snippet kind associated with view.</summary>
      fKindInfo: TSnippetKindInfo;
    type
      ///  <summary>Implementation of IViewKey for snippet kind view.
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
        ///  <remarks>Method of IViewKey.</remarks>
        function IsEqual(const Key: IViewKey): Boolean;
      end;
  public
    ///  <summary>Constructs view for a specified snippet kind.</summary>
    constructor Create(const KindInfo: TSnippetKindInfo);
    ///  <summary>Checks if this view is equal to the one passed as a parameter.
    ///  </summary>
    ///  <remarks>Method of IView.</remarks>
    function IsEqual(View: IView): Boolean;
    ///  <summary>Gets description of view.</summary>
    ///  <remarks>Method of IView.</remarks>
    function GetDescription: string;
    ///  <summary>Gets object containing view's unique key.</summary>
    ///  <remarks>Method of IView.</remarks>
    function GetKey: IViewKey;
    ///  <summary>Checks if view is a grouping.</summary>
    ///  <remarks>Method of IView.</remarks>
    function IsGrouping: Boolean;
    ///  <summary>Gets info about snippet kind associated with view.</summary>
    ///  <remarks>Method of ISnippetKindView.</remarks>
    function GetKindInfo: TSnippetKindInfo;
  end;

type
  ///  <summary>View associated with an initial letter grouping.</summary>
  TInitialLetterView = class sealed(TInterfacedObject,
    IView, IInitialLetterView
  )
  strict private
    var
      ///  <summary>Initial letter associated with view.</summary>
      fInitialLetter: TInitialLetter;
    type
      ///  <summary>Implementation of IViewKey for initial letter view.
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
        ///  <remarks>Method of IViewKey.</remarks>
        function IsEqual(const Key: IViewKey): Boolean;
      end;
  public
    ///  <summary>Constructs view for a specified initial letter.</summary>
    constructor Create(const Letter: TInitialLetter);
    ///  <summary>Checks if this view is equal to the one passed as a parameter.
    ///  </summary>
    ///  <remarks>Method of IView.</remarks>
    function IsEqual(View: IView): Boolean;
    ///  <summary>Gets description of view.</summary>
    ///  <remarks>Method of IView.</remarks>
    function GetDescription: string;
    ///  <summary>Gets object containing view's unique key.</summary>
    ///  <remarks>Method of IView.</remarks>
    function GetKey: IViewKey;
    ///  <summary>Checks if view is a grouping.</summary>
    ///  <remarks>Method of IView.</remarks>
    function IsGrouping: Boolean;
    ///  <summary>Gets unfo about initial letter associated with view.</summary>
    ///  <remarks>Method of IInitialLetterView.</remarks>
    function GetInitialLetter: TInitialLetter;
  end;

{ TSimpleView }

function TSimpleView.GetKey: IViewKey;
begin
  Result := TKey.Create(Self.ClassType);
end;

function TSimpleView.IsEqual(View: IView): Boolean;
begin
  Result := View.GetKey.IsEqual(Self.GetKey);
end;

function TSimpleView.IsGrouping: Boolean;
begin
  Result := False;
end;

{ TSimpleView.TKey }

constructor TSimpleView.TKey.Create(OwnerClass: TClass);
begin
  inherited Create;
  fOwnerClass := OwnerClass;
end;

function TSimpleView.TKey.IsEqual(const Key: IViewKey): Boolean;
begin
  Result := (Key is TKey) and ((Key as TKey).fOwnerClass = Self.fOwnerClass);
end;

{ TNulView }

function TNulView.GetDescription: string;
begin
  Result := '';
end;

{ TNewTabView }

function TNewTabView.GetDescription: string;
resourcestring
  sDescription = 'Empty tab';
begin
  Result := sDescription;
end;

{ TStartPageView }

function TStartPageView.GetDescription: string;
resourcestring
  sDesc = 'Welcome';
begin
  Result := sDesc;
end;

{ TDBUpdateInfoView }

function TDBUpdateInfoView.GetDescription: string;
resourcestring
  sDesc = 'Database Updated';
begin
  Result := sDesc;
end;

{ TSnippetView }

constructor TSnippetView.Create(const Snippet: TSnippet);
begin
  inherited Create;
  fSnippetID := Snippet.ID;
end;

function TSnippetView.GetDescription: string;
begin
  Result := GetSnippet.DisplayName;
end;

function TSnippetView.GetKey: IViewKey;
begin
  Result := TKey.Create(fSnippetID);
end;

function TSnippetView.GetSnippet: TSnippet;
begin
  Result := Database.Snippets.Find(fSnippetID);
  Assert(Assigned(Result), ClassName + '.GetSnippet: Snippet not found');
end;

function TSnippetView.IsEqual(View: IView): Boolean;
var
  SnippetView: ISnippetView;
begin
  if not Supports(View, ISnippetView, SnippetView) then
    Exit(False);
  // don't compare snippet IDs directly in case snippet equality test changes
  Result := GetSnippet.IsEqual(SnippetView.Snippet);
end;

function TSnippetView.IsGrouping: Boolean;
begin
  Result := False;
end;

{ TSnippetView.TKey }

constructor TSnippetView.TKey.Create(const ID: TSnippetID);
begin
  inherited Create;
  fID := ID;
end;

function TSnippetView.TKey.IsEqual(const Key: IViewKey): Boolean;
begin
  if not (Key is TSnippetView.TKey) then
    Exit(False);
  Result := (Key as TSnippetView.TKey).fID = fID;
end;

{ TCategoryView }

constructor TCategoryView.Create(const Category: TCategory);
begin
  inherited Create;
  fCategoryID := Category.ID;
end;

function TCategoryView.GetCategory: TCategory;
begin
  Result := Database.Categories.Find(fCategoryID);
  Assert(Assigned(Result), ClassName + '.GetCategory: Category not found');
end;

function TCategoryView.GetDescription: string;
begin
  Result := GetCategory.Description;
end;

function TCategoryView.GetKey: IViewKey;
begin
  Result := TKey.Create(GetCategory.ID);
end;

function TCategoryView.IsEqual(View: IView): Boolean;
var
  CatView: ICategoryView;
begin
  if not Supports(View, ICategoryView, CatView) then
    Exit(False);
  // don't compare category IDs directly in case equality test changes
  Result := GetCategory.IsEqual(CatView.Category);
end;

function TCategoryView.IsGrouping: Boolean;
begin
  Result := True;
end;

{ TCategoryView.TKey }

constructor TCategoryView.TKey.Create(const ID: string);
begin
  inherited Create;
  fID := ID;
end;

function TCategoryView.TKey.IsEqual(const Key: IViewKey): Boolean;
begin
  if not (Key is TKey) then
    Exit(False);
  Result := StrSameText((Key as TKey).fID, fID);
end;

{ TSnippetKindView }

constructor TSnippetKindView.Create(const KindInfo: TSnippetKindInfo);
begin
  inherited Create;
  fKindInfo := KindInfo;
end;

function TSnippetKindView.GetDescription: string;
begin
  Result := GetKindInfo.DisplayName;
end;

function TSnippetKindView.GetKey: IViewKey;
begin
  Result := TKey.Create(GetKindInfo.Kind);
end;

function TSnippetKindView.GetKindInfo: TSnippetKindInfo;
begin
  Result := fKindInfo;
end;

function TSnippetKindView.IsEqual(View: IView): Boolean;
var
  SnipKindView: ISnippetKindView;
begin
  if not Supports(View, ISnippetKindView, SnipKindView) then
    Exit(False);
  Result := GetKindInfo.Kind = SnipKindView.KindInfo.Kind;
end;

function TSnippetKindView.IsGrouping: Boolean;
begin
  Result := True;
end;

{ TSnippetKindView.TKey }

constructor TSnippetKindView.TKey.Create(const ID: TSnippetKind);
begin
  inherited Create;
  fID := ID;
end;

function TSnippetKindView.TKey.IsEqual(const Key: IViewKey): Boolean;
begin
  if not (Key is TKey) then
    Exit(False);
  Result := (Key as TKey).fID = fID;
end;

{ TInitialLetterView }

constructor TInitialLetterView.Create(const Letter: TInitialLetter);
begin
  inherited Create;
  fInitialLetter := Letter;
end;

function TInitialLetterView.GetDescription: string;
begin
  Result := GetInitialLetter.Letter;
end;

function TInitialLetterView.GetInitialLetter: TInitialLetter;
begin
  Result := fInitialLetter;
end;

function TInitialLetterView.GetKey: IViewKey;
begin
  Result := TKey.Create(GetInitialLetter);
end;

function TInitialLetterView.IsEqual(View: IView): Boolean;
var
  LetterView: IInitialLetterView;
begin
  if not Supports(View, IInitialLetterView, LetterView) then
    Exit(False);
  Result := GetInitialLetter = LetterView.InitialLetter;
end;

function TInitialLetterView.IsGrouping: Boolean;
begin
  Result := True;
end;

{ TInitialLetterView.TKey }

constructor TInitialLetterView.TKey.Create(const ID: TInitialLetter);
begin
  inherited Create;
  fID := ID;
end;

function TInitialLetterView.TKey.IsEqual(const Key: IViewKey): Boolean;
begin
  if not (Key is TKey) then
    Exit(False);
  Result := (Key as TKey).fID = fID;
end;

{ TViewFactory }

class function TViewFactory.Clone(View: IView): IView;
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
  else if Supports(View, INewTabView) then
    Result := CreateNewTabView
  else if Supports(View, IDBUpdateInfoView) then
    Result := CreateDBUpdateInfoView
  else
    raise EBug.CreateFmt(
      '%s.CreateCopy: View does not support a valid interface', [ClassName]
    );
end;

class function TViewFactory.CreateCategoryView(const Category: TCategory):
  IView;
begin
  Result := TCategoryView.Create(Category);
end;

class function TViewFactory.CreateDBItemView(const DBObj: TObject): IView;
begin
  Result := nil;
  if not Assigned(DBObj) then
    Result := TViewFactory.CreateNulView
  else if DBObj is TSnippet then
    Result := TViewFactory.CreateSnippetView(DBObj as TSnippet)
  else if DBObj is TCategory then
    Result := TViewFactory.CreateCategoryView(DBObj as TCategory);
  Assert(Assigned(Result),
    ClassName + '.CreateDBItemView: DBObj is not valid type');
end;

class function TViewFactory.CreateDBUpdateInfoView: IView;
begin
  Result := TDBUpdateInfoView.Create;
end;

class function TViewFactory.CreateInitialLetterView(
  const Letter: TInitialLetter): IView;
begin
  Result := TInitialLetterView.Create(Letter);
end;

class function TViewFactory.CreateNewTabView: IView;
begin
  Result := TNewTabView.Create;
end;

class function TViewFactory.CreateNulView: IView;
begin
  Result := TNulView.Create;
end;

class function TViewFactory.CreateSnippetKindView(
  const KindInfo: TSnippetKindInfo): IView;
begin
  Result := TSnippetKindView.Create(KindInfo);
end;

class function TViewFactory.CreateSnippetView(
  const Snippet: TSnippet): IView;
begin
  Result := TSnippetView.Create(Snippet);
end;

class function TViewFactory.CreateStartPageView: IView;
begin
  Result := TStartPageView.Create;
end;

end.

