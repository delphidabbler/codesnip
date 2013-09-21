{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * A set of classes that wrap various collections controls to manage a
 * relationship between a collection of values and their descriptions that
 * appear in the control.
}


unit CS.UI.Helper.CollectionCtrlKVMgr;

interface

uses
  // Delphi
  Generics.Defaults,
  Generics.Collections,
  StdCtrls,
  // Delphi Collections Library
  Collections.Base,
  Collections.Lists,
  // Project
  UExceptions;

type
  TCollectionCtrlAdapter = class abstract (TObject)
  public  
    function SelectedItemIndex: Integer; virtual; abstract;
    procedure SelectItem(const Idx: Integer); virtual; abstract;
    procedure DeleteItem(const Idx: Integer); virtual; abstract;
    procedure InsertItem(const Idx: Integer; const Value: string); virtual; 
      abstract;
    procedure Clear; virtual; abstract;
    procedure BeginUpdate; virtual; abstract;
    procedure EndUpdate; virtual; abstract;
  end;

  TListBoxAdapter = class(TCollectionCtrlAdapter)
  strict private
    fListBox: TCustomListBox;
  public  
    constructor Create(const AListBox: TCustomListBox);
    function SelectedItemIndex: Integer; override;
    procedure SelectItem(const Idx: Integer); override;
    procedure DeleteItem(const Idx: Integer); override;
    procedure InsertItem(const Idx: Integer; const Value: string); override;
    procedure Clear; override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
  end;

  TComboBoxAdapter = class(TCollectionCtrlAdapter)
  strict private
    fComboBox: TCustomComboBox;
  public  
    constructor Create(const AComboBox: TCustomComboBox);
    function SelectedItemIndex: Integer; override;
    procedure SelectItem(const Idx: Integer); override;
    procedure DeleteItem(const Idx: Integer); override;
    procedure InsertItem(const Idx: Integer; const Value: string); override;
    procedure Clear; override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
  end;

  TAbstractCollectionCtrlKVMgr<TKey> = class abstract (TObject)
  strict private
    var
      fKeyEqualFn: TEqualityComparison<TKey>;
      fCollectionCtrl: TCollectionCtrlAdapter;
      fOwnsCollectionCtrl: Boolean;
    function IndexOfKey(const AKey: TKey): Integer;
  strict protected
    function GetList: IList<TPair<TKey,string>>; virtual; abstract;
    function GetIndexedList: IEnexIndexedCollection<TPair<TKey,string>>; 
      virtual; abstract;
  public
    constructor Create(const ACollectionCtrl: TCollectionCtrlAdapter;
      const AOwnsCollectionCtrl: Boolean;
      const AKeyEqualFn: TEqualityComparison<TKey>);
    destructor Destroy; override;
    function GetSelected: TKey;
    function TryGetSelected(out AKey: TKey): Boolean;
    function GetSelectedDef(const Default: TKey): TKey;
    function HasSelection: Boolean;
    function ContainsKey(const AKey: TKey): Boolean;
    function Count: Integer;
    function Empty: Boolean;
    procedure Select(const AKey: TKey);
    procedure Delete(const AKey: TKey);
    procedure Add(const AKey: TKey; const AStr: string);
    procedure Clear;
  end;

  TSortedCollectionCtrlKVMgr<TKey> = class(TAbstractCollectionCtrlKVMgr<TKey>)
  public
    type
      TSortType = (stRespectCase, stIgnoreCase);
  strict private
    var
      fKVList: TSortedList<TPair<TKey,string>>;
      fListIntf: IList<TPair<TKey,string>>;
      fIndexedListIntf: IEnexIndexedCollection<TPair<TKey,string>>;
      fSortType: TSortType;
  strict protected
    function GetList: IList<TPair<TKey,string>>; override;
    function GetIndexedList: IEnexIndexedCollection<TPair<TKey,string>>;
      override;
  public
    constructor Create(const ACollectionCtrl: TCollectionCtrlAdapter; 
      const AOwnsCollectionCtrl: Boolean; 
      const AKeyEqualFn: TEqualityComparison<TKey>; const ASortType: TSortType);
    destructor Destroy; override;
  end;

  TUnsortedCollectionCtrlKVMgr<TKey> = class(TAbstractCollectionCtrlKVMgr<TKey>)
  strict private
    var
      fKVList: TList<TPair<TKey,string>>;
      fListIntf: IList<TPair<TKey,string>>;
      fIndexedListIntf: IEnexIndexedCollection<TPair<TKey,string>>;
  strict protected
    function GetList: IList<TPair<TKey,string>>; override;
    function GetIndexedList: IEnexIndexedCollection<TPair<TKey,string>>; 
      override;
  public
    constructor Create(const ACollectionCtrl: TCollectionCtrlAdapter; 
      const AOwnsCollectionCtrl: Boolean;
      const AKeyEqualFn: TEqualityComparison<TKey>);
    destructor Destroy; override;
  end;

  ECollectionCtrlKVMgrError = class(EBug);

implementation

uses
  SysUtils,
  Classes,
  CS.Utils.Hashes,
  UStrUtils;

{ TListBoxAdapter }

procedure TListBoxAdapter.BeginUpdate;
begin
  fListBox.Items.BeginUpdate;
end;

procedure TListBoxAdapter.Clear;
begin
  fListBox.Clear;
end;

constructor TListBoxAdapter.Create(const AListBox: TCustomListBox);
begin
  Assert(Assigned(AListBox), ClassName + '.Create: AListBox is nil');
  inherited Create;
  fListBox := AListBox;
end;

procedure TListBoxAdapter.DeleteItem(const Idx: Integer);
begin
  fListBox.Items.Delete(Idx);
end;

procedure TListBoxAdapter.EndUpdate;
begin
  fListBox.Items.EndUpdate;
end;

procedure TListBoxAdapter.InsertItem(const Idx: Integer; const Value: string);
begin
  fListBox.Items.Insert(Idx, Value);
end;

function TListBoxAdapter.SelectedItemIndex: Integer;
begin
  Result := fListBox.ItemIndex;
end;

procedure TListBoxAdapter.SelectItem(const Idx: Integer);
begin
  fListBox.ItemIndex := Idx;
end;

{ TComboBoxAdapter }

procedure TComboBoxAdapter.BeginUpdate;
begin
  fComboBox.Items.BeginUpdate;
end;

procedure TComboBoxAdapter.Clear;
begin
  fComboBox.Clear;
end;

constructor TComboBoxAdapter.Create(const AComboBox: TCustomComboBox);
begin
  Assert(Assigned(AComboBox), ClassName + '.Create: AComboBox is nil');
  inherited Create;
  fComboBox := AComboBox;
end;

procedure TComboBoxAdapter.DeleteItem(const Idx: Integer);
begin
  fComboBox.Items.Delete(Idx);
end;

procedure TComboBoxAdapter.EndUpdate;
begin
  fComboBox.Items.EndUpdate;
end;

procedure TComboBoxAdapter.InsertItem(const Idx: Integer; const Value: string);
begin
  fComboBox.Items.Insert(Idx, Value);
end;

function TComboBoxAdapter.SelectedItemIndex: Integer;
begin
  Result := fComboBox.ItemIndex;
end;

procedure TComboBoxAdapter.SelectItem(const Idx: Integer);
begin
  fComboBox.ItemIndex := Idx;
end;

{ TAbstractCollectionCtrlKVMgr<TKey> }

procedure TAbstractCollectionCtrlKVMgr<TKey>.Add(const AKey: TKey; 
  const AStr: string);
var
  KVPair: TPair<TKey,string>;
  Idx: Integer;
begin
  KVPair := TPair<TKey,string>.Create(AKey, AStr);
  GetList.Add(KVPair);
  fCollectionCtrl.BeginUpdate;
  try
    fCollectionCtrl.InsertItem(GetList.IndexOf(KVPair), AStr);
  finally
    fCollectionCtrl.EndUpdate;
  end;
end;

procedure TAbstractCollectionCtrlKVMgr<TKey>.Clear;
begin
  GetList.Clear;
  fCollectionCtrl.BeginUpdate;
  try
    fCollectionCtrl.Clear;
  finally
    fCollectionCtrl.EndUpdate;
  end;
end;

function TAbstractCollectionCtrlKVMgr<TKey>.ContainsKey(const AKey: TKey): 
  Boolean;
begin
  Result := IndexOfKey(AKey) >= 0;
end;

function TAbstractCollectionCtrlKVMgr<TKey>.Count: Integer;
begin
  Result := GetList.Count;
end;

constructor TAbstractCollectionCtrlKVMgr<TKey>.Create(
  const ACollectionCtrl: TCollectionCtrlAdapter; 
  const AOwnsCollectionCtrl: Boolean; 
  const AKeyEqualFn: TEqualityComparison<TKey>);
begin
  inherited Create;
  fCollectionCtrl := ACollectionCtrl;
  fOwnsCollectionCtrl := AOwnsCollectionCtrl;
  fKeyEqualFn := AKeyEqualFn;
end;

procedure TAbstractCollectionCtrlKVMgr<TKey>.Delete(const AKey: TKey);
var
  Idx: Integer;
begin
  Idx := IndexOfKey(AKey);
  if Idx = -1 then
    Exit;
  fCollectionCtrl.BeginUpdate;
  try
    fCollectionCtrl.DeleteItem(Idx);
  finally
    fCollectionCtrl.EndUpdate;
  end;
  GetList.RemoveAt(Idx);
end;

destructor TAbstractCollectionCtrlKVMgr<TKey>.Destroy;
begin
  fCollectionCtrl.Clear;
  if fOwnsCollectionCtrl then
    fCollectionCtrl.Free;
  inherited;
end;

function TAbstractCollectionCtrlKVMgr<TKey>.Empty: Boolean;
begin
  Result := GetList.Empty;
end;

function TAbstractCollectionCtrlKVMgr<TKey>.GetSelected: TKey;
begin
  if not TryGetSelected(Result) then
    raise ECollectionCtrlKVMgrError.Create('No items selected');
end;

function TAbstractCollectionCtrlKVMgr<TKey>.GetSelectedDef(
  const Default: TKey): TKey;
begin
  if not TryGetSelected(Result) then
    Result := Default;
end;

function TAbstractCollectionCtrlKVMgr<TKey>.HasSelection: Boolean;
begin
  Result := fCollectionCtrl.SelectedItemIndex >= 0;
end;

function TAbstractCollectionCtrlKVMgr<TKey>.IndexOfKey(const AKey: TKey): 
  Integer;
var
  Idx: Integer;
begin
  for Idx := 0 to Pred(GetList.Count) do
    if fKeyEqualFn(GetIndexedList[Idx].Key, AKey) then
      Exit(Idx);
  Result := -1;
end;

procedure TAbstractCollectionCtrlKVMgr<TKey>.Select(const AKey: TKey);
var
  Idx: Integer;
begin
  Idx := IndexOfKey(AKey);
  fCollectionCtrl.SelectItem(Idx);
end;

function TAbstractCollectionCtrlKVMgr<TKey>.TryGetSelected(out AKey: TKey): 
  Boolean;
var
  Idx: Integer;
begin
  Idx := fCollectionCtrl.SelectedItemIndex;
  if Idx = -1 then
    Exit(False);
  AKey := GetIndexedList[Idx].Key;
  Result := True;
end;

{ TSortedCollectionCtrlKVMgr<TKey> }

constructor TSortedCollectionCtrlKVMgr<TKey>.Create(
  const ACollectionCtrl: TCollectionCtrlAdapter;
  const AOwnsCollectionCtrl: Boolean;
  const AKeyEqualFn: TEqualityComparison<TKey>; const ASortType: TSortType);
var
  CompareFn: TComparison<TPair<TKey,string>>;
  EqualsFn: TEqualityComparison<TPair<TKey,string>>;
  HashFn: THasher<TPair<TKey,string>>;
begin
  inherited Create(ACollectionCtrl, AOwnsCollectionCtrl, AKeyEqualFn);
  fSortType := ASortType;
  CompareFn := nil;
  HashFn := nil;
  case ASortType of
    stRespectCase:
    begin
      CompareFn := function (const Left, Right: TPair<TKey,string>): Integer
        begin     
          Result := StrCompareStr(Left.Value, Right.Value);
        end;
      HashFn := function (const Value: TPair<TKey,string>): Integer
        begin
          Result := Integer(PaulLarsonHash(Value.Value));
        end;
    end;
    stIgnoreCase:
    begin
      CompareFn := function (const Left, Right: TPair<TKey,string>): Integer
        begin     
          Result := StrCompareText(Left.Value, Right.Value);
        end;
      HashFn := function (const Value: TPair<TKey,string>): Integer
        begin
          Result := Integer(PaulLarsonHash(StrToUpper(Value.Value)));
        end;
    end;
  end;
  Assert(Assigned(CompareFn) and Assigned(HashFn), 
    ClassName + '.Create: Invalid sort type');
  EqualsFn := function (const Left, Right: TPair<TKey,string>): Boolean
    begin 
      Result := CompareFn(Left, Right) = 0;
    end;
  fKVList := TSortedList<TPair<TKey,string>>.Create(
    TRules<TPair<TKey,string>>.Create(
      TDelegatedComparer<TPair<TKey,string>>.Create(CompareFn),
      TDelegatedEqualityComparer<TPair<TKey,string>>.Create(EqualsFn, HashFn)
    )
  );
  // following assignment to interface type mean that fKVList will be freed
  // automatically when reference count hits zero: don't free explicitly
  fListIntf := fKVList;
  fIndexedListIntf := fKVList;  
end;

function TSortedCollectionCtrlKVMgr<TKey>.GetIndexedList: 
  IEnexIndexedCollection<TPair<TKey,string>>; 
begin
  Result := fIndexedListIntf;
end;

function TSortedCollectionCtrlKVMgr<TKey>.GetList: IList<TPair<TKey,string>>; 
begin
  Result := fListIntf;
end;
  
destructor TSortedCollectionCtrlKVMgr<TKey>.Destroy;
begin
  inherited;
end;

{ TUnsortedCollectionCtrlKVMgr<TKey> }

constructor TUnsortedCollectionCtrlKVMgr<TKey>.Create(
  const ACollectionCtrl: TCollectionCtrlAdapter;
  const AOwnsCollectionCtrl: Boolean; 
  const AKeyEqualFn: TEqualityComparison<TKey>);
begin
  inherited Create(ACollectionCtrl, AOwnsCollectionCtrl, AKeyEqualFn);
  fKVList := TList<TPair<TKey,string>>.Create(
    TRules<TPair<TKey,string>>.Default
  );
  // following assignment to interface type mean that fKVList will be freed
  // automatically when reference count hits zero: don't free explicitly
  fListIntf := fKVLIst;
  fIndexedListIntf := fKVList;
end;

destructor TUnsortedCollectionCtrlKVMgr<TKey>.Destroy;
begin
  inherited;
end;

function TUnsortedCollectionCtrlKVMgr<TKey>.GetIndexedList: 
  IEnexIndexedCollection<TPair<TKey, string>>;
begin
  Result := fIndexedListIntf;
end;

function TUnsortedCollectionCtrlKVMgr<TKey>.GetList: IList<TPair<TKey, string>>;
begin
  Result := fListIntf;
end;

end.

