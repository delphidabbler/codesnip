{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a dialogue box that is used to select criteria for searches for
 * snippets that have specified tags.
}


unit CS.UI.Dialogs.TagsSearch;

interface

uses
  Classes,
  Controls,
  StdCtrls,
  ExtCtrls,
  CheckLst,
  CS.Database.Types,
  CS.UI.Helper.CollectionCtrlKVMgr,
  FmGenericOKDlg,
  UBaseObjects,
  USearch;

type
  TTagsSearchDlg = class(TGenericOKDlg, INoPublicConstruct)
    clbTags: TCheckListBox;
    lblTags: TLabel;
    rgLogic: TRadioGroup;
    rgScope: TRadioGroup;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  strict private
    type
      TSearchParams = class(TObject)
      strict private
        var
          fTags: ITagSet;
          fLogic: TSearchLogic;
          fUpdated: Boolean;
        procedure SetTags(ATags: ITagSet);
        procedure SetLogic(ALogic: TSearchLogic);
        procedure Load;
        procedure Save;
      public
        ///  <summary>Constructs object and initialises properties from
        ///  persistent storage.</summary>
        constructor Create;
        ///  <summary>Saves current property values to persistent storage then
        ///  destroys instance.</summary>
        destructor Destroy; override;
        ///  <summary>Set of tags to include in search.</summary>
        property Tags: ITagSet read fTags write SetTags;
        ///  <summary>Search logic. Either AND or OR.</summary>
        property Logic: TSearchLogic read fLogic write SetLogic;
      end;
  strict private
    var
      fSearch: ISearch;
      fRefinePreviousSearch: Boolean;
      fTagListMgr: TSortedCollectionCtrlKVMgr<TTag>;
      fSearchParams: TSearchParams;
  strict protected
    procedure InitForm; override;
    procedure ArrangeForm; override;
  public
    class function Execute(const AOwner: TComponent;
      out ASearch: ISearch; out RefineExisting: Boolean): Boolean;
  end;

implementation

uses
  CS.Database.Tags,
  DB.UMain,
  UCtrlArranger,
  UIStringList,
  USettings,
  UStrUtils {for inlining};

{$R *.dfm}

procedure TTagsSearchDlg.ArrangeForm;
begin
  TCtrlArranger.AlignLefts([lblTags, clbTags, rgLogic], 0);
  TCtrlArranger.MoveToRightOf(rgLogic, rgScope, 8);
  TCtrlArranger.StretchRightTo(clbTags, TCtrlArranger.RightOf(rgScope));
  lblTags.Top := 0;
  TCtrlArranger.MoveBelow(lblTags, clbTags, 4);
  TCtrlArranger.AlignTops(
    [rgLogic, rgScope], TCtrlArranger.BottomOf(clbTags, 8)
  );
  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody) + 4;
  pnlBody.ClientWidth := TCtrlArranger.TotalControlWidth(pnlBody);
  inherited;
end;

procedure TTagsSearchDlg.btnOKClick(Sender: TObject);

  // Returns set of tags to be included in search
  function GetTags: ITagSet;
  var
    I: Integer;
  begin
    Result := TTagSet.Create;
    for I := 0 to Pred(clbTags.Count) do
      if clbTags.Checked[I] then
        Result.Add(fTagListMgr.GetKeyAt(I));
  end;

  // Returns selected search logic
  function GetLogic: TSearchLogic;
  begin
    if rgLogic.ItemIndex = 0 then
      Result := slAnd   // first radio button
    else
      Result := slOr;   // second radio button
  end;

var
  Filter: ITagSearchFilter; // search filter
begin
  // Create filter and search objects from entries in dialogue box
  Filter := TSearchFilterFactory.CreateTagSearchFilter(GetTags, GetLogic);
  fSearch := TSearchFactory.CreateSearch(Filter);
  // Record search scope
  fRefinePreviousSearch := rgScope.ItemIndex = 0;
  // Persist the search criteria
  fSearchParams.Logic := Filter.Logic;
  fSearchParams.Tags := Filter.Tags;
end;

class function TTagsSearchDlg.Execute(const AOwner: TComponent;
  out ASearch: ISearch; out RefineExisting: Boolean): Boolean;
begin
  with InternalCreate(AOwner) do
    try
      Result := (ShowModal = mrOK);
      if Result then
      begin
        ASearch := fSearch;
        RefineExisting := fRefinePreviousSearch;
      end;
    finally
      Free;
    end;
end;

procedure TTagsSearchDlg.FormCreate(Sender: TObject);
begin
  inherited;
  fTagListMgr := TSortedCollectionCtrlKVMgr<TTag>.Create(
    TListBoxAdapter.Create(clbTags),
    True,
    function (const Left, Right: TTag): Boolean
    begin
      Result := Left = Right;
    end,
    stIgnoreCase
  );
  fSearchParams := TSearchParams.Create;
end;

procedure TTagsSearchDlg.FormDestroy(Sender: TObject);
begin
  fSearchParams.Free;
  fTagListMgr.Free;
  inherited;
end;

procedure TTagsSearchDlg.InitForm;
var
  Tag: TTag;
  CheckedTag: TTag;
  I: Integer;
begin
  inherited;
  // Populate tags check list box
  for Tag in Database.GetAllTags do
    fTagListMgr.Add(Tag, Tag.ToString);
  // Select tags per persistent storage
  for CheckedTag in fSearchParams.Tags do
  begin
    I := fTagListMgr.IndexOfKey(CheckedTag);
    clbTags.Checked[I] := True;
  end;
  // Select appropriate search logic radio button
  // radio button index is ordinal value of Logic
  rgLogic.ItemIndex := Ord(fSearchParams.Logic);
end;

{ TTagsSearchDlg.TSearchParams }

constructor TTagsSearchDlg.TSearchParams.Create;
begin
  inherited Create;
  fTags := TTagSet.Create;
  Load;
end;

destructor TTagsSearchDlg.TSearchParams.Destroy;
begin
  if fUpdated then
    Save;
  inherited;
end;

procedure TTagsSearchDlg.TSearchParams.Load;
var
  Storage: ISettingsSection;
  TagNames: IStringList;
  TagName: string;
begin
  Storage := Settings.ReadSection(ssFindTags);
  fTags.Clear;
  TagNames := Storage.GetStrings('TagCount', 'Tag%d');
  for TagName in TagNames do
    fTags.Add(TTag.Create(TagName));
  fLogic := TSearchLogic(Storage.GetInteger('Logic', Ord(slOr)));
  fUpdated := False;
end;

procedure TTagsSearchDlg.TSearchParams.Save;
var
  Storage: ISettingsSection;  // object used to access persistent storage
  TagNames: IStringList;
  Tag: TTag;
begin
  Storage := Settings.EmptySection(ssFindTags);
  TagNames := TIStringList.Create;
  for Tag in fTags do
    TagNames.Add(Tag.ToString);
  Storage.SetStrings('TagCount', 'Tag%d', TagNames);
  Storage.SetInteger('Logic', Ord(fLogic));
  Storage.Save;
end;

procedure TTagsSearchDlg.TSearchParams.SetLogic(ALogic: TSearchLogic);
begin
  fLogic := ALogic;
  fUpdated := True;
end;

procedure TTagsSearchDlg.TSearchParams.SetTags(ATags: ITagSet);
begin
  fTags := ATags;
  fUpdated := True;
end;

end.

