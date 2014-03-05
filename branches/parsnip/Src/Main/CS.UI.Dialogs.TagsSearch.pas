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
    var
      fSearch: ISearch;
      fRefinePreviousSearch: Boolean;
      fTagListMgr: TSortedCollectionCtrlKVMgr<TTag>;
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
  // Create search filter from entries made in dialog box
  Filter := TSearchFilterFactory.CreateTagSearchFilter(GetTags, GetLogic);
//  // Persist the search criteria
//  fSearchParams.Logic := Filter.Logic;
//  fSearchParams.Tags := Filter.Tags;
  // Create search object from filter
  fSearch := TSearchFactory.CreateSearch(Filter);
  // Record search scope
  fRefinePreviousSearch := rgScope.ItemIndex = 0;
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
end;

procedure TTagsSearchDlg.FormDestroy(Sender: TObject);
begin
  fTagListMgr.Free;
  inherited;
end;

procedure TTagsSearchDlg.InitForm;
var
  Tag: TTag;
begin
  inherited;
  for Tag in Database.GetAllTags do
    fTagListMgr.Add(Tag, Tag.ToString);
end;

end.
