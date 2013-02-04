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
 * Implements a dialogue box that displays and manages the user's favourite
 * snippets.
}


unit FmFavouritesDlg;


interface


uses
  // Delphi
  Classes, ActnList, StdCtrls, Controls, ExtCtrls, Forms, ComCtrls,
  Generics.Collections {must be listed after Classes},
  // 3rd party
  LVEx,
  // Project
  FmGenericNonModalDlg, Favourites.UFavourites, IntfNotifier;


type
  TFavouritesDlg = class(TGenericNonModalDlg)
    alDlg: TActionList;
    btnDisplay: TButton;
    btnDelete: TButton;
    btnDeleteAll: TButton;
    actDelete: TAction;
    actDeleteAll: TAction;
    actDisplay: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actDisplayUpdate(Sender: TObject);
    procedure actDisplayExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actDeleteUpdate(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actDeleteAllUpdate(Sender: TObject);
    procedure actDeleteAllExecute(Sender: TObject);
  strict private
    var
      fLVFavs: TListViewEx;
      fFavourites: TFavourites;
      fNotifier: INotifier;
    class var
      fInstance: TFavouritesDlg;
    ///  <summary>Specifies that a TFavouriteListItem is to be used to create
    ///  new list view items.</summary>
    procedure LVFavouriteCreateItemClass(Sender: TCustomListView;
      var ItemClass: TListItemClass);
    ///  <summary>Compares two list items, Item1 and Item2, using text contained
    ///  in row specified Data parameter, recording result of comparison in
    ///  Compare parameter.</summary>
    procedure LVFavouritesCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure LVDoubleClick(Sender: TObject);
    ///  <summary>Returns text of list item LI in column specified by Idx.
    ///  </summary>
    ///  <remarks>Idx = 0 returns list item's caption while Idx > 0 returns text
    ///  of SubItem[Idx-1].</remarks>
    procedure CreateLV;
    procedure PopulateLV;
    procedure ReSortLV;
    procedure AddLVItem(const Favourite: TFavourite);
    procedure RemoveLVItem(const Favourite: TFavourite);
    procedure FavouritesListener(Sender: TObject; const EvtInfo: IInterface);
  strict protected
    procedure ConfigForm; override;
    procedure ArrangeForm; override;
  public
    class procedure Display(AOwner: TComponent; const Favourites: TFavourites;
      Notifier: INotifier);
    class procedure Close;
    class function IsDisplayed: Boolean;
  end;


implementation


uses
  // Delphi
  SysUtils, DateUtils,
  // Project
  UCtrlArranger, UMessageBox, UStrUtils;

{$R *.dfm}


type
  ///  <summary>Custom list item class that adds ability to store a TFavourite
  ///  record with list item.</summary>
  TFavouriteListItem = class(TListItem)
  strict private
    var
      ///  <summary>Value of Warning property.</summary>
      fFavourite: TFavourite;
  public
    ///  <summary>Favourite associated with list item.</summary>
    property Favourite: TFavourite read fFavourite write fFavourite;
  end;

{ TFavouritesDlg }

procedure TFavouritesDlg.actDeleteAllExecute(Sender: TObject);
resourcestring
  sQuery = 'Do you really want to delete all your favourites?';
begin
  if TMessageBox.Confirm(Self, sQuery) then
  begin
    fLVFavs.Items.BeginUpdate;
    try
      fFavourites.Clear;
    finally
      fLVFavs.Items.EndUpdate;
    end;
  end;
end;

procedure TFavouritesDlg.actDeleteAllUpdate(Sender: TObject);
begin
  actDeleteAll.Enabled := not fFavourites.IsEmpty;
end;

procedure TFavouritesDlg.actDeleteExecute(Sender: TObject);
var
  LI: TFavouriteListItem;
begin
  LI := fLVFavs.Selected as TFavouriteListItem;
  fFavourites.Remove(LI.Favourite.SnippetID);
end;

procedure TFavouritesDlg.actDeleteUpdate(Sender: TObject);
begin
  actDelete.Enabled := Assigned(fLVFavs.Selected);
end;

procedure TFavouritesDlg.actDisplayExecute(Sender: TObject);
var
  LI: TFavouriteListItem;
begin
  LI := fLVFavs.Selected as TFavouriteListItem;
  // TODO: Give user choice of whether to display in new tab or not
  fNotifier.DisplaySnippet(
    LI.Favourite.SnippetID.Name, LI.Favourite.SnippetID.UserDefined, True
  );
  fFavourites.Touch(LI.Favourite.SnippetID);
end;

procedure TFavouritesDlg.actDisplayUpdate(Sender: TObject);
begin
  actDisplay.Enabled := Assigned(fLVFavs.Selected);
end;

procedure TFavouritesDlg.AddLVItem(const Favourite: TFavourite);
var
  LI: TFavouriteListItem;
begin
  LI := fLVFavs.Items.Add as TFavouriteListItem;
  // TODO: change LV to owner draw and display snippets in correct colours
  if Favourite.SnippetID.UserDefined then
    LI.Caption := Favourite.SnippetID.Name + ' (user)'
  else
    LI.Caption := Favourite.SnippetID.Name + ' (main)';
  if IsToday(Favourite.LastAccessed) then
    LI.SubItems.Add(TimeToStr(Favourite.LastAccessed))
  else
    LI.SubItems.Add(DateTimeToStr(Favourite.LastAccessed));
  LI.Favourite := Favourite;
end;

procedure TFavouritesDlg.ArrangeForm;
begin
  TCtrlArranger.AlignTops([fLVFavs, btnDisplay], 0);
  fLVFavs.Left := 0;
  TCtrlArranger.AlignLefts(
    [btnDisplay, btnDelete, btnDeleteAll],
    TCtrlArranger.RightOf(fLVFavs, 8)
  );
  pnlBody.ClientWidth := TCtrlArranger.TotalControlWidth(pnlBody) + 4;
  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody);
  inherited;
end;

class procedure TFavouritesDlg.Close;
begin
  if IsDisplayed then
    FreeAndNil(fInstance);
end;

procedure TFavouritesDlg.ConfigForm;
begin
  inherited;
  fFavourites.AddListener(FavouritesListener);
end;

procedure TFavouritesDlg.CreateLV;
resourcestring
  sSnippetName = 'Snippet';
  sLastAccessed = 'Last used';
begin
  fLVFavs := TListViewEx.Create(Self);
  with fLVFavs do
  begin
    Parent := pnlBody;
    // TODO: make this owner draw
    Height := 240;
    Width := 360;
    HideSelection := False;
    ReadOnly := True;
    RowSelect := True;
    TabOrder := 2;
    ViewStyle := vsReport;
    SortImmediately := False;
    with Columns.Add do
    begin
      Caption := sSnippetName;
      Width := 180;
    end;
    with Columns.Add do
    begin
      Caption := sLastAccessed;
      Width := 140;
    end;
    OnDblClick := LVDoubleClick;
    OnCompare := LVFavouritesCompare;
    OnCreateItemClass := LVFavouriteCreateItemClass;
  end;
end;

class procedure TFavouritesDlg.Display(AOwner: TComponent;
  const Favourites: TFavourites; Notifier: INotifier);
begin
  if not Assigned(fInstance) then
    fInstance := Create(AOwner);
  fInstance.fFavourites := Favourites;
  fInstance.fNotifier := Notifier;
  if not fInstance.Visible then
  begin
    fInstance.Show;
    fInstance.PopulateLV;
  end;
  fInstance.SetFocus;
end;

procedure TFavouritesDlg.FavouritesListener(Sender: TObject;
  const EvtInfo: IInterface);
var
  Evt: IFavouritesChangeEventInfo;
begin
  Evt := EvtInfo as IFavouritesChangeEventInfo;
  case Evt.Action of
    cnAdded:
    begin
      AddLVItem(Evt.Favourite);
      ReSortLV;
    end;
    cnRemoved:
    begin
      RemoveLVItem(Evt.Favourite);
      ReSortLV;
    end;
  end;
end;

procedure TFavouritesDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  fInstance := nil;
  Action := caFree;
end;

procedure TFavouritesDlg.FormCreate(Sender: TObject);
begin
  inherited;
  CreateLV;
end;

procedure TFavouritesDlg.FormDestroy(Sender: TObject);
begin
  fFavourites.RemoveListener(FavouritesListener);
  inherited;
end;

class function TFavouritesDlg.IsDisplayed: Boolean;
begin
  Result := Assigned(fInstance) and fInstance.Visible;
end;

procedure TFavouritesDlg.LVDoubleClick(Sender: TObject);
begin
  actDisplay.Execute;
end;

procedure TFavouritesDlg.LVFavouriteCreateItemClass(Sender: TCustomListView;
  var ItemClass: TListItemClass);
begin
  ItemClass := TFavouriteListItem;
end;

procedure TFavouritesDlg.LVFavouritesCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  Fav1, Fav2: TFavourite;
begin
  Fav1 := (Item1 as TFavouriteListItem).Favourite;
  Fav2 := (Item2 as TFavouriteListItem).Favourite;

  case fLVFavs.SortColumn of
    0:
    begin
      Compare := Fav1.SnippetID.CompareTo(Fav2.SnippetID);
    end;
    1:
    begin
      Compare := CompareDateTime(Fav1.LastAccessed, Fav2.LastAccessed);
    end;
  end;
  if fLVFavs.SortOrder = soDown then
    Compare := -Compare;
end;

procedure TFavouritesDlg.PopulateLV;
var
  Fav: TFavourite;
begin
  fLVFavs.Clear;
  for Fav in fFavourites do
    AddLVItem(Fav);
  fLVFavs.Selected := nil;
  ReSortLV;
end;

procedure TFavouritesDlg.RemoveLVItem(const Favourite: TFavourite);
var
  LI: TListItem;
begin
  for LI in fLVFavs.Items do
  begin
    if (LI as TFavouriteListItem).Favourite.SnippetID = Favourite.SnippetID then
    begin
      LI.Free;
      Exit;
    end;
  end;
end;

procedure TFavouritesDlg.ReSortLV;
begin
  if fLVFavs.SortColumn <> -1 then
    fLVFavs.CustomSort(nil, fLVFavs.SortColumn);
end;

end.

