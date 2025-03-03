{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that displays and manages the user's favourite
 * snippets.
}


unit FmFavouritesDlg;


interface


uses
  // Delphi
  Classes, ActnList, StdCtrls, Controls, ExtCtrls, Forms, ComCtrls, Types,
  Generics.Collections {must be listed after Classes},
  // 3rd party
  LVEx,
  // Project
  FmGenericNonModalDlg, Favourites.UFavourites, IntfNotifier, USnippetIDs,
  UWindowSettings;


type
  ///  <summary>Implements a non-modal dialogue box that displays and manages
  ///  the user's favourite snippets.</summary>
  ///  <remarks>By default the dialogue box fades to semi-transparency when it
  ///  is not active and returns to full opacity when activated.</remarks>
  TFavouritesDlg = class(TGenericNonModalDlg)
    actDelete: TAction;
    actDeleteAll: TAction;
    actDisplay: TAction;
    alDlg: TActionList;
    btnDelete: TButton;
    btnDeleteAll: TButton;
    btnDisplay: TButton;
    chkNewTab: TCheckBox;
    lblTransparency: TLabel;
    tbTransparency: TTrackBar;
    ///  <summary>Clears all favourites.</summary>
    ///  <remarks>Prompts user for permission before proceeding.</remarks>
    procedure actDeleteAllExecute(Sender: TObject);
    ///  <summary>Disables DeleteAll action iff there are no favourites.
    ///  </summary>
    procedure actDeleteAllUpdate(Sender: TObject);
    ///  <summary>Removes favourite selected in list view from list.</summary>
    procedure actDeleteExecute(Sender: TObject);
    ///  <summary>Enables Delete action iff a favourite is selected in list
    ///  view.</summary>
    procedure actDeleteUpdate(Sender: TObject);
    ///  <summary>Displays favourite selected in list view.</summary>
    procedure actDisplayExecute(Sender: TObject);
    ///  <summary>Enables Display action iff a favourite is selected in list
    ///  view.</summary>
    procedure actDisplayUpdate(Sender: TObject);
    ///  <summary>Fades form to full opacity when dialogue box is activated.
    ///  </summary>
    procedure FormActivate(Sender: TObject);
    ///  <summary>Causes form's object instance to be automatically freed when
    ///  closed.</summary>
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    ///  <summary>Sets default property values when form is created.</summary>
    procedure FormCreate(Sender: TObject);
    ///  <summary>Fades form to specified transparency when dialogue box is
    ///  de-activated.</summary>
    procedure FormDeactivate(Sender: TObject);
    ///  <summary>Saves form's window state before form is destroyed.</summary>
    procedure FormDestroy(Sender: TObject);
    ///  <summary>Adjusts form transparency according to new trackbar position
    ///  whenever it changes.</summary>
    procedure tbTransparencyChange(Sender: TObject);
    ///  <summary>Records that a key has been pressed over the transparency
    ///  trackbar and fades the form down to the transparency specified by the
    ///  trackbar.</summary>
    procedure tbTransparencyKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    ///  <summary>Records that the currently depressed key has been released
    ///  over the trackbar and fades the form back up to full opacity.</summary>
    procedure tbTransparencyKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  strict private
    type
      ///  <summary>Reads and stores persistent options in settings.</summary>
      TPersistentOptions = class(TObject)
      strict private
        var
          ///  <summary>Value of DisplayInNewTabs property.</summary>
          fDisplayInNewTabs: Boolean;
          ///  <summary>Value of InactiveAlphaBlendValue property.</summary>
          fInactiveAlphaBlendValue: Byte;
        const
          ///  <summary>Name of settings key where DisplayInNewTabs property is
          ///  stored.</summary>
          DisplayInNewTabsKey = 'DisplayInNewTabs';
          ///  <summary>Name of settings key where InactiveAlphaBlendValue
          ///  property is stored.</summary>
          InactiveAlphaBlendValueKey = 'InactiveAlphaBlendValue';
      public
        ///  <summary>Constructs object and reads options from settings.
        ///  </summary>
        constructor Create;
        ///  <summary>Writes options to settings then destroys object.</summary>
        destructor Destroy; override;
        ///  <summary>Specifies whether or not selected favourites are to be
        ///  displayed in new tabs in main window's detail pane.</summary>
        property DisplayInNewTabs: Boolean
          read fDisplayInNewTabs write fDisplayInNewTabs;
        ///  <summary>Stores alpha blend value that determines transparency of
        ///  deactivated form.</summary>
        property InactiveAlphaBlendValue: Byte
          read fInactiveAlphaBlendValue write fInactiveAlphaBlendValue;
      end;
  strict private
    var
      ///  <summary>Custom list view control used to display favourites.
      ///  </summary>
      fLVFavs: TListViewEx;
      ///  <summary>Object encapsulating favourites.</summary>
      fFavourites: TFavourites;
      ///  <summary>Notifier object used to request display of snippets in main
      ///  form.</summary>
      fNotifier: INotifier;
      ///  <summary>Form's persistent options.</summary>
      fOptions: TPersistentOptions;
      ///  <summary>Object used to read / write window position in settings.
      ///  </summary>
      fWindowSettings: TDlgWindowSettings;
      ///  <summary>Flag indicating if a key is currently pressed when the
      ///  transparency trackbar has keyboard focus.</summary>
      fTrackBarKeyDown: Boolean;
      ///  <summary>Flag indicating if the mouse is currently pressed over the
      ///  transparency trackbar.</summary>
      fTrackBarMouseDown: Boolean;
    class var
      ///  <summary>Reference to dialogue box form instance.</summary>
      ///  <remarks>Set to nil if there is no instance of the form.</remarks>
      fInstance: TFavouritesDlg;
  strict private
    ///  <summary>Specifies class to be used to create favourites list view
    ///  items.</summary>
    ///  <param name="Sender">TCustomListView [in] List view control for which
    ///  list item class is required.</param>
    ///  <param name="ItemClass">TListItemClass [in/out] Set to required class:
    ///  TFavouriteListItem in all cases.</param>
    procedure LVFavouriteCreateItemClass(Sender: TCustomListView;
      var ItemClass: TListItemClass);

    ///  <summary>Compares two list items, Item1 and Item2 and records the
    ///  result of the comparison in the Compare parameter.</summary>
    ///  <remarks>Comparison depends on which column of data is being sorted.
    ///  </remarks>
    procedure LVFavouritesCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);

    ///  <summary>Handles double clicks on list view items by causing the
    ///  associated favourite's snippet to be displayed.</summary>
    procedure LVDoubleClick(Sender: TObject);

    ///  <summary>Overrides default drawing of given list view item caption by
    ///  ensuring the associated favourite's snippet display name is rendered in
    ///  correct colour.</summary>
    ///  <remarks>Colour used depends on whether snippet is user defined.
    ///  </remarks>
    procedure LVCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);

    ///  <summary>Ensures that given list view sub item is drawn in correct
    ///  colour.</summary>
    ///  <remarks>Colour is reverted to window text instead of colour set in
    ///  LVCustomDrawItem method.</remarks>
    procedure LVCustomDrawSubItem(Sender: TCustomListView; Item: TListItem;
      SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean);

    ///  <summary>Creates and initialises properties of custom list view control
    ///  that is used to display favourites.</summary>
    procedure CreateLV;

    ///  <summary>Displays all favourites in favourites list view.</summary>
    procedure PopulateLV;

    ///  <summary>Resorts favourites list view according to current sort column.
    ///  </summary>
    procedure ReSortLV;

    ///  <summary>Adds given favourite to favourites list view.</summary>
    procedure AddLVItem(const Favourite: TFavourite);

    ///  <summary>Removes given favourite from favourites list view.</summary>
    ///  <remarks>Does nothing if favourite is not in list view.</remarks>
    procedure RemoveLVItem(const Favourite: TFavourite);

    ///  <summary>Listener method that receives and acts on change events
    ///  triggered by favourites object.</summary>
    ///  <param name="Sender">TObject [in] Reference to favourites object that
    ///  triggered event.</param>
    ///  <param name="EvtInfo">IInterface [in] Reference to object that carries
    ///  information about the event.</param>
    ///  <remarks>EvtInfo must support IFavouritesChangeEventInfo.</remarks>
    procedure FavouritesListener(Sender: TObject; const EvtInfo: IInterface);

    ///  <summary>Changes the transparency of the form from its current level to
    ///  the given new transparency level.</summary>
    ///  <remarks>The change in transparency is performed in small steps to make
    ///  it appear animated.</remarks>
    procedure FadeTo(const NewTransparency: Byte);

    ///  <summary>Fades the form in from its current transparent state to full
    ///  opacity.</summary>
    procedure FadeIn;

    ///  <summary>Fades the form from its current transparency to the level
    ///  indicated by the transparency trackbar.</summary>
    procedure FadeOut;

    ///  <summary>Finds the favourites list view item that is associated with
    ///  the favourite with the given snippet ID.</summary>
    ///  <remarks>Returns nil if no matching list item exists.</remarks>
    function FindListItem(const SnippetID: TSnippetID): TListItem;

    ///  <summary>Records that a mouse button has been pressed over the
    ///  transparency trackbar and fades the form down to the transparency
    ///  specified by the trackbar.</summary>
    procedure TBTransparencyMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    ///  <summary>Records that the currently depressed mouse button has been
    ///  released over the trackbar and fades the form back up to full
    ///  opacity.</summary>
    procedure TBTransparencyMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

  strict protected
    ///  <summary>Configures some of the form's objects.</summary>
    procedure ConfigForm; override;

    ///  <summary>Arranges and aligns the form's controls.</summary>
    procedure ArrangeForm; override;

    ///  <summary>Initialises form's controls and restores last window position.
    ///  </summary>
    procedure InitForm; override;

  public
    ///  <summary>Displays and intialises the dialogue box non-modally, or
    ///  re-activates it if it is already displayed.</summary>
    ///  <param name="AOwner">TComponent [in] Reference to any owning component.
    ///  May be nil.</param>
    ///  <param name="Favourites">TFavourites [in] Reference to favourites
    ///  object to be displayed and managed.</param>
    ///  <param name="Notifier">INotifier [in] Reference to notifier object used
    ///  to cause a favourite snippet to be displayed in main window.</param>
    class procedure Display(AOwner: TComponent; const Favourites: TFavourites;
      Notifier: INotifier);

    ///  <summary>Closes the dialogue box, freeing any form instance.</summary>
    class procedure Close;

    ///  <summary>Checks if the form is currently displayed.</summary>
    class function IsDisplayed: Boolean;
  end;


implementation


uses
  // Delphi
  SysUtils,
  DateUtils,
  Windows,
  Graphics,
  // Project
  DB.UMain,
  DB.USnippet,
  DB.Vaults,
  UCtrlArranger,
  UMessageBox,
  UPreferences,
  USettings,
  UStructs,
  UStrUtils;

{$R *.dfm}


type
  ///  <summary>Custom list item class that adds ability to associate a
  ///  TFavourite record with list item.</summary>
  TFavouriteListItem = class(TListItem)
  strict private
    var
      ///  <summary>Value of Favourite property.</summary>
      fFavourite: TFavourite;
  public
    ///  <summary>Favourite associated with list item.</summary>
    property Favourite: TFavourite read fFavourite write fFavourite;
  end;

type
  ///  <summary>Hack to enable OnMouseUp and OnMouseDown events of TTrackBar to
  ///  be set.</summary>
  ///  <remarks>For some reason these events are protected in TTrackBar.
  ///  </remarks>
  TTrackBarHack = class(TTrackBar);


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
  SelectedSnippet: TSnippetID;
begin
  LI := fLVFavs.Selected as TFavouriteListItem;
  SelectedSnippet := LI.Favourite.SnippetID;
  fNotifier.DisplaySnippet(
    SelectedSnippet.Key,
    SelectedSnippet.CollectionID,
    chkNewTab.Checked
  );
  fFavourites.Touch(SelectedSnippet);
  fLVFavs.Selected := FindListItem(SelectedSnippet);
end;

procedure TFavouritesDlg.actDisplayUpdate(Sender: TObject);
begin
  actDisplay.Enabled := Assigned(fLVFavs.Selected);
end;

procedure TFavouritesDlg.AddLVItem(const Favourite: TFavourite);
var
  LI: TFavouriteListItem;
  Snippet: TSnippet;
begin
  LI := fLVFavs.Items.Add as TFavouriteListItem;
  Snippet := Database.Snippets.Find(Favourite.SnippetID);
  if Assigned(Snippet) then
    LI.Caption := Snippet.DisplayName
  else
    LI.Caption := Favourite.SnippetID.Key;
  if IsToday(Favourite.LastAccessed) then
    LI.SubItems.Add(TimeToStr(Favourite.LastAccessed))
  else
    LI.SubItems.Add(DateTimeToStr(Favourite.LastAccessed));
  LI.Favourite := Favourite;
end;

procedure TFavouritesDlg.ArrangeForm;
begin
  TCtrlArranger.AlignTops([fLVFavs, btnDisplay], 0);
  TCtrlArranger.AlignLefts([fLVFavs, chkNewTab], 0);
  TCtrlArranger.AlignLefts(
    [btnDisplay, btnDelete, btnDeleteAll],
    TCtrlArranger.RightOf(fLVFavs, 8)
  );
  TCtrlArranger.MoveBelow(fLVFavs, chkNewTab, 8);
  pnlBody.ClientWidth := TCtrlArranger.TotalControlWidth(pnlBody) + 4;
  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody);
  inherited;
  TCtrlArranger.AlignLefts([pnlBody, lblTransparency]);
  TCtrlArranger.MoveToRightOf(lblTransparency, tbTransparency, 4);
  TCtrlArranger.AlignTops([btnClose, lblTransparency, tbTransparency]);
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
  TTrackBarHack(tbTransparency).OnMouseDown := TBTransparencyMouseDown;
  TTrackBarHack(tbTransparency).OnMouseUp := TBTransparencyMouseUp;
end;

procedure TFavouritesDlg.CreateLV;

  procedure AddColumn(const ACaption: string; const AWidth: Integer);
  var
    Col: TListColumn;
  begin
    Col := fLVFavs.Columns.Add;
    Col.Caption := ACaption;
    Col.Width := AWidth;
  end;

resourcestring
  sSnippetName = 'Snippet';
  sLastAccessed = 'Last used';
begin
  fLVFavs := TListViewEx.Create(Self);
  fLVFavs.Parent := pnlBody;
  fLVFavs.Height := 240;
  fLVFavs.Width := 360;
  fLVFavs.HideSelection := False;
  fLVFavs.ReadOnly := True;
  fLVFavs.RowSelect := True;
  fLVFavs.TabOrder := 0;
  fLVFavs.TabStop := True;
  fLVFavs.ViewStyle := vsReport;
  fLVFavs.SortImmediately := False;
  AddColumn(sSnippetName, 180);
  AddColumn(sLastAccessed, 140);
  fLVFavs.OnDblClick := LVDoubleClick;
  fLVFavs.OnCompare := LVFavouritesCompare;
  fLVFavs.OnCreateItemClass := LVFavouriteCreateItemClass;
  fLVFavs.OnCustomDrawItem := LVCustomDrawItem;
  fLVFavs.OnCustomDrawSubItem := LVCustomDrawSubItem;
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
  end;
  fInstance.SetFocus;
end;

procedure TFavouritesDlg.FadeIn;
begin
  FadeTo(High(Byte));
end;

procedure TFavouritesDlg.FadeOut;
begin
  FadeTo(tbTransparency.Position);
end;

procedure TFavouritesDlg.FadeTo(const NewTransparency: Byte);
var
  Step: Int8;
begin
  Step := -1;
  if AlphaBlendValue < NewTransparency then
    Step := -Step;
  while AlphaBlendValue <> NewTransparency do
  begin
    AlphaBlendValue := AlphaBlendValue + Step;
    Sleep(2);
  end;
end;

procedure TFavouritesDlg.FavouritesListener(Sender: TObject;
  const EvtInfo: IInterface);
var
  Evt: IFavouritesChangeEventInfo;
  SelectedSnippet: TSnippetID;
  HaveSelection: Boolean;
begin
  Evt := EvtInfo as IFavouritesChangeEventInfo;
  case Evt.Action of
    cnAdded:
    begin
      HaveSelection := Assigned(fLVFavs.Selected);
      if HaveSelection then
        SelectedSnippet :=
          (fLVFavs.Selected as TFavouriteListItem).Favourite.SnippetID;
      AddLVItem(Evt.Favourite);
      ReSortLV;
      if HaveSelection then
        fLVFavs.Selected := FindListItem(SelectedSnippet);
    end;
    cnRemoved:
    begin
      HaveSelection := Assigned(fLVFavs.Selected);
      if HaveSelection then
        SelectedSnippet :=
          (fLVFavs.Selected as TFavouriteListItem).Favourite.SnippetID;
      RemoveLVItem(Evt.Favourite);
      ReSortLV;
      if HaveSelection and (Evt.Favourite.SnippetID <> SelectedSnippet) then
        fLVFavs.Selected := FindListItem(SelectedSnippet);
    end;
  end;
end;

function TFavouritesDlg.FindListItem(const SnippetID: TSnippetID): TListItem;
var
  LI: TListItem;
begin
  for LI in fLVFavs.Items do
  begin
    if (LI as TFavouriteListItem).Favourite.SnippetID = SnippetID then
      Exit(LI);
  end;
  Result := nil;
end;

procedure TFavouritesDlg.FormActivate(Sender: TObject);
begin
  FadeIn;
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
  fOptions := TPersistentOptions.Create;
  fWindowSettings := TDlgWindowSettings.CreateStandAlone(Self);
  AlphaBlendValue := High(Byte);
end;

procedure TFavouritesDlg.FormDeactivate(Sender: TObject);
begin
  FadeOut;
end;

procedure TFavouritesDlg.FormDestroy(Sender: TObject);
begin
  fFavourites.RemoveListener(FavouritesListener);
  fOptions.DisplayInNewTabs := chkNewTab.Checked;
  fOptions.InactiveAlphaBlendValue := tbTransparency.Position;
  fOptions.Free;
  fWindowSettings.Save;
  inherited;
end;

procedure TFavouritesDlg.InitForm;
begin
  inherited;
  fWindowSettings.Restore;
  PopulateLV;
  chkNewTab.Checked := fOptions.DisplayInNewTabs;
  tbTransparency.OnChange := nil;
  tbTransparency.Position := fOptions.InactiveAlphaBlendValue;
  tbTransparency.OnChange := tbTransparencyChange;
end;

class function TFavouritesDlg.IsDisplayed: Boolean;
begin
  Result := Assigned(fInstance) and fInstance.Visible;
end;

procedure TFavouritesDlg.LVCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  CollectionID: TVaultID;
begin
  CollectionID := (Item as TFavouriteListItem).Favourite.SnippetID.CollectionID;
  fLVFavs.Canvas.Font.Color := Preferences.GetSnippetHeadingColour(
    CollectionID
  );
end;

procedure TFavouritesDlg.LVCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  // required to force colour change on following line
  fLVFavs.Canvas.Font.Color := clNone;
  fLVFavs.Canvas.Font.Color := clWindowText;
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
  case fLVFavs.SortColumn of
    0:
      // use item Caption here since it is set to snippet display name
      // which is not recorded in the item's associated TFavourite record
      Compare := StrCompareText(Item1.Caption, Item2.Caption);
    1:
    begin
      Fav1 := (Item1 as TFavouriteListItem).Favourite;
      Fav2 := (Item2 as TFavouriteListItem).Favourite;
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

procedure TFavouritesDlg.tbTransparencyChange(Sender: TObject);
begin
  if fTrackBarKeyDown or fTrackBarMouseDown then
    AlphaBlendValue := tbTransparency.Position;
end;

procedure TFavouritesDlg.tbTransparencyKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  fTrackBarKeyDown := True;
  FadeOut;
end;

procedure TFavouritesDlg.tbTransparencyKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  FadeIn;
  fTrackBarKeyDown := False;
end;

procedure TFavouritesDlg.TBTransparencyMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FadeOut;
  fTrackBarMouseDown := True;
end;

procedure TFavouritesDlg.TBTransparencyMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fTrackBarMouseDown := False;
  FadeIn;
end;

{ TFavouritesDlg.TPersistentOptions }

constructor TFavouritesDlg.TPersistentOptions.Create;
var
  Section: ISettingsSection;
begin
  inherited Create;
  Section := Settings.ReadSection(ssFavourites);
  fDisplayInNewTabs := Section.GetBoolean(DisplayInNewTabsKey, False);
  fInactiveAlphaBlendValue := Section.GetInteger(
    InactiveAlphaBlendValueKey, 160
  );
end;

destructor TFavouritesDlg.TPersistentOptions.Destroy;
var
  Section: ISettingsSection;
begin
  Section := Settings.EmptySection(ssFavourites);
  Section.SetBoolean(DisplayInNewTabsKey, fDisplayInNewTabs);
  Section.SetInteger(InactiveAlphaBlendValueKey, fInactiveAlphaBlendValue);
  Section.Save;
  inherited;
end;

end.

