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
  ExtCtrls,
  Controls,
  StdCtrls,
  Classes,
  ActnList,
  ComCtrls,
  Forms,
  Types,
  Generics.Defaults,
  // 3rd party
  Collections.Base,
  Collections.Lists,
  // Project
  CS.Database.Types,
  FmGenericNonModalDlg,
  IntfNotifier,
  UBox,
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
    lbFavs: TListBox;
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
    procedure lbFavsDblClick(Sender: TObject);
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

      TListBoxMgr = class(TObject)
      strict private
        var
          fSnippetList: TSortedList<ISnippet>;
          fLB: TListBox;
          fSortFn: TComparison<ISnippet>;
        function IndexOfSnippet(const SnippetID: TSnippetID): Integer;
        function GetSnippetAt(const Idx: Integer): ISnippet;
        procedure SetSortFn(const SortFn: TComparison<ISnippet>);
        procedure InternalAddSnippet(Snippet: ISnippet);
        procedure InternalDeleteSnippetAt(const Idx: Integer);
        procedure LBDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
          State: TOwnerDrawState);
        procedure LBData(Control: TWinControl; Index: Integer;
          var Data: string);
      public
        constructor Create(LB: TListBox);
        destructor Destroy; override;
        procedure Populate(SnippetIDs: ISnippetIDList);
        procedure Clear;
        procedure Delete(const SnippetID: TSnippetID);
        procedure Add(Snippet: ISnippet);
        procedure Update(Snippet: ISnippet);
        function HasSelection: Boolean;
        function GetSelected: TSnippetID;
        procedure ReSort(const SortFn: TComparison<ISnippet>);
      end;
  strict private
    var
      ///  <summary>Object that manages display and painting of list box that
      ///  displays favourite snippets.</summary>
      fFavsLBMgr: TListBoxMgr;
      ///  <summary>Object encapsulating favourites.</summary>
      fFavourites: ISnippetIDList;
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
    procedure LoadFavourites;

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

    ///  <summary>Handles database change events by updating the favorites list
    ///  as necessary.</summary>
    ///  <param name="Sender">TObject [in] Object that triggered event. Not
    ///  used.</param>
    ///  <param name="EvtInfo">IInterface [in] Object that carries information
    ///  about the database change event.</param>
    procedure DBChangeEventHandler(Sender: TObject; const EvtInfo: IInterface);

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
    ///  <param name="Notifier">INotifier [in] Reference to notifier object used
    ///  to cause a favourite snippet to be displayed in main window.</param>
    class procedure Display(AOwner: TComponent; Notifier: INotifier);

    ///  <summary>Closes the dialogue box, freeing any form instance.</summary>
    class procedure Close;

    ///  <summary>Checks if the form is currently displayed.</summary>
    class function IsDisplayed: Boolean;
  end;


implementation


uses
  // Delphi
  SysUtils,
  Windows,
  Graphics,
  // Project
  CS.Database.Snippets,
  DB.UMain,
  IntfCommon,
  UCtrlArranger,
  UMessageBox,
  UPreferences,
  USettings,
  UStrUtils;

{$R *.dfm}


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
var
  SnippetID: TSnippetID;
begin
  if not TMessageBox.Confirm(Self, sQuery) then
    Exit;
  // To avoid lots of flicker as items are deleted one by one, we disable
  // handling of database change events: we know what the outcome of clearing
  // all favourites is, so can put display into correct state.
  Database.RemoveChangeEventHandler(DBChangeEventHandler);
  try
    fFavsLBMgr.Clear;
    // NOTE: if this method is re-implemented with the database change handler
    // still in place, then the following iteration will need to be done on a
    // copy of fFavourites because DBChangeEventHandler will update fFavourites,
    // which is not allowed while it is being enumerated.
    for SnippetID in fFavourites do
      fNotifier.ChangeSnippetStar(SnippetID, False);
    fFavourites.Clear;
  finally
    Database.AddChangeEventHandler(DBChangeEventHandler);
  end;
end;

procedure TFavouritesDlg.actDeleteAllUpdate(Sender: TObject);
begin
  actDeleteAll.Enabled := not fFavourites.IsEmpty;
end;

procedure TFavouritesDlg.actDeleteExecute(Sender: TObject);
begin
  fNotifier.ChangeSnippetStar(fFavsLBMgr.GetSelected, False);
end;

procedure TFavouritesDlg.actDeleteUpdate(Sender: TObject);
begin
  actDelete.Enabled := fFavsLBMgr.HasSelection;
end;

procedure TFavouritesDlg.actDisplayExecute(Sender: TObject);
begin
  fNotifier.DisplaySnippet(fFavsLBMgr.GetSelected, chkNewTab.Checked);
end;

procedure TFavouritesDlg.actDisplayUpdate(Sender: TObject);
begin
  actDisplay.Enabled := fFavsLBMgr.HasSelection;
end;

procedure TFavouritesDlg.ArrangeForm;
begin
  TCtrlArranger.AlignTops([lbFavs, btnDisplay], 0);
  TCtrlArranger.AlignLefts([lbFavs, chkNewTab], 0);
  TCtrlArranger.AlignLefts(
    [btnDisplay, btnDelete, btnDeleteAll],
    TCtrlArranger.RightOf(lbFavs, 8)
  );
  TCtrlArranger.MoveBelow(lbFavs, chkNewTab, 8);
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
    FreeAndNil(fInstance);  // FreeAndNil is necessary here
end;

procedure TFavouritesDlg.ConfigForm;
begin
  inherited;
  LoadFavourites;
  TTrackBarHack(tbTransparency).OnMouseDown := TBTransparencyMouseDown;
  TTrackBarHack(tbTransparency).OnMouseUp := TBTransparencyMouseUp;
  Database.AddChangeEventHandler(DBChangeEventHandler);
end;

procedure TFavouritesDlg.DBChangeEventHandler(Sender: TObject;
  const EvtInfo: IInterface);
var
  EventInfo: IDatabaseChangeEventInfo;  // information about the event
  Snippet: ISnippet;                    // snippet referenced in event

  ///  <summary>Extracts snippet ID from EvtInfo object.</summary>
  function EvtInfoToSnippetID: TSnippetID;
  begin
    Result := (EventInfo.Info as TBox<TSnippetID>).Value;
  end;

begin
  EventInfo := EvtInfo as IDatabaseChangeEventInfo;
  case EventInfo.Kind of
    evSnippetAdded:
    begin
      Snippet := Database.LookupSnippet(EvtInfoToSnippetID);
      if Snippet.Starred then
      begin
        fFavourites.Add(Snippet.ID);
        fFavsLBMgr.Add(Snippet);
      end;
    end;
    evBeforeSnippetDelete:
    begin
      if fFavourites.Contains(EvtInfoToSnippetID) then
      begin
        // Starred snippet was deleted: remove from list
        fFavsLBMgr.Delete(EvtInfoToSnippetID);
        fFavourites.Remove(EvtInfoToSnippetID);
      end;
    end;
    evSnippetChanged:
    begin
      Snippet := Database.LookupSnippet(EvtInfoToSnippetID);
      if fFavourites.Contains(Snippet.ID) then
      begin
        if not Snippet.Starred then
        begin
          // Snippet that is in favourites list is no longer starred, so remove
          // it.
          fFavsLBMgr.Delete(Snippet.ID);
          fFavourites.Remove(Snippet.ID);
        end
        else
        begin
          // Snippet in list is still starred, but has changed in some other
          // way, so update list in case sort order affected.
          fFavsLBMgr.Update(Snippet);
        end;
      end
      else
      begin
        if Snippet.Starred then
        begin
          // Snippet that wasn't in snippet list has been starred, so add it to
          // list.
          fFavourites.Add(Snippet.ID);
          fFavsLBMgr.Add(Snippet);
        end;
      end;
    end;
  end;
end;

class procedure TFavouritesDlg.Display(AOwner: TComponent; Notifier: INotifier);
begin
  if not Assigned(fInstance) then
    fInstance := Create(AOwner);
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
  fFavsLBMgr := TListBoxMgr.Create(lbFavs);
  fOptions := TPersistentOptions.Create;
  fWindowSettings := TDlgWindowSettings.CreateStandAlone(Self);
  AlphaBlendValue := High(Byte);
  fFavourites := TSnippetIDList.Create;
end;

procedure TFavouritesDlg.FormDeactivate(Sender: TObject);
begin
  FadeOut;
end;

procedure TFavouritesDlg.FormDestroy(Sender: TObject);
begin
  Database.RemoveChangeEventHandler(DBChangeEventHandler);
  fFavsLBMgr.Free;
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
  fFavsLBMgr.Populate(fFavourites);
  chkNewTab.Checked := fOptions.DisplayInNewTabs;
  tbTransparency.OnChange := nil;
  tbTransparency.Position := fOptions.InactiveAlphaBlendValue;
  tbTransparency.OnChange := tbTransparencyChange;
end;

class function TFavouritesDlg.IsDisplayed: Boolean;
begin
  Result := Assigned(fInstance) and fInstance.Visible;
end;

procedure TFavouritesDlg.lbFavsDblClick(Sender: TObject);
begin
  actDisplay.Execute;
end;

procedure TFavouritesDlg.LoadFavourites;
begin
  fFavourites := Database.SelectSnippets(
    function (Snippet: ISnippet): Boolean
    begin
      Result := Snippet.Starred;
    end
  );
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

{ TFavouritesDlg.TListBoxMgr }

procedure TFavouritesDlg.TListBoxMgr.Add(Snippet: ISnippet);
var
  SelSnippet: ISnippet;
begin
  fLB.Items.BeginUpdate;
  try
    if HasSelection then
      SelSnippet := GetSnippetAt(fLB.ItemIndex)
    else
      SelSnippet := nil;
    InternalAddSnippet(Snippet);
    if Assigned(SelSnippet) then
      fLB.ItemIndex := fSnippetList.IndexOf(SelSnippet);
  finally
    fLB.Items.EndUpdate;
  end;
end;

procedure TFavouritesDlg.TListBoxMgr.Clear;
begin
  fLB.Items.BeginUpdate;
  try
    fLB.ItemIndex := -1;
    fLB.Count := 0;
    fSnippetList.Clear;
  finally
    fLB.Items.EndUpdate;
  end;
end;

constructor TFavouritesDlg.TListBoxMgr.Create(LB: TListBox);
begin
  Assert(Assigned(LB), ClassName + '.Create: LB is nil');
  inherited Create;
  fLB := LB;
  SetSortFn(nil); // this creates fSnippetList so don't need to do it here
  fLB.Style := lbVirtualOwnerDraw;
  fLB.ItemHeight := 19;
  fLB.OnDrawItem := LBDrawItem;
  fLB.OnData := LBData;
end;

procedure TFavouritesDlg.TListBoxMgr.Delete(const SnippetID: TSnippetID);
var
  SnippetIdx: Integer;
begin
  fLB.Items.BeginUpdate;
  try
    SnippetIdx := IndexOfSnippet(SnippetID);
    if SnippetIdx = -1 then
      Exit;
    InternalDeleteSnippetAt(SnippetIdx);
    if SnippetIdx >= fLB.Count then
      SnippetIdx := Pred(fLB.Count);
    fLB.ItemIndex := SnippetIdx;
  finally
    fLB.Items.EndUpdate;
  end;
end;

destructor TFavouritesDlg.TListBoxMgr.Destroy;
begin
  Clear;
  fSnippetList.Free;
  inherited;
end;

function TFavouritesDlg.TListBoxMgr.GetSelected: TSnippetID;
begin
  if not HasSelection then
    Exit(TSnippetID.CreateNull);
  Result := GetSnippetAt(fLB.ItemIndex).ID;
end;

function TFavouritesDlg.TListBoxMgr.GetSnippetAt(const Idx: Integer): ISnippet;
begin
  Result := fSnippetList[Idx];
end;

function TFavouritesDlg.TListBoxMgr.HasSelection: Boolean;
begin
  Result := fLB.ItemIndex >= 0;
end;

function TFavouritesDlg.TListBoxMgr.IndexOfSnippet(const SnippetID: TSnippetID):
  Integer;
var
  Idx: Integer;
begin
  for Idx := 0 to Pred(fSnippetList.Count) do
    if SnippetID = fSnippetList[Idx].ID then
      Exit(Idx);
  Result := -1;
end;

procedure TFavouritesDlg.TListBoxMgr.InternalAddSnippet(Snippet: ISnippet);
begin
  fSnippetList.Add(Snippet);
  fLB.Count := fSnippetList.Count;
end;

procedure TFavouritesDlg.TListBoxMgr.InternalDeleteSnippetAt(
  const Idx: Integer);
begin
  fLB.Count := fSnippetList.Count - 1;
  fSnippetList.RemoveAt(Idx);
end;

procedure TFavouritesDlg.TListBoxMgr.LBData(Control: TWinControl;
  Index: Integer; var Data: string);
begin
  Data := fSnippetList[Index].Title;
end;

procedure TFavouritesDlg.TListBoxMgr.LBDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Canvas: TCanvas;
  DisplayText: string;

  { TODO -cSynchSpaces: Revisit this method in the light of synch spaces. }
  function IsUserDefinedItem: Boolean;
  begin
    Result := True;
  end;

begin
  Assert(Control = fLB,
    ClassName + '.LBDrawItem: Event handler called for wrong list box');
  Canvas := fLB.Canvas;
  DisplayText := GetSnippetAt(Index).Title;
  if not (odSelected in State) then
    { TODO -cSynchSpaces: Revisit colour selection in the light of synch spaces.
    }
    Canvas.Font.Color := Preferences.DBHeadingColours[IsUserDefinedItem];
  Canvas.TextRect(
    Rect,
    Rect.Left + 2,
    (Rect.Top + Rect.Bottom - Canvas.TextHeight(DisplayText)) div 2,
    DisplayText
  );
end;

procedure TFavouritesDlg.TListBoxMgr.Populate(SnippetIDs: ISnippetIDList);
var
  SnippetID: TSnippetID;
begin
  fLB.Items.BeginUpdate;
  try
    for SnippetID in SnippetIDs do
      InternalAddSnippet(Database.LookupSnippet(SnippetID));
  finally
    fLB.Items.EndUpdate
  end;
end;

procedure TFavouritesDlg.TListBoxMgr.ReSort(
  const SortFn: TComparison<ISnippet>);
var
  SelSnippet: ISnippet;
begin
  if HasSelection then
    SelSnippet := GetSnippetAt(fLB.ItemIndex)
  else
    SelSnippet := nil;
  SetSortFn(SortFn);
  fLB.Invalidate;
  if Assigned(SelSnippet) then
    fLB.ItemIndex := fSnippetList.IndexOf(SelSnippet);
end;

procedure TFavouritesDlg.TListBoxMgr.SetSortFn(
  const SortFn: TComparison<ISnippet>);
var
  NewList: TSortedList<ISnippet>;
  OldList: TSortedList<ISnippet>;
  Rules: TRules<ISnippet>;
begin
  if not Assigned(SortFn) then
    fSortFn := function(const Left, Right: ISnippet): Integer
      begin
        Result := StrCompareText(Left.Title, Right.Title);
      end
  else
    fSortFn := SortFn;

  Rules := TRules<ISnippet>.Create(
    TComparer<ISnippet>.Construct(fSortFn),
    TEqualityComparer<ISnippet>.Construct(
      function (const Left, Right: ISnippet): Boolean
      begin
        Result := fSortFn(Left, Right) = 0;
      end,
      function (const Value: ISnippet): Integer
      begin
        Result := Value.ID.Hash;
      end
    )
  );
  if Assigned(fSnippetList) then
  begin
    OldList := fSnippetList;
    NewList := TSortedList<ISnippet>.Create(Rules, OldList);
    fSnippetList := NewList;
    OldList.Free;
  end
  else
    fSnippetList := TSortedList<ISnippet>.Create(Rules);
end;

procedure TFavouritesDlg.TListBoxMgr.Update(Snippet: ISnippet);
var
  Idx: Integer;
begin
  fLB.Items.BeginUpdate;
  try
    Idx := IndexOfSnippet(Snippet.ID);
    InternalDeleteSnippetAt(Idx);
    InternalAddSnippet(Snippet);
    fLB.ItemIndex := IndexOfSnippet(Snippet.ID); // index may have changed
  finally
    fLB.Items.EndUpdate;
  end;
end;

end.

