{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2010-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a frame that allows user to set source code generation
 * preferences.
 * Designed for use as one of the tabs in the Preferences dialogue box.
}


unit FrCodeGenPrefs;


interface


uses
  // Delphi
  Classes, ActnList, StdCtrls, Controls, ComCtrls, Menus, Buttons,
  // 3rd party
  LVEx,
  // Project
  FrPrefsBase, UPreferences, UWarnings;


type
  ///  <summary>Frame that allows user to set source code generation
  ///  preferences.</summary>
  ///  <remarks>Designed for use in preferences dialog box.</remarks>
  TCodeGenPrefsFrame = class(TPrefsBaseFrame)
    chkWARNEnabled: TCheckBox;
    lblSymbol: TLabel;
    lblMinCompiler: TLabel;
    edSymbol: TEdit;
    btnAdd: TButton;
    btnDelete: TButton;
    btnUpdate: TButton;
    alMain: TActionList;
    actAdd: TAction;
    actUpdate: TAction;
    actDelete: TAction;
    btnPreview: TButton;
    actPreview: TAction;
    btnPredefined: TBitBtn;
    mnuPreDefCompilers: TPopupMenu;
    lblState: TLabel;
    edMinCompiler: TEdit;
    rbStateOff: TRadioButton;
    rbStateOn: TRadioButton;
    btnRestoreDefaults: TButton;
    actRestoreDefaults: TAction;
    ///  <summary>Creates new warning from data entered in controls and adds it
    ///  to warnings list.</summary>
    procedure actAddExecute(Sender: TObject);
    ///  <summary>Enables or disables "Add" action depending on validity of data
    ///  entered in controls.</summary>
    procedure actAddUpdate(Sender: TObject);
    ///  <summary>Deletes warning selected in list view from warnings list.
    ///  </summary>
    procedure actDeleteExecute(Sender: TObject);
    ///  <summary>Enables "Delete" action only if an item is selected in list
    ///  view.</summary>
    procedure actDeleteUpdate(Sender: TObject);
    ///  <summary>Displays a preview of $WARN compiler directives that will be
    ///  emitted by code generator.</summary>
    procedure actPreviewExecute(Sender: TObject);
    ///  <summary>Enables "Preview" action only if user has specified that $WARN
    ///  directives will be emitted.</summary>
    procedure actPreviewUpdate(Sender: TObject);
    ///  <summary>Restores default $WARN directives, replacing all directives
    ///  shown in list view.</summary>
    procedure actRestoreDefaultsExecute(Sender: TObject);
    ///  <summary>Updates warning selected in list view with data entered in
    ///  controls.</summary>
    procedure actUpdateExecute(Sender: TObject);
    ///  <summary>Enables or disables "Update" action depending on validity of
    ///  data enterend in controls.</summary>
    procedure actUpdateUpdate(Sender: TObject);
    ///  <summary>Displays menu of predefined compiler versions when
    ///  "pre-defined" button is clicked.</summary>
    procedure btnPredefinedClick(Sender: TObject);
    ///  <summary>Handles clicks on check box that enables or disables warnings
    ///  and updates warnings object with state.</summary>
    procedure chkWARNEnabledClick(Sender: TObject);
    ///  <summary>Inhibits non-numerical key presses in Min Compiler edit box.
    ///  </summary>
    procedure edMinCompilerKeyPress(Sender: TObject; var Key: Char);
  strict private
    var
      ///  <summary>Stores details of warnings.</summary>
      fWarnings: IWarnings;
      ///  <summary>References dynamically created list view control.</summary>
      fLVWarnings: TListViewEx;
    ///  <summary>Handles clicks on pre-defined compiler menu items. Stores
    ///  version number of selected compiler in associated edit control.
    ///  </summary>
    procedure PreDefCompilerMenuClick(Sender: TObject);
    ///  <summary>Handles selection of an item in warnings list view. Copies
    ///  details of selected warning into controls, ready for editing.</summary>
    procedure LVWarningsSelected(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    ///  <summary>Specifies that TWarningListItem is to be used to create new
    ///  list view items.</summary>
    procedure LVWarningsCreateItemClass(Sender: TCustomListView;
      var ItemClass: TListItemClass);
    ///  <summary>Compares two list items, Item1 and Item2, using text contained
    ///  in row specified Data parameter, recording result of comparison in
    ///  Compare parameter.</summary>
    procedure LVWarningsCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    ///  <summary>Returns text of list item LI in column specified by Idx.
    ///  </summary>
    ///  <remarks>Idx = 0 returns list item's caption while Idx > 0 returns text
    ///  of SubItem[Idx-1].</remarks>
    function LVColumnText(const LI: TListItem; const Idx: Integer): string;
    ///  <summary>Dynamically creates list view control and sets required
    ///  properties.</summary>
    procedure CreateLV;
    ///  <summary>Populates list view with details of warnings.</summary>
    procedure PopulateLV;
    ///  <summary>Adds details of given warning to list view.</summary>
    procedure AddWarningToLV(const Warning: TWarning);
    ///  <summary>Sets given list view item to display and record details of
    ///  given warning.</summary>
    procedure SetLVItem(const LI: TListItem; const Warning: TWarning);
    ///  <summary>Sorts list view using current sort column and direction.
    ///  </summary>
    ///  <remarks>Does nothing if list view hasn't already been sorted.
    ///  </remarks>
    procedure ReSortLV;
    ///  <summary>Returns given version number formatted as a string.</summary>
    function FormatCompilerVer(const Ver: Single): string;
    ///  <summary>Returns index of list view item associated with warning with
    ///  given symbol.</summary>
    ///  <remarks>Returns -1 if warning is not in list.</remarks>
    function IndexOfSymbolInLV(const Symbol: string): Integer;
    ///  <summary>Gets warning symbol entered in edit control.</summary>
    ///  <param name="Symbol">string [out] Required symbol, trimmed of white
    ///  space.</param>
    ///  <returns>Boolean. True if symbol is non-empty string, False if not.
    ///  </returns>
    function GetSymbol(out Symbol: string): Boolean;
    ///  <summary>Checks if symbol entered in edit control is valid.</summary>
    function IsValidSymbol: Boolean;
    ///  <summary>Gets compiler version entered in edit control.</summary>
    ///  <param name="Ver">Single [out] Required version number, undefined if
    ///  not valid.</param>
    ///  <returns>Boolean. True if version number is valid, False if not.
    ///  </returns>
    function GetCompilerVersion(out Ver: Single): Boolean;
    ///  <summary>Checks if compiler version entered in edit control is
    ///  valid.</summary>
    function IsValidCompilerVersion: Boolean;
    ///  <summary>Gets a warning state from radio buttons.</summary>
    function GetState: Boolean;
    ///  <summary>Checks if a warning state has been specified.</summary>
    function IsValidState: Boolean;
    ///  <summary>Populates pre-defined compiler version pop-up menu with menu
    ///  items.</summary>
    procedure PopulatePreDefCompilerMenu;
    ///  <summary>Selects required switch state in radio buttons.</summary>
    procedure SelectSwitchState(const State: Boolean);
    ///  <summary>Returns warning associated with selected list view item.
    ///  </summary>
    function SelectedWarning: TWarning;
    ///  <summary>Updates edit controls to reflect properties of warning
    ///  selected in list view.</summary>
    ///  <remarks>Nulls edit controls if no warning selected.</remarks>
    procedure UpdateControls;
  public
    ///  <summary>Constructs and initiases frame.</summary>
    constructor Create(AOwner: TComponent); override;
    ///  <summary>Records details of warnings from given preferences object and
    ///  updates controls accordingly.</summary>
    ///  <remarks>Called when page is activated.</remarks>
    procedure Activate(const Prefs: IPreferences; const Flags: UInt64);
      override;
    ///  <summary>Updates given preferences object with details of warnings as
    ///  modified by user.</summary>
    ///  <remarks>Called when page is deactivated.</remarks>
    procedure Deactivate(const Prefs: IPreferences); override;
    ///  <summary>Checks if preference changes require that main window UI is
    ///  updated.</summary>
    ///  <remarks>Called when dialog box containing frame is closing. Always
    ///  returns False because these preferences never affect UI.</remarks>
    function UIUpdated: Boolean; override;
    ///  <summary>Arranges controls on frame.</summary>
    ///  <remarks>Called after frame has been sized.</remarks>
    procedure ArrangeControls; override;
    ///  <summary>Returns a descriptive name for this frame.</summary>
    ///  <remarks>Used for display in the tab sheet that contains this frame
    ///  when displayed in the preferences dialog box.</remarks>
    function DisplayName: string; override;
    ///  <summary>Returns an index number that determines this frame's position
    ///  in relation to other preferences frames.</summary>
    ///  <remarks>Used by preferences dialog box to determine ordering of tabs
    ///  that contain the different preferences frames.</remarks>
    class function Index: Byte; override;
  end;


implementation


uses
  // Delphi
  SysUtils, Types,
  // Project
  FmPreferencesDlg, FmPreviewDlg, IntfCommon, UConsts, UCtrlArranger,
  UEncodings, UKeysHelper, UMessageBox, UStrUtils, UUtils;


{$R *.dfm}


type
  ///  <summary>Custom menu item with additional property to store a compiler
  ///  version.</summary>
  TPreDefCompilerMenuItem = class(TMenuItem)
  strict private
    var
      ///  <summary>Value of CompilerVer property</summary>
      fCompilerVer: Single;
  public
    ///  <summary>Constructs a menu item will all required properties and event
    ///  handlers.</summary>
    ///  <param name="AOwner">TComponent [in] Menu item's owner.</param>
    ///  <param name="ACaption">string [in] Menu item's caption. Should be name
    ///  of a compiler.</param>
    ///  <param name="ACompilerVer">Single [in] Version of compiler named in
    ///  ACaption</param>
    ///  <param name="AClickHandler">TNotifyEvent [in] Reference to an event
    ///  handler for menu item's OnClick event.</param>
    constructor Create(AOwner: TComponent; const ACaption: string;
      const ACompilerVer: Single; const AClickHandler: TNotifyEvent);
      reintroduce;
    ///  <summary>Version number of compiler whose name is displayed in menu
    ///  item's caption.</summary>
    property CompilerVer: Single read fCompilerVer write fCompilerVer;
  end;

type
  ///  <summary>Custom list item class that adds ability to store a TWarning
  ///  record with list item.</summary>
  TWarningListItem = class(TListItem)
  strict private
    var
      ///  <summary>Value of Warning property.</summary>
      fWarning: TWarning;
  public
    ///  <summary>Warning associated with list item.</summary>
    property Warning: TWarning read fWarning write fWarning;
  end;


{ TCodeGenPrefsFrame }

procedure TCodeGenPrefsFrame.actAddExecute(Sender: TObject);
var
  W: TWarning;          // new warning
  Symbol: string;       // name of warning's symbol
  CompilerVer: Single;  // min compiler version that supports warning
begin
  // Add new warning from edit controls to fWarnings and list view
  GetSymbol(Symbol);
  GetCompilerVersion(CompilerVer);
  W := TWarning.Create(Symbol, CompilerVer, GetState);
  fWarnings.Add(W);
  AddWarningToLV(W);
  ReSortLV;
  // Select new list item and make it visible
  fLVWarnings.Selected := fLVWarnings.Items[IndexOfSymbolInLV(Symbol)];
  fLVWarnings.Selected.MakeVisible(False);
end;

procedure TCodeGenPrefsFrame.actAddUpdate(Sender: TObject);
var
  CanAdd: Boolean; // flag that records whether a warning can be added to list

  ///  Checks if symbol entered in edit control is a duplicate of an existing
  ///  symbol in the list view.
  function SymbolIsDuplicate: Boolean;
  var
    Symbol: string; // symbol from edit control
  begin
    GetSymbol(Symbol);
    Result := IndexOfSymbolInLV(Symbol) >= 0;
  end;

begin
  CanAdd := True;
  if not IsValidSymbol then
    CanAdd := False
  else if SymbolIsDuplicate then
    CanAdd := False
  else if not IsValidCompilerVersion then
    CanAdd := False
  else if not IsValidState then
    CanAdd := False;
  actAdd.Enabled := CanAdd;
end;

procedure TCodeGenPrefsFrame.actDeleteExecute(Sender: TObject);
var
  Warning: TWarning;  // selected warning
begin
  Assert(Assigned(fLVWarnings.Selected),
    ClassName + '.actDeleteExecute: No list view item selected');
  // Delete selected warning
  Warning := SelectedWarning;
  fLVWarnings.Selected.Delete;
  fWarnings.Delete(Warning);
  // Ensure nothing selected in list view and clear edit controls
  fLVWarnings.Selected := nil;
  UpdateControls;
end;

procedure TCodeGenPrefsFrame.actDeleteUpdate(Sender: TObject);
begin
  actDelete.Enabled := Assigned(fLVWarnings.Selected);
end;

procedure TCodeGenPrefsFrame.Activate(const Prefs: IPreferences;
  const Flags: UInt64);
begin
  (fWarnings as IAssignable).Assign(Prefs.Warnings);
  chkWARNEnabled.Checked := fWarnings.Enabled;
  PopulateLV;
end;

procedure TCodeGenPrefsFrame.actPreviewExecute(Sender: TObject);
resourcestring
  sCaption = 'Compiler Directives Preview';
begin
  TPreviewDlg.Execute(
    Self.Owner,
    TEncodedData.Create(fWarnings.Render, etUnicode),
    dtPlainText,
    sCaption
  );
end;

procedure TCodeGenPrefsFrame.actPreviewUpdate(Sender: TObject);
begin
  actPreview.Enabled := (fLVWarnings.Items.Count > 0)
    and chkWARNEnabled.Checked;
end;

procedure TCodeGenPrefsFrame.actRestoreDefaultsExecute(Sender: TObject);
resourcestring
  // message displayed in confirmation dialogue box
  sConfirmAction = 'Are you sure you want to restore the default directives? '
    + EOL2 + 'All existing custom directives will be lost.';
begin
  if not TMessageBox.Confirm(Self, sConfirmAction) then
    Exit;
  fWarnings := TWarnings.Defaults;
  fWarnings.Enabled := chkWARNEnabled.Checked;
  PopulateLV;
end;

procedure TCodeGenPrefsFrame.actUpdateExecute(Sender: TObject);
var
  NewSymbol: string;      // new symbol from edit control
  NewCompilerVer: Single; // new compiler version from edit control
  NewState: Boolean;      // new warning state from radio buttons
  SelItem: TListItem;     // reference to selected item in list view
  OldWarning: TWarning;   // warning associated with selected list view item
  NewWarning: TWarning;   // updated warning
begin
  Assert(Assigned(fLVWarnings.Selected),
    ClassName + '.actUpdateExecute: no item selected in lvWarnings');
  GetSymbol(NewSymbol);
  GetCompilerVersion(NewCompilerVer);
  NewState := GetState;
  SelItem := fLVWarnings.Selected;
  OldWarning := SelectedWarning;
  NewWarning := TWarning.Create(NewSymbol, NewCompilerVer, NewState);
  SetLVItem(SelItem, NewWarning);
  ReSortLV;
  // we update warnings by deleting old one and adding updated version
  fWarnings.Delete(OldWarning);
  fWarnings.Add(NewWarning);
  // Ensure updated item is still selected in list view and is visible
  fLVWarnings.Selected := SelItem;
  SelItem.MakeVisible(False);
end;

procedure TCodeGenPrefsFrame.actUpdateUpdate(Sender: TObject);
var
  CanUpdate: Boolean;   // flag that indicates if a warning can be updated

  ///  Checks if symbol entered in edit control is a duplicate of an existing
  ///  symbol in the list view, other than that of the selected warning.
  function SymbolIsDuplicate: Boolean;
  var
    Symbol: string; // symbol entered in edit control
    LIIdx: Integer; // index of list item with Symbol as caption
  begin
    GetSymbol(Symbol);
    LIIdx := IndexOfSymbolInLV(Symbol);
    Result := (LIIdx >= 0) and (LIIdx <> fLVWarnings.Selected.Index);
  end;

begin
  CanUpdate := True;
  if fLVWarnings.Selected = nil then
    CanUpdate := False
  else if not IsValidSymbol then
    CanUpdate := False
  else if SymbolIsDuplicate then
    CanUpdate := False
  else if not IsValidCompilerVersion then
    CanUpdate := False
  else if not IsValidState then
    CanUpdate := False;
  actUpdate.Enabled := CanUpdate;
end;

procedure TCodeGenPrefsFrame.AddWarningToLV(const Warning: TWarning);
var
  LI: TListItem;  // new list item for warning
begin
  LI := fLVWarnings.Items.Add;
  SetLVItem(LI, Warning);
end;

procedure TCodeGenPrefsFrame.ArrangeControls;
begin
  btnPreview.Left := Width - btnPreview.Width;
  fLVWarnings.Width := Width;
  TCtrlArranger.AlignVCentres(0, [chkWARNEnabled, btnPreview]);
  fLVWarnings.Top := TCtrlArranger.BottomOf([chkWARNEnabled, btnPreview], 8);
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(fLVWarnings, 8),
    [lblSymbol, edSymbol]
  );
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf([lblSymbol, edSymbol], 8),
    [lblMinCompiler, edMinCompiler, btnPredefined]
  );
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf([lblMinCompiler, edMinCompiler, btnPredefined], 8),
    [lblState, rbStateOff, rbStateOn]
  );
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf([lblState, rbStateOff, rbStateOn], 8),
    [btnAdd, btnDelete, btnUpdate, btnRestoreDefaults]
  );
  btnRestoreDefaults.Left :=
    fLVWarnings.Left + fLVWarnings.Width - btnRestoreDefaults.Width;
end;

procedure TCodeGenPrefsFrame.btnPredefinedClick(Sender: TObject);
var
  PopupPos: TPoint; // place where menu pops up
begin
  PopupPos := ClientToScreen(
    Point(btnPredefined.Left, btnPredefined.Top + btnPredefined.Height)
  );
  mnuPreDefCompilers.Popup(PopupPos.X, PopupPos.Y);
end;

procedure TCodeGenPrefsFrame.chkWARNEnabledClick(Sender: TObject);
begin
  fWarnings.Enabled := chkWARNEnabled.Checked;
end;

constructor TCodeGenPrefsFrame.Create(AOwner: TComponent);
begin
  inherited;
  HelpKeyword := 'CodeGenPrefs';
  fWarnings := TWarnings.Create;
  PopulatePreDefCompilerMenu;
  CreateLV;
end;

procedure TCodeGenPrefsFrame.CreateLV;
resourcestring
  // column header captions
  sSymbolColCaption = 'Symbol';
  sMinCompilerColCaption = 'Min. Compiler';
  sStateColCaption = 'State';
begin
  fLVWarnings := TListViewEx.Create(Self);
  with fLVWarnings do
  begin
    Parent := Self;
    Height := 150;
    Left := 0;
    HideSelection := False;
    ReadOnly := True;
    RowSelect := True;
    TabOrder := 2;
    ViewStyle := vsReport;
    SortImmediately := False;
    with Columns.Add do
    begin
      Caption := sSymbolColCaption;
      Width := 240;
    end;
    with Columns.Add do
    begin
      Caption := sMinCompilerColCaption;
      Width := 100;
    end;
    with Columns.Add do
    begin
      Caption := sStateColCaption;
      Width := 50;
    end;
    OnSelectItem := LVWarningsSelected;
    OnCompare := LVWarningsCompare;
    OnCreateItemClass := LVWarningsCreateItemClass;
  end;
end;

procedure TCodeGenPrefsFrame.Deactivate(const Prefs: IPreferences);
begin
  fWarnings.Enabled := chkWARNEnabled.Checked;
  Prefs.Warnings := fWarnings;
end;

function TCodeGenPrefsFrame.DisplayName: string;
resourcestring
  sDisplayName = 'Code Generation';
begin
  Result := sDisplayName;
end;

procedure TCodeGenPrefsFrame.edMinCompilerKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not IsValidDecimalNumberKey((Sender as TEdit).Text, Key) then
    KeyErrorBeep;
end;

function TCodeGenPrefsFrame.FormatCompilerVer(const Ver: Single): string;
begin
  Result := Format('%.2f', [Ver]);
end;

function TCodeGenPrefsFrame.GetCompilerVersion(out Ver: Single): Boolean;
var
  ExtVer: Extended;   // version number as Extended float
begin
  Result := TryStrToFloat(StrTrim(edMinCompiler.Text), ExtVer);
  if Result then
    Ver := ExtVer;
end;

function TCodeGenPrefsFrame.GetState: Boolean;
begin
  // Only valid iff one radio button is checked
  Result := rbStateOn.Checked;
end;

function TCodeGenPrefsFrame.GetSymbol(out Symbol: string): Boolean;
begin
  Symbol := StrTrim(edSymbol.Text);
  Result := Symbol <> '';
end;

class function TCodeGenPrefsFrame.Index: Byte;
begin
  Result := 15;
end;

function TCodeGenPrefsFrame.IndexOfSymbolInLV(const Symbol: string): Integer;
var
  LI: TListItem;  // each list item in list view
begin
  for LI in fLVWarnings.Items do
  begin
    if StrSameText(Symbol, LI.Caption) then
      Exit(LI.Index);
  end;
  Result := -1;
end;

function TCodeGenPrefsFrame.IsValidCompilerVersion: Boolean;
var
  Ver: Single;  // compiler version from edit control
begin
  Result := GetCompilerVersion(Ver);
  if Result then
    Result := Ver >= TWarning.MinSupportedCompiler;
end;

function TCodeGenPrefsFrame.IsValidState: Boolean;
begin
  Result := rbStateOff.Checked <> rbStateOn.Checked;
end;

function TCodeGenPrefsFrame.IsValidSymbol: Boolean;
var
  Symbol: string; // stores symbol: unused
begin
  Result := GetSymbol(Symbol);
end;

function TCodeGenPrefsFrame.LVColumnText(const LI: TListItem;
  const Idx: Integer): string;
begin
  if Idx = 0 then
    Result := LI.Caption
  else
    Result := LI.SubItems[Idx - 1];
end;

procedure TCodeGenPrefsFrame.LVWarningsCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  Compare := StrCompareText(
    LVColumnText(Item1, Data + 1), LVColumnText(Item2, Data + 1)
  );
  if fLVWarnings.SortOrder = soDown then
    Compare := -Compare;
end;

procedure TCodeGenPrefsFrame.LVWarningsCreateItemClass(Sender: TCustomListView;
  var ItemClass: TListItemClass);
begin
  ItemClass := TWarningListItem;
end;

procedure TCodeGenPrefsFrame.LVWarningsSelected(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected and Assigned(fLVWarnings.Selected) then
    UpdateControls;
end;

procedure TCodeGenPrefsFrame.PopulateLV;
var
  W: TWarning;  // references each warning
begin
  fLVWarnings.Items.BeginUpdate;
  try
    fLVWarnings.Clear;
    for W in fWarnings do
      AddWarningToLV(W);
    fLVWarnings.Selected := nil;
    ReSortLV;
  finally
    fLVWarnings.Items.EndUpdate;
  end;
  UpdateControls;
end;

procedure TCodeGenPrefsFrame.PopulatePreDefCompilerMenu;

  ///  Adds a menu item for given compiler name and version to the pop-up menu.
  procedure AddMenuItem(const Compiler: string; const Ver: Single);
  begin
    mnuPreDefCompilers.Items.Add(
      TPreDefCompilerMenuItem.Create(
        mnuPreDefCompilers, Compiler, Ver, PreDefCompilerMenuClick
      )
    );
  end;

begin
  AddMenuItem('Delphi 6', 14.0);
  AddMenuItem('Delphi 7', 15.0);
  AddMenuItem('Delphi 2005', 17.0);
  AddMenuItem('Delphi 2006', 18.0);
  AddMenuItem('Delphi 2007 (Win 32)', 18.5);
  AddMenuItem('Delphi 2009', 20.0);
  AddMenuItem('Delphi 2010', 21.0);
  AddMenuItem('Delphi XE', 22.0);
  AddMenuItem('Delphi XE2', 23.0);
  AddMenuItem('Delphi XE3', 24.0);
  AddMenuItem('Delphi XE4', 25.0);
  AddMenuItem('Delphi XE5', 26.0);
  AddMenuItem('Delphi XE6', 27.0);
  AddMenuItem('Delphi XE7', 28.0);
  AddMenuItem('Delphi XE8', 29.0);
  AddMenuItem('Delphi 10 Seattle', 30.0);
  AddMenuItem('Delphi 10.1 Berlin', 31.0);
  AddMenuItem('Delphi 10.2 Tokyo', 32.0);
  AddMenuItem('Delphi 10.3 Rio', 33.0);
  AddMenuItem('Delphi 10.4 Sydney', 34.0);
  AddMenuItem('Delphi 11 Alexandria', 35.0);
end;

procedure TCodeGenPrefsFrame.PreDefCompilerMenuClick(Sender: TObject);
begin
  edMinCompiler.Text := FormatCompilerVer(
    (Sender as TPreDefCompilerMenuItem).CompilerVer
  );
end;

procedure TCodeGenPrefsFrame.ReSortLV;
begin
  if fLVWarnings.SortColumn <> -1 then
    fLVWarnings.CustomSort(nil, fLVWarnings.SortColumn - 1);
end;

function TCodeGenPrefsFrame.SelectedWarning: TWarning;
begin
  Assert(Assigned(fLVWarnings.Selected),
    ClassName + '.SelectedWarning: No warning selected in list view');
  Result := (fLVWarnings.Selected as TWarningListItem).Warning;
end;

procedure TCodeGenPrefsFrame.SelectSwitchState(const State: Boolean);
begin
  if State then
    rbStateOn.Checked := True
  else
    rbStateOff.Checked := True;
end;

procedure TCodeGenPrefsFrame.SetLVItem(const LI: TListItem;
  const Warning: TWarning);
resourcestring
  sOn = 'On';
  sOff = 'Off';
const
  // Map of warning state onto description
  StateDescs: array[Boolean] of string = (sOff, sOn);
begin
  LI.Caption := Warning.Symbol;
  while LI.SubItems.Count < 2 do
    LI.SubItems.Add('');
  LI.SubItems[0] := FormatCompilerVer(Warning.MinCompiler);
  LI.SubItems[1] := StateDescs[Warning.State];
  (LI as TWarningListItem).Warning := Warning;
end;

function TCodeGenPrefsFrame.UIUpdated: Boolean;
begin
  Result := False;
end;

procedure TCodeGenPrefsFrame.UpdateControls;
begin
  if Assigned(fLVWarnings.Selected) then
  begin
    edSymbol.Text := SelectedWarning.Symbol;
    edMinCompiler.Text := FormatCompilerVer(SelectedWarning.MinCompiler);
    SelectSwitchState(SelectedWarning.State);
  end
  else
  begin
    edSymbol.Text := '';
    edMinCompiler.Text := '';
    rbStateOff.Checked := True;
  end;
end;

{ TPreDefCompilerMenuItem }

constructor TPreDefCompilerMenuItem.Create(AOwner: TComponent;
const ACaption: string; const ACompilerVer: Single;
  const AClickHandler: TNotifyEvent);
begin
  inherited Create(AOwner);
  Caption := ACaption;
  CompilerVer := ACompilerVer;
  OnClick := AClickHandler;
end;

initialization

// Register frame with preferences dialog box
TPreferencesDlg.RegisterPage(TCodeGenPrefsFrame);

end.

