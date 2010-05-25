{
 * FrCodeGenPrefs.pas
 *
 * Implements a frame that allows user to set source code generation
 * preferences. Designed for use as one of the tabs in the preferences dialog
 * box.
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
 * The Original Code is FrCodeGenPrefs.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FrCodeGenPrefs;

interface

uses
  // Delphi
  Classes, ActnList, StdCtrls, Controls, ComCtrls, Menus, Buttons,
  // Project
  FrPrefsBase, UPreferences, UWarnings;

type

  TCodeGenPrefsFrame = class(TPrefsBaseFrame)
    chkSwitchOff: TCheckBox;
    lvWarnings: TListView;
    lblSymbol: TLabel;
    lblMinCompiler: TLabel;
    edSymbol: TEdit;
    edMinCompiler: TEdit;
    btnAdd: TButton;
    btnDelete: TButton;
    btnUpdate: TButton;
    actMain: TActionList;
    actAdd: TAction;
    actUpdate: TAction;
    actDelete: TAction;
    btnPreview: TButton;
    actPreview: TAction;
    btnPredefined: TBitBtn;
    mnuPreDefCompilers: TPopupMenu;
    procedure actAddExecute(Sender: TObject);
    procedure actAddUpdate(Sender: TObject);
    procedure actUpdateExecute(Sender: TObject);
    procedure actUpdateUpdate(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actDeleteUpdate(Sender: TObject);
    procedure chkSwitchOffClick(Sender: TObject);
    procedure lvWarningsClick(Sender: TObject);
    procedure edMinCompilerKeyPress(Sender: TObject; var Key: Char);
    procedure actPreviewExecute(Sender: TObject);
    procedure actPreviewUpdate(Sender: TObject);
    procedure btnPredefinedClick(Sender: TObject);
  private
    fWarnings: IWarnings;
    procedure PopulateLV;
    procedure AddWarningToLV(const Warning: TWarning);
    function FormatCompilerVer(const Ver: Single): string;
    function IndexOfSymbolInLV(const Symbol: string): Integer;
    function GetSymbol(out Symbol: string): Boolean;
    function IsValidSymbol: Boolean;
    function GetCompilerVersion(out Ver: Single): Boolean;
    function IsValidCompilerVersion: Boolean;
    procedure PopulatePreDefCompilerMenu;
    procedure PreDefCompilerMenuClick(Sender: TObject);
  public
    // todo: complete and revise all method comments
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets up frame and populates controls.
        @param AOwner [in] Component that owns frame.
      }
    procedure Activate(const Prefs: IPreferences); override;
      {Called when page activated. Must update controls from preferences.
        @param Prefs [in] Object that provides info used to update controls.
      }
    procedure Deactivate(const Prefs: IPreferences); override;
      {Called when page is deactivated. Must store information entered by user.
        @param Prefs [in] Object used to store information.
      }
    procedure ArrangeControls; override;
      {Arranges controls on frame. Called after frame has been sized.
      }
    function DisplayName: string; override;
      {Provides caption that is displayed in the tab sheet that contains this
      frame when displayed in the preference dialog box.
        @return Required display name.
      }
    class function Index: Byte; override;
      {Provides an index number that determines the order in which the tabs
      containing frames are displayed in the preferences dialog box. Gaps
      between indexes should be left where possible to allow for insertion of
      new entries at a later date.
        @return Required index number.
      }
  end;


implementation

uses
  // Delphi
  SysUtils, StrUtils, Types,
  // Project
  FmPreferencesDlg, FmPreviewDlg, IntfCommon, UConsts, UCtrlArranger,
  UUnicodeHelper, UUtils;

{$R *.dfm}

type
  TPreDefCompilerMenuItem = class(TMenuItem)
  strict private
    fCompilerVer: Single;
  public
    constructor Create(AOwner: TComponent; const ACaption: string;
      const ACompilerVer: Single; const AClickHandler: TNotifyEvent);
      reintroduce;
    property CompilerVer: Single read fCompilerVer write fCompilerVer;
  end;


{ TCodeGenPrefsFrame }

procedure TCodeGenPrefsFrame.actAddExecute(Sender: TObject);
var
  W: TWarning;
  Symbol: string;
  CompilerVer: Single;
begin
  // Add new warning from edit controls to fWarnings and list view
  GetSymbol(Symbol);
  GetCompilerVersion(CompilerVer);
  W := TWarning.Create(Symbol, CompilerVer);
  fWarnings.Add(W);
  AddWarningToLV(W);
end;

procedure TCodeGenPrefsFrame.actAddUpdate(Sender: TObject);
var
  CanUpdate: Boolean;

  function SymbolIsDuplicate: Boolean;
  var
    Symbol: string;
  begin
    GetSymbol(Symbol);
    Result := IndexOfSymbolInLV(Symbol) >= 0;
  end;

begin
  // Can add if (1) Symbol is non-empty and isn't already in list (2) compiler
  // version is valid
  CanUpdate := True;
  if not IsValidSymbol then
    CanUpdate := False
  else if SymbolIsDuplicate then
    CanUpdate := False
  else if not IsValidCompilerVersion then
    CanUpdate := False;
  actAdd.Enabled := CanUpdate;
end;

procedure TCodeGenPrefsFrame.actDeleteExecute(Sender: TObject);
var
  Symbol: string;
begin
  // Delete selected warning
  Symbol := Trim(lvWarnings.Selected.Caption);
  lvWarnings.Selected.Delete;
  fWarnings.Delete(Symbol);
end;

procedure TCodeGenPrefsFrame.actDeleteUpdate(Sender: TObject);
begin
  actDelete.Enabled := Assigned(lvWarnings.Selected);
end;

procedure TCodeGenPrefsFrame.Activate(const Prefs: IPreferences);
begin
  (fWarnings as IAssignable).Assign(Prefs.Warnings);
  chkSwitchOff.Checked := fWarnings.SwitchOff;
  PopulateLV;
end;

procedure TCodeGenPrefsFrame.actPreviewExecute(Sender: TObject);
resourcestring
  sCaption = 'Conditional compilation code';
begin
  TPreviewDlg.Execute(Self.Owner, fWarnings.Render, sCaption);
end;

procedure TCodeGenPrefsFrame.actPreviewUpdate(Sender: TObject);
begin
  actPreview.Enabled := (lvWarnings.Items.Count > 0) and chkSwitchOff.Checked;
end;

procedure TCodeGenPrefsFrame.actUpdateExecute(Sender: TObject);
var
  OldSymbol: string;
  NewSymbol: string;
  NewCompilerVer: Single;
  SelItem: TListItem;
begin
  // Update selected warning with new values: update display and warning list
  GetSymbol(NewSymbol);
  GetCompilerVersion(NewCompilerVer);
  SelItem := lvWarnings.Selected;
  OldSymbol := Trim(SelItem.Caption);
  SelItem.Caption := NewSymbol;
  SelItem.SubItems[0] := FormatCompilerVer(NewCompilerVer);
  fWarnings.Delete(OldSymbol);
  fWarnings.Add(TWarning.Create(NewSymbol, NewCompilerVer));
  lvWarnings.Selected := SelItem;
end;

procedure TCodeGenPrefsFrame.actUpdateUpdate(Sender: TObject);
var
  CanUpdate: Boolean;

  function SymbolIsDuplicate: Boolean;
  var
    LIIdx: Integer;
    Symbol: string;
  begin
    GetSymbol(Symbol);
    LIIdx := IndexOfSymbolInLV(Symbol);
    Result := (LIIdx >= 0) and (LIIdx <> lvWarnings.Selected.Index);
  end;

begin
  // Can edit if (1) Symbol is non-empty (2) Symbol is not already in list,
  // except for selected entry (3) compiler version is valid
  CanUpdate := True;
  if lvWarnings.Selected = nil then
    CanUpdate := False
  else if not IsValidSymbol then
    CanUpdate := False
  else if SymbolIsDuplicate then
    CanUpdate := False
  else if not IsValidCompilerVersion then
    CanUpdate := False;
  actUpdate.Enabled := CanUpdate;
end;

procedure TCodeGenPrefsFrame.AddWarningToLV(const Warning: TWarning);
var
  LI: TListItem;
begin
  LI := lvWarnings.Items.Add;
  LI.Caption := Warning.Symbol;
  LI.SubItems.Add(FormatCompilerVer(Warning.MinCompiler));
end;

procedure TCodeGenPrefsFrame.ArrangeControls;
begin
  btnPreview.Left := Width - btnPreview.Width;
  lvWarnings.Width := Width;
  TCtrlArranger.AlignVCentres(0, [chkSwitchOff, btnPreview]);
  lvWarnings.Top := TCtrlArranger.BottomOf([chkSwitchOff, btnPreview], 8);
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(lvWarnings, 8),
    [lblSymbol, edSymbol]
  );
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf([lblSymbol, edSymbol], 8),
    [lblMinCompiler, edMinCompiler, btnPredefined]
  );
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf([lblMinCompiler, edMinCompiler, btnPredefined], 8),
    [btnAdd, btnDelete, btnUpdate]
  );
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

procedure TCodeGenPrefsFrame.chkSwitchOffClick(Sender: TObject);
begin
  fWarnings.SwitchOff := chkSwitchOff.Checked;
end;

constructor TCodeGenPrefsFrame.Create(AOwner: TComponent);
begin
  inherited;
  HelpKeyword := 'CodeGenPrefs';
  fWarnings := TWarnings.Create;
  PopulatePreDefCompilerMenu;
end;

procedure TCodeGenPrefsFrame.Deactivate(const Prefs: IPreferences);
begin
  fWarnings.SwitchOff := chkSwitchOff.Checked;
  Prefs.Warnings := fWarnings;
end;

function TCodeGenPrefsFrame.DisplayName: string;
resourcestring
  sDisplayName = 'Code Generation'; // display name
begin
  Result := sDisplayName;
end;

procedure TCodeGenPrefsFrame.edMinCompilerKeyPress(Sender: TObject;
  var Key: Char);
  {Called whenever a key is pressed in a minimum compiler edit control. We
  prevent all but valid number keys, a single decimal point and backspace.
    @param Sender [in] Not used.
    @param Key [in/out] Key that was pressed. Set to #0 to inhibit if not valid.
  }
begin
  { TODO: extract common code from here and FrPrintingPrefs.NumEditKeyPress
          and move into suitable routine in UKeysHelper }
  if Key = DecimalSeparator then
  begin
    // Only allow decimal point if not already entered (can't have > 1)
    if AnsiContainsStr((Sender as TEdit).Text, DecimalSeparator) then
      Key := #0;
  end
  else if not IsDigit(Key) and (Key <> BACKSPACE) then
    // Disallow any other characters other than backspace or digits
    Key := #0;
  if Key = #0 then
    KeyErrorBeep;
end;

function TCodeGenPrefsFrame.FormatCompilerVer(const Ver: Single): string;
begin
  Result := Format('%.2f', [Ver]);
end;

function TCodeGenPrefsFrame.GetCompilerVersion(out Ver: Single): Boolean;
var
  ExtVer: Extended;
begin
  Result := TryStrToFloat(Trim(edMinCompiler.Text), ExtVer);
  if Result then
    Ver := ExtVer;
end;

function TCodeGenPrefsFrame.GetSymbol(out Symbol: string): Boolean;
begin
  Symbol := Trim(edSymbol.Text);
  Result := Symbol <> '';
end;

class function TCodeGenPrefsFrame.Index: Byte;
begin
  Result := 15;
end;

function TCodeGenPrefsFrame.IndexOfSymbolInLV(const Symbol: string): Integer;
var
  LI: TListItem;
begin
  for LI in lvWarnings.Items do
  begin
    if AnsiSameText(Symbol, LI.Caption) then
      Exit(LI.Index);
  end;
  Result := -1;
end;

function TCodeGenPrefsFrame.IsValidCompilerVersion: Boolean;
var
  Ver: Single;
begin
  Result := GetCompilerVersion(Ver);
  if Result then
    Result := Ver >= TWarning.MinSupportedCompiler;
end;

function TCodeGenPrefsFrame.IsValidSymbol: Boolean;
var
  Symbol: string;
begin
  Result := GetSymbol(Symbol);
end;

procedure TCodeGenPrefsFrame.lvWarningsClick(Sender: TObject);
begin
  if not Assigned(lvWarnings.Selected) then
    Exit;
  edSymbol.Text := lvWarnings.Selected.Caption;
  edMinCompiler.Text := lvWarnings.Selected.SubItems[0];
end;

procedure TCodeGenPrefsFrame.PopulateLV;
var
  W: TWarning;
begin
  lvWarnings.Items.BeginUpdate;
  try
    lvWarnings.Clear;
    for W in fWarnings do
      AddWarningToLV(W);
  finally
    lvWarnings.Items.EndUpdate;
  end;
end;

procedure TCodeGenPrefsFrame.PopulatePreDefCompilerMenu;

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
end;

procedure TCodeGenPrefsFrame.PreDefCompilerMenuClick(Sender: TObject);
begin
  edMinCompiler.Text := FormatCompilerVer(
    (Sender as TPreDefCompilerMenuItem).CompilerVer
  );
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
