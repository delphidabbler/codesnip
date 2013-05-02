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
 * Portions created by the Initial Developer are Copyright (C) 2010-2013 Peter
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
    fWarnings: IWarnings; // Object that stores details of warnings
    procedure PopulateLV;
      {Populates list view with details of warnings.
      }
    procedure AddWarningToLV(const Warning: TWarning);
      {Adds details of a warning to list view.
        @param Warning [in] Warning to be added.
      }
    function FormatCompilerVer(const Ver: Single): string;
      {Formats compiler version number as a string.
        @param Ver [in] Version number to be formatted.
        @return Formatted string.
      }
    function IndexOfSymbolInLV(const Symbol: string): Integer;
      {Gets the index of the list item containing a warning symbol in the list
      view.
        @param Symbol [in] Symbol to be found.
        @return Index of required list view item or -1 if not found.
      }
    function GetSymbol(out Symbol: string): Boolean;
      {Gets warning symbol from edit control.
        @param Symbol [out] Required symbol, trimmed of white space. Undefined
          if not valid.
        @return True if symbol is non-empty string, False if not.
      }
    function IsValidSymbol: Boolean;
      {Checks if symbol entered in edit control is valid.
        @return True if valid, False if not.
      }
    function GetCompilerVersion(out Ver: Single): Boolean;
      {Gets a compiler version number from edit control.
        @param Ver [out] Required version number. Undefined if not valid.
        @return True if version number is valid, False if not.
      }
    function IsValidCompilerVersion: Boolean;
      {Checks if compiler version entered in edit control is valid.
        @return True if valid, False if not.
      }
    procedure PopulatePreDefCompilerMenu;
      {Populates pre-defined compiler version pop-up menu with menu items.
      }
    procedure PreDefCompilerMenuClick(Sender: TObject);
      {Handler for click events on any pre-defined compiler menu item. Stores
      required compiler version in associated edit control.
        @param Sender [in] Reference to clicked menu item.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Constructor. Sets up frame and populates controls.
        @param AOwner [in] Component that owns frame.
      }
    procedure Activate(const Prefs: IPreferences); override;
      {Called when page activated. Records details of warnings and updates
      controls accordingly.
        @param Prefs [in] Object that provides info used to update controls.
      }
    procedure Deactivate(const Prefs: IPreferences); override;
      {Called when page is deactivated. Updates warnings and preferences objects
      from information entered in tab.
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
      containing frames are displayed in the preferences dialog box.
        @return Required index number.
      }
  end;


implementation

uses
  // Delphi
  SysUtils, Types,
  // Project
  FmPreferencesDlg, FmPreviewDlg, IntfCommon, UCtrlArranger, UKeysHelper,
  UUtils;

{$R *.dfm}

type
  {
  TPreDefCompilerMenuItem:
    Custom menu item with additional property to store a compiler version and
    extended constructor that takes all required properties and event handlers.
  }
  TPreDefCompilerMenuItem = class(TMenuItem)
  strict private
    fCompilerVer: Single; // Value of CompilerVer property
  public
    constructor Create(AOwner: TComponent; const ACaption: string;
      const ACompilerVer: Single; const AClickHandler: TNotifyEvent);
      reintroduce;
      {Constructor. Used to fully create a menu item with all required
      properties and event handlers.
        @param AOwner [in] Component that owns menu item.
        @param ACaption [in] Name of compiler displayed in menu item's caption.
        @param ACompilerVer [in] Version number of compiler named in ACaption.
        @param AClickHandler [in] Menu item's OnClick event handler.
      }
    property CompilerVer: Single read fCompilerVer write fCompilerVer;
      {Version number of compiler whose name is displayed in menu item's
      Caption}
  end;


{ TCodeGenPrefsFrame }

procedure TCodeGenPrefsFrame.actAddExecute(Sender: TObject);
  {Creates a new warning from data entered in edit controls and adds it to the
  list view and records in warnings list.
    @param Sender [in] Not used.
  }
var
  W: TWarning;          // new warning
  Symbol: string;       // name of warning's symbol
  CompilerVer: Single;  // min compiler version that supports warning
begin
  // Add new warning from edit controls to fWarnings and list view
  GetSymbol(Symbol);
  GetCompilerVersion(CompilerVer);
  W := TWarning.Create(Symbol, CompilerVer);
  fWarnings.Add(W);
  AddWarningToLV(W);
  // Select new list item and make it visible
  lvWarnings.Selected := lvWarnings.Items[IndexOfSymbolInLV(Symbol)];
  lvWarnings.Selected.MakeVisible(False);
end;

procedure TCodeGenPrefsFrame.actAddUpdate(Sender: TObject);
  {Updates state of action that creates and records a warning. Action enabled
  only if data in edit controls is valid.
    @param Sender [in] Not used.
  }
var
  CanAdd: Boolean; // flag that records whether a warning can be added to list

  // ---------------------------------------------------------------------------
  function SymbolIsDuplicate: Boolean;
    {Checks if symbol entered in edit control is a duplicate of an existing
    symbol in the list view.
      @return True if duplicate, False if not.
    }
  var
    Symbol: string; // symbol from edit control
  begin
    GetSymbol(Symbol);
    Result := IndexOfSymbolInLV(Symbol) >= 0;
  end;
  // ---------------------------------------------------------------------------

begin
  CanAdd := True;
  if not IsValidSymbol then
    CanAdd := False
  else if SymbolIsDuplicate then
    CanAdd := False
  else if not IsValidCompilerVersion then
    CanAdd := False;
  actAdd.Enabled := CanAdd;
end;

procedure TCodeGenPrefsFrame.actDeleteExecute(Sender: TObject);
  {Deletes selected warning from warnings list and list view.
    @param Sender [in] Not used.
  }
var
  Symbol: string; // symbol of selected warning
begin
  // Delete selected warning
  Symbol := Trim(lvWarnings.Selected.Caption);
  lvWarnings.Selected.Delete;
  fWarnings.Delete(Symbol);
  // Ensure nothing selected in list view and clear edit controls
  lvWarnings.Selected := nil;
  edSymbol.Text := '';
  edMinCompiler.Text := '';
end;

procedure TCodeGenPrefsFrame.actDeleteUpdate(Sender: TObject);
  {Updates state of action that deletes a warning. Action enabled only if an
  item is selected in list view.
    @param Sender [in] Not used.
  }
begin
  actDelete.Enabled := Assigned(lvWarnings.Selected);
end;

procedure TCodeGenPrefsFrame.Activate(const Prefs: IPreferences);
  {Called when page activated. Records details of warnings and updates controls
  accordingly.
    @param Prefs [in] Object that provides info used to update controls.
  }
begin
  (fWarnings as IAssignable).Assign(Prefs.Warnings);
  chkSwitchOff.Checked := fWarnings.SwitchOff;
  PopulateLV;
end;

procedure TCodeGenPrefsFrame.actPreviewExecute(Sender: TObject);
  {Displays compiler directives used to switch off listed warnings in a dialog
  box.
    @param Sender [in] Not used.
  }
resourcestring
  sCaption = 'Compiler Directives Preview';
begin
  TPreviewDlg.Execute(Self.Owner, fWarnings.Render, sCaption);
end;

procedure TCodeGenPrefsFrame.actPreviewUpdate(Sender: TObject);
  {Updates state of action that displays preview of compiler directives. Enabled
  only if switching off of warnings is allowed and there are warnings in list
  view.
    @param Sender [in] Not used.
  }
begin
  actPreview.Enabled := (lvWarnings.Items.Count > 0) and chkSwitchOff.Checked;
end;

procedure TCodeGenPrefsFrame.actUpdateExecute(Sender: TObject);
  {Updates selected warning from data entered in edit controls.
    @param Sender [in] Not used.
  }
var
  OldSymbol: string;      // symbol associated with selected warning
  NewSymbol: string;      // new symbol from edit control
  NewCompilerVer: Single; // new compiler version from edit control
  SelItem: TListItem;     // reference to selected item in list view
begin
  // Update selected warning with new values: update display and warning list
  GetSymbol(NewSymbol);
  GetCompilerVersion(NewCompilerVer);
  SelItem := lvWarnings.Selected;
  OldSymbol := Trim(SelItem.Caption);
  SelItem.Caption := NewSymbol;
  SelItem.SubItems[0] := FormatCompilerVer(NewCompilerVer);
  // we update warnings by deleting old one and adding updated version
  fWarnings.Delete(OldSymbol);
  fWarnings.Add(TWarning.Create(NewSymbol, NewCompilerVer));
  // Ensure updated item is still selected in list view and is visible
  lvWarnings.Selected := SelItem;
  SelItem.MakeVisible(False);
end;

procedure TCodeGenPrefsFrame.actUpdateUpdate(Sender: TObject);
  {Updates state of action that updates a warning. Action enabled only if data
  in edit controls is valid.
    @param Sender [in] Not used.
  }
var
  CanUpdate: Boolean;   // flag that indicates if a warning can be updated

  // ---------------------------------------------------------------------------
  function SymbolIsDuplicate: Boolean;
    {Checks if symbol entered in edit control is a duplicate of an existing
    symbol in the list view, other than that of the selected warning.
      @return True if duplicate, False if not.
    }
  var
    Symbol: string; // symbol entered in edit control
    LIIdx: Integer; // index of list item with Symbol as caption
  begin
    GetSymbol(Symbol);
    LIIdx := IndexOfSymbolInLV(Symbol);
    Result := (LIIdx >= 0) and (LIIdx <> lvWarnings.Selected.Index);
  end;
  // ---------------------------------------------------------------------------

begin
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
  {Adds details of a warning to list view.
    @param Warning [in] Warning to be added.
  }
var
  LI: TListItem;  // new list item for warning
begin
  LI := lvWarnings.Items.Add;
  LI.Caption := Warning.Symbol;
  LI.SubItems.Add(FormatCompilerVer(Warning.MinCompiler));
end;

procedure TCodeGenPrefsFrame.ArrangeControls;
  {Arranges controls on frame. Called after frame has been sized.
  }
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
  {Click event handler for pre-defined compiler versions button. Drops down
  associated popup menu.
    @param Sender [in] Not used.
  }
var
  PopupPos: TPoint; // place where menu pops up
begin
  PopupPos := ClientToScreen(
    Point(btnPredefined.Left, btnPredefined.Top + btnPredefined.Height)
  );
  mnuPreDefCompilers.Popup(PopupPos.X, PopupPos.Y);
end;

procedure TCodeGenPrefsFrame.chkSwitchOffClick(Sender: TObject);
  {Click event handler for "switch off warnings" check box. Updated warnings
  object per state of check box.
    @param Sender [in] Not used.
  }
begin
  fWarnings.SwitchOff := chkSwitchOff.Checked;
end;

constructor TCodeGenPrefsFrame.Create(AOwner: TComponent);
  {Constructor. Sets up frame and populates controls.
    @param AOwner [in] Component that owns frame.
  }
begin
  inherited;
  HelpKeyword := 'CodeGenPrefs';
  fWarnings := TWarnings.Create;
  PopulatePreDefCompilerMenu;
end;

procedure TCodeGenPrefsFrame.Deactivate(const Prefs: IPreferences);
  {Called when page is deactivated. Updates warnings and preferences objects
  from information entered in tab.
    @param Prefs [in] Object used to store information.
  }
begin
  fWarnings.SwitchOff := chkSwitchOff.Checked;
  Prefs.Warnings := fWarnings;
end;

function TCodeGenPrefsFrame.DisplayName: string;
  {Provides caption that is displayed in the tab sheet that contains this frame
  when displayed in the preference dialog box.
    @return Required display name.
  }
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
  if not IsValidDecimalNumberKey((Sender as TEdit).Text, Key) then
    KeyErrorBeep;
end;

function TCodeGenPrefsFrame.FormatCompilerVer(const Ver: Single): string;
  {Formats compiler version number as a string.
    @param Ver [in] Version number to be formatted.
    @return Formatted string.
  }
begin
  Result := Format('%.2f', [Ver]);
end;

function TCodeGenPrefsFrame.GetCompilerVersion(out Ver: Single): Boolean;
  {Gets a compiler version number from edit control.
    @param Ver [out] Required version number. Undefined if not valid.
    @return True if version number is valid, False if not.
  }
var
  ExtVer: Extended;   // version number as Extended float
begin
  Result := TryStrToFloat(Trim(edMinCompiler.Text), ExtVer);
  if Result then
    Ver := ExtVer;
end;

function TCodeGenPrefsFrame.GetSymbol(out Symbol: string): Boolean;
  {Gets warning symbol from edit control.
    @param Symbol [out] Required symbol, trimmed of white space. Undefined if
      not valid.
    @return True if symbol is non-empty string, False if not.
  }
begin
  Symbol := Trim(edSymbol.Text);
  Result := Symbol <> '';
end;

class function TCodeGenPrefsFrame.Index: Byte;
  {Provides an index number that determines the order in which the tabs
  containing frames are displayed in the preferences dialog box.
    @return Required index number.
  }
begin
  Result := 15;
end;

function TCodeGenPrefsFrame.IndexOfSymbolInLV(const Symbol: string): Integer;
  {Gets the index of the list item containing a warning symbol in the list view.
    @param Symbol [in] Symbol to be found.
    @return Index of required list view item or -1 if not found.
  }
var
  LI: TListItem;  // each list item in list view
begin
  for LI in lvWarnings.Items do
  begin
    if AnsiSameText(Symbol, LI.Caption) then
      Exit(LI.Index);
  end;
  Result := -1;
end;

function TCodeGenPrefsFrame.IsValidCompilerVersion: Boolean;
  {Checks if compiler version entered in edit control is valid.
    @return True if valid, False if not.
  }
var
  Ver: Single;  // compiler version from edit control
begin
  Result := GetCompilerVersion(Ver);
  if Result then
    Result := Ver >= TWarning.MinSupportedCompiler;
end;

function TCodeGenPrefsFrame.IsValidSymbol: Boolean;
  {Checks if symbol entered in edit control is valid.
    @return True if valid, False if not.
  }
var
  Symbol: string; // stores symbol: unused
begin
  Result := GetSymbol(Symbol);
end;

procedure TCodeGenPrefsFrame.lvWarningsClick(Sender: TObject);
  {Click event handler for warnings list view. Copies details of warning
  associated with selected (clicked) list item to edit controls.
    @param Sender [in] Not used.
  }
begin
  if not Assigned(lvWarnings.Selected) then
    Exit;
  edSymbol.Text := lvWarnings.Selected.Caption;
  edMinCompiler.Text := lvWarnings.Selected.SubItems[0];
end;

procedure TCodeGenPrefsFrame.PopulateLV;
  {Populates list view with details of warnings.
  }
var
  W: TWarning;  // references each warning
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
  {Populates pre-defined compiler version pop-up menu with menu items.
  }

  // ---------------------------------------------------------------------------
  procedure AddMenuItem(const Compiler: string; const Ver: Single);
    {Adds a menu item to the pop-up menu.
      @param Compiler [in] Name of compiler (for menu item caption).
      @param Ver [in] Version of named compiler.
    }
  begin
    mnuPreDefCompilers.Items.Add(
      TPreDefCompilerMenuItem.Create(
        mnuPreDefCompilers, Compiler, Ver, PreDefCompilerMenuClick
      )
    );
  end;
  // ---------------------------------------------------------------------------

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
end;

procedure TCodeGenPrefsFrame.PreDefCompilerMenuClick(Sender: TObject);
  {Handler for click events on any pre-defined compiler menu item. Stores
  required compiler version in associated edit control.
    @param Sender [in] Reference to clicked menu item.
  }
begin
  edMinCompiler.Text := FormatCompilerVer(
    (Sender as TPreDefCompilerMenuItem).CompilerVer
  );
end;

{ TPreDefCompilerMenuItem }

constructor TPreDefCompilerMenuItem.Create(AOwner: TComponent;
  const ACaption: string; const ACompilerVer: Single;
  const AClickHandler: TNotifyEvent);
  {Constructor. Used to fully create a menu item with all required properties
  and event handlers.
    @param AOwner [in] Component that owns menu item.
    @param ACaption [in] Name of compiler displayed in menu item's caption.
    @param ACompilerVer [in] Version number of compiler named in ACaption.
    @param AClickHandler [in] Menu item's OnClick event handler.
  }
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

