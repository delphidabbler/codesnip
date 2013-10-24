{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a frame that allows the user to set syntax highlighter
 * preferences. Designed for use as one of the tabs in the Preferences dialogue
 * box.
}


unit FrHiliterPrefs;


interface


uses
  // Delphi
  Controls, StdCtrls, Classes,
  // Project
  CS.SourceCode.Hiliter.Themes,
  CS.UI.Helper.CollectionCtrlKVMgr,
  FrPrefsBase,
  UPreferences;


type
  ///  <summary>Frame that allows the user to select syntax highlighter themes
  ///  that are used in different parts of the application. The frame can
  ///  persist preferences entered by the user.</summary>
  ///  <remarks>Designed for use in preferences dialogue box.</remarks>
  THiliterPrefsFrame = class(TPrefsBaseFrame)
    btnReset: TButton;
    lblUITheme: TLabel;
    lblExportTheme: TLabel;
    lblPrintTheme: TLabel;
    cbUITheme: TComboBox;
    cbExportTheme: TComboBox;
    cbPrintTheme: TComboBox;
    lblInstructions: TLabel;
    btnEditThemes: TButton;
    ///  <summary>Restores default themes when Restore Defaults button is
    ///  clicked.</summary>
    procedure btnResetClick(Sender: TObject);
    ///  <summary>Handles OnChange events on the three theme combo boxes.
    ///  Records the newly selected theme.</summary>
    procedure ThemeComboChange(Sender: TObject);
    ///  <summary>Displays syntax highilight themes editor dialog box to permit
    ///  themes to be edited.</summary>
    procedure btnEditThemesClick(Sender: TObject);
  strict private
    var
      ///  <summary>Shorthand reference TConfig.Instance.HiliterThemes.
      ///  </summary>
      fThemes: TSyntaxHiliteThemes;
      ///  <summary>Records syntax highlighter themes chosen by user.</summary>
      fCurrentHiliteThemeIDs: TCurrentHiliteThemes;
      ///  <summary>Map of syntax highlighter theme kinds onto the combo boxes
      ///  used to select the themes.</summary>
      fThemeCombos: array[TCurrentHiliteThemeKind] of TComboBox;
      ///  <summary>Objects used to manage theme combos and map their selections
      ///  to the related highlighter themes.</summary>
      fComboBoxMgrs: array[TCurrentHiliteThemeKind] of
        TUnsortedCollectionCtrlKVMgr<TSyntaxHiliteTheme>;
      ///  <summary>Indicates if any preference has changed.</summary>
      fChanged: Boolean;
    ///  <summary>Stores names of all available themes in theme combo boxes and
    ///  selects the current theme in each of them.</summary>
    procedure PopulateThemeCombos;

  public
    ///  <summary>Constructs frame instance and initialises controls.</summary>
    ///  <param name="AOwner">TComponent [in] Component that owns the frame.
    ///  </param>
    constructor Create(AOwner: TComponent); override;
    ///  <summary>Destroys frame instance.</summary>
    destructor Destroy; override;
    ///  <summary>Updates controls from given preferences object.</summary>
    ///  <remarks>Called when the dialogue page containing the frame is
    ///  activated.</remarks>
    procedure Activate(const Prefs: IPreferences); override;
    ///  <summary>Updates given preferences object with data entered in controls.
    ///  </summary>
    ///  <remarks>Called when the dialogue page containing the frame is
    ///  deactivated.</remarks>
    procedure Deactivate(const Prefs: IPreferences); override;
    ///  <summary>Checks if preferences changed in this frame require that the
    ///  main window UI is updated.</summary>
    ///  <remarks>Called when the dialogue box containing the frame is closing.
    ///  </remarks>
    function UIUpdated: Boolean; override;
    ///  <summary>Arranges controls on frame.</summary>
    ///  <remarks>Called after the frame has been sized.</remarks>
    procedure ArrangeControls; override;
    ///  <summary>Returns the caption that is displayed in the tab of the
    ///  dialogue page that contains the frame.</summary>
    ///  <remarks>Called when the containing dialogue page is being created.
    ///  </remarks>
    function DisplayName: string; override;
    ///  <summary>Returns an index number that determines the location of the
    ///  page containing the frame when displayed in the Preferences dialogue
    ///  box.</summary>
    ///  <remarks>Called when the frame is being registered with the Preferences
    ///  dialogue box.</remarks>
    class function Index: Byte; override;
  end;


implementation


uses
  // Project
  CS.Config,
  CS.UI.Dialogs.HiliteThemesEditor,
  FmPreferencesDlg,
  UContainers,
  UCtrlArranger,
  UExceptions,
  UIStringList,
  UStrUtils;


{$R *.dfm}

{ THiliterPrefsFrame }

procedure THiliterPrefsFrame.Activate(const Prefs: IPreferences);
var
  Kind: TCurrentHiliteThemeKind;
begin
  for Kind := Low(TCurrentHiliteThemeKind) to High(TCurrentHiliteThemeKind) do
    fCurrentHiliteThemeIDs[Kind] := Prefs.CurrentHiliteThemeIds[Kind];
  PopulateThemeCombos;
end;

procedure THiliterPrefsFrame.ArrangeControls;
begin
  lblInstructions.Top := 3;
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(lblInstructions, 12),
    [lblUITheme, cbUITheme]
  );
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf([lblUITheme, cbUITheme], 12),
    [lblExportTheme, cbExportTheme]
  );
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf([lblExportTheme, cbExportTheme], 12),
    [lblPrintTheme, cbPrintTheme]
  );
  TCtrlArranger.MoveBelow([lblPrintTheme, cbPrintTheme], btnReset, 24);
  TCtrlArranger.MoveBelow(btnReset, btnEditThemes, 12);
  TCtrlArranger.AlignLefts(
    [lblUITheme, lblExportTheme, lblPrintTheme, lblInstructions], 3
  );
  TCtrlArranger.AlignLefts(
    [cbUITheme, cbExportTheme, cbPrintTheme, btnReset, btnEditThemes],
    TCtrlArranger.RightOf([lblUITheme, lblExportTheme, lblPrintTheme], 18)
  );
  TCtrlArranger.StretchRightTo(
    [cbUITheme, cbExportTheme, cbPrintTheme], Self.ClientWidth - 3
  );
end;

procedure THiliterPrefsFrame.btnEditThemesClick(Sender: TObject);
begin
  if THiliteThemesEditorDlg.Execute(Self, fCurrentHiliteThemeIDs) then
  begin
    PopulateThemeCombos;
    fChanged := True;
  end;
end;

procedure THiliterPrefsFrame.btnResetClick(Sender: TObject);
var
  Kind: TCurrentHiliteThemeKind;
  UIThemeID: TSyntaxHiliteThemeID;
begin
  UIThemeID := fCurrentHiliteThemeIDs[htkUI];
  for Kind := Low(TCurrentHiliteThemeKind) to High(TCurrentHiliteThemeKind) do
  begin
    fComboBoxMgrs[Kind].Select(fThemes.DefaultTheme);
    fCurrentHiliteThemeIDs[Kind] := fComboBoxMgrs[Kind].GetSelected.ID;
  end;
  if fCurrentHiliteThemeIDs[htkUI] <> UIThemeID then
    fChanged := True;
end;

constructor THiliterPrefsFrame.Create(AOwner: TComponent);
resourcestring
  // Colour dialogue style
  sDlgTitle = 'Choose Element Colour';  // colour dialogue title
var
  Kind: TCurrentHiliteThemeKind;
begin
  inherited;
  HelpKeyword := 'HiliterPrefs';

  fThemes := TConfig.Instance.HiliterThemes;

  fThemeCombos[htkUI] := cbUITheme;
  fThemeCombos[htkExport] := cbExportTheme;
  fThemeCombos[htkPrint] := cbPrintTheme;

  for Kind := Low(TCurrentHiliteThemeKind) to High(TCurrentHiliteThemeKind) do
  begin
    fCurrentHiliteThemeIDs[Kind] := TSyntaxHiliteThemeID.CreateNull;
    fComboBoxMgrs[Kind] :=
      TUnsortedCollectionCtrlKVMgr<TSyntaxHiliteTheme>.Create(
        TComboBoxAdapter.Create(fThemeCombos[Kind]),
        True,
        function (const Left, Right: TSyntaxHiliteTheme): Boolean
        begin
          Result := Left.ID = Right.ID;
        end
      );
  end;

  // Clear dirty flag
  fChanged := False;
end;

procedure THiliterPrefsFrame.Deactivate(const Prefs: IPreferences);
var
  Kind: TCurrentHiliteThemeKind;
begin
//  Prefs.HiliteThemes := fHiliteThemes;
  for Kind := Low(TCurrentHiliteThemeKind) to High(TCurrentHiliteThemeKind) do
    Prefs.CurrentHiliteThemeIds[Kind] := fCurrentHiliteThemeIDs[Kind];
end;

destructor THiliterPrefsFrame.Destroy;
var
  Kind: TCurrentHiliteThemeKind;
begin
  for Kind := Low(TCurrentHiliteThemeKind) to High(TCurrentHiliteThemeKind) do
    fComboBoxMgrs[Kind].Free;
  inherited;
end;

function THiliterPrefsFrame.DisplayName: string;
resourcestring
  sDisplayName = 'Syntax Highlighter';  // display name
begin
  Result := sDisplayName;
end;

class function THiliterPrefsFrame.Index: Byte;
begin
  Result := 30;
end;

procedure THiliterPrefsFrame.PopulateThemeCombos;
var
  Kind: TCurrentHiliteThemeKind;
  Theme: TSyntaxHiliteTheme;
begin
  for Kind := Low(TCurrentHiliteThemeKind) to High(TCurrentHiliteThemeKind) do
  begin
    fComboBoxMgrs[Kind].Clear;
    for Theme in fThemes do
      fComboBoxMgrs[Kind].Add(Theme, Theme.FriendlyName);
    fComboBoxMgrs[Kind].Select(
      fThemes[fCurrentHiliteThemeIDs[Kind]]
    );
  end;
end;

procedure THiliterPrefsFrame.ThemeComboChange(Sender: TObject);

  function ComboToThemeKind(CB: TComboBox): TCurrentHiliteThemeKind;
  var
    Kind: TCurrentHiliteThemeKind;
  begin
    for Kind := Low(TCurrentHiliteThemeKind) to High(TCurrentHiliteThemeKind) do
      if fThemeCombos[Kind] = CB then
        Exit(Kind);
    raise EBug.Create(
      ClassName + '.ThemeComboChange:ComboToThemeKind: Unknown combo box'
    );
  end;

var
  Kind: TCurrentHiliteThemeKind;
begin
  Kind := ComboToThemeKind(Sender as TComboBox);
  fCurrentHiliteThemeIDs[Kind] := fComboBoxMgrs[Kind].GetSelected.ID;
  if Kind = htkUI then
    fChanged := True;
end;

function THiliterPrefsFrame.UIUpdated: Boolean;
begin
  Result := fChanged;
end;

initialization

// Register frame with preferences dialogue box
TPreferencesDlg.RegisterPage(THiliterPrefsFrame);

end.

