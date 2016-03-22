{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2016, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a dialogue box where the user can create, edit and delete syntax
 * highlighter themes.
}


unit CS.UI.Dialogs.HiliteThemesEditor;

interface

uses
  // Delphi
  Classes,
  ActnList,
  Forms,
  StdCtrls,
  Controls,
  ExtCtrls,
  Graphics,
  // Project
  CS.SourceCode.Hiliter.Brushes,
  CS.SourceCode.Hiliter.Themes,
  CS.UI.Dialogs.HiliteThemesEditor.ExampleFrame,
  CS.UI.Helper.CollectionCtrlKVMgr,
  FmGenericViewDlg,
  FrBrowserBase,
  FrHTMLPreview,
  UBaseObjects,
  UColorBoxEx,
  UColorDialogEx;

type
  THiliteThemesEditorDlg = class(TGenericViewDlg, INoPublicConstruct)
    lblThemes: TLabel;
    cbThemes: TComboBox;
    lblBrushes: TLabel;
    cbBrushes: TComboBox;
    gbThemeProps: TGroupBox;
    lblFontName: TLabel;
    cbFontName: TComboBox;
    cbFontSize: TComboBox;
    lblDefForeground: TLabel;
    lblDefBackground: TLabel;
    lblFontSize: TLabel;
    btnDelete: TButton;
    btnSave: TButton;
    gbElements: TGroupBox;
    lblElements: TLabel;
    lblForeground: TLabel;
    lblExample: TLabel;
    lbElements: TListBox;
    gbFontStyle: TGroupBox;
    chkBold: TCheckBox;
    chkItalics: TCheckBox;
    chkUnderline: TCheckBox;
    lblBackground: TLabel;
    btnPreview: TButton;
    alDlg: TActionList;
    actDelete: TAction;
    actSave: TAction;
    actPreview: TAction;
    btnNew: TButton;
    actNew: TAction;
    btnDefaultStyle: TButton;
    actDefaultStyle: TAction;
    btnUpdate: TButton;
    actUpdate: TAction;
    frmExample: THiliteThemesExampleFrame;
    chkDefaultFS: TCheckBox;
    procedure actDeleteExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actPreviewExecute(Sender: TObject);
    procedure actDeleteUpdate(Sender: TObject);
    procedure actPreviewUpdate(Sender: TObject);
    procedure cbFontSizeKeyPress(Sender: TObject; var Key: Char);
    procedure FormDestroy(Sender: TObject);
    procedure cbBrushesChange(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure cbThemesChange(Sender: TObject);
    procedure cbFontNameChange(Sender: TObject);
    procedure lbElementsClick(Sender: TObject);
    procedure actDefaultStyleUpdate(Sender: TObject);
    procedure cbFontSizeChange(Sender: TObject);
    procedure actDefaultStyleExecute(Sender: TObject);
    procedure actUpdateExecute(Sender: TObject);
    procedure actUpdateUpdate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FontStyleCheckBoxClick(Sender: TObject);
  strict private
    type
      TAttrStyleInfo = record
        BrushID: string;
        AttrID: string;
        AttrStyle: TSyntaxHiliteAttrStyle;
        InheritedAttrStyle: TSyntaxHiliteAttrStyle;
        constructor Create(const ABrushID, AAttrID: string;
          const AAttrStyle, AInheritedAttrStyle: TSyntaxHiliteAttrStyle);
        function IsDefaultStyle: Boolean;
      end;
  strict private
    var
      cbDefForeground: TColorBoxEx;
      cbDefBackground: TColorBoxEx;
      cbForeground: TColorBoxEx;
      cbBackground: TColorBoxEx;
      dlgColour: TColorDialogEx;
      ///  <summary>Shorthand reference TConfig.Instance.HiliterThemes.
      ///  </summary>
      fThemes: TSyntaxHiliteThemes;
      fBlockedThemeIDs: TArray<TSyntaxHiliteThemeID>;
      fThemesChanged: Boolean;
      fWorkingTheme: TSyntaxHiliteTheme;
      fWorkingAttrStyle: TAttrStyleInfo;
      fWorkingThemeDirty: Boolean;
      fThemesComboMgr: TSortedCollectionCtrlKVMgr<TSyntaxHiliteTheme>;
      fFontNameComboMgr: TSortedCollectionCtrlKVMgr<string>;
      fFontSizeComboMgr: TUnsortedCollectionCtrlKVMgr<Integer>;
      fBrushesComboMgr: TUnsortedCollectionCtrlKVMgr<string>;
      fElementsLBMgr: TSortedCollectionCtrlKVMgr<TSyntaxHiliterAttr>;
    procedure cbDefForegroundChange(Sender: TObject);
    procedure cbDefBackgroundChange(Sender: TObject);
    procedure cbForegroundChange(Sender: TObject);
    procedure cbBackgroundChange(Sender: TObject);
    procedure CreateColourCombos;
    procedure PopulateFontNameCombo;
    procedure PopulateFontSizeCombo;
    procedure PopulateThemeCombo;
    procedure PopulateBrushesCombo;
    procedure PopulateElementsList(const BrushID: string);
    function IsDefaultBrushID(const BrushID: string): Boolean;
    function IsDefaultBrushSelected: Boolean;
    procedure ChangeTheme;
    procedure ChangeBrush(const BrushID: string);
    procedure ChangeAttrStyle;
    procedure UpdateThemePropControls;
    procedure UpdateAttrControls;
    function GetFontStyleFromCheckBoxes: TSyntaxHiliteFontStyles;
    procedure UpdateAttrExample;
    procedure UpdateWorkingThemeStyles;
    procedure SaveWorkingTheme;
    procedure SetWorkingThemeDirty;
  strict protected
    procedure ConfigForm; override;
    procedure ArrangeForm; override;
    procedure InitForm; override;
  public
    ///  <summary>Displays highlight themes editor dialogue box.</summary>
    ///  <param name="AOwner">[in] Component that owns this dialogue box.
    ///  </param>
    ///  <param name="AThemesInUse">[in] String array of IDs of themes that are
    ///  currently in use and therefore musn't be deleted.</param>
    ///  <returns>True if any themes in use were changed, False otherwise.
    ///  </returns>
    class function Execute(AOwner: TComponent;
      const BlockedThemeIDs: array of TSyntaxHiliteThemeID): Boolean;
  end;

// TODO: Implement example pane

implementation


uses
  // Delphi
  SysUtils,
  Character,
  Math,
  Dialogs,
  // Project
  CS.Config,
  CS.SourceCode.Hiliter.Renderers,
  CS.UI.Dialogs.HiliteThemeName,
  CS.Utils.Sound,
  FmPreviewDlg,
  UConsts,
  UContainers,
  UCtrlArranger,
  UFontHelper,
  UMessageBox,
  UStrUtils;

{$R *.dfm}

{ THiliteThemesEditorDlg }

procedure THiliteThemesEditorDlg.actDefaultStyleExecute(Sender: TObject);
begin
  fWorkingAttrStyle.AttrStyle := fWorkingAttrStyle.InheritedAttrStyle;
  UpdateWorkingThemeStyles;
end;

procedure THiliteThemesEditorDlg.actDefaultStyleUpdate(Sender: TObject);
begin
//  actDefaultStyle.Enabled := not IsDefaultBrushSelected and
//    not fWorkingAttrStyle.IsDefaultStyle;
end;

procedure THiliteThemesEditorDlg.actDeleteExecute(Sender: TObject);
var
  SelTheme: TSyntaxHiliteTheme;
begin
  SelTheme := fThemesComboMgr.GetSelected;
  fThemesComboMgr.Delete(SelTheme);
  fThemes.Delete(SelTheme.ID);
  fThemesChanged := True;
  ChangeTheme;
end;

procedure THiliteThemesEditorDlg.actDeleteUpdate(Sender: TObject);
begin
  actDelete.Enabled := fThemesComboMgr.HasSelection
    and not fThemesComboMgr.GetSelected.BuiltIn
    and not TArrayHelper.Contains<TSyntaxHiliteThemeID>(
      fBlockedThemeIDs,
      fThemesComboMgr.GetSelected.ID,
      function (const Left, Right: TSyntaxHiliteThemeID): Boolean
      begin
        Result := Left = Right;
      end
    );
end;

procedure THiliteThemesEditorDlg.actNewExecute(Sender: TObject);
begin
  fThemesComboMgr.ClearSelection;
  ChangeTheme;
end;

procedure THiliteThemesEditorDlg.actPreviewExecute(Sender: TObject);
var
  Brush: TSyntaxHiliterBrush;
resourcestring
  sTitle = '%s Highlighting Preview';
begin
  Brush := TSyntaxHiliterBrushes.CreateBrush(fBrushesComboMgr.GetSelected);
  try
    TPreviewDlg.Execute(
      Self,
      TXHTMLDocumentHiliter.Hilite(
        Brush.SampleSourceCode, Brush, fWorkingTheme
      ),
      dtHTML,
      Format(sTitle, [Brush.FriendlyName])
    );
  finally
    Brush.Free;
  end;

end;

procedure THiliteThemesEditorDlg.actPreviewUpdate(Sender: TObject);
begin
  actPreview.Enabled := not IsDefaultBrushSelected;
end;

procedure THiliteThemesEditorDlg.actSaveExecute(Sender: TObject);
begin
  SaveWorkingTheme;
  fWorkingThemeDirty := False;
end;

procedure THiliteThemesEditorDlg.actUpdateExecute(Sender: TObject);
begin
  Assert(fWorkingTheme.ID = fThemesComboMgr.GetSelected.ID,
    ClassName + '.actUpdateExecute: Working theme ID <> selected theme ID'
  );
  fThemesComboMgr.GetSelected.Assign(fWorkingTheme, False, True);
  fWorkingThemeDirty := False;
  fThemesChanged := True;
end;

procedure THiliteThemesEditorDlg.actUpdateUpdate(Sender: TObject);
begin
  actUpdate.Enabled := fThemesComboMgr.HasSelection
    and not fThemesComboMgr.GetSelected.BuiltIn;
end;

procedure THiliteThemesEditorDlg.ArrangeForm;
begin
  // set main group box sizes
  gbThemeProps.Width := pnlBody.ClientWidth;
  gbElements.Width := gbThemeProps.Width;

  // Highlighter theme row
  TCtrlArranger.AlignLefts(
    [lblThemes, gbThemeProps, lblBrushes, gbElements], 0
  );
  TCtrlArranger.MoveToRightOf(lblThemes, cbThemes, 8);
  TCtrlArranger.AlignRights(
    [btnDelete, btnSave], TCtrlArranger.RightOf(gbThemeProps)
  );
  TCtrlArranger.AlignRights(
    [btnNew, btnUpdate], TCtrlArranger.LeftOf([btnDelete, btnSave], 6)
  );
  cbThemes.Width := btnNew.Left - cbThemes.Left - 16;
  TCtrlArranger.AlignTops([btnNew, btnDelete], 0);
  TCtrlArranger.AlignTops(
    [btnUpdate, btnSave], TCtrlArranger.BottomOf([btnNew, btnDelete], 6)
  );
  TCtrlArranger.AlignVCentresTo([btnNew, btnUpdate], [lblThemes, cbThemes]);

  // Theme properties group box
  TCtrlArranger.MoveBelow(
    [lblThemes, cbThemes, btnNew, btnDelete, btnSave], gbThemeProps, 12
  );
  TCtrlArranger.AlignVCentres(
    22, [lblFontName, cbFontName, lblDefForeground, cbDefForeground]
  );
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(
      [lblFontName, cbFontName, lblDefForeground, cbDefForeground], 12
    ),
    [lblFontSize, cbFontSize, lblDefBackground, cbDefBackground]
  );
  TCtrlArranger.AlignLefts([lblFontName, lblFontSize], 8);
  TCtrlArranger.AlignLefts(
    [cbFontName, cbFontSize],
    TCtrlArranger.RightOf([lblFontName, lblFontSize], 8)
  );
  TCtrlArranger.AlignLefts(
    [cbDefForeground, cbDefBackground],
    gbThemeProps.Width - Max(cbDefForeground.Width, cbDefBackground.Width) - 8
  );
  TCtrlArranger.AlignLefts(
    [lblDefForeground, lblDefBackground],
    TCtrlArranger.LeftOf([cbDefForeground, cbDefBackground])
      - Max(lblDefForeground.Width, lblDefBackground.Width) - 8
  );
  gbThemeProps.Height := TCtrlArranger.TotalControlHeight(gbThemeProps) + 10;

  // Brush row
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(gbThemeProps, 16),
    [lblBrushes, cbBrushes, btnPreview]
  );
  TCtrlArranger.MoveToRightOf(lblBrushes, cbBrushes, 8);
  TCtrlArranger.MoveToRightOf(cbBrushes, btnPreview, 16);

  // Element style group box
  TCtrlArranger.MoveBelow(
    [lblBrushes, cbBrushes, btnPreview], gbElements, 12
  );
  TCtrlArranger.AlignVCentres(22, [lblElements, lblForeground, cbForeground]);
  gbFontStyle.Top := lblElements.Top;
  TCtrlArranger.MoveBelow(lblElements, lbElements, 6);
  TCtrlArranger.AlignBottoms([lbElements, frmExample]);
  TCtrlArranger.MoveAbove(frmExample, lblExample, 6);
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf([lblForeground, cbForeground], 12),
    [lblBackground, cbBackground]
  );
  TCtrlArranger.MoveBelow([lblBackground, cbBackground], btnDefaultStyle, 12);
  TCtrlArranger.AlignLefts(
    [cbForeground, cbBackground],
    gbElements.Width - Max(cbForeground.Width, cbBackground.Width) - 8
  );
  TCtrlArranger.AlignLefts(
    [lblForeground, lblBackground],
    TCtrlArranger.LeftOf([cbForeground, cbBackground])
      - Max(lblForeground.Width, lblBackground.Width) - 8
  );
  TCtrlArranger.AlignRights([cbForeground, btnDefaultStyle]);
  TCtrlArranger.AlignLefts([gbFontStyle, lblExample, frmExample]);
  TCtrlArranger.StretchRightTo(
    frmExample,
    TCtrlArranger.RightOf([cbForeground, cbBackground, btnDefaultStyle])
  );
  gbElements.Height := TCtrlArranger.TotalControlHeight(gbElements) + 10;

  // Size body panel
  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody);
  inherited;
end;

procedure THiliteThemesEditorDlg.cbBackgroundChange(Sender: TObject);
begin
  fWorkingAttrStyle.AttrStyle.Background := cbBackground.Selected;
  UpdateWorkingThemeStyles;
end;

procedure THiliteThemesEditorDlg.cbBrushesChange(Sender: TObject);
begin
  ChangeBrush(fBrushesComboMgr.GetSelected);
end;

procedure THiliteThemesEditorDlg.cbDefBackgroundChange(Sender: TObject);
begin
  fWorkingTheme.DefaultBackground := cbDefBackground.Selected;
  SetWorkingThemeDirty;
end;

procedure THiliteThemesEditorDlg.cbDefForegroundChange(Sender: TObject);
begin
  // TODO: fix how fg and bg element colours reflect default fg and bg colours
  fWorkingTheme.DefaultForeground := cbDefForeground.Selected;
  SetWorkingThemeDirty;
end;

procedure THiliteThemesEditorDlg.cbFontNameChange(Sender: TObject);
begin
  if fFontNameComboMgr.HasSelection then
  begin
    fWorkingTheme.FontName := fFontNameComboMgr.GetSelected;
    SetWorkingThemeDirty;
    UpdateAttrExample;
  end;
end;

procedure THiliteThemesEditorDlg.cbFontSizeChange(Sender: TObject);
begin
  if fFontSizeComboMgr.HasSelection then
  begin
    fWorkingTheme.FontSize := fFontSizeComboMgr.GetSelected;
    SetWorkingThemeDirty;
    UpdateAttrExample;
  end;
end;

procedure THiliteThemesEditorDlg.cbFontSizeKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not TCharacter.IsDigit(Key) and (Key <> BACKSPACE) then
  begin
    Key := #0;
    KeyErrorBeep;
  end;
end;

procedure THiliteThemesEditorDlg.cbForegroundChange(Sender: TObject);
begin
  fWorkingAttrStyle.AttrStyle.Foreground := cbForeground.Selected;
  UpdateWorkingThemeStyles;
end;

procedure THiliteThemesEditorDlg.cbThemesChange(Sender: TObject);
begin
  ChangeTheme;
end;

procedure THiliteThemesEditorDlg.ChangeAttrStyle;
var
  AttrStyle: TSyntaxHiliteAttrStyle;
  InheritedAttrStyle: TSyntaxHiliteAttrStyle;
  BrushID: string;
  AttrID: string;
begin
  BrushID := fBrushesComboMgr.GetSelected;
  AttrID := fElementsLBMgr.GetSelected.ID;
  if IsDefaultBrushSelected then
  begin
    // Using default brush - inherits from theme's base style
    if fWorkingTheme.DefaultBrushStyle.IsAttrSupported(AttrID) then
      AttrStyle := fWorkingTheme.DefaultBrushStyle.AttrStyles[AttrID]
    else
      AttrStyle := TSyntaxHiliteAttrStyle.CreateDefault;
    InheritedAttrStyle := fWorkingTheme.BaseStyle;
  end
  else
  begin
    // Using language brush - inherits from theme's default brush
    if fWorkingTheme.IsBrushSupported(BrushID)
      and fWorkingTheme.BrushStyles[BrushID].IsAttrSupported(AttrID) then
      AttrStyle := fWorkingTheme.BrushStyles[BrushID].AttrStyles[AttrID]
    else
      AttrStyle := TSyntaxHiliteAttrStyle.CreateDefault;
    if fWorkingTheme.DefaultBrushStyle.IsAttrSupported(AttrID) then
      InheritedAttrStyle := fWorkingTheme.DefaultBrushStyle.AttrStyles[AttrID]
    else
      InheritedAttrStyle := TSyntaxHiliteAttrStyle.CreateDefault;
  end;
  fWorkingAttrStyle := TAttrStyleInfo.Create(
    BrushID, AttrID, AttrStyle, InheritedAttrStyle
  );
  UpdateAttrControls;
end;

procedure THiliteThemesEditorDlg.ChangeBrush(const BrushID: string);
begin
  Assert(fBrushesComboMgr.ContainsKey(BrushID),
    ClassName + '.ChangeBrush: Invalid BrushID');
  if not fBrushesComboMgr.HasSelection
    or not StrSameText(fBrushesComboMgr.GetSelected, BrushID) then
    fBrushesComboMgr.Select(BrushID);
  PopulateElementsList(fBrushesComboMgr.GetSelected);
  ChangeAttrStyle;
end;

procedure THiliteThemesEditorDlg.ChangeTheme;
resourcestring
  sQuerySave = 'The current theme has been changed. Would you like to save it?';
begin
  if fWorkingThemeDirty then
  begin
    if TMessageBox.Confirm(Self, sQuerySave) then
      SaveWorkingTheme;
  end;
  fWorkingThemeDirty := False;
  if fThemesComboMgr.HasSelection then
    fWorkingTheme.Assign(fThemesComboMgr.GetSelected)
  else
    fWorkingTheme.Assign(fThemes.NullTheme);
  UpdateThemePropControls;
  ChangeBrush(TSyntaxHiliteTheme.DefaultBrushID);
end;

procedure THiliteThemesEditorDlg.ConfigForm;
begin
  inherited;
  fThemes := TConfig.Instance.HiliterThemes;
  fWorkingTheme := TSyntaxHiliteTheme.Create(
    TSyntaxHiliteThemeID.CreateNull, ''
  );
  fWorkingThemeDirty := False;
  CreateColourCombos;
  fThemesComboMgr := TSortedCollectionCtrlKVMgr<TSyntaxHiliteTheme>.Create(
    TComboBoxAdapter.Create(cbThemes),
    True,
    function (const Left, Right: TSyntaxHiliteTheme): Boolean
    begin
      Result := Left.ID = Right.ID;
    end,
    stIgnoreCase
  );
  fFontNameComboMgr := TSortedCollectionCtrlKVMgr<string>.Create(
    TComboBoxAdapter.Create(cbFontName),
    True,
    function (const Left, Right: string): Boolean
    begin
      Result := StrSameText(Left, Right);
    end,
    stIgnoreCase
  );
  fFontSizeComboMgr := TUnsortedCollectionCtrlKVMgr<Integer>.Create(
    TComboBoxAdapter.Create(cbFontSize),
    True,
    function (const Left, Right: Integer): Boolean
    begin
      Result := Left = Right;
    end
  );
  fBrushesComboMgr := TUnsortedCollectionCtrlKVMgr<string>.Create(
    TComboBoxAdapter.Create(cbBrushes),
    True,
    function (const Left, Right: string): Boolean
    begin
      Result := StrSameText(Left, Right);
    end
  );
  fElementsLBMgr := TSortedCollectionCtrlKVMgr<TSyntaxHiliterAttr>.Create(
    TListBoxAdapter.Create(lbElements),
    True,
    function (const Left, Right: TSyntaxHiliterAttr): Boolean
    begin
      Result := TSyntaxHiliterAttr.Compare(Left, Right) = 0;
    end,
    stIgnoreCase
  );
  fThemesChanged := False;
end;

procedure THiliteThemesEditorDlg.CreateColourCombos;

  // NOTE: MUST be called after setting parent of CB
  procedure SetCommonProps(CB: TColorBoxEx);
  begin
    // cbCustomColor not included in CB.Style since assigning ColorDialog
    // property sets this style
    CB.Style := [cbStandardColors, cbExtendedColors, cbSystemColors,
      cbPrettyNames];
    CB.ItemHeight := 16;
    CB.NoneColorColor := clBlack;
    CB.DefaultColorColor := clBlack;
    CB.ColorDialog := dlgColour;
  end;

resourcestring
  // Colour dialogue style
  sDlgTitle = 'Choose Element Colour';  // colour dialogue title
begin
  // Create and initialise custom colour dialogue box
  dlgColour := TColorDialogEx.Create(Self);
  dlgColour.Title := sDlgTitle;
  // cdShowHelp not included in fColorDlg.Options since setting HelpKeyword
  // property causes this style to be used
  dlgColour.Options := [cdFullOpen];
  dlgColour.HelpKeyword := 'HiliteThemeColourDlg';

  cbDefForeground := TColorBoxEx.Create(Self);
  with cbDefForeground do
  begin
    Parent := gbThemeProps;
    SetCommonProps(cbDefForeground);
    Width := 127;
    Height := 21;
    TabOrder := 2;
    Style := Style + [cbIncludeNone]; // default colour not allowed
    OnChange := cbDefForegroundChange;
  end;

  lblDefForeground.FocusControl := cbDefForeground;

  cbDefBackground := TColorBoxEx.Create(Self);
  with cbDefBackground do
  begin
    Parent := gbThemeProps;
    SetCommonProps(cbDefBackground);
    Width := 127;
    Height := 21;
    TabOrder := 3;
    Style := Style + [cbIncludeNone]; // default colour not allowed
    OnChange := cbDefBackgroundChange;
  end;
  lblDefBackground.FocusControl := cbDefBackground;

  cbForeground := TColorBoxEx.Create(Self);
  with cbForeground do
  begin
    Parent := gbElements;
    SetCommonProps(cbForeground);
    Width := 127;
    Height := 21;
    TabOrder := 2;
    // TODO: remove Style statement if default radio button used
    Style := Style + [cbIncludeDefault];  // "None" colour not allowed
    OnChange := cbForegroundChange;
  end;
  lblForeground.FocusControl := cbForeground;

  cbBackground := TColorBoxEx.Create(Self);
  with cbBackground do
  begin
    Parent := gbElements;
    SetCommonProps(cbBackground);
    Width := 127;
    Height := 21;
    TabOrder := 3;
    // TODO: remove Style statement if default radio button used
    Style := Style + [cbIncludeDefault];  // "None" colour not allowed
    OnChange := cbBackgroundChange;
  end;
  lblBackground.FocusControl := cbBackground;
end;

class function THiliteThemesEditorDlg.Execute(AOwner: TComponent;
  const BlockedThemeIDs: array of TSyntaxHiliteThemeID): Boolean;
begin
  with THiliteThemesEditorDlg.InternalCreate(AOwner) do
    try
      fBlockedThemeIDs := TArrayHelper.Copy<TSyntaxHiliteThemeID>(
        BlockedThemeIDs
      );
      ShowModal;
      Result := fThemesChanged;
    finally
      Free;
    end;
end;

procedure THiliteThemesEditorDlg.FontStyleCheckBoxClick(Sender: TObject);
begin
  fWorkingAttrStyle.AttrStyle.FontStyles := GetFontStyleFromCheckBoxes;
  UpdateWorkingThemeStyles;
end;

procedure THiliteThemesEditorDlg.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
resourcestring
  sQueryExit = 'The current theme has been changed. '
    + 'If you close the dialogue box now you will loose all your changes.'
    + EOL2
    + 'Are you sure you want to close the dialogue box?';
begin
  if fWorkingThemeDirty and not TMessageBox.Confirm(Self, sQueryExit) then
    CanClose := False;
end;

procedure THiliteThemesEditorDlg.FormDestroy(Sender: TObject);
begin
  fElementsLBMgr.Free;
  fBrushesComboMgr.Free;
  fFontSizeComboMgr.Free;
  fFontNameComboMgr.Free;
  fThemesComboMgr.Free;
  fWorkingTheme.Free;
  inherited;
end;

function THiliteThemesEditorDlg.GetFontStyleFromCheckBoxes:
  TSyntaxHiliteFontStyles;
var
  FS: TFontStyles;
begin
  if chkDefaultFS.Checked then
    Result := TSyntaxHiliteFontStyles.CreateDefault
  else
  begin
    FS := [];
    if chkBold.Checked then
      Include(FS, fsBold);
    if chkItalics.Checked then
      Include(FS, fsItalic);
    if chkUnderline.Checked then
      Include(FS, fsUnderline);
    Result := TSyntaxHiliteFontStyles.CreateStyles(FS);
  end;
end;

procedure THiliteThemesEditorDlg.InitForm;
begin
  inherited;
  // change ParentFont on check boxes: must be done *after* form's font set
  chkBold.ParentFont := False;
  chkItalics.ParentFont := False;
  chkUnderline.ParentFont := False;
  PopulateFontNameCombo;
  PopulateFontSizeCombo;
  PopulateThemeCombo;
  PopulateBrushesCombo;
  ChangeTheme;
end;

function THiliteThemesEditorDlg.IsDefaultBrushID(const BrushID: string):
  Boolean;
begin
  Result := StrSameText(BrushID, TSyntaxHiliteTheme.DefaultBrushID);
end;

function THiliteThemesEditorDlg.IsDefaultBrushSelected: Boolean;
begin
  Result := fBrushesComboMgr.HasSelection and
    IsDefaultBrushID(fBrushesComboMgr.GetSelected);
end;

procedure THiliteThemesEditorDlg.lbElementsClick(Sender: TObject);
begin
  ChangeAttrStyle;
end;

procedure THiliteThemesEditorDlg.PopulateBrushesCombo;
var
  BrushID: string;
  Brush: TSyntaxHiliterBrush;
resourcestring
  sDefaultBrushFriendlyName = 'Default';
begin
  fBrushesComboMgr.Clear;
  fBrushesComboMgr.Add(
    TSyntaxHiliteTheme.DefaultBrushID, sDefaultBrushFriendlyName
  );
  for BrushID in TSyntaxHiliterBrushes.SupportedBrushIDs do
  begin
    Brush := TSyntaxHiliterBrushes.CreateBrush(BrushID);
    try
      fBrushesComboMgr.Add(BrushID, Brush.FriendlyName);
    finally
      Brush.Free;
    end;
  end;
  fBrushesComboMgr.Select(TSyntaxHiliteTheme.DefaultBrushID);
  PopulateElementsList(fBrushesComboMgr.GetSelected);
end;

procedure THiliteThemesEditorDlg.PopulateElementsList(const BrushID: string);
var
  Brush: TSyntaxHiliterBrush;
  Attrs: TArray<TSyntaxHiliterAttr>;
  Attr: TSyntaxHiliterAttr;
begin
  if IsDefaultBrushID(BrushID) then
    Attrs := TSyntaxHiliterBrushes.AllSupportedAttrs
  else
  begin
    Brush := TSyntaxHiliterBrushes.CreateBrush(BrushID);
    try
      Attrs := Brush.SupportedAttrs;
    finally
      Brush.Free;
    end;
  end;
  fElementsLBMgr.Clear;
  for Attr in Attrs do
    fElementsLBMgr.Add(Attr, Attr.FriendlyName);
  fElementsLBMgr.Select(fElementsLBMgr.GetFirstKey);
end;

procedure THiliteThemesEditorDlg.PopulateFontNameCombo;
var
  FontName: string;
begin
  for FontName in TFontHelper.ListMonoSpaceFonts do
    fFontNameComboMgr.Add(FontName, FontName);
end;

procedure THiliteThemesEditorDlg.PopulateFontSizeCombo;
var
  FontSize: Integer;
begin
  for FontSize in TSyntaxHiliteTheme.ValidFontSizes do
    fFontSizeComboMgr.Add(FontSize, IntToStr(FontSize));
end;

procedure THiliteThemesEditorDlg.PopulateThemeCombo;
var
  Theme: TSyntaxHiliteTheme;
begin
  fThemesComboMgr.Clear;
  for Theme in TConfig.Instance.HiliterThemes do
    fThemesComboMgr.Add(Theme, Theme.FriendlyName);
  fThemesComboMgr.Select(fThemes.DefaultTheme);
end;

procedure THiliteThemesEditorDlg.SaveWorkingTheme;
var
  ThemeName: string;
  Theme: TSyntaxHiliteTheme;
begin
  if not THiliteThemeNameDlg.Execute(Self, fThemes, ThemeName) then
    Exit;
  fWorkingTheme.FriendlyName := ThemeName;
  fWorkingTheme.BuiltIn := False;
  Theme := fThemes.FindThemeByFriendlyName(ThemeName);
  if Assigned(Theme) then
    Theme.Assign(fWorkingTheme, False, True)
  else
  begin
    Theme := fWorkingTheme.Clone(fThemes.UniqueIDString);
    fThemes.Add(Theme);
    fThemesComboMgr.Add(Theme, Theme.FriendlyName);
  end;
  fThemesChanged := True;
  fWorkingTheme.Assign(Theme);
  if not fThemesComboMgr.HasSelection or
    (Theme.ID <> fThemesComboMgr.GetSelected.ID) then
    fThemesComboMgr.Select(Theme);
end;

procedure THiliteThemesEditorDlg.SetWorkingThemeDirty;
begin
  fWorkingThemeDirty := True;
end;

procedure THiliteThemesEditorDlg.UpdateAttrControls;

  // Ticks or clears a check box without triggering an OnClick event. To fire
  // event would mean that frame would be marked as changed when it should not
  // be.
  procedure SafeCheck(const CB: TCheckBox; const State: Boolean);
  var
    OnClickSave: TNotifyEvent;
  begin
    OnClickSave := CB.OnClick;
    try
      CB.OnClick := nil;
      CB.Checked := State;
    finally
      CB.OnClick := OnClickSave;
    end;
  end;

var
  FontStyles: TFontStyles;
  UseDefFontStyles: Boolean;
begin
  // Font style group
  UseDefFontStyles := fWorkingAttrStyle.AttrStyle.FontStyles.IsDefault;
  if UseDefFontStyles then
  begin
    if not fWorkingAttrStyle.InheritedAttrStyle.FontStyles.IsDefault then
      FontStyles := fWorkingAttrStyle.InheritedAttrStyle.FontStyles
    else
      FontStyles := fWorkingTheme.BaseStyle.FontStyles;
  end
  else
    FontStyles := fWorkingAttrStyle.AttrStyle.FontStyles;
  SafeCheck(chkDefaultFS, UseDefFontStyles);
  SafeCheck(chkBold, fsBold in FontStyles);
  SafeCheck(chkItalics, fsItalic in FontStyles);
  SafeCheck(chkUnderline, fsUnderline in FontStyles);
  chkBold.Enabled := not UseDefFontStyles;
  chkItalics.Enabled := not UseDefFontStyles;
  chkUnderline.Enabled := not UseDefFontStyles;

  // Foreground colour
  cbForeground.Selected := fWorkingAttrStyle.AttrStyle.Foreground;

  // Background colour
  cbBackground.Selected := fWorkingAttrStyle.AttrStyle.Background;

  // Example
  UpdateAttrExample;
end;

procedure THiliteThemesEditorDlg.UpdateAttrExample;
begin
  frmExample.Display(
    fWorkingTheme, fWorkingAttrStyle.BrushID, fWorkingAttrStyle.AttrID
  );
end;

procedure THiliteThemesEditorDlg.UpdateThemePropControls;
begin
  fFontNameComboMgr.Select(fWorkingTheme.FontName);
  fFontSizeComboMgr.Select(fWorkingTheme.FontSize);
  cbDefForeground.Selected := fWorkingTheme.DefaultForeground;
  cbDefBackground.Selected := fWorkingTheme.DefaultBackground;
end;

procedure THiliteThemesEditorDlg.UpdateWorkingThemeStyles;
var
  AttrID: string;
  BrushID: string;
  NewStyle: TSyntaxHiliteAttrStyle;
  BrushStyle: TSyntaxHiliteBrushStyle;
begin
  AttrID := fWorkingAttrStyle.AttrID;
  BrushID := fWorkingAttrStyle.BrushID;
  NewStyle := fWorkingAttrStyle.AttrStyle;
  if IsDefaultBrushID(BrushID) then
    fWorkingTheme.DefaultBrushStyle.AttrStyles[AttrID] := NewStyle
  else
  begin
    if not fWorkingTheme.IsBrushSupported(BrushID) then
      fWorkingTheme.AddBrushStyle(BrushID, TSyntaxHiliteBrushStyle.Create);
    BrushStyle := fWorkingTheme.BrushStyles[BrushID];
    BrushStyle.AttrStyles[AttrID] := NewStyle;
  end;
  SetWorkingThemeDirty;
  UpdateAttrControls;
end;

{ THiliteThemesEditorDlg.TAttrStyleInfo }

constructor THiliteThemesEditorDlg.TAttrStyleInfo.Create(
  const ABrushID, AAttrID: string;
  const AAttrStyle, AInheritedAttrStyle: TSyntaxHiliteAttrStyle);
begin
  BrushID := ABrushID;
  AttrID := AAttrID;
  AttrStyle := AAttrStyle;
  InheritedAttrStyle := AInheritedAttrStyle;
end;

function THiliteThemesEditorDlg.TAttrStyleInfo.IsDefaultStyle: Boolean;
begin
  Result := AttrStyle.IsDefault;
end;

end.

