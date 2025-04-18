{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a frame that allows the user to set syntax highlighter
 * preferences.
 *
 * Designed for use as one of the pages in the Preferences dialogue box.
}


unit FrHiliterPrefs;


interface


uses
  // Delphi
  StdCtrls, Forms, Controls, Classes, Menus, Buttons,
  // Project
  FrPrefsBase, FrRTFShowCase, Hiliter.UGlobals, UColorBoxEx, UColorDialogEx,
  UConsts, UEncodings, UPreferences, URTFUtils;


type
  ///  <summary>Frame that allows the user to set syntax highlighter
  ///  preferences. The frame can persist preferences entered by the user.
  ///  </summary>
  ///  <remarks>Designed for use in preferences dialogue box.</remarks>
  THiliterPrefsFrame = class(TPrefsBaseFrame)
    btnReset: TButton;
    btnSaveStyle: TButton;
    btnStyle: TBitBtn;
    cbFontName: TComboBox;
    cbFontSize: TComboBox;
    chkBold: TCheckBox;
    chkItalics: TCheckBox;
    chkUnderline: TCheckBox;
    frmExample: TRTFShowCaseFrame;
    gbDocFont: TGroupBox;
    gbElements: TGroupBox;
    gbFontStyle: TGroupBox;
    lbElements: TListBox;
    lblColour: TLabel;
    lblExample: TLabel;
    lblFontName: TLabel;
    lblFontSize: TLabel;
    lblElements: TLabel;
    miClassic: TMenuItem;
    miDelphi7: TMenuItem;
    miNamedStyles: TMenuItem;
    miNoHilite: TMenuItem;
    miRADStudio: TMenuItem;
    miSpacer1: TMenuItem;
    miSpacer2: TMenuItem;
    miVisualStudio: TMenuItem;
    mnuStyles: TPopupMenu;
    ///  <summary>Restores default highlighter when Restore Defaults button is
    ///  clicked.</summary>
    procedure btnResetClick(Sender: TObject);
    ///  <summary>Display dialogue box to get a style name from user when Save
    ///  Style button is clicked. Current highlighter is saved with the style
    ///  name if user confirms selection.</summary>
    procedure btnSaveStyleClick(Sender: TObject);
    ///  <summary>Shows pop-up menu that displays pre-defined and named styles
    ///  when Use Named Style button is clicked.</summary>
    procedure btnStyleClick(Sender: TObject);
    ///  <summary>Updates colour of current source code element when a colour is
    ///  selected in Colour drop down list.</summary>
    procedure cbColourChange(Sender: TObject);
    ///  <summary>Updates highlighter's font face when a font name is selected
    ///  in Font Name drop down list.</summary>
    procedure cbFontNameChange(Sender: TObject);
    ///  <summary>Updates highlighter's font size when a size is selected in
    ///  Font Size combo box.</summary>
    procedure cbFontSizeChange(Sender: TObject);
    ///  <summary>Updates font style of current source code element when one of
    ///  the Font Style check boxes is checked or cleared.</summary>
    procedure ChkFontStyleClick(Sender: TObject);
    ///  <summary>Updates Font Style and Colour controls when a source code
    ///  element is selected in Elements list box.</summary>
    procedure lbElementsClick(Sender: TObject);
    ///  <summary>Displays User Defined Highlighters dialogue box when User
    ///  Defined Styles menu item is clicked in the Use Named Styles popup menu.
    ///  </summary>
    ///  <remarks>Changes highlighter being edited to any named highlighter
    ///  selected in dialogue box.</remarks>
    procedure miNamedStylesClick(Sender: TObject);
    ///  <summary>Changes highlighter being edited when one of the pre-defined
    ///  highlighters is selected in the Use Named Styles popup menu.</summary>
    procedure StyleMenuClick(Sender: TObject);
  strict private
    var
      ///  <summary>Custom colour combo box component.</summary>
      fColorBox: TColorBoxEx;
      ///  <summary>Custom colour dialogue box component.</summary>
      fColorDlg: TColorDialogEx;
      ///  <summary>Highlighter object edited by user.</summary>
      fAttrs: IHiliteAttrs;
      ///  <summary>Named highlighter object edited by user.</summary>
      fNamedAttrs: INamedHiliteAttrs;
      ///  <summary>Indicates if any preference has changed.</summary>
      fChanged: Boolean;
    ///  <summary>Populates Elements list box with all highlightable source code
    ///  elements.</summary>
    procedure PopulateElementsList;
    ///  <summary>Populates Font Size combo box with common font sizes.
    ///  </summary>
    procedure PopulateFontSizeCombo;
    ///  <summary>Populates Font Name combo box with all supported monospace
    ///  fonts.</summary>
    procedure PopulateFontNameCombo;
    ///  <summary>Returns ID of highlighter element currently selected in
    ///  Elements list box.</summary>
    function CurrentElementId: THiliteElement;
    ///  <summary>Returns reference to attributes of highlighter element
    ///  currently selected in Elements list box.</summary>
    function CurrentElement: IHiliteElemAttrs;
    ///  <summary>Updates controls to reflect current state of highlighter being
    ///  edited.</summary>
    procedure UpdateControls;
    ///  <summary>Updates preview of currently selected highlighter element.
    ///  </summary>
    procedure UpdatePreview;
    ///  <summary>Updates state of items in styles pop-up menu.</summary>
    procedure UpdatePopupMenu;
    ///  <summary>Generates and returns RTF representation of currently selected
    ///  highlighter element.</summary>
    ///  <remarks>This RTF is used to display elememt in preview pane.</remarks>
    function GenerateRTF: TRTFMarkup;
  public
    ///  <summary>Constructs frame instance and initialises controls.</summary>
    ///  <param name="AOwner">TComponent [in] Component that owns the frame.
    ///  </param>
    constructor Create(AOwner: TComponent); override;
    ///  <summary>Destroy frame instance.</summary>
    destructor Destroy; override;
    ///  <summary>Updates controls from given preferences object.</summary>
    ///  <remarks>Called when the dialogue page containing the frame is
    ///  activated.</remarks>
    procedure Activate(const Prefs: IPreferences; const Flags: UInt64);
      override;
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
  // Delphi
  SysUtils, ExtCtrls, Windows, Graphics, Dialogs,
  // Project
  ClassHelpers.RichEdit,
  FmPreferencesDlg, FmNewHiliterNameDlg, FmUserHiliterMgrDlg, Hiliter.UAttrs,
  IntfCommon, UCtrlArranger, UFontHelper, UIStringList, UMessageBox,
  URTFBuilder, URTFStyles, UUtils;


{$R *.dfm}


resourcestring
  // Highlighter element descriptions (appear in list box)
  sCommentDesc          = 'Comments';
  sReservedDesc         = 'Reserved words';
  sIdentifierDesc       = 'Identifiers';
  sSymbolDesc           = 'Symbols';
  sStringDesc           = 'String literals';
  sNumberDesc           = 'Whole numbers';
  sFloatDesc            = 'Real numbers';
  sHexDesc              = 'Hexadecimal numbers';
  sPreProcessorDesc     = 'Compiler directives';
  sAssemblerDesc        = 'Assembler code';
  sErrorDesc            = 'Errors';

  // Highlighter element examples (appear in Example box)
  // (lines are separated by LF)
  sCommentEg            = '{A comment}' + LF + '// Another comment;';
  sReservedEg           = 'interface' + LF + 'shl';
  sIdentifierEg         = 'AComponent' + LF + 'MyClass.MyMethod';
  sSymbolEg             = ':='+ LF + '[';
  sStringEg             = '''A string''' + LF + '#13#$OA';
  sNumberEg             = '123456';
  sFloatEg              = '1.234e67' + LF + '120.765';
  sHexEg                = '$A59E' + LF + '$a59e';
  sPreProcessorEg       = '{$DEFINE DEBUG}' + LF + '(*$R+*)';
  sAssemblerEg          = 'MOV EAX,1234H' + LF + 'MOV Number,EAX';
  sErrorEg              = 'An error message';

  // Error messages
  sErrBadFontSize       = 'Invalid font size';
  sErrBadFontRange      = 'Font size out of range. '
                          + 'Enter a value between %0:d and %1:d';

const
  ///  <summary>Map of highlighter elements to descriptions.</summary>
  cElementDescs: array[THiliteElement] of string = (
    '',                 // heWhitespace: not displayed in list box
    sCommentDesc,       // heComment
    sReservedDesc,      // heReserved
    sIdentifierDesc,    // heIdentifier
    sSymbolDesc,        // heSymbol
    sStringDesc,        // heString
    sNumberDesc,        // heNumber
    sFloatDesc,         // heFloat
    sHexDesc,           // heHex
    sPreProcessorDesc,  // hePreProcessor
    sAssemblerDesc,     // heAssembler
    sErrorDesc          // heError
  );

const
  ///  <summary>Map of highlighter elements to examples.</summary>
  cElementEgs: array[THiliteElement] of string = (
    '',                 // heWhitespace: not displayed in list box
    sCommentEg,         // heComment
    sReservedEg,        // heReserved
    sIdentifierEg,      // heIdentifier
    sSymbolEg,          // heSymbol
    sStringEg,          // heString
    sNumberEg,          // heNumber
    sFloatEg,           // heFloat
    sHexEg,             // heHex
    sPreProcessorEg,    // hePreProcessor
    sAssemblerEg,       // heAssembler
    sErrorEg            // heError
  );


{ THiliterPrefsFrame }

procedure THiliterPrefsFrame.Activate(const Prefs: IPreferences;
  const Flags: UInt64);
begin
  (fAttrs as IAssignable).Assign(Prefs.HiliteAttrs);
  (fNamedAttrs as IAssignable).Assign(Prefs.NamedHiliteAttrs);
  Prefs.CustomHiliteColours.CopyTo(fColorDlg.CustomColors, True);
  UpdateControls;
  UpdatePopupMenu;
end;

procedure THiliterPrefsFrame.ArrangeControls;
var
  AvailWidth: Integer;  // width available in element group box for controls
  CtrlWidth: Integer;   // width of side-by-side controls in element group box
  Spacing: Integer;     // spacing needed to separate controls in element gp box
begin
  // We can't rely on anchors to resize this group box since we need its width
  // below and anchors don't seem to update width in time to use it here
  gbElements.Width := Width;
  AvailWidth := gbElements.Width - lbElements.Left * 2;
  CtrlWidth := lbElements.Width + gbFontStyle.Width + fColorBox.Width;
  Spacing := (AvailWidth - CtrlWidth) div 2;
  TCtrlArranger.MoveToRightOf(lbElements, gbFontStyle, Spacing);
  fColorBox.Left := gbElements.Width - fColorBox.Width - 8;
  lblColour.Left := fColorBox.Left;
  frmExample.Left := gbFontStyle.Left;
  frmExample.Width := fColorBox.Left + fColorBox.Width - frmExample.Left;
  lblExample.Left := frmExample.Left;
  lbElements.Top := TCtrlArranger.BottomOf(lblElements, 4);
  lbElements.Height := gbElements.ClientHeight - lbElements.Top - 12;
  frmExample.Top :=  TCtrlArranger.BottomOf(lblExample, 4);
  frmExample.Height := gbElements.ClientHeight - frmExample.Top - 12;
  btnReset.Top := TCtrlArranger.BottomOf(btnStyle, 6);
  btnSaveStyle.Top := TCtrlArranger.BottomOf(btnReset, 6);
  fColorBox.Top := TCtrlArranger.BottomOf(lblColour, 4);
end;

procedure THiliterPrefsFrame.btnResetClick(Sender: TObject);
begin
  fAttrs := THiliteAttrsFactory.CreateDefaultAttrs;
  UpdateControls;
  fChanged := True;
end;

procedure THiliterPrefsFrame.btnSaveStyleClick(Sender: TObject);
var
  NewName: string;
begin
  if not TNewHiliterNameDlg.Execute(Self, fNamedAttrs.Names, NewName) then
    Exit;
  fNamedAttrs[NewName] := fAttrs;
  UpdatePopupMenu;
end;

procedure THiliterPrefsFrame.btnStyleClick(Sender: TObject);
var
  PopupPos: TPoint; // place where menu pops up
begin
  PopupPos := ClientToScreen(
    Point(btnStyle.Left, btnStyle.Top + btnStyle.Height)
  );
  mnuStyles.Popup(PopupPos.X, PopupPos.Y);
end;

procedure THiliterPrefsFrame.cbColourChange(Sender: TObject);
begin
  CurrentElement.ForeColor := fColorBox.Selected;
  UpdatePreview;
  fChanged := True;
end;

procedure THiliterPrefsFrame.cbFontNameChange(Sender: TObject);
begin
  inherited;
  fAttrs.FontName := cbFontName.Text;
  UpdatePreview;
  fChanged := True;
end;

procedure THiliterPrefsFrame.cbFontSizeChange(Sender: TObject);
var
  Size: Integer;  // font size entered by user
begin
  inherited;
  // Do nothing if combo box text field cleared
  if cbFontSize.Text = '' then
    Exit;
  if TryStrToInt(cbFontSize.Text, Size) then
  begin
    if TFontHelper.IsInCommonFontSizeRange(Size) then
    begin
      // Combo has valid value entered: update
      fAttrs.FontSize := Size;
      UpdatePreview;
      fChanged := True;
    end
    else
    begin
      TMessageBox.Error(
        ParentForm,
        Format(
          sErrBadFontRange,
          [TFontHelper.CommonFontSizes.Min, TFontHelper.CommonFontSizes.Max]
        )
      );
      cbFontSize.Text := IntToStr(fAttrs.FontSize);
    end;
  end
  else
  begin
    // Combo has invalid value: say so
    TMessageBox.Error(ParentForm, sErrBadFontSize);
    cbFontSize.Text := IntToStr(fAttrs.FontSize);
  end;
end;

procedure THiliterPrefsFrame.ChkFontStyleClick(Sender: TObject);
var
  CB: TCheckBox;            // check box triggering event
  Elem: IHiliteElemAttrs;   // currently selected highlighter element
begin
  CB := Sender as TCheckBox;
  Elem := CurrentElement;
  // Update element's font style per font style stored in check box's tag
  if CB.Checked then
    Elem.FontStyle := Elem.FontStyle + [TFontStyle(CB.Tag)]
  else
    Elem.FontStyle := Elem.FontStyle - [TFontStyle(CB.Tag)];
  UpdatePreview;
  fChanged := True;
end;

constructor THiliterPrefsFrame.Create(AOwner: TComponent);
resourcestring
  // Colour dialogue style
  sDlgTitle = 'Choose Element Colour';  // colour dialogue title
begin
  inherited;
  HelpKeyword := 'HiliterPrefs';

  // Create object used to store customised attributes
  fAttrs := THiliteAttrsFactory.CreateDefaultAttrs;
  fNamedAttrs := THiliteAttrsFactory.CreateNamedAttrs;

  // Create and initialise custom color dialogue box
  fColorDlg := TColorDialogEx.Create(ParentForm);
  fColorDlg.Title := sDlgTitle;
  // cdShowHelp not included in fColorDlg.Options since setting HelpKeyword
  // property causes this style to be used
  fColorDlg.Options := [cdFullOpen];
  fColorDlg.HelpKeyword := 'ChooseElemColourDlg';

  // Create and initialise custom color combo box
  fColorBox := TColorBoxEx.Create(Self);  // automatically freed
  fColorBox.Parent := gbElements;
  fColorBox.Left := 248;
  fColorBox.Top := 36;
  fColorBox.Width := 137;
  fColorBox.Height := 22;
  fColorBox.NoneColorColor := clNone;
  // cbCustomColor not included in fColorBox.Style since assigning ColorDialog
  // property sets this style
  fColorBox.Style := [cbStandardColors, cbExtendedColors, cbSystemColors,
    cbIncludeNone, cbPrettyNames];
  fColorBox.ItemHeight := 16;
  fColorBox.TabOrder := 2;
  fColorBox.ColorDialog := fColorDlg;
  fColorBox.OnChange := cbColourChange;
  lblColour.FocusControl := fColorBox;

  // Populate list and combo controls
  PopulateElementsList;
  PopulateFontNameCombo;
  PopulateFontSizeCombo;

  // Store font style ordinal in font style check box tags
  chkBold.Tag := Ord(fsBold);
  chkItalics.Tag := Ord(fsItalic);
  chkUnderline.Tag := Ord(fsUnderline);

  // Set up predefined style drop down menu
  miClassic.Tag := Ord(hsCodeSnip);
  miDelphi7.Tag := Ord(hsDelphi7);
  miRADStudio.Tag := Ord(hsRADStudio);
  miVisualStudio.Tag := Ord(hsVisualStudio);
  miNoHilite.Tag := Ord(hsNul);

  // Clear dirty flag
  fChanged := False;
end;

function THiliterPrefsFrame.CurrentElement: IHiliteElemAttrs;
begin
  Result := fAttrs.Elements[CurrentElementId];
end;

function THiliterPrefsFrame.CurrentElementId: THiliteElement;
begin
  Result := THiliteElement(lbElements.Items.Objects[lbElements.ItemIndex]);
end;

procedure THiliterPrefsFrame.Deactivate(const Prefs: IPreferences);
begin
  Prefs.HiliteAttrs := fAttrs;
  Prefs.NamedHiliteAttrs := fNamedAttrs;
  Prefs.CustomHiliteColours.CopyFrom(fColorDlg.CustomColors, True);
end;

destructor THiliterPrefsFrame.Destroy;
begin
  FreeAndNil(fColorDlg);
  FreeAndNil(fColorBox);
  inherited;
end;

function THiliterPrefsFrame.DisplayName: string;
resourcestring
  sDisplayName = 'Syntax Highlighter';  // display name
begin
  Result := sDisplayName;
end;

function THiliterPrefsFrame.GenerateRTF: TRTFMarkup;
var
  RTFBuilder: TRTFBuilder;  // object used to create and render RTFBuilder
  EgLines: IStringList;     // list of lines in the example
  EgLine: string;           // each line of example
begin
  // Create builder object to create RTFBuilder document
  RTFBuilder := TRTFBuilder.Create(0); // use default code page
  try
    // Set up font and colour tables
    RTFBuilder.DefaultFontIdx := RTFBuilder.FontTable.Add(
      fAttrs.FontName, rgfModern, DEFAULT_CHARSET
    );
    RTFBuilder.ColourTable.Add(CurrentElement.ForeColor);

    // Set character formating
    RTFBuilder.SetFont(fAttrs.FontName);
    RTFBuilder.SetFontSize(fAttrs.FontSize);
    RTFBuilder.SetColour(CurrentElement.ForeColor);
    RTFBuilder.SetFontStyle(CurrentElement.FontStyle);

    // Write out each line of example
    EgLines := TIStringList.Create(cElementEgs[CurrentElementId], LF, False);
    for EgLine in EgLines do
    begin
      RTFBuilder.AddText(EgLine);
      RTFBuilder.EndPara;
    end;

    // Create RTFBuilder source
    Result := RTFBuilder.Render;

  finally
    RTFBuilder.Free;
  end;
end;

class function THiliterPrefsFrame.Index: Byte;
begin
  Result := 30;
end;

procedure THiliterPrefsFrame.lbElementsClick(Sender: TObject);
begin
  UpdateControls;
end;

procedure THiliterPrefsFrame.miNamedStylesClick(Sender: TObject);
var
  SelectedAttrs: IHiliteAttrs;
begin
  if TUserHiliterMgrDlg.Execute(Self, fNamedAttrs, SelectedAttrs) then
  begin
    (fAttrs as IAssignable).Assign(SelectedAttrs);
    fChanged := True;
    UpdateControls;
  end;
  UpdatePopupMenu;
end;

procedure THiliterPrefsFrame.PopulateElementsList;
var
  ElemId: THiliteElement; // loops thru all highlighter elements
begin
  lbElements.Clear;
  for ElemId := Low(THiliteElement) to High(THiliteElement) do
    if ElemId <> heWhitespace then
      // whitespace element ignored: don't customise since no text displayed
      lbElements.Items.AddObject(cElementDescs[ElemId], TObject(ElemId));
  // Select first item
  lbElements.ItemIndex := 0;
end;

procedure THiliterPrefsFrame.PopulateFontNameCombo;
begin
  TFontHelper.ListMonoSpaceFonts(cbFontName.Items);
end;

procedure THiliterPrefsFrame.PopulateFontSizeCombo;
begin
  TFontHelper.ListCommonFontSizes(cbFontSize.Items);
end;

procedure THiliterPrefsFrame.StyleMenuClick(Sender: TObject);
begin
  // Menu item's Tag property stores required style id
  fAttrs := THiliteAttrsFactory.CreatePredefinedAttrs(
    TPredefinedHiliteStyle((Sender as TMenuItem).Tag)
  );
  UpdateControls;
  fChanged := True;
end;

function THiliterPrefsFrame.UIUpdated: Boolean;
begin
  Result := fChanged;
end;

procedure THiliterPrefsFrame.UpdateControls;

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
  Elem: IHiliteElemAttrs; // currently selected highlighter element
begin
  Elem := CurrentElement;
  cbFontName.ItemIndex := cbFontName.Items.IndexOf(fAttrs.FontName);
  cbFontSize.Text := IntToStr(fAttrs.FontSize);
  SafeCheck(chkBold, fsBold in Elem.FontStyle);
  SafeCheck(chkItalics, fsItalic in Elem.FontStyle);
  SafeCheck(chkUnderline, fsUnderline in Elem.FontStyle);
  fColorBox.Selected := Elem.ForeColor;
  UpdatePreview;
end;

procedure THiliterPrefsFrame.UpdatePopupMenu;
begin
  miNamedStyles.Enabled := not fNamedAttrs.IsEmpty;
end;

procedure THiliterPrefsFrame.UpdatePreview;
begin
  frmExample.RichEdit.Load(GenerateRTF);
end;

initialization

// Register frame with preferences dialogue box
TPreferencesDlg.RegisterPage(THiliterPrefsFrame);

end.

