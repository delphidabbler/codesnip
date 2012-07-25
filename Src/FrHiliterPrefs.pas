{
 * FrHiliterPrefs.pas
 *
 * Implements a frame that allows user to set syntax highlighter preferences.
 * Designed for use as one of the tabs in the preferences dialog box.
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
 * The Original Code is FrHiliterPrefs.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributors:
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FrHiliterPrefs;


interface


uses
  // Delphi
  StdCtrls, Forms, Controls, Classes, Menus, Buttons,
  // Project
  FrPrefsBase, FrRTFShowCase, Hiliter.UGlobals, UColorBoxEx, UColorDialogEx,
  UConsts, UEncodings, UPreferences;


type

  {
  THiliterPrefsFrame:
    Frame that allows user to set syntax highlighter preferences. Can persist
    preferences entered by user. Note: Designed for use in preferences dialog
    box.
  }
  THiliterPrefsFrame = class(TPrefsBaseFrame)
    btnReset: TButton;
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
    lblNotice: TLabel;
    miClassic: TMenuItem;
    miDelphi7: TMenuItem;
    miDelphi2006: TMenuItem;
    miNoHilite: TMenuItem;
    miVisualStudio: TMenuItem;
    miSpacer: TMenuItem;
    mnuStyles: TPopupMenu;
    procedure btnResetClick(Sender: TObject);
    procedure btnStyleClick(Sender: TObject);
    procedure cbColourChange(Sender: TObject);
    procedure cbFontNameChange(Sender: TObject);
    procedure cbFontSizeChange(Sender: TObject);
    procedure ChkFontStyleClick(Sender: TObject);
    procedure lbElementsClick(Sender: TObject);
    procedure StyleMenuClick(Sender: TObject);
  strict private
    fColorBox: TColorBoxEx;     // Custom colour combo box component
    fColorDlg: TColorDialogEx;  // Custom colour dialog box (use with combo)
    fAttrs: IHiliteAttrs;       // Loads and records user's hilite preferences
    procedure PopulateElementsList;
      {Populates list box containing customisable highlighter attribute
      elements.
      }
    procedure PopulateFontSizeCombo;
      {Populates font size combo with common font sizes.
      }
    procedure PopulateFontNameCombo;
      {Populates font name combo with all supported monospace fonts.
      }
    function CurrentElementId: THiliteElement;
      {Gets id of highlighter element currently selected in Elements list box.
        @return Required element id.
      }
    function CurrentElement: IHiliteElemAttrs;
      {Gets reference to highlighter element currently selected in Elements list
      box.
        @return Required reference to element.
      }
    procedure UpdateControls;
      {Updates state of controls and preview to reflect currently selected
      highlighter element.
      }
    procedure UpdatePreview;
      {Updates preview of highlighting of current highlighter element.
      }
    function GenerateRTF: ASCIIString;
      {Generates RTF of example of current highlighter element.
        @return Required RTF code.
      }
    function ParentForm: TForm;
      {Gets reference to form that hosts the frame.
        @return Reference to host form or nil if no such host.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Creates custom control, populates and initialises
      controls and sets up object.
        @param AOwner [in] Not used.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    procedure Activate(const Prefs: IPreferences); override;
      {Called when page activated. Updates controls.
        @param Prefs [in] Object that provides info used to update controls.
      }
    procedure Deactivate(const Prefs: IPreferences); override;
      {Called when page is deactivated. Stores information entered by user.
        @param Prefs [in] Object used to store information.
      }
    procedure ArrangeControls; override;
      {Arranges controls on frame. Called after frame has been sized.
      }
    function DisplayName: string; override;
      {Caption that is displayed in the tab sheet that contains this frame when
      displayed in the preference dialog box.
        @return Required display name.
      }
    class function Index: Byte; override;
      {Index number that determines the location of the tab containing this
      frame when displayed in the preferences dialog box.
        @return Required index number.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, ExtCtrls, Windows, Graphics, Dialogs,
  // Project
  FmPreferencesDlg, Hiliter.UAttrs, IntfCommon, UCtrlArranger, UFontHelper,
  UMessageBox, URTFBuilder, URTFUtils, UUtils;


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

const
  // Map of highlighter elements to descriptions
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

  // Map of highlighter elements to examples
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

procedure THiliterPrefsFrame.Activate(const Prefs: IPreferences);
  {Called when page activated. Updates controls.
    @param Prefs [in] Object that provides info used to update controls.
  }
begin
  (fAttrs as IAssignable).Assign(Prefs.HiliteAttrs);
  Prefs.CustomHiliteColours.CopyTo(fColorDlg.CustomColors, True);
  UpdateControls;
end;

procedure THiliterPrefsFrame.ArrangeControls;
  {Arranges controls on frame. Called after frame has been sized.
  }
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
  fColorBox.Top := TCtrlArranger.BottomOf(lblColour, 4);
end;

procedure THiliterPrefsFrame.btnResetClick(Sender: TObject);
  {Reset button click handler. Reset syntax highlighter attributes to default
  values.
    @param Sender [in] Not used.
  }
begin
  fAttrs := THiliteAttrsFactory.CreateDefaultAttrs;
  UpdateControls;
end;

procedure THiliterPrefsFrame.btnStyleClick(Sender: TObject);
  {Predefined styles button click event handler. Displays menu containing
  available predefined styles.
    @param Sender [in] Not used.
  }
var
  PopupPos: TPoint; // place where menu pops up
begin
  PopupPos := ClientToScreen(
    Point(btnStyle.Left, btnStyle.Top + btnStyle.Height)
  );
  mnuStyles.Popup(PopupPos.X, PopupPos.Y);
end;

procedure THiliterPrefsFrame.cbColourChange(Sender: TObject);
  {Colour combo box OnChange handler. Sets foreground colour of current
  highlighter element to colour value selected by user.
    @param Sender [in] Not used.
  }
begin
  CurrentElement.ForeColor := fColorBox.Selected;
  UpdatePreview;
end;

procedure THiliterPrefsFrame.cbFontNameChange(Sender: TObject);
  {Font name combo box OnChange hander. Sets font name used by highlighter to
  value selected by user.
    @param Sender [in] Not used.
  }
begin
  inherited;
  fAttrs.FontName := cbFontName.Text;
  UpdatePreview;
end;

procedure THiliterPrefsFrame.cbFontSizeChange(Sender: TObject);
  {Font size combo box OnChange handler. Sets font size used by highlighter to
  value selected by user.
    @param Sender [in] Not used.
  }
var
  Size: Integer;  // font size entered by user
begin
  inherited;
  // Do nothing if combo box text field cleared
  if cbFontSize.Text = '' then
    Exit;
  if TryStrToInt(cbFontSize.Text, Size) then
  begin
    // Combo has valid value entered: update
    fAttrs.FontSize := Size;
    UpdatePreview;
  end
  else
  begin
    // Combo has invalid value: say so
    TMessageBox.Error(ParentForm, sErrBadFontSize);
    cbFontSize.Text := IntToStr(fAttrs.FontSize);
  end;
end;

procedure THiliterPrefsFrame.ChkFontStyleClick(Sender: TObject);
  {Handles clicks on any of the font style check boxes. Updates current
  highlighter element's font style accordingly.
    @param Sender [in] Reference to check box that triggered event.
  }
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
end;

constructor THiliterPrefsFrame.Create(AOwner: TComponent);
  {Class constructor. Creates custom components, populates and initialises
  controls and sets up object.
    @param AOwner [in] Not used.
  }
resourcestring
  // Colour dialog style
  sDlgTitle = 'Choose Element Colour';  // colour dialog title
begin
  inherited;
  HelpKeyword := 'HiliterPrefs';

  // Create object used to store customised attributes
  fAttrs := THiliteAttrsFactory.CreateDefaultAttrs;

  // Create and initialise custom color dialog box
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
  miDelphi2006.Tag := Ord(hsDelphi2006);
  miVisualStudio.Tag := Ord(hsVisualStudio);
  miNoHilite.Tag := Ord(hsNul);
end;

function THiliterPrefsFrame.CurrentElement: IHiliteElemAttrs;
  {Gets reference to highlighter element currently selected in Elements list
  box.
    @return Required reference to element.
  }
begin
  Result := fAttrs.Elements[CurrentElementId];
end;

function THiliterPrefsFrame.CurrentElementId: THiliteElement;
  {Gets id of highlighter element currently selected in Elements list box.
    @return Required element id.
  }
begin
  Result := THiliteElement(lbElements.Items.Objects[lbElements.ItemIndex]);
end;

procedure THiliterPrefsFrame.Deactivate(const Prefs: IPreferences);
  {Called when page is deactivated. Stores information entered by user.
    @param Prefs [in] Object used to store information.
  }
begin
  Prefs.HiliteAttrs := fAttrs;
  Prefs.CustomHiliteColours.CopyFrom(fColorDlg.CustomColors, True);
end;

destructor THiliterPrefsFrame.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fColorDlg);
  FreeAndNil(fColorBox);
  inherited;
end;

function THiliterPrefsFrame.DisplayName: string;
  {Caption that is displayed in the tab sheet that contains this frame when
  displayed in the preference dialog box.
    @return Required display name.
  }
resourcestring
  sDisplayName = 'Syntax Highlighter';  // display name
begin
  Result := sDisplayName;
end;

function THiliterPrefsFrame.GenerateRTF: ASCIIString;
  {Generates RTF of example of current highlighter element.
    @return Required RTF code.
  }
var
  RTF: TRTFBuilder;     // object used to create and render RTF
  EgLines: TStringList; // list of lines in the example
  LineIdx: Integer;     // loops thru lines of example
begin
  // Create builder object to create RTF document
  RTF := TRTFBuilder.Create;
  try
    // Set up font and colour tables
    RTF.DefaultFontIdx := RTF.FontTable.Add(
      fAttrs.FontName, rgfModern, DEFAULT_CHARSET
    );
    RTF.ColourTable.Add(CurrentElement.ForeColor);

    // Set character formating
    RTF.SetFont(fAttrs.FontName);
    RTF.SetFontSize(fAttrs.FontSize);
    RTF.SetColour(CurrentElement.ForeColor);
    RTF.SetFontStyle(CurrentElement.FontStyle);

    // Write out each line of example
    EgLines := TStringList.Create;
    try
      ExplodeStr(cElementEgs[CurrentElementId], LF, EgLines, False);
      for LineIdx := 0 to Pred(EgLines.Count) do
      begin
        RTF.AddText(EgLines[LineIdx]);
        RTF.EndPara;
      end;

      // Create RTF source
      Result := RTF.AsString;

    finally
      FreeAndNil(EgLines);
    end;
  finally
    FreeAndNil(RTF);
  end;
end;

class function THiliterPrefsFrame.Index: Byte;
  {Index number that determines the location of the tab containing this frame
  when displayed in the preferences dialog box.
    @return Required index number.
  }
begin
  Result := 30;
end;

procedure THiliterPrefsFrame.lbElementsClick(Sender: TObject);
  {Handles click on Elements list box. Updates controls.
    @param Sender [in] Not used.
  }
begin
  UpdateControls;
end;

function THiliterPrefsFrame.ParentForm: TForm;
  {Gets reference to form that hosts the frame.
    @return Reference to host form or nil if no such host.
  }
var
  ParentCtrl: TWinControl;  // reference to parent controls
begin
  // Loop through parent controls until form found or top level parent reached
  ParentCtrl := Self.Parent;
  while Assigned(ParentCtrl) and not (ParentCtrl is TForm) do
    ParentCtrl := ParentCtrl.Parent;
  if ParentCtrl is TForm then
    Result := ParentCtrl as TForm
  else
    Result := nil;
end;

procedure THiliterPrefsFrame.PopulateElementsList;
  {Populates list box containing customisable highlighter attribute elements.
  }
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
  {Populates font name combo with all supported monospace fonts.
  }
begin
  TFontHelper.ListMonoSpaceFonts(cbFontName.Items);
end;

procedure THiliterPrefsFrame.PopulateFontSizeCombo;
  {Populates font size combo with common font sizes.
  }
begin
  TFontHelper.ListCommonFontSizes(cbFontSize.Items);
end;

procedure THiliterPrefsFrame.StyleMenuClick(Sender: TObject);
  {Click event handler for all predefined style menu item styles.
    @param Sender [in] Menu item that triggered event.
  }
begin
  // Menu item's Tag property stores required style id
  fAttrs := THiliteAttrsFactory.CreatePredefinedAttrs(
    TPredefinedHiliteStyle((Sender as TMenuItem).Tag)
  );
  UpdateControls;
end;

procedure THiliterPrefsFrame.UpdateControls;
  {Updates state of controls and preview to reflect currently selected
  highlighter element.
  }
var
  Elem: IHiliteElemAttrs; // currently selected highlighter element
begin
  Elem := CurrentElement;
  cbFontName.ItemIndex := cbFontName.Items.IndexOf(fAttrs.FontName);
  cbFontSize.Text := IntToStr(fAttrs.FontSize);
  chkBold.Checked := fsBold in Elem.FontStyle;
  chkItalics.Checked := fsItalic in Elem.FontStyle;
  chkUnderline.Checked := fsUnderline in Elem.FontStyle;
  fColorBox.Selected := Elem.ForeColor;
  UpdatePreview;
end;

procedure THiliterPrefsFrame.UpdatePreview;
  {Updates preview of highlighting of current highlighter element.
  }
begin
  RTFLoadFromString(frmExample.RichEdit, GenerateRTF);
end;

initialization

// Register frame with preferences dialog box
TPreferencesDlg.RegisterPage(THiliterPrefsFrame);

end.

