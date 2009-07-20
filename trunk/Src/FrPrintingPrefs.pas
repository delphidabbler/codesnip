{
 * FrPrintingPrefs.pas
 *
 * Implements a frame that allows user to set printing preferences. Designed for
 * use as one of the tabs in the preferences dialog box.
 *
 * v1.0 of 07 Sep 2007  - Original version.
 * v1.1 of 17 Oct 2007  - Refactored code that gets names of measurement units.
 * v1.2 of 21 Apr 2008  - Replaced RichEdit control used to display source code
 *                        preview with a TRTFShowCaseFrame that doesn't permit
 *                        displayed source code to be selected.
 * v1.3 of 16 Dec 2008  - Changed preview frame to reflect syntax highlighting
 *                        changes in preferences, except that font is always
 *                        Courier New.
 *                      - Made private sections of classes strict.
 *                      - Made use TPageMargins constructor to set margins.
 * v1.4 of 13 Jan 2009  - Replaced control char literals with constants.
 * v1.5 of 19 Jul 2009  - Added no-nothing implementation of new inherited
 *                        ArrangeControls method.
 *                      - Resized some controls to accommodate Vista UI font.
 *                      - Used anchors for some controls to automate some
 *                        realignment.
 *
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
 * The Original Code is FrPrintingPrefs.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FrPrintingPrefs;


interface


uses
  // Delphi
  StdCtrls, Controls, Forms, Classes,
  // Project
  FrPrefsBase, FrRTFShowCase, IntfHiliter, UMeasurement, UPreferences;


type

  {
  TPrintingPrefsFrame:
    Frame that allows user to set printing preferences. Can persist preferences
    entered by user. Note: Designed for use in preferences dialog box.
  }
  TPrintingPrefsFrame = class(TPrefsBaseFrame)
    chkSyntaxPrint: TCheckBox;
    chkUseColor: TCheckBox;
    frmPreview: TRTFShowCaseFrame;
    gpMargins: TGroupBox;
    gpOutputOptions: TGroupBox;
    lblBottom: TLabel;
    lblLeft: TLabel;
    lblRight: TLabel;
    lblTop: TLabel;
    edBottom: TEdit;
    edLeft: TEdit;
    edRight: TEdit;
    edTop: TEdit;
    stInfo: TStaticText;
    procedure CheckboxClick(Sender: TObject);
    procedure NumEditKeyPress(Sender: TObject; var Key: Char);
  strict private
    fHiliteAttrs: IHiliteAttrs;
      {Style of syntax highlighting to use in sample output}
    fCurrentUnits: TMeasurementUnits;
      {Current measurement units}
    procedure DisplayPreview;
      {Displays preview of appearance of document according to current state of
      controls.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets up frame object.
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
  end;


implementation


uses
  // Delphi
  SysUtils, StrUtils, Windows, Graphics, Math, ComCtrls,
  // Project
  IntfCommon, UConsts, UHiliteAttrs, UPrintInfo, URTFBuilder, URTFUtils,
  USyntaxHiliters;


{$R *.dfm}


type

  {
  TPrintingPrefsPreview:
    Class that renders a preview of effect of printing preferences changes on a
    printed document.
  }
  TPrintingPrefsPreview = class(TObject)
  strict private
    fRE: TRichEdit;
      {Reference to richedit control used to render preview}
    fHiliteAttrs: IHiliteAttrs;
      {Attributes of syntax highlighter to use to render preview}
    function HiliteSource(const UseColor, SyntaxPrint: Boolean): string;
      {Generates sample highlighted source code.
        @param UseColor [in] Whether to use colour or mono highlighter.
        @param SyntaxPrint [in] Whether source code to be highlighted.
        @return Suitably highlighted source code.
      }
  public
    constructor Create(const RE: TRichEdit; const HiliteAttrs: IHiliteAttrs);
      {Class constructor. Sets up object.
        @param RE [in] Rich edit control used to render preview.
        @param HiliteAttrs [in] Current user defined highlighting.
      }
    procedure Generate(const UseColor, SyntaxPrint: Boolean);
      {Generates RTF source code for preview.
        @param UseColor [in] Whether preview to be in colour or monochrome.
        @param SyntaxPrint [in] Whether preview source code to be syntax
          hilited.
      }
  end;


{ TPrintingPrefsFrame }

procedure TPrintingPrefsFrame.Activate(const Prefs: IPreferences);
  {Called when page activated. Updates controls.
    @param Prefs [in] Object that provides info used to update controls.
  }
resourcestring
  // Caption of group box
  sMarginCaption = 'Page Margins (%s)';
var
  Margins: TPageMargins;  // stores page margins
begin
  // Update the caption to show current units
  gpMargins.Caption := Format(
    ' ' + sMarginCaption + ' ',
    [AnsiLowerCase(UnitName(Prefs.MeasurementUnits))]
  );

  // Update entries in margins edit boxes
  // record margins in mm
  Margins := Prefs.PrinterPageMargins;
  // adjust margins as required
  fCurrentUnits := Prefs.MeasurementUnits;
  case fCurrentUnits of
    muInches:
      // inches: convert mm to inches with max three decimal places
      Margins := TPageMargins.Create(
        RoundTo(MMToInches(Margins.Left), -3),
        RoundTo(MMToInches(Margins.Top), -3),
        RoundTo(MMToInches(Margins.Right), -3),
        RoundTo(MMToInches(Margins.Bottom), -3)
      );
    muMillimeters:
      // millimeters: max two decimal places
      Margins := TPageMargins.Create(
        RoundTo(Margins.Left, -2),
        RoundTo(Margins.Top, -2),
        RoundTo(Margins.Right, -2),
        RoundTo(Margins.Bottom, -2)
      );
  end;
  // write (converted) values into edit box
  edLeft.Text := FloatToStr(Margins.Left);
  edTop.Text := FloatToStr(Margins.Top);
  edRight.Text := FloatToStr(Margins.Right);
  edBottom.Text := FloatToStr(Margins.Bottom);

  // Update print options
  chkSyntaxPrint.Checked := (poSyntaxPrint in Prefs.PrinterOptions);
  chkUseColor.Checked := (poUseColor in Prefs.PrinterOptions);

  // Record current user highlighting choices and display initial preview
  (fHiliteAttrs as IAssignable).Assign(Prefs.HiliteAttrs);
  DisplayPreview;
end;

procedure TPrintingPrefsFrame.ArrangeControls;
  {Arranges controls on frame. Called after frame has been sized.
  }
begin
  // Do nothing: all controls arrange themselves using Anchors property
end;

procedure TPrintingPrefsFrame.CheckboxClick(Sender: TObject);
  {Handles clicks on all check boxes. Updates preview to reflect changes caused
  by changing check box state.
    @param Sender [in] Not used.
  }
begin
  DisplayPreview;
end;

constructor TPrintingPrefsFrame.Create(AOwner: TComponent);
  {Class constructor. Sets up frame object.
  }
begin
  inherited;
  // Create syntax highlighter object for use in sample output
  fHiliteAttrs := THiliteAttrsFactory.CreateDefaultAttrs;
end;

procedure TPrintingPrefsFrame.Deactivate(const Prefs: IPreferences);
  {Called when page is deactivated. Stores information entered by user.
    @param Prefs [in] Object used to store information.
  }
var
  Margins: TPageMargins;  // stores page margins
  Options: TPrintOptions; // stores printer options
begin
  inherited;
  // Record print options
  Options := [];
  if chkSyntaxPrint.Checked then
    Include(Options, poSyntaxPrint);
  if chkUseColor.Checked then
    Include(Options, poUseColor);
  Prefs.PrinterOptions := Options;

  // Record page margins
  // store page margins per edit controls
  Margins := TPageMargins.Create(
    StrToFloatDef(edLeft.Text, 0.0),
    StrToFloatDef(edTop.Text, 0.0),
    StrToFloatDef(edRight.Text, 0.0),
    StrToFloatDef(edBottom.Text, 0.0)
  );
  // if margins entered in inches, convert back to inches
  if fCurrentUnits = muInches then
  begin
    // entered as inches: convert to millimeters
    Margins := TPageMargins.Create(
      InchesToMM(Margins.Left),
      InchesToMM(Margins.Top),
      InchesToMM(Margins.Right),
      InchesToMM(Margins.Bottom)
    );
  end;
  // store margins in mm
  Prefs.PrinterPageMargins := Margins;
end;

procedure TPrintingPrefsFrame.DisplayPreview;
  {Displays preview of appearance of document according to current state of
  controls.
  }
var
  Preview: TPrintingPrefsPreview; // object that renders preview
begin
  Preview := TPrintingPrefsPreview.Create(frmPreview.RichEdit, fHiliteAttrs);
  try
    Preview.Generate(chkUseColor.Checked, chkSyntaxPrint.Checked);
  finally
    FreeAndNil(Preview);
  end;
end;

procedure TPrintingPrefsFrame.NumEditKeyPress(Sender: TObject;
  var Key: Char);
  {Called whenever a key is pressed in a margin edit control. We prevent all
  but valid number keys, a single decimal point and backspace.
    @param Sender [in] Not used.
    @param Key [in/out] Key that was pressed. Set to #0 to inhibit if not valid.
  }
begin
  if Key = DecimalSeparator then
  begin
    // Only allow decimal point if not already entered (can't have > 1)
    if AnsiContainsStr((Sender as TEdit).Text, DecimalSeparator) then
      Key := #0;
  end
  else if not (Key in [#8, '0'..'9']) then
    // Disallow any other characters other than backspace or digits
    Key := #0;
  if Key = #0 then
    // Beep because we disallowed key press
    MessageBeep(MB_ICONHAND);
end;

{ TPrintingPrefsPreview }

constructor TPrintingPrefsPreview.Create(const RE: TRichEdit;
  const HiliteAttrs: IHiliteAttrs);
  {Class constructor. Sets up object.
    @param RE [in] Rich edit control used to render preview.
    @param HiliteAttrs [in] Current user defined highlighting.
  }
begin
  Assert(Assigned(RE),    // ** do not localise
    'TPrintingPrefsPreview.Create: RE is nil');
  inherited Create;
  fRE := RE;
  fHiliteAttrs := HiliteAttrs;
end;

procedure TPrintingPrefsPreview.Generate(const UseColor, SyntaxPrint: Boolean);
  {Generates RTF source code for preview.
    @param UseColor [in] Whether preview to be in colour or monochrome.
    @param SyntaxPrint [in] Whether preview source code to be syntax hilited.
  }
const
  // Placeholder to be replaced by source code
  cPlaceholder = '[[%SourceCode%]]';
resourcestring
  // Heading and dummy paragraph text
  sHeading = 'Sample';
  sBodyText = 'Lorem ipsum dolor sit diam amet.';
var
  Builder: TRTFBuilder; // object used to assemble required RTF code
begin
  Builder := TRTFBuilder.Create;
  try
    // Set global document font and paragraph spacing
    Builder.FontTable.Add('Tahoma', rgfSwiss, 0);
    Builder.SetParaSpacing(0, 2);
    // Add heading text
    Builder.BeginGroup;
    Builder.SetFontSize(10);
    Builder.SetFontStyle([fsBold]);
    Builder.AddText(sHeading);
    Builder.EndGroup;
    Builder.EndPara;
    // Add dummy paragraph
    Builder.SetFontSize(9);
    Builder.AddText(sBodyText);
    Builder.EndPara;
    // Add placeholder for source code
    Builder.AddText(cPlaceholder);
    Builder.EndPara;
    // Load document into rich edit
    RTFLoadFromString(fRE, Builder.AsString);
  finally
    FreeAndNil(Builder);
  end;
  // Merge in source code
  fRE.SelStart := fRE.FindText(cPlaceholder, 0, MaxInt, []);
  fRE.SelLength := Length(cPlaceholder);
  RTFInsertString(fRE, HiliteSource(UseColor, SyntaxPrint));
end;

function TPrintingPrefsPreview.HiliteSource(const UseColor,
  SyntaxPrint: Boolean): string;
  {Generates sample highlighted source code.
    @param UseColor [in] Whether to use colour or mono highlighter.
    @param SyntaxPrint [in] Whether source code to be highlighted.
    @return Suitably highlighted source code.
  }
const
  // Sample source code displayed in preview
  cSourceCode = 'procedure Foo;' + EOL
    + 'begin' + EOL
    + '  // sample comment' + EOL
    + '  ShowMessage(''Bar'');' + EOL
    + 'end;';
var
  Hiliter: ISyntaxHiliter;  // highlighter object
  Attrs: IHiliteAttrs;      // highlighter attributes
begin
  // Determine which highlighter to use depending on options
  if not SyntaxPrint then
    // default highlighter - no syntax highlighting
    Attrs := THiliteAttrsFactory.CreatePrintAttrs(nil, False)
  else
    // user-defined highlighter, maybe in mono
    Attrs := THiliteAttrsFactory.CreatePrintAttrs(fHiliteAttrs, UseColor);
  // Perform highlighting
  Hiliter := TSyntaxHiliterFactory.CreateHiliter(hkRTF);
  Result := Hiliter.Hilite(cSourceCode, Attrs);
end;

end.

