{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements various classes used to create content of a rich text document.
}


unit URTFBuilder;


interface


uses
  // Delphi
  Generics.Collections, Graphics,
  // Project
  UEncodings, URTFStyles, URTFUtils;


type
  ///  <summary>Encapsulates an RTF colour table.</summary>
  TRTFColourTable = class(TObject)
  strict private
    var
      ///  <summary>List of colours in table.</summary>
      fColours: TList<TColor>;
  public
    ///  <summary>Constructs object.</summary>
    constructor Create;
    ///  <summary>Destroys object.</summary>
    destructor Destroy; override;
    ///  <summary>Adds given colour to colour table unless it is already
    ///  present.</summary>
    procedure Add(const Colour: TColor);
    ///  <summary>Ensures that any colour defined in the given style is present
    ///  in the colour table.</summary>
    ///  <remarks>Does nothing if the style doesn't define a colour.</remarks>
    procedure AddFromStyle(const Style: TRTFStyle);
    ///  <summary>Returns index of given colour in colour table.</summary>
    ///  <exception>EBug raised if colour is not in colour table.</exception>
    function ColourRef(const Colour: TColor): Integer;
    ///  <summary>Builds and returns RTF code representing colour
    ///  table.</summary>
    function AsString: ASCIIString;
  end;

type
  ///  <summary>Encapsulates an RTF font table.</summary>
  TRTFFontTable = class(TObject)
  strict private
    var
      ///  <summary>List of fonts in table.</summary>
      fFonts: TList<TRTFFont>;  // List of fonts in table
    ///  <summary>Returns index of given font name in table or -1 if font not
    ///  present.</summary>
    function FindFont(const FontName: string): Integer;
  public
    ///  <summary>Constructs object.</summary>
    constructor Create;
    ///  <summary>Destroys object.</summary>
    destructor Destroy; override;
    ///  <summary>Adds new font to table if not already present.</summary>
    ///  <remarks>Does nothing except return index if a font with same name is
    ///  already in table.</remarks>
    ///  <param name="FontName">string [in] Name of font.</param>
    ///  <param name="Generic">TRTFGenericFont [in] Generic font family of font.
    ///  </param>
    ///  <param name="Charset">TFontCharset [in] Character set used by new font.
    ///  </param>
    ///  <returns>Integer. Index of font in font table, either existing or new.
    ///  </returns>
    function Add(const FontName: string; const Generic: TRTFGenericFont;
      const Charset: TFontCharset): Integer; overload;
    ///  <summary>Adds new font to table if not already present.</summary>
    ///  <remarks>Does nothing except return index if a font with same name is
    ///  already in table.</remarks>
    ///  <param name="Font">TRTFFont [in] Font to be added.</param>
    ///  <returns>Integer. Index of font in font table, either existing or new.
    ///  </returns>
    function Add(const Font: TRTFFont): Integer; overload;
    ///  <summary>Ensures that any font defined in the given style is present in
    ///  the font table.</summary>
    ///  <remarks>Does nothing if the style doesn't define a font or if the font
    ///  is null.</remarks>
    procedure AddFromStyle(const Style: TRTFStyle);
    ///  <summary>Returns index of a named font in table.</summary>
    ///  <exception>EBug raise if font not in table.</exception>
    function FontRef(const FontName: string): Integer;
    ///  <summary>Builds and returns RTF code representing font table.</summary>
    function AsString: ASCIIString;
  end;

type
  ///  <summary>Encapsulate properties of an RTF document.</summary>
  TRTFDocProperties = class(TObject)
  strict private
    var
      ///  <summary>Value of Title property.</summary>
      fTitle: string;
      ///  <summary>Code page used by document.</summary>
      fCodePage: Integer;
    ///  <summary>Checks if document properties are empty, i.e. have not be
    ///  defined.</summary>
    function IsEmpty: Boolean;
  public
    ///  <summary>Constructs object for document using given code page.
    ///  </summary>
    constructor Create(const CodePage: Integer);
    ///  <summary>Builds and returns RTF code representing document properties.
    ///  </summary>
    function AsString: ASCIIString;
    ///  <summary>Document title.</summary>
    property Title: string read fTitle write fTitle;
  end;

type
  ///  <summary>Class used to construct and render a RTF document.</summary>
  TRTFBuilder = class(TObject)
  strict private
    var
      ///  <summary>Accumulates RTF code for doc body.</summary>
      fBody: ASCIIString;
      ///  <summary>Code page used for RTF document.</summary>
      fCodePage: Integer;
      ///  <summary>Flag True when emitting RTF controls and False when emitting
      ///  text.</summary>
      fInControls: Boolean;
      ///  <summary>Value of ColourTable property.</summary>
      fColourTable: TRTFColourTable;
      ///  <summary>Value of FontTable property.</summary>
      fFontTable: TRTFFontTable;
      ///  <summary>Value of DefaultFontIdx property.</summary>
      fDefaultFontIdx: Integer;
      ///  <summary>Value of DocProperties property.</summary>
      fDocProperties: TRTFDocProperties;
    ///  <summary>Appends given text string to document body.</summary>
    procedure AppendBody(const S: ASCIIString);
    ///  <summary>Generates RTF code for document header.</summary>
    function DocHeader: ASCIIString;
    ///  <summary>Adds given RTF control to document body.</summary>
    procedure AddControl(const Ctrl: ASCIIString);
    ///  <summary>Generates RTF code for whole document.</summary>
    function AsString: ASCIIString;
  public
    ///  <summary>Constructs object for RTF document using given code page.
    ///  </summary>
    constructor Create(const CodePage: Integer);
    ///  <summary>Destroys object.</summary>
    destructor Destroy; override;
    ///  <summary>Ends a paragraph in document body.</summary>
    procedure EndPara;
    ///  <summary>Begins a new group in document body.</summary>
    procedure BeginGroup;
    ///  <summary>Closes current group in document body.</summary>
    procedure EndGroup;
    ///  <summary>Adds given text to document body.</summary>
    procedure AddText(const Text: string);
    ///  <summary>Clears paragraph formatting.</summary>
    procedure ClearParaFormatting;
    ///  <summary>Resets character styles to defaults.</summary>
    procedure ResetCharStyle;
    ///  <summary>Sets colour to be used as foreground colour for subsequent
    ///  text.</summary>.
    ///  <exception>EBug raised if colour not in colour table.</exception>
    procedure SetColour(const Colour: TColor);
    ///  <summary>Sets name of font to be used for subsequent text.</summary>
    ///  <exception>EBug raised if font name is not in font table.</exception>
    procedure SetFont(const FontName: string);
    ///  <summary>Sets size of font, in points, to be used for subsequent text.
    ///  </summary>
    procedure SetFontSize(const Points: Double);
    ///  <summary>Sets style of font to be used for subsequent text.</summary>
    procedure SetFontStyle(const Style: TFontStyles);
    ///  <summary>Sets before and after spacing, in points, to be used for
    ///  subsequent paragraphs.</summary>
    procedure SetParaSpacing(const Spacing: TRTFParaSpacing);
    ///  <summary>Sets paragraph and character styling for subsequent text
    ///  according to given RTF style.</summary>
    procedure ApplyStyle(const Style: TRTFStyle);
    ///  <summary>Generates RTF code for whole document.</summary>
    function Render: TRTF;
    ///  <summary>Table of colours used in document.</summary>
    property ColourTable: TRTFColourTable
      read fColourTable write fColourTable;
    ///  <summary>Table of fonts used in document.</summary>
    property FontTable: TRTFFontTable
      read fFontTable write fFontTable;
    ///  <summary>Index of default font in font table.</summary>
    property DefaultFontIdx: Integer
      read fDefaultFontIdx write fDefaultFontIdx;
    ///  <summary>Document's properties.</summary>
    property DocProperties: TRTFDocProperties
      read fDocProperties write fDocProperties;
  end;


implementation


uses
  // Delphi
  Generics.Defaults, Windows, Character,
  // Project
  UConsts, UExceptions, ULocales, UStrUtils, UUtils;


{ TRTFBuilder }

procedure TRTFBuilder.AddControl(const Ctrl: ASCIIString);
begin
  Assert((Ctrl <> '') and not TCharacter.IsWhiteSpace(Char(Ctrl[Length(Ctrl)])),
    ClassName + '.AddControls: Ctrl ends in whitespace');
  AppendBody(Ctrl);
  fInControls := True;
end;

procedure TRTFBuilder.AddText(const Text: string);
begin
  if fInControls then
  begin
    // We were emitting controls: need a space to terminate controls
    AppendBody(' ');
    fInControls := False;
  end;
  // Add text, escaping disallowed characters
  AppendBody(RTFMakeSafeText(Text, fCodePage));
end;

procedure TRTFBuilder.AppendBody(const S: ASCIIString);
begin
  fBody := fBody + S;
end;

procedure TRTFBuilder.ApplyStyle(const Style: TRTFStyle);
begin
  if scParaSpacing in Style.Capabilities then
    SetParaSpacing(Style.ParaSpacing);
  if scFont in Style.Capabilities then
    SetFont(Style.Font.Name);
  if scFontSize in Style.Capabilities then
    SetFontSize(Style.FontSize);
  if scFontStyles in Style.Capabilities then
    SetFontStyle(Style.FontStyles);
  if scColour in Style.Capabilities then
    SetColour(Style.Colour);
end;

function TRTFBuilder.AsString: ASCIIString;
begin
  Result := '{' + DocHeader + fBody + '}';
end;

procedure TRTFBuilder.BeginGroup;
begin
  AppendBody('{');
  fInControls := False;
end;

procedure TRTFBuilder.ClearParaFormatting;
begin
  AddControl(RTFControl(rcPard));
end;

constructor TRTFBuilder.Create(const CodePage: Integer);
begin
  inherited Create;
  if CodePage = 0 then
    fCodePage := ULocales.DefaultAnsiCodePage
  else
    fCodePage := CodePage;
  fColourTable := TRTFColourTable.Create;
  fFontTable := TRTFFontTable.Create;
  fDocProperties := TRTFDocProperties.Create(fCodePage);
  fBody := '';
  fInControls := False;
end;

destructor TRTFBuilder.Destroy;
begin
  fDocProperties.Free;
  fFontTable.Free;
  fColourTable.Free;
  inherited;
end;

function TRTFBuilder.DocHeader: ASCIIString;
begin
  Result := RTFControl(rcRTF, cRTFVersion)
    + RTFControl(rcAnsi)
    + RTFControl(rcAnsiCodePage, fCodePage)
    + RTFControl(rcDefFontNum, DefaultFontIdx)
    + RTFControl(rcDefLanguage, DefaultLanguageID)
    + fFontTable.AsString
    + fColourTable.AsString
    + fDocProperties.AsString
    + EOL;
end;

procedure TRTFBuilder.EndGroup;
begin
  AppendBody('}');
  fInControls := False;
end;

procedure TRTFBuilder.EndPara;
begin
  AddControl(RTFControl(rcPar));
  AppendBody(EOL);
  fInControls := False;
end;

function TRTFBuilder.Render: TRTF;
begin
  Result := TRTF.Create(AsString);
end;

procedure TRTFBuilder.ResetCharStyle;
begin
  AddControl(RTFControl(rcPlain));
end;

procedure TRTFBuilder.SetColour(const Colour: TColor);
begin
  AddControl(RTFControl(rcForeColorNum, fColourTable.ColourRef(Colour)));
end;

procedure TRTFBuilder.SetFont(const FontName: string);
var
  FontIdx: Integer; // index of font in font table
begin
  // We don't emit control if this is default font
  FontIdx := fFontTable.FontRef(FontName);
  if FontIdx <> DefaultFontIdx then
    AddControl(RTFControl(rcFontNum, FontIdx));
end;

procedure TRTFBuilder.SetFontSize(const Points: Double);
begin
  AddControl(RTFControl(rcFontSize, FloatToInt(2 * Points)));
end;

procedure TRTFBuilder.SetFontStyle(const Style: TFontStyles);
begin
  if fsBold in Style then
    AddControl(RTFControl(rcBold));
  if fsItalic in Style then
    AddControl(RTFControl(rcItalic));
  if fsUnderline in Style then
    AddControl(RTFControl(rcUnderline));
end;

procedure TRTFBuilder.SetParaSpacing(const Spacing: TRTFParaSpacing);
begin
  // Note: 20 Twips in a point
  AddControl(RTFControl(rcSpaceBefore, FloatToInt(20 * Spacing.Before)));
  AddControl(RTFControl(rcSpaceAfter, FloatToInt(20 * Spacing.After)));
end;

{ TRTFFontTable }

function TRTFFontTable.Add(const FontName: string;
  const Generic: TRTFGenericFont; const Charset: TFontCharset): Integer;
begin
  Result := FindFont(FontName);
  if Result = -1 then
    Result := fFonts.Add(TRTFFont.Create(FontName, Generic, Charset));
end;

function TRTFFontTable.Add(const Font: TRTFFont): Integer;
begin
  Result := FindFont(Font.Name);
  if Result = -1 then
    Result := fFonts.Add(Font);
end;

procedure TRTFFontTable.AddFromStyle(const Style: TRTFStyle);
begin
  if (scFont in Style.Capabilities) and not Style.Font.IsNull then
    Add(Style.Font);
end;

function TRTFFontTable.AsString: ASCIIString;
const
  // Map of generic font families to RTF controls
  cGenericFonts: array[TRTFGenericFont] of TRTFControl = (
    rcFontFamilyNil, rcFontFamilyRoman, rcFontFamilySwiss, rcFontFamilyModern,
    rcFontFamilyScript, rcFontFamilyDecor, rcFontFamilyTech
  );
var
  Idx: Integer;     // loops thru fonts in table
  Font: TRTFFont;   // reference to a font in table
begin
  Result := '{' + RTFControl(rcFontTable);
  for Idx := 0 to Pred(fFonts.Count) do
  begin
    Font := fFonts[Idx];
    Result := Result + '{'
      + RTFControl(rcFontNum, Idx)
      + RTFControl(rcFontPitch, 1)
      + RTFControl(cGenericFonts[Font.Generic])
      + RTFControl(rcFontCharset, Font.Charset)
      + ' '
      + StringToASCIIString(Font.Name)
      + '}';
  end;
  Result := Result + '}';
end;

constructor TRTFFontTable.Create;
begin
  inherited;
  fFonts := TList<TRTFFont>.Create(
    TDelegatedComparer<TRTFFont>.Create(
      function(const Left, Right: TRTFFont): Integer
      begin
        Result := Left.CompareTo(Right);
      end
    )
  );
end;

destructor TRTFFontTable.Destroy;
begin
  fFonts.Free;
  inherited;
end;

function TRTFFontTable.FindFont(const FontName: string): Integer;
begin
  Result := fFonts.IndexOf(TRTFFont.Create(FontName));
end;

function TRTFFontTable.FontRef(const FontName: string): Integer;
begin
  Result := FindFont(FontName);
  if Result = -1 then
    raise EBug.Create(ClassName + '.FontRef: Font not found');
end;

{ TRTFColourTable }

procedure TRTFColourTable.Add(const Colour: TColor);
begin
  if not fColours.Contains(Colour) then
    fColours.Add(Colour);
end;

procedure TRTFColourTable.AddFromStyle(const Style: TRTFStyle);
begin
  if scColour in Style.Capabilities then
    Add(Style.Colour);
end;

function TRTFColourTable.AsString: ASCIIString;
var
  Colour: TColor;     // each colour in table
  RGB: Cardinal;      // RGB representation of a colour
begin
  // Begin table
  Result := '{'
    + RTFControl(rcColorTable)
    + ' ';
  // Add entry for each colour
  for Colour in fColours do
  begin
    if Colour <> clNone then
    begin
      RGB := ColorToRGB(Colour);
      Result := Result
        + RTFControl(rcRed, GetRValue(RGB))
        + RTFControl(rcGreen, GetGValue(RGB))
        + RTFControl(rcBlue, GetBValue(RGB))
        + ';'
    end
    else
      // Colour not specified: provide empty entry
      Result := Result + ';'
  end;
  Result := Result + '}';
end;

function TRTFColourTable.ColourRef(const Colour: TColor): Integer;
begin
  Result := fColours.IndexOf(Colour);
  if Result = -1 then
    raise EBug.Create(ClassName + '.ColourRef: Unknown colour');
end;

constructor TRTFColourTable.Create;
begin
  inherited;
  fColours := TList<TColor>.Create; // use default integer comparer
  Add(clNone);
end;

destructor TRTFColourTable.Destroy;
begin
  fColours.Free;
  inherited;
end;

{ TRTFDocProperties }

function TRTFDocProperties.AsString: ASCIIString;
begin
  if IsEmpty then
  begin
    // No code if no properties
    Result := '';
    Exit;
  end;
  // Start with \info control word in group
  Result := '{' + RTFControl(rcInfo);
  if fTitle <> '' then
    Result := Result + RTFUnicodeSafeDestination(rcTitle, fTitle, fCodePage);
  // Close \info group
  Result := Result + '}';
end;

constructor TRTFDocProperties.Create(const CodePage: Integer);
begin
  inherited Create;
  fCodePage := CodePage;
end;

function TRTFDocProperties.IsEmpty: Boolean;
begin
  Result := fTitle = '';
end;

end.

