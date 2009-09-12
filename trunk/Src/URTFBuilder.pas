{
 * URTFBuilder.pas
 *
 * Implements various classes used to create content of a rich text document.
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
 * The Original Code is URTFBuilder.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit URTFBuilder;


interface


uses
  // Delphi
  Classes, Graphics,
  // Project
  UConsts, ULists;


type

  {
  TRTFColourTable:
    Class that builds, interogates and renders an RTF colour table.
  }
  TRTFColourTable = class(TObject)
  strict private
    var fColours: TList;  // List of colours in table
    function GetColour(const Idx: Integer): TColor;
      {Gets colour from table.
        @param Idx [in] Index of colour in table.
        @return Colour at specified index.
      }
    function ColourCount: Integer;
      {Gets number of colours in table.
        @return Number of colours.
      }
    function FindColour(const Colour: TColor): Integer;
      {Finds a colour in table.
        @param Colour [in] Colour to be found.
        @return Index of colour in table or -1 if not present.
      }
  public
    constructor Create;
      {Class constructor. Sets up table.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    function Add(const Colour: TColor): Integer;
      {Adds colour to table if it is not already present.
        @param Colour [in] Colour to add to table.
        @return Index of colour in table (either existing or new index).
      }
    function ColourRef(const Colour: TColor): Integer;
      {Gets index of a colour in table.
        @param Colour [in] Colour whose index is required.
        @return Required index.
        @except EBug raised if colour not in table.
      }
    function AsString: string;
      {Builds RTF code representing colour table.
        @return Required RTF code.
      }
  end;

  {
  TRTFGenericFont:
    Enumerates supported generic font families.
  }
  TRTFGenericFont = (
    rgfDontCare,      // unknown or unspecified font family
    rgfRoman,         // proportional serif font
    rgfSwiss,         // proportional sans-serif font
    rgfModern,        // fixed pitch serif or sans-serif font
    rgfScript,        // script font
    rgfDecorative,    // decorative font
    rgfTechnical      // technical, symbol or maths font
  );

  {
  TRTFFont:
    Representation of a font in font table.
  }
  TRTFFont = class(TObject)
  strict private
    var
      fName: string;              // Value of Name property
      fCharset: TFontCharset;     // Value of Charset property
      fGeneric: TRTFGenericFont;  // Value of Generic property
  public
    constructor Create(const Name: string; const Generic: TRTFGenericFont;
      const Charset: TFontCharset);
      {Class constructor. Sets up object.
        @param Name [in] Name of font.
        @param Generic [in] Generic font family to be used if font not
          available.
        @param Charset [in] Font's character set.
      }
    function IsEqual(const FontName: string): Boolean;
      {Checks if font's name is equal to another.
        @param FontName [in] Name of font to be compared.
        @return True if font name's equal.
      }
    property Name: string read fName write fName;
      {Name of font}
    property Generic: TRTFGenericFont read fGeneric write fGeneric;
      {Generic font family to be used if font not available}
    property Charset: TFontCharset read fCharset write fCharset;
      {Character set used by font}
  end;

  {
  TRTFFontTable:
    Class that builds, interogates and renders an RTF font table.
  }
  TRTFFontTable = class(TObject)
  strict private
    var
      fFonts: TObjectListEx;  // List of fonts in table
    function GetFont(const Idx: Integer): TRTFFont;
      {Gets reference to font in table.
        @param Idx [in] Index of font in table.
        @return Required font.
      }
    function FontCount: Integer;
      {Number of fonts in table.
        @return Number of fonts.
      }
    function FindFont(const FontName: string): Integer;
      {Finds index of a named font in font table.
        @param FontName [in] Name of font to be found.
        @return Index of font in table or -1 if not present.
      }
  public
    constructor Create;
      {Class constructor. Sets up object.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    function Add(const FontName: string; const Generic: TRTFGenericFont;
      const Charset: TFontCharset): Integer;
      {Adds a new font to table if not already present.
        @param FontName [in] Name of new font.
        @param Generic [in] Generic font family of new font.
        @param Charset [in] Character set used by new font.
        @return Index of font in font table (either existing or new)
      }
    function FontRef(const FontName: string): Integer;
      {Gets index of a named font in table.
        @param FontName [in] Name of font whose index is required.
        @return Required index.
        @except EBug raised if font not in table.
      }
    function AsString: string;
      {Builds RTF code representing font table.
        @return Required RTF code.
      }
  end;

  {
  TRTFDocProperties:
    Class that stores RTF document properties and builds RTF code.
  }
  TRTFDocProperties = class(TObject)
  strict private
    var fTitle: string; // Value of Title property
    function IsEmpty: Boolean;
      {Checks if document properties are empty, i.e. non have be defined.
        @return True if no document properties have been defined, False
          otherwise.
      }
  public
    function AsString: string;
      {Builds RTF code representing document properties.
        @return Required RTF code.
      }
    property Title: string read fTitle write fTitle;
      {Document title}
  end;

  {
  TRTFBuilder:
    Class used to create content of a rich text document.
  }
  TRTFBuilder = class(TObject)
  strict private
    var
      fBody: string;                      // Accumulates RTF code for doc body
      fInControls: Boolean;               // Tells of emitting RTF ctrls or text
      fColourTable: TRTFColourTable;      // Value of ColourTable property
      fFontTable: TRTFFontTable;          // Value of FontTable property
      fDefaultFontIdx: Integer;           // Value of DefaultFontIdx property
      fDocProperties: TRTFDocProperties;  // Value of DocProperties property
    procedure AppendBody(const S: string);
      {Appends string data to document body.
        @param S [in] String data to add.
      }
    function DocHeader: string;
      {Generates document header RTF.
        @return Required RTF.
      }
    procedure AddControl(const Ctrl: string);
      {Adds an RTF control to document body.
        @param Ctrl [in] Text representation of control to be added.
      }
  public
    constructor Create;
      {Class constructor. Sets up object.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    procedure EndPara;
      {Ends a paragraph.
      }
    procedure BeginGroup;
      {Start a new group in document body.
      }
    procedure EndGroup;
      {Closes a group in document body.
      }
    procedure AddText(const Text: string);
      {Adds text to document body.
        @param Text [in] Text to be added.
      }
    procedure ClearParaFormatting;
      {Clears paragrapah formatting.
      }
    procedure ResetCharStyle;
      {Resets character styles to defaults.
      }
    procedure SetColour(const Colour: TColor);
      {Sets a foreground colour to be used for subsequent text.
        @param Colour [in] Required colour.
        @except Exception raised if colour not in colour table.
      }
    procedure SetFont(const FontName: string);
      {Sets a font to be used for subsequent text.
        @param FontName [in] Name of required font.
        @except Exception raised if font not in font table.
      }
    procedure SetFontSize(const Points: Double);
      {Sets size of font used for subsequent text.
        @param Size [in] Size of font in points.
      }
    procedure SetFontStyle(const Style: TFontStyles);
      {Sets style of font used for subsequent text.
        @param Style [in] Font style.
      }
    procedure SetParaSpacing(const PtsBefore, PtsAfter: Double);
      {Sets spacing above and below a paragraph.
        @param Before [in] Spacing before paragraph in points.
        @param After [in] Spacing after paragraph in points.
      }
    function AsString: string;
      {Generates RTF code for whole document.
        @return Required RTF.
      }
    property ColourTable: TRTFColourTable
      read fColourTable write fColourTable;
      {Table of colours used in document}
    property FontTable: TRTFFontTable
      read fFontTable write fFontTable;
      {Table of fonts used in document}
    property DefaultFontIdx: Integer
      read fDefaultFontIdx write fDefaultFontIdx;
      {Index of default font in font table}
    property DocProperties: TRTFDocProperties
      read fDocProperties write fDocProperties;
      {Document's properties}
  end;


implementation


uses
  // Delphi
  SysUtils, Windows,
  // Project
  UExceptions, URTFUtils;


function FloatToInt(const F: Double): Int64;
  {Converts a floating point number to an integer, rounding to nearest integer.
    @param F [in] Floating point number to be rounded off.
    @return Rounded value as integer.
  }
begin
  Result := Trunc(F + 0.500001);
end;


{ TRTFBuilder }

procedure TRTFBuilder.AddControl(const Ctrl: string);
  {Adds an RTF control to document body.
    @param Ctrl [in] Text representation of control to be added.
  }
begin
  Assert((Ctrl <> '') and not (Ctrl[Length(Ctrl)] in [' ', LF, CR, TAB]),
    ClassName + '.AddControls: Ctrls ends in whitespace');
  AppendBody(Ctrl);
  fInControls := True;
end;

procedure TRTFBuilder.AddText(const Text: string);
  {Adds text to document body.
    @param Text [in] Text to be added.
  }
begin
  if fInControls then
  begin
    // We were emitting controls: need a space to terminate controls
    AppendBody(' ');
    fInControls := False;
  end;
  // Add text, escaping disallowed characters
  AppendBody(RTFMakeSafeText(Text));
end;

procedure TRTFBuilder.AppendBody(const S: string);
  {Appends string data to document body.
    @param S [in] String data to add.
  }
begin
  fBody := fBody + S;
end;

function TRTFBuilder.AsString: string;
  {Generates RTF code for whole document.
    @return Required RTF.
  }
begin
  Result := '{' + DocHeader + fBody + '}';
end;

procedure TRTFBuilder.BeginGroup;
  {Start a new group in document body.
  }
begin
  AppendBody('{');
  fInControls := False;
end;

procedure TRTFBuilder.ClearParaFormatting;
  {Clears paragrapah formatting.
  }
begin
  AddControl(RTFControl(rcPard));
end;

constructor TRTFBuilder.Create;
  {Class constructor. Sets up object.
  }
begin
  inherited;
  fColourTable := TRTFColourTable.Create;
  fFontTable := TRTFFontTable.Create;
  fDocProperties := TRTFDocProperties.Create;
  fBody := '';
  fInControls := False;
end;

destructor TRTFBuilder.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fDocProperties);
  FreeAndNil(fFontTable);
  FreeAndNil(fColourTable);
  inherited;
end;

function TRTFBuilder.DocHeader: string;
  {Generates document header RTF.
    @return Required RTF.
  }
begin
  Result := RTFControl(rcRTF, cRTFVersion)
    + RTFControl(rcAnsi)
    + RTFControl(rcAnsiCodePage, cDefCodePage)
    + RTFControl(rcDefFontNum, DefaultFontIdx)
    + RTFControl(rcDefLanguage, cDefLanguage)
    + fFontTable.AsString
    + fColourTable.AsString
    + fDocProperties.AsString
    + EOL;
end;

procedure TRTFBuilder.EndGroup;
  {Closes a group in document body.
  }
begin
  AppendBody('}');
  fInControls := False;
end;

procedure TRTFBuilder.EndPara;
  {Ends a paragraph.
  }
begin
  AddControl(RTFControl(rcPar));
  AppendBody(EOL);
  fInControls := False;
end;

procedure TRTFBuilder.ResetCharStyle;
  {Resets character styles to defaults.
  }
begin
  AddControl(RTFControl(rcPlain));
end;

procedure TRTFBuilder.SetColour(const Colour: TColor);
  {Sets a foreground colour to be used for subsequent text.
    @param Colour [in] Required colour.
    @except Exception raised if colour not in colour table.
  }
begin
  AddControl(RTFControl(rcForeColorNum, fColourTable.ColourRef(Colour)));
end;

procedure TRTFBuilder.SetFont(const FontName: string);
  {Sets a font to be used for subsequent text.
    @param FontName [in] Name of required font.
    @except Exception raised if font not in font table.
  }
var
  FontIdx: Integer; // index of font in font table
begin
  // We don't emit control if this is default font
  FontIdx := fFontTable.FontRef(FontName);
  if FontIdx <> DefaultFontIdx then
    AddControl(RTFControl(rcFontNum, FontIdx));
end;

procedure TRTFBuilder.SetFontSize(const Points: Double);
  {Sets size of font used for subsequent text.
    @param Points [in] Size of font in points.
  }
begin
  AddControl(RTFControl(rcFontSize, FloatToInt(2 * Points)));
end;

procedure TRTFBuilder.SetFontStyle(const Style: TFontStyles);
  {Sets style of font used for subsequent text.
    @param Style [in] Font style.
  }
begin
  if fsBold in Style then
    AddControl(RTFControl(rcBold));
  if fsItalic in Style then
    AddControl(RTFControl(rcItalic));
  if fsUnderline in Style then
    AddControl(RTFControl(rcUnderline));
end;

procedure TRTFBuilder.SetParaSpacing(const PtsBefore, PtsAfter: Double);
  {Sets spacing above and below a paragraph.
    @param Before [in] Spacing before paragraph in points.
    @param After [in] Spacing after paragraph in points.
  }
begin
  // Note: 20 Twips in a point
  AddControl(RTFControl(rcSpaceBefore, FloatToInt(20 * PtsBefore)));
  AddControl(RTFControl(rcSpaceAfter, FloatToInt(20 * PtsAfter)));
end;

{ TRTFFont }

constructor TRTFFont.Create(const Name: string;
  const Generic: TRTFGenericFont; const Charset: TFontCharset);
  {Class constructor. Sets up object.
    @param Name [in] Name of font.
    @param Generic [in] Generic font family to be used if font not available.
    @param Charset [in] Font's character set.
  }
begin
  inherited Create;
  fName := Name;
  fGeneric := Generic;
  fCharset := Charset;
end;

function TRTFFont.IsEqual(const FontName: string): Boolean;
  {Checks if font's name is equal to another.
    @param FontName [in] Name of font to be compared.
    @return True if font name's equal.
  }
begin
  Result := AnsiSameText(Self.fName, FontName);
end;

{ TRTFFontTable }

function TRTFFontTable.Add(const FontName: string;
  const Generic: TRTFGenericFont; const Charset: TFontCharset): Integer;
  {Adds a new font to table if not already present.
    @param FontName [in] Name of new font.
    @param Generic [in] Generic font family of new font.
    @param Charset [in] Character set used by new font.
    @return Index of font in font table (either existing or new)
  }
begin
  Result := FindFont(FontName);
  if Result = -1 then
    Result := fFonts.Add(TRTFFont.Create(FontName, Generic, Charset));
end;

function TRTFFontTable.AsString: string;
  {Builds RTF code representing font table.
    @return Required RTF code.
  }
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
  for Idx := 0 to Pred(FontCount) do
  begin
    Font := GetFont(Idx);
    Result := Result + '{'
      + RTFControl(rcFontNum, Idx)
      + RTFControl(rcFontPitch, 1)
      + RTFControl(cGenericFonts[Font.Generic])
      + RTFControl(rcFontCharset, Font.Charset)
      + ' '
      + Font.Name
      + '}';
  end;
  Result := Result + '}';
end;

constructor TRTFFontTable.Create;
  {Class constructor. Sets up object.
  }
begin
  inherited;
  fFonts := TObjectListEx.Create(True);
end;

destructor TRTFFontTable.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fFonts);   // frees owned objects
  inherited;
end;

function TRTFFontTable.FindFont(const FontName: string): Integer;
  {Finds index of a named font in font table.
    @param FontName [in] Name of font to be found.
    @return Index of font in table or -1 if not present.
  }
var
  Idx: Integer;   // loops through fonts in table
begin
  Result := -1;
  for Idx := 0 to Pred(FontCount) do
  begin
    if GetFont(Idx).IsEqual(FontName) then
    begin
      Result := Idx;
      Break;
    end;
  end;
end;

function TRTFFontTable.FontCount: Integer;
  {Number of fonts in table.
    @return Number of fonts.
  }
begin
  Result := fFonts.Count;
end;

function TRTFFontTable.FontRef(const FontName: string): Integer;
  {Gets index of a named font in table.
    @param FontName [in] Name of font whose index is required.
    @return Required index.
    @except EBug raised if font not in table.
  }
begin
  Result := FindFont(FontName);
  if Result = -1 then
    raise EBug.Create(ClassName + '.FontRef: Font not found');
end;

function TRTFFontTable.GetFont(const Idx: Integer): TRTFFont;
  {Gets reference to font in table.
    @param Idx [in] Index of font in table.
    @return Required font.
  }
begin
  Result := fFonts[Idx] as TRTFFont;
end;

{ TRTFColourTable }

function TRTFColourTable.Add(const Colour: TColor): Integer;
  {Adds colour to table if it is not already present.
    @param Colour [in] Colour to add to table.
    @return Index of colour in table (either existing or new index).
  }
begin
  Result := FindColour(Colour);
  if Result = -1 then
    Result := fColours.Add(Pointer(Colour));
end;

function TRTFColourTable.AsString: string;
  {Builds RTF code representing colour table.
    @return Required RTF code.
  }
var
  ColourIdx: Integer; // loops through colours in table
  RGB: Cardinal;      // RGB representation of a colour
begin
  // Begin table
  Result := '{'
    + RTFControl(rcColorTable)
    + ' ';
  // Add entry for each colour
  for ColourIdx := 0 to Pred(ColourCount) do
  begin
    if GetColour(ColourIdx) <> clNone then
    begin
      RGB := ColorToRGB(GetColour(ColourIdx));
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

function TRTFColourTable.ColourCount: Integer;
  {Gets number of colours in table.
    @return Number of colours.
  }
begin
  Result := fColours.Count;
end;

function TRTFColourTable.ColourRef(const Colour: TColor): Integer;
  {Gets index of a colour in table.
    @param Colour [in] Colour whose index is required.
    @return Required index.
    @except EBug raised if colour not in table.
  }
begin
  Result := FindColour(Colour);
  if Result = -1 then
    raise EBug.Create(ClassName + '.ColourRef: Unknown colour');
end;

constructor TRTFColourTable.Create;
  {Class constructor. Sets up table.
  }
begin
  inherited;
  fColours := TList.Create;
  Add(clNone);
end;

destructor TRTFColourTable.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fColours);
  inherited;
end;

function TRTFColourTable.FindColour(const Colour: TColor): Integer;
  {Finds a colour in table.
    @param Colour [in] Colour to be found.
    @return Index of colour in table or -1 if not present.
  }
var
  Idx: Integer; // loops through all colours in table
begin
  Result := -1;
  for Idx := 0 to Pred(ColourCount) do
  begin
    if GetColour(Idx) = Colour then
    begin
      Result := Idx;
      Break;
    end;
  end;
end;

function TRTFColourTable.GetColour(const Idx: Integer): TColor;
  {Gets colour from table.
    @param Idx [in] Index of colour in table.
    @return Colour at specified index.
  }
begin
  Result := TColor(fColours[Idx]);
end;

{ TRTFDocProperties }

function TRTFDocProperties.AsString: string;
  {Builds RTF code representing document properties.
    @return Required RTF code.
  }
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
    // Add \title group
    Result := Result + '{'
      + RTFControl(rcTitle) + ' ' + RTFMakeSafeText(fTitle)
      + '}';
  // Close \info group
  Result := Result + '}';
end;

function TRTFDocProperties.IsEmpty: Boolean;
  {Checks if document properties are empty, i.e. non have be defined.
    @return True if no document properties have been defined, False otherwise.
  }
begin
  Result := fTitle = '';
end;

end.

