{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines structures that encapsulate RTF styling elements.
}


unit URTFStyles;


interface


uses
  // Delphi
  Graphics;


type
  ///  <summary>Enumerates supported generic font families.</summary>
  TRTFGenericFont = (
    rgfDontCare,      // unknown or unspecified font family
    rgfRoman,         // proportional serif font
    rgfSwiss,         // proportional sans-serif font
    rgfModern,        // fixed pitch serif or sans-serif font
    rgfScript,        // script font
    rgfDecorative,    // decorative font
    rgfTechnical      // technical, symbol or maths font
  );

type
  ///  <summary>Representation of a font in font table.</summary>
  TRTFFont = record
  strict private
    var
      ///  <summary>Value of Name property.</summary>
      fName: string;
      ///  <summary>Value of CharSet property.</summary>
      fCharset: TFontCharset;
      ///  <summary>Value of Generic property.</summary>
      fGeneric: TRTFGenericFont;
  public
    ///  <summary>Initialises record's properties.</summary>
    ///  <param name="Name">string [in] Font name.</param>
    ///  <param name="Generic">TRTFGenericFont [in] Generic font family to be
    ///  used if font not available.</param>
    ///  <param name="Charset">TFontCharset [in] Font's character set.</param>
    constructor Create(const Name: string;
      const Generic: TRTFGenericFont = rgfDontCare;
      const Charset: TFontCharset = 0);
    ///  <summary>Creates and returns a new null RTF font.</summary>
    class function CreateNull: TRTFFont; static;
    ///  <summary>Checks if the font is null.</summary>
    function IsNull: Boolean;
    ///  <summary>Compares this font to another.</summary>
    ///  <param name="RTFFont">TRTFont [in] Other font to be compared.</param>
    ///  <returns>Integer. -ve if this font is less than RTFFont, 0 if same and
    ///  +ve if RTFFont is less than this one.</returns>
    function CompareTo(const RTFFont: TRTFFont): Integer;
    ///  <summary>Name of font.</summary>
    property Name: string read fName write fName;
    ///  <summary>Generic font family to be used if font not available.
    ///  </summary>
    property Generic: TRTFGenericFont read fGeneric write fGeneric;
    ///  <summary>Character set used by font.</summary>
    property Charset: TFontCharset read fCharset write fCharset;
  end;

type
  TRTFParaSpacing = record
  public
    var
      Before: Double;
      After: Double;
    constructor Create(const ABefore, AAfter: Double);
    class function CreateNull: TRTFParaSpacing; static;
    class operator Equal(const Left, Right: TRTFParaSpacing): Boolean;
    class operator NotEqual(const Left, Right: TRTFParaSpacing): Boolean;
  end;

type
  TRTFStyleCap = (
    scParaSpacing,
    scFont,
    scFontSize,
    scFontStyles,
    scColour
  );

type
  TRTFStyleCaps = set of TRTFStyleCap;

type
  TRTFStyle = record
  public
    var
      ParaSpacing: TRTFParaSpacing;
      Font: TRTFFont;
      FontSize: Double;
      FontStyles: TFontStyles;
      Colour: TColor;
      Capabilities: TRTFStyleCaps;
    constructor Create(const ACapabilities: TRTFStyleCaps;
      const AParaSpacing: TRTFParaSpacing; const AFont: TRTFFont;
      const AFontSize: Double; const AFontStyles: TFontStyles;
      const AColour: TColor); overload;
    constructor Create(const ACapabilities: TRTFStyleCaps;
      const AFont: TRTFFont; const AFontSize:
      Double; const AFontStyles: TFontStyles; const AColour: TColor); overload;
    constructor Create(const AParaSpacing: TRTFParaSpacing); overload;
    class function CreateNull: TRTFStyle; static;
    function IsNull: Boolean;
    class operator Equal(const Left, Right: TRTFStyle): Boolean;
    class operator NotEqual(const Left, Right: TRTFStyle): Boolean;
  end;


implementation


uses
  // Delphi
  Math,
  // Project
  UStrUtils;


{ TRTFFont }

function TRTFFont.CompareTo(const RTFFont: TRTFFont): Integer;
begin
  Result := StrCompareText(Self.Name, RTFFont.Name);
end;

constructor TRTFFont.Create(const Name: string;
  const Generic: TRTFGenericFont; const Charset: TFontCharset);
begin
  fName := Name;
  fGeneric := Generic;
  fCharset := Charset;
end;

class function TRTFFont.CreateNull: TRTFFont;
begin
  Result := TRTFFont.Create('');
end;

function TRTFFont.IsNull: Boolean;
begin
  Result := fName = '';
end;

{ TRTFParaSpacing }

constructor TRTFParaSpacing.Create(const ABefore, AAfter: Double);
begin
  Before := ABefore;
  After := AAfter;
end;

class function TRTFParaSpacing.CreateNull: TRTFParaSpacing;
begin
  Result := TRTFParaSpacing.Create(0.0, 0.0);
end;

class operator TRTFParaSpacing.Equal(const Left,
  Right: TRTFParaSpacing): Boolean;
begin
  Result := (Left.Before = Right.Before) and (Left.After = Right.After);
end;

class operator TRTFParaSpacing.NotEqual(const Left,
  Right: TRTFParaSpacing): Boolean;
begin
  Result := (Left.Before <> Right.Before) or (Left.After <> Right.After);
end;

{ TRTFStyle }

constructor TRTFStyle.Create(const ACapabilities: TRTFStyleCaps;
  const AParaSpacing: TRTFParaSpacing; const AFont: TRTFFont;
  const AFontSize: Double; const AFontStyles: TFontStyles;
  const AColour: TColor);
begin
  Capabilities := ACapabilities;
  ParaSpacing := AParaSpacing;
  Font := AFont;
  FontSize := AFontSize;
  FontStyles := AFontStyles;
  Colour := AColour;
end;

constructor TRTFStyle.Create(const ACapabilities: TRTFStyleCaps;
  const AFont: TRTFFont; const AFontSize: Double;
  const AFontStyles: TFontStyles; const AColour: TColor);
begin
  Create(
    ACapabilities - [scParaSpacing],
    TRTFParaSpacing.CreateNull,
    AFont,
    AFontSize,
    AFontStyles,
    AColour
  );
end;

constructor TRTFStyle.Create(const AParaSpacing: TRTFParaSpacing);
begin
  Create([scParaSpacing], AParaSpacing, TRTFFont.CreateNull, 0.0, [], clNone);
end;

class function TRTFStyle.CreateNull: TRTFStyle;
begin
  Result := TRTFStyle.Create(
    [], TRTFParaSpacing.CreateNull, TRTFFont.CreateNull, 0.0, [], clNone
  );
end;

class operator TRTFStyle.Equal(const Left, Right: TRTFStyle): Boolean;
begin
  if (Left.Capabilities = []) and (Right.Capabilities = []) then
    Exit(True);
  Result := (Left.Capabilities = Right.Capabilities)
    and (Left.ParaSpacing = Right.ParaSpacing)
    and StrSameText(Left.Font.Name, Right.Font.Name)
    and SameValue(Left.FontSize, Right.FontSize)
    and (Left.FontStyles = Right.FontStyles)
    and (Left.Colour = Right.Colour);
end;

function TRTFStyle.IsNull: Boolean;
begin
  Result := Capabilities = [];
end;

class operator TRTFStyle.NotEqual(const Left, Right: TRTFStyle): Boolean;
begin
  Result := not(Left = Right);
end;

end.

