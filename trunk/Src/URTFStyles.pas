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


implementation


uses
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

end.
