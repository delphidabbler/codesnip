{
 * UCSSUtils.pas
 *
 * Utility functions to assist in generating CSS code.
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
 * The Original Code is UCSSUtils.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UCSSUtils;


interface


uses
  // Delphi
  Graphics;


type

  {
  TCSSFontWeight:
    Possible values of CSS font-weight property.
  }
  TCSSFontWeight = (
    cfwNormal,                              // absolute: normal (same as cfw400)
    cfwBold,                                // absolute: bold (same as cfw700)
    cfwBolder,                              // relative: heavier than current
    cfwLighter,                             // relative: lighter than current
    cfw100, cfw200, cfw300, cfw400,         // ordered sequence of weights
    cfw500, cfw600, cfw700, cfw800, cfw900  // .. each darker than next
  );

  {
  TCSSFontStyle:
    Possible values of CSS font-style property.
  }
  TCSSFontStyle = (
    cfsNormal,        // normal, upright, font
    cfsItalic,        // uses font labelled "italic" or failing that "oblique"
    cfsOblique        // uses font labelled "oblique"
  );

  {
  TCSSTextDecoration:
    Possible values of CSS text-decoration property.
  }
  TCSSTextDecoration = (
    ctdNone,          // no decoration
    ctdUnderline,     // text is underlines
    ctdOverline,      // text has overline
    ctdLineThrough,   // text is stirck through
    ctdBlink          // text is blinking
  );

  {
  TCSSTextDecorations:
    Combination of text decoration values. All except ctdNone can be combined.
    ctdNone must exist on its own.
  }
  TCSSTextDecorations = set of TCSSTextDecoration;

  {
  TCSSFontGeneric:
    Enumeration of generic font families recognised by CSS.
  }
  TCSSFontGeneric = (
    cfgDontCare,      // no generic family specified
    cfgSerif,         // serif font (e.g. Times New Roman)
    cfgSansSerif,     // sans-serif font (e.g. Arial)
    cfgCursive,       // scripting font
    cfgFantasy,       // decorative font
    cfgMonoSpace      // fixed pitch font
  );

  {
  TCSSFontVariant:
    Possible values of CSS font-variant property.
  }
  TCSSFontVariant = (
    cfvNormal,        // normal text
    cfvSmallCaps      // small capitals
  );

  {
  TCSSBorderStyle:
    Possible border styles used in the CSS border property and its derivatives.
  }
  TCSSBorderStyle = (
    cbsNone,          // no border
    cbsDotted,        // dotted line border
    cbsDashed,        // dashed line border
    cbsSolid,         // solid line border
    cbsDouble,        // double line border
    cbsGroove,        // 3D groove: colours based on color property
    cbsRidge,         // 3D ridge: colours based on color property
    cbsInset,         // 3D inset: colours based on color property
    cbsOutset         // 3D outset: colours based on color property
  );

  {
  TCSSFloatValue:
    Possible values of CSS float property.
  }
  TCSSFloatValue = (
    cfvLeft,          // element floated left: text wraps on right
    cfvRight,         // element floated right: text wraps on left
    cfvNone           // element displayed in normal position: no text wraps
  );

  {
  TCSSTextAlign:
    Possible values of CSS text-align property.
  }
  TCSSTextAlign = (
    ctaLeft,          // text is left aligned: ragged right margin
    ctaRight,         // text is right aligned: ragged left margin
    ctaCenter,        // text is centred
    ctaJustify        // text is justified flush with right and left margins
  );

  {
  TCSSVerticalAlign:
    Possible values of CSS vertical-align property.
  }
  TCSSVerticalAlign = (
    cvaBaseline,      // align element baseline with parent's baseline
    cvaSub,           // subscript the element
    cvaSuper,         // superscript the element
    cvaTop,           // align top of element with tallest element on line
    cvaTextTop,       // align top of element with top of parent's font
    cvaMiddle,        // align vertical middle with baseline + 1/2 parent height
    cvaBottom,        // align bottom of element with lowest element on line
    cvaTextBottom     // align bottom of element with bottom of parent's font
  );

  {
  TCSSSide:
    Enumeration of "sides" that apply to various CSS properties. Used to specify
    required variant of a property (e.g padding-top).
  }
  TCSSSide = (
    cssAll,           // refers to all sides of an element
    cssTop,           // top of element
    cssLeft,          // left of element
    cssBottom,        // bottom of element
    cssRight          // right of element
  );

  {
  TCSSDisplayStyle:
    Enumeration of various display styles used in CSS display property.
  }
  TCSSDisplayStyle = (
    cdsNone,          // element not displayed
    cdsBlock,         // element displayed as a block
    cdsInline         // element displayed inline
  );

  {
  TCSSLengthType:
    Enumeration of different types of length property values.
  }
  TCSSLengthType = (
    cltAuto,          // "auto"
    cltPixels,        // pixels
    cltEm,            // "em" values
    cltPercent        // percentage values
  );

  {
  TCSSOverflowValue:
    Enumeration of various overflow property values.
  }
  TCSSOverflowValue = (
    covVisible,     // overflow is not clipped and overflows
    covHidden,      // overflow is clipped, rest of the content invisible
    covScroll,      // overflow is clipped, scroll-bar is added
    covAuto,        // if overflow is clipped a scroll-bar is added
    covInherit      // value inherited from the parent element
  );


function CSSBackgroundColorProp(const Color: TColor): string;
  {Creates CSS "background-color" property.
    @param Color [in] Desired background colour.
    @return Required property.
  }

function CSSBorderProp(const Side: TCSSSide; const WidthPx: Cardinal;
  const Style: TCSSBorderStyle = cbsNone; const Color: TColor = clNone): string;
  {Creates CSS "border" or "border-xxx" property where "xxx" is a side.
    @param Side [in] Specifies side(s) of element where border to be drawn.
    @param WidthPx [in] Width of border in pixels. Value of 0 hides border.
    @param Style [in] Border style. Value of cbsNone hides border.
    @param Color [in] Border colour. Value of clNone hides border.
    @return Required property.
  }

function CSSColorProp(const Color: TColor): string;
  {Creates a CSS "color" property.
    @param Color [in] Desired colour.
    @return Required property.
  }

function CSSFloatProp(const Value: TCSSFloatValue): string;
  {Creates a CSS "float" property.
    @param Value [in] Float aligment.
    @return Required property.
  }

function CSSFontProps(const Font: TFont; const Delim: string = ' '): string;
  {Builds string of properties describing a font.
    @param Font [in] Font for which properties are required.
    @param Delim [in] String used to delimit properties.
    @return Required properties.
  }

function CSSFontFamilyProp(const FontName: string;
  const Generic: TCSSFontGeneric = cfgDontCare): string; overload;
  {Creates a CSS "font-family" property for font name.
    @param FontName [in] Name of font.
    @param Generic [in] Generic font family to use if font not available.
    @return Required property.
  }

function CSSFontFamilyProp(const FontNames: array of string;
  const Generic: TCSSFontGeneric = cfgDontCare): string; overload;
  {Creates a CSS "font-family" property for list of font names.
    @param FontNames [in] Array of font names in order of preference.
    @param Generic [in] Generic font family to use if font not available.
    @return Required property.
  }

function CSSFontSizeProp(const Pt: Cardinal): string;
  {Creates a CSS "font-size" property for font sized in points.
    @param Pt [in] Point size.
    @return Required property.
  }

function CSSFontStyleProp(const FS: TCSSFontStyle): string; overload;
  {Creates a CSS "font-style" property from CSS font style.
    @param FS [in] CSS font style id.
    @return Required property.
  }

function CSSFontStyleProp(const FS: TFontStyles): string; overload;
  {Creates a CSS "font-style" property from set of styles.
    @param FS [in] Set of font styles.
    @return Required property.
  }

function CSSFontVariantProp(const Variant: TCSSFontVariant): string;
  {Creates a CSS "font-variant" property.
    @param Variant [in] Required variant.
    @return Required property.
  }

function CSSFontWeightProp(const FW: TCSSFontWeight): string; overload;
  {Creates CSS "font-weight" property from CSS weight id.
    @param FW [in] CSS font weight id.
    @return Required property.
  }

function CSSFontWeightProp(const FS: TFontStyles): string; overload;
  {Creates a CSS "font-weight" property from set of styles.
    @param FS [in] Set of font styles.
    @return Required property.
  }

function CSSHeightProp(const HeightPx: Cardinal): string;
  {Creates a CSS "height" property in pixels.
    @param HeightPx [in] Height in pixels.
    @return Required property.
  }

function CSSMarginProp(const Margin: Cardinal): string; overload;
  {Creates CSS "margin" property with same width on all edges.
    @param Margin [in] Margin width in pixels.
    @return Required property.
  }

function CSSMarginProp(const TopBottom, RightLeft: Cardinal): string; overload;
  {Creates CSS "margin" property with same same top/bottom and right/left
  dimensions.
    @param TopBottom [in] Top and bottom margin in pixels.
    @param RightLeft [in] Right and left margin in pixels.
    @return Required property.
  }

function CSSMarginProp(const Top, Right, Bottom, Left: Cardinal): string;
  overload;
  {Creates CSS "margin" property with potentially different margin widths on
  each side.
    @param Top [in] Top margin in pixels.
    @param Right [in] Right margin in pixels.
    @param Bottom [in] Bottom margin in pixels.
    @param Left [in] Left margin in pixels.
    @return Required property.
  }

function CSSMarginProp(const Side: TCSSSide;
  const Margin: Integer): string; overload;
  {Creates CSS "margin" or "margin-xxx" property where "xxx" is a side.
    @param Side [in] Specifies side(s) of element whose margin to be set.
    @param Margin [in] Width of margin in pixels.
    @return Required property.
  }

function CSSMaxHeightProp(const HeightPx: Cardinal): string;
  {Creates a CSS "max-height" property in pixels.
    @param HeightPx [in] Height in pixels.
    @return Required property.
  }

function CSSOverflowProp(const Value: TCSSOverflowValue): string;
  {Creates CSS "overflow" property.
    @param Value [in] Property value identifier.
  }

function CSSPaddingProp(const Padding: Cardinal): string; overload;
  {Creates CSS "padding" property with same width on all edges.
    @param Padding [in] Padding width in pixels.
    @return Required property.
  }

function CSSPaddingProp(const TopBottom, RightLeft: Cardinal): string; overload;
  {Creates CSS "padding" property with same same top/bottom and right/left
  dimensions.
    @param TopBottom [in] Top and bottom padding in pixels.
    @param RightLeft [in] Right and left padding in pixels.
    @return Required property.
  }

function CSSPaddingProp(const Top, Right, Bottom, Left: Cardinal): string;
  overload;
  {Creates CSS "padding" property with potentially different margin widths on
  each side.
    @param Top [in] Top padding in pixels.
    @param Right [in] Right padding in pixels.
    @param Bottom [in] Bottom padding in pixels.
    @param Left [in] Left padding in pixels.
    @return Required property.
  }

function CSSPaddingProp(const Side: TCSSSide;
  const Padding: Integer): string; overload;
  {Creates CSS "padding" or "padding-xxx" property where "xxx" is a side.
    @param Side [in] Specifies side(s) of element whose padding to be set.
    @param Padding [in] Width of padding in pixels.
    @return Required property.
  }

function CSSTextAlignProp(const TA: TCSSTextAlign): string;
  {Creates CSS "text-align" property.
    @param TA [in] Specifies text alignment.
    @return Required property.
  }

function CSSTextDecorationProp(Decorations: TCSSTextDecorations): string;
  overload;
  {Creates CSS "text-decoration" property from a set of decorations.
    @param Decorations [in] Set of decorations. Can either be [ctdNone], empty
      set or any combination of other ctd values. Empty set taken as [ctdNone].
    @return Required property.
  }

function CSSTextDecorationProp(const FS: TFontStyles): string; overload;
  {Creates CSS "text-decoration" property from font styles.
    @param FS [in] Set of font styles.
    @return Required property.
  }

function CSSVerticalAlignProp(const VA: TCSSVerticalAlign): string; overload;
  {Creates CSS "vertical-align" property per CSS alignment style.
    @param FS [in] CSS alignement id.
    @return Required property.
  }

function CSSVerticalAlignProp(const Percent: Integer): string; overload;
  {Creates CSS "vertical-align" property using a relative percentage.
    @param Percent [in] Relative alignment in percent.
    @return Required property.
  }

function CSSWidthProp(const WidthPx: Cardinal): string; overload;
  {Creates a CSS "width" property in pixels.
    @param WidthPx [in] Width in pixels.
    @return Required property.
  }

function CSSWidthProp(const LengthType: TCSSLengthType;
  const Width: Cardinal): string; overload;
  {Creates a CSS "width" property in a specified length type.
    @param LengthType [in] Required length type. If "auto" Width is ignored.
    @param Width [in] Width in specified units.
    @return Required property.
  }

function CSSDisplayProp(const Style: TCSSDisplayStyle): string;
  {Creates a CSS "display" property.
    @param Style [in] Required display style.
    @return Required property.
  }

function CSSBlockDisplayProp(const Show: Boolean): string;
  {Creates a CSS "display" style that is either hidden or displays as "block".
    @param Show [in] True if block is to be displayed, false if hidden.
    @return Required property.
  }



implementation


uses
  // Delphi
  SysUtils,
  // Project
  UHTMLUtils, UIStringList, UUtils;


{ Forward declarations }
function CSSMarginProp(const Margin: array of Cardinal): string; overload;
  forward;
function CSSPaddingProp(const Padding: array of Cardinal): string; overload;
  forward;
function LengthList(const List: array of Cardinal;
  const LT: TCSSLengthType = cltPixels): string; forward;
function LengthTypeStr(const LT: TCSSLengthType): string; forward;

function CSSBackgroundColorProp(const Color: TColor): string;
  {Creates CSS "background-color" property.
    @param Color [in] Desired background colour.
    @return Required property.
  }
begin
  Result := Format('background-color: %s;', [ColorToHTML(Color)]);
end;

function CSSBorderProp(const Side: TCSSSide; const WidthPx: Cardinal;
  const Style: TCSSBorderStyle = cbsNone; const Color: TColor = clNone): string;
  {Creates CSS "border" or "border-xxx" property where "xxx" is a side.
    @param Side [in] Specifies side(s) of element where border to be drawn.
    @param WidthPx [in] Width of border in pixels. Value of 0 hides border.
    @param Style [in] Border style. Value of cbsNone hides border.
    @param Color [in] Border colour. Value of clNone hides border.
    @return Required property.
  }
const
  // Map of element sides to associated border properties
  cBorderSides: array[TCSSSide] of string = (
    'border', 'border-top', 'border-left', 'border-bottom', 'border-right'
  );
  // Map of border styles to property values
  cBorderStyles: array[TCSSBorderStyle] of string = (
    'none', 'dotted', 'dashed', 'solid', 'double',
    'groove', 'ridge', 'inset', 'outset'
  );
begin
  if (WidthPx > 0) and (Style <> cbsNone) and (Color <> clNone) then
    // Displaying border
    Result := Format(
      '%s: %s %s %s;',
      [cBorderSides[Side], ColorToHTML(Color), cBorderStyles[Style],
      LengthList([WidthPx])]
    )
  else
    // Hiding border
    Result := Format('%s: %s;', [cBorderSides[Side], LengthList([0])]);
end;

function CSSColorProp(const Color: TColor): string;
  {Creates a CSS "color" property.
    @param Color [in] Desired colour.
    @return Required property.
  }
begin
  Result := Format('color: %s;', [ColorToHTML(Color)]);
end;

function CSSFloatProp(const Value: TCSSFloatValue): string;
  {Creates a CSS "float" property.
    @param Value [in] Float aligment.
    @return Required property.
  }
const
  // Map of float values to property values
  cFloatValues: array[TCSSFloatValue] of string = ('left', 'right', 'none');
begin
  Result := Format('float: %s;', [cFloatValues[Value]]);
end;

function CSSFontProps(const Font: TFont; const Delim: string = ' '): string;
  {Builds string of properties describing a font.
    @param Font [in] Font for which properties are required.
    @param Delim [in] String used to delimit properties.
    @return Required properties.
  }
var
  PropList: IStringList;  // list of font properties
begin
  PropList := TIStringList.Create(
    [
      CSSFontFamilyProp(Font.Name),
      CSSFontSizeProp(Font.Size),
      CSSFontWeightProp(Font.Style),
      CSSFontStyleProp(Font.Style),
      CSSTextDecorationProp(Font.Style),
      CSSColorProp(Font.Color)
    ]
  );
  Result := PropList.GetText(Delim, False);
end;

function CSSFontFamilyProp(const FontName: string;
  const Generic: TCSSFontGeneric): string;
  {Creates a CSS "font-family" property for font name.
    @param FontName [in] Name of font.
    @param Generic [in] Generic font family to use if font not available.
    @return Required property.
  }
begin
  Result := CSSFontFamilyProp([FontName], Generic);
end;

function CSSFontFamilyProp(const FontNames: array of string;
  const Generic: TCSSFontGeneric): string;
  {Creates a CSS "font-family" property for list of font names.
    @param FontNames [in] Array of font names in order of preference.
    @param Generic [in] Generic font family to use if font not available.
    @return Required property.
  }
const
  // Map of generic font families to font names
  cGenerics: array[TCSSFontGeneric] of string = (
    '', 'serif', 'sans-serif', 'cursive', 'fantasy', 'monospace'
  );
var
  NameIdx: Integer;     // loops through font names array
  Names: IStringList;   // used to build list of font names
begin
  Names := TIStringList.Create;
  // Add each font in array to list
  for NameIdx := Low(FontNames) to High(FontNames) do
    // quote any font name containing spaces
    Names.Add(QuoteSpacedString(FontNames[NameIdx]));
  // Add generic font family if required
  if Generic <> cfgDontCare then
    Names.Add(cGenerics[Generic]);
  // Generate property
  Result := Format('font-family: %s;', [Names.GetText(', ', False)]);
end;

function CSSFontSizeProp(const Pt: Cardinal): string;
  {Creates a CSS "font-size" property for font sized in points.
    @param Pt [in] Point size.
    @return Required property.
  }
begin
  Result := Format('font-size: %dpt;', [Pt]);
end;

function CSSFontStyleProp(const FS: TCSSFontStyle): string;
  {Creates a CSS "font-style" property from CSS font style.
    @param FS [in] CSS font style id.
    @return Required property.
  }
const
  // Maps font style to property value
  cFontStyles: array[TCSSFontStyle] of string = ('normal', 'italic', 'oblique');
begin
  Result := Format('font-style: %s;', [cFontStyles[FS]]);
end;

function CSSFontStyleProp(const FS: TFontStyles): string;
  {Creates a CSS "font-style" property from set of styles.
    @param FS [in] Set of font styles.
    @return Required property.
  }
begin
  if fsItalic in FS then
    Result := CSSFontStyleProp(cfsItalic)
  else
    Result := CSSFontStyleProp(cfsNormal);
end;

function CSSFontVariantProp(const Variant: TCSSFontVariant): string;
  {Creates a CSS "font-variant" property.
    @param Variant [in] Required variant.
    @return Required property.
  }
const
  // Map of font variants to property values
  cFontVariants: array[TCSSFontVariant] of string = ('normal', 'small-caps');
begin
  Result := Format('font-variant: %s;', [cFontVariants[Variant]]);
end;

function CSSFontWeightProp(const FW: TCSSFontWeight): string;
  {Creates CSS "font-weight" property from CSS weight id.
    @param FW [in] CSS font weight id.
    @return Required property.
  }
const
  // Map of font weights to property values
  cFontWeights: array[TCSSFontWeight] of string = (
    'normal', 'bold', 'bolder', 'lighter',
    '100', '200', '300', '400', '500', '600', '700', '800', '900'
  );
begin
  Result := Format('font-weight: %s;', [cFontWeights[FW]]);
end;

function CSSFontWeightProp(const FS: TFontStyles): string;
  {Creates a CSS "font-weight" property from set of styles.
    @param FS [in] Set of font styles.
    @return Required property.
  }
begin
  if fsBold in FS then
    Result := CSSFontWeightProp(cfwBold)
  else
    Result := CSSFontWeightProp(cfwNormal);
end;

function CSSHeightProp(const HeightPx: Cardinal): string;
  {Creates a CSS "height" property in pixels.
    @param HeightPx [in] Height in pixels.
    @return Required property.
  }
begin
  Result := Format('height: %s;', [LengthList([HeightPx])]);
end;

function CSSMarginProp(const Margin: array of Cardinal): string;
  {Creates CSS "margin" property in pixels.
    @param Margin [in] Array of margin widths (either 1, 2 or 4 values).
    @return Required property.
  }
begin
  Assert(Length(Margin) in [1,2,4], 'CSSMarginProp: Invalid margin parameters');
  Result := 'margin: ' + LengthList(Margin) + ';';
end;

function CSSMarginProp(const Margin: Cardinal): string;
  {Creates CSS "margin" property with same width on all edges.
    @param Margin [in] Margin width in pixels.
    @return Required property.
  }
begin
  Result := CSSMarginProp([Margin]);
end;

function CSSMarginProp(const TopBottom, RightLeft: Cardinal): string;
  {Creates CSS "margin" property with same same top/bottom and right/left
  dimensions.
    @param TopBottom [in] Top and bottom margin in pixels.
    @param RightLeft [in] Right and left margin in pixels.
    @return Required property.
  }
begin
  Result := CSSMarginProp([TopBottom, RightLeft]);
end;

function CSSMarginProp(const Top, Right, Bottom, Left: Cardinal): string;
  {Creates CSS "margin" property with potentially different margin widths on
  each side.
    @param Top [in] Top margin in pixels.
    @param Right [in] Right margin in pixels.
    @param Bottom [in] Bottom margin in pixels.
    @param Left [in] Left margin in pixels.
    @return Required property.
  }
begin
  Result := CSSMarginProp([Top, Right, Bottom, Left]);
end;

function CSSMarginProp(const Side: TCSSSide; const Margin: Integer): string;
  overload;
  {Creates CSS "margin" or "margin-xxx" property where "xxx" is a side.
    @param Side [in] Specifies side(s) of element whose margin to be set.
    @param Margin [in] Width of margin in pixels.
    @return Required property.
  }
const
  // Map of element sides to associated margin properties
  cMarginSides: array[TCSSSide] of string = (
    'margin', 'margin-top', 'margin-left', 'margin-bottom', 'margin-right'
  );
begin
  Result := Format('%s: %s;', [cMarginSides[Side], LengthList([Margin])]);
end;

function CSSMaxHeightProp(const HeightPx: Cardinal): string;
  {Creates a CSS "max-height" property in pixels.
    @param HeightPx [in] Height in pixels.
    @return Required property.
  }
begin
  Result := Format('max-height: %s;', [LengthList([HeightPx])]);
end;

function CSSOverflowProp(const Value: TCSSOverflowValue): string;
  {Creates CSS "overflow" property.
    @param Value [in] Property value identifier.
  }
const
  cValues: array[TCSSOverflowValue] of string = (
    'visible', 'hidden', 'scroll', 'auto', 'inherit'
  );
begin
  Result := Format('overflow: %s;', [cValues[Value]]);
end;

function CSSPaddingProp(const Padding: array of Cardinal): string;
  {Creates CSS "padding" property in pixels.
    @param Padding [in] Array of padding widths (either 1, 2 or 4 values).
    @return Required property.
  }
begin
  Assert(Length(Padding) in [1,2,4],
    'CSSPaddingProp: Invalid padding parameters');
  Result := 'padding: ' + LengthList(Padding) + ';';
end;

function CSSPaddingProp(const Padding: Cardinal): string;
  {Creates CSS "padding" property with same width on all edges.
    @param Padding [in] Padding width in pixels.
    @return Required property.
  }
begin
  Result := CSSPaddingProp([Padding]);
end;

function CSSPaddingProp(const TopBottom, RightLeft: Cardinal): string;
  {Creates CSS "padding" property with same same top/bottom and right/left
  dimensions.
    @param TopBottom [in] Top and bottom padding in pixels.
    @param RightLeft [in] Right and left padding in pixels.
    @return Required property.
  }
begin
  Result := CSSPaddingProp([TopBottom, RightLeft]);
end;

function CSSPaddingProp(const Top, Right, Bottom, Left: Cardinal): string;
  {Creates CSS "padding" property with potentially different margin widths on
  each side.
    @param Top [in] Top padding in pixels.
    @param Right [in] Right padding in pixels.
    @param Bottom [in] Bottom padding in pixels.
    @param Left [in] Left padding in pixels.
    @return Required property.
  }
begin
  Result := CSSPaddingProp([Top, Right, Bottom, Left]);
end;

function CSSPaddingProp(const Side: TCSSSide; const Padding: Integer): string;
  {Creates CSS "padding" or "padding-xxx" property where "xxx" is a side.
    @param Side [in] Specifies side(s) of element whose padding to be set.
    @param Padding [in] Width of padding in pixels.
    @return Required property.
  }
const
  // Map of element sides to associated padding properties
  cPaddingSides: array[TCSSSide] of string = (
    'padding', 'padding-top', 'padding-left', 'padding-bottom', 'padding-right'
  );
begin
  Result := Format('%s: %s;', [cPaddingSides[Side], LengthList([Padding])]);
end;

function CSSTextAlignProp(const TA: TCSSTextAlign): string;
  {Creates CSS "text-align" property.
    @param TA [in] Specifies text alignment.
    @return Required property.
  }
const
  // Map of text alignment to associated property values
  cTextAligns: array[TCSSTextAlign] of string = (
    'left', 'right', 'center', 'justify'
  );
begin
  Result := Format('text-align: %s;', [cTextAligns[TA]]);
end;

function CSSTextDecorationProp(Decorations: TCSSTextDecorations): string;
  {Creates CSS "text-decoration" property from a set of decorations.
    @param Decorations [in] Set of decorations. Can either be [ctdNone], empty
      set or any combination of other ctd values. Empty set taken as [ctdNone].
    @return Required property.
  }
const
  // Map of text decoration ids to associated property values
  cTextDecorations: array[TCSSTextDecoration] of string =
    ('none', 'underline', 'overline', 'line-through', 'blink');
var
  D: TCSSTextDecoration;  // loops thru all decorations
  List: IStringList;      // list of decoration values
begin
  Assert((Decorations = []) or (Decorations = [ctdNone])
    or (Decorations * [ctdNone] = []),
    'CSSTextDecorationProp: Invalid combination of values'
  );
  if Decorations = [] then
    Decorations := [ctdNone];
  List := TIStringList.Create;
  for D := Low(TCSSTextDecoration) to High(TCSSTextDecoration) do
    if D in Decorations then
      List.Add(cTextDecorations[D]);
  Result := Format('text-decoration: %s;', [List.GetText(' ', False)]);
end;

function CSSTextDecorationProp(const FS: TFontStyles): string;
  {Creates CSS "text-decoration" property from font styles.
    @param FS [in] Set of font styles.
    @return Required property.
  }
var
  Decorations: TCSSTextDecorations; // required text decorations
begin
  Decorations := [];
  if fsUnderline in FS then
    Include(Decorations, ctdUnderline);
  if fsStrikeOut in FS then
    Include(Decorations, ctdLineThrough);
  Result := CSSTextDecorationProp(Decorations);
end;

function CSSVerticalAlignProp(const VA: TCSSVerticalAlign): string;
  {Creates CSS "vertical-align" property per CSS alignment style.
    @param FS [in] CSS alignement id.
    @return Required property.
  }
const
  // Map of vertical alignement ids to associated property values
  cVerticalAligns: array[TCSSVerticalAlign] of string = (
    'baseline', 'sub', 'super', 'top', 'text-top', 'middle', 'bottom',
    'text-bottom'
  );
begin
  Result := Format('vertical-align: %s;', [cVerticalAligns[VA]]);
end;

function CSSVerticalAlignProp(const Percent: Integer): string;
  {Creates CSS "vertical-align" property using a relative percentage.
    @param Percent [in] Relative alignment in percent.
    @return Required property.
  }
begin
  Result := Format('vertical-align: %d%;', [Percent]);
end;

function LengthList(const List: array of Cardinal;
  const LT: TCSSLengthType = cltPixels): string;
  {Builds a space separated list of length values using pixel measurements.
    @param List [in] List of pixel values to include in list.
    @param LT [in] Length type: specifies units to use for lengths.
    @return Required space separated list.
  }
var
  Idx: Integer;     // loops thru list of values
  ALength: Integer; // a length from list
begin
  Assert((LT <> cltAuto) or (Length(List) = 1),
    'LengthList: List size may only be 1 when length type is cltAuto');
  if LT = cltAuto then
    Result := LengthTypeStr(LT)
  else
  begin
    Result := '';
    for Idx := Low(List) to High(List) do
    begin
      ALength := List[Idx];
      if Result <> '' then
        Result := Result + ' ';
      Result := Result + IntToStr(ALength);
      if ALength <> 0 then
        Result := Result + LengthTypeStr(LT);  // only add unit if length not 0
    end;
  end;
end;

function LengthTypeStr(const LT: TCSSLengthType): string;
  {Gets the appropriate length specifier for a length type.
    @param LT [in] Length type.
    @return Required specifier.
  }
const
  cUnits: array[TCSSLengthType] of string = (
    'auto', 'px', 'em', '%'
  );
begin
  Result := cUnits[LT];
end;

function CSSWidthProp(const WidthPx: Cardinal): string;
  {Creates a CSS "width" property in pixels.
    @param WidthPx [in] Width in pixels.
    @return Required property.
  }
begin
  Result := CSSWidthProp(cltPixels, WidthPx);
end;

function CSSWidthProp(const LengthType: TCSSLengthType;
  const Width: Cardinal): string;
  {Creates a CSS "width" property in a specified length type.
    @param LengthType [in] Required length type. If "auto" Width is ignored.
    @param Width [in] Width in specified units.
    @return Required property.
  }
begin
  Result := Format('width: %s;', [LengthList([Width], LengthType)]);
end;

function CSSDisplayProp(const Style: TCSSDisplayStyle): string;
  {Creates a CSS "display" property.
    @param Style [in] Required display style.
    @return Required property.
  }
const
  // Map of display ids to associated property values
  cDisplayStyles: array[TCSSDisplayStyle] of string = (
    'none', 'block', 'inline'
  );
begin
  Result := Format('display: %s;', [cDisplayStyles[Style]]);
end;

function CSSBlockDisplayProp(const Show: Boolean): string;
  {Creates a CSS "display" style that is either hidden or displays as "block".
    @param Show [in] True if block is to be displayed, false if hidden.
    @return Required property.
  }
const
  // Map of flag onto required display style
  cDisplayStyles: array[Boolean] of TCSSDisplayStyle = (cdsNone, cdsBlock);
begin
  Result := CSSDisplayProp(cDisplayStyles[Show]);
end;

end.

