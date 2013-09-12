{
 * Hiliter.UCSS.pas
 *
 * Defines a class that generates CSS code to enable syntax highlighted source
 * to be displayed in HTML. CSS code uses a highlighter's attributes. Access to
 * CSS class names is also provided.
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
 * The Original Code is Hiliter.UCSS.pas, formerly UHiliterCSS.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit Hiliter.UCSS;


interface


uses
  // Project
  Hiliter.UGlobals, UCSSBuilder;


type

  {
  THiliterCSS:
    Class generates CSS code to enable syntax highlighted source to be displayed
    in HTML. CSS code uses a highlighter's attributes. Access to CSS class names
    is also provided.
  }
  THiliterCSS = class(TObject)
  private
    fHiliteAttrs: IHiliteAttrs;
      {Highlighter for which CSS is to be generated}
    procedure BuildElemCSS(const Elem: THiliteElement;
      const CSSBuilder: TCSSBuilder);
      {Builds CSS class for a highlighter element.
        @param Elem [in] Highlighter element for which CSS is required.
        @param CSSBuilder [in] Object used to build and store the CSS.
      }
  public
    constructor Create(const HiliteAttrs: IHiliteAttrs);
      {Class constructor. Sets up object ready to generate code for a syntax
      highlighter.
        @param HiliterAttrs [in] Attributes to be used in highlighter.
      }
    class function GetMainCSSClassName: string;
      {Gets name of main CSS class used for all highlighted code.
        @return Required class name.
      }
    class function GetElemCSSClassName(const Elem: THiliteElement): string;
      {Gets name of CSS class associated with a highlighter element.
        @param Elem [in] Identifies element for which class name required.
        @return Required class name.
      }
    procedure BuildCSS(const CSSBuilder: TCSSBuilder);
      {Builds all CSS classes for a highlighter.
        @param CSSBuilder [in] Object used to build and store required CSS.
      }
  end;


implementation


uses
  // Delphi
  Graphics,
  // Project
  UCSSUtils;


{ THiliterCSS }

procedure THiliterCSS.BuildCSS(const CSSBuilder: TCSSBuilder);
  {Builds all CSS classes for a highlighter.
    @param CSSBuilder [in] Object used to build and store required CSS.
  }
var
  Elem: THiliteElement; // loops thru highlighter elements
begin
  // Add font definition in main class
  with CSSBuilder.AddSelector('.' + GetMainCSSClassName) do
  begin
    AddProperty(CSSFontFamilyProp(fHiliteAttrs.FontName, cfgMonoSpace));
    AddProperty(CSSFontSizeProp(fHiliteAttrs.FontSize));
  end;
  // Add font style and colour definitions for each element
  for Elem := Low(THiliteElement) to High(THiliteElement) do
    BuildElemCSS(Elem, CSSBuilder);
end;

procedure THiliterCSS.BuildElemCSS(const Elem: THiliteElement;
   const CSSBuilder: TCSSBuilder);
  {Builds CSS class for a highlighter element.
    @param Elem [in] Highlighter element for which CSS is required.
    @param CSSBuilder [in] Object used to build and output the CSS.
  }
var
  ElemAttr: IHiliteElemAttrs; // reference to highlight element
begin
  ElemAttr := fHiliteAttrs[Elem];
  // We only create CSS class if element attributes are non-nul
  if not ElemAttr.IsNul then
  begin
    with CSSBuilder.AddSelector('.' + GetElemCSSClassName(Elem)) do
    begin
      if ElemAttr.ForeColor <> clNone then
        AddProperty(CSSColorProp(ElemAttr.ForeColor));
      AddProperty(CSSFontWeightProp(ElemAttr.FontStyle));
      AddProperty(CSSFontStyleProp(ElemAttr.FontStyle));
      AddProperty(CSSTextDecorationProp(ElemAttr.FontStyle));
    end;
  end;
end;

constructor THiliterCSS.Create(const HiliteAttrs: IHiliteAttrs);
  {Class constructor. Sets up object ready to generate code for a syntax
  highlighter.
    @param HiliterAttrs [in] Attributes to be used in highlighter.
  }
begin
  inherited Create;
  Assert(Assigned(HiliteAttrs), ClassName + '.Create: HiliteAttrs is nil');
  fHiliteAttrs := HiliteAttrs;
end;

class function THiliterCSS.GetElemCSSClassName(
  const Elem: THiliteElement): string;
  {Gets name of CSS class associated with a highlighter element.
    @param Elem [in] Identifies element for which class name required.
    @return Required class name.
  }
const
  // Map of highlight element kinds onto CSS class used to format it
  cClassMap: array[THiliteElement] of string = (
    'pas-whitespace', // heWhitespace
    'pas-comment',    // heComment
    'pas-reserved',   // heReserved
    'pas-identifier', // heIdentifier
    'pas-symbol',     // heSymbol
    'pas-string',     // heString
    'pas-number',     // heNumber
    'pas-float',      // heFloat
    'pas-hex',        // heHex
    'pas-preproc',    // hePreProcessor
    'pas-asm',        // heAssembler
    'pas-error'       // heError
  );
begin
  Result := cClassMap[Elem];
end;

class function THiliterCSS.GetMainCSSClassName: string;
  {Gets name of main CSS class used for all highlighted code.
    @return Required class name.
  }
begin
  Result := 'pas-source';
end;

end.

