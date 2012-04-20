{
 * UActiveTextHTML.pas
 *
 * Static class that provides assistance when rendering active text as HTML.
 * The class renders the active text as HTML and provides CSS to style it.
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
 * The Original Code is UActiveTextHTML.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributors:
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UActiveTextHTML;


interface


uses
  // Delphi
  Graphics,
  // Project
  UActiveText, UBaseObjects, UCSSBuilder;


type

  {
  TActiveTextHTML:
    Static class that provides assistance when rendering active text as HTML.
    The class renders the active text as HTML and provides CSS to style it.
  }
  TActiveTextHTML = class(TNoConstructObject)
  public
    class function Render(const ActiveText: IActiveText): string;
     {Builds valid HTML containing information from snippet's Extra property.
     May contain links and some formatting.
        @return Required HTML.
      }
    class procedure Styles(const DefFont: TFont; const CSSBuilder: TCSSBuilder);
      {Sets the CSS styles required to render an HTML representation of REML.
        @param DefFont [in] Default font to use for styles.
        @param  CSSBuilder [in] Object that is used to create the CSS.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UColours, UCSSUtils, UFontHelper, UHTMLDetailUtils, UHTMLUtils, UIStringList;


{ TActiveTextHTML }

class function TActiveTextHTML.Render(const ActiveText: IActiveText): string;
 {Builds valid HTML containing information from snippet's Extra property. May
  contain links and some formatting.
    @return Required HTML.
  }
const
  // maps state of an active text element to equivalent HTML tag type
  cTagTypeMap: array[TActiveTextElemState] of THTMLTagType = (
    ttClose,  // fsClose: closing tag e.g. </tagname>
    ttOpen    // fsOpen: opening tag e.g. <tagname [params]>
  );
resourcestring
  sCreditsURLHint = 'Visit %s';       // hint used in <a> tag title attribute
var
  Elem: IActiveTextElem;              // each active text element
  TextElem: IActiveTextTextElem;      // a text active text element
  ActionElem: IActiveTextActionElem;  // an action active text element
  EncloseInDiv: Boolean;

  // ---------------------------------------------------------------------------
  function ClassAttr(const ClassName: string): IHTMLAttributes;
    {Creates an HTML attributes object containing a class attribute.
      @param ClassName [in] Name of class.
      @return Required HTML attributes object.
    }
  begin
    Result := THTMLAttributes.Create;
    Result.Add('class', ClassName);
  end;
  // ---------------------------------------------------------------------------

begin
  Result := '';
  // Process each active text element
  for Elem in ActiveText do
  begin
    if Supports(Elem, IActiveTextTextElem, TextElem) then
      // A text element: write it literally as safe HTML text
      Result := Result + MakeSafeHTMLText(TextElem.Text)
    else if Supports(Elem, IActiveTextActionElem, ActionElem) then
    begin
      // An action element: if supported output it in HTML
      case ActionElem.Kind of
        ekLink:
        begin
          // REML <a> => HTML <a class="external-link">
          //   REML href => HTML href
          if ActionElem.State = fsOpen then
            // opening tag: element's Param property is HTML href attribute
            Result := Result + AOpenTag(
              ActionElem.Param,
              '',
              '|' + Format(sCreditsURLHint, [ActionElem.Param]),
              TIStringList.Create('external-link')
            )
          else
            // an </a> tag
            Result := Result + MakeTag('a', ttClose);
        end;
        ekStrong:
          // REML <strong> => HTML <strong>
          Result := Result + MakeTag('strong', cTagTypeMap[ActionElem.State]);
        ekEm:
          // REML <em> => HTML <em>
          Result := Result + MakeTag('em', cTagTypeMap[ActionElem.State]);
        ekVar:
          // REML <var> => HTML <var class="extra">
          Result := Result + MakeTag(
            'var', cTagTypeMap[ActionElem.State], ClassAttr('extra')
          );
        ekPara:
          // REML <p> => HTML <p>
          Result := Result + MakeTag('p', cTagTypeMap[ActionElem.State]);
        ekWarning:
          // REML <warning> => HTML <span class="extra-warning">
          Result := Result + MakeTag(
            'span', cTagTypeMap[ActionElem.State], ClassAttr('extra-warning')
          );
        ekMono:
          // REML <mono> => HTML <span class="extra-mono">
          Result := Result + MakeTag(
            'span', cTagTypeMap[ActionElem.State], ClassAttr('extra-mono')
          );
        ekHeading:
          // REML <heading> => HTML <h2 class="extra">
          Result := Result + MakeTag(
            'h2', cTagTypeMap[ActionElem.State], ClassAttr('extra')
          );
        else
          {Unsupported action element type: do nothing};
      end;
    end;
  end;
  // Extra property may have "p" or "heading" tags, but may not have. So we
  // check and add enclosing "div" tags if necessary with required properties.
  // paragraph tags if
  EncloseInDiv := not ActiveText.IsEmpty and
    not ((ActiveText[0].Kind in [ekPara, ekHeading]));
  if EncloseInDiv then
    Result := MakeTag('div', ttOpen, ClassAttr('extra-wrapper')) +
      Result +
      MakeTag('div', ttClose);
end;

class procedure TActiveTextHTML.Styles(const DefFont: TFont;
  const CSSBuilder: TCSSBuilder);
  {Sets the CSS styles required to render an HTML representation of REML.
    @param DefFont [in] Default font to use for styles.
    @param  CSSBuilder [in] Object that is used to create the CSS.
  }
var
  CSSFont: TFont; // font used for CSS styles
begin
  // Add CSS relating to Extra REML code
  // -- heading tag
  with CSSBuilder.AddSelector('h2.extra') do
  begin
    AddProperty(CSSFontSizeProp(DefFont.Size + 1));
  end;
  // -- warning tag
  with CSSBuilder.AddSelector('span.extra-warning') do
  begin
    AddProperty(CSSFontWeightProp(cfwBold));
    AddProperty(CSSColorProp(clWarningText));
  end;
  // -- mono tag
  with CSSBuilder.AddSelector('span.extra-mono') do
  begin
    CSSFont := TFont.Create;
    try
      TFontHelper.SetDefaultMonoFont(CSSFont, True);
      AddProperty(CSSFontProps(CSSFont));
    finally
      FreeAndNil(CSSFont);
    end;
  end;
  // -- var tag
  with CSSBuilder.AddSelector('var.extra') do
  begin
    AddProperty(CSSColorProp(clVarText));
    AddProperty(CSSFontStyleProp(cfsItalic));
  end;
end;

end.
