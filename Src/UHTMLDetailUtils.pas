{
 * UHTMLDetailUtils.pas
 *
 * Utility functions that assist in generating HTML code used in creating detail
 * pane pages.
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
 * The Original Code is UHTMLDetailUtils.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UHTMLDetailUtils;


interface


uses
  // Project
  UHTMLUtils, UIStringList;


function TextLink(const URL, JSFn, Hint: string; Classes: IStringList;
  const Text: string): string;
  {Creates an <a>..</a> link surrounding some text.
    @param URL [in] URL accessed by link. Should be url encoded. Setting URL to
      '' creates a link to "javascript:void(0);".
    @param JSFn [in] JavaScript code called in tag's onclick event. Setting to
      JSFn to '' prevents onclick event being used. When set the onclick event
      returns false.
    @param Hint [in] Standard Delphi hint string, in form short-hint|long-hint.
      Link's title attribute is set to short-hint while if long-hint is provided
      the external object's ShowHint method is called with long-hint when mouse
      passes over the link.
    @param Classes [in] List of class names for link's class attribute. May be
      nil if no classes required.
    @param Text [in] Plain text to be enclosed between tags. Any usupported
      characters in the text are converted to HTML entities.
    @return Complete <a> tag.
  }

///  <summary>Creates HTML attrubites required to produce a roll-over hint with
///  given name.</summary>
function RollOverHintAttrs(const Hint: string): IHTMLAttributes;


implementation


uses
  // Delphi
  SysUtils, Controls,
  // Project
  UJavaScriptUtils;


///  <summary>Appends a false return statement to given JavaScript function
///  call and returns combined statement.</summary>
function JSFnWithFalseReturn(const JSFn: string): string;
begin
  Result := JSFn + '; return false;';
end;

function AOpenTag(const URL, JSFn, Hint: string;
  Classes: IStringList): string;
  {Creates an opening <a> tag with specified properties.
    @param URL [in] URL accessed by link. Should be url encoded. Setting URL to
      '' creates a link to "javascript:void(0);".
    @param JSFn [in] JavaScript code called in the tag's onclick event. Setting
      to '' prevents the onclick event being used. When set the onclick event
      returns false.
    @param Hint [in] Standard Delphi hint string, in form short-hint|long-hint.
      Link's title attribute is set to short-hint while if long-hint is provided
      the external object's ShowHint method is called with long-hint when mouse
      passes over the link.
    @param Classes [in] List of class names used in tag's class attribute. May
      be nil if no classes required.
    @return Required opening <a> tag.
  }
const
  cVoid = 'javascript:void(0);';  // used as nul href
var
  Attrs: IHTMLAttributes; // tag's attributes
  ShortHint: string;      // short hint from Hint param
  LongHint: string;       // long hint from Hint param
begin
  Attrs := THTMLAttributes.Create;
  // Decode hint string into short and long hints (separated by '|' )
  ShortHint := GetShortHint(Hint);
  LongHint := GetLongHint(Hint);
  // Determine href attribute from URL param
  if URL <> '' then
    Attrs.Add('href', URL)
  else
    Attrs.Add('href', cVoid);
  // Set title attribute if short hint provided
  if ShortHint <> '' then
    Attrs.Add('title', ShortHint);
  // Set class attribute if provided
  if Assigned(Classes) and (Classes.Count > 0) then
    Attrs.Add('class', Classes);
  // Set onclick event if JavaScript function provided
  if JSFn <> '' then
    Attrs.Add('onclick', JSFnWithFalseReturn(JSFn));
  // Set onmouseover and onmouseout to external ShowHint if long hint provided
  if LongHint <> '' then
    Attrs.Append(RollOverHintAttrs(LongHint));
  // Build and return the tag
  Result := MakeTag('a', ttOpen, Attrs);
end;

function RollOverHintAttrs(const Hint: string): IHTMLAttributes;
begin
  Result := THTMLAttributes.Create;
  if Hint = '' then
    Exit;
  Result.Add(
    'onmouseover', JSFnWithFalseReturn(JSLiteralFunc('showHint', [Hint]))
  );
  Result.Add(
    'onmouseout', JSFnWithFalseReturn(JSLiteralFunc('clearHint', []))
  );
end;

function ALink(const URL, JSFn, Hint: string; Classes: IStringList;
  const InnerHTML: string): string;
  {Creates a hyper-link compound tag surrounding some inner HTML.
    @param URL [in] URL accessed by link. Should be url encoded. Setting URL to
      '' creates a link to "javascript:void(0);".
    @param JSFn [in] JavaScript code called in the tag's onclick event. Setting
      to '' prevents the onclick event being used. When set the onclick event
      returns false.
    @param Hint [in] Standard Delphi hint string, in form short-hint|long-hint.
      Link's title attribute is set to short-hint while if long-hint is provided
      the external object's ShowHint method is called with long-hint when mouse
      passes over the link.
    @param Classes [in] List of class names used in tag's class attribute. May
      be nil if no classes required.
    @param InnerHTML [in] HTML to be included within link. Must be valid HTML
      code.
    @return Required HTML tag.
  }
begin
  Result := AOpenTag(URL, JSFn, Hint, Classes)
    + InnerHTML
    + MakeTag('a', ttClose)
end;

function TextLink(const URL, JSFn, Hint: string; Classes: IStringList;
  const Text: string): string;
  {Creates an <a>..</a> link surrounding some text.
    @param URL [in] URL accessed by link. Should be url encoded. Setting URL to
      '' creates a link to "javascript:void(0);".
    @param JSFn [in] JavaScript code called in the tag's onclick event. Setting
      to '' prevents the onclick event being used. When set the onclick event
      returns false.
    @param Hint [in] Standard Delphi hint string, in form short-hint|long-hint.
      Link's title attribute is set to short-hint while if long-hint is provided
      the external object's ShowHint method is called with long-hint when mouse
      passes over the link.
    @param Classes [in] List of class names used in tag's class attribute. May
      be nil if no classes required.
    @param Text [in] Plain text to be enclosed between tags. Any usupported
      characters in the text are converted to HTML entities.
    @return Complete <a> tag.
  }
begin
  Result := ALink(URL, JSFn, Hint, Classes, MakeSafeHTMLText(Text));
end;

end.

