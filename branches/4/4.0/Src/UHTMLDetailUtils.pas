{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Utility functions that assist in generating HTML code used in creating detail
 * pane pages.
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
begin
  Attrs := THTMLAttributes.Create;
  // Decode hint string into short and long hints (separated by '|' )
  ShortHint := GetShortHint(Hint);
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
  // Build and return the tag
  Result := MakeTag('a', ttOpen, Attrs);
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
  Result := AOpenTag(URL, JSFn, Hint, Classes)
    + MakeSafeHTMLText(Text)
    + MakeTag('a', ttClose);
end;

end.

