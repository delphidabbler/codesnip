{
 * UHTMLDetailUtils.pas
 *
 * Utility functions that assist in generating HTML code used in creating detail
 * pane pages.
 *
 * v0.1 of 20 Feb 2005  - Original version.
 * v0.2 of 20 Apr 2005  - Changed to use renamed IntfCompilers unit.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 * v1.1 of 25 Oct 2006  - Removed code in ALink() and TextLink() functions that
 *                        created "target" attribute in a-links.
 *                      - Changed so that a-links with '' passed for URL now
 *                        generate href = 'javascript:void(0);' rather than '#'.
 * v1.2 of 26 Nov 2006  - Changed to use JSLiteralFunc routine to create
 *                        JavaScript routine calls.
 * v1.3 of 02 Dec 2006  - CompilerImg() routine removed.
 * v1.4 of 31 Oct 2007  - Added support for adding class names to generated
 *                        a-links.
 *                      - Added 'routine-link' class to routine a-links.
 *                      - Changed to use IStringList instead of TStringList.
 * v1.5 of 26 Aug 2008  - Modified parameter list of RoutineALink to add a
 *                        UserDefined parameter. This data is used used as a
 *                        parameter to underlying JScript function.
 * v1.6 of 27 Dec 2008  - Added new AOpenTag routine to create and opening
 *                        hyperlink tag and modified ALink routine to use it.
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
 * The Original Code is UHTMLDetailUtils.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UHTMLDetailUtils;


interface


uses
  // Project
  UIStringList;


function TextLink(const URL, JSFn, Hint: string; const Classes: IStringList;
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

function JSLink(const JSFn, Hint: string; const Classes: IStringList;
   const Text: string): string;
  {Creates an <a>..</a> link surrounding some text. The link triggers JavaScript
  code and does not access a URL.
    @param JSFn [in] JavaScript code called in tag's onclick event. Passing ''
      prevents onclick event being used. When set the onclick event returns
      false.
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

function RoutineALink(const RoutineName: string;
  const UserDefined: Boolean): string;
  {Creates an <a>..</a> link that triggers external object's DisplayRoutine
  method when clicked. The link does not access a URL. The link's text is the
  name of the routine. The external object's ShowHint method is called to
  display a hint containing the routine's name when the mouse passes over the
  link.
    @param RoutineName [in] Name of routine to be displayed when link is
      clicked.
    @param UserDefined [in] Flag indicating if routine is user defined.
    @return Complete <a> tag.
  }

function AOpenTag(const URL, JSFn, Hint: string;
  const Classes: IStringList): string;
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

implementation


uses
  // Delphi
  SysUtils, Controls,
  // Project
  UHTMLUtils, UJavaScriptUtils;


function AOpenTag(const URL, JSFn, Hint: string;
  const Classes: IStringList): string;
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
  cJSFn = '%s; return false;';    // adds a false return to any JavaScript func
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
    Attrs.Add('onclick', Format(cJSFn, [JSFn]));
  // Set onmouseover and onmouseout to external ShowHint if long hint provided
  if LongHint <> '' then
  begin
    Attrs.Add(
      'onmouseover', Format(cJSFn, [JSLiteralFunc('showHint', [LongHint])])
    );
    Attrs.Add('onmouseout', Format(cJSFn, [JSLiteralFunc('clearHint', [])]));
  end;
  // Build and return the tag
  Result := MakeTag('a', ttOpen, Attrs);
end;

function ALink(const URL, JSFn, Hint: string; const Classes: IStringList;
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

function TextLink(const URL, JSFn, Hint: string; const Classes: IStringList;
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

function JSLink(const JSFn, Hint: string; const Classes: IStringList;
   const Text: string): string;
  {Creates an <a>..</a> link surrounding some text. The link triggers JavaScript
  code and does not access a URL.
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
  Result := TextLink('', JSFn, Hint, Classes, Text);
end;

function RoutineALink(const RoutineName: string;
  const UserDefined: Boolean): string;
  {Creates an <a>..</a> link that triggers external object's DisplayRoutine
  method when clicked. The link does not access a URL. The link's text is the
  name of the routine. The external object's ShowHint method is called to
  display a hint containing the routine's name when the mouse passes over the
  link.
    @param RoutineName [in] Name of routine to be displayed when link is
      clicked.
    @param UserDefined [in] Flag indicating if routine is user defined.
    @return Complete <a> tag.
  }
resourcestring
  // Long hint
  sRoutineHint = 'Display "%s"';
begin
  // Create javascript link enclosing routine name
  Result := JSLink(
    JSLiteralFunc(                // JS fn to display routine
      'displayRoutine',
      [RoutineName, UserDefined]
    ),
    '|' + Format(sRoutineHint, [RoutineName]),      // long hint only
    TIStringList.Create('routine-link'),            // class indicates routine
    RoutineName                                     // displayed name of routine
  );
end;

end.
