{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines a static class that gets information about and manipulates HTML
 * anchor element.
}


unit UAnchors;


interface


uses
  // Project
  UBaseObjects, UDispatchList;


type
  ///  <summary>
  ///  Enumeration of various kinds of anchor displayed in browser controls.
  ///  </summary>
  ///  <remarks>
  ///  Anchor kinds are specified by CSS class.
  ///  </remarks>
  TAnchorKind = (
    akExternal,   // external link: class name = 'external-link'
    akSnippet,    // link to a snippet: class name = 'snippet-link'
    akTag,        // link to a tag: class name = 'tag-link'
    akCommand,    // link to a JS command: class name = 'command-link'
    akHelp,       // link to help topic: class name = 'help-link'
    akUnknown,    // unknown link kind
    akError       // error finding link kind (element may not be a link)
  );

type
  ///  <summary>
  ///  Static class that gets information about and manipulates HTML anchor
  ///  element.
  ///  </summary>
  TAnchors = class(TNoConstructObject)
  strict private
    ///  <summary>Checks if a HTML element Elem is an anchor element.</summary>
    class function IsAnchor(const Elem: IDispatch): Boolean;
  public
    ///  <summary>Gets kind of given anchor from CSS class associated with it.
    ///  </summary>
    ///  <remarks>
    ///  <para>akUnknown returned if no recognised CSS class associated with
    ///  anchor.</para>
    ///  <para>akError returned if parameter is not an anchor.</para>
    ///  </remarks>
    class function AnchorKind(const Anchor: IDispatch): TAnchorKind;

    ///  <summary>Returns a list of IDispatch interfaces to all anchors in given
    ///  HTML document Doc.</summary>
    class function GetAllAnchors(const Doc: IDispatch): IDispatchList;

    ///  <summary>Simulates a click on given anchor element and returns True
    ///  if anchor element is valid, False if not.</summary>
    ///  <remarks>Does nothing if anchor is not valid.</remarks>
    class function Click(const Anchor: IDispatch): Boolean;

    ///  <summary>Simulates a click on active link within given document and
    ///  returns True if active elememt is an anchor, False if not.</summary>
    ///  <remarks>Does nothing if active elememt is not an anchor.</remarks>
    class function ClickActiveLink(const Doc: IDispatch): Boolean;

    ///  <summary>Returns URL of given anchor.</summary>
    ///  <remarks>Empty string returned if anchor is not valid.</remarks>
    class function GetURL(const Anchor: IDispatch): string;

    ///  <summary>Returns inner text of given anchor.</summary>
    class function GetInnerText(const Anchor: IDispatch): string;

    ///  <summary>Returns any anchor element that encloses, or is, the given
    ///  HTML element.</summary>
    ///  <remarks>nil is returned if there is no enclosing anchor element.
    ///  </remarks>
    class function FindEnclosingAnchor(const Elem: IDispatch): IDispatch;
  end;


implementation


uses
  // Delphi
  SysUtils, Variants, MSHTML,
  // Project
  UHTMLDOMHelper, UIStringList;


{ TAnchors }

class function TAnchors.AnchorKind(const Anchor: IDispatch): TAnchorKind;
var
  ClassNames: IStringList;  // list of Anchor's CSS classes
begin
  if not IsAnchor(Anchor) then
    Exit(akError);
  ClassNames := THTMLDOMHelper.GetElemClasses(Anchor);
  if ClassNames.Contains('command-link') then
    Result := akCommand
  else if ClassNames.Contains('help-link') then
    Result := akHelp
  else if ClassNames.Contains('external-link') then
    Result := akExternal
  else if ClassNames.Contains('snippet-link') then
    Result := akSnippet
  else if ClassNames.Contains('tag-link') then
    Result := akTag
  else
    Result := akUnknown;
end;

class function TAnchors.Click(const Anchor: IDispatch): Boolean;
var
  Elem: IHTMLElement; // IHTMLElement interface to Anchor
begin
  Result := IsAnchor(Anchor) and Supports(Anchor, IHTMLElement, Elem);
  if Result then
    Elem.click;
end;

class function TAnchors.ClickActiveLink(const Doc: IDispatch): Boolean;
begin
  Result := Click(THTMLDOMHelper.GetActiveElem(Doc));
end;

class function TAnchors.FindEnclosingAnchor(const Elem: IDispatch): IDispatch;
var
  Element: IHTMLElement;  // IHTMLElement interface to Elem
begin
  if not Supports(Elem, IHTMLElement, Element) then
    Exit(nil);
  // Search up tree of elements looking for ALink element
  Result := Element;
  while Assigned(Result) and not Supports(Result, IHTMLAnchorElement) do
    Result := (Result as IHTMLElement).parentElement;
end;

class function TAnchors.GetAllAnchors(const Doc: IDispatch): IDispatchList;
var
  Document: IHTMLDocument2; // IHTMLDocument2 interface to document
  Idx: Integer;             // loops through all elements in document
  Elem: IHTMLElement;       // reference to an element in document
begin
  Result := TDispatchList.Create;
  if not Supports(Doc, IHTMLDocument2, Document) then
    Exit;
  for Idx := 0 to Pred(Document.all.length) do
  begin
    Elem := Document.all.item(Idx, EmptyParam) as IHTMLElement;
    if Supports(Elem, IHTMLAnchorElement) then
      Result.Add(Elem);
  end;
end;

class function TAnchors.GetInnerText(const Anchor: IDispatch): string;
var
  Elem: IHTMLElement; // IHTMLElement inteface to Anchor
begin
  if not IsAnchor(Anchor) then
    Exit('');
  if not Supports(Anchor, IHTMLElement, Elem) then
    Exit('');
  Result := Elem.innerText
end;

class function TAnchors.GetURL(const Anchor: IDispatch): string;
var
  AnchorElem: IHTMLAnchorElement; // IHTMLAnchorElement interface to Anchor
begin
  if not Supports(Anchor, IHTMLAnchorElement, AnchorElem) then
    Exit('');
  Result := AnchorElem.href;
end;

class function TAnchors.IsAnchor(const Elem: IDispatch): Boolean;
begin
  Result := Supports(Elem, IHTMLAnchorElement);
end;

end.

