{
 * UAnchors.pas
 *
 * Defines a static class that gets information about and manipulates HTML
 * anchor element.
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
 * The Original Code is UAnchors.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UAnchors;


interface


uses
  // Project
  UBaseObjects, UDispatchList;


type

  {
  TAnchorKind:
    Enumeration of various kinds of anchor displayed in browser controls.
    Anchor kinds are specified by CSS class.
  }
  TAnchorKind = (
    akExternal,   // external link: class name = 'external-link'
    akRoutine,    // link to a routine: class name = 'routine-link'
    akCategory,   // link to a category: class name = 'category-link'
    akCommand,    // link to a JS command: class name = 'command-link'
    akHelp,       // link to help topic: class name = 'help-link'
    akUnknown,    // unknown link kind
    akError       // error finding link kind (element may not be a link)
  );

  {
  TAnchors:
    Static class that gets information about and manipulates HTML anchor
    element.
  }
  TAnchors = class(TNoConstructObject)
  strict private
    class function IsAnchor(const Elem: IDispatch): Boolean;
      {Checks if an HTML element is an anchor element.
        @param Elem [in] IDispatch interface to HTML element.
        @return True if element is anchor, False if not.
      }
  public
    class function AnchorKind(const Anchor: IDispatch): TAnchorKind;
      {Gets kind of anchor dependent on css class of anchor element.
        @param Anchor [in] Anchor to be tested.
        @return Anchor kind.
      }
    class function GetAllAnchors(const Doc: IDispatch): IDispatchList;
      {Gets a list of all anchors in a HTML document.
        @param Doc [in] IDispatch interface to HTML document.
        @return List of IDispatch interfaces to anchors.
      }
    class function Click(const Anchor: IDispatch): Boolean;
      {Emulates a click on an anchor element. Does nothing if provided anchor is
      not valid.
        @param Anchor [in] IDispatch interface to anchor element.
        @return True if anchor is valid or False if not.
      }
    class function GetURL(const Anchor: IDispatch): string;
      {Gets URL of an anchor tag.
        @param Anchor [in] Reference to tag for which URL is required.
        @return Required URL or '' if Anchor is not an anchor tag.
      }
    class function GetInnerText(const Anchor: IDispatch): string;
      {Gets the inner text of an anchor.
        @param Anchor [in] IDispath interface to anchor element.
        @return Required text.
      }
    class function FindEnclosingAnchor(const Elem: IDispatch): IDispatch;
      {Finds any anchor HTML element that encloses, or is, a specified element.
        @param Elem [in] Element for which enclosing anchor is required.
        @return IDispatch interface of enclosing anchor element (which may be
          Elem itself if Elem is anchor element) or nil if there is no enclosing
          anchor element.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Variants, MSHTML,
  // Project
  UHTMLDocHelper, UIStringList;


{ TAnchors }

class function TAnchors.AnchorKind(const Anchor: IDispatch): TAnchorKind;
  {Gets kind of anchor dependent on css class of anchor element.
    @param Anchor [in] Anchor to be tested.
    @return Anchor kind.
  }
var
  ClassNames: IStringList;  // list of element's CSS classes
begin
  if IsAnchor(Anchor) then
  begin
    ClassNames := THTMLDocHelper.GetElemClasses(Anchor);
    if ClassNames.Contains('command-link') then
      Result := akCommand
    else if ClassNames.Contains('help-link') then
      Result := akHelp
    else if ClassNames.Contains('external-link') then
      Result := akExternal
    else if ClassNames.Contains('routine-link') then
      Result := akRoutine
    else if ClassNames.Contains('category-link') then
      Result := akCategory
    else
      Result := akUnknown
  end
  else
    Result := akError;
end;

class function TAnchors.Click(const Anchor: IDispatch): Boolean;
  {Emulates a click on an anchor element. Does nothing if provided anchor is not
  valid.
    @param Anchor [in] IDispatch interface to anchor element.
    @return True if anchor is valid or False if not.
  }
var
  Elem: IHTMLElement; // IHTMLElement interface to Elem
begin
  Result := IsAnchor(Anchor) and Supports(Anchor, IHTMLElement, Elem);
  if Result then
    Elem.click;
end;

class function TAnchors.FindEnclosingAnchor(
  const Elem: IDispatch): IDispatch;
  {Finds any anchor HTML element that encloses, or is, a specified element.
    @param Elem [in] Element for which enclosing anchor is required.
    @return IDispatch interface of enclosing anchor element (which may be
      Elem itself if Elem is anchor element) or nil if there is no enclosing
      anchor element.
  }
var
  Element: IHTMLElement;  // IHTMLElement interface to Elem
begin
  Result := nil;
  if not Supports(Elem, IHTMLElement, Element) then
    Exit;
  // Search up tree of elements looking for ALink element
  Result := Element;
  while Assigned(Result) and not Supports(Result, IHTMLAnchorElement) do
    Result := (Result as IHTMLElement).parentElement;
end;

class function TAnchors.GetAllAnchors(const Doc: IDispatch): IDispatchList;
  {Gets a list of all anchors in a HTML document.
    @param Doc [in] IDispatch interface to HTML document.
    @return List of IDispatch interfaces to anchors.
  }
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
  {Gets the inner text of an anchor.
    @param Anchor [in] IDispath interface to anchor element.
    @return Required text.
  }
var
  Elem: IHTMLElement; // IHTMLElement inteface to Elem
begin
  if IsAnchor(Anchor) and Supports(Anchor, IHTMLElement, Elem) then
    Result := Elem.innerText
  else
    Result := '';
end;

class function TAnchors.GetURL(const Anchor: IDispatch): string;
  {Gets URL of an anchor tag.
    @param Anchor [in] Reference to tag for which URL is required.
    @return Required URL or '' if Anchor is not an anchor tag.
  }
var
  AnchorElem: IHTMLAnchorElement; // IHTMLAnchorElement interface to ALink
begin
  Result := '';
  if not Supports(Anchor, IHTMLAnchorElement, AnchorElem) then
    Exit;
  Result := AnchorElem.href;
end;

class function TAnchors.IsAnchor(const Elem: IDispatch): Boolean;
  {Checks if an HTML element is an anchor element.
    @param Elem [in] IDispatch interface to HTML element.
    @return True if element is anchor, False if not.
  }
begin
  Result := Supports(Elem, IHTMLAnchorElement);
end;

end.

