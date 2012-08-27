{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines a static class that provides helper methods for manipulating HTML
 * documents and elements.
}


unit UHTMLDOMHelper;


interface


uses
  // Delphi
  MSHTML,
  // Project
  UBaseObjects, UIStringList;


type

  {
  THTMLDOMHelper:
    Static class that provides helper methods for manipulating HTML documents
    and elements.
  }
  THTMLDOMHelper = class(TNoConstructObject)
  public
    class function ParentWindow(const Doc: IDispatch): IHTMLWindow2;
      {Gets reference to window object that hosts an HTML document.
        @param Doc [in] IDispatch interface of document.
        @return Reference to parent window object or nil if document not valid
          or has no parent window.
      }
    class function IsValidDocument(const Doc: IDispatch): Boolean;
      {Checks if an object is a valid HTML document.
        @param Doc [in] IDispatch interface to document to be checked.
        @return True if document is valid, False otherwise.
      }
    class function GetActiveElem(const Doc: IDispatch): IDispatch;
      {Gets active HTML element in a document.
        @param Doc [in] IDispatch interface to document.
        @return Reference to active element or nil if document is not valid or
          there is no active element.
      }
    class function GetDocTitle(const Doc: IDispatch): string;
      {Gets the title of an HTML document, i.e. content of <title> tag.
        @param Doc [in] IDispatch interface to document.
        @return Document title.
      }
    class function GetBodyElem(const Doc: IDispatch): IDispatch;
      {Gets reference to HTML document's body element.
        @param Doc [in] IDispatch interface to document.
        @return Reference to document's body or nil if document has no body
          element or is not valid.
      }
    class function GetElementById(const Doc: IDispatch;
      const Id: string): IDispatch;
      {Gets element from HTML document that has a specified ID.
        @param Doc [in] IDispatch interface to document containing element.
        @param Id [in] Id of required element.
        @return IDispatch interface of required element or nil if no such
          element.
      }
    class function GetScrollHeight(const Elem: IDispatch): Integer;
      {Gets scroll height of an HTML element.
        @param Elem [in] IDispatch interface to element.
        @return Required scroll height or 0 if element is not valid.
      }
    class function GetTextSelection(const Doc: IDispatch): string;
      {Gets selected text from a HTML document.
        @param Doc [in] IDispatch interface to document.
        @return Selected text or '' if no text selected, or document is not
          valid.
      }
    class function CreateBodyTextRange(const Doc: IDispatch): IHTMLTxtRange;
      {Creates a text range on an HTML document's body element.
        @param Doc [in] IDispatch interface to document.
        @return Text range or nil if document is not a valid HTML document.
      }
    class procedure FocusElem(const Elem: IDispatch);
      {Focusses an HTML element. Does nothing if element is not valid.
        @param Elem [in] IDispatch interface to HTML element to be focussed.
      }
    class procedure ScrollTo(const Doc: IDispatch; const X, Y: Integer);
      {Scrolls an HTML document to specified co-ordinates. Does nothing if
      document not valid or has no window.
        @param Doc [in] IDispatch interface to HTML document.
        @param X [in] X scroll co-ordinate.
        @param Y [in] Y scroll co-ordinate.
      }
    class procedure SetInnerHTML(const Elem: IDispatch; const HTML: string);
      {Sets HTML contained by an HTML element. Does nothing if element is not
      valid.
        @param Elem [in] IDispatch interface to HTML element.
        @param HTML [in] New HTML to be contained in element.
      }
    class procedure SetTitle(const Elem: IDispatch; const Title: string);
      {Sets the title attribute of an HTML element.
        @param Elem [in] IDispatch interface of HTML element whose title is to
          be set.
        @param Title [in] Required title. Should be valid HTML text.
      }
    class function GetElemClasses(const Elem: IDispatch): IStringList;
      {Gets list of CSS classes associated with an HTML element.
        @param Elem [in] IDispatch interface to a HTML element.
        @return List of class names.
      }
    class function ElemHasClass(const Elem: IDispatch;
      const ClassName: string): Boolean;
      {Checks if an HTML element has a specified CSS class.
        @param Elem [in] IDispatch interface of HTML element.
        @param ClassName [in] Name of CSS class being queried.
        @return True if elem has CSS class, False if not.
      }
    class function ElemIsVisible(const Elem: IDispatch): Boolean;
      {Checks if an HTML element is visible. It is considered visible only if it
      and all its parent elements are visible.
        @param Elem [in] IDispatch interface of HTML element.
        @return True if element is visible, False if hidden or Elem not an HTML
          element.
      }
    class function DocumentFromElem(const Elem: IDispatch): IDispatch;
      {Gets HTML document associated with an HTML element.
        @param Elem [in] IDispatch interface of HTML element.
        @return IDispatch interface of required HTML document.
      }
    class function ParentElem(const Elem: IDispatch;
      const TagName: string = ''; const ClassName: string = ''): IDispatch;
      {Gets reference to a parent element of a tag.
        @param Elem [in] IDispatch interface of HTML element for which parent is
          required.
        @param TagName [in] Name of parent tag. If parent tag doesn't have this
          name it is ignored and nil is returned. If TagName is '' then any tag
          matches.
        @param ClassName [in] Class name to be supported by parent tag. If
          parent doesn't have this class it is ignored and nil is returned. If
          ClassName is '' then any class matches.
        @return IDispatch interface of matching parent tag or nil if there is no
          parent or no matching parent.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Variants, Windows {for inlining},
  // Project
  UStrUtils, UUtils;


{ THTMLDocHelper }

class function THTMLDOMHelper.CreateBodyTextRange(
  const Doc: IDispatch): IHTMLTxtRange;
  {Creates a text range on an HTML document's body element.
    @param Doc [in] IDispatch interface to document.
    @return Text range or nil if document is not a valid HTML document.
  }
var
  BodyElem: IHTMLBodyElement; // reference to body HTML element of document
begin
  if Supports(GetBodyElem(Doc), IHTMLBodyElement, BodyElem) then
    Result := BodyElem.createTextRange
  else
    Result := nil;
end;

class function THTMLDOMHelper.DocumentFromElem(
  const Elem: IDispatch): IDispatch;
  {Gets HTML document associated with an HTML element.
    @param Elem [in] IDispatch interface of HTML element.
    @return IDispatch interface of required HTML document.
  }
var
  Element: IHTMLElement;  // IHTMLElement interface to Elem
begin
  if Supports(Elem, IHTMLElement, Element) then
    Result := Element.document
  else
    Result := nil;
end;

class function THTMLDOMHelper.ElemHasClass(const Elem: IDispatch;
  const ClassName: string): Boolean;
  {Checks if an HTML element has a specified CSS class.
    @param Elem [in] IDispatch interface of HTML element.
    @param ClassName [in] Name of CSS class being queried.
    @return True if elem has CSS class, False if not.
  }
var
  ClassNames: IStringList;  // list of CSS class names of element
begin
  ClassNames := GetElemClasses(Elem);
  Result := ClassNames.Contains(ClassName);
end;

class function THTMLDOMHelper.ElemIsVisible(const Elem: IDispatch): Boolean;
  {Checks if an HTML element is visible. It is considered visible only if it
  and all its parent elements are visible.
    @param Elem [in] IDispatch interface of HTML element.
    @return True if element is visible, False if hidden or Elem not an HTML
      element.
  }
var
  Element: IHTMLElement;  // IHTMLElement interface to Elem
begin
  if Supports(Elem, IHTMLElement, Element) then
  begin
    // Check if element itself is visible
    Result := (Element.style.display <> 'none');
    if Result and Assigned(Element.parentElement) then
      // Element is visible: check if parent is visible (recursive)
      Result := ElemIsVisible(Element.parentElement);
  end
  else
    // Not an HTML element
    Result := False;
end;

class procedure THTMLDOMHelper.FocusElem(const Elem: IDispatch);
  {Focusses an HTML element. Does nothing if element is not valid.
    @param Elem [in] IDispatch interface to HTML element to be focussed.
  }
var
  Element: IHTMLElement2; // IHTMLElement2 interface to element
begin
  if Supports(Elem, IHTMLElement2, Element) then
    Element.focus;
end;

class function THTMLDOMHelper.GetActiveElem(const Doc: IDispatch): IDispatch;
  {Gets active HTML element in a document.
    @param Doc [in] IDispatch interface to document.
    @return Reference to active element or nil if document is not valid or there
      is no active element.
  }
begin
  if IsValidDocument(Doc) then
    GetIntf((Doc as IHTMLDocument2).activeElement, IHTMLElement, Result)
  else
    Result := nil;
end;

class function THTMLDOMHelper.GetBodyElem(const Doc: IDispatch): IDispatch;
  {Gets reference to HTML document's body element.
    @param Doc [in] IDispatch interface to document.
    @return Reference to document's body or nil if document has no body element
      or is not valid.
  }
begin
  if IsValidDocument(Doc) then
    GetIntf((Doc as IHTMLDocument2).body, IDispatch, Result)
  else
    Result := nil;
end;

class function THTMLDOMHelper.GetDocTitle(const Doc: IDispatch): string;
  {Gets the title of an HTML document, i.e. content of <title> tag.
    @param Doc [in] IDispatch interface to document.
    @return Document title.
  }
begin
  if IsValidDocument(Doc) then
    Result := (Doc as IHTMLDocument2).title
  else
    Result := '';
end;

class function THTMLDOMHelper.GetElemClasses(
  const Elem: IDispatch): IStringList;
  {Gets list of CSS classes associated with an HTML element.
    @param Elem [in] IDispatch interface to a HTML element.
    @return List of class names.
  }
var
  Element: IHTMLElement;  // IHTMLElement inteface to Elem
begin
  Result := TIStringList.Create;
  Result.CaseSensitive := False;
  if Supports(Elem, IHTMLElement, Element) then
    Result.Add(Element.className, ' ', False);
end;

class function THTMLDOMHelper.GetElementById(const Doc: IDispatch;
  const Id: string): IDispatch;
  {Gets element from HTML document that has a specified ID.
    @param Doc [in] IDispatch interface to document containing element.
    @param Id [in] Id of required element.
    @return IDispatch interface of required element or nil if no such element.
  }
var
  BodyElem: IHTMLElement2;      // document body element
  Tags: IHTMLElementCollection; // all tags in document body
  Tag: IHTMLElement;            // a tag in document body
  I: Integer;                   // loops thru tags in document body
begin
  Result := nil;
  if not Supports(GetBodyElem(Doc), IHTMLElement2, BodyElem) then
    Exit;
  // Scan through all tags in body
  Tags := BodyElem.getElementsByTagName('*');
  for I := 0 to Pred(Tags.length) do
  begin
    // Check tag's id and return it if id matches
    Tag := Tags.item(I, EmptyParam) as IHTMLElement;
    if StrSameText(Tag.id, Id) then
    begin
      Result := Tag;
      Break;
    end;
  end;
end;

class function THTMLDOMHelper.GetScrollHeight(
  const Elem: IDispatch): Integer;
  {Gets scroll height of an HTML element.
    @param Elem [in] IDispatch interface to element.
    @return Required scroll height or 0 if element is not valid.
  }
var
  Element: IHTMLElement;  // IHTMLElement inteface to Elem
begin
  if Supports(Elem, IHTMLElement, Element) then
    Result := Element.getAttribute('scrollHeight', 0)
  else
    Result := 0;
end;

class function THTMLDOMHelper.GetTextSelection(
  const Doc: IDispatch): string;
  {Gets selected text from a HTML document.
    @param Doc [in] IDispatch interface to document.
    @return Selected text or '' if no text selected, or document is not valid.
  }
var
  Sel: IHTMLSelectionObject;  // object encapsulating the current selection
  Range: IHTMLTxtRange;       // object that encapsulates a text range
begin
  if IsValidDocument(Doc) and
    // first we get selection ...
    Supports((Doc as IHTMLDocument2).selection, IHTMLSelectionObject, Sel) and
    // ... then check it contains text ...
    (Sel.type_ = 'Text') and
    // ... the create a text range on it and read the text
    Supports(Sel.createRange, IHTMLTxtRange, Range) then
    Result := Range.text
  else
    Result := '';
end;

class function THTMLDOMHelper.IsValidDocument(
  const Doc: IDispatch): Boolean;
  {Checks if an object is a valid HTML document.
    @param Doc [in] IDispatch interface to document to be checked.
    @return True if document is valid, False otherwise.
  }
begin
  // Document is considered valid if it supports IHTMLDocument2
  Result := Supports(Doc, IHTMLDocument2);
end;

class function THTMLDOMHelper.ParentElem(const Elem: IDispatch;
  const TagName, ClassName: string): IDispatch;
  {Gets reference to a parent element of a tag.
    @param Elem [in] IDispatch interface of HTML element for which parent is
      required.
    @param TagName [in] Name of parent tag. If parent tag doesn't have this
      name it is ignored and nil is returned. If TagName is '' then any tag
      matches.
    @param ClassName [in] Class name to be supported by parent tag. If
      parent doesn't have this class it is ignored and nil is returned. If
      ClassName is '' then any class matches.
    @return IDispatch interface of matching parent tag or nil if there is no
      parent or no matching parent.
  }
var
  Element: IHTMLElement;  // IHTMLElement interface to element
begin
  Result := nil;
  if Supports(Elem, IHTMLElement, Element) then
  begin
    // Get any parent tag
    Result := Element.parentElement;
    if not Assigned(Result) then
      Exit;
    // Match tag name if specified
    if (TagName <> '') and
      not StrSameText(Element.parentElement.tagName, TagName) then
    begin
      Result := nil;
      Exit;
    end;
    // Match class name if specified
    if (ClassName <> '') and
      not ElemHasClass(Element.parentElement, ClassName) then
      Result := nil;
  end;
end;

class function THTMLDOMHelper.ParentWindow(const Doc: IDispatch): IHTMLWindow2;
  {Gets reference to window object that hosts an HTML document.
    @param Doc [in] IDispatch interface of document.
    @return Reference to parent window object or nil if document not valid or
      has no parent window.
  }
begin
  if IsValidDocument(Doc) then
    GetIntf((Doc as IHTMLDocument2).parentWindow, IHTMLWindow2, Result)
  else
    Result := nil;
end;

class procedure THTMLDOMHelper.ScrollTo(const Doc: IDispatch; const X,
  Y: Integer);
  {Scrolls an HTML document to specified co-ordinates. Does nothing if document
  not valid or has no window.
    @param Doc [in] IDispatch interface to HTML document.
    @param X [in] X scroll co-ordinate.
    @param Y [in] Y scroll co-ordinate.
  }
var
  Wdw: IHTMLWindow2;  // reference to document's parent window
begin
  Wdw := ParentWindow(Doc);
  if Assigned(Wdw) then
    Wdw.scroll(X, Y);
end;

class procedure THTMLDOMHelper.SetInnerHTML(const Elem: IDispatch;
  const HTML: string);
  {Sets HTML contained by an HTML element. Does nothing if element is not valid.
    @param Elem [in] IDispatch interface to HTML element.
    @param HTML [in] New HTML to be contained in element.
  }
var
  Element: IHTMLElement;  // IHTMLElement interface to element
begin
  if Supports(Elem, IHTMLElement, Element) then
    Element.innerHTML := WideString(HTML);
end;

class procedure THTMLDOMHelper.SetTitle(const Elem: IDispatch;
  const Title: string);
  {Sets the title attribute of an HTML element.
    @param Elem [in] IDispatch interface of HTML element whose title is to be
      set.
    @param Title [in] Required title. Should be valid HTML text.
  }
var
  Element: IHTMLElement;  // IHTMLElement interface to element
begin
  if Supports(Elem, IHTMLElement, Element) then
    Element.title := WideString(Title);
end;

end.

