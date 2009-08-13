{
 * UImageTags.pas
 *
 * Defines a static class that gets information about and manipulates HTML
 * image elements.
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
 * The Original Code is UImageTags.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UImageTags;


interface


uses
  // Project
  UBaseObjects, UDispatchList;


type

  {
  TImageTags:
    Static class that gets information about and manipulates HTML image
    elements.
  }
  TImageTags = class(TNoConstructObject)
  strict private
    class function IsImageTag(const Elem: IDispatch): Boolean;
      {Checks if an HTML element is an image tag.
        @param Elem [in] IDispatch interface of HTML element to be checked.
        @return True if Elem is an image element, False if not.
      }
  public
    class function GetSrc(const Img: IDispatch): string;
      {Gets the URL of the image displayed by an image tag.
        @param Img [in] IDispatch interface to an image element.
        @return Required source URL.
      }
    class procedure SetSrc(const Img: IDispatch; const URL: string;
      const Width: Integer = -1; const Height: Integer = -1);
      {Causes image tag to display image at a specified URL.
        @param Img [in] IDispatch interface to image element.
        @param URL [in] URL of image to be displayed.
        @param Width [in] Optional new width of image. Ignored if < 0.
        @param Height [in] Optional new height of image. Ignored if < 0.
      }
    class function GetAllImageTags(const Parent: IDispatch): IDispatchList;
      {Gets a list of all image tags that are children of a parent element.
        @param Parent [in] IDispatch interface to HTML element that contains
          image tags as immediate children.
        @return List of IDispatch interfaces to image elements.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Variants, MSHTML;


{ TImageTags }

class function TImageTags.GetAllImageTags(
  const Parent: IDispatch): IDispatchList;
  {Gets a list of all image tags that are children of a parent element.
    @param Parent [in] IDispatch interface to HTML element that contains image
      tags as immediate children.
    @return List of IDispatch interfaces to image elements.
  }
var
  Idx: Integer;                       // loops through all child elements
  ParentElem: IHTMLElement2;          // IHTMLElement2 interface to parent elem
  ChildElems: IHTMLElementCollection; // all child elements of parent
  ChildElem: IDispatch;               // reference to an element in document
begin
  Result := TDispatchList.Create;
  if not Supports(Parent, IHTMLElement2, ParentElem) then
    Exit;
  ChildElems := ParentElem.getElementsByTagName('*');
  for Idx := 0 to Pred(ChildElems.length) do
  begin
    ChildElem := ChildElems.item(Idx, EmptyParam);
    if IsImageTag(ChildElem) then
      Result.Add(ChildElem);
  end;
end;

class function TImageTags.GetSrc(const Img: IDispatch): string;
  {Gets the URL of the image displayed by an image tag.
    @param Img [in] IDispatch interface to an image element.
    @return Required source URL.
  }
var
  ImgElem: IHTMLImgElement; // IHTMLImgElement interface to Img
begin
  Result := '';
  if not Supports(Img, IHTMLImgElement, ImgElem) then
    Exit;
  Result := ImgElem.src;
end;

class function TImageTags.IsImageTag(const Elem: IDispatch): Boolean;
  {Checks if an HTML element is an image tag.
    @param Elem [in] IDispatch interface of HTML element to be checked.
    @return True if Elem is an image element, False if not.
  }
begin
  Result := Supports(Elem, IHTMLImgElement);
end;

class procedure TImageTags.SetSrc(const Img: IDispatch; const URL: string;
  const Width, Height: Integer);
  {Causes image tag to display image at a specified URL.
    @param Img [in] IDispatch interface to image element.
    @param URL [in] URL of image to be displayed.
    @param Width [in] Optional new width of image. Ignored if < 0.
    @param Height [in] Optional new height of image. Ignored if < 0.
  }
var
  ImgElem: IHTMLImgElement; // IHTMLImgElement interface to Img
begin
  if not Supports(Img, IHTMLImgElement, ImgElem) then
    Exit;
  ImgElem.src := URL;
  if Width >= 0 then
    ImgElem.width := Width;
  if Height >= 0 then
    ImgElem.height := Height;
end;

end.

