{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Set of classes and interfaces to use to dynamically update HTML in detail
 * pane.
}


unit UDHTML;


interface


uses
  // Project
  Compilers.UGlobals, IntfHTMLDocHostInfo, UBaseObjects;


{ TODO: Keep this unit until development is complete and then remove, along with
        definition of IHTMLDocHostInfo if not needed. I've left the base class
        here in case any DHTML is required. }


implementation


uses
  // Project
  UCompResHTML, UHTMLDOMHelper, UHTMLUtils, UImageTags;


type

  {
  TDHTML:
    Base class for all objects that dynamically manipulate HTML documents.
  }
  TDHTML = class(TInterfacedObject)
  strict private
    fHostInfo: IHTMLDocHostInfo; // Value of HostInfo property
  strict protected
    property HostInfo: IHTMLDocHostInfo read fHostInfo;
      {Reference to object providing information about HTML host object}
    procedure SetInnerHTML(const Id, HTML: string);
      {Sets a specified HTML tag's inner HTML.
        @param Id [in] Id attribute of required tag.
        @param HTML [in] Required inner HTML.
      }
    procedure SetImage(const Id, URL, Title: string);
      {Sets a specified HTML image tag's source URL and title.
        @param Id [in] Id attribute of image tag.
        @param URL [in] URL of image to be displayed.
        @param Title [in] Title of image.
      }
  public
    constructor Create(const HostInfo: IHTMLDocHostInfo);
      {Class constructor. Sets up object.
        @param HostInfo [in] Object that provides information about object
          hosting HTML document.
      }
  end;

{ TDHTML }

constructor TDHTML.Create(const HostInfo: IHTMLDocHostInfo);
  {Class constructor. Sets up object.
    @param HostInfo [in] Object that provides information about object hosting
      HTML document.
  }
begin
  Assert(Assigned(HostInfo), ClassName + '.Create: HostInfo is nil');
  inherited Create;
  fHostInfo := HostInfo;
end;

procedure TDHTML.SetImage(const Id, URL, Title: string);
  {Sets a specified HTML image tag's source URL and title.
    @param Id [in] Id attribute of image tag.
    @param URL [in] URL of image to be displayed.
    @param Title [in] Title of image.
  }
var
  Img: IDispatch; // reference to required image element
begin
  Img := THTMLDOMHelper.GetElementById(HostInfo.HTMLDocument, Id);
  TImageTags.SetSrc(Img, MakeSafeHTMLText(URL));
  THTMLDOMHelper.SetTitle(Img, MakeSafeHTMLText(Title));
end;

procedure TDHTML.SetInnerHTML(const Id, HTML: string);
  {Sets a specified HTML tag's inner HTML.
    @param Id [in] Id attribute of required tag.
    @param HTML [in] Required inner HTML.
  }
begin
  THTMLDOMHelper.SetInnerHTML(
    THTMLDOMHelper.GetElementById(HostInfo.HTMLDocument, Id),
    HTML
  );
end;

end.

