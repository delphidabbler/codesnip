{
 * UDHTML.pas
 *
 * Set of classes and interfaces to use to dynamically update HTML in detail
 * pane.
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
 * The Original Code is UDHTML.pas
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


unit UDHTML;


interface


uses
  // Project
  Compilers.UGlobals, IntfHTMLDocHostInfo, UBaseObjects;


type

  {
  ICompCheckRoutineDHTML:
    Interface to DHTML object that manipulates snippet display in compiler check
    pane.
  }
  ICompCheckRoutineDHTML = interface(IInterface)
    ['{112DD126-0A16-4F7F-992C-35F94963B52C}']
    procedure DisplayCompileResults(const ACompilers: ICompilers);
      {Updates HTML to display results of last compile.
        @param ACompilers [in] Compilers object containing required results.
      }
  end;

  {
  TDHTMLFactory:
    Factory class for IDHTML objects.
  }
  TDHTMLFactory = class(TNoConstructObject)
  public
    class function CreateCompCheckRoutineDHTML(
      const HostInfo: IHTMLDocHostInfo): ICompCheckRoutineDHTML;
     {Creates instance of ICompCheckRoutineDHTML for use in compiler check
      frames.
        @param HostInfo [in] Provides information about object hosting the HTML
          that is to be manipulated.
        @return Required ICompCheckRoutineDHTML object.
      }
  end;


implementation


uses
  // Project
  UCompResHTML, UHTMLDocHelper, UHTMLUtils, UImageTags;


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

  {
  TCompCheckRoutineDHTML:
    Object that manipulates HTML of compiler results for a snippet displayed in
    compiler check pane.
  }
  TCompCheckRoutineDHTML = class(TDHTML,
    ICompCheckRoutineDHTML
  )
  protected
    { ICompCheckRoutineDHTML methods }
    procedure DisplayCompileResults(const ACompilers: ICompilers);
      {Updates HTML to display results of last compile.
        @param ACompilers [in] Compilers object containing required results.
      }
  end;


{ TDHTMLFactory }

class function TDHTMLFactory.CreateCompCheckRoutineDHTML(
  const HostInfo: IHTMLDocHostInfo): ICompCheckRoutineDHTML;
 {Creates instance of ICompCheckRoutineDHTML for use in compiler check frames.
    @param HostInfo [in] Provides information about object hosting the HTML that
      is to be manipulated.
    @return Required ICompCheckRoutineDHTML object.
  }
begin
  Result := TCompCheckRoutineDHTML.Create(HostInfo)
    as ICompCheckRoutineDHTML;
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
  Img := THTMLDocHelper.GetElementById(HostInfo.HTMLDocument, Id);
  TImageTags.SetSrc(Img, MakeSafeHTMLText(URL));
  THTMLDocHelper.SetTitle(Img, MakeSafeHTMLText(Title));
end;

procedure TDHTML.SetInnerHTML(const Id, HTML: string);
  {Sets a specified HTML tag's inner HTML.
    @param Id [in] Id attribute of required tag.
    @param HTML [in] Required inner HTML.
  }
begin
  THTMLDocHelper.SetInnerHTML(
    THTMLDocHelper.GetElementById(HostInfo.HTMLDocument, Id),
    HTML
  );
end;

{ TCompCheckRoutineDHTML }

procedure TCompCheckRoutineDHTML.DisplayCompileResults(
  const ACompilers: ICompilers);
  {Updates HTML to display results of last compile.
    @param ACompilers [in] Compilers object containing required results.
  }
var
  Compiler: ICompiler;  // references each compiler
begin
  for Compiler in ACompilers do
  begin
    // update compiler image
    SetImage(
      TCompCheckResHTML.TestImgId(Compiler),
      TCompCheckResHTML.ImageResURL(Compiler.GetLastCompileResult),
      TCompCheckResHTML.CompileResultDesc(Compiler.GetLastCompileResult)
    );
    // display any error / warning message links
    SetInnerHTML(
      TCompCheckResHTML.ErrCellId(Compiler),
      TCompCheckResHTML.LogLink(Compiler)
    );
  end;
end;

end.

