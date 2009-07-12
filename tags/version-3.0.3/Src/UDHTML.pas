{
 * UDHTML.pas
 *
 * Set of classes and interfaces to use to dynamically update HTML in detail
 * pane.
 *
 * v1.0 of 02 Dec 2006  - Original version.
 * v1.1 of 03 Dec 2006  - Added interfaces and classes to support dynamic
 *                        updating of routine view in information pane.
 *                      - Reorganised class heirachy to move code common to
 *                        information and compiler check classes into base
 *                        classes. This reorganisation also takes account of
 *                      - Changed factory class to work with information pane
 *                        code.
 *                      - Changed to use new URoutineHTML unit to provide HTML
 *                        relating to routines for information and compiler
 *                        check panes. Removed HTML generation code from
 *                        TCompCheckRoutineInfoDHTML as a result.
 * v1.2 of 04 Feb 2007  - Replaced redundant TDetailView class references with
 *                        TViewItem.
 * v1.3 of 16 Feb 2007  - Added new SupportsUpdating method to IDHTML designed
 *                        to check if a DTHML object supports dynamic updating.
 *                        Added suitable method to implementing classes.
 * v1.4 of 17 Feb 2007  - Calls new version of JSExec in UJavaScriptUtils
 *                        passing function name and parameters separately.
 *                      - Changed return type of IDHTMLHostInfo.HTMLDocument to
 *                        IDispatch.
 * v1.5 of 14 Sep 2008  - Modified TCompCheckRoutineDHTML to hide toggle test
 *                        unit visibility and test compile command links when
 *                        routine is not in standard format.
 *                      - Made some private and protected class sections strict.
 *                      - Made user defined routine headings appear in blue.
 *                      - Added new TWelcomeDetailsDHTML class that dynamically
 *                        update revised welcome page. Modified factory class to
 *                        create it when required.
 *                      - Modified TInfoRoutineDHTML to show edit link for user
 *                        defined routines.
 *                      - Added TDetailDHTML.SetOnClick method.
 * v1.6 of 04 Oct 2008  - Changed TDHTMLFactory to derive from
 *                        TNoConstructObject and hence prevented it from being
 *                        constructed.
 *                      - Now use ClassName method in assert statement.
 * v1.7 of 15 Dec 2008  - Changed to iterate compilers using for..in loops.
 * v1.8 of 09 Jan 2009  - Added parameter to ICompCheckRoutineDHTML's
 *                        DisplayCompileResults method to provide reference to
 *                        compilers object whose results are required. Modified
 *                        TCompCheckRoutineDHTML implementation accordingly.
 *                      - Changed TRoutineDetailDHTML to use own instantiation
 *                        of compilers object for compiler info rather than
 *                        global compilers singleton. Provides this to sub
 *                        classes as a protected property.
 * v1.9 of 13 Jan 2009  - Replaced control char literals with constants.
 * v2.0 of 25 Jan 2009  - Heavliy revised now that many interfaces and much code
 *                        is no longer used since generation of detgail HTML
 *                        changed to avoid use of dynamic code generation.
 *                        required interfaces from IHTMLDocHostInfo.
 *                      - HostInfo interfaces that are still required now come
 *                        from IntfHTMLDocHostInfo.
 *                      - Now set inner HTML and image attributes by
 *                        manipulating HTML elements directly instead of by
 *                        calling JavaScript routines.
 * v2.1 of 06 Jun 2009  - Removed UpdateTestUnitVisibility method removed from
 *                        ICompCheckRoutineDHTML.
 *                      - Removed ExecJSFn method from TDHTML.
 *                      - Removed UpdateTestUnitVisibility and ShowTestUnit
 *                        methods from TCompCheckRoutineDHTML.
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
 * The Original Code is UDHTML.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UDHTML;


interface


uses
  // Project
  IntfCompilers, IntfHTMLDocHostInfo, UBaseObjects;


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

