{
 * UDetailPageLoader.pas
 *
 * Heirachy of classes that can produce and load HTML pages displayed in the
 * detail panes. Also provides a factory class to create the loader objects.
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
 * The Original Code is UDetailPageLoader.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UDetailPageLoader;


interface


uses
  // Delphi
  SysUtils,
  // Project
  Browser.UController, UBaseObjects, UDetailPageHTML, UView;


type

  {
  TDetailPageKind:
    Enumeration that defines kind of page to be loaded.
  }
  TDetailPageKind = (
    pkInfo,         // for use on detailed information page
    pkComp          // for use on compiler check page
  );

  {
  TDetailPageLoader:
    Static class that ensures an HTML document is loaded and sets its body to
    required HTML.
  }
  TDetailPageLoader = class(TNoConstructObject)
  strict private
    class function CreateGenerator(const PageKind: TDetailPageKind;
      View: IView): TDetailPageHTML;
      {Creates correct HTML generator object for specified display pane and
      view.
        @param PageKind [in] Kind of page required: information or compiler
          check.
        @param View [in] View to be displayed.
        @return Required generator object.
      }
    class procedure InitBrowser(const WBController: TWBController);
      {Loads a blank document into browser control if it doesn't contain one
      already.
        @param WBController [in] Controller object for HTML document.
      }
    class procedure DisplayHTML(const Generator: TDetailPageHTML;
      const WBController: TWBController);
      {Generates a displays HTML document.
        @param Generator [in] Object used to generate HTML.
        @param WBController [in] Controller object that loads HTML document.
      }
  public
    class procedure LoadPage(const PageKind: TDetailPageKind; View: IView;
      const WBController: TWBController);
      {Loads required HTML into body of a suitable host document.
        @param PageKind [in] Kind of page required: information or compiler
          check.
        @param View [in] View to be displayed.
        @param WBController [in] Controller object for displayed HTML document.
      }
  end;


implementation


{ TDetailPageLoader }

class function TDetailPageLoader.CreateGenerator(
  const PageKind: TDetailPageKind; View: IView): TDetailPageHTML;
  {Creates correct HTML generator object for specified display pane and view.
    @param PageKind [in] Kind of page required: information or compiler check.
    @param View [in] View to be displayed.
    @return Required generator object.
  }
begin
  // Create required generator
  Result := nil;
  if Supports(View, INulView) then
    Result := TNulPageHTML.Create(View)
  else if Supports(View, IStartPageView) then
    Result := TWelcomePageHTML.Create(View)
  else if Supports(View, ISnippetView) then
    case PageKind of
      pkInfo: Result := TRoutineInfoPageHTML.Create(View);
      pkComp: Result := TRoutineCompCheckPageHTML.Create(View);
    end
  else if Supports(View, ICategoryView) then
    case PageKind of
      pkInfo: Result := TCategoryPageHTML.Create(View);
      pkComp: Result := TNoCompCheckPageHTML.Create(View);
    end
  else if Supports(View, ISnippetKindView) then
    case PageKind of
      pkInfo: Result := TSnipKindPageHTML.Create(View);
      pkComp: Result := TNoCompCheckPageHTML.Create(View);
    end
  else if Supports(View, IInitialLetterView) then
    case PageKind of
      pkInfo: Result := TAlphaListPageHTML.Create(View);
      pkComp: Result := TNoCompCheckPageHTML.Create(View);
    end;
  Assert(Assigned(Result), ClassName + '.CreateGenerator: No HTML generator');
end;

class procedure TDetailPageLoader.DisplayHTML(const Generator: TDetailPageHTML;
  const WBController: TWBController);
  {Generates a displays HTML document.
    @param Generator [in] Object used to generate HTML.
    @param WBController [in] Controller object that loads HTML document.
  }
var
  HTML: string; // HTML to be displayed
begin
  HTML := Generator.Generate;
  WBController.IOMgr.ReplaceExistingBodyHTML(HTML);
end;

class procedure TDetailPageLoader.InitBrowser(
  const WBController: TWBController);
  {Loads a blank document into browser control if it doesn't contain one
  already.
    @param WBController [in] Controller object for HTML document.
  }
begin
  if not WBController.IOMgr.HTMLDocumentExists then
    WBController.IOMgr.NavigateToResource(HInstance, 'detail.html');
end;

class procedure TDetailPageLoader.LoadPage(const PageKind: TDetailPageKind;
  View: IView; const WBController: TWBController);
  {Loads required HTML into body of a suitable host document.
    @param PageKind [in] Kind of page required: information or compiler check.
    @param View [in] View to be displayed.
    @param WBController [in] Controller object for displayed HTML document.
  }
var
  Generator: TDetailPageHTML; // object used to generate body's inner HTML
begin
  Assert(Assigned(View), ClassName + '.LoadPage: View is nil');
  Assert(Assigned(WBController), ClassName + '.LoadPage: WBController is nil');
  InitBrowser(WBController);
  Generator := CreateGenerator(PageKind, View);
  try
    DisplayHTML(Generator, WBController);
  finally
    Generator.Free;
  end;
end;

end.

