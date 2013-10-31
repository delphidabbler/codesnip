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
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
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
      const View: TViewItem): TDetailPageHTML;
      {Creates correct HTML generator object for specified display pane and
      view.
        @param PageKind [in] Kind of page required: information or compiler
          check.
        @param View [in] View to be displayed.
        @return Required generator object.
      }
    class procedure InitBrowser(const PageKind: TDetailPageKind;
      const WBController: TWBController);
      {Loads a blank document into browser control if it doesn't contain one
      already.
        @param PageKind [in] Specifies which page that requires document
          loading.
        @param WBController [in] Controller object for HTML document.
      }
    class procedure DisplayHTML(const Generator: TDetailPageHTML;
      const WBController: TWBController);
      {Generates a displays HTML document.
        @param Generator [in] Object used to generate HTML.
        @param WBController [in] Controller object that loads HTML document.
      }
  public
    class procedure LoadPage(const PageKind: TDetailPageKind;
      const View: TViewItem; const WBController: TWBController);
      {Loads required HTML into body of a suitable host document.
        @param PageKind [in] Kind of page required: information or compiler
          check.
        @param View [in] View to be displayed.
        @param WBController [in] Controller object for displayed HTML document.
      }
  end;


implementation


uses
  // Delphi
  Classes;


{ TDetailPageLoader }

class function TDetailPageLoader.CreateGenerator(
  const PageKind: TDetailPageKind; const View: TViewItem): TDetailPageHTML;
  {Creates correct HTML generator object for specified display pane and view.
    @param PageKind [in] Kind of page required: information or compiler check.
    @param View [in] View to be displayed.
    @return Required generator object.
  }
begin
  // Create required generator
  Result := nil;
  case View.Kind of
    vkNone:
      Result := TNulPageHTML.Create(View);
    vkWelcome:
      Result := TWelcomePageHTML.Create(View);
    vkRoutine:
      case PageKind of
        pkInfo: Result := TRoutineInfoPageHTML.Create(View);
        pkComp: Result := TRoutineCompCheckPageHTML.Create(View);
      end;
    vkCategory:
      case PageKind of
        pkInfo: Result := TCategoryPageHTML.Create(View);
        pkComp: Result := TNoCompCheckPageHTML.Create(View);
      end;
    vkSnipKind:
      case PageKind of
        pkInfo: Result := TSnipKindPageHTML.Create(View);
        pkComp: Result := TNoCompCheckPageHTML.Create(View);
      end;
    vkAlphabet:
      case PageKind of
        pkInfo: Result := TAlphaListPageHTML.Create(View);
        pkComp: Result := TNoCompCheckPageHTML.Create(View);
      end;
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
  Stm: TStringStream; // stream that receives generated HTML
begin
  Stm := TStringStream.Create('');
  try
    Generator.Generate(Stm);
    WBController.IOMgr.ReplaceExistingBodyHTML(Stm.DataString);
  finally
    FreeAndNil(Stm);
  end;
end;

class procedure TDetailPageLoader.InitBrowser(const PageKind: TDetailPageKind;
  const WBController: TWBController);
  {Loads a blank document into browser control if it doesn't contain one
  already.
    @param PageKind [in] Specifies which page that requires document loading.
    @param WBController [in] Controller object for HTML document.
  }
begin
  if not WBController.IOMgr.HTMLDocumentExists then
    case PageKind of
      pkInfo:
        WBController.IOMgr.NavigateToResource(
          HInstance, 'detail-info.html'
        );
      pkComp:
        WBController.IOMgr.NavigateToResource(
          HInstance, 'detail-compcheck.html'
        );
    end;
end;

class procedure TDetailPageLoader.LoadPage(const PageKind: TDetailPageKind;
  const View: TViewItem; const WBController: TWBController);
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
  InitBrowser(PageKind, WBController);
  Generator := CreateGenerator(PageKind, View);
  try
    DisplayHTML(Generator, WBController);
  finally
    FreeAndNil(Generator);
  end;
end;

end.

