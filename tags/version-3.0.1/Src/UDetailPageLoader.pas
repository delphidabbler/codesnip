{
 * UDetailPageLoader.pas
 *
 * Heirachy of classes that can produce and load HTML pages displayed in the
 * detail panes. Also provides a factory class to create the loader objects.
 *
 * v0.1 of 21 Feb 2005  - Original version.
 * v0.2 of 25 Apr 2005  - Changed to use same welcome page (loaded by
 *                        TWelcomePageLoader) for both info and compiler check
 *                        panes when database not empty.
 *                      - Renamed TInfoWelcomePageLoader class as
 *                        TWelcomePageLoader now used for both panes.
 *                      - Deleted redundant TInfoWelcomePageLoader class that
 *                        was used to load compiler check welcome page.
 *                      - Changed TWelcomePageLoader to load renamed
 *                        welcome.html document.
 * v0.3 of 03 Jun 2005  - Changed to load dynamic pages by first loading a
 *                        suitable blank document containing required styles and
 *                        scripts and then inserting code generated using
 *                        templates into blank document's <body> section. This
 *                        is done to allow JavaScript to run when scripts are
 *                        disabled in user's Internet zone. Previous method was
 *                        to load whole document from stream which made web
 *                        browser control use security in internet zone.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 *                      - Relocated and rationalised $WARN directives.
 * v1.1 of 03 Dec 2006  - Replaced two different loaders for routines in
 *                        compiler check pane with single one.
 *                      - Added new TCompRoutinePageLoader, that loads renamed
 *                        comp-routine.html, and deleted TCompPageLoader,
 *                        TCompDBRoutinePageLoader and
 *                        TCompTestRoutinePageLoader subclasses.
 *                      - Updated TDetailPageKind enumeration to reflect change
 *                        from three page kinds to two.
 *                      - Updated TDetailPageLoaderFactory re changes.
 * v1.2 of 03 Dec 2006  - Changed TInfoRoutinePageLoader to descend directly
 *                        from TDynamicPageLoader instead of TInfoPageLoader and
 *                        made use info-routine.html as its resource document
 *                        instead of info-blank.html.
 *                      - Renamed TInfoPageLoader as TInfoRoutineListPageLoader
 *                        since it is now base class for information pane views
 *                        that display a table of routines. Made appropriate
 *                        changes to associated sub-classes.
 * v1.3 of 04 Feb 2007  - Replaced redundant TDetailView class references with
 *                        TViewItem.
 * v1.4 of 14 Sep 2008  - Revised TWelcomePageLoader to load only welcome.html.
 *                        nodata-welcome.html no longer used.
 * v1.5 of 04 Oct 2008  - Changed TDetailPageLoaderFactory to derive from
 *                        TNoConstructObject and hence prevented it from being
 *                        constructed.
 *                      - Made various protected and private sections strict.
 *                      - Now use ClassName method in all assert statements.
 * v2.0 of 25 Jan 2009  - Total rewrite and simplification. Now provides a
 *                        single static class that ensures a blank HTML document
 *                        is loaded and modifies body to display HTML generated
 *                        by a generator object.
 *                      - Removed TDetailPageLoaderFactory, TDetailPageLoader,
 *                        TEmptyPageLoader, TStaticPageLoader,
 *                        TWelcomePageLoader, TCompNAPageLoader,
 *                        TDynamicPageLoader, TInfoRoutineListPageLoader,
 *                        TInfoUncatHeaderPageLoader, TInfoCategoryPageLoader,
 *                        TInfoRoutinePageLoader and TCompRoutinePageLoader
 *                        classes.
 *                      - Removed TDetailPageLoaderClass class reference.
 *                      - Rewrote TDetailPageLoader and static class.
 *                      - Removed unnecessary $WARN directive.
 *                      - Now use only detail-info.html and
 *                        detail-compcheck.html as base documents.
 * v2.1 of 16 May 2009  - Removed IDetailViewHostInfo parameter from
 *                        TDetailPageLoader.LoadPage method.
 *                      - Removed generators for Uncategorised view type.
 *                      - Added generators for Alphabet and SnippetsKind view
 *                        types.
 *                      - Refactored code into several class methods.
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
 * The Original Code is UDetailPageLoader.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UDetailPageLoader;


interface


uses
  // Delphi
  SysUtils,
  // Project
  UBaseObjects, UDetailPageHTML, UView, UWBController;


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

