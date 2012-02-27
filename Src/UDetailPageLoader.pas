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
  // Project
  Browser.UController, UBaseObjects, UDetailPageHTML, UView;


type
  { TODO: Consider changing class into normal class and maintain an instance in
          detail frame. This way detail.html can be loaded in constructor and
          reference kept to controller, which is also passed in constructor.
  }
  ///  <summary>
  ///  Static class that displays an HTML page that represents a view.
  ///  </summary>
  ///  <remarks>
  ///  Used to display views in main window's detail pane.
  ///  </remarks>
  TDetailPageLoader = class(TNoConstructObject)
  strict private
    ///  <summary>Loads a blank document into a browser control if no document
    ///  already exists.</summary>
    ///  <param name="WBController">TWBController [in] Controller object for
    ///  an associated web browser control.</param>
    ///  <remarks>Blank document imports all CSS and JavaScript required for
    ///  detail pane views.</remarks>
    class procedure InitBrowser(const WBController: TWBController);
    ///  <summary>Generates and displays a view in a browser control.</summary>
    ///  <param name="Generator">TDetailPageHTML [in] Object that generates
    ///  HTML to be displayed.</param>
    ///  <param name="WBController">TWBController [in] Controller object used to
    ///  display HTML in its web browser control.</param>
    ///  <remarks>Generated HTML replaces current body of document currently
    ///  loaded in web browser.</remarks>
    class procedure DisplayHTML(const Generator: TDetailPageHTML;
      const WBController: TWBController);
  public
    ///  <summary>Loads an HTML representation of a view into a web browser
    ///  control.</summary>
    ///  <param name="View">IView [in] View to be displayed.</param>
    ///  <param name="WBController">TWBController [in] Controller object for
    ///  an associated web browser control.</param>
    class procedure LoadPage(View: IView; const WBController: TWBController);
  end;


implementation


{ TDetailPageLoader }

class procedure TDetailPageLoader.DisplayHTML(const Generator: TDetailPageHTML;
  const WBController: TWBController);
var
  HTML: string; // HTML to be displayed
begin
  HTML := Generator.Generate;
  WBController.IOMgr.ReplaceExistingBodyHTML(HTML);
end;

class procedure TDetailPageLoader.InitBrowser(
  const WBController: TWBController);
begin
  if not WBController.IOMgr.HTMLDocumentExists then
    WBController.IOMgr.NavigateToResource(HInstance, 'detail.html');
end;

class procedure TDetailPageLoader.LoadPage(View: IView;
  const WBController: TWBController);
var
  Generator: TDetailPageHTML; // object used to generate body's inner HTML
begin
  Assert(Assigned(View), ClassName + '.LoadPage: View is nil');
  Assert(Assigned(WBController), ClassName + '.LoadPage: WBController is nil');
  InitBrowser(WBController);
  Generator := TDetailPageHTMLFactory.CreateGenerator(View);
  try
    DisplayHTML(Generator, WBController);
  finally
    Generator.Free;
  end;
end;

end.

