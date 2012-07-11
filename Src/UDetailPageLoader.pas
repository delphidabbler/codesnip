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
 * Portions created by the Initial Developer are Copyright (C) 2005-2012 Peter
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
  Browser.UController, UDetailPageHTML, UView;


type
  ///  <summary>Generates and displays an HTML page that represents a view.
  ///  </summary>
  ///  <remarks>Used to display views in main window's detail pane.</remarks>
  TDetailPageLoader = class(TObject)
  strict private
    var
      ///  <summary>Web browser control host.</summary>
      fWBController: TWBController;
    ///  <summary>Loads a blank document into a browser control if no document
    ///  already exists or if forced.</summary>
    ///  <param name="Reload">Boolean [in] Flag indicating whether HTML document
    ///  is to be forcibly reloaded (True) or only loading if a document does
    ///  not yet exist (False).</param>
    ///  <remarks>Loading a document imports all CSS and JavaScript required for
    ///  detail pane views.</remarks>
    procedure InitBrowser(const Reload: Boolean);
    ///  <summary>Generates and displays a view in a browser control.</summary>
    ///  <param name="Generator">TDetailPageHTML [in] Object that generates
    ///  HTML to be displayed.</param>
    ///  <remarks>Generated HTML replaces current body of document currently
    ///  loaded in web browser.</remarks>
    procedure DisplayHTML(const Generator: TDetailPageHTML);
  public
    ///  <summary>Constructs object for use with given browser host/controller.
    ///  </summary>
    constructor Create(WBController: TWBController);
    ///  <summary>Loads an HTML representation of a view into a web browser
    ///  control.</summary>
    ///  <param name="View">IView [in] View to be displayed.</param>
    ///  <param name="Reload">Boolean [in] Flag indicating whether HTML document
    ///  is to be reloaded (True) or any existing document is to be re-used
    ///  (False).</param>
    procedure LoadPage(View: IView; const Reload: Boolean);
  end;


implementation


{ TDetailPageLoader }

constructor TDetailPageLoader.Create(WBController: TWBController);
begin
  Assert(Assigned(WBController), ClassName + '.Create: WBController is nil');
  inherited Create;
  fWBController := WBController;
end;

procedure TDetailPageLoader.DisplayHTML(const Generator: TDetailPageHTML);
var
  HTML: string; // HTML to be displayed
begin
  HTML := Generator.Generate;
  fWBController.IOMgr.ReplaceExistingBodyHTML(HTML);
end;

procedure TDetailPageLoader.InitBrowser(const Reload: Boolean);
begin
  if Reload or not fWBController.IOMgr.HTMLDocumentExists then
    fWBController.IOMgr.NavigateToResource(HInstance, 'detail.html');
end;

procedure TDetailPageLoader.LoadPage(View: IView; const Reload: Boolean);
var
  Generator: TDetailPageHTML; // object used to generate body's inner HTML
begin
  Assert(Assigned(View), ClassName + '.LoadPage: View is nil');
  InitBrowser(Reload);
  Generator := TDetailPageHTMLFactory.CreateGenerator(View);
  try
    DisplayHTML(Generator);
  finally
    Generator.Free;
  end;
end;

end.

