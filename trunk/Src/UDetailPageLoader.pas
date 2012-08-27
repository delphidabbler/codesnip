{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Heirachy of classes that can produce and load HTML pages displayed in the
 * detail panes. Also provides a factory class to create the loader objects.
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

