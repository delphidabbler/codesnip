{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2021, Peter Johnson (gravatar.com/delphidabbler).
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
    ///  <summary>Generates and displays a view in a browser control.</summary>
    ///  <param name="Generator">TDetailPageHTML [in] Object that generates
    ///  HTML to be displayed.</param>
    procedure DisplayHTML(const Generator: TDetailPageHTML);
  public
    ///  <summary>Constructs object for use with given browser host/controller.
    ///  </summary>
    constructor Create(WBController: TWBController);
    ///  <summary>Loads an HTML representation of a view into a web browser
    ///  control.</summary>
    ///  <param name="View">IView [in] View to be displayed.</param>
    procedure LoadPage(View: IView);
  end;


implementation


uses
  // Project
  UConsts, UEncodings, UHTMLTemplate, UResourceUtils, USystemInfo;


{ TDetailPageLoader }

constructor TDetailPageLoader.Create(WBController: TWBController);
begin
  Assert(Assigned(WBController), ClassName + '.Create: WBController is nil');
  inherited Create;
  fWBController := WBController;
end;

procedure TDetailPageLoader.DisplayHTML(const Generator: TDetailPageHTML);
begin
  fWBController.IOMgr.LoadFromString(
    Generator.Generate
  );
end;

procedure TDetailPageLoader.LoadPage(View: IView);
var
  Generator: TDetailPageHTML; // object used to generate body's inner HTML
begin
  Assert(Assigned(View), ClassName + '.LoadPage: View is nil');
  Generator := TDetailPageHTMLFactory.CreateGenerator(View);
  try
    DisplayHTML(Generator);
  finally
    Generator.Free;
  end;
end;

end.

