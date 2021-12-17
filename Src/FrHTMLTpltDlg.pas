{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a frame containing a web browser control that displays HTML
 * content generated from a template that takes on the appearance of a dialogue
 * box.
}


unit FrHTMLTpltDlg;


interface


uses
  // Delphi
  OleCtrls, SHDocVw, Classes, Controls, ExtCtrls,
  // Project
  FrHTMLDlg, UHTMLTemplate;


type
  ///  <summary>
  ///  Frame containing a web browser control that displays content generated
  ///  from a HTML template. By default the frame takes on the appearance of a
  ///  dialog box.
  ///  </summary>
  THTMLTpltDlgFrame = class(THTMLDlgFrame)
  public
    type
      ///  <summary>Type of anonymous method called to resolve placeholders.
      ///  </summary>
      ///  <param name="Tplt">THTMLTemplate [in] Template object to be used to
      ///  resolve placeholders.</param>
      TResolver = reference to procedure(Tplt: THTMLTemplate);
  public
    ///  <summary>Initialises frame. Loads HTML into browser control from a
    ///  template file after resolving placeholders.
    ///  </summary>
    ///  <param name="ResName">string [in] Name of HTML resource containing
    ///  template file.</param>
    ///  <param name="Resolver">TResolver [in] Anonymous method that is called
    ///  to resolve placeholders.</param>
    procedure Initialise(const ResName: string; Resolver: TResolver);
  end;


implementation


{$R *.dfm}


{ THTMLTpltDlgFrame }

procedure THTMLTpltDlgFrame.Initialise(const ResName: string;
  Resolver: TResolver);
var
  Tplt: THTMLTemplate;  // object used to resolve placeholders
begin
  Tplt := THTMLTemplate.Create(HInstance, ResName);
  try
    Resolver(Tplt); // user provides method to resolve placeholders
    WBController.IOMgr.LoadFromString(Tplt.HTML);
  finally
    Tplt.Free;
  end;
end;

end.

