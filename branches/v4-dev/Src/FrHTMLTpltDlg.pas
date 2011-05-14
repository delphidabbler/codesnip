{
 * FrHTMLTpltDlg.pas
 *
 * Frame containing a web browser control that displays HTML content generated
 * from a template that also takes on the appearance of a dialog box.
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
 * The Original Code is FrHTMLTpltDlg.pas
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

