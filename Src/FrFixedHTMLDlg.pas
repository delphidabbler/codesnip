{
 * FrFixedHTMLDlg.pas
 *
 * Frame containing a web browser control that displays content loaded from an
 * HTML resource.
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
 * The Original Code is FrFixedHTMLDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FrFixedHTMLDlg;


interface


uses
  // Delphi
  OleCtrls, SHDocVw, Classes, Controls, ExtCtrls,
  // Project
  FrHTMLDlg;


type

  {
  TFixedHTMLDlgFrame:
    Frame containing a web browser control that displays content loaded from an
    HTML resource.
  }
  TFixedHTMLDlgFrame = class(THTMLDlgFrame)
  public
    procedure Initialise(const ResName: string);
      {Initialises display by loading HTML resource.
        @param ResName [in] Name of RT_HTML resource containing HTML to display.
      }
  end;


implementation


{$R *.dfm}


{ TFixedHTMLDlgFrame }

procedure TFixedHTMLDlgFrame.Initialise(const ResName: string);
{Initialises display by loading HTML resource.
  @param ResName [in] Name of RT_HTML resource containing HTML to display.
}
begin
  WBController.IOMgr.NavigateToResource(HInstance, PChar(ResName));
end;

end.

