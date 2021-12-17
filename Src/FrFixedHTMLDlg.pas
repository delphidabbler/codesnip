{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a frame containing a web browser control that displays content
 * loaded from an HTML resource.
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

