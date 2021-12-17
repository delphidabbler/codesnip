{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a base class for frames that display a title bar.
}


unit FrTitled;


interface


uses
  // Delphi
  ExtCtrls, Controls, StdCtrls, Classes, Forms;


type

  {
  TTitledFrame:
    Base class for frames that display a title bar.
  }
  TTitledFrame = class(TFrame)
    pnlTitle: TPanel;
    lblTitle: TLabel;
    bvlTop: TBevel;
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets up frame to use OS' default font for title.
        @param AOwner [in] Component that owns the frame.
      }
  end;


implementation


uses
  // Project
  UFontHelper;


{$R *.dfm}


{ TTitledFrame }

constructor TTitledFrame.Create(AOwner: TComponent);
  {Class constructor. Sets up frame to use OS' default font for title.
    @param AOwner [in] Component that owns the frame.
  }
begin
  inherited;
  TFontHelper.SetDefaultBaseFont(lblTitle.Font);
end;

end.

