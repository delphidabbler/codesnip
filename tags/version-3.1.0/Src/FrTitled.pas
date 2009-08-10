{
 * FrTitled.pas
 *
 * Base class for frames that display a title bar.
 *
 * v0.1 of 30 Jan 2005  - Original version.
 * v0.2 of 18 Feb 2005  - Deleted unused units from uses clause.
 * v1.0 of 24 May 2006  - Made minor commenting changes.
 * v1.1 of 10 Jul 2009  - Made title text use OS' default font.
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
 * The Original Code is FrTitled.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
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
  TFontHelper.SetDefaultBaseFont(lblTitle.Font, False);
end;

end.

