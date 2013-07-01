{
 * FmHTMLViewDlg.pas
 *
 * Implements abstract base class for HTML view dialogs that need to size
 * themselves to their HTML content.
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
 * The Original Code is FmHTMLViewDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmHTMLViewDlg;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericViewDlg;


type

  {
  THTMLViewDlg:
    Abstract base class for HTML view dialogs that need to size themselves to
    their HTML content.
  }
  THTMLViewDlg = class(TGenericViewDlg)
  protected
    procedure InitHTMLFrame; virtual; abstract;
      {Initialises HTML frame by loading required HTML in browser control.
      }
    procedure ConfigForm; override;
      {Initialises dialog's HTML content.
      }
  end;


implementation


{$R *.dfm}


{ THTMLViewDlg }

procedure THTMLViewDlg.ConfigForm;
  {Initialises dialog's HTML content.
  }
begin
  inherited;
  InitHTMLFrame;
end;

end.

