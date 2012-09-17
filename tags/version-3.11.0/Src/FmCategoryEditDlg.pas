{
 * FmCategoryEditDlg.pas
 *
 * Implements an abstract base class for dialog boxes used to edit user-defined
 * categories.
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
 * The Original Code is FmCategoryEditDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmCategoryEditDlg;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericOKDlg;


type

  {
  TCategoryEditDlg:
    Abstract base class for dialog boxes used to edit user-defined categories.
  }
  TCategoryEditDlg = class(TGenericOKDlg)
  strict protected
    procedure ArrangeForm; override;
      {Sizes dialog box to fit its controls. Sub-classes should layout and size
      controls before calling inherited.
      }
    procedure InitForm; override; final;
      {Initialises form and state of OK button. Sub-classes must override
      InitControls instead of this method.
      }
    procedure InitControls; virtual;
      {Method for overriding by subclasses to initialise form's controls. Does
      nothing in this implementation.
      }
    procedure UpdateOKBtn; virtual; abstract;
      {Updates state of OK button depending on entries in dialog box.
      }
  end;


implementation


uses
  // Project
  UCtrlArranger;

{$R *.dfm}


{ TCategoryEditDlg }

procedure TCategoryEditDlg.ArrangeForm;
  {Sizes dialog box to fit its controls. Sub-classes should layout and size
  controls before calling inherited.
  }
begin
  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody);
  pnlBody.ClientWidth := TCtrlArranger.TotalControlWidth(pnlBody);
  inherited;
end;

procedure TCategoryEditDlg.InitControls;
  {Method for overriding by subclasses to initialise form's controls. Does
  nothing in this implementation.
  }
begin
  // Do nothing
end;

procedure TCategoryEditDlg.InitForm;
  {Initialises form and state of OK button. Sub-classes must override
  InitControls instead of this method.
  }
begin
  inherited;
  InitControls;
  UpdateOKBtn;
end;

end.

