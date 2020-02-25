{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements an abstract base class for dialogue boxes used to edit user
 * defined categories.
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

