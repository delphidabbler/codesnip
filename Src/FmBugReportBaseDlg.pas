{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Provides a base class and common functionality for bug report dialogue boxes.
}


unit FmBugReportBaseDlg;


interface


uses
  // Delphi
  Classes, ActnList, StdCtrls, Controls, ExtCtrls,
  // Project
  FmGenericViewDlg, System.Actions;


type
  {
  TBugReportBaseDlg:
    Base class providing common functionality bug report dialog boxes.
  }
  TBugReportBaseDlg = class(TGenericViewDlg)
    actBugTracker: TAction;
    alMain: TActionList;
    lblBugTracker: TLabel;
    lblBugTrackerKey: TLabel;
    procedure actBugTrackerExecute(Sender: TObject);
    procedure lblBugTrackerClick(Sender: TObject);
  strict protected
    procedure CustomiseControls; override;
      {Sets up label fonts.
      }
    procedure ArrangeControls; override;
      {Arranges controls and sizes form to accommodate all controls.
      }
    procedure GoToTracker; virtual;
      {Displays online bug tracker. Descendants should override to add extra
      functionality.
      }
  end;


implementation


uses
  // Delphi
  ExtActns,
  // Project
  UColours, UCtrlArranger, UFontHelper, UUrl;


{$R *.dfm}

{ TBugReportBaseDlg }

procedure TBugReportBaseDlg.actBugTrackerExecute(Sender: TObject);
  {Displays online bug tracker. This action handles hotkey for link label.
    @param Sender [in] Not used.
  }
begin
  GoToTracker;
end;

procedure TBugReportBaseDlg.ArrangeControls;
  {Arranges controls and sizes form to accommodate all controls.
  }
begin
  TCtrlArranger.AlignVCentres(
    lblBugTracker.Top, [lblBugTracker, lblBugTrackerKey]
  );
  TCtrlArranger.MoveToRightOf(lblBugTracker, lblBugTrackerKey, 6);
  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody) + 4;
  inherited;
end;

procedure TBugReportBaseDlg.CustomiseControls;
  {Sets up label fonts.
  }
begin
  inherited;
  lblBugTracker.Font.Color := clExternalLink;
  TFontHelper.SetDefaultBaseFont(lblBugTracker.Font);
end;

procedure TBugReportBaseDlg.GoToTracker;
  {Displays online bug tracker. Descendants should override to add extra
  functionality.
  }
begin
  // NOTE: Don't change actBugTracker to TBrowseURL and delete this. Subclasses
  // must be able to override this method.
  with TBrowseURL.Create(nil) do
    try
      URL := TURL.CodeSnipBugTracker;
      Execute;
    finally
      Free;
    end;
end;

procedure TBugReportBaseDlg.lblBugTrackerClick(Sender: TObject);
  {Click event handler for bug tracker link label. Displays online bug tracker.
    @param Sender [in] Not used.
  }
begin
  actBugTracker.Execute;
end;

end.
