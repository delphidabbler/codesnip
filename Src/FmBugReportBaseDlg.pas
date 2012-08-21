{
 * FmBugReportBaseDlg.pas
 *
 * Provides a base class and common functionality for bug report dialog boxes.
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
 * The Original Code is FmBugReportBaseDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmBugReportBaseDlg;


interface


uses
  // Delphi
  Classes, ActnList, StdCtrls, Controls, ExtCtrls,
  // Project
  FmGenericViewDlg;


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
    procedure ConfigForm; override;
      {Sets up label fonts.
      }
    procedure ArrangeForm; override;
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
  UColours, UCtrlArranger, UFontHelper, Web.UInfo;


{$R *.dfm}

{ TBugReportBaseDlg }

procedure TBugReportBaseDlg.actBugTrackerExecute(Sender: TObject);
  {Displays online bug tracker. This action handles hotkey for link label.
    @param Sender [in] Not used.
  }
begin
  GoToTracker;
end;

procedure TBugReportBaseDlg.ArrangeForm;
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

procedure TBugReportBaseDlg.ConfigForm;
  {Sets up label fonts.
  }
begin
  inherited;
  lblBugTracker.Font.Color := clLinkText;
  TFontHelper.SetDefaultBaseFont(lblBugTracker.Font, False);
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
      URL := TWebInfo.BugTrackerURL;
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
