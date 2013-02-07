{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a bug report dialogue box that is displayed when unexpected
 * exceptions are detected.
}


unit FmTrappedBugReportDlg;


interface


uses
  // Delphi
  SysUtils, Classes, ActnList, ExtActns, StdCtrls, Controls, ExtCtrls,
  // Project
  FmBugReportBaseDlg, UBaseObjects;


type

  {
  TTrappedBugReportDlg:
    Bug report dialog box for display when unexpected exceptions are detected.
  }
  TTrappedBugReportDlg = class(TBugReportBaseDlg, INoPublicConstruct)
    bvlBugDesc: TBevel;
    lblBugInfo: TLabel;
    lblInstruct1: TLabel;
    lblInstruct2: TLabel;
    lblInstruct3: TLabel;
    lblInstruct4: TLabel;
    lblIntro: TLabel;
    btnTerminate: TButton;
    actTerminate: TAction;
    procedure actTerminateExecute(Sender: TObject);
  strict private
    fErrorObj: Exception; // Reference to exception being reported.
    procedure CopyBugInfoToClipboard;
      {Places information about the exception, the OS and the program version on
      the clipboard ready for pasting into the bug report.
      }
  strict protected
    procedure ConfigForm; override;
      {Configures form. Ensures correct font is used in labels and displays
      details of exception.
      }
    procedure ArrangeForm; override;
      {Aligns and sizes of controls depending on text sizes.
      }
    procedure GoToTracker; override;
      {Displays online bug tracker after placing info about bug on clipboard.
      }
  public
    class procedure Execute(Owner: TComponent; const ErrorObj: Exception);
      {Creates and displays bug report dialog box.
        @param Owner [in] Component that owns dialog box. Dialog box is aligned
          over this component if it is a form. If Owner it is nil or not a form
          the dialog is aligned over the active form.
        @param ErrorObj [in] Exception that caused dialog box to be displayed.
      }
  end;


implementation


uses
  // Delphi
  Clipbrd, Forms, Windows,
  // Project
  UAppInfo, UConsts, UCtrlArranger, UFontHelper, UMessageBox, USystemInfo;

{$R *.dfm}


{ TTrappedBugReportDlg }

procedure TTrappedBugReportDlg.actTerminateExecute(Sender: TObject);
  {Terminates application only if user confirms.
    @param Sender [in] Not used.
  }
resourcestring
  // Text for custom confirmation dialog box
  sTitle = 'Confirm';
  sConfirmMsg = 'Please confirm that you want to terminate the application.'
    + EOL2
    + 'CodeSnip will attempt to save any unsaved changes to the database.';
  sOKText = 'Terminate';
  sCancelText = 'Cancel';
begin
  if TMessageBox.Custom(
    Self,
    sConfirmMsg,
    [
      TMessageBoxButton.Create(sOKText, mrOK),
      TMessageBoxButton.Create(sCancelText, mrCancel, True, True)
    ],
    sTitle,
    IDI_QUESTION
  ) = mrOK then
    Application.Terminate;
end;

procedure TTrappedBugReportDlg.ArrangeForm;
  {Aligns and sizes of controls depending on text sizes.
  }
begin
  TCtrlArranger.SetLabelHeights(Self);
  bvlBugDesc.Height := lblBugInfo.Height + 12;
  bvlBugDesc.Top := TCtrlArranger.BottomOf(lblIntro, 8);
  lblBugInfo.Top := bvlBugDesc.Top + 6;
  lblInstruct1.Top := TCtrlArranger.BottomOf(bvlBugDesc, 8);
  lblInstruct2.Top := TCtrlArranger.BottomOf(lblInstruct1, 6);
  lblInstruct3.Top := TCtrlArranger.BottomOf(lblInstruct2, 6);
  lblBugTracker.Top := TCtrlArranger.BottomOf(lblInstruct3, 6);
  lblInstruct4.Top := TCtrlArranger.BottomOf(lblBugTracker, 12);
  inherited;
  btnTerminate.Top := btnClose.Top;
  TCtrlArranger.MoveToLeftOf(btnClose, btnTerminate, 4);
end;

procedure TTrappedBugReportDlg.ConfigForm;
  {Configures form. Ensures correct font is used in labels and displays details
  of exception.
  }
begin
  inherited;
  // set required label fonts
  TFontHelper.SetDefaultBaseFont(lblIntro.Font);
  TFontHelper.SetDefaultBaseFont(lblBugInfo.Font);
  TFontHelper.SetDefaultBaseFont(btnTerminate.Font);
  // display the exception's message
  lblBugInfo.Caption := fErrorObj.Message;
end;

procedure TTrappedBugReportDlg.CopyBugInfoToClipboard;
  {Places information about the exception, the OS and the program version on the
  clipboard ready for pasting into the bug report.
  }
const
  // Information template
  cBugInfo = '----- BEGIN BUG INFO -----' + EOL
    + 'Version = %0:s' + EOL
    + 'OS = %1:s' + EOL
    + 'Exception = %2:s' + EOL
    + '----- END BUG INFO -----' + EOL2;
begin
  Clipboard.AsText := Format(
    cBugInfo,
    [TAppInfo.ProgramFileVersion, TOSInfo.Description, fErrorObj.Message]
  );
end;

class procedure TTrappedBugReportDlg.Execute(Owner: TComponent;
  const ErrorObj: Exception);
  {Creates and displays bug report dialog box.
    @param Owner [in] Component that owns dialog box. Dialog box is aligned
      over this component if it is a form. If Owner it is nil or not a form the
      dialog is aligned over the active form.
    @param ErrorObj [in] Exception that caused dialog box to be displayed.
  }
begin
  Assert(Assigned(ErrorObj), ClassName + '.Execute: ErrorObj is nil');
  with InternalCreate(Owner) do
    try
      fErrorObj := ErrorObj;
      ShowModal;
    finally
      Free;
    end;
end;

procedure TTrappedBugReportDlg.GoToTracker;
  {Displays online bug tracker after placing info about bug on clipboard.
  }
begin
  CopyBugInfoToClipboard;
  inherited;  // displays tracker
end;

end.

