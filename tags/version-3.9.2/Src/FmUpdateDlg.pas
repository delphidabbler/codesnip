{
 * FmUpdateDlg.pas
 *
 * Dialog box that updates the CodeSnip database from web.
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
 * The Original Code is FmUpdateDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmUpdateDlg;


interface


uses
  // Project
  Forms, StdCtrls, Controls, ExtCtrls, Classes, Messages,
  // Delphi
  FmGenericViewDlg, UBaseObjects, UMemoProgBarMgr, UUpdateMgr;



type

  {
  TUpdateHeadlineStyle:
    Enumeration that specifies display style of "headline" text in dialog.
  }
  TUpdateHeadlineStyle = (
    hsNormal,
    hsCancelled,
    hsError
  );

  {
  TUpdateDlg:
    Implements dialog box that updates database from web.
  }
  TUpdateDlg = class(TGenericViewDlg, INoPublicConstruct)
    btnCancel: TButton;
    btnDoUpdate: TButton;
    lblUpdateFromWeb: TLabel;
    lblError: TLabel;
    edProgress: TMemo;
    lblHeadline: TLabel;
    btnNews: TButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnDoUpdateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure btnNewsClick(Sender: TObject);
  private
    fProgressBarMgr: TMemoProgBarMgr; // Displays progress bar in progress memo
    fDataUpdated: Boolean;            // Flag true if any data was updated
    fCancelled: Boolean;              // Flag true if user cancelled update
    fUpdateMgr: TUpdateMgr;           // Handles updating from web
    procedure WMActivateApp(var Msg: TMessage); message WM_ACTIVATEAPP;
      {Responds to activation of application and this window. Refreshes display.
        @param Msg [in/out] Not used.
      }
    function GetDataDir: string;
      {Returns directory where data files stored and ensures it exists.
        @return Data directory.
      }
    procedure UpdateStatusHandler(Sender: TObject; Status: TUpdateStatus;
      var Cancel: Boolean);
      {Event handler called by update manager to report progress and permit user
      to cancel update.
        @param Sender [in] Not used.
        @param Status [in] Current status of update manager.
        @param Cancel [in/out] Flag that cancels update when set true.
      }
    procedure DownloadProgressHandler(Sender: TObject; const BytesReceived,
      BytesExpected: Int64; var Cancel: Boolean);
      {OnProgress event handler for update manager object. Displays download
      progress using a progress bar.
        @param Sender [in] Not used.
        @param BytesReceived [in] Number of bytes downloaded to date.
        @param BytesExpected [in] Number of bytes expected in download.
        @param Cancel [in/out] Set to true to cancel update. (This does not
          cancel a download).
      }
    procedure ProgressMsg(const Msg: string);
      {Writes a message to progress control.
        @param Msg [in] Message to be written.
      }
    procedure HeadlineMsg(const Msg: string;
      const Kind: TUpdateHeadlineStyle = hsNormal);
      {Displays messages in headline label. Display state depends on kind of
      headline display required.
        @param Msg [in] Message to be displayed
        @param Kind [in] Style of message to be displayed.
      }
  protected
    procedure ArrangeForm; override;
      {Positions additional controls added to inherited form.
      }
    procedure InitForm; override;
      {Initialises controls.
      }
  public
    class function Execute(AOwner: TComponent): Boolean;
      {Displays dialog box and returns whether updated files were downloaded.
      true if files were updated and false if not.
        @param AOwner [in] Component that owns dialog box (and aligns it if a
          form).
        @return True if files were actually downloaded or false if no update
          needed, an error occurred or if user cancelled.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  FmNewsDlg, UAppInfo, UColours, UConsts, UCtrlArranger, UUtils;


{$R *.dfm}

resourcestring
  // Progress report messages
  sLoggingOn        = 'Logging on to web server';
  sCheckForUpdates  = 'Checking for updates';
  sDownloading      = 'Downloading database';
  sDownloaded       = 'Database downloaded';
  sUpdating         = 'Updating local database';
  sUpToDate         = 'Database is up to date';
  sLoggingOff       = 'Logging off';
  sCompleted        = 'Update completed';
  sCancelled        = 'Update cancelled';
  sCancelling       = 'Cancelling...';
  // Dialog box messages
  sRunning          = 'Performing update';
  sUpdtSuccess      = 'Files updated successfully';
  sUpdtUpToDate     = 'Database is up to date';
  sUpdtCancelled    = 'Update cancelled';
  sUpdtError        = '%0:s:';

  // Detailed error message
  sErrorDetail      = 'Full details of "%0:s" error message are:'
                      + EOL2 + '%1:s';


{ TDownloadDlg }

procedure TUpdateDlg.ArrangeForm;
  {Positions additional controls added to inherited form.
  }
begin
  // Arrange inherited controls
  inherited;
  TCtrlArranger.SetLabelHeights(Self);
  // Controls in initial display
  btnDoUpdate.Top := TCtrlArranger.BottomOf(lblUpdateFromWeb, 16);
  // Arrange additonal cancel button
  btnCancel.Left := btnClose.Left + btnClose.Width - btnCancel.Width;
  btnCancel.Top := btnClose.Top;
  // Arrange "latest news" button
  btnNews.Left := 8;
  btnNews.Top := btnClose.Top;
  // Align error label
  lblError.Left := (pnlBody.Width - lblError.Width) div 2;
end;

procedure TUpdateDlg.btnCancelClick(Sender: TObject);
  {Cancels database update.
    @param Sender [in] Not used.
  }
begin
  inherited;
  // Sets cancelled flag checked during download
  fCancelled := True;
  ProgressMsg(sCancelling);
end;

procedure TUpdateDlg.btnDoUpdateClick(Sender: TObject);
  {Update button clicked. Updates database from web.
    @param Sender [in] Not used.
  }
begin
  inherited;
  // Create update manager
  fUpdateMgr := TUpdateMgr.Create(GetDataDir);
  try
    fUpdateMgr.OnStatus := UpdateStatusHandler;
    fUpdateMgr.OnDownloadProgress := DownloadProgressHandler;
    // Update control visibility
    lblUpdateFromWeb.Visible := False;
    lblError.Visible := False;
    btnDoUpdate.Visible := False;
    btnClose.Visible := False;
    btnCancel.Visible := True;
    edProgress.Visible := True;
    HeadlineMsg(sRunning);
    // Reset download flag to allow download to continue. Clicking cancel button
    // sets this flag true
    fCancelled := False;
    // Perform the download and display message depending on result
    fDataUpdated := False;
    case fUpdateMgr.Execute of
      urUpdated:
      begin
        // connection to web succeeded and files were updated
        HeadlineMsg(sUpdtSuccess);
        fDataUpdated := True;
      end;
      urNoUpdate:
      begin
        // connection to web succeeded but no files were updated
        HeadlineMsg(sUpdtUpToDate);
      end;
      urCancelled:
      begin
        // user cancelled update
        HeadlineMsg(sUpdtCancelled, hsCancelled);
      end;
      urError:
      begin
        // an error occured during update
        fProgressBarMgr.Hide;
        ProgressMsg('');
        ProgressMsg(fUpdateMgr.LongError);
        HeadlineMsg(Format(sUpdtError, [fUpdateMgr.ShortError]), hsError);
      end;
    end;
  finally
    FreeAndNil(fUpdateMgr);
    // Reset cancelled flag
    fCancelled := False;
    // Hide cancel button and show close button
    btnCancel.Visible := False;
    btnClose.Visible := True;
  end;
end;

procedure TUpdateDlg.btnNewsClick(Sender: TObject);
  {Displays latest CodeSnip news in dialog box.
    @param Sender [in] Not used.
  }
begin
  TNewsDlg.Execute(Self);
end;

procedure TUpdateDlg.DownloadProgressHandler(Sender: TObject;
  const BytesReceived, BytesExpected: Int64; var Cancel: Boolean);
  {OnProgress event handler for update manager object. Displays download
  progress using a progress bar.
    @param Sender [in] Not used.
    @param BytesReceived [in] Number of bytes downloaded to date.
    @param BytesExpected [in] Number of bytes expected in download.
    @param Cancel [in/out] Set to true to cancel update. (This does not
      cancel a download).
  }
begin
  if BytesReceived = 0 then
    fProgressBarMgr.Max := BytesExpected;
  fProgressBarMgr.Position := BytesReceived;
  Cancel := fCancelled;
  Application.ProcessMessages;
end;

class function TUpdateDlg.Execute(AOwner: TComponent): Boolean;
  {Displays dialog box and returns whether updated files were downloaded.
  true if files were updated and false if not.
    @param AOwner [in] Component that owns dialog box (and aligns it if a form).
    @return True if files were actually downloaded or false if no update needed,
      an error occurred or if user cancelled.
  }
begin
  with InternalCreate(AOwner) do
    try
      ShowModal;
      Result := fDataUpdated;
    finally
      Free;
    end;
end;

procedure TUpdateDlg.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
  {Checks if form can close following click on Close or system menu button.
  Prevents closure while downloading.
    @param Sender [in] Not used.
    @param CanClose [in/out] Flag to be set false to prevent dialog closing.
  }
begin
  inherited;
  if btnClose.Visible then
    CanClose := True
  else
  begin
    CanClose := False;
    fCancelled := True;
  end;
end;

procedure TUpdateDlg.FormCreate(Sender: TObject);
  {Creates owned objects and initialises flags when form is created.
    @param Sender [in] Not used.
  }
begin
  inherited;
  // Record that no update yet taken place
  fDataUpdated := False;
  // Object that handles location and display of progress bar
  fProgressBarMgr := TMemoProgBarMgr.Create(edProgress);
end;

procedure TUpdateDlg.FormDestroy(Sender: TObject);
  {Form destruction event handler. Destroys owned objects.
    @param Sender [in] Not used.
  }
begin
  inherited;
  FreeAndNil(fProgressBarMgr);
end;

function TUpdateDlg.GetDataDir: string;
  {Returns directory where data files stored and ensures it exists.
    @return Data directory.
  }
begin
  Result := TAppInfo.AppDataDir;
  EnsureFolders(Result);
end;

procedure TUpdateDlg.HeadlineMsg(const Msg: string;
  const Kind: TUpdateHeadlineStyle);
  {Displays messages in headline label. Display state depends on kind of
  headline display required.
    @param Msg [in] Message to be displayed
    @param Kind [in] Style of message to be displayed.
  }
begin
  // Display message
  lblHeadline.Caption := Msg;
  lblHeadline.Visible := True;
  // Determine appearance
  case Kind of
    hsNormal:
    begin
      // Normal message: standard colours
      lblHeadline.ParentFont := True;
      lblError.Visible := False;
    end;
    hsCancelled:
    begin
      // Cancel message: show in warning text colour
      lblHeadline.Font.Color := clWarningText;
      lblError.Visible := False;
    end;
    hsError:
    begin
      // Error message: show in warning text colour followed by extra error
      // info. Make sure headline ends in one space to separate headline from
      // error message
      lblHeadline.Caption := TrimRight(lblHeadline.Caption) + ' ';
      lblHeadline.Font.Color := clWarningText;
      TCtrlArranger.MoveToRightOf(lblHeadline, lblError);
      lblError.Top := lblHeadline.Top;
      lblError.Visible := True;
    end;
  end;
end;

procedure TUpdateDlg.InitForm;
  {Initialises controls.
  }
begin
  inherited;
  // Hide Cancel and show Close buttons
  btnCancel.Visible := False;
  btnClose.Visible := True;
end;

procedure TUpdateDlg.ProgressMsg(const Msg: string);
  {Writes a message to progress control.
    @param Msg [in] Message to be written.
  }
begin
  edProgress.Lines.Add(Msg);
end;

procedure TUpdateDlg.UpdateStatusHandler(Sender: TObject;
  Status: TUpdateStatus; var Cancel: Boolean);
  {Event handler called by update manager to report progress and permit user
  to cancel update.
    @param Sender [in] Not used.
    @param Status [in] Current status of update manager.
    @param Cancel [in/out] Flag that cancels update when set true.
  }
begin
  // Update UI according to status
  case Status of
    usLogOn:
      ProgressMsg(sLoggingOn);
    usCheckForUpdates:
      ProgressMsg(sCheckForUpdates);
    usDownloadStart:
    begin
      ProgressMsg(sDownloading + '  ');
      fProgressBarMgr.Show(edProgress.Lines.Count - 1);
    end;
    usDownloadEnd:
    begin
      fProgressBarMgr.Hide;
      ProgressMsg(sDownloaded);
    end;
    usUpdating:
      ProgressMsg(sUpdating);
    usNoUpdate:
      ProgressMsg(sUpToDate);
    usLogOff:
      ProgressMsg(sLoggingOff);
    usCompleted:
      ProgressMsg(sCompleted);
    usCancelled:
      ProgressMsg(sCancelled);
  end;
  // Application needs to process messages to allow any UI updates
  Application.ProcessMessages;
  // Halt update if user cancelled
  Cancel := fCancelled;
end;

procedure TUpdateDlg.WMActivateApp(var Msg: TMessage);
  {Responds to activation of application and this window. Refreshes display.
    @param Msg [in/out] Not used.
  }
begin
  // This is needed since hiding window and switching back causes some of frame
  // controls to be hidden. Also sometimes the controls are not displayed
  // correctly when dialog is first displayed.
  Refresh;
end;

end.

