{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a dialogue box that updates the CodeSnip database from web.
}


unit FmDBUpdateDlg;


interface


uses
  // Project
  Forms, StdCtrls, Controls, ExtCtrls, Classes, Messages,
  // Delphi
  FmGenericViewDlg, UBaseObjects, UDBUpdateMgr, UMemoProgBarMgr;



type
  ///  <summary>Dialogue box that checks to see if an update to the local copy
  ///  of the online Code Snippets database is available and downloads it if so.
  ///  </summary>
  TDBUpdateDlg = class(TGenericViewDlg, INoPublicConstruct)
    btnCancel: TButton;
    btnDoUpdate: TButton;
    lblUpdateFromWeb: TLabel;
    lblError: TLabel;
    edProgress: TMemo;
    lblHeadline: TLabel;
    ///  <summary>Handles click on cancel button and cancels the update.
    ///  </summary>
    procedure btnCancelClick(Sender: TObject);
    ///  <summary>Handles click on update button and performs update process.
    ///  </summary>
    ///  <remarks>Database is only updated if updates are available.</remarks>
    procedure btnDoUpdateClick(Sender: TObject);
    ///  <summary>Performs required initialisation on form creation.</summary>
    procedure FormCreate(Sender: TObject);
    ///  <summary>Checks if form can close in response to user request.
    ///  </summary>
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    ///  <summary>Tidies up on form destruction.</summary>
    procedure FormDestroy(Sender: TObject);
  strict private
    type
      ///  <summary>Enumeration that specifies display style of 'headline' text
      ///  in dialogue box.</summary>
      THeadlineStyle = (hsNormal, hsCancelled, hsError);
  strict private
    var
      ///  <summary>Manages display of progress bar in prgress memo control.
      ///  </summary>
      fProgressBarMgr: TMemoProgBarMgr;
      ///  <summary>Flag that indicates if local database was updated.</summary>
      fDataUpdated: Boolean;
      ///  <summary>Flag that indicates if user cancelled update.</summary>
      fCancelled: Boolean;
      ///  <summary>Object manages download and update process.</summary>
      fUpdateMgr: TDBUpdateMgr;

    ///  <summary>Handles activation of application by refreshing the display.
    ///  </summary>
    ///  <remarks>This is necessary since hiding the dialogue window by
    ///  switching to another appliction then switching back causes some of the
    ///  controls to be hidden. Additionally, some controls are not always
    ///  displayed correctly when dialogue box is first displayed.</remarks>
    procedure WMActivateApp(var Msg: TMessage); message WM_ACTIVATEAPP;

    ///  <summary>Returns the directory containing the local database files.
    ///  </summary>
    function GetDataDir: string;

    ///  <summary>Handles events triggered by the update manager object to
    ///  report current status.</summary>
    ///  <param name="Sender">TObject [in] Object triggering event.</param>
    ///  <param name="Status">TDBUpdateMgr.TStatus [in] Current status of update
    ///  manager.</param>
    ///  <param name="Cancel">Boolean [in/out] Flag that can be set True to
    ///  indicate that the update should be cancelled.</param>
    procedure UpdateStatusHandler(Sender: TObject; Status: TDBUpdateMgr.TStatus;
      var Cancel: Boolean);

    ///  <summary>Handles OnProgress event triggered by the database update
    ///  manager when downloading the database. Displays progress in a progress
    ///  bar.</summary>
    ///  <param name="Sender">TObject [in] Object triggering event.</param>
    ///  <param name="BytesReceived">Int64 [in] Total number of bytes received
    ///  to date.</param>
    ///  <param name="BytesExpected">Int64 [in] Total number of bytes to be
    ///  downloaded.</param>
    ///  <param name="Cancel">Boolean [in/out] Flag that can be set True to
    ///  indicate that the update should be cancelled.</param>
    ///  <remarks>NOTE: Setting cancel to True does not cancel the download,
    ///  which runs to completion. Instead the update process is cancelled after
    ///  the download completes.</remarks>
    procedure DownloadProgressHandler(Sender: TObject; const BytesReceived,
      BytesExpected: Int64; var Cancel: Boolean);

    ///  <summary>Handles OnProgress event triggered by the file updater when
    ///  when downloading the database. Displays progress in a progress bar.
    ///  </summary>
    ///  <param name="Sender">TObject [in] Object triggering event.</param>
    ///  <param name="Progress">Bytes [in] Pecentage progress to date.</param>
    ///  <param name="Cancel">Boolean [in,out] Flag that event handler can set
    ///  True to abort the update.</param>
    ///  <remarks>NOTE: Setting cancel to True does not cancel the file update,
    ///  which runs to completion. Instead the update process is cancelled after
    ///  the file update completes.</remarks>
    procedure FileUpdateProgressHandler(Sender: TObject; const Percent: Byte;
      var Cancel: Boolean);

    ///  <summary>Displays the given progress message.</summary>
    procedure ProgressMsg(const Msg: string);

    ///  <summary>Displays a 'headline' message.</summary>
    ///  <param name="Msg">string [in] Message to display.</param>
    ///  <param name="Kind">THeadlineStyle [in] Style of message to display.
    ///  </param>
    procedure HeadlineMsg(const Msg: string;
      const Kind: THeadlineStyle = hsNormal);

  strict protected

    ///  <summary>Positions this dialogue box's controls with the form.
    ///  </summary>
    procedure ArrangeForm; override;

    ///  <summary>Initialises the form's controls.</summary>
    procedure InitForm; override;

  public
    ///  <summary>Displays the dialogue box and performs any required database
    ///  update.</summary>
    ///  <param name="AOwner">TComponent [in] Component that owns the dialogue
    ///  box. If the component has an associated window the dialogue box is
    ///  aligned to it. May be nil.</param>
    ///  <returns>Boolean. True if the local database was updated or False if no
    ///  update was performed for any reason (i.e. cancelling, loca database is
    ///  up to date or an error occurred.</returns>
    class function Execute(AOwner: TComponent): Boolean;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UAppInfo, UColours, UConsts, UCtrlArranger, UStrUtils, UUtils;


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

{ TDBUpdateDlg }

procedure TDBUpdateDlg.ArrangeForm;
begin
  // Arrange inherited controls
  inherited;
  TCtrlArranger.SetLabelHeights(Self);
  // Controls in initial display
  btnDoUpdate.Top := TCtrlArranger.BottomOf(lblUpdateFromWeb, 16);
  // Arrange additonal cancel button
  btnCancel.Left := btnClose.Left + btnClose.Width - btnCancel.Width;
  btnCancel.Top := btnClose.Top;
  // Align error label
  lblError.Left := (pnlBody.Width - lblError.Width) div 2;
end;

procedure TDBUpdateDlg.btnCancelClick(Sender: TObject);
begin
  inherited;
  // Sets cancelled flag checked during download
  fCancelled := True;
  ProgressMsg(sCancelling);
end;

procedure TDBUpdateDlg.btnDoUpdateClick(Sender: TObject);
begin
  inherited;
  // Create update manager
  fUpdateMgr := TDBUpdateMgr.Create(GetDataDir, 'Manual');
  try
    fUpdateMgr.OnStatus := UpdateStatusHandler;
    fUpdateMgr.OnDownloadProgress := DownloadProgressHandler;
    fUpdateMgr.OnFileUpdateProgress := FileUpdateProgressHandler;
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

procedure TDBUpdateDlg.DownloadProgressHandler(Sender: TObject;
  const BytesReceived, BytesExpected: Int64; var Cancel: Boolean);
begin
  if BytesReceived = 0 then
    fProgressBarMgr.Max := BytesExpected;
  fProgressBarMgr.Position := BytesReceived;
  Cancel := fCancelled;
  Application.ProcessMessages;
end;

class function TDBUpdateDlg.Execute(AOwner: TComponent): Boolean;
begin
  with InternalCreate(AOwner) do
    try
      ShowModal;
      Result := fDataUpdated;
    finally
      Free;
    end;
end;

procedure TDBUpdateDlg.FileUpdateProgressHandler(Sender: TObject;
  const Percent: Byte; var Cancel: Boolean);
begin
  if Percent = 0 then
    fProgressBarMgr.Max := 100;
  fProgressBarMgr.Position := Percent;
  Cancel := fCancelled;
  Application.ProcessMessages;
end;

procedure TDBUpdateDlg.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
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

procedure TDBUpdateDlg.FormCreate(Sender: TObject);
begin
  inherited;
  // Record that no update yet taken place
  fDataUpdated := False;
  // Object that handles location and display of progress bar
  fProgressBarMgr := TMemoProgBarMgr.Create(edProgress);
end;

procedure TDBUpdateDlg.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(fProgressBarMgr);
end;

function TDBUpdateDlg.GetDataDir: string;
begin
  Result := TAppInfo.AppDataDir;
end;

procedure TDBUpdateDlg.HeadlineMsg(const Msg: string;
  const Kind: THeadlineStyle);
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
      lblHeadline.Caption := StrTrimRight(lblHeadline.Caption) + ' ';
      lblHeadline.Font.Color := clWarningText;
      TCtrlArranger.MoveToRightOf(lblHeadline, lblError);
      lblError.Top := lblHeadline.Top;
      lblError.Visible := True;
    end;
  end;
end;

procedure TDBUpdateDlg.InitForm;
begin
  inherited;
  btnCancel.Visible := False;
  btnClose.Visible := True;
end;

procedure TDBUpdateDlg.ProgressMsg(const Msg: string);
begin
  edProgress.Lines.Add(Msg);
end;

procedure TDBUpdateDlg.UpdateStatusHandler(Sender: TObject;
  Status: TDBUpdateMgr.TStatus; var Cancel: Boolean);
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
    begin
      ProgressMsg(sUpdating + '  ');
      fProgressBarMgr.Show(edProgress.Lines.Count - 1);
    end;
    usNoUpdate:
      ProgressMsg(sUpToDate);
    usLogOff:
      ProgressMsg(sLoggingOff);
    usCompleted:
    begin
      fProgressBarMgr.Hide;
      ProgressMsg(sCompleted);
    end;
    usCancelled:
      ProgressMsg(sCancelled);
  end;
  // Application needs to process messages to allow any UI updates
  Application.ProcessMessages;
  // Halt update if user cancelled
  Cancel := fCancelled;
end;

procedure TDBUpdateDlg.WMActivateApp(var Msg: TMessage);
begin
  Refresh;
end;

end.

