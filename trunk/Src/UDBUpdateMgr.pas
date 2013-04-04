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
 * Object used to manage database updates from web.
}


unit UDBUpdateMgr;


interface


uses
  // Delphi
  SysUtils,
  // Project
  UDOSDateTime, UEncodings, Web.UDBDownloadMgr;


type

  {
  TDBUpdateResult:
    Possible results of download.
  }
  TDBUpdateResult = (
    urUpdated,          // files were updated: downloaded, deleted or both
    urNoUpdate,         // no files were updated: up to date
    urCancelled,        // use cancelled download
    urError             // an error occurred
  );

  {
  TDBUpdateQueryResult:
    Possible results when checking if update available.
  }
  TDBUpdateQueryResult = (
    uqUpToDate,
    uqUpdateAvailable,
    uqError
  );

  {
  TDBUpdateStatus:
    Possible states during download.
  }
  TDBUpdateStatus = (
    usLogOn,            // logging on to web service
    usCheckForUpdates,  // checking for updates
    usDownloadStart,    // starting to download database
    usDownloadEnd,      // finished downloading database
    usUpdating,         // updating local files
    usNoUpdate,         // no update required
    usLogOff,           // logging off web service
    usCompleted,        // completed update process: can display result
    usCancelled         // cancelled update process
  );

  {
  TDBUpdateStatusEvent:
    Event triggered when update status changes.
      @param Sender [in] Reference to object triggering event.
      @param Status [in] Current download status
      @param Cancel [in,out] Flag that handler can set true to abort the update.
  }
  TDBUpdateStatusEvent = procedure(Sender: TObject; Status: TDBUpdateStatus;
    var Cancel: Boolean) of object;

  {
  TDBUpdateDownloadEvent:
    Event triggered when downloading data to report progress.
      @param Sender [in] Reference to object triggering event.
      @param BytesHandled [in] Number of bytes downloaded to date.
      @param TotalBytes [in] Total number of bytes to be downloaded.
      @param Cancel [in/out] Flag that handler can set true to abort the update.
  }
  TDBUpdateDownloadEvent = procedure(Sender: TObject; const BytesHandled,
    TotalBytes: Int64; var Cancel: Boolean) of object;

  {
  TDBUpdateMgr:
    Manages update of CodeSnip database from web.
  }
  TDBUpdateMgr = class(TObject)
  strict private
    fCancelled: Boolean;
      {Flag true if update is cancelled}
    fDownloadMgr: TDBDownloadMgr;
      {Object used to interact with web service}
    fLocalDir: string;
      {Directory where CodeSnip "database" files are stored on local machine}
    fLongError: string;
      {Value of LongError property}
    fShortError: string;
      {Value of ShortError property}
    fOnStatus: TDBUpdateStatusEvent;
      {Event handler for OnStatus event}
    fOnDownloadProgress: TDBUpdateDownloadEvent;
      {Event handler for OnDownloadProgress event}
    procedure DownloadProgresshandler(Sender: TObject; const BytesToDate,
      ExpectedBytes: Int64);
      {Handles download manager's OnProgress event by passing values to own
      OnDownloadProgress event.
        @param Sender [in] Not used.
        @param BytesToDate [in] Bytes downloaded to date.
        @param ExpectedBytes [in] Total number of bytes in download.
      }
    function LocalFileCount: Integer;
      {Counts files in local database.
        @return Number of files in local database.
      }
    function NewestLocalFileDate: IDOSDateTime;
      {Finds date of most recently updated file in local database.
        @return Object representing DOS file date of newest file.
      }
    function UpdateNeeded: Boolean;
      {Checks if local files need to be updated. This is the case when there are
      newer files on remote database than in local database.
        @return True if update needed, false if not.
      }
    function PerformUpdate: Boolean;
      {Updates local files from remote database.
        @return True if update succeeded or false if update was cancelled.
      }
    function LogOn: Boolean;
      {Logs on to web server.
        @return True if log on successful or false if user cancelled.
      }
    procedure LogOff;
      {Logs off web server.
      }
    function DownloadDatabase(out Data: TEncodedData): Boolean;
      {Downloads database from web server.
        @param Data [out] Receives downloaded data.
        @return True on success or false if cancelled.
      }
    function UpdateLocalDatabase(const Data: TEncodedData): Boolean;
      {Udpates files in local database from stream of data that has been
      downloaded from web server.
        @param Data [in] Data containing updates.
        @return True if successfully updated, false if cancelled.
      }
    function HandleException(const E: Exception): Boolean;
      {Handles various kinds of known exception, converting exceptions into long
      and short messages that are stored in LongError and ShortError properties.
        @param E [in] Exception to handle.
        @return True if exception handled and false if not handled.
      }
  strict protected
    function NotifyStatus(Status: TDBUpdateStatus): Boolean; virtual;
      {Notifies change in download status by triggering OnStatus event. Checks
      if download was cancelled in event handler.
        @param Status [in] Status code to be notified.
        @return False if cancel flagged when event handler returns, true
          otherwise.
      }
  public
    constructor Create(const LocalDir: string);
      {Class constructor. Sets up object.
        @param LocalDir [in] Directory storing data files on local machine.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    function Execute: TDBUpdateResult;
      {Performs the update.
        @return Value indicating whether successfully updated, no update needed,
          user cancelled or error.
      }
    function CheckForUpdates: TDBUpdateQueryResult;
      {Checks if updates to the local database are available.
        @return Value indicating whether an update is needed or not or if an
          error occurred.
      }
    property LongError: string read fLongError;
      {Full description of last update error}
    property ShortError: string read fShortError;
      {Abbreviated description of last update error}
    property OnStatus: TDBUpdateStatusEvent read fOnStatus write fOnStatus;
      {Event triggered when update status changes. Informs of current status and
      gives user a chance to cancel the update}
    property OnDownloadProgress: TDBUpdateDownloadEvent
      read fOnDownloadProgress write fOnDownloadProgress;
      {Event triggered while downloading data from web server. Tracks download
      progress}
  end;


implementation


uses
  // Delphi
  Classes,
  // Project
  IntfCommon, UConsts, UFileUpdater, UUtils, UExceptions;


resourcestring
  // Error messages
  sGeneralUpdateError = 'Update Error';
  sChecksumShortError = 'Corrupt File Error';
  sChecksumLongError = '%s'
    + EOL2
    + 'This is probably caused by an internet transmisson error. Please try '
    + 'downloading again. If the problem persists please report it.';


{ TDBUpdateMgr }

function TDBUpdateMgr.CheckForUpdates: TDBUpdateQueryResult;
  {Checks if updates to the local database are available.
    @return Value indicating whether an update is needed or not or if an error
      occurred.
  }
begin
  try
    LogOn;
    if UpdateNeeded then
      Result := uqUpdateAvailable
    else
      Result := uqUpToDate;
    LogOff;
  except
    on E: Exception do
    begin
      Result := uqError;
      if not HandleException(E) then
        raise;
    end;
  end;
end;

constructor TDBUpdateMgr.Create(const LocalDir: string);
  {Class constructor. Sets up object.
    @param LocalDir [in] Directory storing data files on local machine.
  }
begin
  inherited Create;
  // Create download manager to download from remote web server
  fDownloadMgr := TDBDownloadMgr.Create;
  fDownloadMgr.OnProgress := DownloadProgressHandler;
  // Record local data directory & ensure it exists
  fLocalDir := LocalDir;
  EnsureFolders(fLocalDir);
end;

destructor TDBUpdateMgr.Destroy;
  {Class destructor. Tears down object.
  }
begin
  fDownloadMgr.Free;
  inherited;
end;

function TDBUpdateMgr.DownloadDatabase(out Data: TEncodedData): Boolean;
  {Downloads database from web server.
    @param Data [out] Receives downloaded data.
    @return True on success or false if cancelled.
  }
begin
  Result := False;
  if not NotifyStatus(usDownloadStart) then
    Exit;
  Data := fDownloadMgr.GetDatabase(True);
  if not NotifyStatus(usDownloadEnd) then
    Exit;
  Result := not fCancelled;
end;

procedure TDBUpdateMgr.DownloadProgresshandler(Sender: TObject;
  const BytesToDate, ExpectedBytes: Int64);
  {Handles download manager's OnProgress event by passing values to own
  OnDownloadProgress event.
    @param Sender [in] Not used.
    @param BytesToDate [in] Bytes downloaded to date.
    @param ExpectedBytes [in] Total number of bytes in download.
  }
begin
  if Assigned(fOnDownloadProgress) then
    fOnDownloadProgress(Self, BytesToDate, ExpectedBytes, fCancelled);
end;

function TDBUpdateMgr.Execute: TDBUpdateResult;
  {Performs the update.
    @return Value indicating whether successfully updated, no update needed,
      user cancelled or error.
  }
begin
  // Assume user cancelled
  Result := urCancelled;
  try
    try
      // Log on to web server
      if not LogOn then
        Exit;
      // Check if we need an update
      if UpdateNeeded then
      begin
        if not PerformUpdate then
          Exit;
        Result := urUpdated;
      end
      else
      begin
        if not NotifyStatus(usNoUpdate) then
          Exit;
        Result := urNoUpdate;
      end;
      // Log off web server
      if not NotifyStatus(usLogOff) then
        Exit;
      LogOff;
      if not NotifyStatus(usCompleted) then
        Exit;
    except
      // Handle known exceptions
      on E: Exception do
      begin
        Result := urError;
        if not HandleException(E) then
          raise;
      end;
    end;
  finally
    if fCancelled then
      NotifyStatus(usCancelled);
  end;
end;

function TDBUpdateMgr.HandleException(const E: Exception): Boolean;
  {Handles various kinds of known exception, converting exceptions into long and
  short messages that are stored in LongError and ShortError properties.
    @param E [in] Exception to handle.
    @return True if exception handled and false if not handled.
  }
begin
  if E is EDBDownloadMgr then
  begin
    // Download manager exceptions provide both long and short error messages
    fLongError := E.Message;
    fShortError := (E as EDBDownloadMgr).ShortMsg;
    Result := True;
  end
  else if E is EFileUpdater then
  begin
    // File updater exception represent checksum errors
    fLongError := Format(sChecksumLongError, [E.Message]);
    fShortError := sChecksumShortError;
    Result := True;
  end
  else if E is ECodeSnip then
  begin
    // General non-bug exceptions
    fLongError := E.Message;
    fShortError := sGeneralUpdateError;
    Result := True;
  end
  else
    // Don't handle any other exception types
    Result := False;
end;

function TDBUpdateMgr.LocalFileCount: Integer;
  {Counts files in local database.
    @return Number of files in local database.
  }
var
  LocalFiles: TStringList;  // list of files in local database directory
begin
  LocalFiles := TStringList.Create;
  try
    ListFiles(fLocalDir, '*.*', LocalFiles);
    Result := LocalFiles.Count;
  finally
    LocalFiles.Free;
  end;
end;

procedure TDBUpdateMgr.LogOff;
  {Logs off web server.
  }
begin
  fDownloadMgr.LogOff;
end;

function TDBUpdateMgr.LogOn: Boolean;
  {Logs on to web server.
    @return True if log on successful or false if user cancelled.
  }
begin
  Result := False;
  if not NotifyStatus(usLogOn) then
    Exit;
  fDownloadMgr.LogOn;
  Result := True;
end;

function TDBUpdateMgr.NewestLocalFileDate: IDOSDateTime;
  {Finds date of most recently updated file in local database.
    @return Object representing DOS file date of newest file.
  }
var
  LocalFiles: TStringList;  // list of files in local data directory
  FileName: string;         // name of a file in local data directory
  FileDate: IDOSDateTime;   // DOS date of a file
begin
  Result := TDOSDateTimeFactory.CreateFromDOSTimeStamp(0);
  // Get all files in directory
  LocalFiles := TStringList.Create;
  try
    ListFiles(fLocalDir, '*.*', LocalFiles);
    if LocalFiles.Count > 0 then
    begin
      for FileName in LocalFiles do
      begin
        FileDate  := TDOSDateTimeFactory.CreateFromFile(FileName);
        if Result.CompareTo(FileDate) < 0 then
          (Result as IAssignable).Assign(FileDate);
      end;
    end;
  finally
    LocalFiles.Free;
  end;
end;

function TDBUpdateMgr.NotifyStatus(Status: TDBUpdateStatus): Boolean;
  {Notifies change in download status by triggering OnStatus event. Checks if
  download was cancelled in event handler.
    @param Status [in] Status code to be notified.
    @return False if cancel flagged when event handler returns, true
      otherwise.
  }
begin
  if Assigned(fOnStatus) then
    fOnStatus(Self, Status, fCancelled);
  Result := not fCancelled;
end;

function TDBUpdateMgr.PerformUpdate: Boolean;
  {Updates local files from remote database.
    @return True if update succeeded or false if update was cancelled.
  }
var
  Data: TEncodedData; // stores downloaded data
begin
  Result := False;
  if fCancelled then
    Exit;
  if DownloadDatabase(Data) then
    Result := UpdateLocalDatabase(Data);
end;

function TDBUpdateMgr.UpdateLocalDatabase(const Data: TEncodedData): Boolean;
  {Udpates files in local database from stream of data that has been downloaded
  from web server.
    @param Data [in] Data containing updates.
    @return True if successfully updated, false if cancelled.
  }
var
  Updater: TFileUpdater;  // object that performs file updates.
begin
  Result := False;
  if not NotifyStatus(usUpdating) then
    Exit;
  Updater := TFileUpdater.Create(fLocalDir, Data);
  try
    Updater.Execute;
    Result := True;
  finally
    Updater.Free;
  end;
end;

function TDBUpdateMgr.UpdateNeeded: Boolean;
  {Checks if local files need to be updated. This is the case when there are
  newer files on remote database than in local database, or if numbers of files
  in local database and remote database differ.
    @return True if update needed, false if not.
  }
var
  LastDatabaseUpdate: IDOSDateTime; // date of newest file in remote database
begin
  Result := False;
  if not NotifyStatus(usCheckForUpdates) then
    Exit;
  // first check if file counts match
  Result := fDownloadMgr.FileCount <> LocalFileCount;
  if not Result then
  begin
    // file count OK: compare last update dates
    LastDatabaseUpdate := TDOSDateTimeFactory.CreateFromUnixTimeStamp(
      StrToInt64(fDownloadMgr.LastUpdate)
    );
    Result := LastDatabaseUpdate.CompareTo(NewestLocalFileDate) > 0;
  end;
end;

end.

