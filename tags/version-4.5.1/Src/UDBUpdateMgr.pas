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
  ///  <summary>Manages the process of checking update availability and perform
  ///  updates of the local Code Snippets Database from the online version.
  ///  </summary>
  TDBUpdateMgr = class(TObject)
  public
    type
      ///  <summary>
      ///  <para>Enumeration of possible results of a database update request.
      ///  </para>
      ///  <para>- urUpdated - local database was updated</para>
      ///  <para>- urNoUpdate - no update available: local database up to date.
      ///  </para>
      ///  <para>- urCancelled - user cancelled request.</para>
      ///  <para>- urError - an error occurred.</para>
      ///  </summary>
      TUpdateResult = (urUpdated, urNoUpdate, urCancelled, urError);
    type
      ///  <summary>
      ///  <para>Enumeration of possible result when querying if a database
      ///  updated is available.</para>
      ///  <para>- upUpToDate - local database is up to date.</para>
      ///  <para>- uqUpdateAvailable - a database update is available.</para>
      ///  <para>- uqError - an error occurred during the request.</para>
      ///  </summary>
      TUpdateQueryResult = (uqUpToDate, uqUpdateAvailable, uqError);
    type
      ///  <summary>
      ///  <para>Enumeration of different possible states of database update
      ///  manager during an update request.</para>
      ///  <para>-usLogon - logging on to update web service.</para>
      ///  <para>-usCheckForUpdates - checking for availability of update.
      ///  </para>
      ///  <para>-usDownloadStart - starting to download online database.</para>
      ///  <para>-usDownloadEnd - finished downloading online database.</para>
      ///  <para>-usUpdating - updating local database.</para>
      ///  <para>-usNoUpdate - no update available.</para>
      ///  <para>-usLogOff - logging off web service.</para>
      ///  <para>-usCompleted - completed update process.</para>
      ///  <para>-usCancelled - update process was cancelled.</para>
      ///  </summary>
      TStatus = (
        usLogOn, usCheckForUpdates, usDownloadStart, usDownloadEnd, usUpdating,
        usNoUpdate, usLogOff, usCompleted, usCancelled
      );
    type
      ///  <summary>Type of event triggered when status changes during an update
      ///  request.</summary>
      ///  <param name="Sender">TObject [in] Object that triggered event.
      ///  </param>
      ///  <param name="Status">TDBUpdateMgr.TStatus [in] Current status.
      ///  </param>
      ///  <param name="Cancel">Boolean [in,out] Flag that event handler can set
      ///  True to abort the update.</param>
      TStatusEvent = procedure(Sender: TObject; Status: TStatus;
        var Cancel: Boolean) of object;
    type
      ///  <summary>Type of event triggered to report progress when downloading
      ///  the online database.</summary>.
      ///  <param name="Sender">TObject [in] Object that triggered event.
      ///  </param>
      ///  <param name="BytesHandled">Int64 [in] Number of bytes downloaded so
      ///  far.</param>
      ///  <param name="TotalBytes">Int64 [in] Total number of bytes to be
      ///  downloaded.</param>
      ///  <param name="Cancel">Boolean [in,out] Flag that event handler can set
      ///  True to abort the update.</param>
      TDownloadEvent = procedure(Sender: TObject; const BytesHandled,
        TotalBytes: Int64; var Cancel: Boolean) of object;
    type
      ///  <summary>Type of event triggered to report progress when updating
      ///  local database files.</summary>
      ///  <param name="Sender">TObject [in] Object that triggered event.
      ///  </param>
      ///  <param name="Progress">Bytes [in] Pecentage progress to date.</param>
      ///  <param name="Cancel">Boolean [in,out] Flag that event handler can set
      ///  True to abort the update.</param>
      TProgressEvent = procedure(Sender: TObject; const Progress: Byte;
        var Cancel: Boolean) of object;
  strict private
    var
      ///  <summary>Flag that indicates if an update request was cancelled.
      ///  </summary>
      fCancelled: Boolean;
      ///  <summary>Object that interacts with database download web service.
      ///  </summary>
      fDownloadMgr: TDBDownloadMgr;
      ///  <summary>Directory when the local database files are stored.
      ///  </summary>
      fLocalDir: string;
      ///  <summary>Value of LongError property.</summary>
      fLongError: string;
      ///  <summary>Value of ShortError property.</summary>
      fShortError: string;
      ///  <summary>Event handler for OnStatus event.</summary>
      fOnStatus: TStatusEvent;
      ///  <summary>Event handler for OnDownloadProgress event.</summary>
      fOnDownloadProgress: TDownloadEvent;
      ///  <summary>Event handler for OnFileUpdateProgress event.</summary>
      fOnFileUpdateProgress: TProgressEvent;
      ///  <summary>Information specific to the code using this object.
      ///  </summary>
      fCallerInfo: string;

    ///  <summary>Handles database download manager's OnProgress event.
    ///  </summary>
    ///  <param name="Sender">TObject [in] Object that triggered event.</param>
    ///  <param name="BytesToDate">Int64 [in] Number of bytes downloaded so
    ///  far.</param>
    ///  <param name="ExpectedBytes">Int64 [in] Total number of bytes to be
    ///  downloaded.</param>
    procedure DownloadProgressHandler(Sender: TObject; const BytesToDate,
      ExpectedBytes: Int64);

    ///  <summary>Handles file updater's OnProgress event.</summary>
    ///  <param name="Sender">TObject [in] Object that triggered event.</param>
    ///  <param name="FilesHandled">Cardinal [in] Number of files updated to
    ///  date.</param>
    ///  <param name="TotalFiles">Cardinal [in] Total number of files to be
    ///  updated.</param>
    procedure FileUpdateProgressHandler(Sender: TObject; const AFilesHandled,
      ATotalFiles: Cardinal);

    ///  <summary>Returns the number of files in the local copy of the Code
    ///  Snippets database.</summary>
    function LocalFileCount: Integer;

    ///  <summary>Returns an object that encapsulate the last update date of
    ///  the newest file in the local copy of the Code Snippets database.
    ///  </summary>
    function NewestLocalFileDate: IDOSDateTime;

    ///  <summary>Checks if the local Code Snippets database files need to be
    ///  updated.</summary>
    ///  <returns>Boolean. True if an update is required or False if not or if
    ///  process was cancelled.</returns>
    ///  <remarks>Compares date of newest local database file with last update
    ///  date of online database.</remarks>
    function UpdateNeeded: Boolean;

    ///  <summary>Downloads database and updates local files.</summary>
    ///  <returns>Boolean. True if download and update succeeded or False if
    ///  the process was cancelled.</returns>
    function PerformUpdate: Boolean;

    ///  <summary>Logs on to web server.</summary>
    ///  <returns>Boolean. True if log-on succeeded or False if the process was
    ///  cancelled.</returns>
    function LogOn: Boolean;

    ///  <summary>Logs off web server.</summary>
    procedure LogOff;

    ///  <summary>Downloads a copy of the online database.</summary>
    ///  <param name="Data">TEncodedData [out] Receives data containing the
    ///  downloaded database.</param>
    ///  <returns>Boolean. True on success or False if process is cancelled.
    ///  </returns>
    function DownloadDatabase(out Data: TEncodedData): Boolean;

    ///  <summary>Updates the files in the local database from the downloaded
    ///  data.</summary>
    ///  <param name="Data">TEncodedData [in] Data containing downloaded
    ///  database.</param>
    ///  <returns>Boolean. True on success or False if process is cancelled.
    ///  </returns>
    function UpdateLocalDatabase(const Data: TEncodedData): Boolean;

    ///  <summary>Handles various types of known exception, recording their
    ///  messages.</summary>
    ///  <param name="E">Exception [in] Exception to be handled.</param>
    ///  <returns>True if exception was handled or False otherwise.</returns>
    ///  <remarks>Supported exception messages are made available via the
    ///  ShortError and LongError properties.</remarks>
    function HandleException(const E: Exception): Boolean;

  strict protected
    ///  <summary>Notifies a change in the status of an update request by
    ///  triggering the OnStatus event.</summary>
    ///  <param name="Status">TDBUpdateMgr.TStatus [in] New status.</param>
    ///  <returns>Boolean. True if event handler did not set Cancelled flag,
    ///  False otherwise.</returns>
    function NotifyStatus(Status: TStatus): Boolean; virtual;

  public

    ///  <summary>Constructs and initialises a new object instance.</summary>
    ///  <param name="LocalDir">string [in] Directory that contains the local
    ///  copy of the Code Snippets database.</param>
    ///  <param name="CallerInfo">string [in] Information about calling code.
    ///  </param>
    constructor Create(const LocalDir, CallerInfo: string);

    ///  <summary>Destroys object instance.</summary>
    destructor Destroy; override;

    ///  <summary>Performs a database update.</summary>
    ///  <returns>TDBUpdateMgr.TUpdateResult. Value that indicates if database
    ///  was updated, if process was cancelled or if there has been an error.
    ///  </returns>
    function Execute: TUpdateResult;

    ///  <summary>Queries whether updates to the local database are available.
    ///  </summary>
    ///  <returns>TDBUpdateMgr.TUpdateQueryResult. Value indicating whether a
    ///  update is available or not of if and error occured with the query.
    ///  </returns>
    function CheckForUpdates: TUpdateQueryResult;

    ///  <summary>Full description of last update error.</summary>
    property LongError: string read fLongError;

    ///  <summary>Abbreviated description of last update error.</summary>
    property ShortError: string read fShortError;

    ///  <summary>Event triggered when the status of an update request changes.
    ///  </summary>
    property OnStatus: TStatusEvent read fOnStatus write fOnStatus;

    ///  <summary>Event triggered to report progress when downloading a copy of
    ///  the online database.</summary>
    property OnDownloadProgress: TDownloadEvent
      read fOnDownloadProgress write fOnDownloadProgress;

    ///  <summary>Event triggered to report progress when updating local
    ///  database files.</summary>
    property OnFileUpdateProgress: TProgressEvent
      read fOnFileUpdateProgress write fOnFileUpdateProgress;
  end;


implementation


uses
  // Delphi
  Classes, Math,
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

function TDBUpdateMgr.CheckForUpdates: TUpdateQueryResult;
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

constructor TDBUpdateMgr.Create(const LocalDir, CallerInfo: string);
begin
  inherited Create;
  // Create download manager to download from remote web server
  fDownloadMgr := TDBDownloadMgr.Create;
  fDownloadMgr.OnProgress := DownloadProgressHandler;
  fCallerInfo := CallerInfo;
  fLocalDir := LocalDir;
  EnsureFolders(fLocalDir);
end;

destructor TDBUpdateMgr.Destroy;
begin
  fDownloadMgr.Free;
  inherited;
end;

function TDBUpdateMgr.DownloadDatabase(out Data: TEncodedData): Boolean;
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
begin
  if Assigned(fOnDownloadProgress) then
    fOnDownloadProgress(Self, BytesToDate, ExpectedBytes, fCancelled);
end;

function TDBUpdateMgr.Execute: TUpdateResult;
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

procedure TDBUpdateMgr.FileUpdateProgressHandler(Sender: TObject;
  const AFilesHandled, ATotalFiles: Cardinal);
var
  FilesHandled: Cardinal;
  TotalFiles: Cardinal;
  Percent: Byte;
begin
  if not Assigned(fOnFileUpdateProgress) then
    Exit;
  // Ensure TotalFiles > 0 and 0 <= FilesHandled <= TotalFiles
  TotalFiles := Max(1, ATotalFiles);
  FilesHandled := Min(AFilesHandled, ATotalFiles);
  Percent := Round(100 * FilesHandled / TotalFiles);
  fOnFileUpdateProgress(Self, Percent, fCancelled);
end;

function TDBUpdateMgr.HandleException(const E: Exception): Boolean;
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
begin
  fDownloadMgr.LogOff;
end;

function TDBUpdateMgr.LogOn: Boolean;
begin
  Result := False;
  if not NotifyStatus(usLogOn) then
    Exit;
  fDownloadMgr.LogOn(fCallerInfo);
  Result := True;
end;

function TDBUpdateMgr.NewestLocalFileDate: IDOSDateTime;
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

function TDBUpdateMgr.NotifyStatus(Status: TStatus): Boolean;
begin
  if Assigned(fOnStatus) then
    fOnStatus(Self, Status, fCancelled);
  Result := not fCancelled;
end;

function TDBUpdateMgr.PerformUpdate: Boolean;
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
var
  Updater: TFileUpdater;  // object that performs file updates.
begin
  Result := False;
  if not NotifyStatus(usUpdating) then
    Exit;
  Updater := TFileUpdater.Create(fLocalDir, Data);
  try
    Updater.OnProgress := FileUpdateProgressHandler;
    Updater.Execute;
    Result := True;
  finally
    Updater.Free;
  end;
end;

function TDBUpdateMgr.UpdateNeeded: Boolean;
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

