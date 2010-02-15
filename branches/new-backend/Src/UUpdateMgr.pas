{
 * UUpdateMgr.pas
 *
 * Object used to manage database updates from web.
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
 * The Original Code is UUpdateMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UUpdateMgr;


interface


uses
  // Delphi
  SysUtils, Classes,
  // Project
  UDownloadMgr, UDOSDateTime, UNews;


type

  {
  TUpdateResult:
    Possible results of download.
  }
  TUpdateResult = (
    urUpdated,          // files were updated: downloaded, deleted or both
    urNoUpdate,         // no files were updated: up to date
    urCancelled,        // use cancelled download
    urError             // an error occurred
  );

  {
  TUpdateStatus:
    Possible states during download.
  }
  TUpdateStatus = (
    usLogOn,            // logging on to web service
    usNews,             // displaying news
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
  TUploadStatusEvent:
    Event triggered when update status changes.
      @param Sender [in] Reference to object triggering event.
      @param Status [in] Current download status
      @param Cancel [in,out] Flag that handler can set true to abort the update.
  }
  TUpdateStatusEvent = procedure(Sender: TObject; Status: TUpdateStatus;
    var Cancel: Boolean) of object;

  {
  TUpdateDownloadEvent:
    Event triggered when downloading data to report progress.
      @param Sender [in] Reference to object triggering event.
      @param BytesHandled [in] Number of bytes downloaded to date.
      @param TotalBytes [in] Total number of bytes to be downloaded.
      @param Cancel [in/out] Flag that handler can set true to abort the update.
  }
  TUpdateDownloadEvent = procedure(Sender: TObject; const BytesHandled,
    TotalBytes: Integer; var Cancel: Boolean) of object;

  {
  TUpdateMgr:
    Manages update of CodeSnip database from web.
  }
  TUpdateMgr = class(TObject)
  strict private
    fNews: TNews;
      {List of news items}
    fCancelled: Boolean;
      {Flag true if update is cancelled}
    fDownloadMgr: TDownloadMgr;
      {Object used to interact with web service}
    fLocalDir: string;
      {Directory where CodeSnip "database" files are stored on local machine}
    fLongError: string;
      {Value of LongError property}
    fShortError: string;
      {Value of ShortError property}
    fOnStatus: TUpdateStatusEvent;
      {Event handler for OnStatus event}
    fOnDownloadProgress: TUpdateDownloadEvent;
      {Event handler for OnDownloadProgress event}
    procedure DownloadProgresshandler(Sender: TObject; const BytesToDate,
      ExpectedBytes: Integer);
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
    function LogOnAndGetNews: Boolean;
      {Logs on to web server and downloads any news items.
        @return True if log on successful or false if user cancelled.
      }
    function DownloadDatabase(const Data: TStream): Boolean;
      {Downloads database from web server.
        @param Data [in] Stream that receives downloaded data.
        @return True on success or false if cancelled.
      }
    function UpdateLocalDatabase(const Data: TStream): Boolean;
      {Udpates files in local database from stream of data that has been
      downloaded from web server.
        @param Data [in] Stream of data containing updates.
        @return True if successfully updated, false if cancelled.
      }
    function HandleException(const E: Exception): Boolean;
      {Handles various kinds of known exception, converting exceptions into long
      and short messages that are stored in LongError and ShortError properties.
        @param E [in] Exception to handle.
        @return True if exception handled and false if not handled.
      }
  strict protected
    function NotifyStatus(Status: TUpdateStatus): Boolean; virtual;
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
    function Execute: TUpdateResult;
      {Performs the update.
        @return Value indicating whether successfully updated, no update needed,
          user cancelled or error.
      }
    property News: TNews read fNews;
      {Downloaded news items}
    property LongError: string read fLongError;
      {Full description of last update error}
    property ShortError: string read fShortError;
      {Abbreviated description of last update error}
    property OnStatus: TUpdateStatusEvent read fOnStatus write fOnStatus;
      {Event triggered when update status changes. Informs of current status and
      gives user a chance to cancel the update}
    property OnDownloadProgress: TUpdateDownloadEvent
      read fOnDownloadProgress write fOnDownloadProgress;
      {Event triggered while downloading data from web server. Tracks download
      progress}
  end;


implementation


uses
  // Project
  IntfCommon, UFileUpdater, UUtils, UExceptions;


resourcestring
  // Error messages
  sGeneralUpdateError = 'Update Error';
  sChecksumError = 'Corrupt File Error';


{ TUpdateMgr }

constructor TUpdateMgr.Create(const LocalDir: string);
  {Class constructor. Sets up object.
    @param LocalDir [in] Directory storing data files on local machine.
  }
begin
  inherited Create;
  // Create download manager to download from remote web server
  fDownloadMgr := TDownloadMgr.Create;
  fDownloadMgr.OnProgress := DownloadProgressHandler;
  // Record local data directory
  fLocalDir := LocalDir;
end;

destructor TUpdateMgr.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fNews);
  FreeAndNil(fDownloadMgr);
  inherited;
end;

function TUpdateMgr.DownloadDatabase(const Data: TStream): Boolean;
  {Downloads database from web server.
    @param Data [in] Stream that receives downloaded data.
    @return True on success or false if cancelled.
  }
begin
  Result := False;
  if not NotifyStatus(usDownloadStart) then
    Exit;
  fDownloadMgr.GetDatabase(Data, True);
  if not NotifyStatus(usDownloadEnd) then
    Exit;
  Data.Position := 0;
  Result := not fCancelled;
end;

procedure TUpdateMgr.DownloadProgresshandler(Sender: TObject;
  const BytesToDate, ExpectedBytes: Integer);
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

function TUpdateMgr.Execute: TUpdateResult;
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
      if not LogOnAndGetNews then
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
      fDownloadMgr.LogOff;
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

function TUpdateMgr.HandleException(const E: Exception): Boolean;
  {Handles various kinds of known exception, converting exceptions into long and
  short messages that are stored in LongError and ShortError properties.
    @param E [in] Exception to handle.
    @return True if exception handled and false if not handled.
  }
begin
  if E is EDownloadMgr then
  begin
    // Download manager exceptions provide both long and short error messages
    fLongError := E.Message;
    fShortError := (E as EDownloadMgr).ShortMsg;
    Result := True;
  end
  else if E is EFileUpdater then
  begin
    // File updater exception represent checksum errors
    fLongError := E.Message;
    fShortError := sChecksumError;
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

function TUpdateMgr.LocalFileCount: Integer;
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
    FreeAndNil(LocalFiles);
  end;
end;

function TUpdateMgr.LogOnAndGetNews: Boolean;
  {Logs on to web server and downloads any news items.
    @return True if log on successful or false if user cancelled.
  }
var
  NewsData: TMemoryStream;  // stream containing news data
begin
  Result := False;
  if not NotifyStatus(usLogOn) then
    Exit;
  NewsData := TMemoryStream.Create;
  try
    fDownloadMgr.LogOn(NewsData);
    NewsData.Position := 0;
    fNews := TNews.Create(NewsData);
    if not NotifyStatus(usNews) then
      Exit;
    Result := True;
  finally
    FreeAndNil(NewsData);
  end;
end;

function TUpdateMgr.NewestLocalFileDate: IDOSDateTime;
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
        FileDate  := TDOSDateTimeFactory.CreateFromFile(
          IncludeTrailingPathDelimiter(fLocalDir) + FileName
        );
        if Result.Compare(FileDate) < 0 then
          (Result as IAssignable).Assign(FileDate);
      end;
    end;
  finally
    FreeAndNil(LocalFiles);
  end;
end;

function TUpdateMgr.NotifyStatus(Status: TUpdateStatus): Boolean;
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

function TUpdateMgr.PerformUpdate: Boolean;
  {Updates local files from remote database.
    @return True if update succeeded or false if update was cancelled.
  }
var
  Data: TMemoryStream;  // stream to store downloaded data
begin
  Result := False;
  if fCancelled then
    Exit;
  Data := TMemoryStream.Create;
  try
    if DownloadDatabase(Data) then
      Result := UpdateLocalDatabase(Data);
  finally
    FreeAndNil(Data);
  end;
end;

function TUpdateMgr.UpdateLocalDatabase(const Data: TStream): Boolean;
  {Udpates files in local database from stream of data that has been downloaded
  from web server.
    @param Data [in] Stream of data containing updates.
    @return True if successfully updated, false if cancelled.
  }
var
  Updater: TFileUpdater;  // Object that performs file updates.
begin
  Result := False;
  if not NotifyStatus(usUpdating) then
    Exit;
  Updater := TFileUpdater.Create(fLocalDir, Data);
  try
    Updater.Execute;
    Result := True;
  finally
    FreeAndNil(Updater);
  end;
end;

function TUpdateMgr.UpdateNeeded: Boolean;
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
    Result := LastDatabaseUpdate.Compare(NewestLocalFileDate) > 0;
  end;
end;

end.

