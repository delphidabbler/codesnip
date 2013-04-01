{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements thread classes that check online to find if updates to the
 * CodeSnip program or online database are available. Also implements a manager
 * class for these threads.
}


unit UUpdateCheckers;


interface


uses
  // Delphi
  SysUtils, ExtActns,
  // Project
  Notifications.UData, Notifications.URecorderThread, UThreadGroup;


type
  ///  <summary>Thread that checks online to find if a new version of CodeSnip
  ///  is available.</summary>
  ///  <remarks>
  ///  <para>No check is made if the user has switched the facility off in
  ///  preferences or if a check has been made within the preceding 24 hrs.
  ///  </para>
  ///  <para>The thread fails silently as if no update is available if any
  ///  errors occur when accessing the internet or web service.</para>
  ///  <para>If an update is found a notification containing the details is
  ///  placed in the notification queue to await display.</para>
  ///  </remarks>
  TProgramUpdateCheckerThread = class(TNotificationRecorderThread)
  strict private
    var
      ///  <summary>Action that causes a web page to be displayed from where
      ///  a program update can be downloaded.</summary>
      ///  <remarks>This action is executed when the user clicks the 'task
      ///  button' in any program update notification window.</remarks>
      fDownloadAction: TBrowseURL;
  strict protected
    ///  <summary>Finds if an update of the CodeSnip program is available and
    ///  creates a notification containing information about it if so.
    ///  </summary>
    ///  <param name="N">TNotificationData [out] If an update is available this
    ///  parameter contains information about the update to be added to the
    ///  notification queue. If no update is available this value is undefined.
    ///  </param>
    ///  <returns>Boolean. True if an update is a available and a notification
    ///  record was created or False if no update is available.</returns>
    ///  <remarks>False is also returned if an error was encountered or if the
    ///  user has disabled automatic checking for program updates.</remarks>
    function GetNotification(out N: TNotificationData): Boolean; override;
  public
    ///  <summary>Creates the and initialises the thread instance.</summary>
    ///  <param name="StartDelay">Cardinal [in] Number of milliseconds to delay
    ///  the start of the thread's execution. Passing 0 indicates no delay.
    ///  Non-zero delays are rounded up to the nearest 1/5th second.</param>
    constructor Create(StartDelay: Cardinal = 0);
    ///  <summary>Destroys thread object instance.</summary>
    destructor Destroy; override;
  end;

type
  ///  <summary>Thread that checks online to find if a new version of the online
  ///  code snippets database is available.</summary>
  ///  <remarks>
  ///  <para>No check is made if the user has switched the facility off in
  ///  preferences or if a check has been made within the preceding 24 hrs.
  ///  </para>
  ///  <para>The thread fails silently as if no update is available if any
  ///  errors occur when accessing the internet or web service.</para>
  ///  <para>If an update is found a notification containing the details is
  ///  placed in the notification queue to await display.</para>
  ///  </remarks>
  TDatabaseUpdateCheckerThread = class(TNotificationRecorderThread)
  strict protected
    ///  <summary>Checks if the local copy of the online database is up to date
    ///  and creates a notification containing information about it if so.
    ///  </summary>
    ///  <param name="N">TNotificationData [out] If an update is available this
    ///  parameter contains information about the update to be added to the
    ///  notification queue. If no update is available this value is undefined.
    ///  </param>
    ///  <returns>Boolean. True if an update is a available and a notification
    ///  record was created or False if no update is available.</returns>
    ///  <remarks>False is also returned if an error was encountered or if the
    ///  user has disabled automatic checking for database updates.</remarks>
    function GetNotification(out N: TNotificationData): Boolean; override;
  end;

type
  ///  <summary>Manages creation and lifetime of the background threads that
  ///  check for and notify program and online code snippets database updates.
  ///  </summary>
  TUpdateCheckerMgr = class(TObject)
  strict private
    var
      ///  <summary>Manages the lifetime of the update threads.</summary>
      fThreads: TThreadGroup;
  public
    ///  <summary>Creates new object instance and creates and initialises the
    ///  update checker threads.</summary>
    ///  <remarks>The threads are created suspended.</remarks>
    constructor Create;
    ///  <summary>Destroys object instance and update checker threads.</summary>
    ///  <remarks>Waits for any non-terminated threads to complete.</remarks>
    destructor Destroy; override;
    ///  <summary>Starts the update checker threads.</summary>
    procedure StartThreads;
    ///  <summary>Signals the update checker threads to terminate.</summary>
    ///  <remarks>Does not wait for the threads to complete.</remarks>
    procedure StopThreads;
  end;

type
  ///  <summary>Wrapper around the 'UpdateChecks' section of the per-user
  ///  config file that records information about when update availability was
  ///  checked.</summary>
  TUpdateCheckerConfig = class(TObject)
  public
    type
      ///  <summary>
      ///  <para>Enumeration of supported kinds of update check.</para>
      ///  <para>- ukProgram - program update checks.</para>
      ///  <para>- ukDatabase - database update checks.</para>
      ///  </summary>
      TCheckKind = (ckProgram, ckDatabase);
  strict private
    var
      ///  <summary>Dates and times of last successful update check for each
      ///  supported kind of update.<summary>
      fLastUpdateCheck: array[TCheckKind] of TDateTime;
    ///  <summary>Returns the config file name of the value that stores that
    ///  stores the last upate check date for the given update check.</summary>
    function ValueName(const Kind: TCheckKind): string;
    ///  <summary>Formats the given TDateTime value as a string.</summary>
    function FormatDate(const DT: TDateTime): string;
    ///  <summary>Converts the given string as a TDateTime value.</summary>
    ///  <exceptions>If S is not a valid config file date a fatal exception will
    ///  be raised.</exceptions>
    function ParseDate(const S: string): TDateTime;
  public
    ///  <summary>Construct new object instance and reads last update check
    ///  times from storage.</summary>
    constructor Create;
    ///  <summary>Writes the last update check times to storage then destroys
    ///  the object.</summary>
    destructor Destroy; override;
    ///  <summary>Uses information from storage to determine if update
    ///  availability should be checked for the given update kind.</summary>
    function CanCheck(Kind: TCheckKind): Boolean;
    ///  <summary>Update the last update check time for the given update kind.
    ///  </summary>
    procedure RecordCheck(Kind: TCheckKind);
  end;


implementation


uses
  // Delphi
  Classes, Windows, DateUtils,
  // Project
  FmUpdateDlg, UAppInfo, UPreferences, UProgramUpdateChecker, USettings,
  UStrUtils, UUpdateMgr, UUtils;


{ TProgramUpdateCheckerThread }

constructor TProgramUpdateCheckerThread.Create(StartDelay: Cardinal);
begin
  inherited Create(StartDelay);
  fDownloadAction := TBrowseURL.Create(nil);
end;

destructor TProgramUpdateCheckerThread.Destroy;
begin
  fDownloadAction.Free;
  inherited;
end;

function TProgramUpdateCheckerThread.GetNotification(out N: TNotificationData):
  Boolean;
resourcestring
  sTitle = 'Program Update Available';
  sContent = 'CodeSnip version %s is available for download.';
  sDownloadPrompt = 'Download Now';
const
  HelpKeyword = 'ProgramUpdateNotification';
var
  UpdateChecker: TProgramUpdateChecker;
  Content: TArray<string>;
  TaskCallback: TProc;
  Config: TUpdateCheckerConfig;
begin
  try
    Config := TUpdateCheckerConfig.Create;
    try
      if not Preferences.AutoCheckProgramUpdates
        or not Config.CanCheck(ckProgram) then
        Exit(False);
      Config.RecordCheck(ckProgram);
    finally
      Config.Free;
    end;
    UpdateChecker := TProgramUpdateChecker.Create;
    try
      if not UpdateChecker.Execute('Auto') then
        Exit(False);
      fDownloadAction.URL := UpdateChecker.DownloadURL;
      Content := TArray<string>.Create(
        Format(sContent, [string(UpdateChecker.LatestVersion)])
      );
      TaskCallback :=
        procedure
        begin
          fDownloadAction.Execute;
        end;
      N := TNotificationData.Create(
        sTitle, Content, HelpKeyword, TaskCallback, sDownloadPrompt
      );
      Result := True;
    finally
      UpdateChecker.Free;
    end;
  except
    // Swallow any exception and return false to indicate "no update"
    Result := False;
  end;
end;

{ TDatabaseUpdateCheckerThread }

function TDatabaseUpdateCheckerThread.GetNotification(
  out N: TNotificationData): Boolean;
resourcestring
  sTitle = 'Database Update Available';
  sContent1 = 'The online Code Snippets Database has been updated.';
  sContent2 = 'You can update your local copy of the database to reflect the '
    + 'changes.';
  sUpdatePrompt = 'Update Now';
const
  HelpKeyword = 'DatabaseUpdateNotification';
var
  UpdateMgr: TUpdateMgr;
  Content: TArray<string>;
  TaskCallback: TProc;
  Config: TUpdateCheckerConfig;
begin
  try
    Config := TUpdateCheckerConfig.Create;
    try
      if not Preferences.AutoCheckDatabaseUpdates
        or not Config.CanCheck(ckDatabase) then
        Exit(False);
      Config.RecordCheck(ckDatabase);
    finally
      Config.Free;
    end;
    UpdateMgr := TUpdateMgr.Create(TAppInfo.AppDataDir);
    try
      if UpdateMgr.CheckForUpdates in [uqUpToDate, uqError] then
        Exit(False);
      Content := TArray<string>.Create(sContent1, sContent2);
      TaskCallback :=
        procedure
        begin
          TUpdateDlg.Execute(nil);
        end;
      N := TNotificationData.Create(
        sTitle, Content, HelpKeyword, TaskCallback, sUpdatePrompt
      );
      Result := True;
    finally
      UpdateMgr.Free;
    end
  except
    // Swallow any exception and return false to indicate "no update"
    Result := False;
  end;
end;

{ TUpdateCheckerMgr }

constructor TUpdateCheckerMgr.Create;
begin
  inherited Create;
  fThreads := TThreadGroup.Create;
  fThreads.Add([
    TProgramUpdateCheckerThread.Create(1000),  // begins work after 10s delay
    TDatabaseUpdateCheckerThread.Create(2000)  // begins work after 20s delay
  ]);
  fThreads.SetPriorities(tpLowest);
end;

destructor TUpdateCheckerMgr.Destroy;
begin
  fThreads.Free;
  inherited;
end;

procedure TUpdateCheckerMgr.StartThreads;
begin
  fThreads.Start;
end;

procedure TUpdateCheckerMgr.StopThreads;
begin
  fThreads.Terminate;
end;

{ TUpdateCheckerConfig }

function TUpdateCheckerConfig.CanCheck(Kind: TCheckKind): Boolean;
begin
  Result := DaysBetween(NowGMT, fLastUpdateCheck[Kind]) > 0;
end;

constructor TUpdateCheckerConfig.Create;
var
  Storage: ISettingsSection;
  K: TCheckKind;
  Value: string;
begin
  inherited Create;
  Storage := Settings.ReadSection(ssUpdateChecks);
  for K := Low(TCheckKind) to High(TCheckKind) do
  begin
    Value := Storage.ItemValues[ValueName(K)];
    if Value <> '' then
      fLastUpdateCheck[K] := ParseDate(Storage.ItemValues[ValueName(K)])
    else
      fLastUpdateCheck[K] := EncodeDate(1899, 12, 30);
  end;
end;

destructor TUpdateCheckerConfig.Destroy;
var
  Storage: ISettingsSection;
  K: TCheckKind;
begin
  Storage := Settings.EmptySection(ssUpdateChecks);
  for K := Low(TCheckKind) to High(TCheckKind) do
    Storage.ItemValues[ValueName(K)] := FormatDate(fLastUpdateCheck[K]);
  Storage.Save;
  inherited;
end;

function TUpdateCheckerConfig.FormatDate(const DT: TDateTime): string;
begin
  // In config file we store date in SQL date format
  Result := FormatDateTime('yyyy"-"mm"-"dd" "hh":"nn":"ss', DT);
end;

function TUpdateCheckerConfig.ParseDate(const S: string): TDateTime;
begin
  Result := EncodeDate(
    StrToInt(StrSlice(S, 1, 4)),
    StrToInt(StrSlice(S, 6, 2)),
    StrToInt(StrSlice(S, 9, 2))
  )
  +
  EncodeTime(
    StrToInt(StrSlice(S, 12, 2)),
    StrToInt(StrSlice(S, 15, 2)),
    StrToInt(StrSlice(S, 18, 2)),
    0
  );
end;

procedure TUpdateCheckerConfig.RecordCheck(Kind: TCheckKind);
begin
  fLastUpdateCheck[Kind] := NowGMT;
end;

function TUpdateCheckerConfig.ValueName(const Kind: TCheckKind): string;
const
  NameMap: array[TCheckKind] of string = (
    'Program', 'Database'
  );
  NameFmt = 'Last%sCheck';
begin
  Result := Format(NameFmt, [NameMap[Kind]]);
end;

end.

