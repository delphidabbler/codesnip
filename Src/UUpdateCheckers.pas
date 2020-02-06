{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2014, Peter Johnson (www.delphidabbler.com).
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
  ExtActns,
  // Project
  Notifications.UData, Notifications.URecorderThread, UThreadGroup;


type
  ///  <summary>Abstract base class for threads that check for some update
  ///  and create a notification when an update is available that is added to
  ///  the notification queue.</summary>
  TUpdateCheckerThread = class abstract(TNotificationRecorderThread)
  strict private
    ///  <summary>Checks if it is OK to check for updates.</summary>
    ///  <remarks>Result depends on user's setting and time since last update
    ///  check.</remarks>
    function CanUpdate: Boolean;
    ///  <summary>Records date and time of latest update check.</summary>
    ///  <remarks>The recorded date/time is used by future calls to CanUpdate.
    ///  </remarks>
    procedure RecordUpdate;
  strict protected
    ///  <summary>Returns the frequency of update checks, in days.</summary>
    ///  <remarks>A return value of zero indicates that no checks should be
    ///  made.</remarks>
    function UpdateFrequency: Word; virtual; abstract;
    ///  <summary>Returns the name of the value in the 'UpdateChecks' settings
    ///  section that stores the date the last update check was made.</summary>
    function LastUpdateSettingsName: string; virtual; abstract;
    ///  <summary>Checks to see if an update is available and, if so, creates a
    ///  notification record with details of how to obtain the update.</summary>
    ///  <param name="N">TNotificationData [out] The required notification
    ///  record if an update is available. Undefined otherwise.</param>
    ///  <returns>Boolean. True if an update is available or False if not.
    ///  </returns>
    function DoCheck(out N: TNotificationData): Boolean; virtual; abstract;
    ///  <summary>Finds if an update is available and creates a notification
    ///  containing information about it if so.</summary>
    ///  <param name="N">TNotificationData [out] If an update is available this
    ///  parameter contains information about the update to be added to the
    ///  notification queue. If no update is available this value is undefined.
    ///  </param>
    ///  <returns>Boolean. True if an update is a available and a notification
    ///  record was created or False if no update is available.</returns>
    ///  <remarks>False is also returned if an error was encountered, if
    ///  automatic update check is disabled or if unsufficient time has
    ///  ellapsed since the last check.</remarks>
    function GetNotification(out N: TNotificationData): Boolean; override;
  end;

type
  ///  <summary>Thread that checks online to find if a new version of the online
  ///  code snippets database is available.</summary>
  ///  <remarks>
  ///  <para>No check is made if the user has switched the automatic database
  ///  update checking facility off in preferences or if a check has been made
  ///  within the time period specified by the user.
  ///  </para>
  ///  <para>The thread fails silently as if no update is available if any
  ///  errors occur when accessing the internet or web service.</para>
  ///  <para>If an update is found a notification containing the details is
  ///  placed in the notification queue to await display.</para>
  ///  </remarks>
  TDatabaseUpdateCheckerThread = class sealed(TUpdateCheckerThread)
  strict protected
    ///  <summary>Returns the frequency of update checks, in days.</summary>
    ///  <remarks>A return value of zero indicates that no checks should be
    ///  made.</remarks>
    function UpdateFrequency: Word; override;
    ///  <summary>Returns the name of the value in the 'UpdateChecks' settings
    ///  section that stores the date the last update check was made.</summary>
    function LastUpdateSettingsName: string; override;
    ///  <summary>Checks to see if a database update is available and, if so,
    ///  creates a notification record with details of how to obtain the update.
    ///  </summary>
    ///  <param name="N">TNotificationData [out] The required notification
    ///  record if an update is available. Undefined otherwise.</param>
    ///  <returns>Boolean. True if an update is available or False if not.
    ///  </returns>
    function DoCheck(out N: TNotificationData): Boolean; override;
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


implementation


uses
  // Delphi
  SysUtils, Classes, DateUtils,
  // Project
  FmDBUpdateDlg, UAppInfo, UPreferences, USettings,
  UDBUpdateMgr, UUtils;


{ TUpdateCheckerThread }

function TUpdateCheckerThread.CanUpdate: Boolean;
var
  Frequency: Word;
  Storage: ISettingsSection;
  LastUpdateCheck: TDateTime;
begin
  Frequency := UpdateFrequency;
  if Frequency = 0 then
    Exit(False);
  Storage := Settings.ReadSection(ssUpdateChecks);
  LastUpdateCheck := Storage.GetDateTime(LastUpdateSettingsName);
  Result := DaysBetween(NowGMT, LastUpdateCheck) >= Frequency;
end;

function TUpdateCheckerThread.GetNotification(out N: TNotificationData):
  Boolean;
begin
  try
    if not CanUpdate then
      Exit(False);
    Result := DoCheck(N);
    RecordUpdate;
  except
    // Swallow any exception and return false to indicate "no update"
    Result := False;
  end;
end;

procedure TUpdateCheckerThread.RecordUpdate;
var
  Storage: ISettingsSection;
begin
  Storage := Settings.ReadSection(ssUpdateChecks);
  Storage.SetDateTime(LastUpdateSettingsName, NowGMT);
  Storage.Save;
end;

{ TDatabaseUpdateCheckerThread }

function TDatabaseUpdateCheckerThread.DoCheck(
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
  UpdateMgr: TDBUpdateMgr;
  Content: TArray<string>;
  TaskCallback: TProc;
begin
  UpdateMgr := TDBUpdateMgr.Create(TAppInfo.AppDataDir, 'Auto');
  try
    if UpdateMgr.CheckForUpdates in [uqUpToDate, uqError] then
      Exit(False);
    Content := TArray<string>.Create(sContent1, sContent2);
    TaskCallback :=
      procedure
      begin
        TDBUpdateDlg.Execute(nil);
      end;
    N := TNotificationData.Create(
      sTitle, Content, HelpKeyword, TaskCallback, sUpdatePrompt
    );
    Result := True;
  finally
    UpdateMgr.Free;
  end
end;

function TDatabaseUpdateCheckerThread.LastUpdateSettingsName: string;
begin
  Result := 'LastDatabaseCheck';
end;

function TDatabaseUpdateCheckerThread.UpdateFrequency: Word;
begin
  Result := Preferences.AutoCheckDatabaseFrequency;
end;

{ TUpdateCheckerMgr }

constructor TUpdateCheckerMgr.Create;
begin
  inherited Create;
  fThreads := TThreadGroup.Create;
  fThreads.Add([
//    TProgramUpdateCheckerThread.Create(10000),  // begins work after 10s delay
    TDatabaseUpdateCheckerThread.Create(20000)  // begins work after 20s delay
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

end.

