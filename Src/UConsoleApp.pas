{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * A class that encapsulates and executes a command line application and
 * optionally redirects the application's standard input, output and error.
}


unit UConsoleApp;


interface


uses
  // Delphi
  Classes, Windows;


const
  // Constants for working in milliseconds
  cOneSecInMS = 1000;               // one second in milliseconds
  cOneMinInMS = 60 * cOneSecInMS;   // one minute in milliseconds
  // Default values for some TConsoleApp properties
  cDefTimeSlice = 50;               // default time slice allocated to app
  cDefMaxExecTime = cOneMinInMS;    // maximum execution time of app


type

  {
  TConsoleApp:
    Class that encapsulates and executes a command line application and
    optionally redirects the application's standard input, output and error. The
    application is excuted in time slices and the class triggers an event
    between time slices.
  }
  TConsoleApp = class(TObject)
  strict private
    fOnWork: TNotifyEvent;
      {References OnWork event handler}
    fStdIn: THandle;
      {Handle of Console app's redirected standard input or 0 if not redirected}
    fStdOut: THandle;
      {Handle of Console app's redirected standard output or 0 if not
      redirected}
    fStdErr: THandle;
      {Handle of Console app's redirected standard error or 0 if not redirected}
    fExitCode: LongWord;
      {Exit code returned from console app}
    fMaxExecTime: LongWord;
      {Maximum execution time of console app}
    fProcessHandle: THandle;
      {Process handle of console app. 0 when no app is executing}
    fErrorMessage: string;
      {Description of any error that occured while trying to execute
      application}
    fErrorCode: LongWord;
      {Code of any error that occured while trying to execute application}
    fVisible: Boolean;
      {Whether application is to be visible or hidden}
    fTimeSlice: Integer;
      {Time to let application run before generating each OnWork event}
    function MonitorProcess: Boolean;
      {Monitors a running process, triggering event at end of each timeslice.
        @return True on successful completion or false if application times out.
      }
    function SetExitCode: Boolean;
      {Sets ExitCode property to value returned from application.
        @return True if exit code retrieved OK and false if we fail to retrieve
          it.
      }
    procedure TriggerWorkEvent;
      {Triggers OnWork event if assigned.
      }
    procedure SetMaxExecTime(const Value: LongWord);
      {Sets MaxExecTime property.
        @param Value Required time in miliseconds. If 0 then property's default
          value is used.
      }
    procedure SetTimeSlice(const Value: Integer);
      {Sets TimeSlice property.
        @param Value Required time in miliseconds. If 0 then property's default
          value is used.
      }
  strict protected
    function StartProcess(const CmdLine, CurrentDir: string;
      out ProcessInfo: TProcessInformation): Boolean;
      {Starts a process and gets information about it from OS.
        @param CmdLine Command line to be executed.
        @param CurrentDir Application's current directory.
        @param ProcessInfo Passes OS process info back to caller.
        @return True if process created OK and false if process couldn't be
          started.
      }
    procedure DoWork; virtual;
      {Overridable method called between program timeslices and after
      completion. Simply triggers OnWorkEvent.
      }
    procedure RecordAppError(const Code: LongWord; const Msg: string);
      {Set error code and message to class-defined error. Error code has bit 29
      set.
        @param ErrorCode Required error code. Bit 29 will be forced set.
        @param Msg Required error message.
      }
    procedure RecordWin32Error;
      {Set error code message to the last-reported Windows error.
      }
    procedure ResetError;
      {Resets error code and message to "no error" values of 0 and empty string.
      }
  public
    constructor Create;
      {Class constructor. Instantiates object.
      }
    function Execute(const CmdLine, CurrentDir: string): Boolean;
      {Executes given command line and returns whether application succeeds
      fails.
        @param CmdLine Command line to execute.
        @param CurrentDir Current directory of application.
        @return True if command line application succeeds or false if it fails.
      }
    property StdIn: THandle read fStdIn write fStdIn default 0;
      {Handle of console app's redirected standard input. Leave as 0 if standard
      input is not to be redirected. Ensure handle is inheritable}
    property StdOut: THandle read fStdOut write fStdOut default 0;
      {Handle of console app's redirected standard output. Leave as 0 if
      standard output not to be redirected. Ensure handle is inheritable}
    property StdErr: THandle read fStdErr write fStdErr default 0;
      {Handle of console app's redirected standard error. Leave as 0 if standard
      error is not to be redirected. Ensure handle is inheritable}
    property Visible: Boolean read fVisible write fVisible default False;
      {Determines whether console app is to be displayed of not}
    property MaxExecTime: LongWord read fMaxExecTime write SetMaxExecTime
      default cDefMaxExecTime;
      {Maximum execution time of console app in miliseconds}
    property TimeSlice: Integer read fTimeSlice write SetTimeSlice
      default cDefTimeSlice;
      {Timeslice allocated to console app in miliseconds. The app is paused at
      end of each time slice and Onwork event is triggered}
    property ProcessHandle: THandle read fProcessHandle;
      {Handle of executing console app. Zero when no application executing}
    property ExitCode: LongWord read fExitCode;
      {Exit code set by console app. Not valid if ErrorCode is non zero. Refer
      to console application documentation for meaning of these codes}
    property ErrorCode: DWORD read fErrorCode;
      {Zero if application executes successfully and non-zero if there was an
      error executing the application (e.g. if it timed out). Error codes either
      correspond to Windows error or are set by the class. Class generated error
      codes have bit 29 set}
    property ErrorMessage: string read fErrorMessage;
      {Error message corresponding to ErrorCode. '' if ErrorCode = 0}
    property OnWork: TNotifyEvent read fOnWork write fOnWork;
      {Event triggered each time console application signals the class.
      Frequency of these events depends on TimeSlice}
  end;


implementation


uses
  // Delphi
  SysUtils, Forms;


const
  // Mask that is ORd with application error codes: according to Windows API
  // docs, error codes with bit 29 set are reserved for application use.
  cAppErrorMask = 1 shl 29;


resourcestring
  // Error message
  sErrTimeout = 'Application timed out';


{ TConsoleApp }

constructor TConsoleApp.Create;
  {Class constructor. Instantiates object.
  }
begin
  inherited Create;
  // Set default property values
  fMaxExecTime := cDefMaxExecTime;
  fTimeSlice := cDefTimeSlice;
  fVisible := False;
  fStdIn := 0;
  fStdOut := 0;
  fStdErr := 0;
end;

procedure TConsoleApp.DoWork;
  {Overridable method called between program timeslices and after completion.
  Simply triggers OnWorkEvent.
  }
begin
  TriggerWorkEvent;
end;

function TConsoleApp.Execute(const CmdLine, CurrentDir: string): Boolean;
  {Executes given command line and returns whether application succeeds
  fails.
    @param CmdLine Command line to execute.
    @param CurrentDir Current directory of application.
    @return True if command line application succeeds or false if it fails.
  }
var
  ProcessInfo: TProcessInformation; // information about process
begin
  fExitCode := 0;
  ResetError;
  Result := StartProcess(CmdLine, CurrentDir, ProcessInfo);
  if Result then
  begin
    // Process started: monitor its progress
    try
      fProcessHandle := ProcessInfo.hProcess;
      Result := MonitorProcess and SetExitCode;
    finally
      // Process ended: tidy up
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
      fProcessHandle := 0;
    end;
  end
  else
  begin
    // Couldn't start process: error
    RecordWin32Error;
    fProcessHandle := 0;
  end;
end;

function TConsoleApp.MonitorProcess: Boolean;
  {Monitors a running process, triggering event at end of each timeslice.
    @return True on successful completion or false if application times out.
  }
var
  TimeToLive: Integer;  // Milliseconds app has left before timing out
  AppState: DWORD;      // State of app after last wait
begin
  Result := True;
  TimeToLive := fMaxExecTime;
  repeat
    // Pause and wait for app - length determined by TimeSlice property
    AppState := WaitForSingleObject(fProcessHandle, fTimeSlice);
    Application.ProcessMessages;
    Dec(TimeToLive, fTimeSlice);
    if AppState = WAIT_FAILED then
    begin
      RecordWin32Error;
      Result := False;
    end
    else
      // All OK: do inter-timeslice processing
      DoWork;
  until (AppState <> WAIT_TIMEOUT) or (TimeToLive <= 0);
  // App halted or timed out: check which
  if AppState = WAIT_TIMEOUT then
  begin
    RecordAppError(1, sErrTimeout);
    Result := False;
  end;
end;

procedure TConsoleApp.RecordAppError(const Code: LongWord;
  const Msg: string);
  {Set error code and message to class-defined error. Error code has bit 29
  set.
    @param ErrorCode Required error code. Bit 29 will be forced set.
    @param Msg Required error message.
  }
begin
  fErrorMessage := Msg;
  fErrorCode := Code or cAppErrorMask;
end;

procedure TConsoleApp.RecordWin32Error;
  {Set error code message to the last-reported Windows error.
  }
begin
  fErrorCode := GetLastError;
  fErrorMessage := SysErrorMessage(fErrorCode);
end;

procedure TConsoleApp.ResetError;
  {Resets error code and message to "no error" values of 0 and empty string.
  }
begin
  fErrorCode := 0;
  fErrorMessage := '';
end;

function TConsoleApp.SetExitCode: Boolean;
  {Sets ExitCode property to value returned from application.
    @return True if exit code retrieved OK and false if we fail to retrieve it.
  }
begin
  Result := GetExitCodeProcess(fProcessHandle, fExitCode);
  if not Result then
    RecordWin32Error;
end;

procedure TConsoleApp.SetMaxExecTime(const Value: LongWord);
  {Sets MaxExecTime property.
    @param Value Required time in miliseconds. If 0 then property's default
      value is used.
  }
begin
  if Value = 0 then
    fMaxExecTime := cDefMaxExecTime
  else
    fMaxExecTime := Value;
end;

procedure TConsoleApp.SetTimeSlice(const Value: Integer);
  {Sets TimeSlice property.
    @param Value Required time in miliseconds. If 0 then property's default
      value is used.
  }
begin
  if Value > 0 then
    fTimeSlice := Value
  else
    fTimeSlice := cDefTimeSlice;
end;

function TConsoleApp.StartProcess(const CmdLine, CurrentDir: string;
  out ProcessInfo: TProcessInformation): Boolean;
  {Starts a process and gets information about it from OS.
    @param CmdLine Command line to be executed.
    @param CurrentDir Application's current directory.
    @param ProcessInfo Passes OS process info back to caller.
    @return True if process created OK and false if process couldn't be started.
  }
const
  // Maps Visible property to required wondows flags
  cShowFlags: array[Boolean] of Integer = (SW_HIDE, SW_SHOW);
var
  StartInfo: TStartupInfo;  // information about process from OS
  SafeCmdLine: string;      // command line: safe for CreateProcessW
  CreateFlags: DWORD;       // process creation flags
begin
  // Set up startup information structure
  FillChar(StartInfo, Sizeof(StartInfo),#0);
  with StartInfo do
  begin
    cb := SizeOf(StartInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    if (fStdIn <> 0) or (fStdOut <> 0) or (fStdErr <> 0) then
      dwFlags := dwFlags or STARTF_USESTDHANDLES; // we are redirecting
    hStdInput := fStdIn;                  // std handles (non-zero => redirect)
    hStdOutput := fStdOut;
    hStdError := fStdErr;
    wShowWindow := cShowFlags[fVisible];  // show or hide window
  end;
  // Make CmdLine parameter safe for passing to CreateProcess (Delphi 2009
  // and later). Need to ensure memory space is writeable because of issue with
  // CreateProcessW. Problem does not exist with CreateProcessA.
  // Without the following code this problem would arise if this method was
  // called with a constant or string with -1 reference count as the CmdLine
  // parameter.
  // See http://msdn.microsoft.com/en-us/library/ms682425.aspx for an
  // explanation of the problem: look under the lpCommandLine parameter section.
  // Remy Lebeau suggested the workaround used below in his post to
  // https://forums.codegear.com/thread.jspa?threadID=12826
  SafeCmdLine := CmdLine;
  UniqueString(SafeCmdLine);
  // Set up creation flags: special flag used to determine type of environment
  // block passed to app: Unicode or ANSI.
  CreateFlags := CREATE_UNICODE_ENVIRONMENT;
  // Try to create the process
  Result := CreateProcess(
    nil,
    PChar(SafeCmdLine),
    nil,
    nil,
    True,
    CreateFlags,
    nil,
    PChar(CurrentDir),
    StartInfo,
    ProcessInfo
  );
end;

procedure TConsoleApp.TriggerWorkEvent;
  {Triggers OnWork event if assigned.
  }
begin
  if Assigned(fOnWork) then
    fOnWork(Self);
end;

end.

