{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that executes a thread and displays a dialog box if thread
 * takes more than a specified time to complete.
}


unit UWaitForThreadUI;


interface


uses
  // Delphi
  SysUtils, Classes, ExtCtrls, Forms, SyncObjs,
  // Project
  UBaseObjects;


type

  {
  TWaitForThreadUI:
    Class that executes a thread and displays a dialog box if thread takes more
    than a specified time to complete.
  }
  TWaitForThreadUI = class(TNoPublicConstructObject)
  strict private
    const DefaultPauseBeforeDisplay = 50;
    const DefaultMinDisplayTime = 1000;
    var fThread: TThread;               // Thread to be executed
    var fForm: TForm;                   // Form to be displayed if required
    var fMinDisplayTimer: TTimer;       // Timer to delay closure of form
    var fSaveOnTerminate: TNotifyEvent; // Saves fThread.OnTerminate value
    var fSaveFormShow: TNotifyEvent;    // Saves fForm.OnShow value
    var fFormCloseRequested: Boolean;   // Indicates if form is awaiting closure
    var fPauseBeforeDisplay: Cardinal;  // Time before fForm is displayed
    var fThreadTerminated: Boolean;     // Indicates if fThread has terminated
    var fMinTimeElapsed: Boolean;       // True when form displayed long enough
    class var fLock: TCriticalSection;  // Protects form opening / closing
    procedure ThreadTerminated(Sender: TObject);
      {Handles thread's OnTerminate event. Flags thread has terminated and
      requests closure of any displayed form.
        @param Sender [in] Not used.
      }
    procedure MinTimeElapsed(Sender: TObject);
      {Timer event handler called once, when minimum execution time has elapsed.
        @param Sender [in] Not used.
      }
    procedure ShowForm;
      {Displays required form. A critical section ensures that only one thread
      at a time can attempt to show the form.
      }
    procedure CloseForm;
      {Closes any displayed form if thread has terminated and minimum display
      time has elapsed. A critical section ensures that only one thread at a
      time can attempt to close the form.
      }
    procedure FormShowHandler(Sender: TObject);
      {Event handler for fForm.OnShow event. Checks if form can open.
        @param Sender [in] Reference to form triggering event.
      }
    procedure Pause;
      {Delays execution by fPauseBeforeDisplay ms.
      }
  strict protected
    constructor InternalCreate(const AThread: TThread;
      const AForm: TForm; const APauseBeforeDisplay, AMinDisplayTime: Cardinal);
      {Object constructor. Sets up object.
        @param AThread [in] Thread to be executed.
        @param AForm [in] Form to be displayed while thread executes.
        @param APauseBeforeDisplay [in] Time thread given to execute before form
          is displayed (optional)
        @param AMinDisplayTime [in] Minimum time to display form (optional).
      }
    procedure Execute;
      {Executes thread and displays dialog box if necessary.
      }
  public
    class constructor Create;
      {Class constructor. Initialises critical section.
      }
    class destructor Destroy;
      {Class destructor. Destroys critical section.
      }
    destructor Destroy; override;
      {Object destructor. Tears down object.
      }
    class procedure Run(const AThread: TThread; const AForm: TForm;
      const APauseBeforeDisplay: Cardinal = DefaultPauseBeforeDisplay;
      const AMinDisplayTime: Cardinal = DefaultMinDisplayTime); overload;
      {Creates and executes a TWaitForThreadUI object that executes a thread,
      displaying dialog box if necessary.
        @param AThread [in] Thread to be executed.
        @param AForm [in] Form to be displayed while thread executes.
        @param APauseBeforeDisplay [in] Time thread given to execute before form
          is displayed (optional)
        @param AMinDisplayTime [in] Minimum time to display form (optional).
      }
    class procedure Run(const AProc: TProc; const AIsThreadSafe: Boolean;
      const AForm: TForm;
      const APauseBeforeDisplay: Cardinal = DefaultPauseBeforeDisplay;
      const AMinDisplayTime: Cardinal = DefaultMinDisplayTime); overload;
      {Creates and executes a TWaitForThreadUI object that executes a closure in
      a thread, displaying dialog box if necessary.
        @param AProc [in] Closure to be executed in thread.
        @param AIsThreadSage [in] Indicates if AProc is thread safe. If True
          AProc is excuted in context of thread. If False closure is executed
          in context of main thread.
        @param AForm [in] Form to be displayed while thread executes.
        @param APauseBeforeDisplay [in] Time thread given to execute before form
          is displayed (optional)
        @param AMinDisplayTime [in] Minimum time to display form (optional).
      }
  end;


implementation


uses
  // Project
  UExceptions, UUtils;


type
  ///  <summary>Thread class that executes a given closure.</summary>
  TProcThread = class(TThread)
  strict private
    var
      ///  <summary>Closure to executed.</summary>
      fProc: TProc;
      ///  <summary>Flag indicating if closure is thread safe.</summary>
      fProcIsThreadSafe: Boolean;
  strict protected
    ///  <summary>Executes closure from thread.</summary>
    procedure Execute; override;
  public
    ///  <summary>Constructs a new, suspended, thread instance.</summary>
    ///  <param name="Proc">TProc [in] Closure to be executed by thread.</param>
    ///  <param name="ProcIsThreadSafe">Boolean [in] Flag indicating if Proc is
    ///  thread safe or not.</param>
    ///  <remarks>If Proc is not thread safe it is executed in the context of
    ///  the main thread.</remarks>
    constructor Create(const Proc: TProc; const ProcIsThreadSafe: Boolean);
  end;


{ TWaitForThreadUI }

procedure TWaitForThreadUI.CloseForm;
  {Closes any displayed form if thread has terminated and minimum display time
  has elapsed. A critical section ensures that only one thread at a time can
  attempt to close the form.
  }
begin
  fLock.Acquire;
  try
    if not Assigned(fForm) then
      Exit;
    fFormCloseRequested := fThreadTerminated and fMinTimeElapsed;
    if fFormCloseRequested and fForm.Visible then
      fForm.Close;
  finally
    fLock.Release;
  end;
end;

class constructor TWaitForThreadUI.Create;
  {Class constructor. Initialises critical section.
  }
begin
  fLock := TCriticalSection.Create;
end;

destructor TWaitForThreadUI.Destroy;
  {Object destructor. Tears down object.
  }
begin
  fMinDisplayTimer.Free;
  fThread.OnTerminate := fSaveOnTerminate;
  inherited;
end;

class destructor TWaitForThreadUI.Destroy;
  {Class destructor. Destroys critical section.
  }
begin
  fLock.Free;
end;

procedure TWaitForThreadUI.Execute;
  {Executes thread and displays dialog box if necessary.
  }
begin
  // Flag that form closure not yet requested
  fFormCloseRequested := False;
  // Reset termination flags
  fThreadTerminated := False;
  fMinTimeElapsed := False;
  // Execute thread
  fThread.OnTerminate := ThreadTerminated;
  fThread.Start;
  // Pause before displayng dialog by PauseBeforeDisplay ms
  Pause;
  if not fThread.Finished then
    // Show dialog if thread not completed. Dialog blocks until thread
    // terminates.
    ShowForm;
  // Re-raise any exception that was raised by thread: must be a clone and not
  // the original exception.
  if Assigned(fThread.FatalException)
    and (fThread.FatalException is Exception) then
    raise TExceptionHelper.Clone(fThread.FatalException as Exception);
end;

procedure TWaitForThreadUI.FormShowHandler(Sender: TObject);
  {Event handler for fForm.OnShow event. Checks if form can open.
    @param Sender [in] Reference to form triggering event.
  }
begin
  // Call any earler OnShow handler for form
  if Assigned(fSaveFormShow) then
    fSaveFormShow(Sender);
  // If form has already been requested to close then close it
  if fFormCloseRequested then
    fForm.Close;
end;

constructor TWaitForThreadUI.InternalCreate(const AThread: TThread;
  const AForm: TForm; const APauseBeforeDisplay, AMinDisplayTime: Cardinal);
  {Object constructor. Sets up object.
    @param AThread [in] Thread to be executed.
    @param AForm [in] Form to be displayed while thread executes.
    @param APauseBeforeDisplay [in] Time thread given to execute before form is
      displayed (optional)
    @param AMinDisplayTime [in] Minimum time to display form (optional).
  }
begin
  inherited InternalCreate;
  // Set up timer that triggers event when form can close
  fMinDisplayTimer := TTimer.Create(nil);
  fMinDisplayTimer.Enabled := False;
  fMinDisplayTimer.OnTimer := MinTimeElapsed;
  fMinDisplayTimer.Interval := AMinDisplayTime;
  // Record other fields
  fPauseBeforeDisplay := APauseBeforeDisplay;
  fThread := AThread;
  fSaveOnTerminate := AThread.OnTerminate;
  fForm := AForm;
end;

procedure TWaitForThreadUI.MinTimeElapsed(Sender: TObject);
  {Timer event handler called once, when minimum execution time has elapsed.
    @param Sender [in] Not used.
  }
begin
  // Switch off timer
  fMinDisplayTimer.OnTimer := nil;
  fMinDisplayTimer.Enabled := False;
  fMinTimeElapsed := True;
  // Close form
  CloseForm;
end;

procedure TWaitForThreadUI.Pause;
  {Delays execution by fPauseBeforeDisplay ms.
  }
begin
  if fPauseBeforeDisplay = 0 then
    Exit;
  // Busy wait
  UUtils.Pause(fPauseBeforeDisplay);
end;

class procedure TWaitForThreadUI.Run(const AProc: TProc;
  const AIsThreadSafe: Boolean; const AForm: TForm;
  const APauseBeforeDisplay, AMinDisplayTime: Cardinal);
  {Creates and executes a TWaitForThreadUI object that executes a closure in a
  thread, displaying dialog box if necessary.
    @param AProc [in] Closure to be executed in thread.
    @param AIsThreadSage [in] Indicates if AProc is thread safe. If True AProc
      is excuted in context of thread. If False closure is executed in context
      of main thread.
    @param AForm [in] Form to be displayed while thread executes.
    @param APauseBeforeDisplay [in] Time thread given to execute before form is
      displayed (optional)
    @param AMinDisplayTime [in] Minimum time to display form (optional).
  }
var
  ProcThread: TProcThread;  // thread used to execute AProc
begin
  ProcThread := TProcThread.Create(AProc, AIsThreadSafe);
  try
    Run(ProcThread, AForm, APauseBeforeDisplay, AMinDisplayTime);
  finally
    ProcThread.Free;
  end;
end;

class procedure TWaitForThreadUI.Run(const AThread: TThread;
  const AForm: TForm; const APauseBeforeDisplay, AMinDisplayTime: Cardinal);
  {Creates and executes a TWaitForThreadUI object that executes a thread,
  displaying dialog box if necessary.
    @param AThread [in] Thread to be executed.
    @param AForm [in] Form to be displayed while thread executes.
    @param APauseBeforeDisplay [in] Time thread given to execute before form is
      displayed (optional)
    @param AMinDisplayTime [in] Minimum time to display form (optional).
  }
var
  Instance: TWaitForThreadUI;
begin
  Assert(Assigned(AThread), ClassName + '.Run: AThread is nil');
  Instance := InternalCreate(
    AThread, AForm, APauseBeforeDisplay, AMinDisplayTime
  );
  try
    Instance.Execute;
  finally
    Instance.Free;
  end;
end;

procedure TWaitForThreadUI.ShowForm;
  {Displays required form. A critical section ensures that only one thread at a
  time can attempt to show the form.
  }
begin
  fLock.Acquire;
  try
    if not Assigned(fForm) then
      Exit;
    // Start timer that triggers when form is allowed to close after minimum
    // display time. If time is zero we need to trigger ourselves, because
    // timer won't fire.
    fMinDisplayTimer.Enabled := True;
    if fMinDisplayTimer.Interval = 0 then
      MinTimeElapsed(fMinDisplayTimer);
    // Store any existing OnShow event of form and replace with our handler
    fSaveFormShow := fForm.OnShow;
    fForm.OnShow := FormShowHandler;
    try
      // Show form modally if not already requested to close
      if not fFormCloseRequested then
        fForm.ShowModal;
    finally
      // Restore any previous OnShow event handler
      fForm.OnShow := fSaveFormShow;
    end;
  finally
    fLock.Release;
  end;
end;

procedure TWaitForThreadUI.ThreadTerminated(Sender: TObject);
  {Handles thread's OnTerminate event. Flags thread has terminated and requests
  closure of any displayed form.
    @param Sender [in] Not used.
  }
begin
  fThreadTerminated := True;
  CloseForm;
  if Assigned(fSaveOnTerminate) then
    fSaveOnTerminate(fThread);
end;

{ TProcThread }

constructor TProcThread.Create(const Proc: TProc;
  const ProcIsThreadSafe: Boolean);
begin
  Assert(Assigned(Proc), ClassName + '.Create: Proc is nil');
  inherited Create(True);
  fProc := Proc;
  fProcIsThreadSafe := ProcIsThreadSafe;
end;

procedure TProcThread.Execute;
begin
  if fProcIsThreadSafe then
    fProc
  else
    Synchronize(procedure begin fProc; end);
end;

end.

