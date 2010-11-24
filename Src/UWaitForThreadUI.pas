{
 * UWaitForThreadUI.pas
 *
 * Implements a class that executes a thread and displays a dialog box if thread
 * takes more than a specified time to complete.
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
 * The Original Code is UWaitForThreadUI.pas, formerly UWaitForActionUI.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UWaitForThreadUI;


interface


uses
  // Delphi
  Classes, ExtCtrls, Forms,
  // Project
  UBaseObjects, UThreadEx;


type

  {
  TWaitForThreadUI:
    Class that executes a thread and displays a dialog box if thread takes more
    than a specified time to complete.
  }
  TWaitForThreadUI = class(TNoPublicConstructObject)
  strict private
    var fThread: TThreadEx;             // Thread to be executed
    var fForm: TForm;                   // Form to be displayed if required
    var fMinDisplayTimer: TTimer;       // Timer to delay closure of form
    var fSaveOnTerminate: TNotifyEvent; // Saves fThread.OnTerminate value
    var fSaveFormShow: TNotifyEvent;    // Saves fForm.OnShow value
    var fFormCloseRequested: Boolean;   // Indicates if form is awaiting closure
    var fPauseBeforeDisplay: Cardinal;  // Time before fForm is displayed
    var fThreadTerminated: Boolean;     // Indicates if fThread has terminated
    var fMinTimeElapsed: Boolean;       // True when form displayed long enough
    procedure ThreadTerminated(Sender: TObject);
      {Handles thread's OnTerminate event. Flags thread has terminated and
      requests closure of any displayed form.
        @param Sender [in] Not used.
      }
    procedure MinTimeElapsed(Sender: TObject);
      {Timer event handler called once, when minimum execution time has elapsed.
        @param Sender [in] Not used.
      }
    procedure ShowOrHideForm(const Show: Boolean);
      {Attempts to show or hide fForm. This method should be called instead of
      ShowForm or CloseForm. The method is run inside a critical section.
        @param Show [in] Flag that indicates whether form is to be shown (True)
          or hidden (False).
      }
    procedure ShowForm;
      {Displays required form. Should not be called directly. Always call
      ShowOrHideForm(True) instead.
      }
    procedure CloseForm;
      {Closes any displayed form if thread has terminated and minimum display
      time has elapsed. Should not be called directly. Always call
      ShowOrHideForm(False) instead.
      }
    procedure FormShowHandler(Sender: TObject);
      {Event handler for fForm.OnShow event. Checks if form can open.
        @param Sender [in] Reference to form triggering event.
      }
    procedure Pause;
      {Delays execution by fPauseBeforeDisplay ms.
      }
  strict protected
    constructor InternalCreate(const AThread: TThreadEx;
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
    destructor Destroy; override;
      {Object destructor. Tears down object.
      }
    class procedure Run(const AThread: TThreadEx; const AForm: TForm;
      const APauseBeforeDisplay: Cardinal = 50;
      const AMinDisplayTime: Cardinal = 1000);
      {Creates and executes a TWaitForThreadUI object that executes a thread,
      displaying dialog box if necessary.
        @param AThread [in] Thread to be executed.
        @param AForm [in] Form to be displayed while thread executes.
        @param APauseBeforeDisplay [in] Time thread given to execute before form
          is displayed (optional)
        @param AMinDisplayTime [in] Minimum time to display form (optional).
      }
  end;


implementation


uses
  // Delphi
  SysUtils, SyncObjs,
  // Project
  UExceptions, UUtils;


var
  // Critical section that ensures form opening / closing in TWaitForThreadUI
  // can only be run by one thread at a time
  Lock: TCriticalSection;

{ TWaitForThreadUI }

procedure TWaitForThreadUI.CloseForm;
  {Closes any displayed form if thread has terminated and minimum display time
  has elapsed. Should not be called directly. Always call ShowOrHideForm(False)
  instead.
  }
begin
  Assert(Assigned(fForm), ClassName + '.CloseForm: fForm is nil');
  Lock.Acquire;
  try
    fFormCloseRequested := fThreadTerminated and fMinTimeElapsed;
    if fFormCloseRequested and fForm.Visible then
      fForm.Close;
  finally
    Lock.Release;
  end;
end;

destructor TWaitForThreadUI.Destroy;
  {Object destructor. Tears down object.
  }
begin
  fMinDisplayTimer.Free;
  fThread.OnTerminate := fSaveOnTerminate;
  inherited;
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
  if not fThread.Completed then
    // Show dialog if thread not completed. Dialog blocks until thread
    // terminates. This call is protected by a critical section.
    ShowOrHideForm(True);
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

constructor TWaitForThreadUI.InternalCreate(const AThread: TThreadEx;
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
  ShowOrHideForm(False);  // close form (thread safe call)
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

class procedure TWaitForThreadUI.Run(const AThread: TThreadEx;
  const AForm: TForm; const APauseBeforeDisplay, AMinDisplayTime: Cardinal);
  {Creates and executes a TWaitForThreadUI object that executes a thread,
  displaying dialog box if necessary.
    @param AThread [in] Thread to be executed.
    @param AForm [in] Form to be displayed while thread executes.
    @param APauseBeforeDisplay [in] Time thread given to execute before form is
      displayed (optional)
    @param AMinDisplayTime [in] Minimum time to display form (optional).
  }
begin
  Assert(Assigned(AThread), ClassName + '.Run: AThread is nil');
  with InternalCreate(AThread, AForm, APauseBeforeDisplay, AMinDisplayTime) do
    try
      Execute;
    finally
      Free;
    end;
end;

procedure TWaitForThreadUI.ShowForm;
  {Displays required form. Should not be called directly. Always call
  ShowOrHideForm(True) instead.
  }
begin
  Assert(Assigned(fForm), ClassName + '.ShowForm: fForm is nil');
  // Start timer that triggers when form is allowed to close after minimum
  // display time. If time is zero we need to trigger ourselves, because
  // timer won't fire.
  fMinDisplayTimer.Enabled := True;
  if fMinDisplayTimer.Interval = 0 then
    MinTimeElapsed(fMinDisplayTimer);
  // Store any existing OnShow event of form
  fSaveFormShow := fForm.OnShow;
  fForm.OnShow := FormShowHandler;
  try
    // Show form modally if not already requested to close
    if not (fFormCloseRequested) then
      fForm.ShowModal;
  finally
    // Restore any previous OnShow event handler
    fForm.OnShow := fSaveFormShow;
  end;
end;

procedure TWaitForThreadUI.ShowOrHideForm(const Show: Boolean);
  {Attempts to show or hide fForm. This method should be called instead of
  ShowForm or CloseForm. The method is run inside a critical section.
    @param Show [in] Flag that indicates whether form is to be shown (True) or
      hidden (False).
  }
begin
  Lock.Acquire;
  try
    if Assigned(fForm) then
    begin
      if Show then
        ShowForm
      else
        CloseForm;
    end;
  finally
    Lock.Release;
  end;
end;

procedure TWaitForThreadUI.ThreadTerminated(Sender: TObject);
  {Handles thread's OnTerminate event. Flags thread has terminated and requests
  closure of any displayed form.
    @param Sender [in] Not used.
  }
begin
  fThreadTerminated := True;
  ShowOrHideForm(False);  // close form (thread safe call)
  if Assigned(fSaveOnTerminate) then
    fSaveOnTerminate(fThread);
end;

initialization

// create critical section
Lock := TCriticalSection.Create;

finalization

// disposes of critical section
Lock.Free;

end.

