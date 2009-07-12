{
 * UWaitForActionUI.pas
 *
 * Implements a class that executes an action in a thread and displays a dialog
 * box if action takes more than a specified time to complete.
 *
 * v1.0 of 01 Dec 2006  - Original version.
 * v1.1 of 05 Feb 2007  - Added new static Run method to TWaitForActionUI.
 *                      - Fixed bug in TWaitForActionUI.Pause that was freezing
 *                        program if PauseBeforeDisplay was not multiple of 10.
 * v1.2 of 01 Mar 2007  - Added new overloaded TWaitForActionUI.Run method that
 *                        can execute a thread instead of an action. We use new
 *                        TOwnedAction private class to do this.
 *                      - Fixed possible bug in TWaitForActionUI that fails to
 *                        close dialog if MinDisplayTime is zero.
 *                      - Changed TActionThread to descend from TThreadEx and
 *                        removed TActionThread's own Completed property in
 *                        favour of property of same name in new base class.
 * v1.3 of 24 Sep 2007  - Replaced call to Sleep routine in pause processing
 *                        with busy wait. This fixed bug that was freezing whole
 *                        system and therefore not displaying wait form.
 * v1.4 of 29 Oct 2007  - Made form closure thread safe so that timer and thread
 *                        cannot both access at same time. This was done to try
 *                        to fix occasional bug where dialog box never closes.
 * v1.5 of 02 Jun 2008  - Rewrote code from v1.4 to make form opening / closure
 *                        thread safe. Now use critical section rather than lock
 *                        count. Previous fix did not work.
 * v1.6 of 04 Oct 2008  - Made TWaitForActionUI constructor protected, Execute
 *                        method protected, one overloaded version of Run method
 *                        call other one and removed unused public properties.
 *                        static.
 *                      - Modified TOwnedAction to work with revised
 *                        TThreadWrapper class that can now only be accessed via
 *                        a static Execute method.
 *                      - Made all private and protected sections strict.
 *                      - Now use ClassName method in all assert statements.
 * v1.7 of 15 Dec 2008  - Changed TWaitForActionUI.Pause to call UUtils.Pause.
 * v1.8 of 11 Jan 2009  - Removed TWaitForActionUI.Run method overload that
 *                        takes a thread as a parameter. This caused thread to
 *                        be run in a second thread which is not required.
 *                      - Removed now unused private TOwnedAction class.
 *
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
 * The Original Code is UWaitForActionUI.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UWaitForActionUI;


interface


uses
  // Delphi
  Classes, ExtCtrls, Forms,
  // Project
  UBaseObjects;


type

  {
  TWaitForActionUI:
    Class that executes an action in a thread and displays a dialog box if
    action takes more than a specified time to complete. Any exception raised
    by action is re-raised from main thread.
  }
  TWaitForActionUI = class(TNoPublicConstructObject)
  strict private
    fMinDisplayTimer: TTimer;
      {Timer control used to delay closure of form if necessary}
    fSaveFormShow: TNotifyEvent;
      {Records any existing OnShow handler assigned to form referenced in Form
      property}
    fFormCloseRequested: Boolean;
      {Flag indicating whether form is awaiting closure}
    fAction: TBasicAction;
      {Value of Action property}
    fForm: TForm;
      {Value of Form property}
    fPauseBeforeDisplay: Cardinal;
      {Value of PauseBeforeDisplay property}
    fActionTerminated: Boolean;
      {Flag indicating if action thread has terminated}
    fMinTimeElapsed: Boolean;
      {Flag indicating if minimum form display time has elapsed}
    procedure ActionTerminated(Sender: TObject);
      {Handles action thread's OnTerminate event. Flags thread has terminated
      and requests closure of any displayed form.
        @param Sender [in] Not used.
      }
    procedure MinTimeElapsed(Sender: TObject);
      {Timer event handler called once, when minimum execution time has elapsed.
        @param Sender [in] Not used.
      }
    procedure ShowOrHideForm(const Show: Boolean);
      {Attempts to show or hide the form associated with the Form property. This
      method should be called instead of ShowForm or CloseForm. The method is
      runs inside a critical section.
        @param Show [in] Flag that indicates whether form is to be shown (True)
          or hidden (False).
      }
    procedure ShowForm;
      {Displays any form assigned to Form property. Should not be called
      directly: always call ShowOrHideForm(True) instead.
      }
    procedure CloseForm;
      {Closes any form referenced by Form property, if action thread has
      terminated and minimum display time has elapsed. Should not be called
      directly: always call ShowOrHideForm(False) instead.
      }
    procedure FormShowHandler(Sender: TObject);
      {Event handler for Form property's OnShow event. Checks if form can open.
        @param Sender [in] Reference to form triggering event.
      }
    procedure Pause;
      {Delays execution by PauseBeforeDisplay ms.
      }
  strict protected
    constructor InternalCreate(const AAction: TBasicAction; const AForm: TForm;
      const APauseBeforeDisplay, AMinDisplayTime: Cardinal);
      {Class constructor. Sets up object.
        @param AAction [in] Action to be executed.
        @param AForm [in] Form to be displayed while action executes.
        @param APauseBeforeDisplay [in] Time action given to execute before form
          is displayed (optional)
        @param AMinDisplayTime [in] Minimum time to display form (optional).
      }
    procedure Execute;
      {Executes action and displays dialog box if necessary.
      }
  public
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    class procedure Run(const AAction: TBasicAction; const AForm: TForm;
      const APauseBeforeDisplay: Cardinal = 50;
      const AMinDisplayTime: Cardinal = 1000);
      {Creates and executes a TWaitForActionUI object that executes an action,
      displaying dialog box if necessary.
        @param AAction [in] Action to be executed.
        @param AForm [in] Form to be displayed while action executes.
        @param APauseBeforeDisplay [in] Time action given to execute before form
          is displayed (optional)
        @param AMinDisplayTime [in] Minimum time to display form (optional).
      }
  end;


implementation


uses
  // Delphi
  SysUtils, SyncObjs,
  // Project
  UThreadEx, UUtils;


type

  {
  TActionThread:
    Class that executes an action in a thread.
  }
  TActionThread = class(TThreadEx)
  strict private
    fAction: TBasicAction;
      {Action to be executed in thread}
    fExceptObj: Exception;
      {Value of ExceptObj property}
    procedure DoAction;
      {Checks action is assigned and executes it if so.
      }
  strict protected
    procedure Execute; override;
      {Causes action to be executed then sets completed flag when action
      completes.
      }
  public
    constructor Create(const AAction: TBasicAction);
      {Class constructor. Sets up thread.
        @param AAction [in] Action to be executed in thread.
      }
    property ExceptObj: Exception read fExceptObj;
      {Reference to any exception that occured when action was executed. If
      there was no exception the property is nil}
  end;

var
  // Critical section that ensures form opening / closing in TWaitForActionUI
  // can only be run by one thread at a time
  Lock: TCriticalSection;

{ TWaitForActionUI }

procedure TWaitForActionUI.ActionTerminated(Sender: TObject);
  {Handles action thread's OnTerminate event. Flags thread has terminated and
  requests closure of any displayed form.
    @param Sender [in] Not used.
  }
begin
  fActionTerminated := True;
  ShowOrHideForm(False);  // close form (thread safe call)
end;

procedure TWaitForActionUI.CloseForm;
  {Closes any form referenced by Form property, if action thread has terminated
  and minimum display time has elapsed. Should not be called directly: always
  call ShowOrHideForm(False) instead.
  }
begin
  Assert(Assigned(fForm),                                  // ** do not localise
    ClassName + '.CloseForm: fForm is nil');
  Lock.Acquire;
  try
    fFormCloseRequested := fActionTerminated and fMinTimeElapsed;
    if fFormCloseRequested and fForm.Visible then
      fForm.Close;
  finally
    Lock.Release;
  end;
end;

destructor TWaitForActionUI.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fMinDisplayTimer);
  inherited;
end;

procedure TWaitForActionUI.Execute;
  {Executes action and displays dialog box if necessary.
  }
var
  Thread: TActionThread;  // thread used to execute action
begin
  // Flag that form closure not yet requested
  fFormCloseRequested := False;
  // Reset termination flags
  fActionTerminated := False;
  fMinTimeElapsed := False;
  // Execute action in thread
  Thread := TActionThread.Create(fAction);
  try
    Thread.OnTerminate := ActionTerminated;
    Thread.Resume;
    // Pause before displayng dialog by PauseBeforeDisplay ms
    Pause;
    if not Thread.Completed then
      // Show dialog if thread not completed.
      ShowOrHideForm(True); // shows form, protected by critical section
    // Re-raise any exception that was raised by action thread
    if Assigned(Thread.ExceptObj) then
      raise Thread.ExceptObj;
  finally
    FreeAndNil(Thread);
  end;
end;

procedure TWaitForActionUI.FormShowHandler(Sender: TObject);
  {Event handler for Form property's OnShow event. Checks if form can open.
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

constructor TWaitForActionUI.InternalCreate(const AAction: TBasicAction;
  const AForm: TForm; const APauseBeforeDisplay, AMinDisplayTime: Cardinal);
  {Class constructor. Sets up object.
    @param AAction [in] Action to be executed.
    @param AForm [in] Form to be displayed while action executes.
    @param APauseBeforeDisplay [in] Time action given to execute before form is
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
  fAction := AAction;
  fForm := AForm;
end;

procedure TWaitForActionUI.MinTimeElapsed(Sender: TObject);
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

procedure TWaitForActionUI.Pause;
  {Delays execution by PauseBeforeDisplay ms.
  }
begin
  if fPauseBeforeDisplay = 0 then
    Exit;
  // Busy wait
  UUtils.Pause(fPauseBeforeDisplay);
end;

class procedure TWaitForActionUI.Run(const AAction: TBasicAction;
  const AForm: TForm; const APauseBeforeDisplay, AMinDisplayTime: Cardinal);
  {Creates and executes a TWaitForActionUI object that executes an action,
  displaying dialog box if necessary.
    @param AAction [in] Action to be executed.
    @param AForm [in] Form to be displayed while action executes.
    @param APauseBeforeDisplay [in] Time action given to execute before form is
      displayed (optional)
    @param AMinDisplayTime [in] Minimum time to display form (optional).
  }
begin
  Assert(Assigned(AAction),                                // ** do not localise
    ClassName + '.Run: AAction is nil');
  with InternalCreate(AAction, AForm, APauseBeforeDisplay, AMinDisplayTime) do
    try
      Execute;
    finally
      Free;
    end;
end;

procedure TWaitForActionUI.ShowForm;
  {Displays any form assigned to Form property. Should not be called directly:
  always call ShowOrHideForm(True) instead.
  }
begin
  Assert(Assigned(fForm),                                  // ** do not localise
    ClassName + '.ShowForm: fForm is nil');
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

procedure TWaitForActionUI.ShowOrHideForm(const Show: Boolean);
  {Attempts to show or hide the form associated with the Form property. This
  method should be called instead of ShowForm or CloseForm. The method is runs
  inside a critical section.
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

{ TActionThread }

constructor TActionThread.Create(const AAction: TBasicAction);
  {Class constructor. Sets up thread.
    @param AAction [in] Action to be executed in thread.
  }
begin
  inherited Create(True);
  fAction := AAction;
end;

procedure TActionThread.DoAction;
  {Checks action is assigned and executes it if so.
  }
begin
  if Assigned(fAction) then
    fAction.Execute;
end;

procedure TActionThread.Execute;
  {Causes action to be executed and records any exception that was raised.
  }
begin
  fExceptObj := nil;
  try
    // Action may not be thread safe so we execute it in calling thread
    Synchronize(DoAction);
  except
    // Keep reference to any exception
    fExceptObj := AcquireExceptionObject;
  end;
end;


initialization

// create critical section
Lock := TCriticalSection.Create;

finalization

// disposes of critical section
Lock.Free;

end.

