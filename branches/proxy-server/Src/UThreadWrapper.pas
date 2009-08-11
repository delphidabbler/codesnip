{
 * UThreadWrapper.pas
 *
 * Implements a class that can execute a specified thread while allowing the
 * main thread to process its message queue.
 *
 * v1.0 of 26 Feb 2007  - Original version.
 * v1.1 of 25 Aug 2008  - CloneException method modified to ensure all ECodeSnip
 *                        exceptions are properly cloned using new Assign method
 *                        of ECodeSnip.
 * v1.2 of 04 Oct 2008  - Made constructor protected and made Execute method
 *                        static. Modified other methods accordingly.
 *                      - Made private and protected sections strict.
 *                      - Changed error checking in CloneException method from
 *                        raising EBug to assertion check.
 *                      - Now use ClassName method in all assert statements.
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
 * The Original Code is UThreadWrapper.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UThreadWrapper;


interface


uses
  // Delphi
  SysUtils,
  // Project
  UBaseObjects, UThreadEx;


type

  {
  TThreadWrapper:
    Class that can execute a specified thread while allowing main thread to
    process its message queue.
  }
  TThreadWrapper = class(TNoPublicConstructObject)
  strict private
    fThread: TThreadEx;
      {Reference to wrapped thread}
    fOwnsThread: Boolean;
      {Flag recording whether wrapper owns, and therefore must free, wrapped
      thread}
    function CloneException(const ExceptObj: TObject): Exception;
      {Creates an approximate copy of an exception object.
        @param ExceptObj [in] Exception object to be copied. Must be class
          Exception or descendant.
        @return Exception object of same type and with same message as
          ExceptObj.
      }
  strict protected
    constructor InternalCreate(const AThread: TThreadEx;
      const OwnsThread: Boolean = False);
      {Class constructor. Creates wrapper object for a thread.
        @param AThread [in] Thread object to be wrapped.
        @param OwnsThread [in] Indicates whether wrapper object is to take
          ownership of, and ultimately free, thread.
      }
    procedure DoExecute;
      {Executes wrapped thread and allows application to process any outstanding
      messages while thread is executing.
        @except Exception or descendant class raised if thread raises exception.
      }
  public
    destructor Destroy; override;
      {Class destructor. Tears down object and frees any owned thread.
      }
    class procedure Execute(const AThread: TThreadEx;
      const OwnsThread: Boolean = False); overload;
      {Class constructor. Creates wrapper object for a thread.
        @param AThread [in] Thread object to be wrapped.
        @param OwnsThread [in] Indicates whether wrapper object is to take
          ownership of, and ultimately free, thread.
        @except Exception or descendant class raised if thread raises exception.
      }
  end;


implementation


uses
  // Delphi
  Forms,
  // Project
  UExceptions;


{ TThreadWrapper }

function TThreadWrapper.CloneException(const ExceptObj: TObject): Exception;
  {Creates an approximate copy of an exception object.
    @param ExceptObj [in] Exception object to be copied. Must be class Exception
      or descendant.
    @return Exception object of same type and with same message as ExceptObj.
  }
begin
  Assert(ExceptObj is Exception,                           // ** do not localise
    ClassName + '.CloneException: ExceptObj is not Exception');
  if ExceptObj is ECodeSnip then
  begin
    Result := ExceptObj.ClassType.Create as ECodeSnip;
    (Result as ECodeSnip).Assign(ExceptObj as ECodeSnip);
  end
  else
  begin
    Result := ExceptObj.ClassType.Create as Exception;
    Result.Message := (ExceptObj as Exception).Message;
  end;
end;

destructor TThreadWrapper.Destroy;
  {Class destructor. Tears down object and frees any owned thread.
  }
begin
  if fOwnsThread then
    FreeAndNil(fThread);
  inherited;
end;

procedure TThreadWrapper.DoExecute;
  {Executes wrapped thread and allows application to process any outstanding
  messages while thread is executing.
    @except Exception or descendant class raised if thread raises exception.
  }
begin
  // Wake up thread
  fThread.Resume;
  try
    // Loop while thread executes, enabling application to breath
    while not fThread.Completed do
      Application.ProcessMessages;
    // Re-raise in main thread any exception raised in wrapper thread
    if Assigned(fThread.FatalException) then
      raise CloneException(fThread.FatalException);
  finally
    // Wait for thread to end
    fThread.WaitFor;
  end;
end;

class procedure TThreadWrapper.Execute(const AThread: TThreadEx;
  const OwnsThread: Boolean);
  {Class constructor. Creates wrapper object for a thread.
    @param AThread [in] Thread object to be wrapped.
    @param OwnsThread [in] Indicates whether wrapper object is to take ownership
      of, and ultimately free, thread.
    @except Exception or descendant class raised if thread raises exception.
  }
begin
  with InternalCreate(AThread, OwnsThread) do
    try
      DoExecute;
    finally
      Free;
    end;
end;

constructor TThreadWrapper.InternalCreate(const AThread: TThreadEx;
  const OwnsThread: Boolean);
  {Class constructor. Creates wrapper object for a thread.
    @param AThread [in] Thread object to be wrapped.
    @param OwnsThread [in] Indicates whether wrapper object is to take ownership
      of, and ultimately free, thread.
  }
begin
  Assert(Assigned(AThread),                                // ** do not localise
    ClassName + '.InternalCreate: AThread is nil');
  Assert(AThread.Suspended,                                // ** do not localise
    ClassName + '.InternalCreate: AThread is not suspended');
  inherited InternalCreate;
  fThread := AThread;
  fOwnsThread := OwnsThread;
end;

end.

