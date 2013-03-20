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
 * Implements a class that manages the lifetime of a group of thread objects.
}


unit UThreadGroup;


interface


uses
  Classes, Generics.Collections;


type
  ///  <summary>Class that manages the lifetime of a group of thread objects.
  ///  </summary>
  ///  <remarks>
  ///  <para>Threads should be created suspended (not enforced).</para>
  ///  <para>Threads must not automatically free themselves on termination. Each
  ///  thread's FreeOnTerminate property will be set False when it is added to
  ///  the group.</para>
  ///  </remarks>
  TThreadGroup = class(TObject)
  strict private
    var
      ///  <summary>List if threads.</summary>
      fThreads: TList<TThread>;
    ///  <summary>Read accessor for Threads[] property.</summary>
    function GetThread(Idx: Integer): TThread;
  public
    ///  <summary>Constructs a new empty thread group object.</summary>
    constructor Create;
    ///  <summary>Destroys object instance.</summary>
    destructor Destroy; override;
    ///  <summary>Adds the given thread to the group.</summary>
    procedure Add(AThread: TThread); overload;
    ///  <summary>Adds each thread in the given array to the group.</summary>
    procedure Add(const AThreads: array of TThread); overload;
    ///  <summary>Notifies all threads to terminate.</summary>
    ///  <remarks>Simply sets each thread's Terminate property True.</remarks>
    procedure Terminate;
    ///  <summary>Starts all threads.</summary>
    ///  <remarks>Threads are started in the order they were added.</remarks>
    procedure Start;
    ///  <summary>Waits for each thread to terminate, subject to a timeout.
    ///  </summary>
    ///  <param name="Timeout">Cardinal [in] Timeout in ms.</param>
    ///  <returns>Cardinal. Values are the same as for the Windows API
    ///  WaitForMultipleObjects function. In summary, values $00..$79 represent
    ///  success while values >= $80 are errors.</returns>
    ///  <remarks>WaitFor must not be called when the group is empty.</remarks>
    function WaitFor(Timeout: Cardinal): Cardinal;
    ///  <summary>Signals each thread to terminate and then waits for them to
    ///  complete, subject to a timeout.</summary>
    ///  <param name="Timeout">Cardinal [in] Timeout in ms.</param>
    ///  <returns>Cardinal. Values are the same as for the Windows API
    ///  WaitForMultipleObjects function. In summary, values $00..$79 represent
    ///  success while values >= $80 are errors.</returns>
    ///  <remarks>Kill must not be called when the group is empty.</remarks>
    function Kill(Timeout: Cardinal): Cardinal;
    ///  <summary>Checks of the group is empty, i.e. contains no threads.
    ///  </summary>
    function IsEmpty: Boolean;
    ///  <summary>Returns the number of threads in the group.</summary>
    function Count: Integer;
    ///  <summary>Indexed array of threads in the group.</summary>
    property Threads[Idx: Integer]: TThread read GetThread; default;
  end;


implementation


uses
  Windows;


{ TThreadGroup }

procedure TThreadGroup.Add(AThread: TThread);
begin
  AThread.FreeOnTerminate := False;
  fThreads.Add(AThread);
end;

procedure TThreadGroup.Add(const AThreads: array of TThread);
var
  Thread: TThread;
begin
  for Thread in AThreads do
    Thread.FreeOnTerminate := False;
  fThreads.AddRange(AThreads);
end;

function TThreadGroup.Count: Integer;
begin
  Result := fThreads.Count;
end;

constructor TThreadGroup.Create;
begin
  inherited;
  fThreads := TList<TThread>.Create;
end;

destructor TThreadGroup.Destroy;
var
  Thread: TThread;
begin
  for Thread in fThreads do
    Thread.Free;  // terminates thread cleanly
  fThreads.Free;
  inherited;
end;

function TThreadGroup.GetThread(Idx: Integer): TThread;
begin
  Result := fThreads[Idx];
end;

function TThreadGroup.IsEmpty: Boolean;
begin
  Result := fThreads.Count = 0;
end;

function TThreadGroup.Kill(Timeout: Cardinal): Cardinal;
begin
  Assert(not IsEmpty, ClassName + '.Kill: Thread list empty');
  Terminate;
  Result := WaitFor(Timeout);
end;

procedure TThreadGroup.Start;
var
  Thread: TThread;
begin
  for Thread in fThreads do
    Thread.Start;
end;

procedure TThreadGroup.Terminate;
var
  Thread: TThread;
begin
  for Thread in fThreads do
    Thread.Terminate;
end;

function TThreadGroup.WaitFor(Timeout: Cardinal): Cardinal;
var
  I: Integer;
  Handles: TArray<THandle>;
begin
  Assert(not IsEmpty, ClassName + '.WaitFor: Thread list empty');
  SetLength(Handles, fThreads.Count);
  for I := 0 to Pred(fThreads.Count) do
    Handles[I] := fThreads[I].Handle;
  Result := WaitForMultipleObjects(
    Length(Handles), @Handles[0], True, Timeout
  );
end;

end.
