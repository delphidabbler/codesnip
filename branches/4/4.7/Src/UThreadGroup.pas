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
  // Delphi
  Classes, Generics.Collections;


type
  ///  <summary>Class that manages the lifetime of a group of thread objects.
  ///  </summary>
  ///  <remarks>
  ///  <para>There may be no more than $80 threads in a group.</para>
  ///  <para>Threads should be created suspended (not enforced).</para>
  ///  <para>Threads must not automatically free themselves on termination. Each
  ///  thread's FreeOnTerminate property will be set False when it is added to
  ///  the group.</para>
  ///  </remarks>
  TThreadGroup = class(TObject)
  public
    const
      ///  <summary>Maximum number of threads in group.</summary>
      MaxCount = $80;
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
    ///  <remarks>
    ///  <para>Must not be called if the group is already full.</para>
    ///  <para>The thread's FreeOnTerminate property is set True.</para>
    ///  </remarks>
    procedure Add(AThread: TThread); overload;
    ///  <summary>Adds each thread in the given array to the group.</summary>
    ///  <remarks>
    ///  <para>Must not be called if the number of threads in the array would
    ///  cause the group's capacity to be exceeded.</para>
    ///  <para>Each thread's FreeOnTerminate property is set True.</para>
    ///  </remarks>
    procedure Add(const AThreads: array of TThread); overload;
    ///  <summary>Notifies all threads to terminate.</summary>
    ///  <remarks>Simply sets each thread's Terminate property True.</remarks>
    procedure Terminate;
    ///  <summary>Starts all threads.</summary>
    ///  <remarks>Threads are started in the order they were added.</remarks>
    procedure Start;
    ///  <summary>Sets priority of all threads in group to the given value.
    ///  </summary>
    procedure SetPriorities(const Priority: TThreadPriority);
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
    ///  <summary>Checks if the group is empty, i.e. contains no threads.
    ///  </summary>
    function IsEmpty: Boolean;
    ///  <summary>Checks if the group is full and cannot accept any more
    ///  threads.</summary>
    function IsFull: Boolean;
    ///  <summary>Returns the number of threads in the group.</summary>
    function Count: Integer;
    ///  <summary>Indexed array of threads in the group.</summary>
    property Threads[Idx: Integer]: TThread read GetThread; default;
  end;


implementation


uses
  // Delphi
  Windows;


{ TThreadGroup }

procedure TThreadGroup.Add(AThread: TThread);
begin
  Assert(not IsFull, ClassName + '.Add: Group is full');
  AThread.FreeOnTerminate := False;
  fThreads.Add(AThread);
end;

procedure TThreadGroup.Add(const AThreads: array of TThread);
var
  Thread: TThread;
begin
  Assert(Count + Length(AThreads) <= MaxCount,
    ClassName + '.Add: Number of threads exceed group capacity.');
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

function TThreadGroup.IsFull: Boolean;
begin
  Result := Count = MaxCount;
end;

function TThreadGroup.Kill(Timeout: Cardinal): Cardinal;
begin
  Assert(not IsEmpty, ClassName + '.Kill: Thread list empty');
  Terminate;
  Result := WaitFor(Timeout);
end;

procedure TThreadGroup.SetPriorities(const Priority: TThreadPriority);
var
  Thread: TThread;
begin
  for Thread in fThreads do
    Thread.Priority := Priority;
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
