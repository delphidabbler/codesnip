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
  TThreadGroup = class(TObject)
  strict private
    var
      fThreads: TList<TThread>;
    function GetThread(Idx: Integer): TThread;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AThread: TThread); overload;
    procedure Add(const AThreads: array of TThread); overload;
    procedure Terminate;
    procedure Start;
    function WaitFor(Timeout: Cardinal): Cardinal;
    function Kill(Timeout: Cardinal): Cardinal;
    function IsEmpty: Boolean;
    function Count: Integer;
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
