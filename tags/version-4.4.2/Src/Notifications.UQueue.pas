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
 * Defines a class that manages a thread safe queue of notifications that are
 * awaiting display.
}


unit Notifications.UQueue;


interface


uses
  // Delphi
  SyncObjs, Generics.Collections,
  // Project
  Notifications.UData;


type
  ///  <summary>Manages a thread safe queue of notifications that are awaiting
  ///  display.</summary>
  ///  <remarks>Notifications can  be placed in the queue and removed from it by
  ///  different threads.</remarks>
  TNotificationQueue = class(TObject)
  strict private

    const
      ///  <summary>Maximum size of queue.</summary>
      MaxQueueSize = 20;

    class var
      ///  <summary>Queue of notifications.</summary>
      fQueue: TQueue<TNotificationData>;
      ///  <summary>Critical section that ensures queue is accessed by only one
      ///  thread at a time.</summary>
      fCriticalSection: TCriticalSection;
      ///  <summary>Event object that enables threads reading the queue to block
      ///  while the queue is empty.</summary>
      ///  <remarks>NOTE: This lock is designed for use with a single reading
      ///  thread and is not guaranteed to work properly should more than one
      ///  task block on the empty queue.</remarks>
      fEmptyLock: TSimpleEvent;

  strict private

    ///  <summary>Checks if the queue is full.</summary>
    function IsFull: Boolean; inline;

    ///  <summary>Checks if the queue is empty.</summary>
    function IsEmpty: Boolean; inline;

  public

    ///  <summary>Creates objects shared amongst all instances.</summary>
    class constructor Create;

    ///  <summary>Frees objects shared amongst all instances.</summary>
    class destructor Destroy;

    ///  <summary>Attempts to add a new notification data item onto the end of
    ///  the queue.</summary>
    ///  <param name="Value">TNotificationData [in] Notification data to be
    ///  added to queue.</param>
    ///  <returns>Boolean. True if data was added to the queue or False if the
    ///  queue was full and the data was not added.</returns>
    function Push(const Value: TNotificationData): Boolean;

    ///  <summary>Attempts to read and remove a notification data item from the
    ///  front of the queue.</summary>
    ///  <param name="Value">TNotificationData [out] Receives any notification
    ///  data added to queue.</param>
    ///  <returns>Boolean. True if data was read from the queue or False if the
    ///  queue was empty and no data could be read.</returns>
    ///  <remarks>If False is returned, Value is undefined.</remarks>
    function Pop(out Value: TNotificationData): Boolean;

    ///  <summary>Waits for the empty lock event to enter a singalled state.
    ///  </summary>
    ///  <param name="Timeout">Cardinal [in] Maximum amount of time to wait.
    ///  </param>
    ///  <returns>TWaitResult. Indicates how the wait operation finished.
    ///  </returns>
    class function WaitForEmptyLock(Timeout: Cardinal): TWaitResult;

    ///  <summary>Releases any locks on the queue.</summary>
    class procedure ReleaseLocks;
  end;


implementation


{ TNotificationQueue }

class constructor TNotificationQueue.Create;
begin
  fCriticalSection := TCriticalSection.Create;
  fQueue := TQueue<TNotificationData>.Create;
  fEmptyLock := TSimpleEvent.Create;
  fEmptyLock.ResetEvent;
end;

class destructor TNotificationQueue.Destroy;
begin
  fEmptyLock.Free;
  fQueue.Free;
  fCriticalSection.Free;
end;

function TNotificationQueue.IsEmpty: Boolean;
begin
  Result := fQueue.Count = 0;
end;

function TNotificationQueue.IsFull: Boolean;
begin
  Result := fQueue.Count = MaxQueueSize;
end;

function TNotificationQueue.Pop(out Value: TNotificationData): Boolean;
begin
  fCriticalSection.Acquire;
  try
    Result := not IsEmpty;
    if Result then
    begin
      Value := fQueue.Dequeue;
      if IsEmpty then
        fEmptyLock.ResetEvent;
    end;
  finally
    fCriticalSection.Release;
  end;
end;

function TNotificationQueue.Push(const Value: TNotificationData): Boolean;
var
  WasEmpty: Boolean;
begin
  fCriticalSection.Acquire;
  try
    WasEmpty := IsEmpty;
    Result := not IsFull;
    if Result then
    begin
      fQueue.Enqueue(Value);
      if WasEmpty then
        fEmptyLock.SetEvent;
    end;
  finally
    fCriticalSection.Release;
  end;
end;

class procedure TNotificationQueue.ReleaseLocks;
begin
  // only the one lock!
  fEmptyLock.SetEvent;
end;

class function TNotificationQueue.WaitForEmptyLock(Timeout: Cardinal):
  TWaitResult;
begin
  Result := fEmptyLock.WaitFor(Timeout);
end;

end.

