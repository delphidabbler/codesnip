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
 * Defines an abstract base class for threads that generate and record
 * notification data in the notification queue.
}


unit Notifications.URecorderThread;


interface


uses
  // Delphi
  Classes,
  // Project
  Notifications.UData;


type
  ///  <summary>Abstract base class for threads that generate or receive a
  ///  notification that is to be recorded in the notification queue.</summary>
  ///  <remarks>Sub-classes must override GetNotification to provide any
  ///  notification data record to be queued.</remarks>
  TNotificationRecorderThread = class abstract(TThread)
  strict private
    var
      ///  <summary>Number of milliseconds to delay start of thread's execution.
      ///  </summary>
      fStartDelay: Cardinal;
    ///  <summary>Performs a busy wait for the time specified by fStartDelay.
    ///  </summary>
    procedure DoDelay;
  strict protected
    ///  <summary>Gets notification data to be queued.</summary>
    ///  <param name="N">TNotificationData [out] Set to required notification
    ///  data item.</param>
    ///  <returns>Boolean. True on success or False if no notification data is
    ///  available.</returns>
    ///  <remarks>N can be undefined if False is returned.</remarks>
    function GetNotification(out N: TNotificationData): Boolean; virtual;
      abstract;
    ///  <summary>Stores the given notification data item in the notification
    ///  queue.</summary>
    ///  <remarks>If notification can't be queued it is discarded.</remarks>
    procedure RecordNotification(const N: TNotificationData);
    ///  <summary>Gets and stores a notification in the notification queue.
    ///  </summary>
    ///  <remarks>Called when thread is started.</remarks>
    procedure Execute; override;
  public
    ///  <summary>Creates the and initialises the thread instance.</summary>
    ///  <param name="StartDelay">Cardinal [in] Number of milliseconds to delay
    ///  the start of the thread's execution. Passing 0 indicates no delay.
    ///  Non-zero delays are rounded up to the nearest 1/5th second.</param>
    constructor Create(StartDelay: Cardinal = 0);
  end;


implementation


uses
  // Delphi
  Windows,
  // Project
  Notifications.UQueue;


{ TNotificationRecorderThread }

constructor TNotificationRecorderThread.Create(StartDelay: Cardinal);
begin
  inherited Create(True);
  fStartDelay := StartDelay;
end;

procedure TNotificationRecorderThread.DoDelay;
var
  StartTC: Cardinal;
  CurrentTC: Int64;
begin
  StartTC := GetTickCount;
  repeat
    if Terminated then
      Exit;
    CurrentTC := Windows.GetTickCount;
    if CurrentTC < StartTC then
      CurrentTC := CurrentTC + High(Cardinal);
    Sleep(200); // 1/5 second
  until CurrentTC - StartTC >= fStartDelay;
end;

procedure TNotificationRecorderThread.Execute;
var
  N: TNotificationData;
begin
  if fStartDelay > 0 then
    DoDelay;
  if Terminated then
    Exit;
  if not GetNotification(N) or Terminated then
    Exit;
  RecordNotification(N);
end;

procedure TNotificationRecorderThread.RecordNotification(
  const N: TNotificationData);
var
  Queue: TNotificationQueue;
begin
  Queue := TNotificationQueue.Create;
  try
    if Terminated then
      Exit;
    // Attempt to push notification data onto queue. This may fail if queue is
    // full, in which case we ignore the problem and ditch the notification.
    // If in future some other action needs to be taken when the queue is
    // full, just test the return value of the Push method - False => queue
    // full.
    Queue.Push(N);  // call this may block waiting for critical section
  finally
    Queue.Free;
  end;
end;


end.
