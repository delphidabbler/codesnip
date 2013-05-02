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
 * Implements a thread that pops any waiting items from notification queue and
 * causes them to be displayed.
}


unit Notifications.UDisplayThread;


interface


uses
  // Delphi
  SysUtils, Classes, SyncObjs,
  // Project
  Notifications.UData, Notifications.UQueue;


type
  ///  <summary>Thread that pops pending notifications from the notification
  ///  queue and causes them to be displayed.</summary>
  ///  <remarks>
  ///  <para>This class is display agnostic. It has no knowledge of the object
  ///  that actually displays the notification and simply a provided callback
  ///  procedure to request display of a notification.</para>
  ///  <para>The thread waits for any currently displayed notification to clear
  ///  before displaying another notification.</para>
  ///  <para>NOTE: There should only be one instance of this thread running at
  ///  any time.</para>
  ///  </remarks>
  TNotificationDisplayThread = class(TThread)
  strict private
    const
      ///  <summary>Timeout, in ms, when waiting for a notification display to
      ///  become available.</summary>
      DisplayLockTimeout = 20000; // 20 seconds
      ///  <summary>Timeout, in ms, when waiting for notification queue's
      ///  'empty' lock.</summary>
      EmptyLockTimeout = 10000;   // 10 seconds
    var
      ///  <summary>Object that provides access to notification queue.</summary>
      fQueue: TNotificationQueue;
      ///  <summary>Caller-provided closure used to display a notification.
      ///  </summary>
      ///  <remarks>This closure is called in the context of the main thread.
      ///  </remarks>
      fDisplayProc: TProc<TNotificationData>;
      ///  <summary>Event object that signals when the notification display
      ///  object is available for use.</summary>
      fDisplayLock: TSimpleEvent;
  strict protected
    ///  <summary>Executes the main thread logic. Waits to pop a notification
    ///  item from the queue then causes it to be displayed.</summary>
    procedure Execute; override;
  public
    ///  <summary>Constructs a suspended thread instance.</summary>
    ///  <param name="DisplayLock">TSimpleEvent [in] Lock used by notification
    ///  window while it is being displayed.</param>
    ///  <param name="DisplayProc">TProc&lt;TNotificationData&gt; Anonymous
    ///  method that the thread calls when it wants to display a notification.
    ///  </param>
    constructor Create(const DisplayLock: TSimpleEvent;
      const DisplayProc: TProc<TNotificationData>);
    ///  <summary>Destroys the thread instance, freeing owned objects.</summary>
    destructor Destroy; override;
  end;


implementation


{ TNotificationDisplayThread }

constructor TNotificationDisplayThread.Create(const DisplayLock: TSimpleEvent;
  const DisplayProc: TProc<TNotificationData>);
begin
  Assert(Assigned(DisplayLock), ClassName + '.Create: DisplayLock is nil');
  Assert(Assigned(DisplayProc), ClassName + '.Create: DisplayProc is nil');
  inherited Create(True);
  fDisplayLock := DisplayLock;
  fDisplayProc := DisplayProc;
  fQueue := TNotificationQueue.Create;
end;

destructor TNotificationDisplayThread.Destroy;
begin
  fQueue.Free;
  inherited;
end;

procedure TNotificationDisplayThread.Execute;
var
  NotificationData: TNotificationData;
  EmptyQueueWaitResult: TWaitResult;
begin
  while not Terminated do
  begin
    EmptyQueueWaitResult := fQueue.WaitForEmptyLock(EmptyLockTimeout);
    if not (EmptyQueueWaitResult in [wrSignaled, wrTimeout]) then
      Terminate;
    if Terminated then
      Exit;
    if not fQueue.Pop(NotificationData) then
      Continue;
    if Terminated then
      Exit;

    while not Terminated do
    begin
      case fDisplayLock.WaitFor(DisplayLockTimeout) of
        wrSignaled:
        begin
          Synchronize(
            procedure
            begin
              fDisplayProc(NotificationData);
            end
          );
          Break;
        end;
        wrTimeout:
          Continue;
        else
          Terminate;
      end;
    end;
  end;
end;

end.

