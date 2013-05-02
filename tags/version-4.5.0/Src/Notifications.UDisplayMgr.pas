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
 * Implements a static object that manages the notification display sub-system.
}


unit Notifications.UDisplayMgr;


interface


uses
  // Delphi
  Controls,
  // Project
  Notifications.UDisplayThread, Notifications.UWindow, UBaseObjects;


type
  ///  <summary>Static class that manages the notification display sub-system.
  ///  </summary>
  ///  <remarks>This class manages the background thread that reads the
  ///  notification queue and displays the notifications sequentially in a
  ///  suitable pop-up window.</remarks>
  TNotificationDisplayMgr = class(TNoConstructObject)
  strict private
    const
      ///  <summary>Amount of time the notification window is displayed on
      ///  screen before hiding itself.</summary>
      WindowDisplayTime = 20 * 1000;  // 20 seconds
    class var
      ///  <summary>Notification window object.</summary>
      fWindow: TNotificationWindow;
      ///  <summary>Thread that reads the notification queue and displays the
      ///  notifications sequentially in the notification window.
      ///  </summary>
      fDisplayThread: TNotificationDisplayThread;
  public
    ///  <summary>Class destructor. Ensures the noification window and display
    ///  thread are destroyed when the program closes.</summary>
    class destructor Destroy;
    ///  <summary>Starts the notification display sub-system.</summary>
    ///  <param name="WindowParent">TWinControl [in] Window control that is to
    ///  be the parent of the notification window. Must not be nil.</param>
    ///  <remarks>Creates a new notification window object and a linked display
    ///  thread.</remarks>
    class procedure Start(const WindowParent: TWinControl);
    ///  <summary>Stops the notification display sub-system.</summary>
    ///  <remarks>The display thread and notification window are destroyed.
    ///  </remarks>
    class procedure Stop;
  end;


implementation


uses
  SysUtils, Classes,
  // Project
  Notifications.UData, Notifications.UQueue;


{ TNotificationDisplayMgr }

class destructor TNotificationDisplayMgr.Destroy;
begin
  Stop;
end;

class procedure TNotificationDisplayMgr.Start(const WindowParent: TWinControl);
begin
  Assert(Assigned(WindowParent), ClassName + '.Start: WindowParent is nil');

  if Assigned(fWindow) or Assigned(fDisplayThread) then
    Stop;

  fWindow := TNotificationWindow.Create(nil);
  fWindow.Parent := WindowParent;
  fWindow.DisplayTime := WindowDisplayTime;

  fDisplayThread := TNotificationDisplayThread.Create(
    fWindow.DisplayLock,
    procedure(N: TNotificationData)
    begin
      Assert(fWindow.State = nwsClosed,
        ClassName + '.Create:AnonCallback: Window not closed');
      fWindow.SlideIn(N);
    end
  );
  fDisplayThread.FreeOnTerminate := False;
  fDisplayThread.Priority := tpLowest;
  fDisplayThread.Start;
end;

class procedure TNotificationDisplayMgr.Stop;
begin
  // FreeAndNil is necessary here
  TNotificationQueue.ReleaseLocks;
  FreeAndNil(fDisplayThread);
  FreeAndNil(fWindow);
end;

end.

