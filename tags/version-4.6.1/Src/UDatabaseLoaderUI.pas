{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2010-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines a static class that loads the database in a thread and displays a
 * wait dialog if the loading is taking more than a specified amount of time.
}


unit UDatabaseLoaderUI;


interface


uses
  // Delphi
  Classes,
  // Projects
  UBaseObjects;


type

  {
  TDatabaseLoaderUI:
    Static class that loads the database in a thread and displays a wait dialog
    if the loading is taking more than a specified amount of time.
  }
  TDatabaseLoaderUI = class(TNoConstructObject)
  strict private
    const PauseBeforeDisplay = 500; // time elapsed before dialog is displayed
    const MinDisplayTime = 1000;    // minimum time that dialog is displayed
  public
    class procedure Execute(AOwner: TComponent);
      {Loads the database in a thread, displaying a wait message if the process
      takes more than a certain time.
        @param AOwner [in] Control that owns the wait dialog box. Dialog is
          aligned over this control.
      }
  end;


implementation


uses
  // Project
  FmWaitDlg, UDatabaseLoader, UWaitForThreadUI;


{ TDatabaseLoaderUI }

class procedure TDatabaseLoaderUI.Execute(AOwner: TComponent);
  {Loads the database in a thread, displaying a wait message if the process
  takes more than a certain time.
    @param AOwner [in] Control that owns the wait dialog box. Dialog is aligned
      over this control.
  }
resourcestring
  sLoadingDatabase  = 'Loading database...';  // wait dialog caption
var
  WaitDlg: TWaitDlg;                  // dialog box to display while compiling
  LoadThread: TDatabaseLoaderThread;  // thread that loads database
begin
  LoadThread := nil;
  // Set up dialog that may be displayed while compiling
  WaitDlg := TWaitDlg.Create(AOwner);
  try
    WaitDlg.Caption := sLoadingDatabase;
    // Load the database
    LoadThread := TDatabaseLoaderThread.Create;
    TWaitForThreadUI.Run( // this blocks until thread completes
      LoadThread, WaitDlg, PauseBeforeDisplay, MinDisplayTime
    );
  finally
    LoadThread.Free;
    WaitDlg.Free;
  end;
end;

end.

