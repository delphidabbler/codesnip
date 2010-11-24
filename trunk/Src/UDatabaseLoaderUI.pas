{
 * UDatabaseLoaderUI.pas
 *
 * Defines a static class that loads the database in a thread and displays a
 * wait dialog if the loading is taking more than a specified amount of time.
 *
 * $Rev$
 * $Date$
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
 * The Original Code is UDatabaseLoaderUI.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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

