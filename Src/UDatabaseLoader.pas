{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a static class that load and reset the database along with a
 * thread class that performs the same actions in a thread.
}


unit UDatabaseLoader;


interface


uses
  // Delphi
  Classes,
  // Project
  UBaseObjects;


type

  {
  TDatabaseLoader:
    Static class that loads or reloads the database.
  }
  TDatabaseLoader = class(TNoConstructObject)
  public
    class procedure Load;
      {Loads the database.
      }
  end;

  {
  TDatabaseLoaderThread:
    Class that loads and resets database in a thread.
  }
  TDatabaseLoaderThread = class(TThread)
  strict protected
    procedure Execute; override;
      {Loads database in thread and selects all records.
      }
  public
    constructor Create;
      {Class constructor. Sets up suspended thread ready to load database.
      }
  end;


implementation


uses
  // Project
  UQuery, DB.UMain;


{ TDatabaseLoader }

class procedure TDatabaseLoader.Load;
  {Loads the database.
  }
begin
  Database.Load;
  Query.Reset;
end;

{ TDatabaseLoaderThread }

constructor TDatabaseLoaderThread.Create;
  {Class constructor. Sets up suspended thread ready to load database.
  }
begin
  inherited Create(True);
end;

procedure TDatabaseLoaderThread.Execute;
  {Loads database in thread and selects all records.
  }
begin
  TDatabaseLoader.Load;
end;

end.
