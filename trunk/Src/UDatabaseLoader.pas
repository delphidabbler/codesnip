{
 * UDatabaseLoader.pas
 *
 * Implements a class that loads and resets database in a thread.
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
 * The Original Code is UDatabaseLoader.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UDatabaseLoader;


interface


uses
  // Project
  UThreadEx;


type

  {
  TDatabaseLoader:
    Class that loads and resets database in a thread.
  }
  TDatabaseLoader = class(TThreadEx)
  protected
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
  UQuery, USnippets;


{ TDatabaseLoader }

constructor TDatabaseLoader.Create;
  {Class constructor. Sets up suspended thread ready to load database.
  }
begin
  inherited Create(True);
end;

procedure TDatabaseLoader.Execute;
  {Loads database in thread and selects all records.
  }
begin
  Snippets.Load;
  Query.Reset;
end;

end.
