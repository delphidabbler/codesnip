{
 * UNulDataReader.pas
 *
 * Implements a do nothing data reader for use when a database does not exist.
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
 * The Original Code is UNulDataReader.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UNulDataReader;


interface


uses
  // Project
  UIStringList, USnipData, USnippets;


type

  {
  TNulDataReader:
    A do nothing data reader used when a database does not exist.
  }
  TNulDataReader = class(TInterfacedObject,
    IDataReader
  )
  public
    { IDataReader methods }
    function DatabaseExists: Boolean;
      {Checks if the database exists. This method is always called first. No
      other methods are called if this method returns false.
        @return Always returns True. It always can read a do-nothing database.
      }
    function GetAllCatNames: IStringList;
      {Gets names of all categories in database.
        @return Empty string list.
      }
    procedure GetCatProps(const Cat: string; var Props: TCategoryData);
      {Gets properties of a category.
        @param Cat [in] Name of required category.
        @param Props [in/out] Empty properties passed in. Unchanged.
      }
    function GetCatRoutines(const Cat: string): IStringList;
      {Gets names of all routines in a category.
        @param Cat [in] Name of category containing routines.
        @return Empty list.
      }
    procedure GetRoutineProps(const Routine: string; var Props: TSnippetData);
      {Gets properties of a routine. These are the fields of the routine's
      record in the routines "table".
        @param Routine [in] Name of required routine.
        @param Props [in/out] Empty properties passed in. Unchanged.
      }
    function GetRoutineXRefs(const Routine: string): IStringList;
      {Gets list of all routines that are cross referenced by a routine.
        @param Routine [in] Name of routine we need cross references for.
        @return Empty list.
      }
    function GetRoutineDepends(const Routine: string): IStringList;
      {Gets list of all routines on which a given routine depends.
        @param Routine [in] Name of required routine.
        @return Empty list.
      }
    function GetRoutineUnits(const Routine: string): IStringList;
      {Gets list of all units referenced by a routine.
        @param Routine [in] Name of required routine.
        @return Empty list.
      }
  end;


implementation


{ TNulDataReader }

function TNulDataReader.DatabaseExists: Boolean;
  {Checks if the database exists. This method is always called first. No other
  methods are called if this method returns false.
    @return Always returns True. It always can read a do-nothing database.
  }
begin
  Result := True;
end;

function TNulDataReader.GetAllCatNames: IStringList;
  {Gets names of all categories in database.
    @return Empty string list.
  }
begin
  Result := TIStringList.Create;
end;

procedure TNulDataReader.GetCatProps(const Cat: string;
  var Props: TCategoryData);
  {Gets properties of a category.
    @param Cat [in] Name of required category.
    @param Props [in/out] Empty properties passed in. Unchanged.
  }
begin
  // Do nothing
end;

function TNulDataReader.GetCatRoutines(const Cat: string): IStringList;
  {Gets names of all routines in a category.
    @param Cat [in] Name of category containing routines.
    @return Empty list.
  }
begin
  Result := TIStringList.Create;
end;

function TNulDataReader.GetRoutineDepends(const Routine: string): IStringList;
  {Gets list of all routines on which a given routine depends.
    @param Routine [in] Name of required routine.
    @return Empty list.
  }
begin
  Result := TIStringList.Create;
end;

procedure TNulDataReader.GetRoutineProps(const Routine: string;
  var Props: TSnippetData);
  {Gets properties of a routine. These are the fields of the routine's record in
  the routines "table".
    @param Routine [in] Name of required routine.
    @param Props [in/out] Empty properties passed in. Unchanged.
  }
begin
  // Do nothing
end;

function TNulDataReader.GetRoutineUnits(const Routine: string): IStringList;
  {Gets list of all units referenced by a routine.
    @param Routine [in] Name of required routine.
    @return Empty list.
  }
begin
  Result := TIStringList.Create;
end;

function TNulDataReader.GetRoutineXRefs(const Routine: string): IStringList;
  {Gets list of all routines that are cross referenced by a routine.
    @param Routine [in] Name of routine we need cross references for.
    @return Empty list.
  }
begin
  Result := TIStringList.Create;
end;

end.

