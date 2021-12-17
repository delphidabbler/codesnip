{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a do nothing data reader for use when a database does not exist.
}


unit DBIO.UNulDataReader;


interface


uses
  // Project
  DB.UCategory, DB.USnippet, DBIO.UFileIOIntf, UIStringList;


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
    function GetAllCatIDs: IStringList;
      {Gets ids of all categories in database.
        @return Empty string list.
      }
    procedure GetCatProps(const CatID: string; var Props: TCategoryData);
      {Gets properties of a category.
        @param CatID [in] Id of required category.
        @param Props [in/out] Empty properties passed in. Unchanged.
      }
    function GetCatSnippets(const CatID: string): IStringList;
      {Gets names of all snippets in a category.
        @param CatID [in] Id of category containing snippets.
        @return Empty list.
      }
    procedure GetSnippetProps(const Snippet: string; var Props: TSnippetData);
      {Gets properties of a snippet. These are the fields of the snippet's
      record in the snippets "table".
        @param Snippet [in] Name of required snippet.
        @param Props [in/out] Empty properties passed in. Unchanged.
      }
    function GetSnippetXRefs(const Snippet: string): IStringList;
      {Gets list of all snippets that are cross referenced by a snippet.
        @param Snippet [in] Name of snippet we need cross references for.
        @return Empty list.
      }
    function GetSnippetDepends(const Snippet: string): IStringList;
      {Gets list of all snippets on which a given snippet depends.
        @param Snippet [in] Name of required snippet.
        @return Empty list.
      }
    function GetSnippetUnits(const Snippet: string): IStringList;
      {Gets list of all units referenced by a snippet.
        @param Snippet [in] Name of required snippet.
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

function TNulDataReader.GetAllCatIDs: IStringList;
  {Gets ids of all categories in database.
    @return Empty string list.
  }
begin
  Result := TIStringList.Create;
end;

procedure TNulDataReader.GetCatProps(const CatID: string;
  var Props: TCategoryData);
  {Gets properties of a category.
    @param CatID [in] Id of required category.
    @param Props [in/out] Empty properties passed in. Unchanged.
  }
begin
  // Do nothing
end;

function TNulDataReader.GetCatSnippets(const CatID: string): IStringList;
  {Gets names of all snippets in a category.
    @param CatID [in] Id of category containing snippets.
    @return Empty list.
  }
begin
  Result := TIStringList.Create;
end;

function TNulDataReader.GetSnippetDepends(const Snippet: string): IStringList;
  {Gets list of all snippets on which a given snippet depends.
    @param Snippet [in] Name of required snippet.
    @return Empty list.
  }
begin
  Result := TIStringList.Create;
end;

procedure TNulDataReader.GetSnippetProps(const Snippet: string;
  var Props: TSnippetData);
  {Gets properties of a snippet. These are the fields of the snippet's record in
  the snippets "table".
    @param Snippet [in] Name of required snippet.
    @param Props [in/out] Empty properties passed in. Unchanged.
  }
begin
  // Do nothing
end;

function TNulDataReader.GetSnippetUnits(const Snippet: string): IStringList;
  {Gets list of all units referenced by a snippet.
    @param Snippet [in] Name of required snippet.
    @return Empty list.
  }
begin
  Result := TIStringList.Create;
end;

function TNulDataReader.GetSnippetXRefs(const Snippet: string): IStringList;
  {Gets list of all snippets that are cross referenced by a snippet.
    @param Snippet [in] Name of snippet we need cross references for.
    @return Empty list.
  }
begin
  Result := TIStringList.Create;
end;

end.

