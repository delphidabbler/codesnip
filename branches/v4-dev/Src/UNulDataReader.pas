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
 * Portions created by the Initial Developer are Copyright (C) 2008-2011 Peter
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
  DB.USnippet, UIStringList, USnipData, USnippets;


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
    function GetCatSnippets(const Cat: string): IStringList;
      {Gets names of all snippets in a category.
        @param Cat [in] Name of category containing snippets.
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

function TNulDataReader.GetCatSnippets(const Cat: string): IStringList;
  {Gets names of all snippets in a category.
    @param Cat [in] Name of category containing snippets.
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

