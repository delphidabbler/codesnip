{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements objects that can create suitable objects that can be used to load
 * and save the snippets database.
}


unit CS.Database.IO.Factory;


interface


uses
  // Project
  CS.Database.IO.Types,
  UBaseObjects;


type

  ///  <summary>Factory class used to create database loader and writer objects
  ///  for use in loading and saving the snippets database.</summary>
  TDatabaseIOFactory = class(TNoConstructObject)
  public
    ///  <summary>Creates and returns an instance of a database loader object
    ///  that can be used to load the database.</summary>
    ///  <remarks>If a database in native format is present a loader for that
    ///  database is returned. If there is no native database then any legacy
    ///  XML format database is looked for and a loader for that is returned if
    ///  found. If there is no database present at all then a special "empty
    ///  database" loader is returned.</remarks>
    class function CreateLoader: IDatabaseLoader;
    ///  <summary>Creates and returns an instance of a an object that can be
    ///  used to save the database.</summary>
    ///  <remarks>This method always returns an writer object that saves in
    ///  native format. No other output formats are supported.</remarks>
    class function CreateWriter: IDatabaseWriter;
  end;


implementation


uses
  // Project
  CS.Database.IO.Empty,
  CS.Database.IO.Legacy,
  CS.Database.IO.Native,
  UAppInfo;


{ TDatabaseIOFactory }

class function TDatabaseIOFactory.CreateLoader: IDatabaseLoader;
begin
  Result := TDBNativeReader.Create(TAppInfo.UserDataDir);
  if Result.DatabaseExists then
    Exit;
  Result := TDBLegacyUserDBReader.Create(TAppInfo.UserDataDir);
  if Result.DatabaseExists then
    Exit;
  Result := TEmptyDBReader.Create;
end;

class function TDatabaseIOFactory.CreateWriter: IDatabaseWriter;
begin
  Result := TDBNativeWriter.Create(TAppInfo.UserDataDir);
end;

end.

