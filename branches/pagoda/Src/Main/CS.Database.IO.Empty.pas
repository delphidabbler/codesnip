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
 * Implements a null database loader class that does nothing except create an
 * empty tag list a return a null last modification date.
 *
 * NOTE: This loader is for use when there is no snippets database present.
}


unit CS.Database.IO.Empty;

interface

uses
  CS.Database.IO.Types,
  CS.Database.SnippetsTable,
  CS.Database.Types,
  CS.Utils.Dates;

type
  TEmptyDBReader = class(TInterfacedObject, IDatabaseLoader)
  public
    procedure Load(const ATable: TDBSnippetsTable; out ATagSet: ITagSet;
      out ALastModified: TUTCDateTime);
    function DatabaseExists: Boolean;
  end;

implementation

uses
  CS.Database.Tags;

{ TEmptyDBReader }

function TEmptyDBReader.DatabaseExists: Boolean;
begin
  // The null database always exists
  Result := True;
end;

procedure TEmptyDBReader.Load(const ATable: TDBSnippetsTable;
  out ATagSet: ITagSet; out ALastModified: TUTCDateTime);
begin
  ATable.Clear;
  ATagSet := TTagSet.Create;
  ALastModified := TUTCDateTime.Now;
end;

end.

