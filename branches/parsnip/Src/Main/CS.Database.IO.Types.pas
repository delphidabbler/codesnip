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
 * Contains common type used in database input and / or output operations.
}


unit CS.Database.IO.Types;

interface

uses
  CS.Database.SnippetsTable,
  CS.Database.Types,
  CS.Utils.Dates;

type

  IDatabaseLoader = interface(IInterface)
    ['{8AC208B5-D0D2-498D-92A8-BD62D4ECF02D}']
    procedure Load(const ATable: TDBSnippetsTable; out ATagSet: ITagSet;
      out ALastModified: TUTCDateTime);
    function DatabaseExists(const Path: string): Boolean;
  end;

implementation

end.
