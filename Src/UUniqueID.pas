{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a static class that generates globally unique id strings.
}


unit UUniqueID;


interface


uses
  // Project
  UBaseObjects;


type

  {
  TUniqueID:
    Static class that generates globally unique id strings.
  }
  TUniqueID = class(TNoConstructObject)
  public
    class function Generate: string;
      {Generates a 32 digit globally unique id string of hex characters.
        @return Required unique id.
      }
  end;


implementation


uses
  // Delphi
  SysUtils;


{ TUniqueID }

class function TUniqueID.Generate: string;
  {Generates a 32 digit globally unique id string of hex characters.
    @return Required unique id.
  }
type
  TGUIDFragment = LongWord; // part of a TGUID
  TGUIDFragments =          // array of TGUID parts - assume same size as TGUID
    array[1..SizeOf(TGUID) div SizeOf(TGUIDFragment)] of TGUIDFragment;
var
  GUID: TGUID;    // generated GUID
  Idx: Integer;   // loops through parts of GUID as an array
begin
  Assert(SizeOf(TGUID) = SizeOf(TGUIDFragments),
    ClassName + '.Generate: Size of TGUID <> size of TGUIDFragments');
  // get a GUID
  CreateGUID(GUID);
  Result := '';
  // crack GUID into parts and build result from hex representation of the parts
  for Idx := Low(TGUIDFragments) to High(TGUIDFragments) do
    Result := Result + IntToHex(
      TGUIDFragments(GUID)[Idx], 2 * SizeOf(TGUIDFragment)
    );
end;

end.

