{
 * UUniqueID.pas
 *
 * Implements a static class that generates globally unique id strings.
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
 * The Original Code is UUniqueID.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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

