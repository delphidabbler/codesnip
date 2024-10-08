{
  This Source Code Form is subject to the terms of the Mozilla Public License,
  v. 2.0. If a copy of the MPL was not distributed with this file, You can
  obtain one at https://mozilla.org/MPL/2.0/

  Copyright (C) 2024, Peter Johnson (gravatar.com/delphidabbler).

  Various conversion routines.
}

unit CSLE.Utils.Conversions;

interface

function TryStrToUint16(const AStr: string; out Value: UInt16): Boolean;

implementation

uses
  System.SysUtils;

function TryStrToUInt16(const AStr: string; out Value: UInt16): Boolean;
begin
  var IntValue: Integer;
  if not TryStrToInt(AStr, IntValue) then
    Exit(False);
  if (IntValue < 0) or (IntValue > High(UInt16)) then
    Exit(False);
  Value := UInt16(IntValue);
  Result := True;
end;

end.
