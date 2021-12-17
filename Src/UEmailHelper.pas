{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Provides a helper routine for use in validation of email addresses.
}


unit UEmailHelper;


interface


function IsValidEmailAddress(const EmailAddr: string): Boolean;
  {Checks validity of an email address.
    @param EmailAddr [in] Email address to check.
    @return False if address is invalid, True if may be valid.
  }


implementation


uses
  // Project
  UStrUtils;


function IsValidEmailAddress(const EmailAddr: string): Boolean;
  {Checks validity of an email address.
    @param EmailAddr [in] Email address to check.
    @return False if address is invalid, True if may be valid.
  }
var
  AtPos: Integer;       // position of '@' charater in email address
  LastDotPos: Integer;  // position of last '.' in email address
begin
  Result := False;
  // Check if there's an '@' symbol and its not too close to start
  AtPos := StrPos('@', EmailAddr);
  if AtPos < 2 then
    Exit;
  // Check if there's a '.' after '@' symbol
  LastDotPos := StrLastPos('.', EmailAddr);
  if (LastDotPos - AtPos < 2) or (LastDotPos > Length(EmailAddr) - 2) then
    Exit;
  Result := True;
end;

end.
