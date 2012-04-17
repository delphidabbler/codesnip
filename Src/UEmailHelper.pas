{
 * UEmailHelper.pas
 *
 * Provides a helper routine for use in validation of email addresses.
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
 * The Original Code is UEmailHelper.pas
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


unit UEmailHelper;


interface


function IsValidEmailAddress(const EmailAddr: string): Boolean;
  {Checks validity of an email address.
    @param EmailAddr [in] Email address to check.
    @return False if address is invalid, True if may be valid.
  }


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UUtils;


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
  AtPos := AnsiPos('@', EmailAddr);
  if AtPos < 2 then
    Exit;
  // Check if there's a '.' after '@' symbol
  LastDotPos := LastPos('.', EmailAddr);
  if (LastDotPos - AtPos < 2) or (LastDotPos > Length(EmailAddr) - 2) then
    Exit;
  Result := True;
end;

end.
