{
 * UUserDetails.pas
 *
 * Implements a record that encapsulates information a program user.
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
 * The Original Code is UUserDetails.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UUserDetails;


interface


type
  {
  TUserDetails:
    Record that encapsulates information about a program user.
  }
  TUserDetails = record
    Name: string;       // User name
    Email: string;      // User's email address
    constructor Create(const UserName, UserEmail: string);
      {Initialises all a fields of a record.
        @param UserName [in] Name of user.
        @param UserEmail [in] User's email address.
      }
    class function CreateNul: TUserDetails; static;
      {Create a new nul record.
        @return Required initialised record.
      }
    procedure Assign(const Src: TUserDetails);
      {Sets this record's fields to be same as another TUserInfo record.
        @param Src [in] Record containing fields to be copied.
      }
    procedure Init;
      {Initialises record to nul values.
      }
    function IsNul: Boolean;
      {Checks if record is nul (empty).
        @return True if record is nul, False if not.
      }
  end;


implementation


uses
  // Delphi
  SysUtils;


{ TUserDetails }

procedure TUserDetails.Assign(const Src: TUserDetails);
  {Sets this record's fields to be same as another TUserInfo record.
    @param Src [in] Record containing fields to be copied.
  }
begin
  Name := Src.Name;
  Email := Src.Email;
end;

constructor TUserDetails.Create(const UserName, UserEmail: string);
  {Initialises all a fields of a record.
    @param UserName [in] Name of user.
    @param UserEmail [in] User's email address.
  }
begin
  Name := UserName;
  Email := UserEmail;
end;

class function TUserDetails.CreateNul: TUserDetails;
  {Create a new nul record.
    @return Required initialised record.
  }
begin
  Result.Init;
end;

procedure TUserDetails.Init;
  {Initialises record to nul values.
  }
begin
  Name := '';
  Email := '';
end;

function TUserDetails.IsNul: Boolean;
  {Checks if record is nul (empty).
    @return True if record is nul, False if not.
  }
begin
  Result := (Trim(Name) = '') and (Trim(Email) = '');
end;

end.

