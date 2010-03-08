{
 * UUserDetails.pas
 *
 * Implements a record that encapsulates information about the program user and
 * a static class that persists the information in settings storage.
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
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UUserDetails;


interface


uses
  // Project
  UBaseObjects;


type
  {
  TUserDetails:
    Record that encapsulates information about the program user.
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

  {
  TUserDetailsPersist:
    Static class used to read and write information about a user from and to
    settings storage.
  }
  TUserDetailsPersist = class(TNoConstructObject)
  public
    class function Load: TUserDetails;
      {Loads user details from settings. Applies default values for name if not
      present in settings.
        @return Loaded user details.
      }
    class procedure Save(const Info: TUserDetails);
      {Saves user details to settings storage. Trims strings values before
      saving.
        @param Info [in] Details of user to save.
      }
    class procedure Update(const Info: TUserDetails);
      {Updates current user details settings in storage.
        @param Info [in] Updated details to be saved. Empty values are not
          saved, leaving current value unchanged. Non-empty values are
          overwritten.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UAppInfo, USettings, USystemInfo;


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

{ TUserDetailsPersist }

class function TUserDetailsPersist.Load: TUserDetails;
  {Loads user details from settings. Applies default values for name if not
  present in settings.
    @return Loaded user details.
  }
var
  UserData: ISettingsSection; // persistent user data settings
begin
  inherited;
  // read details from storage
  UserData := Settings.ReadSection(ssUserInfo);
  Result := TUserDetails.Create(
    UserData.ItemValues['Name'], UserData.ItemValues['Email']
  );
  // if no user name, used registered user name if any, otherwise name of
  // logged on user
  if Result.Name = '' then
    Result.Name := Trim(TAppInfo.RegisteredUser);
  if Result.Name = '' then
    Result.Name := Trim(TComputerInfo.UserName);
end;

class procedure TUserDetailsPersist.Save(const Info: TUserDetails);
  {Saves user details to settings storage. Trims strings values before saving.
    @param Info [in] Details of user to save.
  }
var
  UserData: ISettingsSection; // persistent user data settings
begin
  UserData := Settings.EmptySection(ssUserInfo);
  UserData.ItemValues['Name'] := Trim(Info.Name);
  UserData.ItemValues['Email'] := Trim(Info.Email);
  UserData.Save;
end;

class procedure TUserDetailsPersist.Update(const Info: TUserDetails);
  {Updates current user details settings in storage.
    @param Info [in] Updated details to be saved. Empty values are not saved,
      leaving current value unchanged. Non-empty values are overwritten.
  }
var
  Current: TUserDetails;  // current user details from storage
begin
  // get current settings
  Current := Load;
  // only update non-empty values
  if Trim(Info.Name) <> '' then
    Current.Name := Trim(Info.Name);
  if Trim(Info.Email) <> '' then
    Current.Email := Trim(Info.Email);
  // store the modified settings
  Save(Current);
end;

end.
