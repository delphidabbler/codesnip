{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2011-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a static class that can save and load user details to and from
 * persistent settings.
}


unit UUserDetailsPersist;


interface


uses
  // Project
  UBaseObjects, UUserDetails;


type
  ///  <summary>
  ///  Static class used to save and load user details to and from persistent
  ///  settings.
  ///  </summary>
  TUserDetailsPersist = class(TNoConstructObject)
  public
    ///  <summary>Loads user details from settings.</summary>
    ///  <remarks>Applies default values for name if not present in settings.
    ///  </remarks>
    class function Load: TUserDetails;
    ///  <summary>Saves user details to settings storage.</summary>
    class procedure Save(const Info: TUserDetails);
    ///  <summary>Updates current user details settings in storage.</summary>
    ///  <remarks>Empty fields of Info are not saved, leaving the settings value
    ///  unchanged. Non-empty fields overwrite the current setting.</remarks>
    class procedure Update(const Info: TUserDetails);
  end;


implementation


uses
  // Project
  UAppInfo, USettings, UStrUtils, USystemInfo;


{ TUserDetailsPersist }

class function TUserDetailsPersist.Load: TUserDetails;
var
  UserData: ISettingsSection; // persistent user data settings
  Name: string;               // user name
  Email: string;              // user email address
begin
  inherited;
  UserData := Settings.ReadSection(ssUserInfo);
  Name := StrTrim(UserData.GetString('Name'));
  if Name = '' then
    Name := StrTrim(TComputerInfo.UserName);
  Email := StrTrim(UserData.GetString('Email'));
  Result := TUserDetails.Create(Name, Email);
end;

class procedure TUserDetailsPersist.Save(const Info: TUserDetails);
var
  UserData: ISettingsSection; // persistent user data settings
begin
  UserData := Settings.EmptySection(ssUserInfo);
  UserData.SetString('Name', StrTrim(Info.Name));
  UserData.SetString('Email', StrTrim(Info.Email));
  UserData.Save;
end;

class procedure TUserDetailsPersist.Update(const Info: TUserDetails);
var
  Current: TUserDetails;  // current user details from storage
  Name: string;           // user name
  Email: string;          // user email
begin
  // get current settings
  Current := Load;
  Name := Current.Name;
  Email := Current.Email;
  // only update non-empty values
  if StrTrim(Info.Name) <> '' then
    Name := StrTrim(Info.Name);
  if StrTrim(Info.Email) <> '' then
    Email := StrTrim(Info.Email);
  // store the modified settings
  Save(TUserDetails.Create(Name, Email));
end;

end.
