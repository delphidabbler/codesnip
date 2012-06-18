{
 * UUserDetailsPersist.pas
 *
 * Implements a static class that can save and load user details to and from
 * persistent settings.
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
 * The Original Code is UUserDetailsPersist.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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
  Name := StrTrim(UserData.ItemValues['Name']);
  if Name = '' then
    Name := StrTrim(TAppInfo.RegisteredUser);
  if Name = '' then
    Name := StrTrim(TComputerInfo.UserName);
  Email := StrTrim(UserData.ItemValues['Email']);
  Result := TUserDetails.Create(Name, Email);
end;

class procedure TUserDetailsPersist.Save(const Info: TUserDetails);
var
  UserData: ISettingsSection; // persistent user data settings
begin
  UserData := Settings.EmptySection(ssUserInfo);
  UserData.ItemValues['Name'] := StrTrim(Info.Name);
  UserData.ItemValues['Email'] := StrTrim(Info.Email);
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
