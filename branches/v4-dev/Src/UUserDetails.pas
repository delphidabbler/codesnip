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
  ///  <summary>
  ///  Record that encapsulates information about a program user.
  ///  </summary>
  TUserDetails = record
  strict private
    // Property values
    fName: string;
    fEmail: string;
  public
    ///  <summary>User name.</summary>
    property Name: string read fName write fName;
    ///  <summary>User email.</summary>
    property Email: string read fEmail write fEmail;
    ///  <summary>Constructs record with given field values.</summary>
    constructor Create(const UserName, UserEmail: string);
    ///  <summary>Creates and returns a new null record.</summary>
    class function CreateNul: TUserDetails; static;
    ///  <summary>Assigns the field values of a given record to this record.
    ///  </summary>
    procedure Assign(const Src: TUserDetails);
    ///  <summary>Initialises the record to null values.</summary>
    procedure Init;
    ///  <summary>Checks if record is null, i.e. empty.</summary>
    function IsNul: Boolean;
  end;


implementation


uses
  // Delphi
  SysUtils;


{ TUserDetails }

procedure TUserDetails.Assign(const Src: TUserDetails);
begin
  fName := Src.Name;
  fEmail := Src.Email;
end;

constructor TUserDetails.Create(const UserName, UserEmail: string);
begin
  fName := UserName;
  fEmail := UserEmail;
end;

class function TUserDetails.CreateNul: TUserDetails;
begin
  Result.Init;
end;

procedure TUserDetails.Init;
begin
  fName := '';
  fEmail := '';
end;

function TUserDetails.IsNul: Boolean;
begin
  Result := (Trim(Name) = '') and (Trim(Email) = '');
end;

end.

