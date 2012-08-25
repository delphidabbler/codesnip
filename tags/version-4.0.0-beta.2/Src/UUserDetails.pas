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
    ///  <summary>Value of Name property.</summary>
    fName: string;
    ///  <summary>Value of EMail property.</summary>
    fEmail: string;
    ///  <summary>Setter for Name property.</summary>
    ///  <remarks>Trims leading and trailing spaces from new value.</summary>
    procedure SetName(const AName: string);
    ///  <summary>Setter for Email property.</summary>
    ///  <remarks>Trims leading and trailing spaces from new value.</summary>
    procedure SetEmail(const AEmail: string);
  public
    ///  <summary>User name.</summary>
    property Name: string read fName write SetName;
    ///  <summary>User email.</summary>
    property Email: string read fEmail write SetEmail;
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
    ///  <summary>Returns string representation of user details.</summary>
    function ToString: string;
  end;


implementation


uses
  // Project
  UStrUtils;


{ TUserDetails }

procedure TUserDetails.Assign(const Src: TUserDetails);
begin
  SetName(Src.Name);
  SetEmail(Src.Email);
end;

constructor TUserDetails.Create(const UserName, UserEmail: string);
begin
  SetName(UserName);
  SetEmail(UserEmail);
end;

class function TUserDetails.CreateNul: TUserDetails;
begin
  Result.Init;
end;

procedure TUserDetails.Init;
begin
  SetName('');
  SetEmail('');
end;

function TUserDetails.IsNul: Boolean;
begin
  Result := (Name = '') and (Email = '');
end;

procedure TUserDetails.SetEmail(const AEmail: string);
begin
  fEmail := StrTrim(AEmail);
end;

procedure TUserDetails.SetName(const AName: string);
begin
  fName := StrTrim(AName);
end;

function TUserDetails.ToString: string;
begin
  // Returns one of following:
  // +-----------------+----------+-----------+
  // | Result          | Name Set | Email Set |
  // +-----------------+----------+-----------+
  // | ''              | No       | No        |
  // | 'Name'          | Yes      | No        |
  // | '<Email>'       | No       | Yes       |
  // | 'Name <Email>'  | Yes      | Yes       |
  // +-----------------+----------+-----------+
  if IsNul then
    Exit('');
  Result := Name;
  if Email <> '' then
  begin
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + '<' + Email + '>';
  end;
end;

end.

