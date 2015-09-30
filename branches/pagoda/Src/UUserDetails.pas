{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a record that encapsulates information a program user.
}


unit UUserDetails;


interface


type
  ///  <summary>Record that encapsulates information about a program user.
  ///  </summary>
  TUserDetails = record
  strict private
    var
      ///  <summary>Value of Name property.</summary>
      fName: string;
      ///  <summary>Value of EMail property.</summary>
      fEmail: string;
  public
    ///  <summary>User name.</summary>
    property Name: string read fName;
    ///  <summary>User email.</summary>
    property Email: string read fEmail;
    ///  <summary>Constructs record with given field values.</summary>
    ///  <remarks>Both parameters are trimmed of leading and trailing white
    ///  space.</remarks>
    constructor Create(const UserName, UserEmail: string);
    ///  <summary>Creates and returns a new null (i.e. empty) record.</summary>
    class function CreateNull: TUserDetails; static;
    ///  <summary>Checks if record is null (i.e. empty).</summary>
    function IsNull: Boolean;
    ///  <summary>Returns string representation of user details.</summary>
    function ToString: string;
  end;


implementation


uses
  // Project
  UStrUtils;


{ TUserDetails }

constructor TUserDetails.Create(const UserName, UserEmail: string);
begin
  fName := StrTrim(UserName);
  fEmail := StrTrim(UserEmail);
end;

class function TUserDetails.CreateNull: TUserDetails;
begin
  Result := TUserDetails.Create('', '');
end;

function TUserDetails.IsNull: Boolean;
begin
  Result := (Name = '') and (Email = '');
end;

function TUserDetails.ToString: string;
begin
  // Returns one of following, depending on whether name and email are blank:
  //   +---------+----------+----------------+
  //   | Name is | Email is | Result         |
  //   | blank   | blank    |                |
  //   +---------+----------+----------------+
  //   | No      | No       | 'Name <Email>' |
  //   | No      | Yes      | 'Name'         |
  //   | Yes     | No       | '<Email>'      |
  //   | Yes     | Yes      | ''             |
  //   +---------+----------+----------------+
  if IsNull then
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

