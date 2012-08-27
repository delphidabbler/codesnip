{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a record that encapsulates information a program user.
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

