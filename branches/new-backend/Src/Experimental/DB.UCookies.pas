{
 * DB.UCookies.pas
 *
 * Defines cookie type used throughout database along operations to generate
 * unique and nul cookies, compare cookies and test for nul cookies.
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
 * The Original Code is DB.UCookies.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit DB.UCookies;


interface


uses
  // Project
  UBaseObjects;


type
  TDBCookie = record
  private
    Value: UInt64;
  public
    class function CreateNul: TDBCookie; static;
    class function Create: TDBCookie; static;
    class operator Equal(const C1, C2: TDBCookie): Boolean; inline;
    class operator NotEqual(const C1, C2: TDBCookie): Boolean; inline;
    function Hash: Integer; inline;
    function IsNul: Boolean;
    class procedure Reset; static;
  end;


implementation

type
  TDBCookieValueGenerator = class(TNoConstructObject)
  strict private
    class var fCookieValue: UInt64;
  public
    const NulCookieValue = UInt64(0);
    class function GetCookieValue: UInt64; inline;
    class procedure Reset; inline;
  end;


{ TDBCookieValueGenerator }

class function TDBCookieValueGenerator.GetCookieValue: UInt64;
begin
  Inc(fCookieValue);
  Result := fCookieValue;
end;

class procedure TDBCookieValueGenerator.Reset;
begin
  fCookieValue := 0;
end;

{ TDBCookie }

class function TDBCookie.Create: TDBCookie;
begin
  Result.Value := TDBCookieValueGenerator.GetCookieValue;
  Assert(Result.Value <> 0);
end;

class function TDBCookie.CreateNul: TDBCookie;
begin
  Result.Value := TDBCookieValueGenerator.NulCookieValue;
end;

class operator TDBCookie.Equal(const C1, C2: TDBCookie): Boolean;
begin
  Result := C1.Value = C2.Value;
end;

function TDBCookie.Hash: Integer;
begin
  Result := Integer(Value); // simply truncate cookie value to integer
end;

function TDBCookie.IsNul: Boolean;
begin
  Result := Value = TDBCookieValueGenerator.NulCookieValue;
end;

class operator TDBCookie.NotEqual(const C1, C2: TDBCookie): Boolean;
begin
  Result := C1.Value <> C2.Value;
end;

class procedure TDBCookie.Reset;
begin
  TDBCookieValueGenerator.Reset;
end;

end.

