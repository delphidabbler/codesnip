{
 * NsDatabase.UCookies.pas
 *
 * Defines cookie type used throughout database along with a class that
 * generates unique cookies.
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
 * The Original Code is NsDatabase.UCookies.pas
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


unit NsDatabase.UCookies;


interface


uses
  // Project
  UBaseObjects;


type
  TDBCookie = UInt64;

  TDBCookieGenerator = class(TNoConstructObject)
  strict private
    class var fCookie: TDBCookie;
  public
    class function GetCookie: TDBCookie;
    class procedure Reset;
  end;


implementation


{ TDBCookieGenerator }

class function TDBCookieGenerator.GetCookie: TDBCookie;
begin
  Inc(fCookie);
  Result := fCookie;
end;

class procedure TDBCookieGenerator.Reset;
begin
  fCookie := 0;
end;

end.
