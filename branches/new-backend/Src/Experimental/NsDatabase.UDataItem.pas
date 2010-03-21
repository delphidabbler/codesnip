{
 * NsDatabase.UDataItem.pas
 *
 * Implements a base class for all database data items that can be stored in a
 * data pool.
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
 * The Original Code is NsDatabase.UDataItem.pas
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


unit NsDatabase.UDataItem;

interface

uses
  UBaseObjects, NsDatabase.UCookies;

type

  TDBDataItem = class(TOwnedConditionalFreeObject)
  private
    fCookie: TDBCookie;
  public
    // todo: make constructor create own cookie??
    constructor Create(const Cookie: TDBCookie);
    property Cookie: TDBCookie read fCookie;
  end;

implementation

{ TDBDataItem }

constructor TDBDataItem.Create(const Cookie: TDBCookie);
begin
  // create with no owner
  inherited Create;
  fCookie := Cookie;
end;

end.
