{
 * DB.UBaseGroup.pas
 *
 * An abstract base class for all group classes in database. Defines common
 * interface and provides common functionality.
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
 * The Original Code is DB.UBaseGroup.pas
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


unit DB.UBaseGroup;

interface

uses
  DB.UDataItem;

type

  TDBBaseGroup = class abstract(TDBDataItem)
  strict protected
    function GetDisplayName: string; virtual; abstract;
  public
    ///  Creates a new group with unique cookie
    constructor Create;
    ///  Name of group for display in UI
    property DisplayName: string read GetDisplayName;
    // TODO -c??? : Add abstract base class to select all snippets in group
  end;


implementation

uses
  DB.UCookies;

{ TDBBaseGroup }

constructor TDBBaseGroup.Create;
begin
  inherited Create(TDBCookie.Create);
end;

end.
