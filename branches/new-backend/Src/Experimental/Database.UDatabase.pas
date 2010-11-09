{
 * Database.UDatabase.pas
 *
 * Implements top level interface to Code Snippets database backend.
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
 * The Original Code is Database.UDatabase.pas, formerly
 * NsDatabase.UDatabase.pas.
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


unit Database.UDatabase;

interface

uses
  USingleton;

type

  TCSDatabase = class(TSingleton)
  strict private
    class var fInstance: TCSDatabase;
  strict protected
    procedure Initialize; override;
    procedure Finalize; override;
  public
    class function Instance: TCSDatabase;
    // todo: add database services objects here
  end;

function Database: TCSDatabase;

implementation

function Database: TCSDatabase;
begin
  Result := TCSDatabase.Instance;
end;

{ TCSDatabase }

procedure TCSDatabase.Finalize;
begin
  inherited;
end;

procedure TCSDatabase.Initialize;
begin
  inherited;
end;

class function TCSDatabase.Instance: TCSDatabase;
begin
  // we CAN call constructor as often as we like, but there's some overhead to
  // doing so since singleton list is checked for instances, so we cache the
  // first instance created
  if not Assigned(fInstance) then
    fInstance := TCSDatabase.Create;
  Result := fInstance;
end;

end.
