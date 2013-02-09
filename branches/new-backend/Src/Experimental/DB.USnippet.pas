{
 * DB.USnippet.pas
 *
 * Implements classes, records etc. that represent code snippets.
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
 * The Original Code is DB.USnippet.pas
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


unit DB.USnippet;

interface

uses
  DB.UDataItem, DB.UConsts;

type

  TDBSnippetKey = record
  strict private
    fName: string;
    fUserDefined: Boolean;
  public
    constructor Create(const AName: string; const AUserDefined: Boolean);
    property Name: string read fName;
    property UserDefined: Boolean read fUserDefined;
    class operator Equal(Key1, Key2: TDBSnippetKey): Boolean;
    class operator NotEqual(Key1, Key2: TDBSnippetKey): Boolean;
  end;

  TDBSnippetKeys = TArray<TDBSnippetKey>;

  TDBSnippetData = record
  strict private
  public
    var Name: string;
    var UserDefined: Boolean;
    var Description: string;
    var Kind: TDBSnippetKind;
    // NOTE: use properties for assigning objects rather than copying references
    // ...
    function Clone: TDBSnippetData;
  end;

  TDBSnippet = class(TDBDataItem)
  strict private
    fKey: TDBSnippetKey;
    fDescription: string;
    fKind: TDBSnippetKind;
    function GetName: string;
    function GetUserDefined: Boolean;
  strict protected
    procedure UpdateData(Data: TDBSnippetData);
  public
    constructor Create(const Data: TDBSnippetData);
    property Key: TDBSnippetKey read fKey;
    property Name: string read GetName;
    property UserDefined: Boolean read GetUserDefined;
    property Description: string read fDescription;
    property Kind: TDBSnippetKind read fKind;
    // ...
  end;

implementation

uses
  SysUtils, Types,
  DB.UCookies;

{ TDBSnippet }

constructor TDBSnippet.Create(const Data: TDBSnippetData);
begin
  inherited Create(TDBCookie.Create);
  UpdateData(Data);
end;

function TDBSnippet.GetName: string;
begin
  Result := fKey.Name;
end;

function TDBSnippet.GetUserDefined: Boolean;
begin
  Result := fKey.UserDefined;
end;

procedure TDBSnippet.UpdateData(Data: TDBSnippetData);
begin
  fKey := TDBSnippetKey.Create(Data.Name, Data.UserDefined);
  fDescription := Data.Description;
  fKind := Data.Kind;
end;

{ TDBSnippetKey }

constructor TDBSnippetKey.Create(const AName: string;
  const AUserDefined: Boolean);
begin
  fName := AName;
  fUserDefined := AUserDefined;
end;

class operator TDBSnippetKey.Equal(Key1, Key2: TDBSnippetKey): Boolean;
begin
  if not AnsiSameText(Key1.Name, Key2.Name) then
    Exit(False);
  Result := Key1.UserDefined = Key2.UserDefined;
end;

class operator TDBSnippetKey.NotEqual(Key1, Key2: TDBSnippetKey): Boolean;
begin
  Result := not (Key1 = Key2);
end;

{ TDBSnippetData }

function TDBSnippetData.Clone: TDBSnippetData;
begin
  Result.Name := Name;
  Result.UserDefined := UserDefined;
  Result.Description := Description;
  Result.Kind := Kind;
end;

end.

