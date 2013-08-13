{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Basic types used in the snippets database engine.
}


unit CS.Database.Types;

interface

type
  TDBSnippetID = record
  strict private
    var
      fID: string;
  public
    constructor Create(const AIDStr: string);
    class function CreateNew: TDBSnippetID; static;
    class operator Equal(const Left, Right: TDBSnippetID): Boolean; inline;
    class operator NotEqual(const Left, Right: TDBSnippetID): Boolean; inline;
    function ToString: string; inline;
  end;

implementation

uses
  SysUtils;

{ TDBSnippetID }

constructor TDBSnippetID.Create(const AIDStr: string);
begin
  fID := AIDStr;
end;

class function TDBSnippetID.CreateNew: TDBSnippetID;
var
  Bytes: TBytes;
  B: Byte;
  IDStr: string;
begin
  Bytes := TGUID.NewGuid.ToByteArray;
  IDStr := EmptyStr;
  for B in Bytes do
    IDStr := IDStr + IntToHex(B, 2 * SizeOf(B));
  Result := TDBSnippetID.Create(IDStr);
end;

class operator TDBSnippetID.Equal(const Left, Right: TDBSnippetID): Boolean;
begin
  Result := Left.fID = Right.fID;
end;

class operator TDBSnippetID.NotEqual(const Left, Right: TDBSnippetID): Boolean;
begin
  Result := Left.fID <> Right.fID;
end;

function TDBSnippetID.ToString: string;
begin
  Result := fID;
end;

end.
