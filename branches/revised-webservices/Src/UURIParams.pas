{
 * UURIParams.pas
 *
 * Encapsulates URI query string parameters and generates the query string from
 * the parameters.
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
 * The Original Code is UURIParams.pas
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


unit UURIParams;

interface

uses
  SysUtils, Classes, Generics.Collections, Generics.Defaults;

type

  // todo: Move this to own unit [UComparers] (with ElfHash function below)
  {
  TSameStringEqualityComparer:
    Case insensitive string comparer.
  }
  TSameStringEqualityComparer = class(TEqualityComparer<string>,
    IEqualityComparer<string>
  )
    function Equals(const Left, Right: string): Boolean; override;
    function GetHashCode(const Value: string): Integer; override;
  end;

  TURIParam = record
    Name, Value: string;
    constructor Create(const AName, AValue: string);
  end;

  TURIParams = class(TObject)
  strict private
    fDict: TDictionary<string,string>;
    function BuildQueryString(const Encode: TFunc<string,string>): string;
  public
    constructor Create; overload;
    constructor Create(const Name, Value: string); overload;
    constructor Create(const Param: TURIParam); overload;
    constructor Create(const Params: array of TURIParam); overload;
    constructor Create(const Params: TStrings); overload;
    constructor Create(const Params: array of string); overload;
    destructor Destroy; override;
    procedure Add(const Name, Value: string); overload;
    procedure Add(const Param: TURIParam); overload;
    procedure Add(const Params: array of TURIParam); overload;
    procedure Add(const Params: TStrings); overload;
    procedure Add(const Params: array of string); overload;
    function Exists(const Name: string): Boolean; overload;
    function Exists(const Param: TURIParam): Boolean; overload;
    procedure Update(const Name, Value: string); overload;
    procedure Update(const Param: TURIParam); overload;
    function QueryString: string;
    function EncodedQueryString: string;
    function IsEmpty: Boolean;
  end;

implementation

uses
WINDOWS,
  UURIEncode;

// Source: http://www.scalabium.com/faq/dct0136.htm
// TODO: Move to UUtils or UComparers or somewhere like that or to hashes unit
function ElfHash(const Value: string): Integer;
var
  i, x: Integer;
begin
  Result := 0;
  for i := 1 to Length(Value) do
  begin
    Result := (Result shl 4) + Ord(Value[i]);
    x := Result and $F0000000;
    if (x <> 0) then
      Result := Result xor (x shr 24);
    Result := Result and (not x);
  end;
end;

{ TURIParams }

procedure TURIParams.Add(const Param: TURIParam);
begin
  Add(Param.Name, Param.Value);
end;

procedure TURIParams.Add(const Name, Value: string);
begin
  fDict.Add(Name, Value);
end;

procedure TURIParams.Add(const Params: array of TURIParam);
var
  Param: TURIParam;
begin
  for Param in Params do
    Add(Param);
end;

procedure TURIParams.Add(const Params: TStrings);
var
  Idx: Integer;
begin
  Assert(Assigned(Params), ClassName + '.Add(TStrings): Params is nil');
  for Idx := 0 to Pred(Params.Count) do
    Add(Params.Names[Idx], Params.ValueFromIndex[Idx]);
end;

procedure TURIParams.Add(const Params: array of string);
var
  Param: string;
  ParamList: TStrings;
begin
  ParamList := TStringList.Create;
  try
    for Param in Params do
      ParamList.Add(Param);
    Add(ParamList);
  finally
    ParamList.Free;
  end;
end;

function TURIParams.BuildQueryString(
  const Encode: TFunc<string, string>): string;
var
  AParam: TPair<string,string>;
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create;
  try
    for AParam in fDict do
    begin
      if SB.Length > 0 then
        SB.Append('&');
      SB.Append(Encode(AParam.Key));
      if AParam.Value <> '' then
        SB.AppendFormat('=%s', [Encode(AParam.Value)]);
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

constructor TURIParams.Create;
begin
  inherited Create;
  fDict := TDictionary<string,string>.Create(
    24, TSameStringEqualityComparer.Create
  );
end;

constructor TURIParams.Create(const Name, Value: string);
begin
  Create;
  Add(Name, Value);
end;

constructor TURIParams.Create(const Param: TURIParam);
begin
  Create;
  Add(Param);
end;

constructor TURIParams.Create(const Params: array of string);
begin
  Create;
  Add(Params);
end;

constructor TURIParams.Create(const Params: array of TURIParam);
begin
  Create;
  Add(Params);
end;

constructor TURIParams.Create(const Params: TStrings);
begin
  Create;
  Add(Params);
end;

destructor TURIParams.Destroy;
begin
  fDict.Free;
  inherited;
end;

function TURIParams.EncodedQueryString: string;
begin
  Result := BuildQueryString(
    function(S: string): string
    begin
      Result := URIEncodeQueryString(S);
    end
  );
end;

function TURIParams.Exists(const Name: string): Boolean;
begin
  Result := fDict.ContainsKey(Name);
end;

function TURIParams.Exists(const Param: TURIParam): Boolean;
begin
  Result := Exists(Param.Name);
end;

function TURIParams.IsEmpty: Boolean;
begin
  Result := fDict.Count = 0;
end;

function TURIParams.QueryString: string;
begin
  Result := BuildQueryString(
    function(S: string): string
    begin
      Result := S;
    end
  );
end;

procedure TURIParams.Update(const Name, Value: string);
begin
  fDict.Items[Name] := Value;
end;

procedure TURIParams.Update(const Param: TURIParam);
begin
  Update(Param.Name, Param.Value);
end;

{ TURIParam }

constructor TURIParam.Create(const AName, AValue: string);
begin
  Name := AName;
  Value := AValue;
end;

{ TSameStringEqualityComparer }

function TSameStringEqualityComparer.Equals(const Left, Right: string): Boolean;
begin
  Result := AnsiSameText(Left, Right);
end;

function TSameStringEqualityComparer.GetHashCode(const Value: string): Integer;
begin
  // Comparison takes place only done if hashes are same => must ignore case in
  // hash if two strings in different case are to be considered same => hash of
  // 'aaaa' = hash of 'AAAA' => Equals gets called and returns true
  Result := ElfHash(AnsiLowerCase(Value));
end;

end.

