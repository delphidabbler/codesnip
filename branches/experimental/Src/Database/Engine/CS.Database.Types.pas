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

uses
  Generics.Collections, Generics.Defaults,
  CS.Markup;

type
  TDBSnippetID = record
  public
    type
      TEqualityComparer = class(TEqualityComparer<TDBSnippetID>)
      public
        function Equals(const Left, Right: TDBSnippetID): Boolean; override;
        function GetHashCode(const Value: TDBSnippetID): Integer; override;
      end;
  strict private
    var
      fID: string;
  public
    constructor Create(const AIDStr: string);
    class function CreateNew: TDBSnippetID; static;
    class operator Equal(const Left, Right: TDBSnippetID): Boolean; inline;
    class operator NotEqual(const Left, Right: TDBSnippetID): Boolean; inline;
    function ToString: string; inline;
    function Hash: Integer; inline;
  end;

  TDBLanguage = record
  strict private
    var
      fName: string;
  public
    constructor Create(const AName: string);
    class operator Equal(const Left, Right: TDBLanguage): Boolean; inline;
    class operator NotEqual(const Left, Right: TDBLanguage): Boolean; inline;
    class function Compare(const Left, Right: TDBLanguage): Integer; static;
      inline;
    function CompareTo(const Other: TDBLanguage): Integer; inline;
    function ToString: string; inline;
  end;

  TDBSnippetProp = (
    spTitle, spDescription, spSourceCode, spLanguage, spLastUpdated
  );

  TDBSnippetProps = set of TDBSnippetProp;

  IDBSnippet = interface(IInterface)
    ['{29AF7828-DAC2-442D-8156-B335D66A255E}']
    function GetID: TDBSnippetID;
    property ID: TDBSnippetID read GetID;

    function GetTitle: string;
    procedure SetTitle(const ATitle: string);
    property Title: string read GetTitle write SetTitle;

    function GetDescription: TMarkup;
    procedure SetDescription(const ADescription: TMarkup);
    property Description: TMarkup read GetDescription write SetDescription;

    function GetSourceCode: string;
    procedure SetSourceCode(const ASourceCode: string);
    property SourceCode: string read GetSourceCode write SetSourceCode;

    function GetLanguage: TDBLanguage;
    procedure SetLanguage(const ALanguage: TDBLanguage);
    property Language: TDBLanguage read GetLanguage write SetLanguage;

    function GetLastUpdated: TDateTime; // TODO: create a new record for dates
    procedure SetLastUpdated(const ALastUpdated: TDateTime);
    property LastUpdated: TDateTime read GetLastUpdated write SetLastUpdated;

    function GetValidProperties: TDBSnippetProps;
    property ValidProperties: TDBSnippetProps read GetValidProperties;

    function SupportsProperty(const AProp: TDBSnippetProp): Boolean;
    function SupportsAllProperties: Boolean;
    procedure Update(AFrom: IDBSnippet);
  end;

  IDBSnippetIDList = interface(IInterface)
    ['{842B9A92-6CD5-4DC3-9DA7-9753B08A52AB}']
    function GetEnumerator: TEnumerator<TDBSnippetID>;
    procedure Add(const ID: TDBSnippetID);
    procedure Delete(const ID: TDBSnippetID);
    function Contains(const ID: TDBSnippetID): Boolean;
    function GetItem(const Idx: Integer): TDBSnippetID;
    function GetCount: Integer;
    property Items[const Idx: Integer]: TDBSnippetID read GetItem;
    property Count: Integer read GetCount;
  end;

  TDBFilter = reference to function (ASnippet: IDBSnippet): Boolean;

implementation

uses
  SysUtils,

  CS.Utils.Hashes,
  UStrUtils;

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

function TDBSnippetID.Hash: Integer;
begin
  Result := Integer(PaulLarsonHash(fID));
end;

class operator TDBSnippetID.NotEqual(const Left, Right: TDBSnippetID): Boolean;
begin
  Result := Left.fID <> Right.fID;
end;

function TDBSnippetID.ToString: string;
begin
  Result := fID;
end;

{ TDBSnippetID.TEqualityComparer }

function TDBSnippetID.TEqualityComparer.Equals(const Left,
  Right: TDBSnippetID): Boolean;
begin
  Result := Left = Right;
end;

function TDBSnippetID.TEqualityComparer.GetHashCode(
  const Value: TDBSnippetID): Integer;
begin
  Result := Value.Hash;
end;

{ TDBLanguage }

class function TDBLanguage.Compare(const Left, Right: TDBLanguage): Integer;
begin
  Result := StrCompareText(Left.fName, Right.fName);
end;

function TDBLanguage.CompareTo(const Other: TDBLanguage): Integer;
begin
  Result := Compare(Self, Other);
end;

constructor TDBLanguage.Create(const AName: string);
begin
  fName := AName;
end;

class operator TDBLanguage.Equal(const Left, Right: TDBLanguage): Boolean;
begin
  Result := Compare(Left, Right) = 0;
end;

class operator TDBLanguage.NotEqual(const Left, Right: TDBLanguage): Boolean;
begin
  Result := Compare(Left, Right) <> 0;
end;

function TDBLanguage.ToString: string;
begin
  Result := fName;
end;

end.

