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
  SysUtils,
  Generics.Collections, Generics.Defaults,
  CS.Markup,
  CS.SourceCode.Languages,
  CS.Utils.Dates,
  UIStringList;

type
  EDBSnippetID = class(Exception);

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
    const
      MaxIDStringLength = 64;
  public
    constructor Create(const AIDStr: string);
    class function CreateNew: TDBSnippetID; static;
    class operator Equal(const Left, Right: TDBSnippetID): Boolean; inline;
    class operator NotEqual(const Left, Right: TDBSnippetID): Boolean; inline;
    function ToString: string; inline;
    function Hash: Integer; inline;
    class function IsValidIDString(const S: string): Boolean; static;
  end;

  IDBSnippetIDList = interface(IInterface)
    ['{842B9A92-6CD5-4DC3-9DA7-9753B08A52AB}']
    function GetEnumerator: TEnumerator<TDBSnippetID>;
    procedure Add(const ID: TDBSnippetID);
    procedure Delete(const ID: TDBSnippetID);
    procedure Clear;
    function Contains(const ID: TDBSnippetID): Boolean;
    function GetItem(const Idx: Integer): TDBSnippetID;
    function GetCount: Integer;
    property Items[const Idx: Integer]: TDBSnippetID read GetItem;
    property Count: Integer read GetCount;
  end;

  TDBSnippetKind = (
    skFreeform,   // free-form code - not in any of other supported formats
    skRoutine,    // procedure or function in standard format
    skConstant,   // constant definition in standard format
    skTypeDef,    // type definition in standard format
    skUnit,       // complete source code unit
    skClass       // Delphi class or record with methods
  );

  TDBSnippetProp = (
    spID, spTitle, spDescription, spSourceCode, spLanguageID, spModified,
    spCreated, spRequiredModules, spRequiredSnippets, spXRefs, spNotes, spKind
  );

  TDBSnippetProps = set of TDBSnippetProp;

  ISnippetBase = interface(IInterface)
    ['{0F915A15-441B-4180-A9C2-41C52AF63C8A}']
    function GetID: TDBSnippetID;
    function GetCreated: TUTCDateTime;
    function GetModified: TUTCDateTime;
    function GetTitle: string;
    function GetDescription: TMarkup;
    function GetSourceCode: string;
    function GetLanguageID: TSourceCodeLanguageID;
    function GetRequiredModules: IStringList;
    function GetRequiredSnippets: IDBSnippetIDList;
    function GetXRefs: IDBSnippetIDList;
    function GetNotes: TMarkup;
    function GetKind: TDBSnippetKind;

    property ID: TDBSnippetID read GetID;
    property Created: TUTCDateTime read GetCreated;
    property Modified: TUTCDateTime read GetModified;
  end;

  IReadOnlySnippet = interface(ISnippetBase)
    ['{BD221CF4-482D-4FF9-BDAE-D320DDEBD578}']
    function GetValidProperties: TDBSnippetProps;

    property Title: string read GetTitle;
    property Description: TMarkup read GetDescription;
    property SourceCode: string read GetSourceCode;
    property LanguageID: TSourceCodeLanguageID read GetLanguageID;
    property RequiredModules: IStringList read GetRequiredModules;
    property RequiredSnippets: IDBSnippetIDList read GetRequiredSnippets;
    property XRefs: IDBSnippetIDList read GetXRefs;
    property Notes: TMarkup read GetNotes;
    property Kind: TDBSnippetKind read GetKind;

    // TODO: query if following properties are required
    property ValidProperties: TDBSnippetProps read GetValidProperties;
    function SupportsProperty(const AProp: TDBSnippetProp): Boolean;
  end;

  ISnippet = interface(ISnippetBase)
    ['{75F44D0E-7611-4A8D-A4F5-F7612E466238}']
    procedure SetTitle(const ATitle: string);
    procedure SetDescription(const ADescription: TMarkup);
    procedure SetSourceCode(const ASourceCode: string);
    procedure SetLanguageID(const ALanguageID: TSourceCodeLanguageID);
    procedure SetRequiredModules(AModuleList: IStringList);
    procedure SetRequiredSnippets(AIDList: IDBSnippetIDList);
    procedure SetXRefs(AIDList: IDBSnippetIDList);
    procedure SetNotes(const ANotes: TMarkup);
    procedure SetKind(const ASnippetKind: TDBSnippetKind);

    property Title: string read GetTitle write SetTitle;
    property Description: TMarkup read GetDescription write SetDescription;
    property SourceCode: string read GetSourceCode write SetSourceCode;
    property LanguageID: TSourceCodeLanguageID read GetLanguageID
      write SetLanguageID;
    property RequiredModules: IStringList read GetRequiredModules
      write SetRequiredModules;
    property RequiredSnippets: IDBSnippetIDList read GetRequiredSnippets
      write SetRequiredSnippets;
    property XRefs: IDBSnippetIDList read GetXRefs write SetXRefs;
    property Notes: TMarkup read GetNotes write SetNotes;
    property Kind: TDBSnippetKind read GetKind write SetKind;
  end;

  TDBFilter = reference to function (ASnippet: IReadOnlySnippet): Boolean;

implementation

uses
  Character,

  CS.Utils.Hashes,
  UStrUtils;

{ TDBSnippetID }

constructor TDBSnippetID.Create(const AIDStr: string);
resourcestring
  sInvalidIDStr = '"%s" is not a valid snippet ID string';
begin
  if not IsValidIDString(AIDStr) then
    raise EDBSnippetID.CreateFmt(sInvalidIDStr, [AIDStr]);
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

class function TDBSnippetID.IsValidIDString(const S: string): Boolean;

  function IsValidChar(const Ch: Char): Boolean;
  const
    ValidPunctChars = ['_', '-'];
  begin
    Result := TCharacter.IsLetterOrDigit(Ch) or CharInSet(Ch, ValidPunctChars);
  end;

var
  Ch: Char;
begin
  // check for empty string
  if S = EmptyStr then
    Exit(False);
  // check for long string
  if Length(S) > MaxIDStringLength then
    Exit(False);
  // check for valid characters
  for Ch in S do
    if not IsValidChar(Ch) then
      Exit(False);
  Result := True;
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

end.

