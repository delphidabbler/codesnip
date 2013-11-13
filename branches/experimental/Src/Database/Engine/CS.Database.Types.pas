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
  Compilers.UGlobals,
  UIStringList;

type
  EDBSnippetID = class(Exception);

  TDBSnippetID = record
  public
    type
      TComparer = class(TComparer<TDBSnippetID>)
        function Compare(const Left, Right: TDBSnippetID): Integer; override;
      end;
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
    class operator Equal(const Left, Right: TDBSnippetID): Boolean;
    class operator NotEqual(const Left, Right: TDBSnippetID): Boolean;

    class function Compare(const Left, Right: TDBSnippetID): Integer; static;
    function ToString: string; inline;
    function Hash: Integer;
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

  // TODO: move TCompilerIDs to Compilers.UGlobals or successor unit
  TCompilerIDs = set of TCompilerID;

  // TODO: possibly move TDBCompileResults to Compilers.UGlobals or successor
  TDBCompileResults = record
  strict private
    var
      fSucceeds: TCompilerIDs;
      fFails: TCompilerIDs;
    function GetUnknown: TCompilerIDs;
  public
    type
      TTestRule = (trAnd, trOr);
  public
    constructor Create(const ASucceeds, AFails: TCompilerIDs);
    class function CreateNull: TDBCompileResults; static; inline;
    property Succeeds: TCompilerIDs read fSucceeds;
    property Fails: TCompilerIDs read fFails;
    property Unknown: TCompilerIDs read GetUnknown;
    function IsNull: Boolean; inline;
    function SucceedsWith(const Compilers: TCompilerIDs; const Rule: TTestRule):
      Boolean;
    function FailsWith(const Compilers: TCompilerIDs; const Rule: TTestRule):
      Boolean;
  end;

  EDBTag = class(Exception);

  TDBTag = record
  public
    type
      TComparer = class(TComparer<TDBTag>)
        function Compare(const Left, Right: TDBTag): Integer; override;
      end;
      TEqualityComparer = class(TEqualityComparer<TDBTag>)
      public
        function Equals(const Left, Right: TDBTag): Boolean; override;
        function GetHashCode(const Value: TDBTag): Integer; override;
      end;
  strict private
    var
      fTag: string;
    class function PrepareTagString(const AStr: string): string; static;
    class function IsValidPreparedTagString(const AStr: string): Boolean;
      static;
  public
    constructor Create(const ATagStr: string);
    class operator Equal(const Left, Right: TDBTag): Boolean; inline;
    class operator NotEqual(const Left, Right: TDBTag): Boolean; inline;
    class function IsValidTagString(const AStr: string): Boolean; static;
      inline;
    class function Compare(const Left, Right: TDBTag): Integer; static; inline;
    function ToString: string; inline;
    function ToDisplayString: string;
    function Hash: Integer; inline;
  end;

  IDBTagList = interface(IInterface)
    ['{BFADA81C-8226-4350-BDEC-12816792AB78}']
    function GetEnumerator: TEnumerator<TDBTag>;
    procedure Add(const ATag: TDBTag);
    procedure Delete(const ATag: TDBTag);
    procedure Clear;
    function Contains(const ATag: TDBTag): Boolean;
    function GetItem(const Idx: Integer): TDBTag;
    function GetCount: Integer;
    property Items[const Idx: Integer]: TDBTag read GetItem;
    property Count: Integer read GetCount;
  end;

  TSnippetOriginKind = (
    soUser, soImport, soSWAG, soSynchSpace
  );

  { TODO: Implement these (Null implementation would be useful since it will
          apply to all user-defined snippets. }
  ISnippetLinkInfo = interface(IInterface)
    ['{F98BBE9C-B9F1-4FC7-BDE2-BBC90958BF52}']
    function IsLinked: Boolean;
    function GetSynchSpaceID: TGUID;
    function GetLinkedSnippetID: TDBSnippetID;
    property SynchSpaceID: TGUID read GetSynchSpaceID;
    property LinkedSnippetID: TDBSnippetID read GetLinkedSnippetID;
  end;

  TDBSnippetProp = (
    spID, spTitle, spDescription, spSourceCode, spLanguageID, spModified,
    spCreated, spRequiredModules, spRequiredSnippets, spXRefs, spNotes, spKind,
    spCompileResults, spTags
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
    function GetCompileResults: TDBCompileResults;
    function GetTags: IDBTagList;
    function GetLinkInfo: ISnippetLinkInfo;

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
    property CompileResults: TDBCompileResults read GetCompileResults;
    property Tags: IDBTagList read GetTags;
    property LinkInfo: ISnippetLinkInfo read GetLinkInfo;

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
    procedure SetCompileResults(const AResults: TDBCompileResults);
    procedure SetTags(ATagList: IDBTagList);
    procedure SetLinkInfo(ALinkInfo: ISnippetLinkInfo);

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
    property CompileResults: TDBCompileResults read GetCompileResults
      write SetCompileResults;
    property Tags: IDBTagList read GetTags write SetTags;
    property LinkInfo: ISnippetLinkInfo read GetLinkInfo write SetLinkInfo;
  end;

  TDBFilterFn = reference to function (ASnippet: IReadOnlySnippet): Boolean;

  IDBFilter = interface(IInterface)
    ['{6610639A-FC54-4FC7-8DCB-34841B3BC99E}']
    function RequiredProperties: TDBSnippetProps;
    function Match(ASnippet: IReadOnlySnippet): Boolean;
  end;

  TDelegatedDBFilter = class(TInterfacedObject, IDBFilter)
  strict private
    var
      fFilterFn: TDBFilterFn;
      fRequiredProps: TDBSnippetProps;
  public
    constructor Create(const FilterFn: TDBFilterFn;
      const RequiredProps: TDBSnippetProps = []);
    function RequiredProperties: TDBSnippetProps;
    function Match(ASnippet: IReadOnlySnippet): Boolean;
  end;

implementation

uses
  Character,

  CS.Utils.Hashes,
  UStrUtils;

{ TDBSnippetID }

class function TDBSnippetID.Compare(const Left, Right: TDBSnippetID): Integer;
begin
  Result := StrCompareStr(Left.fID, Right.fID);
end;

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
  Result := StrSameStr(Left.fID, Right.fID);
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

{ TDBSnippetID.TComparer }

function TDBSnippetID.TComparer.Compare(const Left,
  Right: TDBSnippetID): Integer;
begin
  Result := TDBSnippetid.Compare(Left, Right);
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

{ TDBCompileResults }

constructor TDBCompileResults.Create(const ASucceeds, AFails: TCompilerIDs);
begin
  fSucceeds := ASucceeds;
  fFails := AFails;
end;

class function TDBCompileResults.CreateNull: TDBCompileResults;
begin
  Result := TDBCompileResults.Create([], []);
end;

function TDBCompileResults.FailsWith(const Compilers: TCompilerIDs;
  const Rule: TTestRule): Boolean;
begin
  Result := False;  // keeps compiler happy
  case Rule of
    trAnd:
      Result := Compilers <= fFails;
    trOr:
      Result := (Compilers * fFails) <> [];
    else
      Assert(False, 'Unknown TTestRule value');
  end;
end;

function TDBCompileResults.GetUnknown: TCompilerIDs;
var
  ID: TCompilerID;
  Known: TCompilerIDs;
begin
  Known := fSucceeds + fFails;
  Result := [];
  for ID := Low(TCompilerID) to High(TCompilerID) do
    if not (ID in Known) then
      Include(Result, ID);
end;

function TDBCompileResults.IsNull: Boolean;
begin
  Result := (fSucceeds = []) and (fFails = []);
end;

function TDBCompileResults.SucceedsWith(const Compilers: TCompilerIDs;
  const Rule: TTestRule): Boolean;
begin
  Result := False;  // keeps compiler happy
  case Rule of
    trAnd:
      Result := Compilers <= fSucceeds;
    trOr:
      Result := (Compilers * fSucceeds) <> [];
    else
      Assert(False, 'Unknown TTestRule value');
  end;
end;

{ TDBTag }

class function TDBTag.Compare(const Left, Right: TDBTag): Integer;
begin
  Result := StrCompareText(Left.fTag, Right.fTag);
end;

constructor TDBTag.Create(const ATagStr: string);
resourcestring
  sBadTagStr = '"%s" is not a valid tag string';
begin
  fTag := PrepareTagString(ATagStr);
  if not IsValidPreparedTagString(fTag) then
    raise EDBTag.Create(sBadTagStr);
end;

class operator TDBTag.Equal(const Left, Right: TDBTag): Boolean;
begin
  Result := StrSameText(Left.fTag, Right.fTag);
end;

function TDBTag.Hash: Integer;
begin
  Result := Integer(PaulLarsonHash(fTag));
end;

class function TDBTag.IsValidPreparedTagString(const AStr: string): Boolean;
begin
  if AStr = EmptyStr then
    Exit(False);
  if Length(AStr) > 64 then
    Exit(False);
  // TODO: more validation of tag string
  Result := True;
end;

class function TDBTag.IsValidTagString(const AStr: string): Boolean;
begin
  Result := IsValidPreparedTagString(PrepareTagString(AStr));
end;

class operator TDBTag.NotEqual(const Left, Right: TDBTag): Boolean;
begin
  Result := not StrSameText(Left.fTag, Right.fTag);
end;

class function TDBTag.PrepareTagString(const AStr: string): string;
begin
  Result := StrReplace(
    StrCompressWhiteSpace(StrTrim(AStr)), ' ', '_'
  );
end;

function TDBTag.ToDisplayString: string;
begin
  Result := StrReplace(fTag, '_', ' ');
end;

function TDBTag.ToString: string;
begin
  Result := fTag;
end;

{ TDBTag.TComparer }

function TDBTag.TComparer.Compare(const Left, Right: TDBTag): Integer;
begin
  Result := TDBTag.Compare(Left, Right);
end;

{ TDBTag.TEqualityComparer }

function TDBTag.TEqualityComparer.Equals(const Left, Right: TDBTag): Boolean;
begin
  Result := Left = Right;
end;

function TDBTag.TEqualityComparer.GetHashCode(const Value: TDBTag): Integer;
begin
  Result := Value.Hash;
end;

{ TDelegatedDBFilter }

constructor TDelegatedDBFilter.Create(const FilterFn: TDBFilterFn;
  const RequiredProps: TDBSnippetProps);
begin
  Assert(Assigned(FilterFn), ClassName + '.Create: FilterFn is nil');
  inherited Create;
  fFilterFn := FilterFn;
  fRequiredProps := RequiredProps;
end;

function TDelegatedDBFilter.Match(ASnippet: IReadOnlySnippet): Boolean;
begin
  Result := fFilterFn(ASnippet);
end;

function TDelegatedDBFilter.RequiredProperties: TDBSnippetProps;
begin
  Result := fRequiredProps;
end;

end.

