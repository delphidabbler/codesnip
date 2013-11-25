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
  // Delphi
  SysUtils,
  Generics.Defaults,
  // 3rd party
  Collections.Base,
  // Project
  CS.ActiveText,
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

  ETag = class(Exception);

  TTag = record
  public
    type
      TComparer = class(TComparer<TTag>)
        function Compare(const Left, Right: TTag): Integer; override;
      end;
      TEqualityComparer = class(TEqualityComparer<TTag>)
      public
        function Equals(const Left, Right: TTag): Boolean; override;
        function GetHashCode(const Value: TTag): Integer; override;
      end;
  strict private
    var
      fTag: string;
    class function PrepareTagString(const AStr: string): string; static;
    class function IsValidPreparedTagString(const AStr: string): Boolean;
      static;
  public
    constructor Create(const ATagStr: string);
    class operator Equal(const Left, Right: TTag): Boolean; inline;
    class operator NotEqual(const Left, Right: TTag): Boolean; inline;
    class function IsValidTagString(const AStr: string): Boolean; static;
      inline;
    class function Compare(const Left, Right: TTag): Integer; static; inline;
    function ToString: string; inline;
    function Hash: Integer; inline;
  end;

  TTagFilter = reference to function(const ATag: TTag): Boolean;

  ITagSet = interface(IInterface)
    ['{CE0E8AB9-0EA0-431D-87B0-E60264E398EE}']
    function GetEnumerator: IEnumerator<TTag>;
    function Contains(const ATag: TTag): Boolean;
    function GetCount: Integer;
    function IsEmpty: Boolean;
    function Filter(const AFilterFn: TTagFilter): ITagSet;
    procedure Assign(Other: ITagSet);
    procedure Add(const ATag: TTag);
    procedure Remove(const ATag: TTag);
    procedure Clear;
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

  ///  <summary>Enumeration providing information about the level to which a
  ///  snippet has been tested.</summary>
  TSnippetTestInfo = (
    stiNone,              // snippet has not been tested
    stiBasic,             // snippet has had some basic testing
    stiAdvanced           // snippet has had advanced (unit) testing
  );

  TDBSnippetProp = (
    spID, spTitle, spDescription, spSourceCode, spLanguageID, spModified,
    spCreated, spRequiredModules, spRequiredSnippets, spXRefs, spNotes, spKind,
    spCompileResults, spTags, spLinkInfo, spTestInfo, spStarred
  );

  TDBSnippetProps = set of TDBSnippetProp;

  ISnippetBase = interface(IInterface)
    ['{0F915A15-441B-4180-A9C2-41C52AF63C8A}']
    function GetID: TDBSnippetID;
    function GetCreated: TUTCDateTime;
    function GetModified: TUTCDateTime;
    function GetTitle: string;
    function GetDescription: IActiveText;
    function GetSourceCode: string;
    function GetLanguageID: TSourceCodeLanguageID;
    function GetRequiredModules: IStringList;
    function GetRequiredSnippets: IDBSnippetIDList;
    function GetXRefs: IDBSnippetIDList;
    function GetNotes: IActiveText;
    function GetKind: TDBSnippetKind;
    function GetCompileResults: TCompileResults;
    function GetTags: ITagSet;
    function GetLinkInfo: ISnippetLinkInfo;
    function GetTestInfo: TSnippetTestInfo;
    function GetStarred: Boolean;

    property ID: TDBSnippetID read GetID;
    property Created: TUTCDateTime read GetCreated;
    property Modified: TUTCDateTime read GetModified;
  end;

  IReadOnlySnippet = interface(ISnippetBase)
    ['{BD221CF4-482D-4FF9-BDAE-D320DDEBD578}']
    function GetValidProperties: TDBSnippetProps;

    property Title: string read GetTitle;
    property Description: IActiveText read GetDescription;
    property SourceCode: string read GetSourceCode;
    property LanguageID: TSourceCodeLanguageID read GetLanguageID;
    property RequiredModules: IStringList read GetRequiredModules;
    property RequiredSnippets: IDBSnippetIDList read GetRequiredSnippets;
    property XRefs: IDBSnippetIDList read GetXRefs;
    property Notes: IActiveText read GetNotes;
    property Kind: TDBSnippetKind read GetKind;
    property CompileResults: TCompileResults read GetCompileResults;
    property Tags: ITagSet read GetTags;
    property LinkInfo: ISnippetLinkInfo read GetLinkInfo;
    property TestInfo: TSnippetTestInfo read GetTestInfo;
    property Starred: Boolean read GetStarred;

    // TODO: query if following properties are required
    property ValidProperties: TDBSnippetProps read GetValidProperties;
    function SupportsProperty(const AProp: TDBSnippetProp): Boolean;
  end;

  ISnippet = interface(ISnippetBase)
    ['{75F44D0E-7611-4A8D-A4F5-F7612E466238}']
    procedure SetTitle(const ATitle: string);
    procedure SetDescription(ADescription: IActiveText);
    procedure SetSourceCode(const ASourceCode: string);
    procedure SetLanguageID(const ALanguageID: TSourceCodeLanguageID);
    procedure SetRequiredModules(AModuleList: IStringList);
    procedure SetRequiredSnippets(AIDList: IDBSnippetIDList);
    procedure SetXRefs(AIDList: IDBSnippetIDList);
    procedure SetNotes(ANotes: IActiveText);
    procedure SetKind(const ASnippetKind: TDBSnippetKind);
    procedure SetCompileResults(const AResults: TCompileResults);
    procedure SetTags(ATagList: ITagSet);
    procedure SetLinkInfo(ALinkInfo: ISnippetLinkInfo);
    procedure SetTestInfo(ATestInfo: TSnippetTestInfo);
    procedure SetStarred(AStarred: Boolean);

    property Title: string read GetTitle write SetTitle;
    property Description: IActiveText read GetDescription write SetDescription;
    property SourceCode: string read GetSourceCode write SetSourceCode;
    property LanguageID: TSourceCodeLanguageID read GetLanguageID
      write SetLanguageID;
    property RequiredModules: IStringList read GetRequiredModules
      write SetRequiredModules;
    property RequiredSnippets: IDBSnippetIDList read GetRequiredSnippets
      write SetRequiredSnippets;
    property XRefs: IDBSnippetIDList read GetXRefs write SetXRefs;
    property Notes: IActiveText read GetNotes write SetNotes;
    property Kind: TDBSnippetKind read GetKind write SetKind;
    property CompileResults: TCompileResults read GetCompileResults
      write SetCompileResults;
    property Tags: ITagSet read GetTags write SetTags;
    property LinkInfo: ISnippetLinkInfo read GetLinkInfo write SetLinkInfo;
    property TestInfo: TSnippetTestInfo read GetTestInfo write SetTestInfo;
    property Starred: Boolean read GetStarred write SetStarred;
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
  UConsts,
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
  Result := StrHash(fID);
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

{ TTag }

class function TTag.Compare(const Left, Right: TTag): Integer;
begin
  Result := StrCompareText(Left.fTag, Right.fTag);
end;

constructor TTag.Create(const ATagStr: string);
resourcestring
  sBadTagStr = '"%s" is not a valid tag string';
begin
  fTag := PrepareTagString(ATagStr);
  if not IsValidPreparedTagString(fTag) then
    raise ETag.Create(sBadTagStr);
end;

class operator TTag.Equal(const Left, Right: TTag): Boolean;
begin
  Result := StrSameText(Left.fTag, Right.fTag);
end;

function TTag.Hash: Integer;
begin
  Result := TextHash(fTag);
end;

class function TTag.IsValidPreparedTagString(const AStr: string): Boolean;
var
  Ch: Char;
begin
  if AStr = EmptyStr then
    Exit(False);
  if Length(AStr) > 64 then
    Exit(False);
  for Ch in AStr do
    if not TCharacter.IsLetter(Ch)
      and not TCharacter.IsNumber(Ch)
      and (Ch <> ' ')
      and not TCharacter.IsPunctuation(Ch)
      and not TCharacter.IsSymbol(Ch) then
      Exit(False);
  Result := True;
end;

class function TTag.IsValidTagString(const AStr: string): Boolean;
begin
  Result := IsValidPreparedTagString(PrepareTagString(AStr));
end;

class operator TTag.NotEqual(const Left, Right: TTag): Boolean;
begin
  Result := not StrSameText(Left.fTag, Right.fTag);
end;

class function TTag.PrepareTagString(const AStr: string): string;
begin
  Result := StrCompressWhiteSpace(StrTrim(AStr));
end;

function TTag.ToString: string;
begin
  Result := fTag;
end;

{ TTag.TComparer }

function TTag.TComparer.Compare(const Left, Right: TTag): Integer;
begin
  Result := TTag.Compare(Left, Right);
end;

{ TTag.TEqualityComparer }

function TTag.TEqualityComparer.Equals(const Left, Right: TTag): Boolean;
begin
  Result := Left = Right;
end;

function TTag.TEqualityComparer.GetHashCode(const Value: TTag): Integer;
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

