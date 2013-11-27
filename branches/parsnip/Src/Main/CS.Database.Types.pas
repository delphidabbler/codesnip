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
  Generics.Collections,
  // 3rd party
  Collections.Base,
  // Project
  CS.ActiveText,
  CS.SourceCode.Languages,
  CS.Utils.Dates,
  Compilers.UGlobals,
  UIStringList;

type
  ///  <summary>Type of exception raised when errors are detected in TSnippetID.
  ///  </summary>
  ESnippetID = class(Exception);

  ///  <summary>Record that uniquely identifies a code snippet.</summary>
  TSnippetID = record
  public
    type
      ///  <summary>Comparer for snippet IDs.</summary>
      TComparer = class(TComparer<TSnippetID>)
        ///  <summary>Compares the two given snippet IDs.</summary>
        ///  <remarks>Returns zero if Left is the same as Right, -ve if Left is
        ///  less than Right or +ve if Left is greater than Right.</remarks>
        function Compare(const Left, Right: TSnippetID): Integer; override;
      end;
      ///  <summary>Equality comparer for snippet IDs.</summary>
      TEqualityComparer = class(TEqualityComparer<TSnippetID>)
      public
        ///  <summary>Checks if the two given snippet IDs are equal.</summary>
        function Equals(const Left, Right: TSnippetID): Boolean; override;
        ///  <summary>Returns the hash code of the given snippet ID.</summary>
        function GetHashCode(const Value: TSnippetID): Integer; override;
      end;
  strict private
    var
      ///  <summary>Internal value of ID.</summary>
      fID: string;
    const
      ///  <summary>Maximum size, in characters, of string representation of ID
      ///  string.</summary>
      MaxIDStringLength = 64;
  public
    ///  <summary>Constructs a new ID with value created from given ID string.
    ///  </summary>
    constructor Create(const AIDStr: string);
    ///  <summary>Returns a new, null, snippet ID.</summary>
    class function CreateNull: TSnippetID; static;
    ///  <summary>Creates and returns a snippet ID with a globally unique value.
    ///  </summary>
    class function CreateNew: TSnippetID; static;
    ///  <summary>Checks if the two given snippet IDs are equal.</summary>
    class operator Equal(const Left, Right: TSnippetID): Boolean;
    ///  <summary>Checks if the two given snippet IDs are not equal.</summary>
    class operator NotEqual(const Left, Right: TSnippetID): Boolean;
    ///  <summary>Compares the two given snippet IDs.</summary>
    ///  <remarks>Returns zero if Left is the same as Right, -ve if Left is less
    ///  than Right or +ve if Left is greater than Right.</remarks>
    class function Compare(const Left, Right: TSnippetID): Integer; static;
    ///  <summary>Returns a string representation of the ID.</summary>
    function ToString: string; inline;
    ///  <summary>Returns a hash of the ID.</summary>
    function Hash: Integer;
    ///  <summary>Checks if the snippet ID is null.</summary>
    function IsNull: Boolean;
    ///  <summary>Checks if the given string is a valid snippet ID.</summary>
    class function IsValidIDString(const S: string): Boolean; static;
  end;

type
  ///  <summary>Interface supported by objects that implement a list of
  ///  TSnippetID records.</summary>
  ISnippetIDList = interface(IInterface)
    ['{238CFDCC-E84E-4D29-9BC6-10FBCECBC4FA}']
    { TODO: When implementation changes to use DelphiColl, remove
            Generics.Collections. specifier from GetEnumerator. }
    ///  <summary>Gets new list enumerator.</summary>
    function GetEnumerator: Generics.Collections.TEnumerator<TSnippetID>;
    ///  <summary>Clears the list.</summary>
    procedure Clear;
    ///  <summary>Adds given snippet ID to list and returns its index in list.
    ///  </summary>
    function Add(const SnippetID: TSnippetID): Integer;
    ///  <summary>Removed the given snippet ID from the list.</summary>
    ///  <remarks>Does nothing if SnippetID is not in the list.</remarks>
    procedure Remove(const SnippetID: TSnippetID);
    ///  <summary>Checks if list contains given snippet ID.</summary>
    function Contains(const SnippetID: TSnippetID): Boolean;
    ///  <summary>Checks if this list has the same content as another list.
    ///  </summary>
    function IsEqual(Other: ISnippetIDList): Boolean;
    ///  <summary>Checks if list is empty.</summary>
    function IsEmpty: Boolean;
    ///  <summary>Returns number of snippet ID records in list.</summary>
    function Count: Integer;
  end;

  ///  <summary>Enumeration of various supported kinds of snippets.</summary>
  TSnippetKind = (
    skFreeform,   // free-form code - not in any of other supported formats
    skRoutine,    // procedure or function in standard format
    skConstant,   // constant definition in standard format
    skTypeDef,    // type definition in standard format
    skUnit,       // complete source code unit
    skClass       // Delphi class or record with methods
  );

  ///  <summary>Set of supported snippet kinds.</summary>
  TSnippetKinds = set of TSnippetKind;

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
    function GetLinkedSnippetID: TSnippetID;
    property SynchSpaceID: TGUID read GetSynchSpaceID;
    property LinkedSnippetID: TSnippetID read GetLinkedSnippetID;
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
    function GetID: TSnippetID;
    function GetCreated: TUTCDateTime;
    function GetModified: TUTCDateTime;
    function GetTitle: string;
    function GetDescription: IActiveText;
    function GetSourceCode: string;
    function GetLanguageID: TSourceCodeLanguageID;
    function GetRequiredModules: IStringList;
    function GetRequiredSnippets: ISnippetIDList;
    function GetXRefs: ISnippetIDList;
    function GetNotes: IActiveText;
    function GetKind: TSnippetKind;
    function GetCompileResults: TCompileResults;
    function GetTags: ITagSet;
    function GetLinkInfo: ISnippetLinkInfo;
    function GetTestInfo: TSnippetTestInfo;
    function GetStarred: Boolean;

    property ID: TSnippetID read GetID;
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
    property RequiredSnippets: ISnippetIDList read GetRequiredSnippets;
    property XRefs: ISnippetIDList read GetXRefs;
    property Notes: IActiveText read GetNotes;
    property Kind: TSnippetKind read GetKind;
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
    procedure SetRequiredSnippets(AIDList: ISnippetIDList);
    procedure SetXRefs(AIDList: ISnippetIDList);
    procedure SetNotes(ANotes: IActiveText);
    procedure SetKind(const ASnippetKind: TSnippetKind);
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
    property RequiredSnippets: ISnippetIDList read GetRequiredSnippets
      write SetRequiredSnippets;
    property XRefs: ISnippetIDList read GetXRefs write SetXRefs;
    property Notes: IActiveText read GetNotes write SetNotes;
    property Kind: TSnippetKind read GetKind write SetKind;
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

{ TSnippetID }

class function TSnippetID.Compare(const Left, Right: TSnippetID): Integer;
begin
  Result := StrCompareText(Left.fID, Right.fID);
end;

constructor TSnippetID.Create(const AIDStr: string);
resourcestring
  sInvalidIDStr = '"%s" is not a valid snippet ID string';
begin
  if not IsValidIDString(AIDStr) then
    raise ESnippetID.CreateFmt(sInvalidIDStr, [AIDStr]);
  fID := AIDStr;
end;

class function TSnippetID.CreateNew: TSnippetID;
var
  Bytes: TBytes;
  B: Byte;
  IDStr: string;
begin
  Bytes := TGUID.NewGuid.ToByteArray;
  IDStr := EmptyStr;
  for B in Bytes do
    IDStr := IDStr + IntToHex(B, 2 * SizeOf(B));
  Result := TSnippetID.Create(IDStr);
end;

class function TSnippetID.CreateNull: TSnippetID;
begin
  Result.fID := EmptyStr;
end;

class operator TSnippetID.Equal(const Left, Right: TSnippetID): Boolean;
begin
  Result := StrSameText(Left.fID, Right.fID);
end;

function TSnippetID.Hash: Integer;
begin
  Result := TextHash(fID);
end;

function TSnippetID.IsNull: Boolean;
begin
  Result := fID = EmptyStr;
end;

class function TSnippetID.IsValidIDString(const S: string): Boolean;

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

class operator TSnippetID.NotEqual(const Left, Right: TSnippetID): Boolean;
begin
  Result := not StrSameText(Left.fID, Right.fID);
end;

function TSnippetID.ToString: string;
begin
  Result := fID;
end;

{ TSnippetID.TComparer }

function TSnippetID.TComparer.Compare(const Left,
  Right: TSnippetID): Integer;
begin
  Result := TSnippetID.Compare(Left, Right);
end;

{ TSnippetID.TEqualityComparer }

function TSnippetID.TEqualityComparer.Equals(const Left,
  Right: TSnippetID): Boolean;
begin
  Result := Left = Right;
end;

function TSnippetID.TEqualityComparer.GetHashCode(
  const Value: TSnippetID): Integer;
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

