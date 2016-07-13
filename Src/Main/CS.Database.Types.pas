{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2016, Peter Johnson (www.delphidabbler.com).
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
  // 3rd party
  Collections.Base,
  // Project
  CS.ActiveText,
  CS.SourceCode.Languages,
  CS.Utils.Dates,
  Compilers.UGlobals,
  UComparers,
  UIStringList;

type
  ///  <summary>Type of exception raised when errors are detected in TSnippetID.
  ///  </summary>
  ESnippetID = class(Exception);

  ///  <summary>Record that uniquely identifies a code snippet.</summary>
  TSnippetID = record
  public
    type
      ///  <summary>Comparator for snippet IDs.</summary>
      TComparator = class(TComparator<TSnippetID>)
        ///  <summary>Compares the two given snippet IDs.</summary>
        ///  <remarks>Returns zero if Left is the same as Right, -ve if Left is
        ///  less than Right or +ve if Left is greater than Right.</remarks>
        function Compare(const Left, Right: TSnippetID): Integer; override;
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
    ///  <summary>Gets new list enumerator.</summary>
    function GetEnumerator: IEnumerator<TSnippetID>;
    ///  <summary>Clears the list.</summary>
    procedure Clear;
    ///  <summary>Adds given snippet ID to list and returns its index in list.
    ///  </summary>
    ///  <remarks>If SnippetID is already in list then nothing happens.
    ///  </remarks>
    procedure Add(const SnippetID: TSnippetID);
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

  ///  <summary>Enumeration of IDs of supported kinds of snippets.</summary>
  TSnippetKindID = (
    skFreeform,   // free-form code - not in any of other supported formats
    skRoutine,    // procedure or function in standard format
    skConstant,   // constant definition in standard format
    skTypeDef,    // type definition in standard format
    skUnit,       // complete source code unit
    skClass       // Delphi class or record with methods
  );

  ///  <summary>Set of supported snippet kind IDs.</summary>
  TSnippetKindIDs = set of TSnippetKindID;

type
  ///  <summary>Encapsulates information about a snippet kind.</summary>
  TSnippetKind = record
  strict private
    var
      ///  <summary>Value of ID property.</summary>
      fID: TSnippetKindID;
      ///  <summary>Value of DisplayName property.</summary>
      fDisplayName: string;
      ///  <summary>Set of IDs of the snippet kinds a snippet with this kind
      ///  can depend upon.</summary>
      fValidDependIDs: TSnippetKindIDs;
  public
    ///  <summary>Initialises record with required property values.</summary>
    constructor Create(AID: TSnippetKindID; const ADisplayName: string;
      const AValidDependIDs: TSnippetKindIDs);
    ///  <summary>ID of snippet kind.</summary>
    property ID: TSnippetKindID read fID;
    ///  <summary>Display name (description) of snippet kind.</summary>
    property DisplayName: string read fDisplayName;
    ///  <summary>Set of IDs of the snippet kinds a snippet with this kind can
    ///  depend upon.</summary>
    property ValidDependIDs: TSnippetKindIDs read fValidDependIDs;
  end;

  ISnippetKindList = interface(IInterface)
    ['{7C6C9E75-F17F-4BF2-AA63-DE8C8070A348}']
    function GetEnumerator: IEnumerator<TSnippetKind>;
    function GetItem(const KindID: TSnippetKindID): TSnippetKind;
    function GetIDs: TSnippetKindIDs;
    function First: TSnippetKind;
    function Last: TSnippetKind;
    property Items[const KindID: TSnippetKindID]: TSnippetKind read GetItem;
      default;
    property IDs: TSnippetKindIDs read GetIDs;
  end;

  ETag = class(Exception);

  TTag = record
  public
    type
      TComparator = class(TComparator<TTag>)
        ///  <summary>Compares tags Left and Right, returning -ve if Left less
        ///  than Right, 0 if equal or +ve if Left greater than Right.</summary>
        ///  <remarks>Method of IComparator and IComparer.</remarks>
        function Compare(const Left, Right: TTag): Integer; override;
        ///  <summary>Checks if two tags, Left and Right, are equal.</summary>
        ///  <remarks>Method of IComparator and IEqualityComparer.</remarks>
        function Equals(const Left, Right: TTag): Boolean; override;
        ///  <summary>Gets hash of tag.</summary>
        ///  <remarks>Method of IComparator and IEqualityComparer.</remarks>
        function GetHashCode(const Value: TTag): Integer; override;
      end;
  strict private
    var
      fTag: string;
    const
      ///  <summary>Maximum size, in characters, of string representation of
      ///  tag.</summary>
      MaxTagStringLength = 64;
    class function IsValidTagChar(const Ch: Char): Boolean; static; inline;
  public
    constructor Create(const ATagStr: string);
    class function CreateNull: TTag; static;
    class operator Equal(const Left, Right: TTag): Boolean; inline;
    class operator NotEqual(const Left, Right: TTag): Boolean; inline;
    class function IsValidTagString(const AStr: string): Boolean; static;
    class function MakeValidTagString(const AStr: string): string; static;
    class function Compare(const Left, Right: TTag): Integer; static; inline;
    function IsNull: Boolean;
    function ToString: string; inline;
    function Hash: Integer; inline;
  end;

  TTagFilter = reference to function(const ATag: TTag): Boolean;

  ITagSet = interface(IInterface)
    ['{CE0E8AB9-0EA0-431D-87B0-E60264E398EE}']
    function GetEnumerator: IEnumerator<TTag>;
    function Contains(const ATag: TTag): Boolean;
    function ContainsSubSet(ASubSet: ITagSet): Boolean;
    function GetCount: Integer;
    function IsEmpty: Boolean;
    function Filter(const AFilterFn: TTagFilter): ITagSet;
    procedure Assign(Other: ITagSet);
    procedure Add(const ATag: TTag);
    procedure Include(Tags: ITagSet);
    procedure Remove(const ATag: TTag);
    procedure Exclude(Tags: ITagSet);
    procedure Clear;
    property Count: Integer read GetCount;
  end;

  TSnippetOriginSource = (
    sosLocal, // snippet created locally
    sosLegacy,
    sosImport,
    sosSWAG,
    sosCSOnline
  );

  ISnippetOrigin = interface(IInterface)
    ['{2155B208-D00F-4C43-A897-823F9AE91739}']
    function GetSource: TSnippetOriginSource;
//    function GetFriendlyName: string;
    function GetOriginalID: string;
    function GetModified: TUTCDateTime;
    function IsLinked: Boolean;
    property Source: TSnippetOriginSource read GetSource;
//    property FriendlyName: string read GetFriendlyName;
    property OriginalID: string read GetOriginalID;
    property Modified: TUTCDateTime read GetModified;
  end;

  ///  <summary>Enumeration providing information about the level to which a
  ///  snippet has been tested.</summary>
  TSnippetTestInfo = (
    stiNone,              // snippet has not been tested
    stiBasic,             // snippet has had some basic testing
    stiAdvanced           // snippet has had advanced (unit) testing
  );

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
    function GetKindID: TSnippetKindID;
    function GetCompileResults: TCompileResults;
    function GetTags: ITagSet;
    function GetOrigin: ISnippetOrigin;
    function GetTestInfo: TSnippetTestInfo;
    function GetStarred: Boolean;

    function CanCompile: Boolean;

    property ID: TSnippetID read GetID;
    property Created: TUTCDateTime read GetCreated;
    property Modified: TUTCDateTime read GetModified;
  end;

  ISnippet = interface(ISnippetBase)
    ['{BD221CF4-482D-4FF9-BDAE-D320DDEBD578}']
    property Title: string read GetTitle;
    property Description: IActiveText read GetDescription;
    property SourceCode: string read GetSourceCode;
    property LanguageID: TSourceCodeLanguageID read GetLanguageID;
    property RequiredModules: IStringList read GetRequiredModules;
    property RequiredSnippets: ISnippetIDList read GetRequiredSnippets;
    property XRefs: ISnippetIDList read GetXRefs;
    property Notes: IActiveText read GetNotes;
    property KindID: TSnippetKindID read GetKindID;
    property CompileResults: TCompileResults read GetCompileResults;
    property Tags: ITagSet read GetTags;
    property Origin: ISnippetOrigin read GetOrigin;
    property TestInfo: TSnippetTestInfo read GetTestInfo;
    property Starred: Boolean read GetStarred;
  end;

  IEditableSnippet = interface(ISnippetBase)
    ['{75F44D0E-7611-4A8D-A4F5-F7612E466238}']
    procedure SetTitle(const ATitle: string);
    procedure SetDescription(ADescription: IActiveText);
    procedure SetSourceCode(const ASourceCode: string);
    procedure SetLanguageID(const ALanguageID: TSourceCodeLanguageID);
    procedure SetRequiredModules(AModuleList: IStringList);
    procedure SetRequiredSnippets(AIDList: ISnippetIDList);
    procedure SetXRefs(AIDList: ISnippetIDList);
    procedure SetNotes(ANotes: IActiveText);
    procedure SetKindID(const ASnippetKind: TSnippetKindID);
    procedure SetCompileResults(const AResults: TCompileResults);
    procedure SetTags(ATagList: ITagSet);
    procedure SetOrigin(AOrigin: ISnippetOrigin);
    procedure SetTestInfo(ATestInfo: TSnippetTestInfo);
    procedure SetStarred(AStarred: Boolean);

    procedure UpdateFrom(AOther: IEditableSnippet); overload;
    procedure UpdateFrom(AOther: ISnippet); overload;
    function CloneAsReadOnly: ISnippet;

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
    property KindID: TSnippetKindID read GetKindID write SetKindID;
    property CompileResults: TCompileResults read GetCompileResults
      write SetCompileResults;
    property Tags: ITagSet read GetTags write SetTags;
    property Origin: ISnippetOrigin read GetOrigin write SetOrigin;
    property TestInfo: TSnippetTestInfo read GetTestInfo write SetTestInfo;
    property Starred: Boolean read GetStarred write SetStarred;
  end;

implementation

uses
  // Delphi
  Character,
  // Project
  CS.Utils.Hashes,
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

{ TSnippetID.TComparator }

function TSnippetID.TComparator.Compare(const Left, Right: TSnippetID): Integer;
begin
  Result := TSnippetID.Compare(Left, Right);
end;

function TSnippetID.TComparator.Equals(const Left, Right: TSnippetID): Boolean;
begin
  Result := Left = Right;
end;

function TSnippetID.TComparator.GetHashCode(const Value: TSnippetID): Integer;
begin
  Result := Value.Hash;
end;

{ TSnippetKind }

constructor TSnippetKind.Create(AID: TSnippetKindID; const ADisplayName: string;
  const AValidDependIDs: TSnippetKindIDs);
begin
  fID := AID;
  fDisplayName := ADisplayName;
  fValidDependIDs := AValidDependIDs;
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
  if not IsValidTagString(ATagStr) then
    raise ETag.CreateFmt(sBadTagStr, [ATagStr]);
  fTag := ATagStr;
end;

class function TTag.CreateNull: TTag;
begin
  Result.fTag := EmptyStr;
end;

class operator TTag.Equal(const Left, Right: TTag): Boolean;
begin
  Result := StrSameText(Left.fTag, Right.fTag);
end;

function TTag.Hash: Integer;
begin
  Result := TextHash(fTag);
end;

function TTag.IsNull: Boolean;
begin
  Result := fTag = EmptyStr;
end;

class function TTag.IsValidTagChar(const Ch: Char): Boolean;
begin
  Result := TCharacter.IsLetter(Ch)
    or TCharacter.IsNumber(Ch)
    or CharInSet(Ch, ['-', '_', ' ', ':', '(', ')']);
end;

class function TTag.IsValidTagString(const AStr: string): Boolean;
var
  Ch: Char;
begin
  if AStr = EmptyStr then
    Exit(False);
  if Length(AStr) > MaxTagStringLength then
    Exit(False);
  for Ch in AStr do
    if not IsValidTagChar(Ch) then
      Exit(False);
  Result := True;
end;

class function TTag.MakeValidTagString(const AStr: string): string;
var
  I: Integer;
begin
  SetLength(Result, Length(AStr));
  for I := 1 to Length(AStr) do
  begin
    if IsValidTagChar(AStr[I]) then
      Result[I] := AStr[I]
    else
      Result[I] := '_';
  end;
end;

class operator TTag.NotEqual(const Left, Right: TTag): Boolean;
begin
  Result := not StrSameText(Left.fTag, Right.fTag);
end;

function TTag.ToString: string;
begin
  Result := fTag;
end;

{ TTag.TComparator }

function TTag.TComparator.Compare(const Left, Right: TTag): Integer;
begin
  Result := TTag.Compare(Left, Right);
end;

function TTag.TComparator.Equals(const Left, Right: TTag): Boolean;
begin
  Result := Left = Right;
end;

function TTag.TComparator.GetHashCode(const Value: TTag): Integer;
begin
  Result := Value.Hash;
end;

end.

