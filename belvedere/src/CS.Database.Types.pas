unit CS.Database.Types;

interface

uses
  System.SysUtils,
  System.Generics.Defaults,
  System.Hash;  // Required in interface for inlining

type

  TTag = record
  public
    type
      TTagComparer = class(TComparer<TTag>)
      public
        ///  <summary>Compares tags Left and Right, returning -ve if Left less
        ///  than Right, 0 if equal or +ve if Left greater than Right.</summary>
        ///  <remarks>Method of IComparator and IComparer.</remarks>
        function Compare(const Left, Right: TTag): Integer; override;
      end;
      TTagEqualityComparer = class(TEqualityComparer<TTag>)
      public
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
      ValidPunctuationChars = ['-', '_', ' ', ':', '(', ')'];
    class function IsValidTagChar(const Ch: Char): Boolean; static; inline;
  public
    const
      ///  <summary>Maximum size, in characters, of string representation of
      ///  tag.</summary>
      MaxTagStringLength = 64;
  public
    constructor Create(const ATagStr: string);
    class function CreateNull: TTag; static;
    class operator Equal(const Left, Right: TTag): Boolean; inline;
    class operator NotEqual(const Left, Right: TTag): Boolean; inline;
    ///  <summary>Checks if string is a valid tag name.</summary>
    ///  <remarks>A valid tag string comprises characters that are from the
    ///  Unicode basic multilingual plane AND are letters or digits or are one
    ///  of the punctuation characters in <c>ValidPunctuationChars</c>.
    ///  </remarks>
    class function IsValidTagString(const AStr: string): Boolean; static;
    class function MakeValidTagString(const AStr: string): string; static;
    class function Compare(const Left, Right: TTag): Integer; static; inline;
    function IsNull: Boolean; inline;
    function ToString: string; inline;
    function Hash: Integer; inline;
  end;

  ///  <summary>Record that uniquely identifies a code snippet.</summary>
  TSnippetID = record
  public
    type
      TComparer = class(TComparer<TSnippetID>)
      public
        ///  <summary>Compares the two given snippet IDs.</summary>
        ///  <remarks>Returns zero if Left is the same as Right, -ve if Left is
        ///  less than Right or +ve if Left is greater than Right.</remarks>
        function Compare(const Left, Right: TSnippetID): Integer; override;
      end;
      TEqualityComparer = class(TEqualityComparer<TSnippetID>)
      public
        ///  <summary>Checks if the two given snippet IDs are equal.</summary>
        function Equals(const Left, Right: TSnippetID): Boolean; override;
        ///  <summary>Returns the hash code of the given snippet ID.</summary>
        function GetHashCode(const Value: TSnippetID): Integer; override;
      end;
    const
      ///  <summary>Maximum size, in characters, of string representation of ID
      ///  string.</summary>
      MaxIDStringLength = 64;
  strict private
    var
      ///  <summary>Internal value of ID.</summary>
      fID: string;
    const
      ValidPunctuationChars = ['_', '-'];
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
    ///  <remarks>A valid ID string comprises characters that are from the
    ///  Unicode basic multilingual plane AND are letters or digits or are one
    ///  of the punctuation characters in <c>ValidPunctuationChars</c>.
    ///  </remarks>
    class function IsValidIDString(const S: string): Boolean; static;
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

  TSnippetOriginSource = (
    sosLocal,     // snippet created locally
    sosLegacy,    // snippet b/f from CodeSnip <= 4
    sosImport,    // snippet imported from a third party CodeSnip >= 5
    sosSWAG,      // snippet imported from SWAG
    sosCSDB       // snippet imported from DelphiDabbler Code Snippets Database
  );

  TSnippetOrigin = record

  end;

  ///  <summary>Enumeration providing information about the level to which a
  ///  snippet has been tested.</summary>
  TSnippetTestInfo = (
    stiNone,              // snippet has not been tested
    stiBasic,             // snippet has had some basic testing
    stiAdvanced           // snippet has had advanced (unit) testing
  );

  TSnippet = record

  end;


  // Exceptions
  ///  <summary>Type of exception raised when errors are detected in TTag.
  ///  </summary>
  ETag = class(Exception);
  ///  <summary>Type of exception raised when errors are detected in TSnippetID.
  ///  </summary>
  ESnippetID = class(Exception);

implementation

uses
  // Delphi
  System.Character,
  System.Math,
  // Project (old style)
  UStrUtils;

{ TTag }

{$REGION TTag}

class function TTag.Compare(const Left, Right: TTag): Integer;
begin
  // TODO -cStrings: ?? string.CompareText(Left.fTag, Right.fTag);
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
  Result.fTag := '';
end;

class operator TTag.Equal(const Left, Right: TTag): Boolean;
begin
  // TODO -cStrings: ?? string.CompareText(Left.fTag, Right.fTag) = 0;
  Result := StrSameText(Left.fTag, Right.fTag);
end;

function TTag.Hash: Integer;
begin
  Result := THashBobJenkins.GetHashValue(fTag);
end;

function TTag.IsNull: Boolean;
begin
  Result := fTag = '';
end;

class function TTag.IsValidTagChar(const Ch: Char): Boolean;
begin
  Result := Ch.IsLetter
    or Ch.IsNumber
    or CharInSet(Ch, ValidPunctuationChars);
end;

class function TTag.IsValidTagString(const AStr: string): Boolean;
begin
  if AStr = '' then
    Exit(False);
  if Length(AStr) > MaxTagStringLength then
    Exit(False);
  for var Ch in AStr do
    if not IsValidTagChar(Ch) then
      Exit(False);
  Result := True;
end;

class function TTag.MakeValidTagString(const AStr: string): string;
begin
  SetLength(Result, Min(Length(AStr), MaxTagStringLength));
  for var I := 1 to Length(Result) do
  begin
    if IsValidTagChar(AStr[I]) then
      Result[I] := AStr[I]
    else
      Result[I] := '_';
  end;
end;

class operator TTag.NotEqual(const Left, Right: TTag): Boolean;
begin
  // TODO -cStrings: ?? string.CompareText(Left.fTag, Right.fTag) = 0;
  Result := not StrSameText(Left.fTag, Right.fTag);
end;

function TTag.ToString: string;
begin
  Result := fTag;
end;

{ TTag.TTagComparer }

function TTag.TTagComparer.Compare(const Left, Right: TTag): Integer;
begin
  Result := TTag.Compare(Left, Right);
end;

{ TTag.TTagEqualityComparer }

function TTag.TTagEqualityComparer.Equals(const Left, Right: TTag): Boolean;
begin
  Result := Left = Right;
end;

function TTag.TTagEqualityComparer.GetHashCode(const Value: TTag): Integer;
begin
  Result := Value.Hash;
end;

{$ENDREGION}

{ TSnippetID }

{$REGION TSnippetID}

class function TSnippetID.Compare(const Left, Right: TSnippetID): Integer;
begin
  // TODO -cStrings: ?? string.CompareText(Left.fTag, Right.fTag);
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
begin
  var Bytes := TGUID.NewGuid.ToByteArray;
  var IDStr := '';
  for var B in Bytes do
    IDStr := IDStr + IntToHex(B, 2 * SizeOf(B));
  Result := TSnippetID.Create(IDStr);
end;

class function TSnippetID.CreateNull: TSnippetID;
begin
  Result.fID := '';
end;

class operator TSnippetID.Equal(const Left, Right: TSnippetID): Boolean;
begin
  // TODO -cStrings: ?? string.CompareText(Left.fTag, Right.fTag) = 0;
  Result := StrSameText(Left.fID, Right.fID);
end;

function TSnippetID.Hash: Integer;
begin
  Result := THashBobJenkins.GetHashValue(fID);
end;

function TSnippetID.IsNull: Boolean;
begin
  Result := fID = '';
end;

class function TSnippetID.IsValidIDString(const S: string): Boolean;

  function IsValidChar(const Ch: Char): Boolean;
  begin
    Result := Ch.IsLetterOrDigit
      or CharInSet(Ch, ValidPunctuationChars);
  end;

begin
  // check for empty string
  if S = '' then
    Exit(False);
  // check for long string
  if Length(S) > MaxIDStringLength then
    Exit(False);
  // check for valid characters
  for var Ch in S do
    if not IsValidChar(Ch) then
      Exit(False);
  Result := True;
end;

class operator TSnippetID.NotEqual(const Left, Right: TSnippetID): Boolean;
begin
  // TODO -cStrings: ?? string.CompareText(Left.fTag, Right.fTag) = 0;
  Result := not StrSameText(Left.fID, Right.fID);
end;

function TSnippetID.ToString: string;
begin
  Result := fID;
end;

{ TSnippetID.TComparer }

function TSnippetID.TComparer.Compare(const Left, Right: TSnippetID): Integer;
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

{$ENDREGION}

{ TSnippetKind }

{$REGION TSnippetKind}

constructor TSnippetKind.Create(AID: TSnippetKindID; const ADisplayName: string;
  const AValidDependIDs: TSnippetKindIDs);
begin
  fID := AID;
  fDisplayName := ADisplayName;
  fValidDependIDs := AValidDependIDs;
end;

{$ENDREGION}

end.

