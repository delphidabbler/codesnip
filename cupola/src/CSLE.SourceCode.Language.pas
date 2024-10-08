{
  This Source Code Form is subject to the terms of the Mozilla Public License,
  v. 2.0. If a copy of the MPL was not distributed with this file, You can
  obtain one at https://mozilla.org/MPL/2.0/

  Copyright (C) 2024, Peter Johnson (gravatar.com/delphidabbler).

  Data type that encapsulates a source code language ID.
}

unit CSLE.SourceCode.Language;

interface

uses
  System.SysUtils,
  System.Generics.Defaults;

type
  TSourceCodeLanguageID = record
  public
    type
      ///  <summary>Comparator for source code language IDs.</summary>
      ///  <remarks>Source code language IDs are not case sensitive.</remarks>
      TComparator = class(TInterfacedObject,
        IComparer<TSourceCodeLanguageID>,
        IEqualityComparer<TSourceCodeLanguageID>
      )
      public
        ///  <summary>Compares the two given source code language IDs.</summary>
        ///  <remarks>Returns zero if Left is the same as Right, -ve if Left is
        ///  less than Right or +ve if Left is greater than Right.</remarks>
        function Compare(const Left, Right: TSourceCodeLanguageID): Integer;
        ///  <summary>Checks if the two given source code language IDs are
        ///  equal.</summary>
        function Equals(const Left, Right: TSourceCodeLanguageID): Boolean;
          reintroduce; overload;
        ///  <summary>Returns the hash code of the given source code language
        ///  ID.</summary>
        function GetHashCode(const Value: TSourceCodeLanguageID): Integer;
          reintroduce; overload;
      end;
  strict private
      // Default language ID. This name is reserved: it must not be used as the
      // ID for any source code language. To prevent this being done by accident
      // the ID is not valid: IsValidIDString will return False for this ID and
      // Create will raise an exception if this ID is passed to it.
      // To create a default ID call the CreateDefault method.
    const
      DefaultLanguageID = '_Default_';
    var
      fID: string;
    class function Compare(const Left, Right: TSourceCodeLanguageID): Integer; static;
  public
    const
      ///  <summary>Maximum length of an ID.</summary>
      MaxLength = 32;
      ///  <summary>Required name for Pascal language ID.</summary>
      ///  <remarks>This name is special since Pascal code is treated specially
      ///  by the program and such special treatment requires the use of this
      ///  ID.</summary>
      PascalLanguageID = 'Pascal';

    ///  <summary>Creates a new record with ID set to <c>AID</c>.</summary>
    ///  <exception>Raises exception if <c>AId</c> is not a valid ID and is not
    ///  the empty string.</exception>
    ///  <remarks>
    ///  <para>If <c>AID</c> is empty then the default language ID is created.
    ///  </para>
    ///  <para>A non-empty <c>AID</c> must be between 1 and 32 characters,
    ///  must start with a letter or digit and subsequent characters must be
    ///  either letters, digits, symbols or punctuation characters.</para>
    ///  </remarks>
    constructor Create(const AID: string);

    ///  <summary>Creates a default source code language ID.</summary>
    class function CreateDefault: TSourceCodeLanguageID; static;

    ///  <summary>Checks if <c>AStr</c> is valid source code language ID string.
    ///  </summary>
    ///  <remarks>A valid ID string is between 1 and 32 characters long, must
    ///  start with a letter or digit and subsequent characters must be either
    ///  letters, digits, symbols or punctuation characters.</remarks>
    class function IsValidIDString(const AStr: string): Boolean; static;

    ///  <summary>Returns string representation of ID.</summary>
    function ToString: string; inline;

    ///  <summary>Checks if the current ID is the default ID.</summary>
    function IsDefault: Boolean; inline;

    ///  <summary>Checks if the current ID is that of the Pascal language.
    ///  </summary>
    ///  <remarks>Detects if the ID is that specified by the
    ///  <c>PascalLanguageID</c> constant.</remarks>
    function IsPascal: Boolean; inline;

    // Default ctor: creates a default source code language ID.
    class operator Initialize(out Dest: TSourceCodeLanguageID);

    // Comparison operators
    class operator Equal(const Left, Right: TSourceCodeLanguageID): Boolean;
    class operator NotEqual(const Left, Right: TSourceCodeLanguageID): Boolean;
  end;

implementation

uses
  System.Character,
  System.Hash,
  System.Types,
  CSLE.Exceptions;

{ TSourceCodeLanguageID }

class function TSourceCodeLanguageID.Compare(const Left,
  Right: TSourceCodeLanguageID): Integer;
begin
  Result := string.CompareText(Left.fID, Right.fID);
end;

constructor TSourceCodeLanguageID.Create(const AID: string);
begin
  if not AID.IsEmpty then
  begin
    if not IsValidIDString(AID) then
      raise EUnexpected.CreateFmt(
        'TSourceCodeLanguageID.Create: Invalid ID string "%s"', [AID]
      );
    fID := AID;
  end
  else
    fID := DefaultLanguageID;
end;

class function TSourceCodeLanguageID.CreateDefault: TSourceCodeLanguageID;
begin
  Result := TSourceCodeLanguageID.Create(string.Empty);
end;

class operator TSourceCodeLanguageID.Equal(const Left,
  Right: TSourceCodeLanguageID): Boolean;
begin
  Result := Compare(Left, Right) = EqualsValue;
end;

class operator TSourceCodeLanguageID.Initialize(out Dest: TSourceCodeLanguageID);
begin
  Dest.fID := DefaultLanguageID;
end;

function TSourceCodeLanguageID.IsDefault: Boolean;
begin
  Result := fID = DefaultLanguageID;
end;

function TSourceCodeLanguageID.IsPascal: Boolean;
begin
  // Use Equal operator to ensure test allows for case diiferences between this
  // record's ID and PascalLangauageID.
  Result := TSourceCodeLanguageID.Create(PascalLanguageID) = Self;
end;

class function TSourceCodeLanguageID.IsValidIDString(const AStr: string):
  Boolean;
begin
  // Per docs:
  //   [ID] must start with a Unicode letter or digit and be followed by
  //   a sequence of zero or more Unicode letters, digits and punctuation
  //   characters.
  Result := False;
  if AStr.IsEmpty then
    Exit;
  if AStr.Length > MaxLength then
    Exit;
  if not AStr[1].IsLetterOrDigit then
    Exit;
  for var Idx := 2 to AStr.Length do
    if not AStr[Idx].IsLetterOrDigit and not AStr[Idx].IsPunctuation
      and not AStr[Idx].IsSymbol then
      Exit;
  Result := True;
end;

class operator TSourceCodeLanguageID.NotEqual(const Left,
  Right: TSourceCodeLanguageID): Boolean;
begin
  Result := Compare(Left, Right) <> EqualsValue;
end;

function TSourceCodeLanguageID.ToString: string;
begin
  Result := fID;
end;

{ TSourceCodeLanguageID.TComparator }

function TSourceCodeLanguageID.TComparator.Compare(const Left,
  Right: TSourceCodeLanguageID): Integer;
begin
  Result := TSourceCodeLanguageID.Compare(Left, Right);
end;

function TSourceCodeLanguageID.TComparator.Equals(const Left,
  Right: TSourceCodeLanguageID): Boolean;
begin
  Result := Left = Right;
end;

function TSourceCodeLanguageID.TComparator.GetHashCode(
  const Value: TSourceCodeLanguageID): Integer;
begin
  Result := THashBobJenkins.GetHashValue(Value.fID);
end;

end.
