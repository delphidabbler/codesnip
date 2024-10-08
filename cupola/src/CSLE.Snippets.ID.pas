{
  This Source Code Form is subject to the terms of the Mozilla Public License,
  v. 2.0. If a copy of the MPL was not distributed with this file, You can
  obtain one at https://mozilla.org/MPL/2.0/

  Copyright (C) 2024, Peter Johnson (gravatar.com/delphidabbler).

  Data type encapsulating snippet IDs.
}

unit CSLE.Snippets.ID;

interface

uses
  System.SysUtils,
  System.Types,
  System.Generics.Defaults;

type
  ///  <summary>Record that uniquely identifies a code snippet.</summary>
  ///  <remarks>New IDs should always be created by calling the CreateNew
  ///  method. The Create constuctor is provided to re-create the ID when read
  ///  in from a file. IDs should be unique and can be from 0 to 
  ///  <c>MaxIDSize</c> bytes. IDs of size 0 are considered null and should only
  ///  be used as placeholders: it is an error to include a null ID in the
  ///  database.</remarks>
  TSnippetID = record
  public
    type
      ///  <summary>Comparator for snippet IDs.</summary>
      TComparator = class(TInterfacedObject,
        IComparer<TSnippetID>, IEqualityComparer<TSnippetID>)
      public
        ///  <summary>Compares the two given snippet IDs.</summary>
        ///  <remarks>Returns zero if Left is the same as Right, -ve if Left is
        ///  less than Right or +ve if Left is greater than Right.</remarks>
        function Compare(const Left, Right: TSnippetID): Integer; inline;
        ///  <summary>Checks if the two given snippet IDs are equal.</summary>
        function Equals(const Left, Right: TSnippetID): Boolean;
          reintroduce; overload; inline;
        ///  <summary>Returns the hash code of the given snippet ID.</summary>
        function GetHashCode(const Value: TSnippetID): Integer;
          reintroduce; overload;
      end;
  strict private
    var
      ///  <summary>Internal value of ID.</summary>
      ///  <remarks>Must not be exposed as a property in case internal
      ///  representation changes.</remarks>
      fID: TBytes;
    const
      ///  <summary>Endianness used when converting GUID to bytes.</summary>
      Endianness = TEndian.Big;
    ///  <summary>Returns a new byte array that is copy of <c>ABytes</c>.
    ///  </summary>
    class function CopyBytes(const ABytes: TBytes): TBytes; static;
  public
    const
      ///  <summary>Maximum permitted size for an ID, in bytes.</summary>
      MaxIDSize = 32;

    ///  <summary>Constructs a new ID with value created from given byte array.
    ///  </summary>
    constructor Create(const ABytes: TBytes);

    ///  <summary>Creates and returns a snippet ID with a globally unique value.
    ///  </summary>
    class function CreateNew: TSnippetID; static;

    ///  <summary>Returns a string representation of the ID.</summary>
    function ToString: string;

    ///  <summary>Returns the ID as an array of bytes, in big endian format.
    ///  </summary>
    function ToByteArray: TBytes;

    ///  <summary>Checks if the snippet ID is null.</summary>
    function IsNull: Boolean;

    function Hash: Integer;

    ///  <summary>Compares the two given snippet IDs.</summary>
    ///  <remarks>Returns zero if Left is the same as Right, -ve if Left is less
    ///  than Right or +ve if Left is greater than Right.</remarks>
    class function Compare(const Left, Right: TSnippetID): Integer; static;

    ///  <summary>Initialises new record instance to null ID.</summary>
    class operator Initialize(out Dest: TSnippetID);

    ///  <summary>Assigns a copy of the value of record <c>Src</c> to
    ///  <c>Dest</c>.</summary>
    class operator Assign (var Dest: TSnippetID; const [ref] Src: TSnippetID);

    ///  <summary>Checks if the two given snippet IDs are equal.</summary>
    class operator Equal(const Left, Right: TSnippetID): Boolean; inline;

    ///  <summary>Checks if the two given snippet IDs are not equal.</summary>
    class operator NotEqual(const Left, Right: TSnippetID): Boolean; inline;

  end;

implementation

uses
  System.Character,
  System.Hash,
  System.Math,
  System.Generics.Collections,
  CSLE.Exceptions;

{ TSnippetID }

class operator TSnippetID.Assign(var Dest: TSnippetID;
  const [ref] Src: TSnippetID);
begin
  // DO NOT call TSnippetID constructor: code crashes
  Dest.fID := CopyBytes(Src.fID);
end;

class function TSnippetID.Compare(const Left, Right: TSnippetID): Integer;
begin
  for var I := Low(Left.fID) to Min(High(Left.fID), High(Right.fID)) do
    if Left.fID[I] > Right.fID[I] then
      Exit(GreaterThanValue)
    else if Left.fID[I] < Right.fID[I] then
      Exit(LessThanValue);
  if Length(Left.fID) = Length(Right.fID) then
    Result := EqualsValue
  else if Length(Left.fID) < Length(Right.fID) then
    Result := LessThanValue
  else
    Result := GreaterThanValue;
end;

class function TSnippetID.CopyBytes(const ABytes: TBytes): TBytes;
begin
  if Length(ABytes) > 0 then
    Result := Copy(ABytes, 0, Length(ABytes))
  else
    SetLength(Result, 0);
end;

constructor TSnippetID.Create(const ABytes: TBytes);
resourcestring
  sInvalidSize = 'Attempting to set a snippet ID > %d bytes';
begin
  if Length(ABytes) > MaxIDSize then
    raise EUnexpected.CreateFmt(sInvalidSize, [MaxIDSize]);
  fID := CopyBytes(ABytes);
end;

class function TSnippetID.CreateNew: TSnippetID;
begin
  Result := TSnippetID.Create(TGUID.NewGuid.ToByteArray(Endianness));
end;

class operator TSnippetID.Equal(const Left, Right: TSnippetID): Boolean;
begin
  Result := Compare(Left, Right) = EqualsValue;
end;

function TSnippetID.Hash: Integer;
begin
  var Data := ToByteArray;
  Result := THashBobJenkins.GetHashValue(Data[0], Length(Data));
end;

class operator TSnippetID.Initialize(out Dest: TSnippetID);
begin
  SetLength(Dest.fID, 0);
end;

function TSnippetID.IsNull: Boolean;
begin
  Result := Length(fID) = 0;
end;

class operator TSnippetID.NotEqual(const Left, Right: TSnippetID): Boolean;
begin
  Result := Compare(Left, Right) <> EqualsValue;
end;

function TSnippetID.ToByteArray: TBytes;
begin
  Result := CopyBytes(fID);
end;

function TSnippetID.ToString: string;
begin
  Result := string.Empty;
  for var B in fID do
    Result := Result + B.ToHexString(2 * SizeOf(B));
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

end.
