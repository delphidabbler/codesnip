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
 * Classes and types used to encapsulate the various source code languages
 * supported by the program.
}


unit CS.SourceCode.Languages;

interface

uses
  Generics.Defaults,
  Generics.Collections;

type
  TSourceCodeLanguageID = record
  strict private
    const
      DefaultLanguageID = '_Default_';
    var
      fID: string;
  public
    type
      TEqualityComparer = class(TEqualityComparer<TSourceCodeLanguageID>)
      public
        function Equals(const Left, Right: TSourceCodeLanguageID): Boolean;
          override;
        function GetHashCode(const Value: TSourceCodeLanguageID): Integer;
          override;
      end;
  public
    constructor Create(const AID: string);
    class operator Equal(const Left, Right: TSourceCodeLanguageID): Boolean;
      inline;
    class operator NotEqual(const Left, Right: TSourceCodeLanguageID): Boolean;
      inline;
    class function Compare(const Left, Right: TSourceCodeLanguageID): Integer;
      static; inline;
    class function CreateDefault: TSourceCodeLanguageID; static; inline;
    class function IsValidIDString(const S: string): Boolean; static;
    function CompareTo(const Other: TSourceCodeLanguageID): Integer; inline;
    function ToString: string; inline;
    function Hash: Integer; inline;
    function IsDefault: Boolean;
  end;

  TSourceCodeLanguage = record
  strict private
    const
      DefaultTabSize = 4;
  strict private
    var
      fID: TSourceCodeLanguageID;
      fFriendlyName: string;
      fEditorTabSize: Byte;
      fHiliterBrushID: string;
      fBuiltIn: Boolean;
  public
    constructor Create(const AID: TSourceCodeLanguageID;
      const AFriendlyName: string; const AIsBuiltIn: Boolean);
    // NOTE: DO NOT MAKE CreateDefault inline: causes internal compiler error!
    class function CreateDefault: TSourceCodeLanguage; static;
    ///  <summary>Updates the record's properties to those of Lang except that
    ///  the BuiltIn property remains unchanged.</summary>
    procedure Update(const Lang: TSourceCodeLanguage);
    property ID: TSourceCodeLanguageID read fID;
    property FriendlyName: string read fFriendlyName write fFriendlyName;
    property EditorTabSize: Byte read fEditorTabSize write fEditorTabSize;
    property HiliterBrushID: string read fHiliterBrushID write fHiliterBrushID;
    property BuiltIn: Boolean read fBuiltIn;
  end;

type
  TSourceCodeLanguages = class(TObject)
  strict private
    var
      fLanguages: TDictionary<TSourceCodeLanguageID,TSourceCodeLanguage>;
    function GetLanguage(const LangID: TSourceCodeLanguageID):
      TSourceCodeLanguage;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Language: TSourceCodeLanguage);
    procedure Update(const Language: TSourceCodeLanguage);
    procedure Clear;
    procedure Delete(const LangID: TSourceCodeLanguageID);
    function HasLanguage(const LangID: TSourceCodeLanguageID): Boolean;
    function SupportedLanguageIDs: TArray<TSourceCodeLanguageID>;
    function GetEnumerator: TEnumerator<TSourceCodeLanguage>;
    property Languages[const ID: TSourceCodeLanguageID]: TSourceCodeLanguage
      read GetLanguage; default;
  end;

implementation

uses
  SysUtils,
  Character,
  CS.SourceCode.Hiliter.Brushes,
  CS.Utils.Hashes,
  UExceptions,
  UStrUtils;

{ TSourceCodeLanguageID }

class function TSourceCodeLanguageID.Compare(const Left,
  Right: TSourceCodeLanguageID): Integer;
begin
  Result := StrCompareText(Left.fID, Right.fID);
end;

function TSourceCodeLanguageID.CompareTo(
  const Other: TSourceCodeLanguageID): Integer;
begin
  Result := Compare(Self, Other);
end;

constructor TSourceCodeLanguageID.Create(const AID: string);
begin
  if AID <> EmptyStr then
  begin
    if not IsValidIDString(AID) then
      raise EBug.CreateFmt(
        'TSourceCodeLanguageID.Create: Invalid ID string "%s"', [AID]
      );
    fID := AID;
  end
  else
    fID := DefaultLanguageID;
end;

class function TSourceCodeLanguageID.CreateDefault: TSourceCodeLanguageID;
begin
  // Passing '' to Create method skips validation and sets id to
  // DefaultLanguageID. Passing DefaultLanguageID to Create will cause
  // DefaultLanguageID to be validated and to fail, which is behaviour we want
  // when Create is called externally.
  Result := TSourceCodeLanguageID.Create('');
end;

class operator TSourceCodeLanguageID.Equal(const Left,
  Right: TSourceCodeLanguageID): Boolean;
begin
  Result := Compare(Left, Right) = 0;
end;

function TSourceCodeLanguageID.Hash: Integer;
begin
  Result := TextHash(fID);
end;

function TSourceCodeLanguageID.IsDefault: Boolean;
begin
  Result := StrSameText(fID, DefaultLanguageID);
end;

class function TSourceCodeLanguageID.IsValidIDString(const S: string): Boolean;
var
  Idx: Integer;
begin
  if S = EmptyStr then
    Exit(False);
  if not TCharacter.IsLetterOrDigit(S[1]) then
    Exit(False);
  for Idx := 2 to Length(S) do
    if not TCharacter.IsLetterOrDigit(S[Idx])
      and not TCharacter.IsPunctuation(S[Idx]) then
      Exit(False);
  Result := True;
end;

class operator TSourceCodeLanguageID.NotEqual(const Left,
  Right: TSourceCodeLanguageID): Boolean;
begin
  Result := Compare(Left, Right) <> 0;
end;

function TSourceCodeLanguageID.ToString: string;
begin
  Result := fID;
end;

{ TSourceCodeLanguageID.TEqualityComparer }

function TSourceCodeLanguageID.TEqualityComparer.Equals(const Left,
  Right: TSourceCodeLanguageID): Boolean;
begin
  Result := Left = Right;
end;

function TSourceCodeLanguageID.TEqualityComparer.GetHashCode(
  const Value: TSourceCodeLanguageID): Integer;
begin
  Result := Value.Hash;
end;

{ TSourceCodeLanguage }

constructor TSourceCodeLanguage.Create(const AID: TSourceCodeLanguageID;
  const AFriendlyName: string; const AIsBuiltIn: Boolean);
begin
  fID := AID;
  fFriendlyName := AFriendlyName;
  fBuiltIn := AIsBuiltIn;
  fEditorTabSize := DefaultTabSize;
  fHiliterBrushID := TSyntaxHiliterBrushes.NullBrushID;
end;

class function TSourceCodeLanguage.CreateDefault: TSourceCodeLanguage;
resourcestring
  sDefaultFriendlyName = 'Unknown';
begin
  Result := TSourceCodeLanguage.Create(
    TSourceCodeLanguageID.CreateDefault,
    sDefaultFriendlyName,
    True
  );
end;

procedure TSourceCodeLanguage.Update(
  const Lang: TSourceCodeLanguage);
var
  OrigBuiltIn: Boolean;
begin
  OrigBuiltIn := fBuiltIn;
  Self := Lang;
  fBuiltIn := OrigBuiltIn;
end;

{ TSourceCodeLanguages }

procedure TSourceCodeLanguages.Add(const Language: TSourceCodeLanguage);
begin
  fLanguages.Add(Language.ID, Language);
end;

procedure TSourceCodeLanguages.Clear;
begin
  fLanguages.Clear;
end;

constructor TSourceCodeLanguages.Create;
begin
  inherited Create;
  fLanguages := TDictionary<TSourceCodeLanguageID,TSourceCodeLanguage>.Create(
    TSourceCodeLanguageID.TEqualityComparer.Create
  );
end;

procedure TSourceCodeLanguages.Delete(const LangID: TSourceCodeLanguageID);
begin
  fLanguages.Remove(LangID);
end;

destructor TSourceCodeLanguages.Destroy;
begin
  fLanguages.Free;
  inherited;
end;

function TSourceCodeLanguages.GetEnumerator: TEnumerator<TSourceCodeLanguage>;
begin
  Result := fLanguages.Values.GetEnumerator;
end;

function TSourceCodeLanguages.GetLanguage(const LangID: TSourceCodeLanguageID):
  TSourceCodeLanguage;
begin
  if HasLanguage(LangID) then
    Result := fLanguages[LangID]
  else if HasLanguage(TSourceCodeLanguage.CreateDefault.ID) then
    Result := TSourceCodeLanguage.CreateDefault;
end;

function TSourceCodeLanguages.HasLanguage(const LangID: TSourceCodeLanguageID):
  Boolean;
begin
  Result := fLanguages.ContainsKey(LangID);
end;

function TSourceCodeLanguages.SupportedLanguageIDs:
  TArray<TSourceCodeLanguageID>;
begin
  Result := fLanguages.Keys.ToArray;
end;

procedure TSourceCodeLanguages.Update(const Language: TSourceCodeLanguage);
var
  UpdateValue: TSourceCodeLanguage;
begin
  if fLanguages.ContainsKey(Language.ID) then
  begin
    // make copy of Language but preserve BuiltIn flag
    UpdateValue := fLanguages[Language.ID];
    UpdateValue.Update(Language);
    fLanguages[Language.ID] := UpdateValue;
  end
  else
    fLanguages.Add(Language.ID, Language);
end;

end.
