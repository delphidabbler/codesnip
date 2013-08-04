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
  Generics.Collections,
  CS.SourceCode.Hiliter.Brushes;

type
  TSourceCodeLanguage = record
  strict private
    const
      DefaultTabSize = 2;
      DefaultBrushID = TSyntaxHiliterBrushes.NullBrushID;
  strict private
    var
      fID: string;
      fFriendlyName: string;
      fEditorTabSize: Byte;
      fHiliterBrushID: string;
      fBuiltIn: Boolean;
  public
    constructor Create(const AID, AFriendlyName: string;
      const AIsBuiltIn: Boolean);
    ///  <summary>Updates the record's properties to those of Lang except that
    ///  the BuiltIn property remains unchanged.</summary>
    procedure Update(const Lang: TSourceCodeLanguage);
    property ID: string read fID;
    property FriendlyName: string read fFriendlyName write fFriendlyName;
    property EditorTabSize: Byte read fEditorTabSize write fEditorTabSize;
    property HiliterBrushID: string read fHiliterBrushID write fHiliterBrushID;
    property BuiltIn: Boolean read fBuiltIn;
  end;

type
  TSourceCodeLanguages = class(TObject)
  strict private
    var
      fLanguages: TDictionary<string,TSourceCodeLanguage>;
    function GetLanguage(const LangID: string): TSourceCodeLanguage;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Language: TSourceCodeLanguage);
    procedure Update(const Language: TSourceCodeLanguage);
    procedure Clear;
    procedure Delete(const LangID: string);
    function HasLanguage(const LangID: string): Boolean;
    function SupportedLanguageIDs: TArray<string>;
    function GetEnumerator: TEnumerator<TSourceCodeLanguage>;
    property Languages[const ID: string]: TSourceCodeLanguage
      read GetLanguage; default;
  end;


implementation

uses
  UComparers;

{ TSourceCodeLanguage }

constructor TSourceCodeLanguage.Create(const AID, AFriendlyName: string;
  const AIsBuiltIn: Boolean);
begin
  fID := AID;
  fFriendlyName := AFriendlyName;
  fBuiltIn := AIsBuiltIn;
  fEditorTabSize := DefaultTabSize;
  fHiliterBrushID := DefaultBrushID;
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
  fLanguages := TDictionary<string,TSourceCodeLanguage>.Create(
    TTextEqualityComparer.Create
  );
end;

procedure TSourceCodeLanguages.Delete(const LangID: string);
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

function TSourceCodeLanguages.GetLanguage(const LangID: string):
  TSourceCodeLanguage;
begin
  Result := fLanguages[LangID];
end;

function TSourceCodeLanguages.HasLanguage(const LangID: string): Boolean;
begin
  Result := fLanguages.ContainsKey(LangID);
end;

function TSourceCodeLanguages.SupportedLanguageIDs: TArray<string>;
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
