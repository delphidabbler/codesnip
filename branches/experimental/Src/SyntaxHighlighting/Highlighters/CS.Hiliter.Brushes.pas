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
 * Classes and types used to encapsulate a syntax highlighter brush.
}


unit CS.Hiliter.Brushes;

interface

uses
  Generics.Collections,
  SynEditHighlighter;

type
  TSyntaxHiliterAttr = record
  strict private
    var
      fID: string;
      fFriendlyName: string;
  public
    constructor Create(const AID, AFriendlyName: string);
    property ID: string read fID;
    property FriendlyName: string read fFriendlyName;
  end;

type
  TSyntaxHiliterBrush = class abstract(TObject)
  strict protected
    function GetID: string; virtual; abstract;
    function GetFriendlyName: string; virtual; abstract;
  public
    function CreateHighlighter: TSynCustomHighlighter; virtual; abstract;
    function SupportedAttrs: TArray<TSyntaxHiliterAttr>; virtual; abstract;
    property ID: string read GetID;
    property FriendlyName: string read GetFriendlyName;
  end;

type
  TSyntaxHiliterBrushes = record
  strict private
    class var
      fSupportedHiliters: TList<TSynCustomHighlighterClass>;
    class function FindHiliterClass(const ID: string):
      TSynCustomHighlighterClass; static;
  public
    class constructor Create;
    class destructor Destroy;
    class function BrushExists(const ID: string): Boolean; static;
    class function CreateBrush(const ID: string): TSyntaxHiliterBrush; static;
    class function CreateNullBrush: TSyntaxHiliterBrush; static;
    class function SupportedBrushIDs: TArray<string>; static;
  end;


implementation

uses
  SynHighlighterHtml,
  SynHighlighterJScript,
  SynHighlighterPas,
  SynHighlighterPHP,

  UStrUtils;

type
  TSynEditBrush = class sealed(TSyntaxHiliterBrush)
  strict private
    var
      fHighlighterClass: TSynCustomHighlighterClass;
  strict protected
    function GetID: string; override;
    function GetFriendlyName: string; override;
  public
    constructor Create(const HighlighterClass: TSynCustomHighlighterClass);
    function CreateHighlighter: TSynCustomHighlighter; override;
    function SupportedAttrs: TArray<TSyntaxHiliterAttr>; override;
  end;

type
  TNullBrush = class sealed(TSyntaxHiliterBrush)
  strict protected
    function GetID: string; override;
    function GetFriendlyName: string; override;
  public
    function CreateHighlighter: TSynCustomHighlighter; override;
    function SupportedAttrs: TArray<TSyntaxHiliterAttr>; override;
  end;

{ TSyntaxHiliterBrushes }

class function TSyntaxHiliterBrushes.BrushExists(
  const ID: string): Boolean;
begin
  Result := Assigned(FindHiliterClass(ID));
end;

class constructor TSyntaxHiliterBrushes.Create;
begin
  fSupportedHiliters := TList<TSynCustomHighlighterClass>.Create;
  with fSupportedHiliters do
  begin
    Add(TSynHTMLSyn);
    Add(TSynJScriptSyn);
    Add(TSynPasSyn);
    Add(TSynPHPSyn);
  end;
end;

class function TSyntaxHiliterBrushes.CreateBrush(const ID: string):
  TSyntaxHiliterBrush;
var
  Cls: TSynCustomHighlighterClass;
begin
  Cls := FindHiliterClass(ID);
  if Assigned(Cls) then
    Result := TSynEditBrush.Create(Cls)
  else
    Result := TNullBrush.Create;
end;

class function TSyntaxHiliterBrushes.CreateNullBrush: TSyntaxHiliterBrush;
begin
  Result := TNullBrush.Create;
end;

class destructor TSyntaxHiliterBrushes.Destroy;
begin
  fSupportedHiliters.Free;
end;

class function TSyntaxHiliterBrushes.FindHiliterClass(const ID: string):
  TSynCustomHighlighterClass;
var
  Cls: TSynCustomHighlighterClass;
begin
  for Cls in fSupportedHiliters do
  begin
    if StrSameText(Cls.GetLanguageName, ID) then
      Exit(Cls);
  end;
  Result := nil;
end;

class function TSyntaxHiliterBrushes.SupportedBrushIDs: TArray<string>;
var
  I: Integer;
begin
  SetLength(Result, fSupportedHiliters.Count);
  for I := 0 to Pred(fSupportedHiliters.Count) do
    Result[I] := fSupportedHiliters[I].GetLanguageName;
end;

{ TSynEditBrush }

constructor TSynEditBrush.Create(
  const HighlighterClass: TSynCustomHighlighterClass);
begin
  inherited Create;
  fHighlighterClass := HighlighterClass;
end;

function TSynEditBrush.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := fHighlighterClass.Create(nil);
end;

function TSynEditBrush.GetFriendlyName: string;
begin
  Result := fHighlighterClass.GetFriendlyLanguageName;
end;

function TSynEditBrush.GetID: string;
begin
  Result := fHighlighterClass.GetLanguageName;
end;

function TSynEditBrush.SupportedAttrs: TArray<TSyntaxHiliterAttr>;
var
  Hiliter: TSynCustomHighlighter;
  I: Integer;
begin
  Hiliter := CreateHighlighter;
  try
    SetLength(Result, Hiliter.AttrCount);
    for I := 0 to Pred(Hiliter.AttrCount) do
      Result[I] := TSyntaxHiliterAttr.Create(
        Hiliter.Attribute[I].Name,
        Hiliter.Attribute[I].FriendlyName
      );
  finally
    Hiliter.Free;
  end;
end;

{ TNullBrush }

function TNullBrush.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := nil;
end;

function TNullBrush.GetFriendlyName: string;
resourcestring
  sFriendlyName = 'None';
begin
  Result := sFriendlyName;
end;

function TNullBrush.GetID: string;
begin
  Result := 'Null';
end;

function TNullBrush.SupportedAttrs: TArray<TSyntaxHiliterAttr>;
begin
  SetLength(Result, 0);
end;

{ TSyntaxHiliterAttr }

constructor TSyntaxHiliterAttr.Create(const AID, AFriendlyName: string);
begin
  fID := AID;
  fFriendlyName := AFriendlyName;
end;

end.
