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
  ///  <summary>Record that encapsulates information about a syntax highlighter
  ///  brush attribute.</summary>
  TSyntaxHiliterAttr = record
  strict private
    var
      ///  <summary>Value of ID property.</summary>
      fID: string;
      ///  <summary>Value of FriendlyName property.</summary>
      fFriendlyName: string;
  public
    ///  <summary>Constructs a new record with the given ID and friendly name.
    ///  </summary>
    constructor Create(const AID, AFriendlyName: string);
    ///  <summary>The attiribute's unique ID.</summary>
    property ID: string read fID;
    ///  <summary>The attribute's friendly name suitable for displaying to
    ///  users.</summary>
    property FriendlyName: string read fFriendlyName;
  end;

type
  ///  <summary>Encapsulates a syntax highlighter "brush" that supports syntax
  ///  highlighting source code in a certain language.</summary>
  TSyntaxHiliterBrush = class abstract(TObject)
  strict protected
    ///  <summary>Read accessor for the ID property.</summary>
    function GetID: string; virtual; abstract;
    ///  <summary>Read accessor for the FriendlyName property.</summary>
    function GetFriendlyName: string; virtual; abstract;
  public
    ///  <summary>Creates a SynEdit highlighter compatible highlighter object
    ///  suitable for use with the SynEdit control.</summary>
    function CreateHighlighter: TSynCustomHighlighter; virtual; abstract;
    ///  <summary>Returns an array of highlighter attributes supported by the
    ///  brush.</summary>
    function SupportedAttrs: TArray<TSyntaxHiliterAttr>; virtual; abstract;
    ///  <summary>Brush's unique ID string.</summary>
    property ID: string read GetID;
    ///  <summary>Friendly name of brush, suitable for displaying to users.
    ///  </summary>
    property FriendlyName: string read GetFriendlyName;
  end;

type
  ///  <summary>Container for methods that manipulate and provide information
  ///  about supported syntax highlighter brushes.</summary>
  TSyntaxHiliterBrushes = record
  strict private
    class var
      ///  <summary>List of supported SynEdit based highlighter brushes.
      ///  </summary>
      fSupportedHiliters: TList<TSynCustomHighlighterClass>;
    ///  <summary>Finds and returns the SynEdit highlighter class that
    ///  implements the brush with the given ID.</summary>
    class function FindHiliterClass(const ID: string):
      TSynCustomHighlighterClass; static;
  public
    ///  <summary>Creates and initialises list of supported syntax highlighter
    ///  brushes.</summary>
    class constructor Create;
    ///  <summary>Destroys class level objects.</summary>
    class destructor Destroy;
    ///  <summary>Checks if the highlighter brush with the given ID exists.
    ///  </summary>
    class function BrushExists(const ID: string): Boolean; static;
    ///  <summary>Creates and returns an instance of the brush object with the
    ///  given ID.</summary>
    ///  <remarks>It is the caller's responsibility to free the return object.
    ///  </remarks>
    class function CreateBrush(const ID: string): TSyntaxHiliterBrush; static;
    ///  <summary>Creates and returns a null highlighter brush instance.
    ///  </summary>
    ///  <remarks>It is the caller's responsibility to free the return object.
    ///  </remarks>
    class function CreateNullBrush: TSyntaxHiliterBrush; static;
    ///  <summary>Returns an array of IDs of supported highlighter brushes.
    ///  </summary>
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
  ///  <summary>Encapsulates a syntax highlighter brush that uses a wrapped
  ///  SynEdit highlighter component to perform the highlighting.</summary>
  TSynEditBrush = class sealed(TSyntaxHiliterBrush)
  strict private
    var
      ///  <summary>Class of the SynEdit highlighter that this class
      ///  encapsulates.</summary>
      fHighlighterClass: TSynCustomHighlighterClass;
  strict protected
    ///  <summary>Read accessor for ID property.</summary>
    ///  <remarks>Gets the value from the wrapper SynEdit highlighter.</remarks>
    function GetID: string; override;
    ///  <summary>Read accessor for FriendlyName property.</summary>
    ///  <remarks>Gets the value from the wrapper SynEdit highlighter.</remarks>
    function GetFriendlyName: string; override;
  public
    ///  <summary>Constructs a new object instance that wraps the given SynEdit
    ///  highlighter component.</summary>
    constructor Create(const HighlighterClass: TSynCustomHighlighterClass);
    ///  <summary>Creates a SynEdit highlighter compatible highlighter object
    ///  suitable for use with the SynEdit control.</summary>
    ///  <remarks>This highlighter has no styling associated with it. It is up
    ///  to the caller to apply the required styling.</remarks>
    function CreateHighlighter: TSynCustomHighlighter; override;
    ///  <summary>Returns an array of highlighter attributes supported by the
    ///  brush.</summary>
    function SupportedAttrs: TArray<TSyntaxHiliterAttr>; override;
  end;

type
  ///  <summary>Encapsulates a null syntax highlighter object that has no effect
  ///  on content passed to it.</summary>
  TNullBrush = class sealed(TSyntaxHiliterBrush)
  strict protected
    ///  <summary>Read accessor for ID property.</summary>
    ///  <remarks>Always returns 'Null', which may not be used by any other
    ///  brush.</remarks>
    function GetID: string; override;
    ///  <summary>Read accessor for FriendlyName property.</summary>
    ///  <remarks>Always returns 'None'.</remarks>
    function GetFriendlyName: string; override;
  public
    ///  <summary>Creates a SynEdit highlighter compatible highlighter object
    ///  suitable for use with the SynEdit control.</summary>
    ///  <remarks>Actually this class simply returns nil, which is valid for
    ///  assigning to a SynEdit control to force it to skip highlighting of its
    ///  content.</remarks>
    function CreateHighlighter: TSynCustomHighlighter; override;
    ///  <summary>Returns an array of highlighter attributes supported by the
    ///  brush.</summary>
    ///  <remarks>Returns an empty array: a null brush supports no attributes.
    ///  </remarks>
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

