{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines a set of classes that manage the flexible structure of the detail
 * pane pages used to display snippets.
}


unit USnippetPageStructure;

interface

uses
  // Delphi
  Generics.Collections,
  // Project
  CS.Database.Types,
  IntfCommon,
  UBaseObjects,
  UContainers,
  USettings,
  USingleton;

type

  ///  <summary>Component parts of a snippet page.</summary>
  TSnippetPagePartId = (
    sppDescription,
    sppSourceCode,
    sppKind,
    sppTags,
    sppUnits,
    sppDepends,
    sppXRefs,
    sppCompileResults,
    sppNotes,
    sppLanguage
  );

type
  TSnippetPagePart = record
  strict private
    var
      fId: TSnippetPagePartId;
      fKey: string;
      fDisplayName: string;
  public
    constructor Create(Id: TSnippetPagePartId; const Key, DisplayName: string);
    property Id: TSnippetPagePartId read fId;
    property Key: string read fKey;
    property DisplayName: string read fDisplayName;
  end;

type
  TSnippetPageStructure = class(TObject)
  strict private
    var
      fParts: TList<TSnippetPagePart>;
      fSnippetKindID: TSnippetKindID;
    function GetParts: TArray<TSnippetPagePart>;
  public
    constructor Create(ASnippetKindID: TSnippetKindID);
    destructor Destroy; override;
    procedure Clear;
    function AppendPart(PartId: TSnippetPagePartId): Integer;
    procedure AppendParts(const PartIds: TArray<TSnippetPagePartId>);
    procedure InsertPart(Idx: Integer; PartId: TSnippetPagePartId);
    procedure MovePart(SrcIdx, DestIdx: Integer);
    procedure DeletePart(Idx: Integer);
    procedure Assign(const Src: TSnippetPageStructure);
    function IsEmpty: Boolean;
    property SnippetKindID: TSnippetKindID read fSnippetKindID;
    property Parts: TArray<TSnippetPagePart> read GetParts;
    function HasPart(PartId: TSnippetPagePartId): Boolean;
  end;

type
  ///  <summary>Provides information about how information pages for different
  ///  kinds of snippets are constructed.</summary>
  TSnippetPageStructures = class(TObject)
  strict private
    var
      fPages: array[TSnippetKindID] of TSnippetPageStructure;
    function GetPages: TArray<TSnippetPageStructure>;
    function GetPage(ASnippetKindID: TSnippetKindID): TSnippetPageStructure;
  public
    constructor Create;
    destructor Destroy; override;
    ///  <summary>Assigns properties of another TSnippetPageStructures instance
    ///  to this object.</summary>
    procedure Assign(const Src: TSnippetPageStructures);
    function GetEnumerator: TArrayEnumerator<TSnippetPageStructure>;
    property Pages[SnippetKindID: TSnippetKindID]: TSnippetPageStructure
      read GetPage; default;
  end;

type
  ///  <summary>Static class that can save and load an ISnippetPageStructures
  ///  object's data to and from persistent storage.</summary>
  TSnippetPageStructuresPersist = class(TNoConstructObject)
  strict private
    class function KindValueName(const Page: TSnippetPageStructure): string;
  public
    ///  <summary>Loads data from given persistent storage section into given
    ///  warnings object.</summary>
    class procedure Load(Storage: ISettingsSection;
      PageStructs: TSnippetPageStructures);
    ///  <summary>Saves data from given warnings object to givenpersistent
    ///  storage section.</summary>
    class procedure Save(Storage: ISettingsSection;
      PageStructs: TSnippetPageStructures);
  end;

type
  TDefaultPageStructures = class(TNoConstructObject)
  public
    class function GetParts(ASnippetKindID: TSnippetKindID):
      TArray<TSnippetPagePartId>;
    class procedure SetDefaults(PS: TSnippetPageStructures);
  end;

type
  TAllSnippetPageParts = class(TNoConstructObject)
  strict private
    class var
      fParts: array[TSnippetPagePartId] of TSnippetPagePart;
    class function GetPart(Idx: TSnippetPagePartId): TSnippetPagePart; static;
  public
    class constructor Create;
    class property Parts[Idx: TSnippetPagePartId]: TSnippetPagePart
      read GetPart;
    class function GetPartId(const Key: string; out Id: TSnippetPagePartId):
      Boolean;
  end;

implementation

uses
  SysUtils,
  UExceptions, UIStringList;

{ TSnippetPageStructure }

function TSnippetPageStructure.AppendPart(PartId: TSnippetPagePartId): Integer;
begin
  Result := fParts.Add(TAllSnippetPageParts.Parts[PartId]);
end;

procedure TSnippetPageStructure.AppendParts(
  const PartIds: TArray<TSnippetPagePartId>);
var
  PartId: TSnippetPagePartId;
begin
  for PartId in PartIds do
    AppendPart(PartId);
end;

procedure TSnippetPageStructure.Assign(const Src: TSnippetPageStructure);
begin
  Assert(Assigned(Src), ClassName + '.Assign: Src is nil');
  Clear;
  fParts.AddRange(Src.fParts);
  fSnippetKindID := Src.fSnippetKindID;
end;

procedure TSnippetPageStructure.Clear;
begin
  fParts.Clear;
end;

constructor TSnippetPageStructure.Create(ASnippetKindID: TSnippetKindID);
begin
  inherited Create;
  fSnippetKindID := ASnippetKindID;
  fParts := TList<TSnippetPagePart>.Create;
end;

procedure TSnippetPageStructure.DeletePart(Idx: Integer);
begin
  fParts.Delete(Idx);
end;

destructor TSnippetPageStructure.Destroy;
begin
  fParts.Free;
  inherited;
end;

function TSnippetPageStructure.GetParts: TArray<TSnippetPagePart>;
begin
  Result := fParts.ToArray;
end;

function TSnippetPageStructure.HasPart(PartId: TSnippetPagePartId): Boolean;
var
  Part: TSnippetPagePart;
begin
  for Part in fParts do
    if Part.Id = PartId then
      Exit(True);
  Result := False;
end;

procedure TSnippetPageStructure.InsertPart(Idx: Integer;
  PartId: TSnippetPagePartId);
begin
  fParts.Insert(Idx, TAllSnippetPageParts.Parts[PartId]);
end;

function TSnippetPageStructure.IsEmpty: Boolean;
begin
  Result := fParts.Count = 0;
end;

procedure TSnippetPageStructure.MovePart(SrcIdx, DestIdx: Integer);
begin
  fParts.Move(SrcIdx, DestIdx);
end;

{ TSnippetPageStructures }

procedure TSnippetPageStructures.Assign(const Src: TSnippetPageStructures);
var
  SnippetKindID: TSnippetKindID;
begin
  Assert(Assigned(Src), ClassName + '.Assign: Src is nil');
  for SnippetKindID := Low(TSnippetKindID) to High(TSnippetKindID) do
    fPages[SnippetKindID].Assign(Src.fPages[SnippetKindID]);
end;

constructor TSnippetPageStructures.Create;
var
  SnippetKindID: TSnippetKindID;
begin
  inherited Create;
  for SnippetKindID := Low(TSnippetKindID) to High(TSnippetKindID) do
    fPages[SnippetKindID] := TSnippetPageStructure.Create(SnippetKindID);
end;

destructor TSnippetPageStructures.Destroy;
var
  SnippetKindID: TSnippetKindID;
begin
  for SnippetKindID := Low(TSnippetKindID) to High(TSnippetKindID) do
    fPages[SnippetKindID].Free;
  inherited;
end;

function TSnippetPageStructures.GetEnumerator:
  TArrayEnumerator<TSnippetPageStructure>;
begin
  Result := TArrayEnumerator<TSnippetPageStructure>.Create(GetPages);
end;

function TSnippetPageStructures.GetPage(ASnippetKindID: TSnippetKindID):
  TSnippetPageStructure;
begin
  Result := fPages[ASnippetKindID];
end;

function TSnippetPageStructures.GetPages: TArray<TSnippetPageStructure>;
var
  Idx: Integer;
  Page: TSnippetPageStructure;
begin
  SetLength(Result, Length(fPages));
  Idx := 0;
  for Page in fPages do
  begin
    Result[Idx] := Page;
    Inc(Idx);
  end;
end;

{ TSnippetPageStructuresPersist }

class function TSnippetPageStructuresPersist.KindValueName(
  const Page: TSnippetPageStructure): string;
begin
  Result := Format('PageKind%d', [Ord(Page.SnippetKindID)]);
end;

class procedure TSnippetPageStructuresPersist.Load(Storage: ISettingsSection;
  PageStructs: TSnippetPageStructures);
var
  Page: TSnippetPageStructure;
  PartIds: TArray<TSnippetPagePartId>;

  function ParsePartsStr(const S: string): TArray<TSnippetPagePartId>;
  var
    PartKeys: IStringList;
    PartKey: string;
    PartId: TSnippetPagePartId;
  begin
    SetLength(Result, 0);
    PartKeys := TIStringList.Create(S, ',', False, True);

    for PartKey in PartKeys do
    begin
      if not TAllSnippetPageParts.GetPartId(PartKey, PartId) then
        Continue; // invalid part
      SetLength(Result, Length(Result) + 1);
      Result[Pred(Length(Result))] := PartId;
    end;
  end;

begin
  for Page in PageStructs do
  begin
    Page.Clear;
    PartIds := ParsePartsStr(Storage.GetString(KindValueName(Page)));
    Page.AppendParts(PartIds);
    if Page.IsEmpty then
      Page.AppendParts(TDefaultPageStructures.GetParts(Page.SnippetKindID));
  end;
end;

class procedure TSnippetPageStructuresPersist.Save(Storage: ISettingsSection;
  PageStructs: TSnippetPageStructures);
var
  Page: TSnippetPageStructure;

  function KindValues(const Page: TSnippetPageStructure): string;
  var
    Part: TSnippetPagePart;
    SL: IStringList;
  begin
    SL := TIStringList.Create;
    for Part in Page.Parts do
      SL.Add(Part.Key);
    Result := SL.GetText(',', False);
  end;

begin
  for Page in PageStructs do
    Storage.SetString(KindValueName(Page), KindValues(Page));
  Storage.Save;
end;

{ TSnippetPagePart }

constructor TSnippetPagePart.Create(Id: TSnippetPagePartId;
  const Key, DisplayName: string);
begin
  fId := Id;
  fKey := Key;
  fDisplayName := DisplayName;
end;

{ TAllSnippetPageParts }

resourcestring
  sDescription = 'Description';
  sSourceCode = 'Source Code';
  sKind = 'Type';
  sTags = 'Tags';
  sUnits = 'Required Units List';
  sDepends = 'Required Snippets List';
  sXRefs = 'Cross Reference List';
  sCompileResults = 'Compile Results Table';
  sNotes = 'Notes';
  sLanguage = 'Programming Language';

class constructor TAllSnippetPageParts.Create;
var
  Part: TSnippetPagePart;
begin
  fParts[sppDescription] := TSnippetPagePart.Create(
    sppDescription, 'Description', sDescription
  );
  fParts[sppSourceCode] := TSnippetPagePart.Create(
    sppSourceCode, 'SourceCode', sSourceCode
  );
  fParts[sppKind] := TSnippetPagePart.Create(
    sppKind, 'Kind', sKind
  );
  fParts[sppTags] := TSnippetPagePart.Create(
    sppTags, 'Tags', sTags
  );
  fParts[sppUnits] := TSnippetPagePart.Create(
    sppUnits, 'Units', sUnits
  );
  fParts[sppDepends] := TSnippetPagePart.Create(
    sppDepends, 'Depends', sDepends
  );
  fParts[sppXRefs] := TSnippetPagePart.Create(
    sppXRefs, 'XRefs', sXRefs
  );
  fParts[sppCompileResults] := TSnippetPagePart.Create(
    sppCompileResults, 'CompileResults', sCompileResults
  );
  fParts[sppNotes] := TSnippetPagePart.Create(
    sppNotes, 'Notes', sNotes
  );
  fParts[sppLanguage] := TSnippetPagePart.Create(
    sppLanguage, 'Language', sLanguage
  );
  for Part in fParts do
    Assert(Part.DisplayName <> '',
      ClassName + '.Create: Not all parts defined');
end;

class function TAllSnippetPageParts.GetPart(Idx: TSnippetPagePartId):
  TSnippetPagePart;
begin
  Result := fParts[Idx];
end;

class function TAllSnippetPageParts.GetPartId(const Key: string;
  out Id: TSnippetPagePartId): Boolean;
var
  Part: TSnippetPagePart;
begin
  for Part in fParts do
  begin
    if Part.Key = Key then
    begin
      Id := Part.Id;
      Exit(True);
    end;
  end;
  Result := False;
end;

{ TDefaultPageStructures }

class function TDefaultPageStructures.GetParts(ASnippetKindID: TSnippetKindID):
  TArray<TSnippetPagePartId>;
begin
  case ASnippetKindID of
    skFreeform:
      Result := TArray<TSnippetPagePartId>.Create(
        sppDescription, sppSourceCode, sppLanguage, sppKind, sppTags, sppUnits,
        sppDepends, sppXRefs, sppNotes
      );
    skUnit:
      Result := TArray<TSnippetPagePartId>.Create(
        sppDescription, sppSourceCode, sppLanguage, sppKind, sppTags, sppXRefs,
        sppCompileResults, sppNotes
      );
    else
      Result := TArray<TSnippetPagePartId>.Create(
        sppDescription, sppSourceCode, sppLanguage, sppKind, sppTags, sppUnits,
        sppDepends, sppXRefs, sppCompileResults, sppNotes
      );
  end;
end;

class procedure TDefaultPageStructures.SetDefaults(PS: TSnippetPageStructures);
var
  Page: TSnippetPageStructure;
begin
  for Page in PS do
  begin
    Page.Clear;
    Page.AppendParts(GetParts(Page.SnippetKindID));
  end;
end;

end.

