{
  This Source Code Form is subject to the terms of the Mozilla Public License,
  v. 2.0. If a copy of the MPL was not distributed with this file, You can
  obtain one at https://mozilla.org/MPL/2.0/

  Copyright (C) 2024, Peter Johnson (gravatar.com/delphidabbler).

  Data type that encapsulate snippets.
}

unit CSLE.Snippets.Snippet;

interface

uses
  System.SysUtils,

  CSLE.Snippets.Format,
  CSLE.Snippets.ID,
  CSLE.Snippets.Markup,
  CSLE.Snippets.Tag,
  CSLE.SourceCode.Language,
  CSLE.Utils.Dates;

type
  TSnippet = record
  strict private
    var
      fID: TSnippetID;
      fTitle: string;
      fDescription: TSnippetMarkup;
      fSourceCode: string;
      fLanguageID: TSourceCodeLanguageID;
      fModified: TUTCDateTime;
      fCreated: TUTCDateTime;
      fRequiredModules: TArray<string>;
      fRequiredSnippets: TArray<TSnippetID>;
      fXRefs: TArray<TSnippetID>;
      fNotes: TSnippetMarkup;
      fFormat: TSnippetFormatID;
      fTags: ITagSet;
      fStarred: Boolean;
    procedure SetModified(const AValue: TUTCDateTime);
    procedure SetRequiredModules(const AValue: TArray<string>);
    procedure SetRequiredSnippets(const AValue: TArray<TSnippetID>);
    procedure SetXRefs(const AValue: TArray<TSnippetID>);
    procedure SetTags(const AValue: ITagSet);
  public

    ///  <summary>Create a new snippet record with the given ID, which must not
    ///  be null.</summary>
    ///  <remarks>All properties except <c>ID</c> are given their default
    ///  values.</remarks>
    constructor Create(const AID: TSnippetID);

    ///  <summary>Creates a new snippet record with a unique ID.</summary>
    ///  <remarks>All properties except <c>ID</c> are given there default
    ///  values.</remarks>
    class function CreateUnique: TSnippet; static;

    ///  <summary>Snippet ID. Must be unique within the database.</summary>
    property ID: TSnippetID
      read fID;

    ///  <summary>Snippet title in plain text.</summary>
    property Title: string
      read fTitle write fTitle;

    ///  <summary>Snippet description in markup.</summary>
    property Description: TSnippetMarkup
      read fDescription write fDescription;

    ///  <summary>Snippet source code in plain text.</summary>
    property SourceCode: string
      read fSourceCode write fSourceCode;

    ///  <summary>ID of snippet source code language.</summary>
    property LanguageID: TSourceCodeLanguageID
      read fLanguageID write fLanguageID;

    { TODO: Change Modified to return Created when Modified is Null ? }
    ///  <summary>Date snippet last modified.</summary>
    ///  <remarks>A null <c>TUTCDateTime</c> must not be assigned to this
    ///  property.</remarks>
    property Modified: TUTCDateTime
      read fModified write SetModified;

    ///  <summary>Date snippet was created.</summary>
    property Created: TUTCDateTime
      read fCreated;

    ///  <summary>Modules required to compile this snippet.</summary>
    property RequiredModules: TArray<string>
      read fRequiredModules write SetRequiredModules;

    ///  <summary>IDs of any other snippets required to compile this snippet.
    ///  </summary>
    property RequiredSnippets: TArray<TSnippetID>
      read fRequiredSnippets write SetRequiredSnippets;

    ///  <summary>IDs of any other snippets cross referenced by this snippet.
    ///  </summary>
    property XRefs: TArray<TSnippetID>
      read fXRefs write SetXRefs;

    ///  <summary>Additional notes about this snippet.</summary>
    property Notes: TSnippetMarkup
      read fNotes write fNotes;

    ///  <summary>ID of snippet format.</summary>
    property Format: TSnippetFormatID
      read fFormat write fFormat;

    ///  <summary>List of tags associated with this snippet.</summary>
    property Tags: ITagSet
      read fTags write SetTags;

    ///  <summary>Flag indicating if the user has starred this snippet.
    ///  </summary>
    property Starred: Boolean
      read fStarred write fStarred;

    // TODO: property CompileResults: TCompileResults
    // TODO: property TestInfo: TSnippetTestInfo
    // TODO: property Origin: TSnippetOrigin
    // TODO: property Sync: TSnippetSync

    ///  <summary>Hash of this snippet.</summary>
    function Hash: Integer;

    // Operator overloads

    // Snippet initialisation: snippets properties are all given their default
    // values.
    class operator Initialize(out Dest: TSnippet);

    {TODO: write test}
    // Snippet assignment: all <c>Dest</c> properties are deep copies of
    // <c>Src</c>.
    class operator Assign(var Dest: TSnippet; const [ref] Src: TSnippet);

  end;

implementation

{ TSnippet }

class operator TSnippet.Assign(var Dest: TSnippet; const [ref] Src: TSnippet);
begin
  Dest.fID := Src.fID;
  Dest.fTitle := Src.fTitle;
  Dest.fDescription := Src.fDescription;
  Dest.fSourceCode := Src.fSourceCode;
  Dest.fLanguageID := Src.fLanguageID;
  Dest.fCreated := Src.fCreated;
  Dest.fModified := Src.fModified;
  Dest.fRequiredModules := Copy(Src.fRequiredModules);
  Dest.fRequiredSnippets := Copy(Src.fRequiredSnippets);
  Dest.fXRefs := Copy(Src.fXRefs);
  Dest.fNotes := Src.fNotes;
  Dest.fFormat := Src.fFormat;
  Dest.fTags := TTagSet.Create(Src.fTags);
  Dest.fStarred := Src.fStarred;
end;

constructor TSnippet.Create(const AID: TSnippetID);
begin
  Assert(not AID.IsNull, 'TSnippet.Create: AID is null');
  fID := AID;
  // ** No need to initialise other fields since the default ctr (Initialize
  //    operator overload) has done this automatically.
end;

class function TSnippet.CreateUnique: TSnippet;
begin
  Result := TSnippet.Create(TSnippetID.CreateNew);
end;

function TSnippet.Hash: Integer;
begin
  // Hash is simply the hash of the snippet ID
  Result := fID.Hash;
end;

class operator TSnippet.Initialize(out Dest: TSnippet);
begin
  // ** Do not call TSnippet.Create

  var NullID: TSnippetID;   // ID initialised to Null
  var NullMarkup: TSnippetMarkup; // Markup initialised to Null (empty)

  Dest.fID := NullID;
  Dest.fTitle := string.Empty;
  Dest.fDescription := NullMarkup;
  Dest.fSourceCode := string.Empty;
  Dest.fLanguageID := TSourceCodeLanguageID.CreateDefault;
  Dest.fCreated := TUTCDateTime.Now;
  Dest.fModified := TUTCDateTime.CreateNull;
  SetLength(Dest.fRequiredModules, 0);
  SetLength(Dest.fRequiredSnippets, 0);
  SetLength(Dest.fXRefs, 0);
  Dest.fNotes := NullMarkup;
  Dest.fFormat := TSnippetFormatID.Freeform;
  Dest.fTags := TTagSet.Create;
  Dest.fStarred := False;
end;

procedure TSnippet.SetModified(const AValue: TUTCDateTime);
begin
  Assert(not AValue.IsNull);
  fModified := AValue;
end;

procedure TSnippet.SetRequiredModules(const AValue: TArray<string>);
begin
  fRequiredModules := Copy(AValue);
end;

procedure TSnippet.SetRequiredSnippets(const AValue: TArray<TSnippetID>);
begin
  fRequiredSnippets := Copy(AValue);
end;

procedure TSnippet.SetTags(const AValue: ITagSet);
begin
  fTags := TTagSet.Create(AValue);
end;

procedure TSnippet.SetXRefs(const AValue: TArray<TSnippetID>);
begin
  fXRefs := Copy(AValue);
end;

end.
