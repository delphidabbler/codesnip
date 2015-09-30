{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a class that converts snippets from the SWAG database into
 * CodeSnip format and adds them to the database.
}


unit SWAG.UImporter;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  CS.ActiveText,
  CS.Database.Types,
  SWAG.UCommon;


type
  ///  <summary>Class that converts snippets from the SWAG database into
  ///  CodeSnip database format and imports them into the snippets database.
  ///  </summary>
  ///  <remarks>This class creates new snippets from records that define a SWAG
  ///  snippet. It is for the caller to acquire the required SWAG snippets from
  ///  the SWAG database.</remarks>
  TSWAGImporter = class(TObject)
  strict private
    type
      ///  <summary>Type of callback function used to notify the caller that a
      ///  snippet has been imported.</summary>
      ///  <param name="SWAGSnippet">TSWAGSnippet [in] Data of imported snippet.
      ///  </param>
      TProgressCallback = reference to procedure (
        const SWAGSnippet: TSWAGSnippet);
    var
      ///  <summary>List of SWAG snippets to be imported.</summary>
      fImportList: TList<TSWAGSnippet>;
      ///  <summary>Records the common active text that is included in the Notes
      ///  property of each imported snippet.</summary>
      fNotesBoilerplate: IActiveText;
    ///  <summary>Returns the common active text that will be included in the
    ///  Notes property of each imported snippet.</summary>
    function NotesBoilerplate: IActiveText;
    ///  <summary>Records the data from the given SWAG snippet into a data
    ///  structure suitable for adding to the snippets database.</summary>
    function BuildSnippetInfo(const SWAGSnippet: TSWAGSnippet):
      IEditableSnippet;
    ///  <summary>Imports (i.e. adds) the given SWAG snippet into the database.
    ///  </summary>
    procedure ImportSnippet(const SWAGSnippet: TSWAGSnippet);
  public
    ///  <summary>Constructs new object instance.</summary>
    constructor Create;
    ///  <summary>Destroys object instance.</summary>
    destructor Destroy; override;
    ///  <summary>Resets list of snippets to import.</summary>
    ///  <remarks>After calling this method no snippets are recorded for
    ///  inclusion in the import.</remarks>
    procedure Reset;
    ///  <summary>Records the given SWAG snippet ready for import into the
    ///  database.</summary>
    procedure IncludeSnippet(const SWAGSnippet: TSWAGSnippet);
    ///  <summary>Removes the given SWAG snippet from the list of snippets
    ///  awaiting import into the database.</summary>
    procedure ExcludeSnippet(const SWAGSnippet: TSWAGSnippet);
    ///  <summary>Checks if the given SWAG snippet had already been imported
    ///  into the database.</summary>
    function SnippetExistsInDB(const SWAGSnippet: TSWAGSnippet): Boolean;
    ///  <summary>Imports all the required SWAG snippets into the database.
    ///  </summary>
    ///  <param name="Callback">TProgressCallback [in] Optional callback to be
    ///  called after each SWAG snippet is imported.</param>
    ///  <remarks>The snippets that are imported are those that have been
    ///  recorded by calling IncludeSnippet.</remarks>
    procedure Import(const Callback: TProgressCallback = nil);
    ///  <summary>Creates and returns a valid snippet ID string, based on the
    ///  given SWAG snippet ID, that is unique in the snippets database.
    ///  </summary>
    class function MakeValidSnippetIDString(SWAGSnippetID: Cardinal): string;
    ///  <summary>Name of tag applied to all imported SWAG snippets.</summary>
    class function SWAGTagName: string;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  CS.Database.SnippetOrigins,
  CS.Database.Tags,
  CS.SourceCode.Languages,
  CS.Utils.Dates,
  DB.UMain;


{ TSWAGImporter }

function TSWAGImporter.BuildSnippetInfo(const SWAGSnippet: TSWAGSnippet):
  IEditableSnippet;

  // Constructs and returns the new snippet's description as active text.
  function BuildDescription: IActiveText;
  begin
    Result := TActiveTextFactory.CreateActiveText;
    Result.AddElem(
      TActiveTextFactory.CreateActionElem(ekPara, fsOpen)
    );
    Result.AddElem(TActiveTextFactory.CreateTextElem(SWAGSnippet.Title));
    Result.AddElem(
      TActiveTextFactory.CreateActionElem(ekPara, fsClose)
    );
  end;

  // Constructs and returns the active text to be stored in the new snippet's
  // Notes field.
  function BuildNotes: IActiveText;
  resourcestring
    sAuthor = 'Author(s): %s';
  begin
    Result := TActiveTextFactory.CloneActiveText(NotesBoilerplate);
    Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsOpen));
    Result.AddElem(
      TActiveTextFactory.CreateTextElem(Format(sAuthor, [SWAGSnippet.Author]))
    );
    Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsClose));
  end;

  function BuildTags: ITagSet;
  begin
    Result := TTagSet.Create;
    Result.Add(TTag.Create(SWAGTagName));
  end;

begin
  Result := Database.NewSnippet;
  Result.KindID := skFreeform;
  Result.Tags := BuildTags;
  Result.Description := BuildDescription;
  Result.SourceCode := SWAGSnippet.SourceCode;
  if SWAGSnippet.IsDocument then
    Result.LanguageID := TSourceCodeLanguageID.CreatePlainText
  else
    Result.LanguageID := TSourceCodeLanguageID.CreatePascal;
  Result.Title := SWAGSnippet.Title;
  Result.Notes := BuildNotes;
  // NOTE: Snippet has no required units, required snippets or cross-references
  Result.Origin := TRemoteSnippetOrigin.Create(
    sosSWAG,
    MakeValidSnippetIDString(SWAGSnippet.ID),
    TUTCDateTime.Create(SWAGSnippet.DateStamp)
  );
end;

constructor TSWAGImporter.Create;
begin
  inherited Create;
  fImportList := TList<TSWAGSnippet>.Create;
end;

destructor TSWAGImporter.Destroy;
begin
  fImportList.Free;
  inherited;
end;

procedure TSWAGImporter.ExcludeSnippet(const SWAGSnippet: TSWAGSnippet);
begin
  fImportList.Remove(SWAGSnippet);
end;

procedure TSWAGImporter.Import(const Callback: TProgressCallback);
var
  SWAGSnippet: TSWAGSnippet;
begin
  for SWAGSnippet in fImportList do
  begin
    if Assigned(Callback) then
      Callback(SWAGSnippet);
    ImportSnippet(SWAGSnippet);
  end;
end;

procedure TSWAGImporter.ImportSnippet(const SWAGSnippet: TSWAGSnippet);
begin
  // TODO: implement a "bulk add snippet" option into TDatabase
  Database.AddSnippet(BuildSnippetInfo(SWAGSnippet));
end;

procedure TSWAGImporter.IncludeSnippet(const SWAGSnippet: TSWAGSnippet);
begin
  if not fImportList.Contains(SWAGSnippet) then
    fImportList.Add(SWAGSnippet);
end;

class function TSWAGImporter.MakeValidSnippetIDString(SWAGSnippetID: Cardinal):
  string;
begin
  Result := IntToStr(SWAGSnippetID);
end;

function TSWAGImporter.NotesBoilerplate: IActiveText;
resourcestring
  sStatementPrefix = 'This snippet was imported from the ';
  sStatementLinkText = 'SWAG Pascal Archive';
  sStatementPostfix = '. ';
  sLicensePrefix = 'Unless stated otherwise this snippet is licensed under '
    + 'the ';
  sLicenseLinkText = 'BSD 3-Clause License';
  sLicensePostfix = '.';
const
  // URLs of web pages referenced from links in boilerplate
  SWAGDBURI = 'http://swag.delphidabbler.com/';
  BSD3URI = 'http://opensource.org/licenses/BSD-3-Clause';
var
  // Active text attributes for links included in boilerplate
  SWAGDBURIAttr: IActiveTextAttrs;
  BSD3URIAttr: IActiveTextAttrs;
begin
  if not Assigned(fNotesBoilerplate) then
  begin
    SWAGDBURIAttr := TActiveTextFactory.CreateAttrs(
      TActiveTextAttr.Create('href', SWAGDBURI)
    );
    BSD3URIAttr := TActiveTextFactory.CreateAttrs(
      TActiveTextAttr.Create('href', BSD3URI)
    );
    fNotesBoilerplate := TActiveTextFactory.CreateActiveText;
    fNotesBoilerplate.AddElem(
      TActiveTextFactory.CreateActionElem(ekPara, fsOpen)
    );
    fNotesBoilerplate.AddElem(
      TActiveTextFactory.CreateTextElem(sStatementPrefix)
    );
    fNotesBoilerplate.AddElem(
      TActiveTextFactory.CreateActionElem(ekLink, SWAGDBURIAttr, fsOpen)
    );
    fNotesBoilerplate.AddElem(
      TActiveTextFactory.CreateTextElem(sStatementLinkText)
    );
    fNotesBoilerplate.AddElem(
      TActiveTextFactory.CreateActionElem(ekLink, SWAGDBURIAttr, fsClose)
    );
    fNotesBoilerplate.AddElem(
      TActiveTextFactory.CreateTextElem(sStatementPostfix)
    );
    fNotesBoilerplate.AddElem(
      TActiveTextFactory.CreateTextElem(sLicensePrefix)
    );
    fNotesBoilerplate.AddElem(
      TActiveTextFactory.CreateActionElem(ekLink, BSD3URIAttr, fsOpen)
    );
    fNotesBoilerplate.AddElem(
      TActiveTextFactory.CreateTextElem(sLicenseLinkText)
    );
    fNotesBoilerplate.AddElem(
      TActiveTextFactory.CreateActionElem(ekLink, BSD3URIAttr, fsClose)
    );
    fNotesBoilerplate.AddElem(
      TActiveTextFactory.CreateTextElem(sLicensePostfix)
    );
    fNotesBoilerplate.AddElem(
      TActiveTextFactory.CreateActionElem(ekPara, fsClose)
    );
  end;
  Result := fNotesBoilerplate;
end;

procedure TSWAGImporter.Reset;
begin
  fImportList.Clear;
end;

function TSWAGImporter.SnippetExistsInDB(const SWAGSnippet: TSWAGSnippet):
  Boolean;
var
  Dummy: ISnippet;
  IDStr: string;
begin
  IDStr := MakeValidSnippetIDString(SWAGSnippet.ID);
  Result := Database.TrySelectSnippet(
    function (TestSnippet: ISnippet): Boolean
    begin
      Result := (TestSnippet.Origin.Source = sosSWAG)
        and (TestSnippet.Origin.OriginalID = IDStr);
    end,
    Dummy
  );
end;

class function TSWAGImporter.SWAGTagName: string;
resourcestring
  sTagName = 'SWAG';
begin
  Result := sTagName;
end;

end.

