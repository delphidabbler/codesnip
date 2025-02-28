{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2024, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements an abstract base class that renders a text document that describes
 * a snippet. Should be overridden by classes that generate actual documents in
 * required output format.
}


unit USnippetDoc;


interface


uses
  // Delphi
  Classes,
  // Project
  DB.UCollections,
  ActiveText.UMain, Compilers.UGlobals, DB.USnippet, UEncodings, UIStringList;


type
  ///  <summary>Records information about a compilation result as text.
  ///  </summary>
  TCompileDocInfo = record
    ///  <summary>Name of compiler.</summary>
    Compiler: string;
    ///  <summary>Description of compilation result.</summary>
    Result: string;
    ///  <summary>Initialises record files from given compiler name and
    ///  compilation result.</summary>
    constructor Create(const ACompiler: string; const ACompRes: TCompileResult);
  end;

type
  ///  <summary>Array of textual compiler result information.</summary>
  TCompileDocInfoArray = TArray<TCompileDocInfo>;

type
  ///  <summary>Abstract base class for classes that render documents that
  ///  describe snippets.</summary>
  TSnippetDoc = class(TObject)
  strict private
    ///  <summary>Creates and returns a string list containing snippet names
    ///  from given snippet list.</summary>
    function SnippetsToStrings(const SnippetList: TSnippetList): IStringList;
    ///  <summary>Creates and returns an array of compiler compatibility
    ///  information for given snippet.</summary>
    function CompilerInfo(const Snippet: TSnippet): TCompileDocInfoArray;
    ///  <summary>Generates and returns a string containing information about
    ///  the given collection.</summary>
    ///  <remarks>Information includes license and copyright information if
    ///  the collection's data format supports it.</remarks>
    function CollectionInfo(const ACollectionID: TCollectionID): string;
  strict protected
    ///  <summary>Initialise document.</summary>
    ///  <remarks>Does nothing. Descendant classes should perform any required
    ///  initialisation here.</remarks>
    procedure InitialiseDoc; virtual;

    ///  <summary>Output given heading, i.e. snippet name for snippet from a
    ///  given collection..</summary>
    ///  <remarks>Heading may be rendered differently depending on the snippet's
    ///  collection.</remarks>
    procedure RenderHeading(const Heading: string;
      const ACollectionID: TCollectionID); virtual; abstract;
    ///  <summary>Output given snippet description.</summary>
    procedure RenderDescription(const Desc: IActiveText); virtual; abstract;
    ///  <summary>Output given source code.</summary>
    procedure RenderSourceCode(const SourceCode: string); virtual; abstract;
    ///  <summary>Output given title followed by given text.</summary>
    procedure RenderTitledText(const Title, Text: string); virtual; abstract;
    ///  <summary>Output given comma-separated list of text, preceded by given
    ///  title.</summary>
    procedure RenderTitledList(const Title: string; List: IStringList);
      virtual; abstract;
    ///  <summary>Output given compiler test info, preceded by given heading.
    ///  </summary>
    procedure RenderCompilerInfo(const Heading: string;
      const Info: TCompileDocInfoArray); virtual; abstract;
    ///  <summary>Output message stating that there is no compiler test info,
    ///  preceded by given heading.</summary>
    procedure RenderNoCompilerInfo(const Heading, NoCompileTests: string);
      virtual; abstract;
    ///  <summary>Output given extra information to document.</summary>
    ///  <remarks>Active text must be interpreted in a manner that makes sense
    ///  for document format.</remarks>
    procedure RenderExtra(const ExtraText: IActiveText); virtual; abstract;
    ///  <summary>Output given information about a collection.</summary>
    procedure RenderCollectionInfo(const Text: string); virtual; abstract;
    ///  <summary>Finalise document and return content as encoded data.
    ///  </summary>
    ///  <remarks>Descendant classes should perform any required finalisation
    ///  here.</remarks>
    function FinaliseDoc: TEncodedData; virtual; abstract;
    ///  <summary>Builds and returns a comma separated list from strings in
    ///  given string list.</summary>
    function CommaList(const List: IStringList): string;
  public
    ///  <summary>Generates a document that describes given snippet and returns
    ///  as encoded data using an encoding that suits type of document.
    ///  </summary>
    function Generate(const Snippet: TSnippet): TEncodedData;
  end;


implementation


uses
  // Delphi
  SysUtils,
  Generics.Collections,
  // Project
  Compilers.UCompilers,
  DB.DataFormats,
  DB.UMain,
  DB.UMetaData,
  DB.USnippetKind,
  DBIO.MetaData.DCSC,
  UStrUtils,
  UUrl;


{ TSnippetDoc }

function TSnippetDoc.CollectionInfo(const ACollectionID: TCollectionID): string;
resourcestring
  sCollectionInfo = 'A snippet from the "%s" collection.';
var
  MetaData: IDBMetaData;
  Collection: TCollection;
begin
  Collection := TCollections.Instance.GetCollection(ACollectionID);
  Result := Format(sCollectionInfo, [Collection.Name]);
  MetaData := TMetaDataFactory.CreateInstance(Collection.Storage);
  if mdcLicense in MetaData.GetCapabilities then
  begin
    Result := Result + ' ' + MetaData.GetLicenseInfo.NameWithURL + '.';
    if (mdcCopyright in MetaData.GetCapabilities) then
      Result := Result + ' ' + MetaData.GetCopyrightInfo.ToString + '.';
  end;
end;

function TSnippetDoc.CommaList(const List: IStringList): string;
resourcestring
  sNone = 'None.';  // string output for empty lists
begin
  Assert(Assigned(List), ClassName + '.CommaList: List is nil');
  if List.Count > 0 then
    Result := StrMakeSentence(List.GetText(', ', False))
  else
    Result := sNone;
end;

function TSnippetDoc.CompilerInfo(const Snippet: TSnippet):
  TCompileDocInfoArray;
var
  Compilers: ICompilers;  // provided info about compilers
  Compiler: ICompiler;    // each supported compiler
  ResList: TList<TCompileDocInfo>;
begin
  Compilers := TCompilersFactory.CreateAndLoadCompilers;
  SetLength(Result, Compilers.Count);
  ResList := TList<TCompileDocInfo>.Create;
  try
    for Compiler in Compilers do
    begin
      if Snippet.Compatibility[Compiler.GetID] <> crQuery then
      ResList.Add(
        TCompileDocInfo.Create(
          Compiler.GetName, Snippet.Compatibility[Compiler.GetID]
        )
      );
    end;
    Result := ResList.ToArray;
  finally
    ResList.Free;
  end;
end;

function TSnippetDoc.Generate(const Snippet: TSnippet): TEncodedData;
resourcestring
  // Literal string required in output
  sKindTitle = 'Snippet Type:';
  sCategoryTitle = 'Category:';
  sUnitListTitle = 'Required units:';
  sDependListTitle = 'Required snippets:';
  sXRefListTitle = 'See also:';
  sCompilers = 'Compiler test results:';
  sNoCompilerTests = 'No compiler tests were carried out.';
var
  CompileResults: TCompileDocInfoArray;
begin
  Assert(Assigned(Snippet), ClassName + '.Create: Snippet is nil');
  // generate document
  InitialiseDoc;
  RenderHeading(Snippet.DisplayName, Snippet.CollectionID);
  RenderDescription(Snippet.Description);
  RenderSourceCode(Snippet.SourceCode);
  RenderTitledText(
    sKindTitle, TSnippetKindInfoList.Items[Snippet.Kind].DisplayName
  );
  RenderTitledText(
    sCategoryTitle, Database.Categories.Find(Snippet.Category).Description
  );
  RenderTitledList(sUnitListTitle, TIStringList.Create(Snippet.Units));
  RenderTitledList(sDependListTitle, SnippetsToStrings(Snippet.Depends));
  RenderTitledList(sXRefListTitle, SnippetsToStrings(Snippet.XRef));
  if Snippet.Kind <> skFreeform then
  begin
    CompileResults := CompilerInfo(Snippet);
    if Length(CompileResults) > 0 then
      RenderCompilerInfo(sCompilers, CompilerInfo(Snippet))
    else
      RenderNoCompilerInfo(sCompilers, sNoCompilerTests);
  end;
  if Snippet.Extra.HasContent then
    RenderExtra(Snippet.Extra);
  RenderCollectionInfo(CollectionInfo(Snippet.CollectionID));
  Result := FinaliseDoc;
end;

procedure TSnippetDoc.InitialiseDoc;
begin
  // Do nothing
end;

function TSnippetDoc.SnippetsToStrings(const SnippetList: TSnippetList):
  IStringList;
var
  Snippet: TSnippet;  // each snippet in list
begin
  Result := TIStringList.Create;
  for Snippet in SnippetList do
    Result.Add(Snippet.DisplayName);
end;

{ TCompileDocInfo }

constructor TCompileDocInfo.Create(const ACompiler: string;
  const ACompRes: TCompileResult);
resourcestring
  // Compiler results descriptions
  sSuccess = 'Compiles OK';
  sWarning = 'Compiles with warnings';
  sError = 'Does not compile';
  sQuery = 'Not tested';
const
  // Map of compiler results to descriptions
  cResults: array[TCompileResult] of string = (
    sSuccess, sWarning, sError, sQuery
  );
begin
  Compiler := ACompiler;
  Result := cResults[ACompRes];
end;

end.

