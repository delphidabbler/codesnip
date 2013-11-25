{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
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
  CS.ActiveText,
  Compilers.UGlobals,
  DB.USnippet,
  UEncodings,
  UIStringList,
  USnippetIDs;


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
    function SnippetsToStrings(SnippetList: ISnippetIDList): IStringList;
    ///  <summary>Creates and returns an array of compiler compatibility
    ///  information for given snippet.</summary>
    function CompilerInfo(const Snippet: TSnippet): TCompileDocInfoArray;
  strict protected
    ///  <summary>Initialise document.</summary>
    ///  <remarks>Does nothing. Descendant classes should perform any required
    ///  initialisation here.</remarks>
    procedure InitialiseDoc; virtual;
    ///  <summary>Output given heading.</summary>
    procedure RenderHeading(const Heading: string); virtual; abstract;
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
    ///  <summary>Output given compiler info, preceeded by given heading.
    ///  </summary>
    procedure RenderCompilerInfo(const Heading: string;
      const Info: TCompileDocInfoArray); virtual; abstract;
    ///  <summary>Outputs given snippet notes.</summary>
    ///  <remarks>Active text must be interpreted in a manner that makes sense
    ///  for document format.</remarks>
    procedure RenderNotes(const NotesText: IActiveText); virtual; abstract;
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
  // Project
  Compilers.UCompilers, DB.UMain, DB.USnippetKind, UStrUtils, Web.UInfo;


{ TSnippetDoc }

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
  InfoIdx: Integer;       // index into output array
begin
  Compilers := TCompilersFactory.CreateAndLoadCompilers;
  SetLength(Result, Compilers.Count);
  InfoIdx := 0;
  for Compiler in Compilers do
  begin
    Result[InfoIdx] := TCompileDocInfo.Create(
      Compiler.GetName, Snippet.Compatibility[Compiler.GetID]
    );
    Inc(InfoIdx);
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
  sCompilers = 'Supported compilers:';
  sMainDatabaseInfo = 'A snippet from the DelphiDabbler CodeSnip Database (%s)';
begin
  Assert(Assigned(Snippet), ClassName + '.Create: Snippet is nil');
  // generate document
  InitialiseDoc;
  RenderHeading(Snippet.Title);
  RenderDescription(Snippet.Description);
  RenderSourceCode(Snippet.SourceCode);
  RenderTitledText(
    sKindTitle, TSnippetKindInfoList.Items[Snippet.Kind].DisplayName
  );
  RenderTitledText(
    sCategoryTitle, Database.Categories.Find(Snippet.Category).Description
  );
  RenderTitledList(sUnitListTitle, Snippet.RequiredModules);
  RenderTitledList(
    sDependListTitle, SnippetsToStrings(Snippet.RequiredSnippets)
  );
  RenderTitledList(sXRefListTitle, SnippetsToStrings(Snippet.XRefs));
  if Snippet.Kind <> skFreeform then
    RenderCompilerInfo(sCompilers, CompilerInfo(Snippet));
  if not Snippet.Notes.IsEmpty then
    RenderNotes(Snippet.Notes);
  Result := FinaliseDoc;
end;

procedure TSnippetDoc.InitialiseDoc;
begin
  // Do nothing
end;

function TSnippetDoc.SnippetsToStrings(SnippetList: ISnippetIDList):
  IStringList;
var
  SnippetID: TSnippetID;  // each snippet in list
begin
  Result := TIStringList.Create;
  for SnippetID in SnippetList do
    Result.Add(Database.Lookup(SnippetID).Title);
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

