{
 * URoutineDoc.pas
 *
 * Implements an abstract base class that renders a text document that describes
 * a snippet. Should be overridden by class that generate actual documents in
 * required output format.
 *
 * $Rev$
 * $Date$
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is URoutineDoc.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit URoutineDoc;


interface


uses
  // Delphi
  Classes,
  // Project
  Compilers.UGlobals, UActiveText, UIStringList, USnippets;


type

  {
  TCompileDocInfo:
    Provides information about a compilation result as text.
  }
  TCompileDocInfo = record
    Compiler: string;   // name of compiler
    Result: string;     // description of compilation result
    constructor Create(const ACompiler: string; const ACompRes: TCompileResult);
      {Record constructor. Initialises fields from compiler name and a compiler
      result.
        @param ACompiler [in] Compiler name.
        @param CompRes [in] Compiler result.
      }
  end;

  {
  TCompileDocInfoArray:
    Array of compiler result information.
  }
  TCompileDocInfoArray = array of TCompileDocInfo;

  {
  TRoutineDoc:
    Abstract base class that renders a text document that describes a snippet.
  }
  TRoutineDoc = class(TObject)
  strict private
    fDocStream: TStream;  // Stream that receives rendered document
    function RoutinesToStrings(const RoutineList: TRoutineList): IStringList;
      {Creates a string list containing a list of snippet names.
        @param RoutineList [in] List of snippets.
        @return String list containing names of snippets from list.
      }
    function CompilerInfo(const Routine: TRoutine): TCompileDocInfoArray;
      {Gets compiler compatibility information for a snippet.
        @param Routine [in] Snippet for which compiler information is required.
        @return Array of compiler compatibility information.
      }
  strict protected
    procedure InitialiseDoc; virtual;
      {Initialises document. Does nothing. Descendant classes should add any
      required initialisation here.
      }
    procedure RenderHeading(const Heading: string); virtual; abstract;
      {Outputs heading (snippet name).
        @param Heading [in] Heading to be written.
      }
    procedure RenderDescription(const Desc: string); virtual; abstract;
      {Outputs snippet description.
        @param Desc [in] Description to be written.
      }
    procedure RenderSourceCode(const SourceCode: string); virtual; abstract;
      {Outputs snippet's source code.
        @param SourceCode [in] Source code to be written.
      }
    procedure RenderTitledList(const Title: string; List: IStringList);
      virtual; abstract;
      {Outputs a list preceded by a title.
        @param Title [in] List title.
        @param List [in] List of text to be written.
      }
    procedure RenderTitledText(const Title, Text: string); virtual; abstract;
      {Outputs text preceded by a title.
        @param Title [in] Text title.
        @param Text [in] Text to be written.
      }
    procedure RenderCompilerInfo(const Heading: string;
      const Info: TCompileDocInfoArray); virtual; abstract;
      {Outputs details of compiler information.
        @param Heading [in] Heading for compiler information.
        @param Info [in] Array of compiler results (name and result as text).
      }
    procedure RenderExtra(const ExtraText: IActiveText); virtual; abstract;
      {Outputs snippet's extra information.
        @param ExtraText [in] Text to be written.
      }
    procedure RenderDBInfo(const Text: string); virtual; abstract;
      {Outputs information about code snippets database.
        @param Text [in] Text to be written.
      }
    procedure FinaliseDoc; virtual;
      {Finalises document. Does nothing. Descendant classes should add any
      required finalisation here.
      }
    function CommaList(const List: IStringList): string;
      {Builds a comma delimited list of names from a string list.
        @param List [in] List of names.
        @return Required comma separated list or "none" if list is empty.
      }
    property DocStream: TStream read fDocStream;
      {Reference to stream that receives rendered document. For use by
      sub-classes}
  public
    procedure Generate(const Routine: TRoutine; const DocStream: TStream);
      {Generates document that describes a snippet.
        @param Routine [in] Snippet for which document is required.
        @param Stream [in] Stream to which document is written.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  Compilers.UCompilers, UUtils, USnippetKindInfo, Web.UInfo;


{ TRoutineDoc }

function TRoutineDoc.CommaList(const List: IStringList): string;
  {Builds a comma delimited list of names from a string list.
    @param List [in] List of names.
    @return Required comma separated list or "none" if list is empty.
  }
resourcestring
  sNone = 'None.';  // string output for empty lists
begin
  Assert(Assigned(List), ClassName + '.CommaList: List is nil');
  if List.Count > 0 then
    Result := MakeSentence(List.GetText(', ', False))
  else
    Result := sNone;
end;

function TRoutineDoc.CompilerInfo(
  const Routine: TRoutine): TCompileDocInfoArray;
  {Gets compiler compatibility information for a snippet.
    @param Routine [in] Snippet for which compiler information is required.
    @return Array of compiler compatibility information.
  }
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
      Compiler.GetName, Routine.Compatibility[Compiler.GetID]
    );
    Inc(InfoIdx);
  end;
end;

procedure TRoutineDoc.FinaliseDoc;
  {Finalises document. Does nothing. Descendant classes should add any required
  finalisation here.
  }
begin
  // Do nothing
end;

procedure TRoutineDoc.Generate(const Routine: TRoutine;
  const DocStream: TStream);
  {Generates document that describes a snippet.
    @param Routine [in] Snippet for which document is required.
    @param Stream [in] Stream to which document is written.
  }
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
  Assert(Assigned(Routine), ClassName + '.Create: Routine is nil');
  Assert(Assigned(DocStream), ClassName + '.Create: DocStream is nil');
  // record output stream
  fDocStream := DocStream;
  // generate document
  InitialiseDoc;
  RenderHeading(Routine.Name);
  RenderDescription(Routine.Description);
  RenderSourceCode(Routine.SourceCode);
  RenderTitledText(
    sKindTitle, TSnippetKindInfoList.Instance[Routine.Kind].Description
  );
  RenderTitledText(
    sCategoryTitle, Snippets.Categories.Find(Routine.Category).Description
  );
  RenderTitledList(sUnitListTitle, TIStringList.Create(Routine.Units));
  RenderTitledList(sDependListTitle, RoutinesToStrings(Routine.Depends));
  RenderTitledList(sXRefListTitle, RoutinesToStrings(Routine.XRef));
  if Routine.Kind <> skFreeform then
    RenderCompilerInfo(sCompilers, CompilerInfo(Routine));
  if not Routine.Extra.IsEmpty then
    RenderExtra(Routine.Extra);
  if not Routine.UserDefined then
    // database info written only if snippet is from main database
    RenderDBInfo(Format(sMainDatabaseInfo, [TWebInfo.DatabaseURL]));
  FinaliseDoc;
end;

procedure TRoutineDoc.InitialiseDoc;
  {Initialises document. Does nothing. Descendant classes should add any
  required initialisation here.
  }
begin
  // Do nothing
end;

function TRoutineDoc.RoutinesToStrings(
  const RoutineList: TRoutineList): IStringList;
  {Creates a string list containing a list of snippet names.
    @param RoutineList [in] List of snippets.
    @return String list containing names of snippets from list.
  }
var
  Routine: TRoutine;  // each snippet in list
begin
  Result := TIStringList.Create;
  for Routine in RoutineList do
    Result.Add(Routine.Name);
end;

{ TCompileDocInfo }

constructor TCompileDocInfo.Create(const ACompiler: string;
  const ACompRes: TCompileResult);
  {Record constructor. Initialises fields from compiler name and a compiler
  result.
    @param ACompiler [in] Compiler name.
    @param CompRes [in] Compiler result.
  }
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

