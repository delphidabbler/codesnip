{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2022, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that renders a document that describes a snippet as plain
 * text.
}


unit UTextSnippetDoc;


interface


uses
  // Delphi
  Classes,
  // Project
  ActiveText.UMain, UEncodings, UIStringList, USnippetDoc;


type
  ///  <summary>Renders a document that describes a snippet as plain text.
  ///  </summary>
  TTextSnippetDoc = class(TSnippetDoc)
  strict private
    var
      ///  <summary>Object used to build plain text document.</summary>
      fWriter: TStringWriter;
    const
      ///  <summary>Width of output in characters.</summary>
      cPageWidth = 80;
      ///  <summary>Size of a single level of indentation in characters.
      ///  </summary>
      cIndent = 4;
  strict private
    ///  <summary>Renders given active text as word-wrapped paragraphs of width
    ///  cPageWidth.</summary>
    procedure RenderActiveText(ActiveText: IActiveText);
  strict protected
    ///  <summary>Initialises plain text document.</summary>
    procedure InitialiseDoc; override;
    ///  <summary>Adds given heading (i.e. snippet name) to document. Can be
    ///  user defined or from main database.</summary>
    ///  <remarks>Heading is output the same whether user defined or not, so
    ///  UserDefined parameter is ignored.</remarks>
    procedure RenderHeading(const Heading: string; const UserDefined: Boolean);
      override;
    ///  <summary>Interprets and adds given snippet description to document.
    ///  </summary>
    ///  <remarks>Active text is converted to word-wrapped plain text
    ///  paragraphs.</remarks>
    procedure RenderDescription(const Desc: IActiveText); override;
    ///  <summary>Adds given source code to document.</summary>
    procedure RenderSourceCode(const SourceCode: string); override;
    ///  <summary>Adds given title followed by given text to document.</summary>
    procedure RenderTitledText(const Title, Text: string); override;
    ///  <summary>Adds a comma-separated list of text, preceded by given title,
    ///  to document.</summary>
    procedure RenderTitledList(const Title: string; List: IStringList);
      override;
    ///  <summary>Adds given compiler info, preceeded by given heading, to
    ///  document.</summary>
    procedure RenderCompilerInfo(const Heading: string;
      const Info: TCompileDocInfoArray); override;
    ///  <summary>Interprets and adds given extra information to document.
    ///  </summary>
    ///  <remarks>Active text is converted to word-wrapped plain text
    ///  paragraphs.</remarks>
    procedure RenderExtra(const ExtraText: IActiveText); override;
    ///  <summary>Adds given information about code snippets database to
    ///  document.</summary>
    procedure RenderDBInfo(const Text: string); override;
    ///  <summary>Finalises document and returns content as encoded data.
    ///  </summary>
    function FinaliseDoc: TEncodedData; override;
  end;


implementation


uses
  // Delphi
  SysUtils, Character,
  // Project
  ActiveText.UTextRenderer, UConsts, UStrUtils;


{ TTextSnippetDoc }

function TTextSnippetDoc.FinaliseDoc: TEncodedData;
begin
  Result := TEncodedData.Create(fWriter.ToString, etUnicode);
  fWriter.Free;
end;

procedure TTextSnippetDoc.InitialiseDoc;
begin
  fWriter := TStringWriter.Create;
end;

procedure TTextSnippetDoc.RenderActiveText(ActiveText: IActiveText);
var
  Renderer: TActiveTextTextRenderer;
begin
  Renderer := TActiveTextTextRenderer.Create;
  try
    Renderer.DisplayURLs := True;
    Renderer.IndentDelta := cIndent;
    fWriter.WriteLine(
      Renderer.RenderWrapped(ActiveText, cPageWidth, 0)
    );
  finally
    Renderer.Free;
  end;
end;

procedure TTextSnippetDoc.RenderCompilerInfo(const Heading: string;
  const Info: TCompileDocInfoArray);
var
  MaxNameLength: Integer;
  CompilerInfo: TCompileDocInfo;
begin
  // Calculate length of longest compiler name
  MaxNameLength := 0;
  for CompilerInfo in Info do
    if Length(CompilerInfo.Compiler) > MaxNameLength then
      MaxNameLength := Length(CompilerInfo.Compiler);
  // Write out compilers with results
  fWriter.WriteLine;
  fWriter.WriteLine(Heading);
  for CompilerInfo in Info do
    fWriter.WriteLine(
      '%-*s%s', [MaxNameLength + 4, CompilerInfo.Compiler, CompilerInfo.Result]
    );
end;

procedure TTextSnippetDoc.RenderDBInfo(const Text: string);
begin
  fWriter.WriteLine;
  fWriter.WriteLine(StrWrap(Text, cPageWidth, 0));
end;

procedure TTextSnippetDoc.RenderDescription(const Desc: IActiveText);
begin
  fWriter.WriteLine;
  RenderActiveText(Desc);
end;

procedure TTextSnippetDoc.RenderExtra(const ExtraText: IActiveText);
begin
  Assert(ExtraText.HasContent,
    ClassName + '.RenderExtra: ExtraText has no content');
  fWriter.WriteLine;
  RenderActiveText(ExtraText);
end;

procedure TTextSnippetDoc.RenderHeading(const Heading: string;
  const UserDefined: Boolean);
begin
  fWriter.WriteLine(Heading);
end;

procedure TTextSnippetDoc.RenderSourceCode(const SourceCode: string);
begin
  fWriter.WriteLine;
  fWriter.WriteLine(SourceCode);
  fWriter.WriteLine;
end;

procedure TTextSnippetDoc.RenderTitledList(const Title: string;
  List: IStringList);
begin
  RenderTitledText(Title, CommaList(List));
end;

procedure TTextSnippetDoc.RenderTitledText(const Title, Text: string);
begin
  fWriter.WriteLine(Title);
  fWriter.WriteLine(StrWrap(Text, cPageWidth - cIndent, cIndent));
end;

end.

