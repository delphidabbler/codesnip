{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2025, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that renders a document that describes a snippet in
 * Markdown format.
}


unit UMarkdownSnippetDoc;

interface

uses
  // Delphi
  SysUtils,
  // Project
  ActiveText.UMain,
  DB.Vaults,
  Hiliter.UGlobals,
  UEncodings,
  UIStringList,
  USnippetDoc;

type
  ///  <summary>Renders a document that describes a snippet in Markdown format.
  ///  </summary>
  TMarkdownSnippetDoc = class sealed (TSnippetDoc)
  strict private
    var
      ///  <summary>Object used to build Markdown source code document.
      ///  </summary>
      fDocument: TStringBuilder;
      ///  <summary>Flag indicating if the snippet has Pascal code.</summary>
      ///  <remarks>When <c>False</c> plain text is assumed.</remarks>
      fIsPascal: Boolean;
  strict private
    ///  <summary>Renders a Markdown paragraph with all given text emboldened.
    ///  </summary>
    procedure RenderStrongPara(const AText: string);
    ///  <summary>Renders the given active text as Markdown.</summary>
    function ActiveTextToMarkdown(ActiveText: IActiveText): string;
  strict protected
    ///  <summary>Initialises the Markdown document.</summary>
    procedure InitialiseDoc; override;
    ///  <summary>Output given heading, i.e. snippet name for snippet from a
    ///  given vault.</summary>
    ///  <remarks>Heading may be rendered differently depending on the snippet's
    ///  vault.</remarks>
    procedure RenderHeading(const Heading: string;
      const AVaultID: TVaultID); override;
    ///  <summary>Adds the given snippet description to the document.</summary>
    ///  <remarks>Active text formatting is observed and styled to suit the
    ///  document.</remarks>
    procedure RenderDescription(const Desc: IActiveText); override;
    ///  <summary>Highlights the given source code and adds it to the document.
    ///  </summary>
    procedure RenderSourceCode(const SourceCode: string); override;
    ///  <summary>Adds the given title, followed by the given text, to the
    ///  document.</summary>
    procedure RenderTitledText(const Title, Text: string); override;
    ///  <summary>Adds a comma-separated list of text, preceded by the given
    ///  title, to the document.</summary>
    procedure RenderTitledList(const Title: string; List: IStringList);
      override;
    ///  <summary>Outputs the given compiler test info, preceded by the given
    ///  heading.</summary>
    procedure RenderCompilerInfo(const Heading: string;
      const Info: TCompileDocInfoArray); override;
    ///  <summary>Outputs the given message stating that there is no compiler
    ///  test info, preceded by the given heading.</summary>
    procedure RenderNoCompilerInfo(const Heading, NoCompileTests: string);
      override;
    ///  <summary>Adds the given extra information about the snippet to the
    ///  document.</summary>
    ///  <remarks>Active text formatting is observed and styled to suit the
    ///  document.</remarks>
    procedure RenderExtra(const ExtraText: IActiveText); override;
    ///  <summary>Output given information about a vault.</summary>
    procedure RenderVaultInfo(const Text: string); override;
    ///  <summary>Finalises the document and returns its content as encoded
    ///  data.</summary>
    function FinaliseDoc: TEncodedData; override;
  public
    ///  <summary>Constructs an object to render Markdown information.</summary>
    ///  <param name="AIsPascal"><c>Boolean</c> [in] Flag indicating whether the
    ///  snippet contains Pascal code.</param>
    constructor Create(const AIsPascal: Boolean);
    ///  <summary>Destroys the object.</summary>
    destructor Destroy; override;
  end;

implementation

uses
  // Delphi
  UStrUtils,
  // Project
  ActiveText.UMarkdownRenderer,
  UMarkdownUtils;

{ TMarkdownSnippetDoc }

function TMarkdownSnippetDoc.ActiveTextToMarkdown(
  ActiveText: IActiveText): string;
var
  Renderer: TActiveTextMarkdown;
begin
  Renderer := TActiveTextMarkdown.Create;
  try
    Result := Renderer.Render(ActiveText);
  finally
    Renderer.Free;
  end;
end;

constructor TMarkdownSnippetDoc.Create(const AIsPascal: Boolean);
begin
  inherited Create;
  fDocument := TStringBuilder.Create;
  fIsPascal := AIsPascal;
end;

destructor TMarkdownSnippetDoc.Destroy;
begin
  fDocument.Free;
  inherited;
end;

function TMarkdownSnippetDoc.FinaliseDoc: TEncodedData;
begin
  Result := TEncodedData.Create(fDocument.ToString, etUnicode);
end;

procedure TMarkdownSnippetDoc.InitialiseDoc;
begin
  // Do nowt
end;

procedure TMarkdownSnippetDoc.RenderCompilerInfo(const Heading: string;
  const Info: TCompileDocInfoArray);
resourcestring
  sCompiler = 'Compiler';
  sResults = 'Results';
var
  CompilerInfo: TCompileDocInfo;  // info about each compiler
begin
  RenderStrongPara(Heading);

  fDocument.AppendLine(TMarkdown.TableHeading([sCompiler, sResults]));
  for CompilerInfo in Info do
    fDocument.AppendLine(
      TMarkdown.TableRow([CompilerInfo.Compiler, CompilerInfo.Result])
    );
  fDocument.AppendLine;
end;

procedure TMarkdownSnippetDoc.RenderDescription(const Desc: IActiveText);
var
  DescStr: string;
begin
  DescStr := ActiveTextToMarkdown(Desc);
  if not StrIsEmpty(DescStr, True) then
    fDocument.AppendLine(DescStr);
end;

procedure TMarkdownSnippetDoc.RenderExtra(const ExtraText: IActiveText);
var
  ExtraStr: string;
begin
  ExtraStr := ActiveTextToMarkdown(ExtraText);
  if not StrIsEmpty(ExtraStr, True) then
    fDocument.AppendLine(ExtraStr);
end;

procedure TMarkdownSnippetDoc.RenderHeading(const Heading: string;
  const AVaultID: TVaultID);
begin
  fDocument
    .AppendLine(TMarkdown.Heading(TMarkdown.EscapeText(Heading), 1))
    .AppendLine;
end;

procedure TMarkdownSnippetDoc.RenderNoCompilerInfo(const Heading,
  NoCompileTests: string);
begin
  RenderStrongPara(Heading);
  fDocument
    .AppendLine(TMarkdown.Paragraph(TMarkdown.EscapeText(NoCompileTests)))
    .AppendLine;
end;

procedure TMarkdownSnippetDoc.RenderSourceCode(const SourceCode: string);
begin
  fDocument
    .AppendLine(
      TMarkdown.FencedCode(SourceCode, StrIf(fIsPascal, 'pascal', ''))
    )
    .AppendLine;
end;

procedure TMarkdownSnippetDoc.RenderStrongPara(const AText: string);
begin
  fDocument
    .AppendLine(
      TMarkdown.Paragraph(
        TMarkdown.StrongEmphasis(TMarkdown.EscapeText(AText))
      )
    )
    .AppendLine;
end;

procedure TMarkdownSnippetDoc.RenderTitledList(const Title: string;
  List: IStringList);
begin
  RenderTitledText(Title, CommaList(List));
end;

procedure TMarkdownSnippetDoc.RenderTitledText(const Title, Text: string);
begin
  RenderStrongPara(Title);
  fDocument
    .AppendLine(TMarkdown.Paragraph(TMarkdown.EscapeText(Text)))
    .AppendLine;
end;

procedure TMarkdownSnippetDoc.RenderVaultInfo(const Text: string);
begin
  fDocument
    .AppendLine(TMarkdown.WeakEmphasis(TMarkdown.EscapeText(Text)))
    .AppendLine;
end;

end.
