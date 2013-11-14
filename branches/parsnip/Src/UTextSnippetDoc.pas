{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
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
      cIndent = 2;
  strict private
    ///  <summary>Renders given active text as word-wrapped paragraphs of width
    ///  cPageWidth and given indent. Blank lines are added between paragraphs
    ///  iff SpaceParas in True.</summary>
    procedure RenderActiveText(ActiveText: IActiveText; const Indent: Cardinal;
      const SpaceParas: Boolean);
  strict protected
    ///  <summary>Initialises plain text document.</summary>
    procedure InitialiseDoc; override;
    ///  <summary>Adds given heading to document.</summary>
    procedure RenderHeading(const Heading: string); override;
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
    ///  <summary>Finalises document and returns content as encoded data.
    ///  </summary>
    function FinaliseDoc: TEncodedData; override;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  ActiveText.UTextRenderer, UStrUtils;


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

procedure TTextSnippetDoc.RenderActiveText(ActiveText: IActiveText;
  const Indent: Cardinal; const SpaceParas: Boolean);
var
  Renderer: TActiveTextTextRenderer;
  Lines: TStringList;
begin
  Renderer := TActiveTextTextRenderer.Create;
  try
    Renderer.DisplayURLs := True;
    Lines := TStringList.Create;
    try
      Lines.Text := Renderer.Render(ActiveText);
      fWriter.WriteLine(
        StrTrimRight(
          StrWrap(
            Lines, cPageWidth - Indent, Indent, True
          )
        )
      );
    finally
      Lines.Free;
    end;
  finally
    Renderer.Free;
  end;
end;

procedure TTextSnippetDoc.RenderCompilerInfo(const Heading: string;
  const Info: TCompileDocInfoArray);
var
  Idx: Integer; // loops compiler information table
begin
  fWriter.WriteLine;
  fWriter.WriteLine(Heading);
  for Idx := Low(Info) to High(Info) do
    fWriter.WriteLine('%-20s%s', [Info[Idx].Compiler, Info[Idx].Result]);
end;

procedure TTextSnippetDoc.RenderDescription(const Desc: IActiveText);
begin
  fWriter.WriteLine;
  RenderActiveText(Desc, 0, True);
end;

procedure TTextSnippetDoc.RenderExtra(const ExtraText: IActiveText);
begin
  Assert(not ExtraText.IsEmpty, ClassName + '.RenderExtra: ExtraText is empty');
  fWriter.WriteLine;
  RenderActiveText(ExtraText, 0, True);
end;

procedure TTextSnippetDoc.RenderHeading(const Heading: string);
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

