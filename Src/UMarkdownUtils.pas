{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2025, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Helper class used to generate Markdown formatted text.
}

unit UMarkdownUtils;

interface

uses
  // Project
  UConsts;

type
  TMarkdown = class
  strict private
    const
      ///  <summary>Character used in multiples of 1 to 6 to introduce a
      ///  heading.</summary>
      HeadingOpenerChar = Char('#');
      ///  <summary>Character used to introduce a block quote. Sometimes used in
      ///  multiple for nested block quotes.</summary>
      BlockquoteOpenerChar = Char('>');
      ///  <summary>Character used to delimit inline code, sometimes in
      ///  multiple, or in multiples of at least three for code fences.
      ///  </summary>
      CodeDelim = Char('`');
      ///  <summary>Characters used to delimit strongly emphasised text (bold).
      ///  </summary>
      StrongEmphasisDelim = '**';
      ///  <summary>Character used to delimit weakly emphasised text (italic).
      ///  </summary>
      WeakEmphasisDelim = Char('*');
      ///  <summary>Format string used to render a link (description first, URL
      ///  second).</summary>
      LinkFmtStr = '[%0:s](%1:s)';
      ///  <summary>Character used to introduce a bare URL.</summary>
      URLOpenerChar = Char('<');
      ///  <summary>Character used to close a bare URL.</summary>
      URLCloserChar = Char('>');
      ///  <summary>Character used to delimit table columns.</summary>
      TableColDelim = Char('|');
      ///  <summary>Character used in multiple for the ruling that separates a
      ///  table head from the body.</summary>
      TableRulingChar = Char('-');
      ///  <summary>Character used to introduce a bullet list item.</summary>
      ListItemBullet = Char('-');
      ///  <summary>String used to format a number that introduces a number list
      ///  item.</summary>
      ListItemNumberFmt = '%d.';
      ///  <summary>String used to indicate a ruling.</summary>
      Ruling = '----';
      ///  <summary>Characters that are escaped by prepending a \ to the same
      ///  character.</summary>
      EscapeChars = '\`*_{}[]<>()#+-!|';
      ///  <summary>Escape sequence used to specify a non-breaking space.
      ///  </summary>
      NonBreakingSpace = '\ ';

      ///  <summary>Size of each level of indentation in spaces.</summary>
      IndentSize = UInt8(4);

      ///  <summary>Minimum length of a code fence delimiter.</summary>
      MinCodeFenceLength = Cardinal(3);

    ///  <summary>Prepends an indent to the lines of given text.</summary>
    ///  <param name="AText"><c>string</c> [in] Text to be indented. If the text
    ///  contains multiple lines then each line is indented.</param>
    ///  <param name="AIndentLevel"><c>UInt8</c> [in] The number of levels of
    ///  indentation to be applied. If zero then no indentation is performed.
    ///  </param>
    ///  <remarks>Empty lines are not indented.</remarks>
    class function ApplyIndent(const AText: string; const AIndentLevel: UInt8):
      string;

  public

    ///  <summary>Replaces any escapable characters in given text with escaped
    ///  versions of the characters, to make the text suitable for inclusion in
    ///  Markdown code.</summary>
    ///  <param name="AText"><c>string</c> [in] Text to be escaped.</param>
    ///  <returns><c>string</c>. The escaped text.</returns>
    ///  <remarks>
    ///  <para>If <c>AText</c> includes any markdown code then it will be
    ///  escaped and will be rendered literally and have no effect. For example,
    ///  <c>**bold**</c> will be transformed to <c>\*\*bold\*\*</c>.</para>
    ///  <para>Sequences of N spaces, where N &gt;= 2, will be replaced with a
    ///  single space followed by N-1 non-breaking spaces.</para>
    ///  </remarks>
    class function EscapeText(const AText: string): string;

    ///  <summary>Renders markdown as a heading, optionally indented.</summary>
    ///  <param name="AMarkdown"><c>string</c> [in] Valid Markdown to include in
    ///  the heading. Will not be escaped.</param>
    ///  <param name="AHeadingLevel"><c>UInt8</c> [in] The heading level. Must
    ///  be in the range <c>1</c> to <c>6</c>.</param>
    ///  <param name="AIndentLevel"><c>UInt8</c> [in] The number of levels of
    ///  indentation required. Set to <c>0</c> (the default) for no indentation.
    ///  </param>
    ///  <returns><c>string</c>. The required heading Markdown.</returns>
    class function Heading(const AMarkdown: string; const AHeadingLevel: UInt8;
      const AIndentLevel: UInt8 = 0): string;

    ///  <summary>Renders markdown as a paragraph, optionally indented.
    ///  </summary>
    ///  <param name="AMarkdown"><c>string</c> [in] Valid Markdown to include in
    ///  the paragraph. Will not be escaped.</param>
    ///  <param name="AIndentLevel"><c>UInt8</c> [in] The number of levels of
    ///  indentation required. Set to <c>0</c> (the default) for no indentation.
    ///  </param>
    ///  <returns><c>string</c>. The required paragraph Markdown.</returns>
    class function Paragraph(const AMarkdown: string;
      const AIndentLevel: UInt8 = 0): string;

    ///  <summary>Renders markdown as a block quote, optionally indented.
    ///  </summary>
    ///  <param name="AMarkdown"><c>string</c> [in] Valid Markdown to include in
    ///  the block quote. Will not be escaped.</param>
    ///  <param name="ANestLevel"><c>UInt8</c> [in] The nesting level of the
    ///  block quote.</param>
    ///  <param name="AIndentLevel"><c>UInt8</c> [in] The number of levels of
    ///  indentation required. Set to <c>0</c> (the default) for no indentation.
    ///  </param>
    ///  <returns><c>string</c>. The required block quote Markdown.</returns>
    class function BlockQuote(const AMarkdown: string;
      const ANestLevel: UInt8 = 0; const AIndentLevel: UInt8 = 0): string;

    ///  <summary>Renders markdown as a bullet list item, optionally indented.
    ///  </summary>
    ///  <param name="AMarkdown"><c>string</c> [in] Valid Markdown to include in
    ///  the list item. Will not be escaped.</param>
    ///  <param name="AIndentLevel"><c>UInt8</c> [in] The number of levels of
    ///  indentation required. Set to <c>0</c> (the default) for no indentation.
    ///  </param>
    ///  <returns><c>string</c>. The required bullet list item Markdown.
    ///  </returns>
    class function BulletListItem(const AMarkdown: string;
      const AIndentLevel: UInt8 = 0): string;

    ///  <summary>Renders markdown as a number list item, optionally indented.
    ///  </summary>
    ///  <param name="AMarkdown"><c>string</c> [in] Valid Markdown to include in
    ///  the list item. Will not be escaped.</param>
    ///  <param name="ANumber"><c>UInt8</c> [in] The number to be used in the
    ///  list item. Must be &gt; <c>0</c>.</param>
    ///  <param name="AIndentLevel"><c>UInt8</c> [in] The number of levels of
    ///  indentation required. Set to <c>0</c> (the default) for no indentation.
    ///  </param>
    ///  <returns><c>string</c>. The required number list item Markdown.
    ///  </returns>
    class function NumberListItem(const AMarkdown: string;
      const ANumber: UInt8; const AIndentLevel: UInt8 = 0): string;

    ///  <summary>Renders pre-formatted code within code fences, optionally
    ///  indented.</summary>
    ///  <param name="ACode"><c>string</c> [in] The text of the code, which may
    ///  contain more than one line. Any markdown formatting within <c>ACode</c>
    ///  will be rendered literally.</param>
    ///  <param name="ALanguage"><c>string</c> [in] The name of any programming
    ///  language associated with the code. Set to an empty string (the default)
    ///  if there is no such language.</param>
    ///  <param name="AIndentLevel"><c>UInt8</c> [in] The number of levels of
    ///  indentation required. Set to <c>0</c> (the default) for no indentation.
    ///  </param>
    ///  <returns><c>string</c>. The required fenced code.</returns>
    class function FencedCode(const ACode: string; const ALanguage: string = '';
      const AIndentLevel: UInt8 = 0): string;

    ///  <summary>Renders pre-formatted code using indentation, optionally
    ///  indented further.</summary>
    ///  <param name="ACode"><c>string</c> [in] The text of the code block,
    ///  which may contain more than one line. Any markdown formatting within
    ///  <c>ACode</c> will be rendered literally.</param>
    ///  <param name="AIndentLevel"><c>UInt8</c> [in] The number of levels of
    ///  indentation required in addition to that required for the code block.
    ///  Set to <c>0</c> (the default) for no additional indentation.</param>
    ///  <returns><c>string</c>. The required fenced code.</returns>
    class function CodeBlock(const ACode: string;
      const AIndentLevel: UInt8 = 0): string;

    ///  <summary>Renders the headings to use at the top of a Markdown table.
    ///  Includes the ruling the is required below the table heading.
    ///  </summary>
    ///  <param name="AHeadings"><c>array of string</c> [in] An array of heading
    ///  text. There will be one table column per element. Each heading is
    ///  assumed to be valid Markdown and will not be escaped.</param>
    ///  <param name="AIndentLevel"><c>UInt8</c> [in] The number of levels of
    ///  indentation required before the table. Set to <c>0</c> (the default)
    ///  for no indentation.</param>
    ///  <returns><c>string</c>. The required Markdown formatted table heading.
    ///  </returns>
    ///  <remarks>This method MUST be called before the 1st call to
    ///  <c>TableRow</c>.</remarks>
    class function TableHeading(const AHeadings: array of string;
      const AIndentLevel: UInt8 = 0): string;

    ///  <summary>Renders the columns of text to use for a row of a Markdown
    ///  table.</summary>
    ///  <param name="AEntries"><c>array of string</c> [in] An array of column
    ///  text. There will be one table column per element. Each element is
    ///  assumed to be valid Markdown and will not be escaped.</param>
    ///  <param name="AIndentLevel"><c>UInt8</c> [in] The number of levels of
    ///  indentation required before the table. Set to <c>0</c> (the default)
    ///  for no indentation.</param>
    ///  <returns><c>string</c>. The required Markdown formatted table row.
    ///  </returns>
    ///  <remarks>
    ///  <para>Call this method once per table row.</para>
    ///  <para>The 1st call to this method MUST follow a call to
    ///  <c>TableHeading</c>.</para>
    ///  <para>The number of elements of <c>AEntries</c> should be the same for
    ///  each call of the method in the same table, and should be the same as
    ///  the number of headings passed to <c>TableHeading</c>.</para>
    ///  </remarks>
    class function TableRow(const AEntries: array of string;
      const AIndentLevel: UInt8 = 0): string;

    ///  <summary>Renders the Markdown representation of a ruling.</summary>
    ///  <param name="AIndentLevel"><c>UInt8</c> [in] The number of levels of
    ///  indentation required before the ruling. Set to <c>0</c> (the default)
    ///  for no indentation.</param>
    ///  <returns><c>string</c>. The required Markdown ruling.</returns>
    class function Rule(const AIndentLevel: UInt8 = 0): string;

    ///  <summary>Renders text as inline code.</summary>
    ///  <param name="ACode"><c>string</c> [in] The code. Any markdown
    ///  formatting within <c>ACode</c> will be rendered literally.</param>
    ///  <returns><c>string</c>. The required Markdown formatted code.</returns>
    class function InlineCode(const ACode: string): string;

    ///  <summary>Renders weakly formatted text.</summary>
    ///  <param name="AMarkdown"><c>string</c> [in] Text to be formatted.
    ///  May contain other inline Mardown formatting. Will not be escaped.
    ///  </param>
    ///  <returns><c>string</c>. The required Markdown formatted text.</returns>
    ///  <remarks>Usually rendered in italics.</remarks>
    class function WeakEmphasis(const AMarkdown: string): string;

    ///  <summary>Renders strongly formatted text.</summary>
    ///  <param name="AMarkdown"><c>string</c> [in] Text to be formatted.
    ///  May contain other inline Mardown formatting. Will not be escaped.
    ///  </param>
    ///  <returns><c>string</c>. The required Markdown formatted text.</returns>
    ///  <remarks>Usually rendered in bold.</remarks>
    class function StrongEmphasis(const AMarkdown: string): string;

    ///  <summary>Renders a link.</summary>
    ///  <param name="AMarkdown"><c>string</c> [in] The link's text, which may
    ///  include other inline Markdown formatting.</param>
    ///  <param name="AURL"><c>string</c> [in] The URL of the link. Must be
    ///  valid and correctly URL encoded.</param>
    ///  <returns><c>string</c>. The required Markdown formatted link.</returns>
    class function Link(const AMarkdown, AURL: string): string;

    ///  <summary>Renders a bare URL.</summary>
    ///  <param name="AURL"><c>string</c> [in] The required URL. Must be valid
    ///  and correctly URL encoded.</param>
    ///  <returns><c>string</c>. The required Markdown formatted URL.</returns>
    class function BareURL(const AURL: string): string;

  end;

implementation

uses
  // Delphi
  SysUtils,
  Classes,
  Math,
  // Project
  UStrUtils;

{ TMarkdown }

class function TMarkdown.ApplyIndent(const AText: string;
  const AIndentLevel: UInt8): string;
var
  Line: string;
  InLines, OutLines: TStrings;
begin
  Result := '';
  OutLines := nil;
  InLines := TStringList.Create;
  try
    OutLines := TStringList.Create;
    StrExplode(StrWindowsLineBreaks(AText), EOL, InLines);
    for Line in InLines do
      if Line <> '' then
        OutLines.Add(StrOfChar(' ', IndentSize * AIndentLevel) + Line)
      else
        OutLines.Add('');
    Result := StrJoin(OutLines, EOL);
  finally
    OutLines.Free;
    InLines.Free;
  end;
end;

class function TMarkdown.BareURL(const AURL: string): string;
begin
  Result := URLOpenerChar + AURL + URLCloserChar;
end;

class function TMarkdown.BlockQuote(const AMarkdown: string; const ANestLevel,
  AIndentLevel: UInt8): string;
begin
  Result := ApplyIndent(
    StrOfChar(BlockquoteOpenerChar, ANestLevel + 1) + ' ' + AMarkdown,
    AIndentLevel
  )
end;

class function TMarkdown.BulletListItem(const AMarkdown: string;
  const AIndentLevel: UInt8): string;
begin
  Result := ApplyIndent(ListItemBullet + ' ' + AMarkdown, AIndentLevel);
end;

class function TMarkdown.CodeBlock(const ACode: string;
  const AIndentLevel: UInt8): string;
var
  NormalisedCode: string;
begin
  if ACode = '' then
    Exit('');
  // Ensure code uses windows line breaks and is trimmed of trailing white space
  NormalisedCode := StrTrimRight(StrWindowsLineBreaks(ACode));
  // Indent each line by indent level + 1 since code blocks are identified by
  // being indented from the normal flow
  Result := ApplyIndent(NormalisedCode, AIndentLevel + 1);
end;

class function TMarkdown.EscapeText(const AText: string): string;
var
  MultipleSpaceLen: Cardinal;
  Spaces: string;
  EscapedSpaces: string;
  Idx: Integer;
begin
  // Escape non-space characters
  Result := StrBackslashEscape(AText, EscapeChars, EscapeChars);
  // Escape sequences of >= 2 spaces, with \ before each space except 1st one
  MultipleSpaceLen := StrMaxSequenceLength(' ', Result);
  while MultipleSpaceLen > 1 do
  begin
    Spaces := StrOfChar(' ', MultipleSpaceLen);
    EscapedSpaces := ' ';
    for Idx := 1 to Pred(MultipleSpaceLen) do
      EscapedSpaces := EscapedSpaces + NonBreakingSpace;
    Result := StrReplace(Result, Spaces, EscapedSpaces);
    MultipleSpaceLen := StrMaxSequenceLength(' ', Result);
  end;
  // Escape list starter chars if at start of line
end;

class function TMarkdown.FencedCode(const ACode, ALanguage: string;
  const AIndentLevel: UInt8): string;
var
  FenceLength: Cardinal;
  Fence: string;
  FencedCode: string;
  NormalisedCode: string;
begin
  if ACode = '' then
    Exit('');
  // Ensure code ends in at least one line break
  NormalisedCode := StrUnixLineBreaks(ACode);
  if NormalisedCode[Length(NormalisedCode)] <> LF then
    NormalisedCode := NormalisedCode + LF;
  NormalisedCode := StrWindowsLineBreaks(NormalisedCode);
  // Create fence that has correct length
  // TODO: only need to detect max fence length at start of line (excl spaces)
  FenceLength := Max(
    StrMaxSequenceLength(CodeDelim, ACode) + 1, MinCodeFenceLength
  );
  Fence := StrOfChar(CodeDelim, FenceLength);
  // Build fenced code
  FencedCode := Fence + ALanguage + EOL + NormalisedCode + Fence;
  // Indent each line of fenced code
  Result := ApplyIndent(FencedCode, AIndentLevel);
end;

class function TMarkdown.Heading(const AMarkdown: string;
  const AHeadingLevel, AIndentLevel: UInt8): string;
begin
  Assert(AHeadingLevel in [1..6],
    ClassName + '.Heading: AHeadingLevel must be in range 1..6');
  Result := ApplyIndent(
    StrOfChar(HeadingOpenerChar, AHeadingLevel) + ' ' + AMarkdown, AIndentLevel
  );
end;

class function TMarkdown.InlineCode(const ACode: string): string;
var
  CodeDelimLength: Cardinal;
  Delim: string;
begin
  CodeDelimLength := StrMaxSequenceLength(CodeDelim, ACode) + 1;
  Delim := StrOfChar(CodeDelim, CodeDelimLength);
  Result := Delim + ACode + Delim;
end;

class function TMarkdown.Link(const AMarkdown, AURL: string): string;
begin
  // TODO: make URL safe
  Result := Format(LinkFmtStr, [AMarkdown, AURL]);
end;

class function TMarkdown.NumberListItem(const AMarkdown: string; const ANumber,
  AIndentLevel: UInt8): string;
begin
  Assert(ANumber > 0, ClassName + 'NumberListItem: ANumber = 0');
  Result := ApplyIndent(
    Format(ListItemNumberFmt, [ANumber]) + ' ' + AMarkdown, AIndentLevel
  );
end;

class function TMarkdown.Paragraph(const AMarkdown: string;
  const AIndentLevel: UInt8): string;
begin
  Result := ApplyIndent(AMarkdown, AIndentLevel);
end;

class function TMarkdown.Rule(const AIndentLevel: UInt8): string;
begin
  Result := ApplyIndent(Ruling, AIndentLevel);
end;

class function TMarkdown.StrongEmphasis(const AMarkdown: string): string;
begin
  Result := StrongEmphasisDelim + AMarkdown + StrongEmphasisDelim;
end;

class function TMarkdown.TableHeading(const AHeadings: array of string;
  const AIndentLevel: UInt8): string;
var
  Heading: string;
  Ruling: string;
  HeadingRow: string;
begin
  if Length(AHeadings) = 0 then
    Exit('');
  Ruling := TableColDelim;
  HeadingRow := TableColDelim;
  for Heading in AHeadings do
  begin
    Ruling := Ruling + StrOfChar(TableRulingChar, Length(Heading) + 2)
      + TableColDelim;
    HeadingRow := HeadingRow + ' ' + Heading + ' ' + TableColDelim;
  end;
  Result := ApplyIndent(HeadingRow + EOL + Ruling, AIndentLevel);
end;

class function TMarkdown.TableRow(const AEntries: array of string;
  const AIndentLevel: UInt8): string;
var
  Entry: string;
  Row: string;
begin
  if Length(AEntries) = 0 then
    Exit('');
  Row := TableColDelim;
  for Entry in AEntries do
    Row := Row + ' ' + Entry + ' ' + TableColDelim;
  Result := ApplyIndent(Row, AIndentLevel);
end;

class function TMarkdown.WeakEmphasis(const AMarkdown: string): string;
begin
  Result := WeakEmphasisDelim + AMarkdown + WeakEmphasisDelim;
end;

end.
