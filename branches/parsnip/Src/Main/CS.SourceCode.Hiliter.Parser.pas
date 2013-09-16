{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines a class that parses source files according to a given syntax
 * highlighter brush and emits correctly styled highlighting elements.
}


unit CS.SourceCode.Hiliter.Parser;


interface


uses
  // Delphi
  Classes,
  // 3rd party
  SynEditHighlighter,
  // Project
  CS.SourceCode.Hiliter.Brushes;


type
  ///  <summary>Provides information about the brush and attribute that apply to
  ///  a syntax highlight element.</summary>
  ///  <remarks>Callers can use this information to determine the styling to
  ///  apply to the element.</remarks>
  TSyntaxHiliteElemInfo = record
  strict private
    var
      fBrushID: string;
      fAttrID: string;
  public
    constructor Create(const ABrushID, AAttrID: string);
    property BrushID: string read fBrushID;
    property AttrID: string read fAttrID;
  end;

  ///  <summary>Parses source code into different highlighting elements in a
  ///  manner dependant on a given syntax highlighter brush.</summary>
  ///  <remarks>Objects using this class are notified of each element via
  ///  events.</remarks>
  TSyntaxHiliteParser = class(TObject)
  public
    type
      ///  <summary>Type of event triggered when a code element has been parsed.
      ///  </summary>
      ///  <remarks>Provides information about the element to handler.</remarks>
      ///  <param name="Parser">TObject [in] Reference to the parser object that
      ///  triggered event.</param>
      ///  <param name="AttrID">string [in] Identifies the highlight attribute
      ///  to be used to render ElemText. May be the empty string if no specific
      ///  attribute applies.</param>
      ///  <param name="ElemText">string [in] Text to be rendered.</param>
      ///  <remarks>The caller must determine how to style each highlight
      ///  atrribute.</remarks>
      TParseElementEvent = procedure(Parser: TObject;
        const ElemInfo: TSyntaxHiliteElemInfo; const ElemText: string)
        of object;
  strict private
    var
      ///  <summary>Reference to OnElement event handler.</summary>
      fOnElement: TParseElementEvent;
      ///  <summary>Reference to OnLineBegin event handler.</summary>
      fOnLineBegin: TNotifyEvent;
      ///  <summary>Reference to OnLineEnd event handler.</summary>
      fOnLineEnd: TNotifyEvent;
      ///  <summary>Performs the required highlighting operation on the given
      ///  source code.</summary>
      ///  <param name="Source">TStrings [in] Lines of source code to be
      ///  processed.</param>
      ///  <param name="Brush">TSyntaxHiliterBrush [in] Brush used to perform
      ///  the highlighting. Must not be a null brush.</param>
      procedure DoHighlighting(const Source: TStrings;
        const Brush: TSyntaxHiliterBrush);
      ///  <summary>Performs a null highlighting operation on the given source
      ///  code.</summary>
      ///  <param name="Source">TStrings [in] Lines of source code to be
      ///  processed.</param>
      ///  <param name="Brush">TSyntaxHiliterBrush [in] Null highlighter brush
      ///  that does the "highlighting". Must be a null brush.</param>
      ///  <remarks>The source is simply broken into lines which are emitted one
      ///  at a time.</remarks>
      procedure DoNullHighlighting(const Source: TStrings;
        const Brush: TSyntaxHiliterBrush);
  strict protected
    ///  <summary>Triggers OnElement event for the given element text and
    ///  highlighting attribute.</summary>
    procedure DoElement(const ElemInfo: TSyntaxHiliteElemInfo;
      const ElemText: string);
    ///  <summary>Triggers OnLineBegin event.</summary>
    procedure DoLineBegin; virtual;
    ///  <summary>Triggers OnLineEnd events.</summary>
    procedure DoLineEnd; virtual;
  public
    ///  <summary>Parses the given source code using the given highlighter
    ///  brush, triggering events to inform the caller of progress.</summary>
    procedure Parse(const Source: string; const Brush: TSyntaxHiliterBrush);
    ///  <summary>Event triggered when a source element has been parsed to
    ///  inform the caller of text to be output and highlight attribute to be
    ///  used to render it.</summary>
    property OnElement: TParseElementEvent read fOnElement write fOnElement;
    ///  <summary>Event triggered just before the first element on a new line is
    ///  parsed.</summary>
    ///  <remarks>Callers should emit any output required to open a new line.
    ///  </remarks>
    property OnLineBegin: TNotifyEvent read fOnLineBegin write fOnLineBegin;
    ///  <summary>Event triggered at the end of each line of source code.
    ///  </summary>
    ///  <remarks>Calllers should emit any output required to end a line.
    ///  </remarks>
    property OnLineEnd: TNotifyEvent read fOnLineEnd write fOnLineEnd;
  end;


implementation


uses
  // Project
  UStrUtils;


{ TSyntaxHiliteParser }

procedure TSyntaxHiliteParser.DoElement(const ElemInfo: TSyntaxHiliteElemInfo;
  const ElemText: string);
begin
  if Assigned(fOnElement) then
    fOnElement(Self, ElemInfo, ElemText);
end;

procedure TSyntaxHiliteParser.DoHighlighting(const Source: TStrings;
  const Brush: TSyntaxHiliterBrush);
var
  LineIdx: Integer;
  Line: string;
  Attr: TSynHighlighterAttributes;
  Highlighter: TSynCustomHighlighter;
begin
  Assert(not Brush.IsNull, ClassName + '.DoHighlighting: Brush is null');
  // Do the parsing
  Highlighter := Brush.CreateHighlighter;
  try
    Highlighter.ResetRange;
    for LineIdx := 1 to Source.Count do  // this is a 1-based loop deliberately
    begin
      DoLineBegin;
      Line := StrTrimRight(Source[LineIdx - 1]); // trim trailing white space
      // NOTE: LineIdx parameter to .SetLine seems to be redundant except in
      // SynHighlighterMulti, which is not used here
      Highlighter.SetLine(Line, LineIdx);
      while not Highlighter.GetEOL do
      begin
        Attr := Highlighter.GetTokenAttribute;
        // NOTE: We check there is an attribute since some highlighters can
        // return a nil Attr: e.g. the Pascal highlighter does this if a
        // trailing EOF or null terminates the code
        if Assigned(Attr) then
          DoElement(
            TSyntaxHiliteElemInfo.Create(Brush.ID, Attr.Name),
            Highlighter.GetToken
          );
        Highlighter.Next;
      end;
      DoLineEnd;
    end;
  finally
    Highlighter.Free;
  end;
end;

procedure TSyntaxHiliteParser.DoLineBegin;
begin
  if Assigned(fOnLineBegin) then
    fOnLineBegin(Self);
end;

procedure TSyntaxHiliteParser.DoLineEnd;
begin
  if Assigned(fOnLineEnd) then
    fOnLineEnd(Self);
end;

procedure TSyntaxHiliteParser.DoNullHighlighting(const Source: TStrings;
  const Brush: TSyntaxHiliterBrush);
var
  Line: string;
begin
  for Line in Source do
  begin
    DoLineBegin;
    DoElement(TSyntaxHiliteElemInfo.Create(Brush.ID, ''), Line);
    DoLineEnd;
  end;
end;

procedure TSyntaxHiliteParser.Parse(const Source: string;
  const Brush: TSyntaxHiliterBrush);
var
  Lines: TStringList;
begin
  Assert(Assigned(Brush), ClassName + '.Parse: Brush is nil.');
  // Split source code into lines, trimming leading and trailing blank lines
  Lines := TStringList.Create;
  try
    Lines.Text := Source;
    while (Lines.Count > 0) and StrIsBlank(Lines[Pred(Lines.Count)]) do
      Lines.Delete(Pred(Lines.Count));
    while (Lines.Count > 0) and StrIsBlank(Lines[0]) do
      Lines.Delete(0);
    if not Brush.IsNull then
      DoHighlighting(Lines, Brush)
    else
      DoNullHighlighting(Lines, Brush);
  finally
    Lines.Free;
  end
end;

{ TSyntaxHiliteElemInfo }

constructor TSyntaxHiliteElemInfo.Create(const ABrushID, AAttrID: string);
begin
  fBrushID := ABrushID;
  fAttrID := AAttrID;
end;

end.

