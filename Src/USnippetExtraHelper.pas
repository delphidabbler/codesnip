{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that helps with parsing of a snippet's extra property as
 * active text and vice versa.
}


unit USnippetExtraHelper;


interface


uses
  // Project
  ActiveText.UMain, UBaseObjects, UIStringList;


type

  {
  TSnippetExtraHelper:
    Class that helps with the parsing of a snippet's Extra property as active
    text and vice versa.
  }
  TSnippetExtraHelper = class(TNoConstructObject)
  strict private
    type
      ///  <summary>Class that parses markup used in Credits element read from
      ///  snippets data files. Markup is translated into active text.</summary>
      ///  <remarks>
      ///  <para>Generated active text IS NOT embedded in an ekDocument block.
      ///  </para>
      ///  <para>The Credits element may occur in main database files and v1 of
      ///  the user database and export files.</para>
      ///  <para>Credits markup is simple. It is just plain text with at most
      ///  one group of text delimited by '[' and ']' characters. The text
      ///  enclosed in brackets represents a hyperlink. The destination URL of
      ///  the hyperlink is given by the URL parameter passed to the
      ///  constructor.</para>
      ///  <para>Eamples:</para>
      ///  <para><c>Some markup without a link.</c></para>
      ///  <para><c>Some markup with a [link].</c></para>
      ///  </remarks>
      TCreditsParser = class(TInterfacedObject, IActiveTextParser)
      strict private
        var
          ///  <summary>URL to be used in any link contained in markup.
          ///  </summary>
          fURL: string;
      public
        ///  <summary>Object constructor. Sets up object.</summary>
        ///  <param name="URL">string [in] URL to be used in any hyperlinks
        ///  defined by Credit markup.</param>
        constructor Create(const URL: string);
        ///  <summary>Parses markup and updates active text object.</summary>
        ///  <param name="Markup">string [in] Markup containing definition of
        ///  active text. Must be valid Credits element markup.</param>
        ///  <param name="ActiveText">IActiveText [in] Active text object
        ///  updated by parser.</param>
        ///  <remarks>
        ///  <para>NOTE: Does not wrap generated text in any block tags,
        ///  including top level document tags.</para>
        ///  <para>Implements IActiveTextParser.Parse.</para>
        ///  </remarks>
        procedure Parse(const Markup: string; const ActiveText: IActiveText);
      end;
  public
    class function BuildActiveText(const PrefixText, CreditsMarkup,
      URL: string): IActiveText; overload;
      {Builds an active text object containing some plain text followed by
      active text defined by markup in the "Credits" format.
        @param PrefixText [in] PrefixText text. If not empty string this is
          added as plain text before any credits markup.
        @param CreditsMarkup [in] "Credits" markup. May contain a link indicated
          by bracketed text. Both plain text and links can be generated. May be
          empty string.
        @param URL [in] URL to use in any link defined in CreditsMarkup.
        @return Required active text object. Will be an empty object if both
          PrefixText and CreditsMarkup are empty.
      }
    class function BuildActiveText(const REML: string): IActiveText; overload;
      {Builds an active text object from a string containing markup in REML
      format.
        @param REML [in] REML markup language that defines active text.
        @return Required active text object. Will be an empty object if REML is
          empty string.
      }
    class function BuildREMLMarkup(const ActiveText: IActiveText): string;
      {Creates REML markup from an active text object using latest version.
        @param ActiveText [in] Active text object used to generate markup.
        @return Required REML markup.
      }
    ///  <summary>Converts given plain text into an active text paragraph.
    ///  </summary>
    ///  <remarks>All text is treated as one paragraph, embedded newlines are
    ///  ignored. Text is trimmed of leading and trailing spaces. If trimmed
    ///  text is empty, empty active text is returned.</remarks>
    class function PlainTextToActiveText(Text: string): IActiveText;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UREMLDataIO, UStrUtils;


{ TSnippetExtraHelper }

class function TSnippetExtraHelper.BuildActiveText(const PrefixText,
  CreditsMarkup, URL: string): IActiveText;
  {Builds an active text object containing some plain followed by active text
  defined by markup in the "Credits" format.
    @param PrefixText [in] PrefixText text. If not empty string this is added as
      plain text before any credits markup.
    @param CreditsMarkup [in] "Credits" markup. May contain a link indicated by
      bracketed text. Both plain text and links can be generated. May be empty
      string.
    @param URL [in] URL to use in any link defined in CreditsMarkup.
    @return Required active text object. Will be an empty object if both
      PrefixText and CreditsMarkup are empty.
  }
begin
  // Create new empty active text object
  Result := TActiveTextFactory.CreateActiveText;
  Result.AddElem(TActiveTextFactory.CreateActionElem(ekDocument, fsOpen));
  if (PrefixText <> '') then
  begin
    // We have prefix text: add it to result as a paragraph containing a single
    // text element
    Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsOpen));
    Result.AddElem(
      TActiveTextFactory.CreateTextElem(StrMakeSentence(PrefixText))
    );
    Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsClose));
  end;
  if (CreditsMarkup <> '') then
  begin
    // We have credits: add credits and any hyperlink as a paragraph
    Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsOpen));
    Result.Append(
      TActiveTextFactory.CreateActiveText(
        StrMakeSentence(CreditsMarkup),
        TCreditsParser.Create(URL)
      )
    );
    Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsClose));
  end;
  Result.AddElem(TActiveTextFactory.CreateActionElem(ekDocument, fsClose));
end;

class function TSnippetExtraHelper.BuildActiveText(
  const REML: string): IActiveText;
  {Builds an active text object from a string containing markup in REML format.
    @param REML [in] REML markup language that defines active text.
    @return Required active text object. Will be an empty object if REML is
      empty string.
  }
begin
  // Create active text by parsing REML
  // .. the REML parser returns correct document or empty object if REML=''
  Result := TActiveTextFactory.CreateActiveText(REML, TREMLReader.Create);
end;

class function TSnippetExtraHelper.BuildREMLMarkup(
  const ActiveText: IActiveText): string;
  {Creates REML markup from an active text object using latest version.
    @param ActiveText [in] Active text object used to generate markup.
    @return Required REML markup.
  }
begin
  Result := TREMLWriter.Render(ActiveText);
end;

class function TSnippetExtraHelper.PlainTextToActiveText(
  Text: string): IActiveText;
begin
  Result := TActiveTextFactory.CreateActiveText;
  Text := StrTrim(Text);
  if Text = '' then
    Exit;
  Result.AddElem(TActiveTextFactory.CreateActionElem(ekDocument, fsOpen));
  Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsOpen));
  Result.AddElem(
    TActiveTextFactory.CreateTextElem(Text)
  );
  Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsClose));
  Result.AddElem(TActiveTextFactory.CreateActionElem(ekDocument, fsClose));
end;

{ TSnippetExtraHelper.TCreditsParser }

constructor TSnippetExtraHelper.TCreditsParser.Create(const URL: string);
begin
  inherited Create;
  fURL := URL;
end;

procedure TSnippetExtraHelper.TCreditsParser.Parse(const Markup: string;
  const ActiveText: IActiveText);
const
  cOpenBracket  = '[';  // open bracket character that starts a link
  cCloseBracket = ']';  // close bracket character that ends a link
resourcestring
  // Error messages
  sUnexpectedCloser   = 'Unexpected closing bracket found';
  sUnterminatedLink   = 'Unterminated link';
  sEmptyLink          = 'Empty link definition';
  sWrongBracketOrder  = 'Close bracket preceeds link open bracket';
  sMultipleOpeners    = 'More than one open bracket is present';
  sMultipleClosers    = 'More than one close bracket is present';
  sNoURL              = 'No URL specified';
var
  OpenBracketPos: Integer;  // position of opening bracket in markup
  CloseBracketPos: Integer; // position of closing bracket in markup
  Prefix, Postfix: string;  // text before and after link (can be empty)
  LinkText: string;         // link text
begin
  // Find open and closing brackets that delimit link text
  OpenBracketPos := StrPos(cOpenBracket, Markup);
  CloseBracketPos := StrPos(cCloseBracket, Markup);
  if OpenBracketPos = 0 then
  begin
    // No links: plain text only
    // check for errors
    if CloseBracketPos > 0 then
      raise EActiveTextParserError.Create(sUnexpectedCloser);
    // record text element
    ActiveText.AddElem(TActiveTextFactory.CreateTextElem(Markup));
  end
  else
  begin
    // We have a potential link
    // check for errors
    if CloseBracketPos = 0 then
      raise EActiveTextParserError.Create(sUnterminatedLink);
    if CloseBracketPos = OpenBracketPos + 1 then
      raise EActiveTextParserError.Create(sEmptyLink);
    if CloseBracketPos < OpenBracketPos then
      raise EActiveTextParserError.Create(sWrongBracketOrder);
    if StrCountDelims(cOpenBracket, Markup) > 1 then
      raise EActiveTextParserError.Create(sMultipleOpeners);
    if StrCountDelims(cCloseBracket, Markup) > 1 then
      raise EActiveTextParserError.Create(sMultipleClosers);
    // must have a URL
    if fURL = '' then
      raise EActiveTextParserError.Create(sNoURL);
    // get the various components
    LinkText := StrSlice(
      Markup, OpenBracketPos + 1, CloseBracketPos - OpenBracketPos - 1
    );
    Assert(LinkText <> '',
      ClassName + '.Parse: Link text is '' but has passed check');
    Prefix := StrSliceLeft(Markup, OpenBracketPos - 1);
    Postfix := StrSliceRight(Markup, Length(Markup) - CloseBracketPos);
    // record the elements
    if Prefix <> '' then
      ActiveText.AddElem(TActiveTextFactory.CreateTextElem(Prefix));
    ActiveText.AddElem(
      TActiveTextFactory.CreateActionElem(
        ekLink,
        TActiveTextFactory.CreateAttrs(
          TActiveTextAttr.Create(TActiveTextAttrNames.Link_URL, fURL)
        ),
        fsOpen
      )
    );
    ActiveText.AddElem(TActiveTextFactory.CreateTextElem(LinkText));
    ActiveText.AddElem(
      TActiveTextFactory.CreateActionElem(
        ekLink,
        TActiveTextFactory.CreateAttrs(
          TActiveTextAttr.Create(TActiveTextAttrNames.Link_URL, fURL)
        ),
        fsClose
      )
    );
    if Postfix <> '' then
      ActiveText.AddElem(TActiveTextFactory.CreateTextElem(Postfix));
  end;
end;

end.

