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
 * Provides an implementation of IActiveTextParser that can parse the markup
 * used in Credits elements of data files and convert the markup into an active
 * text object.
}


unit CS.ActiveText.Parsers.Credits;


interface


uses
  // Project
  ActiveText.UMain;


type

  ///  <summary>Class that parses markup used in Credits element read from
  ///  snippets data files. Markup is translated into active text.</summary>
  ///  <remarks>The Credits element may occur in main database files and v1 of
  ///  the user database and export files.</remarks>
  TActiveTextCreditsParser = class(TInterfacedObject, IActiveTextParser)
  strict private
    var
      ///  <summary>URL to be used in any link contained in markup.</summary>
      fURL: string;
  public
    ///  <summary>Object constructor. Sets up object.</summary>
    ///  <param name="URL">string [in] URL to be used in any hyperlinks defined
    ///  by Credit markup.</param>
    constructor Create(const URL: string);
    ///  <summary>Parses markup credits mark-up into active text.</summary>
    ///  <param name="Markup">string [in] Markup containing definition of active
    ///  text. Must be valid Credits element markup.</param>
    ///  <returns>IActiveText. Active text object created by parser.</param>
    ///  <remarks>Implements IActiveTextParser.Parse.</remarks>
    function Parse(const Markup: string): IActiveText;
  end;


implementation


{
  About the "Credits" Markup
  --------------------------
  The markup is simple. It is just plain text with at most one group of text
  delimited by '[' and ']' characters. The text enclosed in brackets represents
  a hyperlink. The destination URL of the hyperlink is given by the URL
  parameter passed to the constructor.

  Examples:
    "Some markup without a link."
    "Some markup with a [link]."
}


uses
  // Project
  UStrUtils;


{ TActiveTextCreditsParser }

constructor TActiveTextCreditsParser.Create(const URL: string);
begin
  inherited Create;
  fURL := URL;
end;

function TActiveTextCreditsParser.Parse(const Markup: string): IActiveText;
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
  Result := TActiveTextFactory.CreateActiveText;
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
    Result.AddElem(TActiveTextFactory.CreateTextElem(Markup));
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
      Result.AddElem(TActiveTextFactory.CreateTextElem(Prefix));
    Result.AddElem(
      TActiveTextFactory.CreateActionElem(
        ekLink,
        TActiveTextFactory.CreateAttrs(
          TActiveTextAttr.Create(TActiveTextAttrNames.Link_URL, fURL)
        ),
        fsOpen
      )
    );
    Result.AddElem(TActiveTextFactory.CreateTextElem(LinkText));
    Result.AddElem(
      TActiveTextFactory.CreateActionElem(
        ekLink,
        TActiveTextFactory.CreateAttrs(
          TActiveTextAttr.Create(TActiveTextAttrNames.Link_URL, fURL)
        ),
        fsClose
      )
    );
    if Postfix <> '' then
      Result.AddElem(TActiveTextFactory.CreateTextElem(Postfix));
  end;
end;

end.

