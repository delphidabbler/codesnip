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
 * Implements a class that helps with parsing of a snippet's extra property as
 * active text and vice versa.
}


unit USnippetExtraHelper;


interface


uses
  // Project
  ActiveText.UMain,
  UBaseObjects,
  UIStringList;


type

  {
  TSnippetExtraHelper:
    Class that helps with the parsing of a snippet's Extra property as active
    text and vice versa.
  }
  TSnippetExtraHelper = class(TNoConstructObject)
  public
    class function BuildActiveText(const PrefixText, CreditsMarkup,
      URL: string): IActiveText; overload;
      {Builds an active text object containing some plain followed by active
      text defined by markup in the "Credits" format.
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
  CS.ActiveText.Parsers.Credits,
  CS.ActiveText.Parsers.REML,
  CS.ActiveText.Renderers.REML,
  UREMLDataIO,
  UStrUtils;


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
  // Add credits if present, enclosed in a paragraph block
  Result.Append(
    TActiveTextFactory.CreateActiveText(
      StrMakeSentence(CreditsMarkup),
      TActiveTextCreditsParser.Create(URL)
    ).Normalise // normalise ensures required paragraph block
  );
end;

class function TSnippetExtraHelper.BuildActiveText(
  const REML: string): IActiveText;
  {Builds an active text object from a string containing markup in REML format.
    @param REML [in] REML markup language that defines active text.
    @return Required active text object. Will be an empty object if REML is
      empty string.
  }
var
  ActiveText: IActiveText;  // receives active text built from REML
begin
  if REML = '' then
    Exit(TActiveTextFactory.CreateActiveText);
  // Create active text by parsing REML
  ActiveText := TActiveTextFactory.CreateActiveText(
    REML, TActiveTextREMLParser.Create
  );
  Result := ActiveText.Normalise;
end;

class function TSnippetExtraHelper.PlainTextToActiveText(
  Text: string): IActiveText;
begin
  Result := TActiveTextFactory.CreateActiveText;
  Text := StrTrim(Text);
  if Text = '' then
    Exit;
  Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsOpen));
  Result.AddElem(
    TActiveTextFactory.CreateTextElem(Text)
  );
  Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsClose));
end;

end.

