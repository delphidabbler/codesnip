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
 * Implements a class that helps with parsing of data from input files into
 * active text for use in relevant properties of a snippet.
}


unit CS.ActiveText.Helper;


interface


uses
  // Project
  CS.ActiveText,
  UBaseObjects,
  UIStringList;


type

  {
  TActiveTextHelper:
    Class that helps with parsing of data from input files into active text for
    use in relevant properties of a snippet.
  }
  TActiveTextHelper = class(TNoConstructObject)
  public
    class function ParseCommentsAndCredits(const PrefixText, CreditsMarkup,
      URL: string): IActiveText;
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
    class function ParseREML(const REML: string): IActiveText;
      {Creates an active text object from a string containing markup in REML
      format.
        @param REML [in] REML markup language that defines active text.
        @return Required active text object. Will be an empty object if REML is
          empty string.
      }
    ///  <summary>Creates an active text object from a plain text string.
    ///  </summary>
    ///  <remarks>All text is treated as one paragraph, embedded newlines are
    ///  ignored. Text is trimmed of leading and trailing spaces. If trimmed
    ///  text is empty, empty active text is returned.</remarks>
    class function ParsePlainText(Text: string): IActiveText;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  CS.ActiveText.Parsers.Credits,
  CS.ActiveText.Parsers.PlainText,
  CS.ActiveText.Parsers.REML,
  CS.ActiveText.Renderers.REML,
  UREMLDataIO,
  UStrUtils;


{ TActiveTextHelper }

class function TActiveTextHelper.ParseCommentsAndCredits(const PrefixText,
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
  if not StrIsBlank(PrefixText) then
    // Add comments if present, enclosed in a paragraph block
    Result.Append(
      TActiveTextFactory.CreateActiveText(
        StrMakeSentence(StrTrim(PrefixText)),
        TActiveTextPlainTextParser.Create('', [])
      ).Normalise // Normalise ensures required paragraph block
    );
  if not StrIsBlank(CreditsMarkup) then
    // Add credits if present, enclosed in a paragraph block
    Result.Append(
      TActiveTextFactory.CreateActiveText(
        StrMakeSentence(StrTrim(CreditsMarkup)),
        TActiveTextCreditsParser.Create(URL)
      ).Normalise // Normalise ensures required paragraph block
    );
end;

class function TActiveTextHelper.ParsePlainText(Text: string): IActiveText;
begin
  Result := TActiveTextFactory.CreateActiveText(
    Text,
    TActiveTextPlainTextParser.Create('', [])
  ).Normalise;
end;

class function TActiveTextHelper.ParseREML(const REML: string): IActiveText;
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

end.

