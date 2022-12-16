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
  UREMLDataIO, USnippetCreditsParser, UStrUtils;


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
  if (CreditsMarkup <> '') then
  begin
    // We have credits: add credits and any hyperlink as a paragraph
    Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsOpen));
    Result.Append(
      TActiveTextFactory.CreateActiveText(
        StrMakeSentence(CreditsMarkup),
        TSnippetCreditsParser.Create(URL)
      )
    );
    Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsClose));
  end;
end;

class function TSnippetExtraHelper.BuildActiveText(
  const REML: string): IActiveText;
  {Builds an active text object from a string containing markup in REML format.
    @param REML [in] REML markup language that defines active text.
    @return Required active text object. Will be an empty object if REML is
      empty string.
  }

  // Check for an opening block tag
  function IsBlockOpener(Elem: IActiveTextElem): Boolean;
  var
    ActionElem: IActiveTextActionElem;
  begin
    if not Supports(Elem, IActiveTextActionElem, ActionElem) then
      Exit(False);
    Result := (TActiveTextElemCaps.DisplayStyleOf(ActionElem.Kind) = dsBlock)
      and (ActionElem.State = fsOpen);
  end;

  // Check for a closing block tag
  function IsBlockCloser(Elem: IActiveTextElem): Boolean;
  var
    ActionElem: IActiveTextActionElem;
  begin
    if not Supports(Elem, IActiveTextActionElem, ActionElem) then
      Exit(False);
    Result := (TActiveTextElemCaps.DisplayStyleOf(ActionElem.Kind) = dsBlock)
      and (ActionElem.State = fsClose);
  end;

  // Embed given content in a para block and append to result, unless content is
  // empty when do nothing.
  procedure AddNoneEmptyParaToResult(ParaContent: IActiveText);
  begin
    if ParaContent.IsEmpty then
      Exit;
    if StrTrim(ParaContent.ToString) = '' then
      Exit;
    Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsOpen));
    Result.Append(ParaContent);
    Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsClose));
  end;

var
  ActiveText: IActiveText;              // receives active text built from REML
  OutsideBlockActiveText: IActiveText;  // receives text outside of blocks
  Elem: IActiveTextElem;                // each element in active text
  Level: Integer;                       // depth of block levels
begin
  Result := TActiveTextFactory.CreateActiveText;
  if REML = '' then
    Exit;
  // Create active text by parsing REML
  ActiveText := TActiveTextFactory.CreateActiveText(REML, TREMLReader.Create);
  if ActiveText.IsEmpty then
    Exit;
  // Init block level & obj used to accumulate text outside blocks
  Level := 0;
  OutsideBlockActiveText := TActiveTextFactory.CreateActiveText;
  for Elem in ActiveText do
  begin
    if IsBlockOpener(Elem) then
    begin
      // We have block opener tag. Check for any text that preceeded a level
      // zero block and wrap it in a paragraph before writing the block opener
      if Level = 0 then
      begin
        if not OutsideBlockActiveText.IsEmpty then
        begin
          AddNoneEmptyParaToResult(OutsideBlockActiveText);
          OutsideBlockActiveText := TActiveTextFactory.CreateActiveText;
        end;
      end;
      Result.AddElem(Elem);
      Inc(Level); // drop down one level
    end
    else if IsBlockCloser(Elem) then
    begin
      // Block closer
      Dec(Level);
      Result.AddElem(Elem); // climb up one level
    end
    else
    begin
      // Not block opener or closer
      // If we're outside any block, append elem to store of elems not included
      // in blocks. If we're in a block, just add the elem to output
      if Level = 0 then
        OutsideBlockActiveText.AddElem(Elem)
      else
        Result.AddElem(Elem);
    end;
  end;
  Assert(Level = 0, ClassName + '.BuildActiveText: Unbalanced blocks');
  // Write any outstanding elems that occured outside a block
  if not OutsideBlockActiveText.IsEmpty then
    AddNoneEmptyParaToResult(OutsideBlockActiveText);
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
  Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsOpen));
  Result.AddElem(
    TActiveTextFactory.CreateTextElem(Text)
  );
  Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsClose));
end;

end.

