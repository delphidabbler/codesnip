{
 * USnippetExtraHelper.pas
 *
 * Implements a class that helps with parsing of a snippet's extra property as
 * active text and vice versa.
 *
 * $Rev$
 * $Date$
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is USnippetExtraHelper.pas, formerly
 * URoutineExtraHelper.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit USnippetExtraHelper;


interface


uses
  // Project
  UActiveText, UBaseObjects, UIStringList;


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
    class function BuildActiveText(REMLBlocks: IStringList): IActiveText;
      overload;
    class function BuildREMLMarkup(const ActiveText: IActiveText): string;
      {Creates REML markup from an active text object using latest version.
        @param ActiveText [in] Active text object used to generate markup.
        @return Required REML markup.
      }
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

  function IsBlockOpener(Elem: IActiveTextElem): Boolean;
  var
    ActionElem: IActiveTextActionElem;
  begin
    if not Supports(Elem, IActiveTextActionElem, ActionElem) then
      Exit(False);
    Result := (ActionElem.DisplayStyle = dsBlock)
      and (ActionElem.State = fsOpen);
  end;

  function IsBlockCloser(Elem: IActiveTextElem): Boolean;
  var
    ActionElem: IActiveTextActionElem;
  begin
    if not Supports(Elem, IActiveTextActionElem, ActionElem) then
      Exit(False);
    Result := (ActionElem.DisplayStyle = dsBlock)
      and (ActionElem.State = fsClose);
  end;

type
  ///  <summary>Describes different parts of parsed REML code in relation to
  ///  block tags.</summary>
  ///  <remarks>Can be within a pair of block tags; without, i.e. not enclosed
  ///  by block tags or in the transitional state between one and the other.
  ///  </remarks>
  TBlockState = (bsWithin, bsWithout, bsTransition);
var
  ActiveText: IActiveText;  // receives active text built from REML
  TextElem: IActiveTextTextElem;
  Text: string;
  BlockState: TBlockState;  // state of current position relating to block tags
  Elem: IActiveTextElem;    // each element in active text
begin
  Result := TActiveTextFactory.CreateActiveText;
  if REML = '' then
    Exit;
  // Create active text by parsing REML
  ActiveText := TActiveTextFactory.CreateActiveText(REML, TREMLReader.Create);
  if ActiveText.IsEmpty then
    Exit;
  // Scan active text, inserting paragraph level block tags where the active
  // text is not enclosed by them: this can be at the start, at the end or
  // between existing blocks. E.g for "xxx <p>yyy</p> xxx <p>yyy</p> xxx", xxx
  // is without any block and will be enclosed in paragraphs while yyy is within
  // a block and will be unchanged.
  BlockState := bsTransition;
  for Elem in ActiveText do
  begin
    if IsBlockOpener(Elem) then
    begin
      Assert(BlockState <> bsWithin,
        ClassName + '.BuildActiveText: Block is nested.');
      if BlockState = bsWithout then
        Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsClose));
      Result.AddElem(Elem);
      BlockState := bsWithin;
    end
    else if IsBlockCloser(Elem) then
    begin
      Assert(BlockState = bsWithin,
        ClassName + '.BuildActiveText: Block closer outside block.');
      Result.AddElem(Elem);
      BlockState := bsTransition;
    end
    else
    begin
      if BlockState = bsTransition then
      begin
        if Supports(Elem, IActiveTextTextElem, TextElem) then
        begin
          // make sure we don't start a paragraph block if text contains only
          // spaces
          Text := StrTrimLeft(TextElem.Text);
          if Text <> '' then
          begin
            Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsOpen));
            Result.AddElem(TActiveTextFactory.CreateTextElem(Text));
            BlockState := bsWithout;
          end;
        end
        else
        begin
          Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsOpen));
          Result.AddElem(Elem);
          BlockState := bsWithout;
        end;
      end
      else
        Result.AddElem(Elem);
    end;
  end;
  if BlockState = bsWithout then
    Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsClose));
end;

class function TSnippetExtraHelper.BuildActiveText(REMLBlocks: IStringList):
  IActiveText;
var
  Block: string;  // eack block of REML in string list
begin
  Result := TActiveTextFactory.CreateActiveText;
  for Block in REMLBlocks do
    Result.Append(BuildActiveText(Block));
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

end.

