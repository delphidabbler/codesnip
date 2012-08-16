{
 * URoutineExtraHelper.pas
 *
 * Implements a class that helps with parsing of routine's extra property as
 * active text and generating extra property values from active text.
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
 * The Original Code is URoutineExtraHelper.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit URoutineExtraHelper;


interface


uses
  // Project
  UActiveText, UBaseObjects;


type

  {
  TRoutineExtraHelper:
    Class that helps with parsing of routine's extra property as active text and
    generating extra property values from active text.
  }
  TRoutineExtraHelper = class(TNoConstructObject)
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
    class function BuildREMLMarkupLowestVer(
      const ActiveText: IActiveText): string;
      {Creates REML markup from an active text object using lowest possible
      version.
        @param ActiveText [in] Active text object used to generate markup.
        @return Required REML markup.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UREMLDataIO, URoutineCreditsParser, UUtils;


{ TRoutineExtraHelper }

class function TRoutineExtraHelper.BuildActiveText(const PrefixText,
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
      TActiveTextFactory.CreateTextElem(MakeSentence(PrefixText))
    );
    Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsClose));
  end;
  if (CreditsMarkup <> '') then
  begin
    // We have credits: add credits and any hyperlink as a paragraph
    Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsOpen));
    Result.Append(
      TActiveTextFactory.CreateActiveText(
        MakeSentence(CreditsMarkup),
        TRoutineCreditsParser.Create(URL)
      )
    );
    Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsClose));
  end;
end;

class function TRoutineExtraHelper.BuildActiveText(
  const REML: string): IActiveText;
  {Builds an active text object from a string containing markup in REML format.
    @param REML [in] REML markup language that defines active text.
    @return Required active text object. Will be an empty object if REML is
      empty string.
  }
var
  ActiveText: IActiveText;  // receives active text built from REML
begin
  if REML <> '' then
  begin
    // create active text by parsing REML
    ActiveText := TActiveTextFactory.CreateActiveText(REML, TREMLReader.Create);
    if not ActiveText.IsEmpty then
    begin
      if not Supports(ActiveText[0], IActiveTextActionElem) or
        ((ActiveText[0] as IActiveTextActionElem).DisplayStyle <> dsBlock) then
      begin
        // active text is not enclosed in a block: so put in a paragraph
        Result := TActiveTextFactory.CreateActiveText;
        Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsOpen));
        Result.Append(ActiveText);
        Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsClose));
      end
      else
        // active text is enclosed in block
        Result := ActiveText;
    end;
  end
  else
    // no REML: empty active text
    Result := TActiveTextFactory.CreateActiveText;
end;

class function TRoutineExtraHelper.BuildREMLMarkup(
  const ActiveText: IActiveText): string;
  {Creates REML markup from an active text object using latest version.
    @param ActiveText [in] Active text object used to generate markup.
    @return Required REML markup.
  }
begin
  Result := TREMLWriter.Render(ActiveText, TREMLAnalyser.LATEST_VERSION);
end;

class function TRoutineExtraHelper.BuildREMLMarkupLowestVer(
  const ActiveText: IActiveText): string;
  {Creates REML markup from an active text object using lowest possible version.
    @param ActiveText [in] Active text object used to generate markup.
    @return Required REML markup.
  }
begin
  Result := TREMLWriter.Render(
    ActiveText, TREMLAnalyser.LowestWriterVersion(ActiveText)
  );
end;

end.

