{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Provides a class that create active text by parsing plain text mark-up.
}


unit CS.ActiveText.Parsers.PlainText;

interface


uses
  // Project
  CS.ActiveText;

type
  ///  <summary>Class that creates active text by parsing plain text.</summary>
  ///  <remarks>Text can optionally be parsed into active text paragraph
  ///  blocks.</remarks>
  TActiveTextPlainTextParser = class(TInterfacedObject, IActiveTextParser)
  public
    type
      ///  <summary>Enumeration of options used to govern how the text parser
      ///  operates.</summary>
      ///  <remarks>
      ///  <para>- ptpSplitIntoParas - The text is split into zero or more
      ///  active text paragraph blocks.</para>
      ///  <para>- ptpIgnoreEmptyParas - Any empty paragraphs in the text are
      ///  ignored. Ignored if ptpSplitIntoParas is not also specified.</para>
      ///  <para>- ptpTrim - Trim the text (and any paragraphs) of leading and
      ///  trailing white space.</para>
      ///  <para>- ptpCompressWhiteSpace - Compresses all white space into a
      ///  single space. Where ptpSplitIntoParas is supplied this is done after
      ///  the text is split into paragraphs.</para>
      ///  </remarks>
      TOption = (
        ptpSplitIntoParas, ptpIgnoreEmptyParas, ptpTrim, ptpCompressWhiteSpace
      );
      ///  <summary>Set of options used to govern how the text parser operates.
      ///  </summary>
      TOptions = set of TOption;
  strict private
    var
      ///  <summary>String used to separate paragraphs.</summary>
      ///  <remarks>Used only when ptpSplitIntoParas is in Options.</remarks>
      fParaSeparator: string;
      ///  <summary>Set of options that govern how the text parser operates.
      ///  </summary>
      fOptions: TOptions;
  public
    ///  <summary>Constructs new parser instance, customised per the given
    ///  parameters.</summary>
    ///  <param name="ParaSeparator">string [in] String used to delimit
    ///  paragraphs within the text being parsed. Ignored if ptpSplitIntoParas
    ///  option is not set, otherwise the value must not be the empty string.
    ///  </param>
    ///  <param name="Options">TOptions [in] Set of options governing how the
    ///  text is parsed.</param>
    constructor Create(const ParaSeparator: string; const Options: TOptions);
    ///  <summary>Parses plain text into active text.</summary>
    ///  <param name="Text">string [in] Plain text to be parsed.</param>
    ///  <returns>IActiveText. Active text object created from Text.</returns>
    ///  <remarks>Method of IActiveTextParser.</remarks>
    function Parse(const Text: string): IActiveText;
  end;


implementation

uses
  SysUtils,
  UIStringList,
  UStrUtils;

{ TActiveTextPlainTextParser }

constructor TActiveTextPlainTextParser.Create(const ParaSeparator: string;
  const Options: TOptions);
begin
  inherited Create;
  fParaSeparator := ParaSeparator;
  fOptions := Options;
end;

function TActiveTextPlainTextParser.Parse(const Text: string): IActiveText;
var
  Paras: IStringList;
  ProcessedText: string;
  Para: string;
  ProcessedPara: string;
begin
  Result := TActiveTextFactory.CreateActiveText;
  if ptpTrim in fOptions then
    ProcessedText := StrTrim(Text)
  else
    ProcessedText := Text;
  if ProcessedText = EmptyStr then
    Exit;
  if ptpSplitIntoParas in fOptions then
  begin
    Assert(fParaSeparator <> EmptyStr,
      ClassName + '.Parse: paragraph separator is empty');
    Paras := TIStringList.Create(
      ProcessedText,
      fParaSeparator,
      not (ptpIgnoreEmptyParas in fOptions),
      ptpTrim in fOptions
    );
    for Para in Paras do
    begin
      Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsOpen));
      if ptpCompressWhiteSpace in fOptions then
        ProcessedPara := StrCompressWhiteSpace(Para)
      else
        ProcessedPara := Para;
      Result.AddElem(TActiveTextFactory.CreateTextElem(ProcessedPara));
      Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsClose));
    end;
  end
  else
  begin
    if ptpCompressWhiteSpace in fOptions then
      ProcessedText := StrCompressWhiteSpace(ProcessedText);
    Result.AddElem(TActiveTextFactory.CreateTextElem(ProcessedText));
  end;
end;

end.
