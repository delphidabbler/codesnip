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
 * Provides a class that can create active text by parsing REML mark-up.
}


unit CS.ActiveText.Parsers.REML;


interface


uses
  // Project
  ActiveText.UMain,
  UREMLDataIO;


type
  ///  <summary>Class that creates active text by parsing REML mark-up.
  ///  </summary>
  TActiveTextREMLParser = class(TInterfacedObject, IActiveTextParser)
  strict private
    var
      ///  <summary>Receives active text created as REML is parsed.</summary>
      fActiveText: IActiveText;
    ///  <summary>Handler for TREMLParser's OnText event. Adds a text node to
    ///  the active text.</summary>
    procedure REMLTextHandler(Sender: TObject; const Text: string);
    ///  <summary>Handler for TREMLParser's OnTag event. Adds a suitable action
    ///  element to the active text.</summary>
    procedure REMLTagHandler(Sender: TObject; const TagInfo: TREMLTagInfo);
  public
    ///  <summary>Parses REML mark-up into active text.</summary>
    ///  <param name="Markup">string [in] REML mark-up to be parsed.</param>
    ///  <param name="ActiveText">IActiveText [in] Active text object that
    ///  is updated from parsed mark-up. If ActiveText already contains some
    ///  active text the result of parsing the mark-up is appended.</param>
    ///  <remarks>Method of IActiveTextParser.</remarks>
    procedure Parse(const Markup: string; const ActiveText: IActiveText);
  end;


implementation


{ TActiveTextREMLParser }

procedure TActiveTextREMLParser.Parse(const Markup: string;
  const ActiveText: IActiveText);
var
  REMLParser: TREMLParser;
begin
  Assert(Assigned(ActiveText), ClassName + '.Parse: ActiveText is nil');
  try
    fActiveText := ActiveText;
    REMLParser := TREMLParser.Create;
    try
      REMLParser.OnText := REMLTextHandler;
      REMLParser.OnTag := REMLTagHandler;
      REMLParser.Parse(Markup);
    finally
      REMLParser.Free;
    end;
  except
    on E: EREMLParseError do
      raise EActiveTextParserError.Create(E);
    else
      raise;
  end;
end;

procedure TActiveTextREMLParser.REMLTagHandler(Sender: TObject;
  const TagInfo: TREMLTagInfo);
const
  IDMap: array[TREMLTagID] of TActiveTextActionElemKind = (
    ekLink, ekStrong, ekEm, ekVar, ekPara, ekWarning, ekHeading, ekMono
  );
  StateMap: array[TREMLTagState] of TActiveTextElemState = (
    fsOpen, fsClose
  );
var
  Attrs: IActiveTextAttrs;
begin
  if TagInfo.Attr.IsNull then
    Attrs := TActiveTextFactory.CreateAttrs
  else
    Attrs := TActiveTextFactory.CreateAttrs(
      TActiveTextAttr.Create(TagInfo.Attr.Key, TagInfo.Attr.Value)
    );
  fActiveText.AddElem(
    TActiveTextFactory.CreateActionElem(
      IDMap[TagInfo.ID], Attrs, StateMap[TagInfo.State]
    )
  );
end;

procedure TActiveTextREMLParser.REMLTextHandler(Sender: TObject;
  const Text: string);
begin
  fActiveText.AddElem(TActiveTextFactory.CreateTextElem(Text));
end;

end.
