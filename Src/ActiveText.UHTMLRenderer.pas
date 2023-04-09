{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Provides a class that renders active text as HTML.
}


unit ActiveText.UHTMLRenderer;


interface


uses
  // Delphi
  SysUtils,
  // Project
  ActiveText.UMain, UHTMLUtils;


type
  TActiveTextHTML = class(TObject)
  strict private
    type
      TTagInfo = class(TObject)
      public
        type
          TTagAttrCallback = reference to function
            (Elem: IActiveTextActionElem): IHTMLAttributes;
      strict private
        var
          fName: string;
          fAttrs: TTagAttrCallback;
      public
        property Name: string read fName;
        property Attrs: TTagAttrCallback read fAttrs;
        constructor Create(const AName: string; AAttrs: TTagAttrCallback);
      end;
    type
      TTagInfoMap = array[TActiveTextActionElemKind] of TTagInfo;
  public
    type
      TCSSStyles = class(TObject)
      strict private
        var
          fElemClassMap: array[TActiveTextActionElemKind] of string;
        procedure SetElemClass(ElemKind: TActiveTextActionElemKind;
          const Value: string); inline;
        function GetElemClass(ElemKind: TActiveTextActionElemKind): string;
          inline;
      public
        constructor Create;
        property ElemClasses[Kind: TActiveTextActionElemKind]: string
          read GetElemClass write SetElemClass;
      end;
  strict private
    var
      fCSSStyles: TCSSStyles;
      fBuilder: TStringBuilder;
      fLevel: Integer;
      fTagInfoMap: TTagInfoMap;
      fIsStartOfTextLine: Boolean;
      fLINestingDepth: Cardinal;
    const
      IndentMult = 2;
    procedure InitialiseTagInfoMap;
    function RenderTag(const TagElem: IActiveTextActionElem): string;
    function RenderText(const TextElem: IActiveTextTextElem): string;
    function MakeOpeningTag(const Elem: IActiveTextActionElem): string;
    function MakeClosingTag(const Elem: IActiveTextActionElem): string;
  public
    constructor Create;
    destructor Destroy; override;
    function Render(ActiveText: IActiveText): string;
  end;


implementation

uses
  UConsts, UIStringList, UStrUtils;


{ TActiveTextHTML }

constructor TActiveTextHTML.Create;
begin
  inherited Create;
  fCSSStyles := TCSSStyles.Create;
  fBuilder := TStringBuilder.Create;
  fLINestingDepth := 0;
  InitialiseTagInfoMap;
end;

destructor TActiveTextHTML.Destroy;
var
  TagInfo: TTagInfo;
begin
  fBuilder.Free;
  fCSSStyles.Free;
  for TagInfo in fTagInfoMap do
    TagInfo.Free;
  inherited;
end;

procedure TActiveTextHTML.InitialiseTagInfoMap;
var
  NullAttrs: TTagInfo.TTagAttrCallback;
  LinkAttrs: TTagInfo.TTagAttrCallback;
  AttrFn: TTagInfo.TTagAttrCallback;
  ElemKind: TActiveTextActionElemKind;
const
  Tags: array[TActiveTextActionElemKind] of string = (
    'a' {ekLink}, 'strong' {ekStrong}, 'em' {ekEm}, 'var' {ekVar}, 'p' {ekPara},
    'span' {ekWarning}, 'h2' {ekHeading}, 'code' {ekMono},
    'ul' {ekUnorderedList}, 'ol' {ekUnorderedList}, 'li' {ekListItem},
    'div' {ekBlock}, 'div' {ekDocument}
  );
begin
  NullAttrs := function(Elem: IActiveTextActionElem): IHTMLAttributes
    begin
      Result := nil;
    end;
  LinkAttrs := function(Elem: IActiveTextActionElem): IHTMLAttributes
    begin
      Result := THTMLAttributes.Create(
        'href', Elem.Attrs[TActiveTextAttrNames.Link_URL]
      );
    end;
  for ElemKind := Low(TActiveTextActionElemKind) to
    High(TActiveTextActionElemKind) do
  begin
    if ElemKind <> ekLink then
      AttrFn := NullAttrs
    else
      AttrFn := LinkAttrs;
    fTagInfoMap[ElemKind] := TTagInfo.Create(Tags[ElemKind], AttrFn);
  end;
end;

function TActiveTextHTML.MakeClosingTag(const Elem: IActiveTextActionElem):
  string;
begin
  Result := THTML.ClosingTag(fTagInfoMap[Elem.Kind].Name);
end;

function TActiveTextHTML.MakeOpeningTag(const Elem: IActiveTextActionElem):
  string;
var
  Attrs: IHTMLAttributes;
begin
  Attrs := fTagInfoMap[Elem.Kind].Attrs(Elem);
  if fCSSStyles.ElemClasses[Elem.Kind] <> '' then
  begin
    if not Assigned(Attrs) then
      Attrs := THTMLAttributes.Create;
    Attrs.Add('class', fCSSStyles.ElemClasses[Elem.Kind])
  end;
  Result := THTML.OpeningTag(fTagInfoMap[Elem.Kind].Name, Attrs);
end;

function TActiveTextHTML.Render(ActiveText: IActiveText): string;
var
  Elem: IActiveTextElem;          // each element in active text object
  TextElem: IActiveTextTextElem;  // an active text text element
  TagElem: IActiveTextActionElem; // an active text action element
  Text: string;
  SrcLines: IStringList;
  SrcLine: string;
  DestLines: IStringList;
  DestLine: string;
begin
  if not ActiveText.HasContent then
    Exit('');
  Text := '';
  fLevel := 0;
  for Elem in ActiveText do
  begin
    if Supports(Elem, IActiveTextTextElem, TextElem) then
      Text := Text + RenderText(TextElem)
    else if Supports(Elem, IActiveTextActionElem, TagElem) then
      Text := Text + RenderTag(TagElem);
  end;
  SrcLines := TIStringList.Create(Text, EOL, False);
  DestLines := TIStringList.Create;
  for SrcLine in SrcLines do
  begin
    DestLine := StrTrimRight(SrcLine);
    if not StrIsEmpty(DestLine) then
      DestLines.Add(DestLine);
  end;
  Result := DestLines.GetText(EOL, False);
end;

function TActiveTextHTML.RenderTag(const TagElem: IActiveTextActionElem):
  string;
begin
  Result := '';
  case TagElem.State of
    fsClose:
    begin
      Result := MakeClosingTag(TagElem);
      if TActiveTextElemCaps.DisplayStyleOf(TagElem.Kind) = dsBlock then
      begin
        Dec(fLevel);
        Result := EOL + StrOfSpaces(IndentMult * fLevel) + Result + EOL;
        fIsStartOfTextLine := True;
      end;
    end;
    fsOpen:
    begin
      Result := MakeOpeningTag(TagElem);
      if TActiveTextElemCaps.DisplayStyleOf(TagElem.Kind) = dsBlock then
      begin
        Result := EOL + StrOfSpaces(IndentMult * fLevel) + Result + EOL;
        Inc(fLevel);
        fIsStartOfTextLine := True;
      end
      else if TActiveTextElemCaps.DisplayStyleOf(TagElem.Kind) = dsInline then
      begin
        if fIsStartOfTextLine then
        begin
          Result := StrOfSpaces(IndentMult * fLevel) + Result;
          fIsStartOfTextLine := False;
        end;
      end;
    end;
  end;
end;

function TActiveTextHTML.RenderText(const TextElem: IActiveTextTextElem):
  string;
begin
  if fIsStartOfTextLine then
  begin
    Result := StrOfSpaces(IndentMult * fLevel);
    fIsStartOfTextLine := False;
  end
  else
    Result := '';
  Result := Result + THTML.Entities(TextElem.Text);
end;

{ TActiveTextHTML.TCSSStyles }

constructor TActiveTextHTML.TCSSStyles.Create;
const
  DefaultClasses: array[TActiveTextActionElemKind] of string = (
    'external-link' {ekLink}, '' {ekStrong}, '' {ekEm}, '' {ekVar}, '' {ekPara},
    'warning' {ekWarning}, '' {ekHeading}, '' {ekMono}, '' {ekUnorderedList},
    '' {ekOrderedList}, '' {ekListItem}, '' {ekBlock},
    'active-text' {ekDocument}
  );
var
  ElemKind: TActiveTextActionElemKind;
begin
  inherited Create;
  for ElemKind := Low(TActiveTextActionElemKind)
    to High(TActiveTextActionElemKind) do
    SetElemClass(ElemKind, DefaultClasses[ElemKind]);
end;

function TActiveTextHTML.TCSSStyles.GetElemClass(
  ElemKind: TActiveTextActionElemKind): string;
begin
  Result := fElemClassMap[ElemKind];
end;

procedure TActiveTextHTML.TCSSStyles.SetElemClass(
  ElemKind: TActiveTextActionElemKind; const Value: string);
begin
  fElemClassMap[ElemKind] := Value;
end;

{ TActiveTextHTML.TTagInfo }

constructor TActiveTextHTML.TTagInfo.Create(const AName: string;
  AAttrs: TTagAttrCallback);
begin
  inherited Create;
  fName := AName;
  fAttrs := AAttrs;
end;

end.

