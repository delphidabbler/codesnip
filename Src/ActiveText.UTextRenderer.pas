{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2022, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements class that renders active text as plain text in fixed width, word
 * wrapped paragraphs.
}


unit ActiveText.UTextRenderer;

interface

uses
  SysUtils, Generics.Collections,
  ActiveText.UMain,
  UConsts;

type
  TActiveTextTextRenderer = class(TObject)
  strict private
    const
      ///  <summary>Special space character used to indicate the start of a list
      ///  item.</summary>
      ///  <remarks>This special character is a necessary kludge because some
      ///  code that renders active text as formatted plain text strips away
      ///  leading #32 characters as part of the formatting process. Therefore
      ///  indentation in list items is lost if #32 characters are used for it.
      ///  NBSP was chosen since it should render the same as a space if not
      ///  removed.</remarks>
      LISpacer = NBSP;  // Do not localise. Must be <> #32
      ///  <summary>Bullet character used when rendering unordered list items.
      ///  </summary>
      Bullet = '*';     // Do not localise. Must be <> #32 and <> LISpacer
      DefaultIndentDelta = 2;
    type
      TListKind = (lkNumber, lkBullet);
      TListState = record
      public
        ListNumber: Cardinal;
        ListKind: TListKind;
        constructor Create(AListKind: TListKind);
      end;
      TLIState = record
        IsFirstPara: Boolean;
        constructor Create(AIsFirstPara: Boolean);
      end;
    var
      fDisplayURLs: Boolean;
      fParaBuilder: TStringBuilder;
      fDocBuilder: TStringBuilder;
      fBlocksStack: TStack<TActiveTextActionElemKind>;
      fListStack: TStack<TListState>;
      fLIStack: TStack<TLIState>;
      fIndent: UInt16;
      fInPara: Boolean;
      fInListItem: Boolean;
      fIndentDelta: UInt8;
    function CanEmitInline: Boolean;
    procedure AppendToPara(const AText: string);
    procedure InitialiseRender;
    procedure FinaliseRender;
    procedure OutputParagraph;
    procedure RenderTextElem(Elem: IActiveTextTextElem);
    procedure RenderBlockActionElem(Elem: IActiveTextActionElem);
    procedure RenderInlineActionElem(Elem: IActiveTextActionElem);
    procedure RenderURL(Elem: IActiveTextActionElem);
    function Render(ActiveText: IActiveText): string;
  public
    constructor Create;
    destructor Destroy; override;
    property DisplayURLs: Boolean read fDisplayURLs write fDisplayURLs
      default False;
    property IndentDelta: UInt8 read fIndentDelta write fIndentDelta
      default DefaultIndentDelta;
    function RenderWrapped(ActiveText: IActiveText; const PageWidth,
      LMargin: Cardinal): string;
  end;


implementation

uses
  // Delphi
  Character,
  // Project
  UIStringList,
  UStrUtils;

{ TActiveTextTextRenderer }

procedure TActiveTextTextRenderer.AppendToPara(const AText: string);
begin
  if AText = '' then
    Exit;
  fParaBuilder.Append(AText);
  fInPara := True;
end;

function TActiveTextTextRenderer.CanEmitInline: Boolean;
begin
  if fBlocksStack.Count <= 0 then
    Exit(False);
  Result := TActiveTextElemCaps.CanContainText(fBlocksStack.Peek);
end;

constructor TActiveTextTextRenderer.Create;
begin
  Assert(LISpacer <> ' ', ClassName + '.Create: LISpacer can''t be #32');
  Assert(Bullet <> ' ', ClassName + '.Create: Bullet can''t be #32');
  Assert(Bullet <> LISpacer, ClassName + '.Create: Bullet = LISpacer');
  inherited Create;
  fParaBuilder := TStringBuilder.Create;
  fDocBuilder := TStringBuilder.Create;
  fDisplayURLs := False;
  fBlocksStack := TStack<TActiveTextActionElemKind>.Create;
  fListStack := TStack<TListState>.Create;
  fLIStack := TStack<TLIState>.Create;
  fIndent := 0;
  fInPara := False;
  fInListItem := False;
  fIndentDelta := DefaultIndentDelta;
end;

destructor TActiveTextTextRenderer.Destroy;
begin
  fLIStack.Free;
  fListStack.Free;
  fBlocksStack.Free;
  fDocBuilder.Free;
  fParaBuilder.Free;
  inherited;
end;

procedure TActiveTextTextRenderer.FinaliseRender;
begin
  OutputParagraph;
end;

procedure TActiveTextTextRenderer.InitialiseRender;
begin
  fParaBuilder.Clear;
  fDocBuilder.Clear;
end;

procedure TActiveTextTextRenderer.OutputParagraph;
var
  LIState: TLIState;
begin
  if fParaBuilder.Length = 0 then
    Exit;
  fDocBuilder.Append(StrOfChar(NBSP, fIndent));
  if fInListItem and not fLIStack.Peek.IsFirstPara then
    // Do we need fInListItem? - test for non-empty list stack?
    // if we do need it, put it on list stack
    fDocBuilder.Append(StrOfChar(NBSP, IndentDelta));
  if fLIStack.Count > 0 then
  begin
    if not fLIStack.Peek.IsFirstPara then
    begin
      fDocBuilder.Append(StrOfChar(NBSP, IndentDelta));
    end
    else
    begin
      // Update item at top of stack
      LIState := fLIStack.Pop;
      LIState.IsFirstPara := False;
      fLIStack.Push(LIState);
    end;
  end;
  fDocBuilder.AppendLine(StrTrimRight(fParaBuilder.ToString));
  fParaBuilder.Clear;
  fInPara := False;
end;

function TActiveTextTextRenderer.Render(ActiveText: IActiveText): string;
var
  Elem: IActiveTextElem;
  TextElem: IActiveTextTextElem;
  ActionElem: IActiveTextActionElem;
begin
  InitialiseRender;
  for Elem in ActiveText do
  begin
    if Supports(Elem, IActiveTextTextElem, TextElem) then
      RenderTextElem(TextElem)
    else if Supports(Elem, IActiveTextActionElem, ActionElem) then
    begin
      if TActiveTextElemCaps.DisplayStyleOf(ActionElem.Kind) = dsBlock then
        RenderBlockActionElem(ActionElem)
      else
        RenderInlineActionElem(ActionElem);
    end;
  end;
  FinaliseRender;
  Result := StrTrimRight(fDocBuilder.ToString);
end;

procedure TActiveTextTextRenderer.RenderBlockActionElem(
  Elem: IActiveTextActionElem);

  procedure OpenListContainer(const ListKind: TListKind);
  begin
    if (fListStack.Count > 0) and (fInPara) then
      OutputParagraph;
    fListStack.Push(TListState.Create(ListKind));
    Inc(fIndent, IndentDelta);
  end;

  procedure AddListMarker(const Marker: string);
  begin
    fParaBuilder.Append(Marker);
    fParaBuilder.Append(StringOfChar(NBSP, IndentDelta - Length(Marker)));
  end;

var
  ListState: TListState;
begin
  case Elem.State of
    fsOpen:
    begin
      fBlocksStack.Push(Elem.Kind);
      case Elem.Kind of
        ekPara, ekHeading, ekBlock:
          {Do nothing} ;
        ekUnorderedList:
          OpenListContainer(lkBullet);
        ekOrderedList:
          OpenListContainer(lkNumber);
        ekListItem:
        begin
          // Update list number of current list
          ListState := fListStack.Pop;
          Inc(ListState.ListNumber, 1);
          fListStack.Push(ListState);
          // Push this list item to list item stack
          fLIStack.Push(TLIState.Create(True));
          // Act depending on current list kind
          case fListStack.Peek.ListKind of
            lkNumber:
              AddListMarker(IntToStr(fListStack.Peek.ListNumber));
            lkBullet:
              AddListMarker(Bullet);
          end;
        end;
      end;
    end;
    fsClose:
    begin
      case Elem.Kind of
        ekPara, ekHeading, ekBlock:
          OutputParagraph;
        ekUnorderedList, ekOrderedList:
        begin
          OutputParagraph;
          fListStack.Pop;
          Dec(fIndent, IndentDelta);
        end;
        ekListItem:
        begin
          OutputParagraph;
          fInListItem := False;
          fLIStack.Pop;
        end;
      end;
      fBlocksStack.Pop;
    end;
  end;
end;

procedure TActiveTextTextRenderer.RenderInlineActionElem(
  Elem: IActiveTextActionElem);
begin
  if not CanEmitInline then
    Exit;
  if (Elem.Kind = ekLink) and (Elem.State = fsClose) and fDisplayURLs then
    RenderURL(Elem);
  // else ignore element: formatting elements have no effect on plain text
end;

procedure TActiveTextTextRenderer.RenderTextElem(Elem: IActiveTextTextElem);
var
  TheText: string;
begin
  if not CanEmitInline then
    Exit;
  TheText := Elem.Text;
  // no white space emitted after block start until 1st non-white space
  // character encountered
  if not fInPara then
    TheText := StrTrimLeft(Elem.Text);
  if TheText = '' then
    Exit;
  AppendToPara(TheText);
end;

procedure TActiveTextTextRenderer.RenderURL(Elem: IActiveTextActionElem);
resourcestring
  sURL = ' (%s)';                     // formatting for URLs from hyperlinks
begin
  Assert(Elem.Kind = ekLink, ClassName + '.RenderURL: Not a link element');
  AppendToPara(Format(sURL, [Elem.Attrs[TActiveTextAttrNames.Link_URL]]));
end;

function TActiveTextTextRenderer.RenderWrapped(ActiveText: IActiveText;
  const PageWidth, LMargin: Cardinal):
  string;
var
  Paras: IStringList;
  Para: string;
  ParaIndent: UInt16;
  WrappedPara: string;
  Offset: Int16;

  // Calculate indent of paragraph by counting LISpacer characters inserted by
  // Render method
  function CalcParaIndent: UInt16;
  var
    Ch: Char;
  begin
    Result := 0;
    for Ch in Para do
    begin
      if Ch <> LISpacer then
        Break;
      Inc(Result);
    end;
  end;

  // Calculate if we are currently processing a list item by detecting Bullet,
  // digits and LISpacer characters inserted by Render method
  function IsListItem: Boolean;
  var
    Remainder: string;
    Digits: string;
    Ch: Char;
  begin
    Result := False;
    // Strip any leading spacer chars from start of para
    Remainder := StrTrimLeftChars(Para, LISpacer);
    // Check for bullet list: starts with bullet character then spacer
    if StrStartsStr(Bullet + LISpacer, Remainder) then
      Exit(True);
    // Check for number list: starts with digit(s) then spacer
    Digits := '';
    for Ch in Remainder do
      if TCharacter.IsDigit(Ch) then
        Digits := Digits + Ch
      else
        Break;
    if (Digits <> '') and
      StrStartsStr(Digits + LISpacer, Remainder) then
      Exit(True);
  end;

begin
  Result := '';
  Paras := TIStringList.Create(Render(ActiveText), EOL, True);
  for Para in Paras do
  begin
    if IsListItem then
    begin
      Offset := -IndentDelta;
      ParaIndent := CalcParaIndent + LMargin + IndentDelta;
    end
    else
    begin
      Offset := 0;
      ParaIndent := CalcParaIndent + LMargin;
    end;
    WrappedPara := StrWrap(
      StrReplace(Para, LISpacer, ' '),
      PageWidth - ParaIndent,
      ParaIndent,
      Offset
    );
    if Result <> '' then
      Result := Result + EOL;
    Result := Result + StrTrimRight(WrappedPara);
  end;
  Result := StrTrimRight(Result);
end;

{ TActiveTextTextRenderer.TListState }

constructor TActiveTextTextRenderer.TListState.Create(AListKind: TListKind);
begin
  ListNumber := 0;
  ListKind := AListKind;
end;

{ TActiveTextTextRenderer.TLIState }

constructor TActiveTextTextRenderer.TLIState.Create(AIsFirstPara: Boolean);
begin
  IsFirstPara := AIsFirstPara;
end;

end.
