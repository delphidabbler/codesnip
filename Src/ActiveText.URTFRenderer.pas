{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class and helpers that create RTF representations of active text
 * with customised styling.
}


unit ActiveText.URTFRenderer;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  ActiveText.UMain, UBaseObjects, URTFBuilder, URTFStyles;


type
  TActiveTextRTFStyleMap = class(TObject)
  strict private
    var
      fMap: TDictionary<TActiveTextActionElemKind,TRTFStyle>;
    function GetStyle(ElemKind: TActiveTextActionElemKind): TRTFStyle;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const ElemKind: TActiveTextActionElemKind;
      const Style: TRTFStyle);
    procedure Assign(const Src: TActiveTextRTFStyleMap);
    procedure MakeMonochrome;
    // enumerator enumerates styles, not pair or action element kind
    function GetEnumerator: TEnumerator<TRTFStyle>;
    property Styles[ElemKInd: TActiveTextActionElemKind]: TRTFStyle
      read GetStyle; default;
  end;

type
  TActiveTextRTF = class(TObject)
  strict private
    const
      // Difference between indent levels in twips
      IndentDelta = 360;
      // RTF Bullet character
      Bullet = #$2022;
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
        Prefix: string;
        constructor Create(AIsFirstPara: Boolean; const APrefix: string);
      end;
    var
      fElemStyleMap: TActiveTextRTFStyleMap;
      fDisplayURLs: Boolean;
      fURLStyle: TRTFStyle;
      fBlockStack: TStack<TActiveTextActionElemKind>;
      fListStack: TStack<TListState>;
      fIndentStack: TStack<SmallInt>;
      fLIStack: TStack<TLIState>;
      fIndentLevel: Byte;  // logical indent level
      fInPara: Boolean;
    procedure SetElemStyleMap(const ElemStyleMap: TActiveTextRTFStyleMap);
    procedure Initialise(const Builder: TRTFBuilder);
    procedure RenderTextElem(Elem: IActiveTextTextElem;
      const Builder: TRTFBuilder);
    procedure RenderBlockActionElem(Elem: IActiveTextActionElem;
      const Builder: TRTFBuilder);
    procedure RenderInlineActionElem(Elem: IActiveTextActionElem;
      const Builder: TRTFBuilder);
    procedure RenderURL(Elem: IActiveTextActionElem;
      const Builder: TRTFBuilder);
    function CanEmitInline: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property ElemStyleMap: TActiveTextRTFStyleMap
      read fElemStyleMap write SetElemStyleMap;
    property DisplayURLs: Boolean read fDisplayURLs write fDisplayURLs;
    property URLStyle: TRTFStyle read fURLStyle write fURLStyle;
    procedure Render(ActiveText: IActiveText;
      const RTFBuilder: TRTFBuilder);
  end;


implementation


uses
  // Delphi
  SysUtils, Generics.Defaults,
  // Project
  UConsts, UStrUtils;


{ TActiveTextRTFStyleMap }

procedure TActiveTextRTFStyleMap.Add(const ElemKind: TActiveTextActionElemKind;
  const Style: TRTFStyle);
begin
  Assert(not fMap.ContainsKey(ElemKind),
    ClassName + '.Add: ElemKind already in map');
  fMap.Add(ElemKind, Style);
end;

procedure TActiveTextRTFStyleMap.Assign(const Src: TActiveTextRTFStyleMap);
var
  Entry: TPair<TActiveTextActionElemKind, TRTFStyle>;
begin
  fMap.Clear;
  for Entry in Src.fMap do
    fMap.Add(Entry.Key, Entry.Value);
end;

constructor TActiveTextRTFStyleMap.Create;
begin
  inherited Create;
  fMap := TDictionary<TActiveTextActionElemKind,TRTFStyle>.Create(
    TDelegatedEqualityComparer<TActiveTextActionElemKind>.Create(
      function (const Left, Right: TActiveTextActionElemKind): Boolean
      begin
        Result := Left = Right;
      end,
      function (const ElemKind: TActiveTextActionElemKind): Integer
      begin
        Result := Ord(ElemKind);
      end
    )
  )
end;

destructor TActiveTextRTFStyleMap.Destroy;
begin
  fMap.Free;
  inherited;
end;

function TActiveTextRTFStyleMap.GetEnumerator: TEnumerator<TRTFStyle>;
begin
  Result := fMap.Values.GetEnumerator;
end;

function TActiveTextRTFStyleMap.GetStyle(ElemKind: TActiveTextActionElemKind):
  TRTFStyle;
begin
  if fMap.ContainsKey(ElemKind) then
    Result := fMap[ElemKind]
  else
    Result := TRTFStyle.CreateNull;
end;

procedure TActiveTextRTFStyleMap.MakeMonochrome;
var
  Style: TRTFStyle;
  Kind: TActiveTextActionElemKind;
begin
  for Kind := Low(TActiveTextActionElemKind)
    to High(TActiveTextActionElemKind) do
  begin
    if fMap.ContainsKey(Kind) then
    begin
      Style := fMap[Kind];
      Style.MakeMonochrome;
      fMap[Kind] := Style;
    end;
  end;
end;

{ TActiveTextRTF }

function TActiveTextRTF.CanEmitInline: Boolean;
begin
  if fBlockStack.Count <= 0 then
    Exit(False);
  Result := TActiveTextElemCaps.CanContainText(fBlockStack.Peek);
end;

constructor TActiveTextRTF.Create;
begin
  inherited Create;
  fElemStyleMap := TActiveTextRTFStyleMap.Create;
  fURLStyle := TRTFStyle.CreateNull;
  fBlockStack := TStack<TActiveTextActionElemKind>.Create;
  fListStack := TStack<TListState>.Create;
  fIndentStack := TStack<SmallInt>.Create;
  fLIStack := TStack<TLIState>.Create;
  fIndentLevel := 0;
  fInPara := False;
end;

destructor TActiveTextRTF.Destroy;
begin
  fLIStack.Free;
  fIndentStack.Free;
  fListStack.Free;
  fBlockStack.Free;
  fElemStyleMap.Free;
  inherited;
end;

procedure TActiveTextRTF.Initialise(const Builder: TRTFBuilder);
var
  Style: TRTFStyle;
begin
  for Style in fElemStyleMap do
  begin
    Builder.FontTable.AddFromStyle(Style);
    Builder.ColourTable.AddFromStyle(Style);
  end;
  Builder.FontTable.AddFromStyle(fURLStyle);
  Builder.ColourTable.AddFromStyle(fURLStyle);
end;

procedure TActiveTextRTF.Render(ActiveText: IActiveText;
  const RTFBuilder: TRTFBuilder);
var
  Elem: IActiveTextElem;
  TextElem: IActiveTextTextElem;
  ActionElem: IActiveTextActionElem;
begin
  Initialise(RTFBuilder);
  for Elem in ActiveText do
  begin
    if Supports(Elem, IActiveTextTextElem, TextElem) then
      RenderTextElem(TextElem, RTFBuilder)
    else if Supports(Elem, IActiveTextActionElem, ActionElem) then
    begin
      if TActiveTextElemCaps.DisplayStyleOf(ActionElem.Kind) = dsBlock then
        RenderBlockActionElem(ActionElem, RTFBuilder)
      else
        RenderInlineActionElem(ActionElem, RTFBuilder);
    end;
  end;
end;

procedure TActiveTextRTF.RenderBlockActionElem(Elem: IActiveTextActionElem;
  const Builder: TRTFBuilder);

  procedure OpenListContainer(const ListKind: TListKind);
  begin
    fListStack.Push(TListState.Create(ListKind));
    Inc(fIndentLevel);
    Builder.BeginGroup;
  end;

  function IndentTwips: SmallInt;
  begin
    Result := fElemStyleMap[ekListItem].IndentLevelToTwips(fIndentLevel)
  end;

var
  ListState: TListState;
  LIState: TLIState;
  Style: TRTFStyle;
begin
  case Elem.State of
    fsOpen:
    begin
      fInPara := False;
      fBlockStack.Push(Elem.Kind);
      case Elem.Kind of
        ekPara, ekHeading, ekBlock:
        begin
          Builder.BeginGroup;
          Style := fElemStyleMap[Elem.Kind];
          if fLIStack.Count > 0 then
          begin
            Builder.SetTabStops([IndentTwips]);
            if fLIStack.Peek.IsFirstPara then
            begin
              Builder.SetIndents(
                IndentTwips, -fElemStyleMap[ekListItem].IndentDelta
              );
              if (fListStack.Count > 0) then
              begin
                if fListStack.Peek.ListNumber = 1 then
                begin
                  Style.Capabilities := Style.Capabilities + [scParaSpacing];
                  if fListStack.Peek.ListKind = lkNumber then
                    Style.ParaSpacing := TRTFParaSpacing.Create(
                      fElemStyleMap[ekOrderedList].ParaSpacing.Before, 0.0
                    )
                  else
                    Style.ParaSpacing := TRTFParaSpacing.Create(
                      fElemStyleMap[ekUnorderedList].ParaSpacing.Before, 0.0
                    )
                end
                else if fListStack.Peek.ListNumber > 1 then
                begin
                  if Elem.Kind = ekHeading then
                  begin
                    Style.Capabilities := Style.Capabilities + [scParaSpacing];
                    Style.ParaSpacing := fElemStyleMap[ekPara].ParaSpacing;
                  end;
                end;
              end;
              Builder.ApplyStyle(Style);
              Builder.AddText(fLIStack.Peek.Prefix);
              Builder.AddText(TAB);
              fInPara := True;
            end
            else
            begin
              Builder.ApplyStyle(Style);
              Builder.SetIndents(IndentTwips, 0);
            end;
          end
          else
          begin
            Builder.ApplyStyle(Style);
            Builder.SetIndents(IndentTwips, 0);
          end;
        end;
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
          Builder.BeginGroup;
          Builder.ApplyStyle(fElemStyleMap[Elem.Kind]);
          case fListStack.Peek.ListKind of
            lkNumber:
            begin
              fLIStack.Push(
                TLIState.Create(
                  True, IntToStr(fListStack.Peek.ListNumber) + '.'
                )
              );
            end;
            lkBullet:
            begin
              fLIStack.Push(TLIState.Create(True, Bullet));
            end;
          end;
          Builder.ClearParaFormatting;
        end;
      end;
    end;
    fsClose:
    begin
      case Elem.Kind of
        ekPara, ekHeading, ekBlock:
        begin
          if (fLIStack.Count > 0) and (fLIStack.Peek.IsFirstPara) then
          begin
            // Update item at top of LI stack to record not first para
            LIState := fLIStack.Pop;
            LIState.IsFirstPara := False;
            fLIStack.Push(LIState);
          end;
          if fInPara then
            Builder.EndPara;
          Builder.EndGroup;
        end;
        ekUnorderedList, ekOrderedList:
        begin
          if fInPara then
            Builder.EndPara;
          Builder.EndGroup;
          fListStack.Pop;
          Dec(fIndentLevel);
        end;
        ekListItem:
        begin
          if fInPara then
            Builder.EndPara;
          Builder.EndGroup;
          fLIStack.Pop;
        end;
      end;
      fBlockStack.Pop;
      fInPara := False;
    end;
  end;
end;

procedure TActiveTextRTF.RenderInlineActionElem(Elem: IActiveTextActionElem;
  const Builder: TRTFBuilder);
begin
  if not CanEmitInline then
    Exit;
  case Elem.State of
    fsOpen:
    begin
      Builder.BeginGroup;
      Builder.ApplyStyle(fElemStyleMap[Elem.Kind]);
    end;
    fsClose:
    begin
      if (Elem.Kind = ekLink) and fDisplayURLs then
        RenderURL(Elem, Builder);
      Builder.EndGroup;
    end;
  end;
end;

procedure TActiveTextRTF.RenderTextElem(Elem: IActiveTextTextElem;
  const Builder: TRTFBuilder);
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
  Builder.AddText(TheText);
  fInPara := True;
end;

procedure TActiveTextRTF.RenderURL(Elem: IActiveTextActionElem;
  const Builder: TRTFBuilder);
resourcestring
  sURL = ' (%s)';                     // formatting for URLs from hyperlinks
begin
  Assert(Elem.Kind = ekLink, ClassName + '.RenderURL: Not a link element');
  Builder.BeginGroup;
  Builder.ApplyStyle(URLStyle);
  Builder.AddText(
    Format(sURL, [Elem.Attrs[TActiveTextAttrNames.Link_URL]])
  );
  Builder.EndGroup;
end;

procedure TActiveTextRTF.SetElemStyleMap(
  const ElemStyleMap: TActiveTextRTFStyleMap);
begin
  fElemStyleMap.Assign(ElemStyleMap);
end;

{ TActiveTextRTF.TListState }

constructor TActiveTextRTF.TListState.Create(AListKind: TListKind);
begin
  ListNumber := 0;
  ListKind := AListKind;
end;

{ TActiveTextRTF.TLIState }

constructor TActiveTextRTF.TLIState.Create(AIsFirstPara: Boolean;
  const APrefix: string);
begin
  IsFirstPara := AIsFirstPara;
  Prefix := APrefix;
end;

end.

