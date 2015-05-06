{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
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
    var
      fElemStyleMap: TActiveTextRTFStyleMap;
      fDisplayURLs: Boolean;
      fURLStyle: TRTFStyle;
      fInBlock: Boolean;
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
  // Project
  SysUtils, Generics.Defaults;


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

constructor TActiveTextRTF.Create;
begin
  inherited Create;
  fElemStyleMap := TActiveTextRTFStyleMap.Create;
  fURLStyle := TRTFStyle.CreateNull;
end;

destructor TActiveTextRTF.Destroy;
begin
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
  fInBlock := False;
  for Elem in ActiveText do
  begin
    if Supports(Elem, IActiveTextTextElem, TextElem) then
      RenderTextElem(TextElem, RTFBuilder)
    else if Supports(Elem, IActiveTextActionElem, ActionElem) then
    begin
      if ActionElem.DisplayStyle = dsBlock then
        RenderBlockActionElem(ActionElem, RTFBuilder)
      else
        RenderInlineActionElem(ActionElem, RTFBuilder);
    end;
  end;
end;

procedure TActiveTextRTF.RenderBlockActionElem(Elem: IActiveTextActionElem;
  const Builder: TRTFBuilder);
begin
  case Elem.State of
    fsOpen:
    begin
      fInBlock := True;
      Builder.BeginGroup;
      Builder.ApplyStyle(fElemStyleMap[Elem.Kind]);
    end;
    fsClose:
    begin
      Builder.EndPara;
      Builder.EndGroup;
      fInBlock := False;
    end;
  end;
end;

procedure TActiveTextRTF.RenderInlineActionElem(Elem: IActiveTextActionElem;
  const Builder: TRTFBuilder);
begin
  if not fInBlock then
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
begin
  if not fInBlock then
    Exit;
  Builder.AddText(Elem.Text);
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

end.

