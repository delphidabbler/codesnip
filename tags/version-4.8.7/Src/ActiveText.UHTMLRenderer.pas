{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Provides a class that renders active text as HTML.
}


unit ActiveText.UHTMLRenderer;


interface


uses
  // Delphi
  SysUtils, Graphics, Generics.Collections,
  // Project
  ActiveText.UMain, UBaseObjects, UCSSBuilder, UHTMLUtils;


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
          fWrapperClass: string;
          fElemClassMap: array[TActiveTextActionElemKind] of string;
        procedure SetElemClass(ElemKind: TActiveTextActionElemKind;
          const Value: string); inline;
        function GetElemClass(ElemKind: TActiveTextActionElemKind): string;
          inline;
      public
        constructor Create;
        property WrapperClass: string read fWrapperClass write fWrapperClass;
        property ElemClasses[Kind: TActiveTextActionElemKind]: string
          read GetElemClass write SetElemClass;
      end;
  strict private
    var
      fCSSStyles: TCSSStyles;
      fBuilder: TStringBuilder;
      fInBlock: Boolean;
      fTagInfoMap: TTagInfoMap;
    procedure InitialiseTagInfoMap;
    procedure InitialiseRender;
    procedure RenderTextElem(Elem: IActiveTextTextElem);
    procedure RenderBlockActionElem(Elem: IActiveTextActionElem);
    procedure RenderInlineActionElem(Elem: IActiveTextActionElem);
    procedure FinaliseRender;
    function MakeOpeningTag(const Elem: IActiveTextActionElem): string;
    function MakeClosingTag(const Elem: IActiveTextActionElem): string;
  public
    constructor Create;
    destructor Destroy; override;
    function Render(ActiveText: IActiveText): string;
    property Styles: TCSSStyles read fCSSStyles;
  end;


implementation


uses
  // Project
  UColours, UCSSUtils, UFontHelper, UIStringList;


{ TActiveTextHTML }

constructor TActiveTextHTML.Create;
begin
  inherited Create;
  fCSSStyles := TCSSStyles.Create;
  fBuilder := TStringBuilder.Create;
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

procedure TActiveTextHTML.FinaliseRender;
begin
  fBuilder.AppendLine(THTML.ClosingTag('div'));
end;

procedure TActiveTextHTML.InitialiseRender;
var
  WrapperClassAttr: IHTMLAttributes;
begin
  if fCSSStyles.WrapperClass <> '' then
    WrapperClassAttr := THTMLAttributes.Create('class', fCSSStyles.WrapperClass)
  else
    WrapperClassAttr := nil;
  fBuilder.AppendLine(THTML.OpeningTag('div', WrapperClassAttr));
end;

procedure TActiveTextHTML.InitialiseTagInfoMap;
var
  NullAttrs: TTagInfo.TTagAttrCallback;
  LinkAttrs: TTagInfo.TTagAttrCallback;
  AttrFn: TTagInfo.TTagAttrCallback;
  ElemKind: TActiveTextActionElemKind;
const
  Tags: array[TActiveTextActionElemKind] of string = (
    'a', 'strong', 'em', 'var', 'p', 'span', 'h2', 'code'
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
  Elem: IActiveTextElem;
  TextElem: IActiveTextTextElem;
  ActionElem: IActiveTextActionElem;
begin
  fBuilder.Clear;
  fInBlock := False;
  InitialiseRender;
  for Elem in ActiveText do
  begin
    if Supports(Elem, IActiveTextTextElem, TextElem) then
      RenderTextElem(TextElem)
    else if Supports(Elem, IActiveTextActionElem, ActionElem) then
    begin
      if ActionElem.DisplayStyle = dsBlock then
        RenderBlockActionElem(ActionElem)
      else
        RenderInlineActionElem(ActionElem);
    end;
  end;
  FinaliseRender;
  Result := fBuilder.ToString;
end;

procedure TActiveTextHTML.RenderBlockActionElem(Elem: IActiveTextActionElem);
begin
  case Elem.State of
    fsOpen:
    begin
      fBuilder.Append(MakeOpeningTag(Elem));
      fInBlock := True;
    end;
    fsClose:
    begin
      fInBlock := False;
      fBuilder.AppendLine(MakeClosingTag(Elem));
    end;
  end;
end;

procedure TActiveTextHTML.RenderInlineActionElem(Elem: IActiveTextActionElem);
begin
  if not fInBlock then
    Exit;
  case Elem.State of
    fsOpen:
      fBuilder.Append(MakeOpeningTag(Elem));
    fsClose:
      fBuilder.Append(MakeClosingTag(Elem));
  end;
end;

procedure TActiveTextHTML.RenderTextElem(Elem: IActiveTextTextElem);
begin
  if not fInBlock then
    Exit;
  fBuilder.Append(THTML.Entities(Elem.Text));
end;

{ TActiveTextHTML.TCSSStyles }

constructor TActiveTextHTML.TCSSStyles.Create;
const
  DefaultClasses: array[TActiveTextActionElemKind] of string = (
    'external-link', '', '', '', '', 'warning', '', ''
  );
var
  ElemKind: TActiveTextActionElemKind;
begin
  inherited Create;
  fWrapperClass := 'active-text';
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

