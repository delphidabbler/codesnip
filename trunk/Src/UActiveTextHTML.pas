{
 * UActiveTextHTML.pas
 *
 * Static class that provides assistance when rendering active text as HTML.
 * The class renders the active text as HTML and provides CSS to style it.
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
 * The Original Code is UActiveTextHTML.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributors:
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UActiveTextHTML;


interface


uses
  // Delphi
  SysUtils, Graphics, Generics.Collections,
  // Project
  UActiveText, UBaseObjects, UCSSBuilder, UHTMLUtils;


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
  UColours, UCSSUtils, UFontHelper, UHTMLDetailUtils, UIStringList;


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
  fBuilder.AppendLine(MakeTag('div', ttClose));
end;

procedure TActiveTextHTML.InitialiseRender;
var
  WrapperClassAttr: IHTMLAttributes;
begin
  if fCSSStyles.WrapperClass <> '' then
    WrapperClassAttr := THTMLAttributes.Create('class', fCSSStyles.WrapperClass)
  else
    WrapperClassAttr := nil;
  fBuilder.AppendLine(MakeTag('div', ttOpen, WrapperClassAttr));
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
      Result := RollOverHintAttrs(
        Elem.Attrs[TActiveTextAttrNames.Link_URL]
      );
      Result.Add('href', Elem.Attrs[TActiveTextAttrNames.Link_URL]);
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
  Result := MakeTag(fTagInfoMap[Elem.Kind].Name, ttClose);
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
  Result := MakeTag(fTagInfoMap[Elem.Kind].Name, ttOpen, Attrs);
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
  fBuilder.Append(MakeSafeHTMLText(Elem.Text));
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

