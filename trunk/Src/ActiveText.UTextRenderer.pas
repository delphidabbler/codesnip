{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements class that renders active text as plain text in fixed width, word
 * wrapped paragraphs.
}


unit ActiveText.UTextRenderer;

interface

uses
  SysUtils,
  UActiveText;

type
  TActiveTextTextRenderer = class(TObject)
  strict private
    var
      fDisplayURLs: Boolean;
      fLineWidth: Cardinal;
      fLineIndent: Cardinal;
      fSpaceParas: Boolean;
      fInBlock: Boolean;
      fParaBuilder: TStringBuilder;
      fDocBuilder: TStringBuilder;
    procedure InitialiseRender;
    procedure FinaliseRender;
    procedure FormatParagraph;
    procedure RenderTextElem(Elem: IActiveTextTextElem);
    procedure RenderBlockActionElem(Elem: IActiveTextActionElem);
    procedure RenderInlineActionElem(Elem: IActiveTextActionElem);
    procedure RenderURL(Elem: IActiveTextActionElem);
  public
    constructor Create;
    destructor Destroy; override;
    property DisplayURLs: Boolean read fDisplayURLs write fDisplayURLs;
    property LineWidth: Cardinal read fLineWidth write fLineWidth
      default 80;
    property LineIndent: Cardinal read fLineIndent write fLineIndent
      default 0;
    property SpaceParas: Boolean read fSpaceParas write fSpaceParas
      default False;
    function Render(ActiveText: IActiveText): string;
  end;


implementation

uses
  UStrUtils;

{ TActiveTextTextRenderer }

constructor TActiveTextTextRenderer.Create;
begin
  inherited Create;
  fParaBuilder := TStringBuilder.Create;
  fDocBuilder := TStringBuilder.Create;
  fLineWidth := 80;
  fLineIndent := 0;
  fSpaceParas := False;
end;

destructor TActiveTextTextRenderer.Destroy;
begin
  fDocBuilder.Free;
  fParaBuilder.Free;
  inherited;
end;

procedure TActiveTextTextRenderer.FinaliseRender;
begin
  FormatParagraph;
end;

procedure TActiveTextTextRenderer.FormatParagraph;
begin
  if fParaBuilder.Length = 0 then
    Exit;
  fDocBuilder.AppendLine(
    StrWrap(StrTrimRight(fParaBuilder.ToString), fLineWidth, fLineIndent)
  );
  if fSpaceParas then
    fDocBuilder.AppendLine;
  fParaBuilder.Clear;
end;

procedure TActiveTextTextRenderer.InitialiseRender;
begin
  fParaBuilder.Clear;
  fDocBuilder.Clear;
end;

function TActiveTextTextRenderer.Render(ActiveText: IActiveText): string;
var
  Elem: IActiveTextElem;
  TextElem: IActiveTextTextElem;
  ActionElem: IActiveTextActionElem;
begin
  InitialiseRender;
  fInBlock := False;
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
  Result := StrTrimRight(fDocBuilder.ToString);
end;

procedure TActiveTextTextRenderer.RenderBlockActionElem(
  Elem: IActiveTextActionElem);
begin
  case Elem.State of
    fsOpen:
    begin
      fInBlock := True;
    end;
    fsClose:
    begin
      FormatParagraph;
      fInBlock := False;
    end;
  end;
end;

procedure TActiveTextTextRenderer.RenderInlineActionElem(
  Elem: IActiveTextActionElem);
begin
  if not fInBlock then
    Exit;
  if (Elem.Kind = ekLink) and (Elem.State = fsClose) and fDisplayURLs then
    RenderURL(Elem);
end;

procedure TActiveTextTextRenderer.RenderTextElem(Elem: IActiveTextTextElem);
begin
  if not fInBlock then
    Exit;
  fParaBuilder.Append(Elem.Text);
end;

procedure TActiveTextTextRenderer.RenderURL(Elem: IActiveTextActionElem);
resourcestring
  sURL = ' (%s)';                     // formatting for URLs from hyperlinks
begin
  Assert(Elem.Kind = ekLink, ClassName + '.RenderURL: Not a link element');
  fParaBuilder.AppendFormat(sURL, [Elem.Attrs[TActiveTextAttrNames.Link_URL]]);
end;

end.
