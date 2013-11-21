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


unit CS.ActiveText.Renderers.HTML;


interface


uses
  // Delphi
  SysUtils,
  Graphics,
  Generics.Collections,
  // Project
  CS.ActiveText,
  UBaseObjects,
  UCSSBuilder,
  UHTMLUtils;


type
  TActiveTextHTMLRenderer = class(TInterfacedObject, IActiveTextRenderer)
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
        procedure Initialise;
      public
        constructor Create;
        procedure Assign(const Other: TCSSStyles);
        property WrapperClass: string read fWrapperClass write fWrapperClass;
        property ElemClasses[Kind: TActiveTextActionElemKind]: string
          read GetElemClass write SetElemClass;
      end;
  strict private
    var
      fCSSStyles: TCSSStyles;
      fBuilder: TStringBuilder;
      fInBlock: Boolean;
    procedure SetCSSStyles(const AStyles: TCSSStyles);
    function GetTagName(const Kind: TActiveTextActionElemKind): string;
    function MakeOpeningTag(const Kind: TActiveTextActionElemKind;
      Attrs: IHTMLAttributes = nil): string;
    function MakeClosingTag(const Kind: TActiveTextActionElemKind): string;
  public
    constructor Create(const Builder: TStringBuilder);
    destructor Destroy; override;
    class function Render(ActiveText: IActiveText;
      const CSSStyles: TCSSStyles = nil): string;
    property CSSStyles: TCSSStyles read fCSSStyles write SetCSSStyles;

    ///  <summary>Called before active text is processed.</summary>
    ///  <remarks>Method of IActiveTextRenderer.</remarks>
    procedure Initialise;

    ///  <summary>Called after last active text element has been processed.
    ///  </summary>
    ///  <remarks>Method of IActiveTextRenderer.</remarks>
    procedure Finalise;

    ///  <summary>Called when plain text should be output.</summary>
    ///  <param name="AText">string. Text to be output.</param>
    ///  <remarks>Method of IActiveTextRenderer.</remarks>
    procedure OutputText(const AText: string);

    ///  <summary>Called at the start of a new block of active text.</summary>
    ///  <param name="Kind">TActiveTextActionElemKind [in] Kind of block being
    ///  opened. This will be an active text element with DisplayStyle =
    ///  dsBlock.</param>
    ///  <remarks>Method of IActiveTextRenderer.</remarks>
    procedure BeginBlock(const Kind: TActiveTextActionElemKind);

    ///  <summary>Called when the current active text block ends.</summary>
    ///  <param name="Kind">TActiveTextActionElemKind [in] Kind of block being
    ///  opened. This will be an active text element with DisplayStyle =
    ///  dsBlock.</param>
    ///  <remarks>Method of IActiveTextRenderer.</remarks>
    procedure EndBlock(const Kind: TActiveTextActionElemKind);

    ///  <summary>Called at the start of a new active text styling element.
    ///  </summary>
    ///  <param name="Kind">TActiveTextActionElemKind [in] Kind of styling
    ///  element. The kind indicates the type of styling to be applied. This
    ///  will be an active text element with DisplayStyle = dsInline that is
    ///  not ekLink.</param>
    ///  <remarks>Method of IActiveTextRenderer.</remarks>
    procedure BeginInlineStyle(const Kind: TActiveTextActionElemKind);

    ///  <summary>Called the current active text styling element ends.</summary>
    ///  <param name="Kind">TActiveTextActionElemKind [in] Kind of styling
    ///  element. The kind indicates the type of styling to be applied. This
    ///  will be an active text element with DisplayStyle = dsInline that is
    ///  not ekLink.</param>
    ///  <remarks>Method of IActiveTextRenderer.</remarks>
    procedure EndInlineStyle(const Kind: TActiveTextActionElemKind);

    ///  <summary>Called at the start of a new active text link element.
    ///  </summary>
    ///  <param name="URL">string. URL to accessed from the link.</param>
    ///  <remarks>Method of IActiveTextRenderer.</remarks>
    procedure BeginLink(const URL: string);

    ///  <summary>Called when the current active text link element ends.
    ///  </summary>
    ///  <param name="URL">string. URL to accessed from the link.</param>
    ///  <remarks>Method of IActiveTextRenderer.</remarks>
    procedure EndLink(const URL: string);
  end;


implementation


uses
  // Project
  UColours,
  UCSSUtils,
  UFontHelper,
  UIStringList,
  UStrUtils;


{ TActiveTextHTMLRenderer }

procedure TActiveTextHTMLRenderer.BeginBlock(
  const Kind: TActiveTextActionElemKind);
begin
  fBuilder.Append(MakeOpeningTag(Kind));
  fInBlock := True;
end;

procedure TActiveTextHTMLRenderer.BeginInlineStyle(
  const Kind: TActiveTextActionElemKind);
begin
  if not fInBlock then
    Exit;
  fBuilder.Append(MakeOpeningTag(Kind));
end;

procedure TActiveTextHTMLRenderer.BeginLink(const URL: string);
begin
  if not fInBlock then
    Exit;
  fBuilder.Append(MakeOpeningTag(ekLink, THTMLAttributes.Create('href', URL)));
end;

constructor TActiveTextHTMLRenderer.Create(const Builder: TStringBuilder);
begin
  Assert(Assigned(Builder), ClassName + '.Create: Builder is nil');
  inherited Create;
  fCSSStyles := TCSSStyles.Create;
  fBuilder := Builder;
end;

destructor TActiveTextHTMLRenderer.Destroy;
begin
  fCSSStyles.Free;
  inherited;
end;

procedure TActiveTextHTMLRenderer.EndBlock(
  const Kind: TActiveTextActionElemKind);
begin
  fInBlock := False;
  fBuilder.Append(MakeClosingTag(Kind));
end;

procedure TActiveTextHTMLRenderer.EndInlineStyle(
  const Kind: TActiveTextActionElemKind);
begin
  if not fInBlock then
    Exit;
  fBuilder.Append(MakeClosingTag(Kind));
end;

procedure TActiveTextHTMLRenderer.EndLink(const URL: string);
begin
  if not fInBlock then
    Exit;
  fBuilder.Append(MakeClosingTag(ekLink));
end;

procedure TActiveTextHTMLRenderer.Finalise;
begin
  fBuilder.Append(THTML.ClosingTag('div'));
end;

function TActiveTextHTMLRenderer.GetTagName(
  const Kind: TActiveTextActionElemKind): string;
const
  Tags: array[TActiveTextActionElemKind] of string = (
    'a', 'strong', 'em', 'var', 'p', 'span', 'h2', 'code'
  );
begin
  Result := Tags[Kind];
end;

procedure TActiveTextHTMLRenderer.Initialise;
var
  WrapperClassAttr: IHTMLAttributes;
begin
  if fCSSStyles.WrapperClass <> '' then
    WrapperClassAttr := THTMLAttributes.Create('class', fCSSStyles.WrapperClass)
  else
    WrapperClassAttr := nil;
  fBuilder.Append(THTML.OpeningTag('div', WrapperClassAttr));
end;

function TActiveTextHTMLRenderer.MakeClosingTag(
  const Kind: TActiveTextActionElemKind): string;
begin
  Result := THTML.ClosingTag(GetTagName(Kind));
end;

function TActiveTextHTMLRenderer.MakeOpeningTag(
  const Kind: TActiveTextActionElemKind; Attrs: IHTMLAttributes): string;
var
  AllAttrs: IHTMLAttributes;
begin
  AllAttrs := THTMLAttributes.Create;
  if fCSSStyles.ElemClasses[Kind] <> EmptyStr then
    AllAttrs.Add('class', fCSSStyles.ElemClasses[Kind]);
  AllAttrs.Append(Attrs);
  Result := THTML.OpeningTag(GetTagName(Kind), AllAttrs);
end;

procedure TActiveTextHTMLRenderer.OutputText(const AText: string);
begin
  if not fInBlock then
    Exit;
  fBuilder.Append(THTML.Entities(AText));
end;

class function TActiveTextHTMLRenderer.Render(ActiveText: IActiveText;
  const CSSStyles: TCSSStyles): string;
var
  SB: TStringBuilder;
  Renderer: TActiveTextHTMLRenderer;
begin
  SB := TStringBuilder.Create;
  try
    // Renderer will be freed automatically since it is reference counted and
    // passed to ActiveText.Render via its IActiveTextRenderer interface.
    Renderer := TActiveTextHTMLRenderer.Create(SB);
    Renderer.CSSStyles := CSSStyles;
    ActiveText.Render(Renderer);
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

procedure TActiveTextHTMLRenderer.SetCSSStyles(const AStyles: TCSSStyles);
begin
  fCSSStyles.Assign(AStyles)
end;

{ TActiveTextHTMLRenderer.TCSSStyles }

procedure TActiveTextHTMLRenderer.TCSSStyles.Assign(const Other: TCSSStyles);
var
  Kind: TActiveTextActionElemKind;
begin
  if Assigned(Other) then
  begin
    fWrapperClass := Other.fWrapperClass;
    for Kind := Low(TActiveTextActionElemKind)
      to High(TActiveTextActionElemKind) do
      fElemClassMap[Kind] := Other.fElemClassMap[Kind];
  end
  else
    Initialise;
end;

constructor TActiveTextHTMLRenderer.TCSSStyles.Create;
begin
  inherited Create;
  Initialise;
end;

function TActiveTextHTMLRenderer.TCSSStyles.GetElemClass(
  ElemKind: TActiveTextActionElemKind): string;
begin
  Result := fElemClassMap[ElemKind];
end;

procedure TActiveTextHTMLRenderer.TCSSStyles.Initialise;
const
  DefaultWrapperClassName = 'active-text';
  DefaultClasses: array[TActiveTextActionElemKind] of string = (
    'external-link', '', '', '', '', 'warning', '', ''
  );
var
  ElemKind: TActiveTextActionElemKind;
begin
  fWrapperClass := DefaultWrapperClassName;
  for ElemKind := Low(TActiveTextActionElemKind)
    to High(TActiveTextActionElemKind) do
    SetElemClass(ElemKind, DefaultClasses[ElemKind]);
end;

procedure TActiveTextHTMLRenderer.TCSSStyles.SetElemClass(
  ElemKind: TActiveTextActionElemKind; const Value: string);
begin
  fElemClassMap[ElemKind] := Value;
end;

end.

