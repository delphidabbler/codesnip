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
 * Provides a class that can render active text as plain text.
}


unit CS.ActiveText.Renderers.PlainText;

interface

uses
  // Delphi
  SysUtils,
  // Project
  ActiveText.UMain,
  UIStringList;

type
  TActiveTextPlainTextRenderer = class(TInterfacedObject, IActiveTextRenderer)
  public
    type
      TOption = (
        ptrIgnoreEmptyBlocks, ptrTrimLines, ptrIgnoreInterBlockText,
        ptrIncludeURLs
      );
      TOptions = set of TOption;
  strict private
    const
      URLOpenParen = '(';
      URLCloseParan = ')';
    var
      fBuilder: TStringBuilder;
      fBlockSeparator: string;
      fOptions: TOptions;
      fIsFirstBlock: Boolean;
      fInBlock: Boolean;
      fParaBuilder: TStringBuilder;
      fParas: IStringList;
    procedure OutputParagraph;
  public
    constructor Create(const Builder: TStringBuilder;
      const BlockSeparator: string; const Options: TOptions);
    destructor Destroy; override;
    class function Render(ActiveText: IActiveText; const BlockSeparator: string;
      const Options: TOptions): string;

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
  UStrUtils;

{ TActiveTextPlainTextRenderer }

procedure TActiveTextPlainTextRenderer.BeginBlock(
  const Kind: TActiveTextActionElemKind);
begin
  if fParaBuilder.Length > 0 then
  begin
    if not fInBlock and not (ptrIgnoreInterBlockText in fOptions) then
      OutputParagraph // this clears fParaBuilder
    else
      fParaBuilder.Clear;
  end;
  fInBlock := True;
end;

procedure TActiveTextPlainTextRenderer.BeginInlineStyle(
  const Kind: TActiveTextActionElemKind);
begin
  // do nothing
end;

procedure TActiveTextPlainTextRenderer.BeginLink(const URL: string);
begin
  // do nothing
end;

constructor TActiveTextPlainTextRenderer.Create(const Builder: TStringBuilder;
  const BlockSeparator: string; const Options: TOptions);
begin
  Assert(Assigned(Builder), ClassName + '.Create: Builder is nil');
  Assert(StrIsBlank(BlockSeparator),
    ClassName + '.Create: BlockSeparator contains non-whitespace characters');
  inherited Create;
  fBuilder := Builder;
  fBlockSeparator := BlockSeparator;
  fOptions := Options;
  fParas := TIStringList.Create;
  fParaBuilder := TStringBuilder.Create;
end;

destructor TActiveTextPlainTextRenderer.Destroy;
begin
  fParaBuilder.Free;
  inherited;
end;

procedure TActiveTextPlainTextRenderer.EndBlock(
  const Kind: TActiveTextActionElemKind);
begin
  OutputParagraph;
  fInBlock := False;
end;

procedure TActiveTextPlainTextRenderer.EndInlineStyle(
  const Kind: TActiveTextActionElemKind);
begin
  // do nothing
end;

procedure TActiveTextPlainTextRenderer.EndLink(const URL: string);
begin
  if ptrIncludeURLs in fOptions then
    fParaBuilder.Append(URLOpenParen + URL + URLCloseParan);
end;

procedure TActiveTextPlainTextRenderer.Finalise;
begin
  if fInBlock
    or (
      (fParaBuilder.Length > 0) and not (ptrIgnoreInterBlockText in fOptions)
    ) then
    OutputParagraph;
  fBuilder.Append(
    fParas.GetText(fBlockSeparator, not (ptrIgnoreEmptyBlocks in fOptions))
  )
end;

procedure TActiveTextPlainTextRenderer.Initialise;
begin
  fIsFirstBlock := True;
  fInBlock := False;
  fBuilder.Clear;
  fParas.Clear;
  fParaBuilder.Clear;
end;

procedure TActiveTextPlainTextRenderer.OutputParagraph;
begin
  if ptrTrimLines in fOptions then
    fParas.Add(StrTrim(fParaBuilder.ToString))
  else
    fParas.Add(fParaBuilder.ToString);
  fParaBuilder.Clear;
end;

procedure TActiveTextPlainTextRenderer.OutputText(const AText: string);
begin
  fParaBuilder.Append(AText);
end;

class function TActiveTextPlainTextRenderer.Render(ActiveText: IActiveText;
  const BlockSeparator: string; const Options: TOptions): string;
var
  Builder: TStringBuilder;
  Renderer: TActiveTextPlainTextRenderer;
begin
  Builder := TStringBuilder.Create;
  try
    Renderer := Create(Builder, BlockSeparator, Options);
    ActiveText.Render(Renderer);
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

end.
