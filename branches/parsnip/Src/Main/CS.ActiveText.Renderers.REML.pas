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
 * Provides an object that can be passed to IActiveText.Render to render active
 * text as REML code.
}


unit CS.ActiveText.Renderers.REML;


interface


uses
  // Delphi
  SysUtils,
  // Project
  ActiveText.UMain,
  UREMLDataIO;


type
  ///  <summary>Object that can be used to render active text as REML code.
  ///  </summary>
  ///  <remarks>Designed for use with the IActiveText.Render method.</remarks>
  TActiveTextREMLRenderer = class(TInterfacedObject, IActiveTextRenderer)
  strict private
    var
      ///  <summary>Object used to construct string of REML.</summary>
      fBuilder: TStringBuilder;
      ///  <summary>Any whitespace to be appended to the end of REML blocks.
      ///  </summary>
      fBlockTerminator: string;

    ///  <summary>Converts the given plain text to REML compatible text.
    ///  </summary>
    ///  <remarks>Illegal REML characters are converted to the equivalent
    ///  character entity.</remarks>
    function TextToREMLText(const Text: string): string;

    ///  <summary>Adds the REML tag to output that corresponds to the given kind
    ///  of active text element.</summary>
    ///  <param name="Kind">TActiveTextActionElemKind [in] Kind of active text
    ///  element for which REML tag is required.</param>
    ///  <param name="State">TREMLTagState [in] Required state of tag: opening
    ///  or closing.</param>
    ///  <param name="Attr">TREMLAttr [in] Attribute name/value pair to be
    ///  included in REML tag. Ignored if Attr is null or if State = rtsClose.
    ///  </param>
    procedure EmitREMLTagFor(const Kind: TActiveTextActionElemKind;
      const State: TREMLTagState; const Attr: TREMLAttr); overload;

    ///  <summary>Adds the REML tag to output that corresponds to the given kind
    ///  of active text element.</summary>
    ///  <param name="Kind">TActiveTextActionElemKind [in] Kind of active text
    ///  element for which REML tag is required.</param>
    ///  <param name="State">TREMLTagState [in] Required state of tag: opening
    ///  or closing.</param>
    ///  <remarks>This form of the method is for use when the required tag has
    ///  no attribute.</remarks>
    procedure EmitREMLTagFor(const Kind: TActiveTextActionElemKind;
      const State: TREMLTagState); overload;

  public

    ///  <summary>Constructs a new renderer that stores REML in given string
    ///  builder object.</summary>
    ///  <param name="Builder">TStringBuilder [in] Object that receives
    ///  rendered REML code.</param>
    ///  <param name="BlockTerminator">string [in] Any text used to terminate
    ///  REML blocks. This can be either the empty string or any white space.
    ///  </param>
    constructor Create(const Builder: TStringBuilder;
      const BlockTerminator: string);

    ///  <summary>Renders active text as REML source code.</summary>
    ///  <param name="ActiveText">IActiveText [in] Active text to be rendered.
    ///  </param>
    ///  <param name="BlockTerminator">string [in] Any text used to terminate
    ///  REML blocks. This can be either the empty string or any white space.
    ///  </param>
    ///  <returns>string. REML representation of the active text.</returns>
    ///  <remarks>This is a helper method encapsulating a common use for this
    ///  renderer.</remarks>
    class function Render(ActiveText: IActiveText;
      const BlockTerminator: string): string;

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
  UConsts,
  UExceptions,
  UStrUtils;


{ TActiveTextREMLRenderer }

procedure TActiveTextREMLRenderer.BeginBlock(
  const Kind: TActiveTextActionElemKind);
begin
  EmitREMLTagFor(Kind, rtsOpen);
end;

procedure TActiveTextREMLRenderer.BeginInlineStyle(
  const Kind: TActiveTextActionElemKind);
begin
  EmitREMLTagFor(Kind, rtsOpen);
end;

procedure TActiveTextREMLRenderer.BeginLink(const URL: string);
var
  ParamName: string;
begin
  if not TREMLTags.LookupParamName(rtLink, ParamName) then
    raise EBug.Create(
      ClassName + '.BeginLink: No parameter name found for REML link tag'
    );
  EmitREMLTagFor(ekLink, rtsOpen, TREMLAttr.Create(ParamName, URL));
end;

constructor TActiveTextREMLRenderer.Create(const Builder: TStringBuilder;
  const BlockTerminator: string);
begin
  Assert(Assigned(Builder), ClassName + '.Create: Builder is nil');
  Assert(StrIsBlank(BlockTerminator),
    ClassName + '.Create: BlockTerminator contains non-whitespace characters');
  inherited Create;
  fBuilder := Builder;
  fBlockTerminator := BlockTerminator;
end;

procedure TActiveTextREMLRenderer.EmitREMLTagFor(
  const Kind: TActiveTextActionElemKind; const State: TREMLTagState);
begin
  EmitREMLTagFor(Kind, State, TREMLAttr.CreateNull);
end;

procedure TActiveTextREMLRenderer.EmitREMLTagFor(
  const Kind: TActiveTextActionElemKind; const State: TREMLTagState;
  const Attr: TREMLAttr);
const
  TagIDMap: array[TActiveTextActionElemKind] of TREMLTagID = (
    rtLink, rtStrong, rtEm, rtVar, rtPara, rtWarning, rtHeading, rtMono
  );
  TagStartFmt: array[TREMLTagState] of string = ('<%s', '</%s');
var
  TagName: string;
begin
  if not TREMLTags.LookupTagName(TagIDMap[Kind], TagName) then
    raise EBug.Create(ClassName + '.EmitClosingREMLTagFor: Invalid tag ID');
  fBuilder.AppendFormat(TagStartFmt[State], [TagName]);
  if (State = rtsOpen) and not Attr.IsNull then
  begin
    fBuilder.Append(' ');
    fBuilder.Append(TextToREMLText(Attr.Key));
    fBuilder.Append('=');
    fBuilder.Append(DOUBLEQUOTE);
    fBuilder.Append(TextToREMLText(Attr.Value));
    fBuilder.Append(DOUBLEQUOTE);
  end;
  fBuilder.Append('>');
end;

procedure TActiveTextREMLRenderer.EndBlock(
  const Kind: TActiveTextActionElemKind);
begin
  EmitREMLTagFor(Kind, rtsClose);
  if fBlockTerminator <> EmptyStr then
    fBuilder.Append(fBlockTerminator);
end;

procedure TActiveTextREMLRenderer.EndInlineStyle(
  const Kind: TActiveTextActionElemKind);
begin
  EmitREMLTagFor(Kind, rtsClose);
end;

procedure TActiveTextREMLRenderer.EndLink(const URL: string);
begin
  EmitREMLTagFor(ekLink, rtsClose);
end;

procedure TActiveTextREMLRenderer.Finalise;
begin
  // Do nothing
end;

procedure TActiveTextREMLRenderer.Initialise;
begin
  fBuilder.Clear;
end;

procedure TActiveTextREMLRenderer.OutputText(const AText: string);
begin
  fBuilder.Append(TextToREMLText(AText));
end;

class function TActiveTextREMLRenderer.Render(ActiveText: IActiveText;
  const BlockTerminator: string): string;
var
  Builder: TStringBuilder;
  Renderer: TActiveTextREMLRenderer;
begin
  Builder := TStringBuilder.Create;
  try
    Renderer := Create(Builder, BlockTerminator);
    ActiveText.Render(Renderer);
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

function TActiveTextREMLRenderer.TextToREMLText(const Text: string): string;
var
  Ch: Char;         // each character in plain text
  Entity: string;   // stores each required entity
begin
  Result := '';
  for Ch in Text do
  begin
    Entity := TREMLEntities.MapToEntity(Ch);
    if Entity = '' then
      Result := Result + Ch
    else
      Result := Result + '&' + Entity + ';';
  end;
end;

end.
