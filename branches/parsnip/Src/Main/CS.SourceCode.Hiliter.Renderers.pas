{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Provides highlighter classes used to format and highlight source code in
 * various file formats. Contains a factory object and implementation of various
 * different highlighter objects.
}


unit CS.SourceCode.Hiliter.Renderers;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  CS.SourceCode.Hiliter.Brushes,
  CS.SourceCode.Hiliter.Parser,
  CS.SourceCode.Hiliter.Themes,
  UBaseObjects,
  UEncodings,
  UHTMLBuilder,
  URTFBuilder;

// TODO: fix documentation comments re changes

type
  ///  <summary>Interface implemented by objects that format different source
  ///  code elements on behalf of syntax highlighter.</summary>
  ///  <remarks>Implement this interface for each required output format.
  ///  Syntax highlighter calls the methods of this interface.</remarks>
  IHiliteRenderer2 = interface(IInterface)
  { TODO: rename IHiliteRenderer2 back to IHiliteRenderer when original
          removed. }
    ['{20ED37E9-DE80-42B5-A920-2A62F1753866}']
    ///  <summary>Called by syntax highlighter before any source code is
    ///  processed.</summary>
    procedure Initialise;
    ///  <summary>Called by syntax highlighter after all source code has been
    ///  processed.</summary>
    procedure Finalise;
    ///  <summary>Called by syntax highlighter when a new line of source code
    ///  is started.</summary>
    procedure BeginLine;
    ///  <summary>Called by syntax highlighter after a line of souce code is
    ///  complete.</summary>
    procedure EndLine;
    ///  <summary>Called by syntax highlighter just before a source code
    ///  element is to be output.</summary>
    ///  <param name="Elem">THiliteElement [in] Type of element to be output.
    ///  </param>
    procedure BeforeElem(const ElemInfo: TSyntaxHiliteElemInfo);
    // TODO: revise comment for BeforeElem
    ///  <summary>Called by syntax highlighter for each element of source code
    ///  read. All the given text should be formatted in same style.</summary>
    ///  <remarks>Type of the element will have been specified in prior call to
    ///  BeforeElem.</remarks>
    procedure WriteElemText(const Text: string);
    ///  <summary>Called by syntax highlighter just after an element of source
    ///  code has been written.</summary>
    ///  <param name="Elem">THiliteElement [in] Type of element that has just
    ///  been output.</param>
    procedure AfterElem(const ElemInfo: TSyntaxHiliteElemInfo);
    // TODO: revise comment for AfterElem
  end;

type
  ///  <summary>
  ///  Static class that implements a syntax highlighter for Pascal source code.
  ///  </summary>
  ///  <remarks>
  ///  Uses a user-provided rendering object to create highlighted output in
  ///  various formats.
  ///  </remarks>
  TSyntaxHiliter = class sealed(TNoPublicConstructObject)
  strict private
    var
      ///  <summary>Object used to render parsed code in required format.
      ///  </summary>
      fRenderer: IHiliteRenderer2;
    ///  <summary>Handles Pascal parser's OnElement event.</summary>
    ///  <param name="Parser">THilitePasParser [in] Reference to parser object.
    ///  Not used.</param>
    ///  <param name="Elem">THiliteElement [in] Type of element to be output.
    ///  </param>
    ///  <param name="ElemText">string [in] Name of element to be output.
    ///  </param>
    ///  <remarks>Event triggered by parser for each different source code
    ///  element encountered.</remarks>
    procedure ElementHandler(Parser: TObject;
      const ElemInfo: TSyntaxHiliteElemInfo; const ElemText: string);
    ///  <summary>Handles Pascal parser's OnLineBegin event.</summary>
    ///  <param name="Parser">THilitePasParser [in] Reference to parser object.
    ///  Not used.</param>
    ///  <remarks>Called when a new line of source code is about to be started.
    ///  </remarks>
    procedure LineBeginHandler(Parser: TObject);
    ///  <summary>Handles Pascal parser's OnLineEnd event.</summary>
    ///  <param name="Parser">THilitePasParser [in] Reference to parser object.
    ///  Not used.</param>
    ///  <remarks>Called when a new line of source code has ended.</remarks>
    procedure LineEndHandler(Parser: TObject);
    ///  <summary>Performs syntax highlighting of given source code.</summary>
    procedure DoHilite(const RawCode: string; const Brush: TSyntaxHiliterBrush);
  strict protected
    ///  <summary>Internal object constructor. Sets up object to perform
    ///  highlighting using given renderer object.</summary>
    constructor InternalCreate(Renderer: IHiliteRenderer2);
  public
    ///  <summary>Syntax highlights source code in an output format specified by
    ///  caller.</summary>
    ///  <param name="RawCode">string [in] Plain text source code to be
    ///  highlighted.</param>
    ///  <param name="Renderer">IHiliteRenderer [in] Object used to format and
    ///  record output.</param>
    ///  <remarks>Output is written via renderer in some user defined way. This
    ///  may update an object associated with the renderer.</remarks>
    class procedure Hilite(const RawCode: string;
      const Brush: TSyntaxHiliterBrush; Renderer: IHiliteRenderer2);
  end;

type
  ///  <summary>
  ///  Pure abstract base class for classes that create complete highlighted
  ///  source code documents.
  ///  </summary>
  TDocumentHiliter = class abstract(TNoConstructObject)
  public
    ///  <summary>Creates document containing highlighted source code.</summary>
    ///  <param name="RawCode">string [in] Source code to be processed.</param>
    ///  <param name="Brush">TSyntaxHiliterBrush [in] Specifies highlighter
    ///  brush to be used to perform highlighting.</param>
    ///  <param name="Theme">TSyntaxHiliteTheme [in] Specifies highlighter
    ///  styling to be used by highlighter brush.</param>
    ///  <param name="Title">string [in] Title of document to be included in
    ///  document as meta data. Defaults may be used if Title not specified.
    ///  </param>
    ///  <returns>TEncodedData. Highlighted source code in format applicable to
    ///  output type.</returns>
    ///  <remarks>
    ///  <para>Not all document types support formatting, in which case Brush
    ///  and Theme will be ignored.</para>
    ///  <para>Not all document types support meta data, in which case Title
    ///  will be ignored.</para>
    ///  </remarks>
    class function Hilite(const RawCode: string;
      const Brush: TSyntaxHiliterBrush; const Theme: TSyntaxHiliteTheme;
      const Title: string = ''): TEncodedData; virtual; abstract;
  end;

type
  TDocumentHiliterClass = class of TDocumentHiliter;

type
  ///  <summary>
  ///  Creates a Unicode plain text source code document.
  ///  </summary>
  TNulDocumentHiliter = class sealed(TDocumentHiliter)
  public
    ///  <summary>Creates a plain text document containing source code.
    ///  </summary>
    ///  <param name="RawCode">string [in] Source code to be processed.</param>
    ///  <param name="Brush">TSyntaxHiliterBrush [in] Specifies highlighter
    ///  brush to be used to perform highlighting. Ignored because plain text
    ///  documents do not support syntax highlighting.</param>
    ///  <param name="Theme">TSyntaxHiliteTheme [in] Specifies highlighter
    ///  styling to be used by highlighter brush. Ignored because plain text
    ///  documents do not support syntax highlighting.</param>
    ///  <param name="Title">string [in] Title of document. Ignored because
    ///  plain text documents do not support meta-data.</param>
    ///  <returns>TEncodedData. Plain text in Unicode LE format.</returns>
    class function Hilite(const RawCode: string;
      const Brush: TSyntaxHiliterBrush; const Theme: TSyntaxHiliteTheme;
      const Title: string = ''): TEncodedData; override;
  end;

type
  ///  <summary>
  ///  Creates a highlighted source code document in XHTML format.
  ///  </summary>
  TXHTMLDocumentHiliter = class sealed(TDocumentHiliter)
  strict private
    ///  <summary>Generates the CSS rules to be used in the document.</summary>
    ///  <param name="Brush">TSyntaxHiliterBrush [in] Highlighter brush to be
    ///  used to perform highlighting.</param>
    ///  <param name="Theme">TSyntaxHiliteTheme [in] Styling to be used by
    ///  highlighter brush.</param>
    ///  <returns>string. CSS rules that apply styles specified in Attrs.
    ///  </returns>
    class function GenerateCSSRules(const Brush: TSyntaxHiliterBrush;
      const Theme: TSyntaxHiliteTheme): string;
  public
    ///  <summary>Creates XHTML document containing highlighted source code.
    ///  </summary>
    ///  <param name="RawCode">string [in] Source code to be processed.</param>
    ///  <param name="Brush">TSyntaxHiliterBrush [in] Specifies highlighter
    ///  brush to be used to perform highlighting.</param>
    ///  <param name="Theme">TSyntaxHiliteTheme [in] Specifies highlighter
    ///  styling to be used by highlighter brush.</param>
    ///  <param name="Title">string [in] Title of document to be included in
    ///  document header. If empty string a default title is used.</param>
    ///  <returns>TEncodedData. XHTML code in UTF-8 format.</returns>
    class function Hilite(const RawCode: string;
      const Brush: TSyntaxHiliterBrush; const Theme: TSyntaxHiliteTheme;
      const Title: string = ''): TEncodedData;
      override;
  end;

type
  ///  <summary>
  ///  Creates a highlighted source code document in rich text format.
  ///  </summary>
  TRTFDocumentHiliter = class sealed(TDocumentHiliter)
  public
    ///  <summary>Creates rich text format document containing highlighted
    ///  source code.</summary>
    ///  <param name="RawCode">string [in] Source code to be processed.</param>
    ///  <param name="Brush">TSyntaxHiliterBrush [in] Specifies highlighter
    ///  brush to be used to perform highlighting.</param>
    ///  <param name="Theme">TSyntaxHiliteTheme [in] Specifies highlighter
    ///  styling to be used by highlighter brush.</param>
    ///  <param name="Title">string [in] Title of document to be included in
    ///  document header. No title written if Title is empty string.
    ///  </param>
    ///  <returns>TEncodedData. RTF code in ASCII format.</returns>
    class function Hilite(const RawCode: string;
      const Brush: TSyntaxHiliterBrush; const Theme: TSyntaxHiliteTheme;
      const Title: string = ''): TEncodedData; override;
  end;

type
  ///  <summary>Base class for highlighter rendering objects. Provides common
  ///  functionality needed by all implementations.</summary>
  ///  <remarks>Marked as "abstract" because this class is meaningless if
  ///  instantiated. It is intended only as a sub-class.</remarks>
  THiliteRenderer = class abstract(TInterfacedObject)
  strict private
    var
      fTheme: TSyntaxHiliteTheme;
      fBrush: TSyntaxHiliterBrush;
  strict protected
    ///  <summary>Highlighting styles to be used in rendering.</summary>
    property Theme: TSyntaxHiliteTheme read fTheme;
    property Brush: TSyntaxHiliterBrush read fBrush;
  public
    ///  <summary>Object constructor. Records a copy of highlighter style
    ///  attributes passed in Attrs parameter.</summary>
    constructor Create(const Brush: TSyntaxHiliterBrush;
      const Theme: TSyntaxHiliteTheme);
  end;

type
  ///  <summary>
  ///  Renders highlighted source code in rich text format. Generated code is
  ///  recorded in a given rich text code builder object.
  ///  </summary>
  ///  <remarks>
  ///  Designed for use with TSyntaxHiliter objects.
  ///  </remarks>
  TRTFHiliteRenderer = class(THiliteRenderer, IHiliteRenderer2)
  strict private
    var
      ///  <summary>Object used to record generated RTF code.</summary>
      fBuilder: TRTFBuilder;
    function IsEmptyGroup(const Style: TSyntaxHiliteAttrStyle): Boolean;
  public
    ///  <summary>Object constructor. Sets up object to render documents.
    ///  </summary>
    ///  <param name="Builder">TRTFBuilder [in] Object that receives generated
    ///  RTF code.</param>
    ///  <param name="Brush">TSyntaxHiliterBrush [in] Highlighter brush to be
    ///  used to perform highlighting.</param>
    ///  <param name="Theme">TSyntaxHiliteTheme [in] Styling to be used by
    ///  highlighter brush.</param>
    constructor Create(const Builder: TRTFBuilder;
      const Brush: TSyntaxHiliterBrush; const Theme: TSyntaxHiliteTheme);
    ///  <summary>Initialises RTF ready to receive highlighted code.</summary>
    ///  <remarks>Method of IHiliteRenderer.</remarks>
    procedure Initialise;
    ///  <summary>Tidies up RTF after all highlighted code processed.</summary>
    ///  <remarks>Method of IHiliteRenderer.</remarks>
    procedure Finalise;
    ///  <summary>Resets styles ready for a new line (paragraph) of highlighted
    ///  code.</summary>
    ///  <remarks>Method of IHiliteRenderer.</remarks>
    procedure BeginLine;
    ///  <summary>Closes current RTF paragraph.</summary>
    ///  <remarks>Method of IHiliteRenderer.</remarks>
    procedure EndLine;
    ///  <summary>Sets any highlighting style required for following source code
    ///  element as specified by Elem.</summary>
    ///  <remarks>Method of IHiliteRenderer.</remarks>
    procedure BeforeElem(const ElemInfo: TSyntaxHiliteElemInfo);
    // TODO: revise comment for BeforeElem
    ///  <summary>Writes given source code element text.</summary>
    ///  <remarks>Method of IHiliteRenderer.</remarks>
    procedure WriteElemText(const Text: string);
    ///  <summary>Switches off any highlighting styles used for source code
    ///  element specified by Elem.</summary>
    ///  <remarks>Method of IHiliteRenderer.</remarks>
    procedure AfterElem(const ElemInfo: TSyntaxHiliteElemInfo);
    // TODO: revise comment for AfterElem
  end;

type
  ///  <summary>
  ///  Renders highlighted source code in XHTML format. Generated code is
  ///  recorded in a given HTML code builder object.
  ///  </summary>
  ///  <remarks>
  ///  Designed for use with TSyntaxHiliter objects.
  ///  </remarks>
  THTMLHiliteRenderer = class(THiliteRenderer, IHiliteRenderer2)
  strict private
    var
      ///  <summary>Object used to record generated XHTML code.</summary>
      fBuilder: THTMLBuilder;
      ///  <summary>Flag indicating if writing first line of output.</summary>
      fIsFirstLine: Boolean;
  public
    ///  <summary>Object constructor. Sets up object to render documents.
    ///  </summary>
    ///  <param name="Builder">THTMLBuilder [in] Object that receives generated
    ///  XHTML code.</param>
    ///  <param name="Brush">TSyntaxHiliterBrush [in] Highlighter brush to be
    ///  used to perform highlighting.</param>
    ///  <param name="Theme">TSyntaxHiliteTheme [in] Styling to be used by
    ///  highlighter brush.</param>
    constructor Create(const Builder: THTMLBuilder;
      const Brush: TSyntaxHiliterBrush; const Theme: TSyntaxHiliteTheme);
    ///  <summary>Initialises XHTML ready to receive highlighted code.</summary>
    ///  <remarks>Method of IHiliteRenderer.</remarks>
    procedure Initialise;
    ///  <summary>Tidies up XHTML after all highlighted code processed.
    ///  </summary>
    ///  <remarks>Method of IHiliteRenderer.</remarks>
    procedure Finalise;
    ///  <summary>Emits new line if necessary.</summary>
    ///  <remarks>Method of IHiliteRenderer.</remarks>
    procedure BeginLine;
    ///  <summary>Does nothing.</summary>
    ///  <remarks>
    ///  <para>Handling of new lines is all done by BeginLine.</para>
    ///  <para>Method of IHiliteRenderer.</para>
    ///  </remarks>
    procedure EndLine;
    ///  <summary>Emits any span tag required to style following source code
    ///  element as specified by Elem.</summary>
    ///  <remarks>Method of IHiliteRenderer.</remarks>
    procedure BeforeElem(const ElemInfo: TSyntaxHiliteElemInfo);
    ///  <summary>Writes given source code element text.</summary>
    ///  <remarks>Method of IHiliteRenderer.</remarks>
    procedure WriteElemText(const Text: string);
    ///  <summary>Closes any span tag used to style source code element
    ///  specified by Elem.</summary>
    ///  <remarks>Method of IHiliteRenderer.</remarks>
    procedure AfterElem(const ElemInfo: TSyntaxHiliteElemInfo);
  end;


implementation


uses
  // Delphi
  SysUtils, Graphics,
  // Project
  CS.SourceCode.Hiliter.Renderers.CSS,
  IntfCommon,
  UComparers,
  UCSSBuilder,
  URTFStyles;


{ TSyntaxHiliter }

procedure TSyntaxHiliter.DoHilite(const RawCode: string;
  const Brush: TSyntaxHiliterBrush);
var
  Parser: TSyntaxHiliteParser;   // object used to parse source code
begin
  // Set up parser
  Parser := TSyntaxHiliteParser.Create;
  try
    Parser.OnElement := ElementHandler;
    Parser.OnLineBegin := LineBeginHandler;
    Parser.OnLineEnd := LineEndHandler;
    // Parse the document:
    fRenderer.Initialise;
    Parser.Parse(RawCode, Brush);
    fRenderer.Finalise;
  finally
    Parser.Free;
  end;
end;

procedure TSyntaxHiliter.ElementHandler(Parser: TObject;
  const ElemInfo: TSyntaxHiliteElemInfo; const ElemText: string);
begin
  fRenderer.BeforeElem(ElemInfo);
  fRenderer.WriteElemText(ElemText);
  fRenderer.AfterElem(ElemInfo);
end;

class procedure TSyntaxHiliter.Hilite(const RawCode: string;
  const Brush: TSyntaxHiliterBrush; Renderer: IHiliteRenderer2);
begin
  Assert(Assigned(Renderer), ClassName + '.Create: Renderer is nil');
  with InternalCreate(Renderer) do
    try
      DoHilite(RawCode, Brush);
    finally
      Free;
    end;
end;

procedure TSyntaxHiliter.LineBeginHandler(Parser: TObject);
begin
  fRenderer.BeginLine;
end;

procedure TSyntaxHiliter.LineEndHandler(Parser: TObject);
begin
  fRenderer.EndLine;
end;

constructor TSyntaxHiliter.InternalCreate(Renderer: IHiliteRenderer2);
begin
  inherited InternalCreate;
  fRenderer := Renderer;
end;

{ TNulDocumentHiliter }

class function TNulDocumentHiliter.Hilite(const RawCode: string;
  const Brush: TSyntaxHiliterBrush; const Theme: TSyntaxHiliteTheme;
  const Title: string): TEncodedData;
begin
  Result := TEncodedData.Create(RawCode, etUnicode);
end;

{ TXHTMLDocumentHiliter }

class function TXHTMLDocumentHiliter.GenerateCSSRules(
  const Brush: TSyntaxHiliterBrush; const Theme: TSyntaxHiliteTheme):
  string;
var
  CSSBuilder: TCSSBuilder;  // builds CSS code
  HiliterCSS: THiliterCSS;  // generates CSS names and properties for hiliter
begin
  HiliterCSS := THiliterCSS.Create(Theme);
  try
    CSSBuilder := TCSSBuilder.Create;
    try
      HiliterCSS.BuildBrushCSS(Brush, CSSBuilder);
      Result := CSSBuilder.AsString;
    finally
      CSSBuilder.Free;
    end;
  finally
    HiliterCSS.Free;
  end;
end;

class function TXHTMLDocumentHiliter.Hilite(const RawCode: string;
  const Brush: TSyntaxHiliterBrush; const Theme: TSyntaxHiliteTheme;
  const Title: string): TEncodedData;
resourcestring
  // Default document title
  sDefaultTitle = 'DelphiDabbler CodeSnip Database';
var
  Renderer: IHiliteRenderer2;    // XHTML renderer object
  Builder: THTMLBuilder;        // object used to construct XHTML document
begin
  Builder := THTMLBuilder.Create;
  try
    if Title <> '' then
      Builder.Title := Title
    else
      Builder.Title := sDefaultTitle;
    Builder.CSS := GenerateCSSRules(Brush, Theme);
    Renderer := THTMLHiliteRenderer.Create(Builder, Brush, Theme);
    TSyntaxHiliter.Hilite(RawCode, Brush, Renderer);
    Result := TEncodedData.Create(Builder.HTMLDocument, etUTF8);
  finally
    Builder.Free;
  end;
end;

{ TRTFDocumentHiliter }

class function TRTFDocumentHiliter.Hilite(const RawCode: string;
  const Brush: TSyntaxHiliterBrush; const Theme: TSyntaxHiliteTheme;
  const Title: string): TEncodedData;
var
  Renderer: IHiliteRenderer2;  // RTF renderer object
  Builder: TRTFBuilder;       // object used to construct RTF document
begin
  Builder := TRTFBuilder.Create(0);
  try
    Builder.DocProperties.Title := Title;
    Renderer := TRTFHiliteRenderer.Create(Builder, Brush, Theme);
    TSyntaxHiliter.Hilite(RawCode, Brush, Renderer);
    Result := TEncodedData.Create(Builder.Render.ToBytes, etASCII);
  finally
    Builder.Free;
  end;
end;

{ THiliteRenderer }

constructor THiliteRenderer.Create(const Brush: TSyntaxHiliterBrush;
  const Theme: TSyntaxHiliteTheme);
begin
  inherited Create;
  fTheme := Theme;
  fBrush := Brush;
end;

{ TRTFHiliteRenderer }

procedure TRTFHiliteRenderer.AfterElem(const ElemInfo: TSyntaxHiliteElemInfo);
begin
  if not IsEmptyGroup(Theme.GetStyle(ElemInfo.BrushID, ElemInfo.AttrID)) then
    fBuilder.EndGroup;
end;

procedure TRTFHiliteRenderer.BeforeElem(const ElemInfo: TSyntaxHiliteElemInfo);
var
  AttrStyle: TSyntaxHiliteAttrStyle;
begin
  AttrStyle := Theme.GetStyle(ElemInfo.BrushID, ElemInfo.AttrID);
  if not IsEmptyGroup(AttrStyle) then
  begin
    fBuilder.BeginGroup;
    // TODO: Find a way of displaying background colour in RTF
    //    if AttrStyle.Background <> clNone then
    //      fBuilder.SetColour(AttrStyle.Foreground);
    if AttrStyle.Foreground <> Theme.DefaultForeground then
      fBuilder.SetColour(AttrStyle.Foreground);
    if AttrStyle.FontStyles.Styles <> [] then
      fBuilder.SetFontStyle(AttrStyle.FontStyles);
  end;
end;

procedure TRTFHiliteRenderer.BeginLine;
begin
  fBuilder.ResetCharStyle;
  fBuilder.SetFont(Theme.FontName);
  fBuilder.SetFontSize(Theme.FontSize);
  fBuilder.SetColour(Theme.DefaultForeground);
end;

constructor TRTFHiliteRenderer.Create(const Builder: TRTFBuilder;
  const Brush: TSyntaxHiliterBrush; const Theme: TSyntaxHiliteTheme);
begin
  Assert(Assigned(Builder), ClassName + '.Create: Builder is nil');
  inherited Create(Brush, Theme);
  fBuilder := Builder;
end;

procedure TRTFHiliteRenderer.EndLine;
begin
  fBuilder.EndPara;
end;

procedure TRTFHiliteRenderer.Finalise;
begin
  // End group containing all source code
  fBuilder.EndGroup;
end;

procedure TRTFHiliteRenderer.Initialise;
var
  Attrs: TArray<TSyntaxHiliterAttr>;
  Attr: TSyntaxHiliterAttr;
  AttrStyle: TSyntaxHiliteAttrStyle;
begin
  // Set up font table
  fBuilder.FontTable.Add(Theme.FontName, rgfModern, 0);
  // Set up colour table
  Attrs := Brush.SupportedAttrs;
  for Attr in Attrs do
  begin
    AttrStyle := Theme.GetStyle(Brush.ID, Attr.ID);
    fBuilder.ColourTable.Add(AttrStyle.Foreground);
  end;
  // Begin group containing all source code
  fBuilder.BeginGroup;
  // Clear any paragraph formatting
  fBuilder.ClearParaFormatting;
end;

function TRTFHiliteRenderer.IsEmptyGroup(
  const Style: TSyntaxHiliteAttrStyle): Boolean;
begin
  { TODO: if and when Style.Background is supported, replace this with test on
          Theme.IsBaseStyle }
  Result := (Style.Foreground = Theme.DefaultForeground)
    and Style.FontStyles.IsNull;
end;

procedure TRTFHiliteRenderer.WriteElemText(const Text: string);
begin
  fBuilder.AddText(Text);
end;

{ THTMLHiliteRenderer }

procedure THTMLHiliteRenderer.AfterElem(const ElemInfo: TSyntaxHiliteElemInfo);
begin
  if not Theme.IsBaseStyle(
    Theme.GetStyle(ElemInfo.BrushID, ElemInfo.AttrID)
  ) then
    fBuilder.CloseSpan;
end;

procedure THTMLHiliteRenderer.BeforeElem(const ElemInfo: TSyntaxHiliteElemInfo);
begin
  if not Theme.IsBaseStyle(
    Theme.GetStyle(ElemInfo.BrushID, ElemInfo.AttrID)
  ) then
    fBuilder.OpenSpan(
      THiliterCSS.GetElemCSSClassName(ElemInfo.BrushID, ElemInfo.AttrID)
    );
end;

procedure THTMLHiliteRenderer.BeginLine;
begin
  // Note we don't emit CRLF before first line since it must be on same line as
  // <pre> tag that will have been written.
  if fIsFirstLine then
    fIsFirstLine := False
  else
    fBuilder.NewLine;
end;

constructor THTMLHiliteRenderer.Create(const Builder: THTMLBuilder;
  const Brush: TSyntaxHiliterBrush; const Theme: TSyntaxHiliteTheme);
begin
  Assert(Assigned(Builder), ClassName + '.Create: Builder is nil');
  inherited Create(Brush, Theme);
  fBuilder := Builder;
end;

procedure THTMLHiliteRenderer.EndLine;
begin
  // Do nothing: we use BeginLine() to start new lines in preformatted text
end;

procedure THTMLHiliteRenderer.Finalise;
begin
  fBuilder.ClosePre;
end;

procedure THTMLHiliteRenderer.Initialise;
begin
  // Note that we are about to write first line
  fIsFirstLine := True;
  fBuilder.OpenPre(THiliterCSS.GetMainCSSClassName);
end;

procedure THTMLHiliteRenderer.WriteElemText(const Text: string);
begin
  fBuilder.AddText(Text);
end;

end.

