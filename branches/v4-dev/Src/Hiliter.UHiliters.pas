{
 * Hiliter.UHiliters.pas
 *
 * Provides highlighter classes used to format and highlight source code in
 * various file formats. Contains a factory object and implementation of various
 * different highlighter objects.
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
 * The Original Code is Hiliter.UHiliters.pas, formerly USyntaxHiliters.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit Hiliter.UHiliters;


interface


uses
  // Project
  Hiliter.UGlobals, Hiliter.UPasParser, UBaseObjects, UEncodings, UHTMLBuilder,
  URTFBuilder;


type
  ///  <summary>
  ///  Implements a syntax highlighter for Pascal source code.
  ///  </summary>
  TSyntaxHiliter = class sealed(TInterfacedObject, ISyntaxHiliter)
  strict private
    var
      ///  <summary>Object used to render parsed code in required format.
      ///  </summary>
      fRenderer: IHiliteRenderer;
    ///  <summary>Handles Pascal parser's OnElement event.</summary>
    ///  <param name="Parser">THilitePasParser [in] Reference to parser object.
    ///  Not used.</param>
    ///  <param name="Elem">THiliteElement [in] Type of element to be output.
    ///  </param>
    ///  <param name="ElemText">string [in] Name of element to be output.
    ///  </param>
    ///  <remarks>Event triggered by parser for each different source code
    ///  element encountered.</remarks>
    procedure ElementHandler(Parser: THilitePasParser; Elem: THiliteElement;
      const ElemText: string);
    ///  <summary>Handles Pascal parser's OnLineBegin event.</summary>
    ///  <param name="Parser">THilitePasParser [in] Reference to parser object.
    ///  Not used.</param>
    ///  <remarks>Called when a new line of source code is about to be started.
    ///  </remarks>
    procedure LineBeginHandler(Parser: THilitePasParser);
    ///  <summary>Handles Pascal parser's OnLineEnd event.</summary>
    ///  <param name="Parser">THilitePasParser [in] Reference to parser object.
    ///  Not used.</param>
    ///  <remarks>Called when a new line of source code has ended.</remarks>
    procedure LineEndHandler(Parser: THilitePasParser);
  public
    ///  <summary>Object constructor. Sets up hiliter for a specified output
    ///  format.</summary>
    ///  <param name="Renderer">IHiliteRenderer [in] Object to be used to
    ///  render output.</param>
    constructor Create(Renderer: IHiliteRenderer);
    ///  <summary>Highlights the given raw source code.</summary>
    ///  <remarks>
    ///  <para>Method of ISyntaxHiliter</para>
    ///  <para>Output is written via renderer is some user defined way. This may
    ///  update an object associated with the renderer.</para>
    ///  </remarks>
    procedure Hilite(const RawCode: string);
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
    ///  <param name="Attrs">IHiliteAttrs [in] Specifies required highlighting
    ///  style. If nil document is not highlighted.</param>
    ///  <param name="Title">string [in] Title of document to be included in
    ///  document as meta data. Defaults may be used if Title not specified.
    ///  </param>
    ///  <returns>TEncodedData. Highlighted source code in format applicable to
    ///  output type.</returns>
    ///  <remarks>
    ///  <para>Not all document types support formatting, in which case Attrs
    ///  will be ignored.</para>
    ///  <para>Not all document types support meta data, in which case Title
    ///  will be ignored.</para>
    ///  </remarks>
    class function Hilite(const RawCode: string;
      Attrs: IHiliteAttrs = nil; const Title: string = ''): TEncodedData;
      virtual; abstract;
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
    ///  <param name="Attrs">IHiliteAttrs [in] Required highlighting style.
    ///  Ignored because plain text documents do not support formatting.
    ///  </param>
    ///  <param name="Title">string [in] Title of document. Ignored because
    ///  plain text documents do not support meta-data.</param>
    ///  <returns>TEncodedData. Plain text in Unicode LE format.
    ///  </returns>
    class function Hilite(const RawCode: string;
      Attrs: IHiliteAttrs = nil; const Title: string = ''): TEncodedData;
      override;
  end;

type
  ///  <summary>
  ///  Creates a highlighted source code document in XHTML format.
  ///  </summary>
  TXHTMLDocumentHiliter = class sealed(TDocumentHiliter)
  strict private
    ///  <summary>Generates the CSS rules to be used in the document.</summary>
    ///  <param name="Attrs">IHiliteAttrs [in] Highlighting styles used in
    ///  document.</param>
    ///  <returns>string. CSS rules that apply styles specified in Attrs.
    ///  </returns>
    class function GenerateCSSRules(Attrs: IHiliteAttrs): string;
  public
    ///  <summary>Creates XHTML document containing highlighted source code.
    ///  </summary>
    ///  <param name="RawCode">string [in] Source code to be processed.</param>
    ///  <param name="Attrs">IHiliteAttrs [in] Specifies required highlighting
    ///  style. If nil document is not highlighted.</param>
    ///  <param name="Title">string [in] Title of document to be included in
    ///  document header. If empty string a default title is used.</param>
    ///  <returns>TEncodedData. XHTML code in UTF-8 format.</returns>
    class function Hilite(const RawCode: string;
      Attrs: IHiliteAttrs = nil; const Title: string = ''): TEncodedData;
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
    ///  <param name="Attrs">IHiliteAttrs [in] Specifies required highlighting
    ///  style. If nil document is not highlighted.</param>
    ///  <param name="Title">string [in] Title of document to be included in
    ///  document header. No title written if Title is empty string.
    ///  </param>
    ///  <returns>TEncodedData. RTF code in ASCII format.</returns>
    class function Hilite(const RawCode: string;
      Attrs: IHiliteAttrs = nil; const Title: string = ''): TEncodedData;
      override;
  end;

type
  ///  <summary>
  ///  Renders highlighted source code in rich text format. Generated code is
  ///  recorded in a given rich text code builder object.
  ///  </summary>
  ///  <remarks>
  ///  Designed for use with TSyntaxHiliter objects.
  ///  </remarks>
  TRTFHiliteRenderer = class(TInterfacedObject, IHiliteRenderer)
  strict private
    var
      ///  <summary>Object used to record generated RTF code.</summary>
      fBuilder: TRTFBuilder;
      ///  <summary>Specifies highlighting style to be used.</summary>
      fAttrs: IHiliteAttrs;
  public
    ///  <summary>Object constructor. Sets up object to render documents.
    ///  </summary>
    ///  <param name="Builder">TRTFBuilder [in] Object that receives generated
    ///  RTF code.</param>
    ///  <param name="Attrs">IHiliteAttrs [in] Specifies required highlighting
    ///  style. If nil document is not highlighted.</param>
    constructor Create(const Builder: TRTFBuilder;
      const Attrs: IHiliteAttrs = nil);
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
    procedure BeforeElem(Elem: THiliteElement);
    ///  <summary>Writes given source code element text.</summary>
    ///  <remarks>Method of IHiliteRenderer.</remarks>
    procedure WriteElemText(const Text: string);
    ///  <summary>Switches off any highlighting styles used for source code
    ///  element specified by Elem.</summary>
    ///  <remarks>Method of IHiliteRenderer.</remarks>
    procedure AfterElem(Elem: THiliteElement);
  end;

type
  ///  <summary>
  ///  Renders highlighted source code in XHTML format. Generated code is
  ///  recorded in a given HTML code builder object.
  ///  </summary>
  ///  <remarks>
  ///  Designed for use with TSyntaxHiliter objects.
  ///  </remarks>
  THTMLHiliteRenderer = class(TInterfacedObject, IHiliteRenderer)
  strict private
    var
      ///  <summary>Object used to record generated XHTML code.</summary>
      fBuilder: THTMLBuilder;
      ///  <summary>Specifies highlighting style to be used.</summary>
      fAttrs: IHiliteAttrs;
      ///  <summary>Flag indicating if writing first line of output.</summary>
      fIsFirstLine: Boolean;
  public
    ///  <summary>Object constructor. Sets up object to render documents.
    ///  </summary>
    ///  <param name="Builder">THTMLBuilder [in] Object that receives generated
    ///  XHTML code.</param>
    ///  <param name="Attrs">IHiliteAttrs [in] Specifies required highlighting
    ///  style. If nil document is not highlighted.</param>
    constructor Create(const Builder: THTMLBuilder;
      const Attrs: IHiliteAttrs = nil);
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
    procedure BeforeElem(Elem: THiliteElement);
    ///  <summary>Writes given source code element text.</summary>
    ///  <remarks>Method of IHiliteRenderer.</remarks>
    procedure WriteElemText(const Text: string);
    ///  <summary>Closes any span tag used to style source code element
    ///  specified by Elem.</summary>
    ///  <remarks>Method of IHiliteRenderer.</remarks>
    procedure AfterElem(Elem: THiliteElement);
  end;


implementation


uses
  // Delphi
  SysUtils, Graphics,
  // Project
  Hiliter.UAttrs, Hiliter.UCSS, IntfCommon, UCSSBuilder;


{ TSyntaxHiliter }

constructor TSyntaxHiliter.Create(Renderer: IHiliteRenderer);
begin
  Assert(Assigned(Renderer), ClassName + '.Create: Renderer is nil');
  inherited Create;
  fRenderer := Renderer;
end;

procedure TSyntaxHiliter.ElementHandler(Parser: THilitePasParser;
  Elem: THiliteElement; const ElemText: string);
begin
  fRenderer.BeforeElem(Elem);
  fRenderer.WriteElemText(ElemText);
  fRenderer.AfterElem(Elem);
end;

procedure TSyntaxHiliter.Hilite(const RawCode: string);
var
  Parser: THilitePasParser;   // object used to parse source code
begin
  Parser := nil;
  try
    // Create parser
    Parser := THilitePasParser.Create;
    Parser.OnElement := ElementHandler;
    Parser.OnLineBegin := LineBeginHandler;
    Parser.OnLineEnd := LineEndHandler;
    // Parse the document:
    fRenderer.Initialise;
    Parser.Parse(RawCode);
    fRenderer.Finalise;
  finally
    Parser.Free;
  end;
end;

procedure TSyntaxHiliter.LineBeginHandler(Parser: THilitePasParser);
begin
  fRenderer.BeginLine;
end;

procedure TSyntaxHiliter.LineEndHandler(Parser: THilitePasParser);
begin
  fRenderer.EndLine;
end;

{ TNulDocumentHiliter }

class function TNulDocumentHiliter.Hilite(const RawCode: string;
  Attrs: IHiliteAttrs; const Title: string): TEncodedData;
begin
  Result := TEncodedData.Create(RawCode, etUnicode);
end;

{ TXHTMLDocumentHiliter }

class function TXHTMLDocumentHiliter.GenerateCSSRules(Attrs: IHiliteAttrs):
  string;
var
  CSSBuilder: TCSSBuilder;  // builds CSS code
  HiliterCSS: THiliterCSS;  // generates CSS names and properties for hiliter
begin
  if not Assigned(Attrs) then
    Attrs := THiliteAttrsFactory.CreateNulAttrs;
  HiliterCSS := THiliterCSS.Create(Attrs);
  try
    CSSBuilder := TCSSBuilder.Create;
    try
      HiliterCSS.BuildCSS(CSSBuilder);
      Result := CSSBuilder.AsString;
    finally
      CSSBuilder.Free;
    end;
  finally
    HiliterCSS.Free;
  end;
end;

class function TXHTMLDocumentHiliter.Hilite(const RawCode: string;
  Attrs: IHiliteAttrs; const Title: string): TEncodedData;
resourcestring
  // Default document title
  sDefaultTitle = 'DelphiDabbler CodeSnip Database';
var
  Hiliter: ISyntaxHiliter;      // syntax hiliter object
  Renderer: IHiliteRenderer;    // XHTML renderer object
  Builder: THTMLBuilder;        // object used to construct XHTML document
begin
  Builder := THTMLBuilder.Create;
  try
    if Title <> '' then
      Builder.Title := Title
    else
      Builder.Title := sDefaultTitle;
    Builder.CSS := GenerateCSSRules(Attrs);
    Renderer := THTMLHiliteRenderer.Create(Builder, Attrs);
    Hiliter := TSyntaxHiliter.Create(Renderer);
    Hiliter.Hilite(RawCode);
    Result := TEncodedData.Create(Builder.HTMLDocument, etUTF8);
  finally
    Builder.Free;
  end;
end;

{ TRTFDocumentHiliter }

class function TRTFDocumentHiliter.Hilite(const RawCode: string;
  Attrs: IHiliteAttrs; const Title: string): TEncodedData;
var
  Hiliter: ISyntaxHiliter;    // syntax highlighter object
  Renderer: IHiliteRenderer;  // RTF renderer object
  Builder: TRTFBuilder;       // object used to construct RTF document
begin
  Builder := TRTFBuilder.Create(0);
  try
    Builder.DocProperties.Title := Title;
    Renderer := TRTFHiliteRenderer.Create(Builder, Attrs);
    Hiliter := TSyntaxHiliter.Create(Renderer);
    Hiliter.Hilite(RawCode);
    Result := TEncodedData.Create(Builder.Render.ToBytes, etASCII);
  finally
    Builder.Free;
  end;
end;

{ TRTFHiliteRenderer }

procedure TRTFHiliteRenderer.AfterElem(Elem: THiliteElement);
begin
  if not fAttrs[Elem].IsNul then
    fBuilder.EndGroup;
end;

procedure TRTFHiliteRenderer.BeforeElem(Elem: THiliteElement);
begin
  if not fAttrs[Elem].IsNul then
  begin
    fBuilder.BeginGroup;
    if fAttrs[Elem].ForeColor <> clNone then
      fBuilder.SetColour(fAttrs[Elem].ForeColor);
    if fAttrs[Elem].FontStyle <> [] then
      fBuilder.SetFontStyle(fAttrs[Elem].FontStyle);
  end;
end;

procedure TRTFHiliteRenderer.BeginLine;
begin
  fBuilder.ResetCharStyle;
  fBuilder.SetFont(fAttrs.FontName);
  fBuilder.SetFontSize(fAttrs.FontSize);
end;

constructor TRTFHiliteRenderer.Create(const Builder: TRTFBuilder;
  const Attrs: IHiliteAttrs = nil);
begin
  Assert(Assigned(Builder), ClassName + '.Create: Builder is nil');
  inherited Create;
  fBuilder := Builder;
  fAttrs := THiliteAttrsFactory.CreateNulAttrs;
  (fAttrs as IAssignable).Assign(Attrs);
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
  Elem: THiliteElement; // loops thru all highlight elements
begin
  // Set up font table
  fBuilder.FontTable.Add(fAttrs.FontName, rgfModern, 0);
  // Set up colour table
  for Elem := Low(THiliteElement) to High(THiliteElement) do
    fBuilder.ColourTable.Add(fAttrs[Elem].ForeColor);
  // Begin group containing all source code
  fBuilder.BeginGroup;
  // Clear any paragraph formatting
  fBuilder.ClearParaFormatting;
end;

procedure TRTFHiliteRenderer.WriteElemText(const Text: string);
begin
  fBuilder.AddText(Text);
end;

{ THTMLHiliteRenderer }

procedure THTMLHiliteRenderer.AfterElem(Elem: THiliteElement);
begin
  if not fAttrs[Elem].IsNul then
    fBuilder.CloseSpan;
end;

procedure THTMLHiliteRenderer.BeforeElem(Elem: THiliteElement);
begin
  if not fAttrs.Elements[Elem].IsNul then
    fBuilder.OpenSpan(THiliterCSS.GetElemCSSClassName(Elem));
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
  const Attrs: IHiliteAttrs);
begin
  Assert(Assigned(Builder), ClassName + '.Create: Builder is nil');
  inherited Create;
  fBuilder := Builder;
  fAttrs := THiliteAttrsFactory.CreateNulAttrs;
  (fAttrs as IAssignable).Assign(Attrs);
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

