{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2016, Peter Johnson (www.delphidabbler.com).
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
  URTFBuilder,
  USourceFileInfo;


type
  ///  <summary>Interface implemented by objects that format different source
  ///  code elements on behalf of syntax highlighter.</summary>
  ///  <remarks>Implement this interface for each required output format.
  ///  Syntax highlighter calls the methods of this interface.</remarks>
  IHiliteRenderer = interface(IInterface)
    ['{17816D15-0565-449A-AFE3-D3CD74C3C6FC}']
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
    ///  <param name="ElemInfo"><c>TSyntaxHiliteElemInfo</c> [in] Provides
    ///  information about the brush and attribute of the source code element
    ///  to be highlighted.</param>
    procedure BeforeElem(const ElemInfo: TSyntaxHiliteElemInfo);
    ///  <summary>Called by syntax highlighter for each element of source code
    ///  read.</summary>
    ///  <param name="Text"><c>string</c> [in] Text of the source code element.
    ///  </param>
    ///  <remarks>
    ///  <para>The type of the element will have been specified in a prior call
    ///  to <c>BeforeElem</c>.</para>
    ///  <para>All the given text should be formatted in the same style.</para>
    ///  </remarks>
    procedure WriteElemText(const Text: string);
    ///  <summary>Called by syntax highlighter just after an element of source
    ///  code has been written.</summary>
    ///  <param name="ElemInfo"><c>TSyntaxHiliteElemInfo</c> [in] Provides
    ///  information about the brush and attribute of the source code element
    ///  that has just been written.</param>
    procedure AfterElem(const ElemInfo: TSyntaxHiliteElemInfo);
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
      fRenderer: IHiliteRenderer;
    ///  <summary>Handles Pascal parser's <c>OnElement</c> event.</summary>
    ///  <param name="Parser"><c>THilitePasParser</c> [in] Reference to parser
    ///  object. Not used.</param>
    ///  <param name="Elem"><c>THiliteElement</c> [in] Type of element to be
    ///  output.</param>
    ///  <param name="ElemText"><c>string</c> [in] Name of element to be output.
    ///  </param>
    ///  <remarks>Event triggered by parser for each different source code
    ///  element encountered.</remarks>
    procedure ElementHandler(Parser: TObject;
      const ElemInfo: TSyntaxHiliteElemInfo; const ElemText: string);
    ///  <summary>Handles Pascal parser's <c>OnLineBegin</c> event.</summary>
    ///  <param name="Parser"><c>THilitePasParser</c> [in] Reference to parser
    ///  object. Not used.</param>
    ///  <remarks>Called when a new line of source code is about to be started.
    ///  </remarks>
    procedure LineBeginHandler(Parser: TObject);
    ///  <summary>Handles Pascal parser's <c>OnLineEnd</c> event.</summary>
    ///  <param name="Parser"><c>THilitePasParser</c> [in] Reference to parser
    ///  object. Not used.</param>
    ///  <remarks>Called when a line of source code has ended.</remarks>
    procedure LineEndHandler(Parser: TObject);
    ///  <summary>Performs syntax highlighting of given source code.</summary>
    procedure DoHilite(const RawCode: string; const Brush: TSyntaxHiliterBrush);
  strict protected
    ///  <summary>Internal object constructor. Sets up object to perform
    ///  highlighting using given renderer object.</summary>
    constructor InternalCreate(Renderer: IHiliteRenderer);
  public
    ///  <summary>Syntax highlights source code in an output format specified by
    ///  caller.</summary>
    ///  <param name="RawCode"><c>string</c> [in] Plain text source code to be
    ///  highlighted.</param>
    ///  <param name="Brush"><c>TSyntaxHiliterBrush</c> [in] Syntax highlighter
    ///  brush used determine how highlighting is performed.</param>
    ///  <param name="Renderer"><c>IHiliteRenderer</c> [in] Object that renders
    ///  highlighted source code.</param>
    ///  <remarks>The <c>Brush</c> object interprets the source code (e.g. as
    ///  Pascal or JavaScript) and creates a stream of tokens along with details
    ///  of the highlighting to be applied to each one. The <c>Renderer</c>
    ///  object takes this information and renders the tokens in a particular
    ///  format (e.g. HTML).</remarks>
    class procedure Hilite(const RawCode: string;
      const Brush: TSyntaxHiliterBrush; Renderer: IHiliteRenderer);
  end;

type
  ///  <summary>
  ///  Pure abstract base class for classes that create complete highlighted
  ///  source code documents.
  ///  </summary>
  TDocumentHiliter = class abstract(TNoConstructObject)
  public
    ///  <summary>Creates document containing highlighted source code.</summary>
    ///  <param name="RawCode"><c>string</c> [in] Source code to be processed.
    ///  </param>
    ///  <param name="Brush"><c>TSyntaxHiliterBrush</c> [in] Specifies
    ///  highlighter brush to be used to perform highlighting.</param>
    ///  <param name="Theme"><c>TSyntaxHiliteTheme</c> [in] Specifies styling
    ///  to be used by highlighter brush.</param>
    ///  <param name="Title"><c>string</c> [in] Title of document to be included
    ///  in document as meta data. Defaults may be used if not specified.
    ///  </param>
    ///  <returns><c>TEncodedData</c>. Highlighted source code in format
    ///  applicable to output type.</returns>
    ///  <remarks>
    ///  <para>Not all document types support formatting, in which case
    ///  <c>Brush</c> and <c>Theme</c> will be ignored.</para>
    ///  <para>Not all document types support meta data, in which case
    ///  <c>Title</c> will be ignored.</para>
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
  // TODO: Rename class with Null spelled correctly!
  TNulDocumentHiliter = class sealed(TDocumentHiliter)
  public
    ///  <summary>Creates a plain, unhighlighted, text document containing
    ///  source code.</summary>
    ///  <param name="RawCode"><c>string</c> [in] Source code to be processed.
    ///  </param>
    ///  <param name="Brush"><c>TSyntaxHiliterBrush</c> [in] Specifies
    ///  highlighter brush to be used to perform highlighting. Ignored because
    ///  plain text documents do not support syntax highlighting.</param>
    ///  <param name="Theme"><c>TSyntaxHiliteTheme</c> [in] Specifies styling
    ///  styling to be used by highlighter brush. Ignored because plain text
    ///  documents do not support syntax highlighting.</param>
    ///  <param name="Title"><c>string</c> [in] Title of document. Ignored
    ///  because plain text documents do not support meta-data.</param>
    ///  <returns><c>TEncodedData</c>. Plain text in Unicode LE format.
    ///  </returns>
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
    ///  <param name="Brush"><c>TSyntaxHiliterBrush</c> [in] Highlighter brush
    ///  to be used to perform highlighting.</param>
    ///  <param name="Theme"><c>TSyntaxHiliteTheme</c> [in] Styling to be used
    ///  by highlighter brush.</param>
    ///  <returns><c>string</c>. CSS rules that apply the styles specified by
    ///  <c>Theme</c> for the given <c>Brush</c>.</returns>
    class function GenerateCSSRules(const Brush: TSyntaxHiliterBrush;
      const Theme: TSyntaxHiliteTheme): string;
  public
    ///  <summary>Creates XHTML document containing highlighted source code.
    ///  </summary>
    ///  <param name="RawCode"><c>string</c> [in] Source code to be processed.
    ///  </param>
    ///  <param name="Brush"><c>TSyntaxHiliterBrush</c> [in] Specifies
    ///  highlighter brush to be used to perform highlighting.</param>
    ///  <param name="Theme"><c>TSyntaxHiliteTheme</c> [in] Specifies styling to
    ///  be used by highlighter brush.</param>
    ///  <param name="Title"><c>string</c> [in] Title of document to be included
    ///  in document header. If empty string a default title is used.</param>
    ///  <returns><c>TEncodedData</c>. XHTML code in UTF-8 format.</returns>
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
    ///  <param name="RawCode"><c>string</c> [in] Source code to be processed.
    ///  </param>
    ///  <param name="Brush"><c>TSyntaxHiliterBrush</c> [in] Specifies
    ///  highlighter brush to be used to perform highlighting.</param>
    ///  <param name="Theme"><c>TSyntaxHiliteTheme</c> [in] Specifies styling to
    ///  be used by highlighter brush.</param>
    ///  <param name="Title"><c>string</c> [in] Title of document to be included
    ///  in document header. No title is written if empty string.</param>
    ///  <returns><c>TEncodedData</c>. RTF code in ASCII format.</returns>
    class function Hilite(const RawCode: string;
      const Brush: TSyntaxHiliterBrush; const Theme: TSyntaxHiliteTheme;
      const Title: string = ''): TEncodedData; override;
  end;

type
  ///  <summary>Container for methods to help with highlighting source code for
  ///  various different output file types.</summary>
  TDocumentHiliterHelper = record
  public
    ///  <summary>Checks if syntax highlighting is supported for the given
    ///  output file type.</summary>
    class function IsHilitingSupported(const FileType: TSourceOutputFileType):
      Boolean; static;
    ///  <summary>Returns the highlighter class to be used to syntax highlight
    ///  source code for the given output file type.</summary>
    ///  <remarks>A null highlighter will be returned if syntax highlighting is
    ///  not supported for the given file type.</remarks>
    class function GetHiliterClass(const FileType: TSourceOutputFileType):
      TDocumentHiliterClass; static;
  end;

type
  ///  <summary>Base class for highlighter rendering objects. Provides common
  ///  functionality needed by all implementations.</summary>
  ///  <remarks>Marked as "abstract" because this class is meaningless if
  ///  instantiated. It is intended only as a sub-class.</remarks>
  THiliteRenderer = class abstract(TInterfacedObject)
  strict private
    var
      ///  <summary>Value of <c>Theme</c> property.</summary>
      fTheme: TSyntaxHiliteTheme;
      ///  <summary>Value of <c>Brush</c> property.</summary>
      fBrush: TSyntaxHiliterBrush;
  strict protected
    ///  <summary>Styling to be applied to highlighted document.</summary>
    property Theme: TSyntaxHiliteTheme read fTheme;
    ///  <summary>Syntax highlighter brush used to determine how highlighting is
    ///  performed.</summary>
    property Brush: TSyntaxHiliterBrush read fBrush;
  public
    ///  <summary>Object constructor. Sets up object to use given theme and
    ///  syntax highlighter brush.</summary>
    constructor Create(const Brush: TSyntaxHiliterBrush;
      const Theme: TSyntaxHiliteTheme);
  end;

type
  ///  <summary>
  ///  Renders highlighted source code in rich text format. Generated code is
  ///  recorded in a given rich text code builder object.
  ///  </summary>
  ///  <remarks>
  ///  Designed for use with <c>TSyntaxHiliter</c> objects.
  ///  </remarks>
  TRTFHiliteRenderer = class(THiliteRenderer, IHiliteRenderer)
  strict private
    var
      ///  <summary>Object used to record generated RTF code.</summary>
      fBuilder: TRTFBuilder;
    ///  <summary>Checks if the RTF group required to apply formatting specified
    ///  by <c>Style</c> would be empty.</summary>
    function IsEmptyGroup(const Style: TSyntaxHiliteAttrStyle): Boolean;
  public
    ///  <summary>Creates object instance to render RTF documents using a given
    ///  syntax highlighter brush styled according to a given theme.</summary>
    ///  <param name="Builder"><c>TRTFBuilder</c> [in] Object that receives
    ///  generated RTF code.</param>
    ///  <param name="Brush"><c>TSyntaxHiliterBrush</c> [in] Highlighter brush
    ///  to be used to perform highlighting.</param>
    ///  <param name="Theme"><c>TSyntaxHiliteTheme</c> [in] Styling to be
    ///  applied to highlighted document.</param>
    constructor Create(const Builder: TRTFBuilder;
      const Brush: TSyntaxHiliterBrush; const Theme: TSyntaxHiliteTheme);
    ///  <summary>Initialises RTF ready to receive highlighted code.</summary>
    ///  <remarks>Method of <c>IHiliteRenderer</c>.</remarks>
    procedure Initialise;
    ///  <summary>Tidies up RTF after all highlighted code processed.</summary>
    ///  <remarks>Method of <c>IHiliteRenderer</c>.</remarks>
    procedure Finalise;
    ///  <summary>Resets styles ready for a new line (paragraph) of highlighted
    ///  code.</summary>
    ///  <remarks>Method of <c>IHiliteRenderer</c>.</remarks>
    procedure BeginLine;
    ///  <summary>Closes current RTF paragraph.</summary>
    ///  <remarks>Method of <c>IHiliteRenderer</c>.</remarks>
    procedure EndLine;
    ///  <summary>Sets any highlighting style required for following source code
    ///  element.</summary>
    ///  <param name="ElemInfo"><c>TSyntaxHiliteElemInfo</c> [in] Provides
    ///  information about the highlighting to be applied to the following
    ///  source code element.</param>
    ///  <remarks>Method of <c>IHiliteRenderer</c>.</remarks>
    procedure BeforeElem(const ElemInfo: TSyntaxHiliteElemInfo);
    ///  <summary>Writes source code element text.</summary>
    ///  <param name="Text"><c>string</c> [in] Text of the source code element.
    ///  </param>
    ///  <remarks>Method of <c>IHiliteRenderer</c>.</remarks>
    procedure WriteElemText(const Text: string);
    ///  <summary>Switches off any highlighting styles that were used for source
    ///  code element just written.</summary>
    ///  <param name="ElemInfo"><c>TSyntaxHiliteElemInfo</c> [in] Provides
    ///  information about the highlighting that was applied to the source code
    ///  element just written.</param>
    ///  <remarks>Method of <c>IHiliteRenderer</c>.</remarks>
    procedure AfterElem(const ElemInfo: TSyntaxHiliteElemInfo);
  end;

type
  ///  <summary>
  ///  Renders highlighted source code in XHTML format. Generated code is
  ///  recorded in a given HTML code builder object.
  ///  </summary>
  ///  <remarks>
  ///  Designed for use with <c>TSyntaxHiliter</c> objects.
  ///  </remarks>
  THTMLHiliteRenderer = class(THiliteRenderer, IHiliteRenderer)
  strict private
    var
      ///  <summary>Object used to record generated XHTML code.</summary>
      fBuilder: THTMLBuilder;
      ///  <summary>Flag indicating if writing first line of output.</summary>
      fIsFirstLine: Boolean;
  public
    ///  <summary>Object constructor. Sets up object to render documents.
    ///  </summary>
    ///  <param name="Builder"><c>THTMLBuilder</c> [in] Object that receives
    ///  generated XHTML code.</param>
    ///  <param name="Brush"><c>TSyntaxHiliterBrush</c> [in] Highlighter brush
    ///  to be used to perform highlighting.</param>
    ///  <param name="Theme"><c>TSyntaxHiliteTheme</c> [in] Styling to be
    ///  applied to highlighted document.</param>
    constructor Create(const Builder: THTMLBuilder;
      const Brush: TSyntaxHiliterBrush; const Theme: TSyntaxHiliteTheme);
    ///  <summary>Initialises XHTML ready to receive highlighted code.</summary>
    ///  <remarks>Method of <c>IHiliteRenderer</c>.</remarks>
    procedure Initialise;
    ///  <summary>Tidies up XHTML after all highlighted code processed.
    ///  </summary>
    ///  <remarks>Method of <c>IHiliteRenderer</c>.</remarks>
    procedure Finalise;
    ///  <summary>Emits new line if necessary.</summary>
    ///  <remarks>Method of <c>IHiliteRenderer</c>.</remarks>
    procedure BeginLine;
    ///  <summary>Does nothing.</summary>
    ///  <remarks>
    ///  <para>Handling of new lines is all done by <c>BeginLine</c>.</para>
    ///  <para>Method of <c>IHiliteRenderer</c>.</para>
    ///  </remarks>
    procedure EndLine;
    ///  <summary>Emits any span tag required to style following source code
    ///  element.</summary>
    ///  <param name="ElemInfo"><c>TSyntaxHiliteElemInfo</c> [in] Provides
    ///  information about the highlighting to be applied to the following
    ///  source code element.</param>
    ///  <remarks>Method of <c>IHiliteRenderer</c>.</remarks>
    procedure BeforeElem(const ElemInfo: TSyntaxHiliteElemInfo);
    ///  <summary>Writes source code element text.</summary>
    ///  <param name="Text"><c>string</c> [in] Text of the source code element.
    ///  </param>
    ///  <remarks>Method of <c>IHiliteRenderer</c>.</remarks>
    procedure WriteElemText(const Text: string);
    ///  <summary>Closes any span tag used to style source code element just
    ///  written.</summary>
    ///  <param name="ElemInfo"><c>TSyntaxHiliteElemInfo</c> [in] Provides
    ///  information about the highlighting that was applied to the source code
    ///  element just written.</param>
    ///  <remarks>Method of <c>IHiliteRenderer</c>.</remarks>
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
  const Brush: TSyntaxHiliterBrush; Renderer: IHiliteRenderer);
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

constructor TSyntaxHiliter.InternalCreate(Renderer: IHiliteRenderer);
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
  Renderer: IHiliteRenderer;  // XHTML renderer object
  Builder: THTMLBuilder;      // object used to construct XHTML document
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
  Renderer: IHiliteRenderer;  // RTF renderer object
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
    { TODO: Find a way of displaying background colour in RTF
        if AttrStyle.Background <> clNone then
          fBuilder.SetColour(AttrStyle.Foreground);
    }
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

{ TDocumentHiliterHelper }

class function TDocumentHiliterHelper.GetHiliterClass(
  const FileType: TSourceOutputFileType): TDocumentHiliterClass;
begin
  case FileType of
    sfRTF: Result := TRTFDocumentHiliter;
    sfHTML: Result := TXHTMLDocumentHiliter;
  else
    Result := TNulDocumentHiliter;
  end;
end;

class function TDocumentHiliterHelper.IsHilitingSupported(
  const FileType: TSourceOutputFileType): Boolean;
begin
  Result := FileType in [sfRTF, sfHTML];
end;

end.

