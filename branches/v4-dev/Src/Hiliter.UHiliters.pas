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
  Hiliter.UGlobals, UBaseObjects, UEncodings, UHTMLBuilder, URTFBuilder;


type
  IHiliteRenderer = interface(IInterface)
    ['{791CE200-C614-40FC-B93D-744ED2984755}']
    procedure Initialise;
    procedure Finalise;
    procedure BeginLine;
    procedure EndLine;
    procedure WriteElemText(const Text: string);
      {Called for each different highlight element in document and is overridden
      to output element's text.
        @param Text [in] Text of the element.
      }
    procedure BeforeElem(Elem: THiliteElement);
      {Called before a highlight element is output. Used to write code to
      display element in required format.
        @param Elem [in] Kind of highlight element.
      }
    procedure AfterElem(Elem: THiliteElement);
      {Called after a highlight element is output. Used to write code to
      finalise element formatting.
        @param Elem [in] Kind of highlight element.
      }
  end;

type
  {
  TSyntaxHiliterFactory:
    Factory class used to create syntax highlighter objects.
  }
  TSyntaxHiliterFactory = class(TNoConstructObject)
  public
    class function CreateHiliter(const Kind: TSyntaxHiliterKind):
      ISyntaxHiliter;
      {Creates syntax highlighter of required kind.
        @param Kind [in] Kind of highlighter required.
        @return Highlighter object.
      }
  end;

type
  TDocumentHiliter = class abstract(TNoConstructObject)
  public
    class function Hilite(const RawCode: string;
      Attrs: IHiliteAttrs = nil; const Title: string = ''): TEncodedData;
      virtual; abstract;
  end;

type
  TDocumentHiliterClass = class of TDocumentHiliter;

type
  TNulDocumentHiliter = class sealed(TDocumentHiliter)
  public
    class function Hilite(const RawCode: string;
      Attrs: IHiliteAttrs = nil; const Title: string = ''): TEncodedData;
      override;
  end;

type
  TXHTMLDocumentHiliter = class sealed(TDocumentHiliter)
  strict private
    class function GenerateCSSRules(Attrs: IHiliteAttrs): string;
  public
    class function Hilite(const RawCode: string;
      Attrs: IHiliteAttrs = nil; const Title: string = ''): TEncodedData;
      override;
  end;

type
  TRTFDocumentHiliter = class sealed(TDocumentHiliter)
  public
    class function Hilite(const RawCode: string;
      Attrs: IHiliteAttrs = nil; const Title: string = ''): TEncodedData;
      override;
  end;

type
  TRTFHiliteRenderer = class(TInterfacedObject, IHiliteRenderer)
  strict private
    fBuilder: TRTFBuilder;
    fAttrs: IHiliteAttrs;
  public
    constructor Create(const Builder: TRTFBuilder;
      const Attrs: IHiliteAttrs = nil);
    procedure Initialise;
    procedure Finalise;
    procedure BeginLine;
    procedure EndLine;
    procedure WriteElemText(const Text: string);
    procedure BeforeElem(Elem: THiliteElement);
    procedure AfterElem(Elem: THiliteElement);
  end;

type
  THTMLHiliteRenderer = class(TInterfacedObject, IHiliteRenderer)
  strict private
    fBuilder: THTMLBuilder;
    fAttrs: IHiliteAttrs;
    fIsFirstLine: Boolean;
  public
    constructor Create(const Builder: THTMLBuilder;
      const Attrs: IHiliteAttrs = nil);
    procedure Initialise;
    procedure Finalise;
    procedure BeginLine;
    procedure EndLine;
    procedure WriteElemText(const Text: string);
    procedure BeforeElem(Elem: THiliteElement);
    procedure AfterElem(Elem: THiliteElement);
  end;

// todo: replace with direct access to class
function CreateRenderedHiliter(Renderer: IHiliteRenderer): ISyntaxHiliter;

implementation


{
  NOTES:

  The class heirachy for syntax highlighter classes in this unit is:

  TSyntaxHiliter - abstract base class
  |
  +-- * TNulHiliter - do nothing class that passes source thru unchanged
  |                   ignores highlight attributes
  |
  +-- TParsedHiliter - abstract base class for classes that parse source code
      |
      +-- TBaseHTMLHiliter - base class for highlighters that generate HTML
      |   |
      |   +-- * TDetailHTMLHiliter - creates HTML fragment for Details pane
      |   |                          uses highlight attributes
      |   |
      |   +-- * TXHTMLHiliter - creates complete XHTML document
      |                         uses highlight attributes
      |
      +-- * TRTFHiliter - creates complete RTF document
                          uses highlight attributes

* indicates a class constructed by factory class

}


uses
  // Delphi
  SysUtils, Graphics,
  // Project
  Hiliter.UAttrs, Hiliter.UCSS, Hiliter.UPasParser, IntfCommon, UCSSBuilder;


type
  {
  TSyntaxHiliter:
    Abstract base class for all syntax highlighter classes. Provides virtual
    abstract methods and a virtual constructor that descendants override. This
    class is provided to give common base class that allows factory class to
    use TSyntaxHiliterClass for object creation.
  }
  TSyntaxHiliter = class(TInterfacedObject)
  protected
    { ISyntaxHiliter methods }
    function Hilite(const RawCode: string; const Attrs: IHiliteAttrs = nil;
      const Title: string = ''): TEncodedData; virtual; abstract;
      {Creates string containing highlighted source code.
        @param RawCode [in] Contains source code to be highlighted.
        @param Attrs [in] Attributes to be used by highlighter. Nil value causes
          a nul highlighter to be used.
        @param Title [in] Optional title to be used as meta data in output
          document. Will be ignored if document type does not support title.
        @return Formatted / highlighted source code with encoding info.
      }
    constructor Create; virtual;
      {Class constructor. Instantiates object. This do-nothing virtual
      constructor is required to enable polymorphism to work for descendant
      classes.
      }
  end;

type
  {
  TSyntaxHiliterClass:
    Class type for syntax highlighters. Used by factory class to create syntax
    highlighter objects of different types.
  }
  TSyntaxHiliterClass = class of TSyntaxHiliter;

type
  {
  TNulHiliter:
    A do nothing highlighter class that passes source code through unchanged.
  }
  TNulHiliter = class(TSyntaxHiliter,
    ISyntaxHiliter
  )
  protected
    { ISyntaxHiliter methods }
    function Hilite(const RawCode: string; const Attrs: IHiliteAttrs = nil;
      const Title: string = ''): TEncodedData; override;
      {Returns provided source code unchanged.
        @param RawCode [in] Contains source code.
        @param Attrs [in] Attributes to be used by highlighter. Ignored.
        @param Title [in] Title of output document. Ignored.
        @return Unchanged source code as encoded data.
      }
  end;

type
  {
  TParsedHiliter:
    Abstract base class for all highlighter classes that parse source code using
    Pascal parser object. Handles parser events and calls virtual methods to
    write the various document parts.
  }
  TParsedHiliter = class(TSyntaxHiliter)
  strict private
    fAttrs: IHiliteAttrs;
      {Reference to highlighter attributes}
    fTitle: string;
      {Document title}
    procedure ElementHandler(Parser: THilitePasParser; Elem: THiliteElement;
      const ElemText: string);
      {Handles parser's OnElement event. Calls virtual do nothing and abstract
      methods that descendants override to write a document element in required
      format.
        @param Parser [in] Reference to parser that triggered event (unused).
        @param Elem [in] Type of element to output.
        @param ElemText [in] Text to be output for element.
      }
    procedure LineBeginHandler(Parser: THilitePasParser);
      {Handles parser's OnLineBegin event. Calls virtual do nothing method that
      descendants override to output data needed to start a new line.
        @param Parser [in] Reference to parser that triggered event (unused).
      }
    procedure LineEndHandler(Parser: THilitePasParser);
      {Handles parser's OnLineEnd event. Calls virtual do nothing method that
      descendants override to output data needed to end a new line.
        @param Parser [in] Reference to parser that triggered event (unused).
      }
  protected
    { ISyntaxHiliter methods }
    function Hilite(const RawCode: string; const Attrs: IHiliteAttrs = nil;
      const Title: string = ''): TEncodedData; override;
      {Creates string containing highlighted source code.
        @param RawCode [in] Contains source code to be highlighted.
        @param Attrs [in] Attributes to be used by highlighter. Nil value causes
          a nul highlighter to be used.
        @param Title [in] Optional title to be used as meta data in output
          document. Will be ignored if document type does not support title.
        @return Formatted / highlighted source code with encoding info.
      }
  strict protected
    function Render: TEncodedData; virtual; abstract;
      {Renders highlighted document as encoded data.
        @return Rendered document with encoding info.
      }
    procedure BeginDoc; virtual;
      {Called just before document is parsed. Used to initialise document.
      }
    procedure EndDoc; virtual;
      {Called after parsing complete. Used to finalise document.
      }
    procedure BeginLine; virtual;
      {Called when a new line in output is started. Used to initialise a line in
      output.
      }
    procedure EndLine; virtual;
      {Called when a line is ending. Used to terminate a line in output.
      }
    procedure WriteElem(const ElemText: string); virtual; abstract;
      {Called for each different highlight element in document and is overridden
      to output element's text.
        @param ElemText [in] Text of the element.
      }
    procedure BeforeElem(Elem: THiliteElement); virtual;
      {Called before a highlight element is output. Used to write code to
      display element in required format.
        @param Elem [in] Kind of highlight element.
      }
    procedure AfterElem(Elem: THiliteElement); virtual;
      {Called after a highlight element is output. Used to write code to
      finalise element formatting.
        @param Elem [in] Kind of highlight element.
      }
    property Attrs: IHiliteAttrs read fAttrs;
      {Object storing attributes of highlighter. Defines appearance of document
      and each highlight element}
    property Title: string read fTitle;
      {Title of document. May be ignored if document type doesn't support title
      meta data}
  public
    constructor Create; override;
      {Class constructor. Sets up object.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
  end;

type
  TRenderedHiliter = class(TParsedHiliter, ISyntaxHiliter)
  strict private
    fRenderer: IHiliteRenderer;
  strict protected
    function Render: TEncodedData; override;
    procedure BeginDoc; override;
    procedure EndDoc; override;
    procedure BeginLine; override;
    procedure EndLine; override;
    procedure WriteElem(const ElemText: string); override;
    procedure BeforeElem(Elem: THiliteElement); override;
    procedure AfterElem(Elem: THiliteElement); override;
  public
    constructor Create(Renderer: IHiliteRenderer); reintroduce;
  end;

function CreateRenderedHiliter(Renderer: IHiliteRenderer): ISyntaxHiliter;
begin
  Result := TRenderedHiliter.Create(Renderer);
end;

type
  {
  TRTFHiliter:
    Highlighter object used to write highlighted rich text.
  }
  TRTFHiliter = class(TParsedHiliter,
    ISyntaxHiliter
  )
  strict private
    fRTFBuilder: TRTFBuilder;
      {Object used to construct rich text document}
  strict protected
    function Render: TEncodedData; override;
      {Renders highlighted document as encoded data.
        @return Rendered document with encoding info.
      }
    procedure BeginDoc; override;
      {Called just before document is parsed. Used to emit RTF header.
      }
    procedure BeginLine; override;
      {Called when a new line in output is started. Used to initialise a line in
      output.
      }
    procedure EndLine; override;
      {Called when a line is ending. Writes end of line RTF control.
      }
    procedure WriteElem(const ElemText: string); override;
      {Outputs element's text.
        @param ElemText [in] Text of the element.
      }
    procedure BeforeElem(Elem: THiliteElement); override;
      {Called before a highlight element is output. Used to emit RTF controls
      that define style of element.
        @param Elem [in] Kind of highlight element.
      }
    procedure AfterElem(Elem: THiliteElement); override;
      {Called after a highlight element is output. Used to write code to
      finalise element formatting.
        @param Elem [in] Kind of highlight element.
      }
  public
    constructor Create; override;
      {Class constructor. Sets up object.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
  end;

type
  {
  TBaseHTMLHiliter:
    Base class for all highlighters that emit HTML. Provides some common
    functionality. Emits HTML for highlighted source code only, not a complete
    HTML document. No CSS definitions are written since these are expected to
    have been provided elsewhere and match standard highlighter class names.
    (THiliterCSS is used to provide CSS class names and definitions).
  }
  TBaseHTMLHiliter = class(TParsedHiliter)
  strict private
    fIsFirstLine: Boolean;
      {Records whether we are about to write first line}
    fHTMLBuilder: THTMLBuilder;
      {Object used to construct XHTML document}
  strict protected
    function Render: TEncodedData; override; abstract;
      {Renders highlighted document as encoded data.
        @return Rendered document with encoding info.
      }
    procedure BeginDoc; override;
      {Called just before document is parsed. Used to initialise document.
      }
    procedure BeginLine; override;
      {Called when a new line in output is started. Writes new line where
      required.
      }
    procedure WriteElem(const ElemText: string); override;
      {Outputs element's text.
        @param ElemText [in] Text of the element.
      }
    procedure BeforeElem(Elem: THiliteElement); override;
      {Called before a highlight element is output. Used to write span tag for
      any CSS class that is required.
        @param Elem [in] Kind of highlight element.
      }
    procedure AfterElem(Elem: THiliteElement); override;
      {Called after a highlight element is output. Writes closing span tag if
      required.
        @param Elem [in] Kind of highlight element.
      }
    property IsFirstLine: Boolean read fIsFirstLine;
      {Flag true when line to be written is first line and false after that}
    property HTMLBuilder: THTMLBuilder read fHTMLBuilder;
      {Object used to construct XHTML document}
  public
    constructor Create; override;
      {Class constructor. Sets up object.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
  end;

type
  {
  TDetailHTMLHiliter:
    Highlighter that is used to display source code in program's Details pane.
    The class produces HTML suitable for use in HTML template document,
    therefore it does not emit a complete document, just HTML between
    <pre>..</pre> tags.
  }
  TDetailHTMLHiliter = class(TBaseHTMLHiliter,
    ISyntaxHiliter
  )
  strict protected
    function Render: TEncodedData; override;
      {Renders highlighted document as encoded data.
        @return Rendered document with encoding info.
      }
    procedure BeginDoc; override;
      {Called just before document is parsed. Used to write opening pre tag.
      }
    procedure EndDoc; override;
      {Called after parsing complete. Writes out HTML.
      }
  end;

type
  {
  TXHTMLHiliter:
    Highlighter that emits a complete XHTML document containing the source code.
    It creates an emebedded style sheet containing information from given
    highlight attribute objects and then writes code that uses these classes for
    formatting.
  }
  TXHTMLHiliter = class(TBaseHTMLHiliter,
    ISyntaxHiliter
  )
  strict private
    function GenerateCSSRules: string;
      {Generates CSS rule for use in head section of HTML document.
        @return List of CSS class rules.
      }
  strict protected
    function Render: TEncodedData; override;
      {Renders highlighted document as encoded data.
        @return Rendered document with encoding info.
      }
    procedure BeginDoc; override;
      {Called just before document is parsed. Used to write XHTML code for
      document head section and first part of body.
      }
    procedure EndDoc; override;
      {Called after parsing complete. Writes XHTML that closes document.
      }
  end;


{ TSyntaxHiliterFactory }

class function TSyntaxHiliterFactory.CreateHiliter(
  const Kind: TSyntaxHiliterKind): ISyntaxHiliter;
  {Creates syntax highlighter of required kind.
    @param Kind [in] Kind of highlighter required.
    @return Highlighter object.
  }
const
  // Map of highlighter kinds to highlighter classes
  cHiliterMap: array[TSyntaxHiliterKind] of TSyntaxHiliterClass = (
    TNulHiliter,
    TDetailHTMLHiliter,
    TXHTMLHiliter,
    TRTFHiliter
  );
var
  Obj: TSyntaxHiliter;  // created object
begin
  Obj := cHiliterMap[Kind].Create;  // create object
  Result := Obj as ISyntaxHiliter;  // return ISyntaxHiliter interface to object
end;

{ TSyntaxHiliter }

constructor TSyntaxHiliter.Create;
  {Class constructor. Instantiates object. This do-nothing virtual constructor
  is required to enable polymorphism to work for descendant classes.
  }
begin
  inherited;
  // Do nothing ** Do not remove - required for polymorphism to work **
end;

{ TNulHiliter }

function TNulHiliter.Hilite(const RawCode: string; const Attrs: IHiliteAttrs;
  const Title: string): TEncodedData;
  {Returns provided source code unchanged.
    @param RawCode [in] Contains source code.
    @param Attrs [in] Attributes to be used by highlighter. Ignored.
    @param Title [in] Title of output document. Ignored.
    @return Unchanged source code as encoded data.
  }
begin
  Result := TEncodedData.Create(RawCode, etUnicode);
end;

{ TParsedHiliter }

procedure TParsedHiliter.AfterElem(Elem: THiliteElement);
  {Called after a highlight element is output. Used to write code to finalise
  element formatting.
    @param Elem [in] Kind of highlight element.
  }
begin
  // Do nothing: descendants override
end;

procedure TParsedHiliter.BeforeElem(Elem: THiliteElement);
  {Called before a highlight element is output. Used to write code to display
  element in required format.
    @param Elem [in] Kind of highlight element.
  }
begin
  // Do nothing: descendants override
end;

procedure TParsedHiliter.BeginDoc;
  {Called just before document is parsed. Used to initialise document.
  }
begin
  // Do nothing: descendants override
end;

procedure TParsedHiliter.BeginLine;
  {Called when a new line in output is started. Used to initialise a line in
  output.
  }
begin
  // Do nothing: descendants override
end;

constructor TParsedHiliter.Create;
  {Class constructor. Sets up object.
  }
begin
  inherited;
  // Create nul highlighter object used by default
  fAttrs := THiliteAttrsFactory.CreateNulAttrs;
end;

destructor TParsedHiliter.Destroy;
  {Class destructor. Tears down object.
  }
begin
  fAttrs := nil;
  inherited;
end;

procedure TParsedHiliter.ElementHandler(Parser: THilitePasParser;
  Elem: THiliteElement; const ElemText: string);
  {Handles parser's OnElement event. Calls virtual do nothing and abstract
  methods that descendants override to write a document element in required
  format.
    @param Parser [in] Reference to parser that triggered event (unused).
    @param Elem [in] Type of element to output.
    @param ElemText [in] Text to be output for element.
  }
begin
  BeforeElem(Elem);
  WriteElem(ElemText);
  AfterElem(Elem);
end;

procedure TParsedHiliter.EndDoc;
  {Called after parsing complete. Used to finalise document.
  }
begin
  // Do nothing: descendants override
end;

procedure TParsedHiliter.EndLine;
  {Called when a line is ending. Used to terminate a line in output.
  }
begin
  // Do nothing: descendants override
end;

function TParsedHiliter.Hilite(const RawCode: string; const Attrs: IHiliteAttrs;
  const Title: string): TEncodedData;
  {Creates string containing highlighted source code.
    @param RawCode [in] Contains source code to be highlighted.
    @param Attrs [in] Attributes to be used by highlighter. Nil value causes a
      nul highlighter to be used.
    @param Title [in] Optional title to be used as meta data in output document.
      Will be ignored if document type does not support title.
    @return Formatted / highlighted source code with encoding info.
  }
var
  Parser: THilitePasParser;   // object used to parse source
begin
  // Record copy of highlighter attributes and title
  (fAttrs as IAssignable).Assign(Attrs);
  fTitle := Title;
  Parser := nil;
  try
    // Create parser
    Parser := THilitePasParser.Create;
    Parser.OnElement := ElementHandler;
    Parser.OnLineBegin := LineBeginHandler;
    Parser.OnLineEnd := LineEndHandler;
    // Parse the document:
    BeginDoc;   // overridden in descendants to initialise document
    Parser.Parse(RawCode);
    EndDoc;     // overridden in descendants to finalise document
    // Return string stored in destination stream
    Result := Render;
  finally
    Parser.Free;
  end;
end;

procedure TParsedHiliter.LineBeginHandler(Parser: THilitePasParser);
  {Handles parser's OnLineBegin event. Calls virtual do nothing method that
  descendants override to output data needed to start a new line.
    @param Parser [in] Reference to parser that triggered event (unused).
  }
begin
  BeginLine;
end;

procedure TParsedHiliter.LineEndHandler(Parser: THilitePasParser);
  {Handles parser's OnLineEnd event. Calls virtual do nothing method that
  descendants override to output data needed to end a new line.
    @param Parser [in] Reference to parser that triggered event (unused).
  }
begin
  EndLine;
end;

{ TRTFHiliter }

procedure TRTFHiliter.AfterElem(Elem: THiliteElement);
  {Called after a highlight element is output. Used to write code to finalise
  element formatting.
    @param Elem [in] Kind of highlight element.
  }
begin
  if not Attrs[Elem].IsNul then
    fRTFBuilder.EndGroup;
end;

procedure TRTFHiliter.BeforeElem(Elem: THiliteElement);
  {Called before a highlight element is output. Used to emit RTF controls that
  define style of element.
    @param Elem [in] Kind of highlight element.
  }
begin
  if not Attrs[Elem].IsNul then
  begin
    fRTFBuilder.BeginGroup;
    if Attrs[Elem].ForeColor <> clNone then
      fRTFBuilder.SetColour(Attrs[Elem].ForeColor);
    if Attrs[Elem].FontStyle <> [] then
      fRTFBuilder.SetFontStyle(Attrs[Elem].FontStyle);
  end;
end;

procedure TRTFHiliter.BeginDoc;
  {Called just before document is parsed. Used to emit RTF header.
  }
var
  Elem: THiliteElement; // loops thru all highlight elements
begin
  // Set up font table
  fRTFBuilder.FontTable.Add(Attrs.FontName, rgfModern, 0);
  // Set up colour table
  for Elem := Low(THiliteElement) to High(THiliteElement) do
    fRTFBuilder.ColourTable.Add(Attrs[Elem].ForeColor);
  // Add any document title
  fRTFBuilder.DocProperties.Title := Title;
  // Clear any paragraph formatting
  fRTFBuilder.ClearParaFormatting;
end;

procedure TRTFHiliter.BeginLine;
  {Called when a new line in output is started. Used to initialise a line in
  output.
  }
begin
  fRTFBuilder.ResetCharStyle;
  fRTFBuilder.SetFont(Attrs.FontName);
  fRTFBuilder.SetFontSize(Attrs.FontSize);
end;

constructor TRTFHiliter.Create;
  {Class constructor. Sets up object.
  }
begin
  inherited;
  fRTFBuilder := TRTFBuilder.Create(0); // uses default code page
end;

destructor TRTFHiliter.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fRTFBuilder);
  inherited;
end;

procedure TRTFHiliter.EndLine;
  {Called when a line is ending. Writes end of line RTF control.
  }
begin
  fRTFBuilder.EndPara;
end;

function TRTFHiliter.Render: TEncodedData;
  {Renders highlighted document as encoded data.
    @return Rendered document with encoding info.
  }
begin
  Result := TEncodedData.Create(fRTFBuilder.Render.ToBytes, etASCII);
end;

procedure TRTFHiliter.WriteElem(const ElemText: string);
  {Outputs element's text.
    @param ElemText [in] Text of the element.
  }
begin
  fRTFBuilder.AddText(ElemText);
end;

{ TBaseHTMLHiliter }

procedure TBaseHTMLHiliter.AfterElem(Elem: THiliteElement);
  {Called after a highlight element is output. Writes closing span tag if
  required.
    @param Elem [in] Kind of highlight element.
  }
begin
  if not Attrs[Elem].IsNul then
    HTMLBuilder.CloseSpan;
end;

procedure TBaseHTMLHiliter.BeforeElem(Elem: THiliteElement);
  {Called before a highlight element is output. Used to write span tag for any
  CSS class that is required.
    @param Elem [in] Kind of highlight element.
  }
begin
  if not Attrs.Elements[Elem].IsNul then
    HTMLBuilder.OpenSpan(THiliterCSS.GetElemCSSClassName(Elem));
end;

procedure TBaseHTMLHiliter.BeginDoc;
  {Called just before document is parsed. Used to initialise document.
  }
begin
  // Note that we are about to write first line
  fIsFirstLine := True;
end;

procedure TBaseHTMLHiliter.BeginLine;
  {Called when a new line in output is started. Writes new line where required.
  }
begin
  // Note we don't emit CRLF before first line since it must be on same line as
  // <pre> tag that will have been written.
  if fIsFirstLine then
    fIsFirstLine := False
  else
    HTMLBuilder.NewLine;
end;

constructor TBaseHTMLHiliter.Create;
  {Class constructor. Sets up object.
  }
begin
  inherited;
  fHTMLBuilder := THTMLBuilder.Create;
end;

destructor TBaseHTMLHiliter.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fHTMLBuilder);
  inherited;
end;

procedure TBaseHTMLHiliter.WriteElem(const ElemText: string);
  {Outputs element's text.
    @param ElemText [in] Text of the element.
  }
begin
  HTMLBuilder.AddText(ElemText);
end;

{ TDetailHTMLHiliter }

procedure TDetailHTMLHiliter.BeginDoc;
  {Called just before document is parsed. Used to write opening pre tag.
  }
begin
  inherited;
  HTMLBuilder.OpenPre(THiliterCSS.GetMainCSSClassName);
end;

procedure TDetailHTMLHiliter.EndDoc;
  {Called after parsing complete. Writes out HTML.
  }
begin
  inherited;
  HTMLBuilder.ClosePre;
end;

function TDetailHTMLHiliter.Render: TEncodedData;
  {Renders highlighted document as encoded data.
    @return Rendered document with encoding info.
  }
begin
  Result := TEncodedData.Create(HTMLBuilder.HTMLFragment, etUTF8);
end;

{ TXHTMLHiliter }

procedure TXHTMLHiliter.BeginDoc;
  {Called just before document is parsed. Used to write XHTML code for document
  head section and first part of body.
  }
resourcestring
  // Default document title
  sDefaultTitle = 'DelphiDabbler CodeSnip Database';
begin
  inherited;
  if Title <> '' then
    HTMLBuilder.Title := Title
  else
    HTMLBuilder.Title := sDefaultTitle;
  HTMLBuilder.CSS := GenerateCSSRules;
  HTMLBuilder.OpenPre(THiliterCSS.GetMainCSSClassName);
end;

procedure TXHTMLHiliter.EndDoc;
  {Called after parsing complete. Writes XHTML that closes document.
  }
begin
  inherited;
  HTMLBuilder.ClosePre;
end;

function TXHTMLHiliter.GenerateCSSRules: string;
  {Generates CSS rule for use in head section of HTML document.
    @return List of CSS class rules.
  }
var
  CSSBuilder: TCSSBuilder;  // builds CSS code
  HiliterCSS: THiliterCSS;  // generates CSS names and properties for hiliter
begin
  HiliterCSS := THiliterCSS.Create(Attrs);
  CSSBuilder := nil;
  try
    CSSBuilder := TCSSBuilder.Create;
    HiliterCSS.BuildCSS(CSSBuilder);
    Result := CSSBuilder.AsString;
  finally
    FreeAndNil(CSSBuilder);
    FreeAndNil(HiliterCSS);
  end;
end;

function TXHTMLHiliter.Render: TEncodedData;
  {Renders highlighted document as encoded data.
    @return Rendered document with encoding info.
  }
begin
  Result := TEncodedData.Create(HTMLBuilder.HTMLDocument, etUTF8);
end;

{ TNulDocumentHiliter }

class function TNulDocumentHiliter.Hilite(const RawCode: string;
  Attrs: IHiliteAttrs; const Title: string): TEncodedData;
var
  Hiliter: ISyntaxHiliter;
begin
  Hiliter := TNulHiliter.Create;
  Result := Hiliter.Hilite(RawCode, Attrs, Title);
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
  Hiliter: ISyntaxHiliter;
  Renderer: IHiliteRenderer;
  Builder: THTMLBuilder;
begin
  Builder := THTMLBuilder.Create;
  try
    if Title <> '' then
      Builder.Title := Title
    else
      Builder.Title := sDefaultTitle;
    Builder.CSS := GenerateCSSRules(Attrs);
    Renderer := THTMLHiliteRenderer.Create(Builder, Attrs);
    Hiliter := TRenderedHiliter.Create(Renderer);
    Hiliter.Hilite(RawCode, Attrs);
    Result := TEncodedData.Create(Builder.HTMLDocument, etUTF8);
  finally
    Builder.Free;
  end;
//  Hiliter := TXHTMLHiliter.Create;
//  Result := Hiliter.Hilite(RawCode, Attrs, Title);
end;

{ TRTFDocumentHiliter }

class function TRTFDocumentHiliter.Hilite(const RawCode: string;
  Attrs: IHiliteAttrs; const Title: string): TEncodedData;
var
  Hiliter: ISyntaxHiliter;
  Renderer: IHiliteRenderer;
  Builder: TRTFBuilder;
begin
  Builder := TRTFBuilder.Create(0);
  try
    Builder.DocProperties.Title := Title;
    Renderer := TRTFHiliteRenderer.Create(Builder, Attrs);
    Hiliter := TRenderedHiliter.Create(Renderer);
    Hiliter.Hilite(RawCode, Attrs);
    Result := TEncodedData.Create(Builder.Render.ToBytes, etASCII);
  finally
    Builder.Free;
  end;
end;

{ TRenderedHiliter }

procedure TRenderedHiliter.AfterElem(Elem: THiliteElement);
begin
  fRenderer.AfterElem(Elem);
end;

procedure TRenderedHiliter.BeforeElem(Elem: THiliteElement);
begin
  fRenderer.BeforeElem(Elem);
end;

procedure TRenderedHiliter.BeginDoc;
begin
  fRenderer.Initialise;
end;

procedure TRenderedHiliter.BeginLine;
begin
  fRenderer.BeginLine;
end;

constructor TRenderedHiliter.Create(Renderer: IHiliteRenderer);
begin
  Assert(Assigned(Renderer), ClassName + '.Create: Renderer is nil');
  inherited Create;
  fRenderer := Renderer;
end;

procedure TRenderedHiliter.EndDoc;
begin
  fRenderer.Finalise;
end;

procedure TRenderedHiliter.EndLine;
begin
  fRenderer.EndLine;
end;

function TRenderedHiliter.Render: TEncodedData;
begin
  Result := TEncodedData.Create(TBytes.Create(), etSysDefault);
end;

procedure TRenderedHiliter.WriteElem(const ElemText: string);
begin
  fRenderer.WriteElemText(ElemText);
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

