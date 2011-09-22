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
  TSyntaxHiliter = class sealed(TInterfacedObject, ISyntaxHiliter)
  strict private
    var
      fRenderer: IHiliteRenderer;
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
  public
    constructor Create(Renderer: IHiliteRenderer);
      {Class constructor. Sets up object.
      }
    { ISyntaxHiliter method }
    procedure Hilite(const RawCode: string);
      {Highlights source code using renderer passed to constructor. Output is
      dealt with by renderer.
        @param RawCode [in] Source code to be highlighted.
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


implementation


uses
  // Delphi
  SysUtils, Graphics,
  // Project
  Hiliter.UAttrs, Hiliter.UCSS, IntfCommon, UCSSBuilder;


{ TSyntaxHiliter }

constructor TSyntaxHiliter.Create(Renderer: IHiliteRenderer);
  {Class constructor. Sets up object.
  }
begin
  Assert(Assigned(Renderer), ClassName + '.Create: Renderer is nil');
  inherited Create;
  fRenderer := Renderer;
end;

procedure TSyntaxHiliter.ElementHandler(Parser: THilitePasParser;
  Elem: THiliteElement; const ElemText: string);
  {Handles parser's OnElement event. Calls virtual do nothing and abstract
  methods that descendants override to write a document element in required
  format.
    @param Parser [in] Reference to parser that triggered event (unused).
    @param Elem [in] Type of element to output.
    @param ElemText [in] Text to be output for element.
  }
begin
  fRenderer.BeforeElem(Elem);
  fRenderer.WriteElemText(ElemText);
  fRenderer.AfterElem(Elem);
end;

procedure TSyntaxHiliter.Hilite(const RawCode: string);
var
  Parser: THilitePasParser;   // object used to parse source
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
  {Handles parser's OnLineBegin event. Calls virtual do nothing method that
  descendants override to output data needed to start a new line.
    @param Parser [in] Reference to parser that triggered event (unused).
  }
begin
  fRenderer.BeginLine;
end;

procedure TSyntaxHiliter.LineEndHandler(Parser: THilitePasParser);
  {Handles parser's OnLineEnd event. Calls virtual do nothing method that
  descendants override to output data needed to end a new line.
    @param Parser [in] Reference to parser that triggered event (unused).
  }
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
  Hiliter: ISyntaxHiliter;
  Renderer: IHiliteRenderer;
  Builder: TRTFBuilder;
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

