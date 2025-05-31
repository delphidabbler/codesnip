{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2025, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that renders a HTML document that describes a snippet.
}


unit UHTMLSnippetDoc;

interface

uses
  // Delphi
  SysUtils,
  Graphics,
  // Project
  ActiveText.UHTMLRenderer,
  ActiveText.UMain,
  DB.Vaults,
  Hiliter.UGlobals,
  UColours,
  UEncodings,
  UHTMLBuilder,
  UHTMLUtils,
  UIStringList,
  USnippetDoc;

type
  THTMLSnippetDocClass = class of THTMLSnippetDoc;

  ///  <summary>Abstract base class for classes that render a document that
  ///  describes a snippet using HTML.</summary>
  THTMLSnippetDoc = class abstract (TSnippetDoc)
  strict private
    var
      ///  <summary>Attributes that determine the formatting of highlighted
      ///  source code.</summary>
      fHiliteAttrs: IHiliteAttrs;
      ///  <summary>Flag indicates whether to output in colour.</summary>
      fUseColour: Boolean;
      ///  <summary>Object used to build HTML source code document.</summary>
      fDocument: TStringBuilder;
      ///  <summary>Type of class used to generate the HTML of the snippet's
      ///  source code and to provide addition HTML information.</summary>
      fBuilderClass: THTMLBuilderClass;
      ///  <summary>Static class used to generate HTML tags.</summary>
      fTagGen: THTMLClass;
    const
      ///  <summary>Colour of plain text in the HTML document.</summary>
      TextColour = clBlack;
      ///  <summary>Colour of HTML links in the document.</summary>
      LinkColour = clExternalLink;
      ///  <summary>Colour of warning text in the HTML document.</summary>
      WarningColour = clWarningText;
      ///  <summary>Colour used for &lt;var&gt; tags in the HTML document.
      ///  </summary>
      VarColour = clVarText;

      // Names of various HTML tags used in the document
      HTMLTag = 'html';
      HeadTag = 'head';
      TitleTag = 'title';
      BodyTag = 'body';
      H1Tag = 'h1';
      H2Tag = 'h2';
      DivTag = 'div';
      ParaTag = 'p';
      StrongTag = 'strong';
      EmphasisTag = 'em';
      CodeTag = 'code';
      LinkTag = 'a';
      StyleTag = 'style';
      TableTag = 'table';
      TableBodyTag = 'tbody';
      TableRowTag = 'tr';
      TableColTag = 'td';

      // Names of HTML attributes used in the document
      ClassAttr = 'class';
      StyleAttr = 'style';

      // Names of HTML classes used in the document
      VaultInfoClass = 'vault-info';
      IndentClass = 'indent';
      WarningClass = 'warning';

      ///  <summary>Name of document body font.</summary>
      BodyFontName = 'Tahoma';
      ///  <summary>Size of paragraph font, in points.</summary>
      BodyFontSize = 10;  // points
      ///  <summary>Size of H1 heading font, in points.</summary>
      H1FontSize = 14;    // points
      ///  <summary>Size of H2 heading font, in points.</summary>
      H2FontSize = 12;    // points
      ///  <summary>Size of font used for vault information, in points.
      ///  </summary>
      VaultInfoFontSize = 9; // points

  strict private
    ///  <summary>Creates and returns the inline CSS used in the HTML document.
    ///  </summary>
    function BuildCSS: string;
    ///  <summary>Renders the given active text as HTML.</summary>
    function ActiveTextToHTML(ActiveText: IActiveText): string;
  strict protected
    ///  <summary>Returns a reference to the builder class used to create the
    ///  required flavour of HTML.</summary>
    function BuilderClass: THTMLBuilderClass; virtual; abstract;
    ///  <summary>Initialises the HTML document.</summary>
    procedure InitialiseDoc; override;
    ///  <summary>Output given heading, i.e. snippet name for snippet from a
    ///  given vault.</summary>
    ///  <remarks>Heading may be rendered differently depending on the snippet's
    ///  vault.</remarks>
    procedure RenderHeading(const Heading: string;
      const AVaultID: TVaultID); override;
    ///  <summary>Adds the given snippet description to the document.</summary>
    ///  <remarks>Active text formatting is observed and styled to suit the
    ///  document.</remarks>
    procedure RenderDescription(const Desc: IActiveText); override;
    ///  <summary>Highlights the given source code and adds it to the document.
    ///  </summary>
    procedure RenderSourceCode(const SourceCode: string); override;
    ///  <summary>Adds the given title, followed by the given text, to the
    ///  document.</summary>
    procedure RenderTitledText(const Title, Text: string); override;
    ///  <summary>Adds a comma-separated list of text, preceded by the given
    ///  title, to the document.</summary>
    procedure RenderTitledList(const Title: string; List: IStringList);
      override;
    ///  <summary>Outputs the given compiler test info, preceded by the given
    ///  heading.</summary>
    procedure RenderCompilerInfo(const Heading: string;
      const Info: TCompileDocInfoArray); override;
    ///  <summary>Outputs the given message stating that there is no compiler
    ///  test info, preceded by the given heading.</summary>
    procedure RenderNoCompilerInfo(const Heading, NoCompileTests: string);
      override;
    ///  <summary>Adds the given extra information about the snippet to the
    ///  document.</summary>
    ///  <remarks>Active text formatting is observed and styled to suit the
    ///  document.</remarks>
    procedure RenderExtra(const ExtraText: IActiveText); override;
    ///  <summary>Output given information about a vault.</summary>
    procedure RenderVaultInfo(const Text: string); override;
    ///  <summary>Finalises the document and returns its content as encoded
    ///  data.</summary>
    function FinaliseDoc: TEncodedData; override;
  public
    ///  <summary>Constructs an object to render snippet information.</summary>
    ///  <param name="HiliteAttrs"><c>IHiliteAttrs</c> [in] Defines the style of
    ///  syntax highlighting to be used for the source code.</param>
    ///  <param name="UseColour"><c>Boolean</c> [in] Set <c>True</c> to render
    ///  the document in colour or <c>False</c> for black and white.</param>
    constructor Create(const HiliteAttrs: IHiliteAttrs;
      const UseColour: Boolean = True);
    ///  <summary>Destroys the object.</summary>
    destructor Destroy; override;
  end;

  ///  <summary>Class that renders a document that describes a snippet using
  ///  XHTML.</summary>
  TXHTMLSnippetDoc = class sealed (THTMLSnippetDoc)
  strict protected
    ///  <summary>Returns a reference to the builder class used to create valid
    ///  XHTML.</summary>
    function BuilderClass: THTMLBuilderClass; override;
  end;

  ///  <summary>Class that renders a document that describes a snippet using
  ///  HTML 5.</summary>
  THTML5SnippetDoc = class sealed (THTMLSnippetDoc)
  strict protected
    ///  <summary>Returns a reference to the builder class used to create valid
    ///  HTML 5.</summary>
    function BuilderClass: THTMLBuilderClass; override;
  end;

implementation

uses
  // Project
  Hiliter.UCSS,
  Hiliter.UHiliters,
  UCSSBuilder,
  UCSSUtils,
  UFontHelper,
  UPreferences;

{ THTMLSnippetDoc }

function THTMLSnippetDoc.ActiveTextToHTML(ActiveText: IActiveText): string;
var
  HTMLWriter: TActiveTextHTML;  // Object that generates HTML from active text
begin
  HTMLWriter := TActiveTextHTML.Create(fTagGen);
  try
    Result := HTMLWriter.Render(ActiveText);
  finally
    HTMLWriter.Free;
  end;
end;

function THTMLSnippetDoc.BuildCSS: string;
var
  CSS: TCSSBuilder;
  HiliterCSS: THiliterCSS;
  BodyFont: TFont;                // default content font sized per preferences
  MonoFont: TFont;                // default mono font sized per preferences
begin
  BodyFont := nil;
  MonoFont := nil;
  CSS := TCSSBuilder.Create;
  try
    MonoFont := TFont.Create;
    TFontHelper.SetDefaultMonoFont(MonoFont);
    BodyFont := TFont.Create;
    BodyFont.Name := BodyFontName;
    BodyFont.Size := BodyFontSize;
    MonoFont.Size := BodyFontSize;

    // <body> tag style
    CSS.AddSelector(BodyTag)
      .AddProperty(TCSS.FontProps(BodyFont))
      .AddProperty(TCSS.ColorProp(TextColour));
    // <h1> tag style
    CSS.AddSelector(H1Tag)
      .AddProperty(TCSS.FontSizeProp(H1FontSize))
      .AddProperty(TCSS.FontWeightProp(cfwBold))
      .AddProperty(TCSS.MarginProp(0.75, 0, 0.75, 0, cluEm));
    // <h2> tag
    CSS.AddSelector(H2Tag)
      .AddProperty(TCSS.FontSizeProp(H2FontSize));
    // <p> tag style
    CSS.AddSelector(ParaTag)
      .AddProperty(TCSS.MarginProp(0.5, 0, 0.5, 0, cluEm));
    // <table> tag style
    // note: wanted to use :last-child to style right column, but not supported
    // by TWebBrowser that is used for the preview
    CSS.AddSelector(TableTag)
      .AddProperty(TCSS.MarginProp(0.5, 0, 0.5, 0, cluEm));
    CSS.AddSelector(TableColTag)
      .AddProperty(TCSS.PaddingProp(cssRight, 0.5, cluEm))
      .AddProperty(TCSS.PaddingProp(cssLeft, 0));
    // <code> tag style
    CSS.AddSelector(CodeTag)
      .AddProperty(TCSS.FontProps(MonoFont));
    // <a> tag style
    CSS.AddSelector(LinkTag)
      .AddProperty(TCSS.ColorProp(LinkColour))
      .AddProperty(TCSS.TextDecorationProp([ctdUnderline]));
    // <var> tag style
    CSS.AddSelector('var')
      .AddProperty(TCSS.ColorProp(VarColour))
      .AddProperty(TCSS.FontStyleProp(cfsItalic));

    // Set active text list classes

    // list styling
    CSS.AddSelector('ul, ol')
      .AddProperty(TCSS.MarginProp(0.5, 0, 0.5, 0, cluEm))
      .AddProperty(TCSS.PaddingProp(cssAll, 0))
      .AddProperty(TCSS.PaddingProp(cssLeft, 1.5, cluEm))
      .AddProperty(TCSS.ListStylePositionProp(clspOutside))
      .AddProperty(TCSS.ListStyleTypeProp(clstDisc));
    CSS.AddSelector('ul')
      .AddProperty(TCSS.ListStyleTypeProp(clstDisc));
    CSS.AddSelector('ol')
      .AddProperty(TCSS.ListStyleTypeProp(clstDecimal));
    CSS.AddSelector('li')
      .AddProperty(TCSS.PaddingProp(cssAll, 0))
      .AddProperty(TCSS.MarginProp(0.25, 0, 0.25, 0, cluEm));
    CSS.AddSelector('li ol, li ul')
      .AddProperty(TCSS.MarginProp(0.25, 0, 0.25, 0, cluEm));
    CSS.AddSelector('li li')
      .AddProperty(TCSS.PaddingProp(cssLeft, 0))
      .AddProperty(TCSS.MarginProp(0));

    // class used for smaller text describing vault
    CSS.AddSelector('.' + VaultInfoClass)
      .AddProperty(TCSS.FontSizeProp(VaultInfoFontSize))
      .AddProperty(TCSS.FontStyleProp(cfsItalic));
    // class used to indent tag content
    CSS.AddSelector('.' + IndentClass)
      .AddProperty(TCSS.MarginProp(cssLeft, 1.5, cluEm));

    // default active text classes
    CSS.AddSelector('.' + WarningClass)
      .AddProperty(TCSS.ColorProp(WarningColour))
      .AddProperty(TCSS.FontWeightProp(cfwBold));

    // CSS used by highlighters
    fHiliteAttrs.FontSize := BodyFontSize;
    HiliterCSS := THiliterCSS.Create(fHiliteAttrs);
    try
      HiliterCSS.BuildCSS(CSS);
    finally
      HiliterCSS.Free;
    end;

    Result := CSS.AsString;
  finally
    BodyFont.Free;
    MonoFont.Free;
    CSS.Free;
  end;
end;

constructor THTMLSnippetDoc.Create(const HiliteAttrs: IHiliteAttrs;
  const UseColour: Boolean);
begin
  inherited Create;
  fDocument := TStringBuilder.Create;
  fBuilderClass := BuilderClass;
  fTagGen := BuilderClass.TagGenerator;
  fHiliteAttrs := HiliteAttrs;
  fUseColour := UseColour;
end;

destructor THTMLSnippetDoc.Destroy;
begin
  fDocument.Free;
  inherited;
end;

function THTMLSnippetDoc.FinaliseDoc: TEncodedData;
begin
  // </body>
  fDocument.AppendLine(fTagGen.ClosingTag(BodyTag));
  // </html>
  fDocument.AppendLine(fTagGen.ClosingTag(HTMLTag));

  Result := TEncodedData.Create(fDocument.ToString, etUTF8);
end;

procedure THTMLSnippetDoc.InitialiseDoc;
resourcestring
  sTitle = 'Snippet Information';
begin
  // doc type etc
  fDocument.AppendLine(BuilderClass.Preamble);
  // <html>
  fDocument.AppendLine(fTagGen.OpeningTag(HTMLTag, BuilderClass.HTMLTagAttrs));
  // <head>
  fDocument.AppendLine(fTagGen.OpeningTag(HeadTag));
  // <meta /> ..
  fDocument.AppendLine(BuilderClass.MetaTags);
  // <title />
  fDocument.AppendLine(fTagGen.CompoundTag(TitleTag, fTagGen.Entities(sTitle)));
  // <style>
  fDocument.AppendLine(
    fTagGen.OpeningTag(StyleTag, THTMLAttributes.Create('type', 'text/css'))
  );
  fDocument.Append(BuildCSS);
  // </style>
  fDocument.AppendLine(fTagGen.ClosingTag(StyleTag));
  // </head>
  fDocument.AppendLine(fTagGen.ClosingTag(HeadTag));
  // <body>
  fDocument.AppendLine(fTagGen.OpeningTag(BodyTag));
end;

procedure THTMLSnippetDoc.RenderCompilerInfo(const Heading: string;
  const Info: TCompileDocInfoArray);
var
  CompilerInfo: TCompileDocInfo;  // info about each compiler
begin
  fDocument.AppendLine(
    fTagGen.CompoundTag(
      ParaTag, fTagGen.CompoundTag(StrongTag, fTagGen.Entities(Heading))
    )
  );
  fDocument
    .AppendLine(
      fTagGen.OpeningTag(
        TableTag, THTMLAttributes.Create(ClassAttr, IndentClass)
      )
    )
    .AppendLine(fTagGen.OpeningTag(TableBodyTag));

  for CompilerInfo in Info do
  begin
    fDocument
      .AppendLine(fTagGen.OpeningTag(TableRowTag))
      .AppendLine(
        fTagGen.CompoundTag(
          TableColTag, fTagGen.Entities(CompilerInfo.Compiler)
        )
      )
      .AppendLine(
        fTagGen.CompoundTag(
          TableColTag,
          fTagGen.CompoundTag(
            EmphasisTag, fTagGen.Entities(CompilerInfo.Result)
          )
        )
      )
      .AppendLine(fTagGen.ClosingTag(TableRowTag));
  end;

  fDocument
    .AppendLine(fTagGen.ClosingTag(TableBodyTag))
    .AppendLine(fTagGen.ClosingTag(TableTag));
end;

procedure THTMLSnippetDoc.RenderDescription(const Desc: IActiveText);
begin
  fDocument.AppendLine(ActiveTextToHTML(Desc));
end;

procedure THTMLSnippetDoc.RenderExtra(const ExtraText: IActiveText);
begin
  fDocument.AppendLine(ActiveTextToHTML(ExtraText));
end;

procedure THTMLSnippetDoc.RenderHeading(const Heading: string;
  const AVaultID: TVaultID);
var
  Attrs: IHTMLAttributes;
begin
  Attrs := THTMLAttributes.Create(
    StyleAttr, TCSS.ColorProp(Preferences.GetSnippetHeadingColour(AVaultID))
  );
  fDocument.AppendLine(
    fTagGen.CompoundTag(H1Tag, Attrs, fTagGen.Entities(Heading))
  );
end;

procedure THTMLSnippetDoc.RenderNoCompilerInfo(const Heading,
  NoCompileTests: string);
begin
  fDocument.AppendLine(
    fTagGen.CompoundTag(
      ParaTag, fTagGen.CompoundTag(StrongTag, fTagGen.Entities(Heading))
    )
  );
  fDocument.AppendLine(
    fTagGen.CompoundTag(
      ParaTag,
      THTMLAttributes.Create(ClassAttr, IndentClass),
      fTagGen.Entities(NoCompileTests)
    )
  );
end;

procedure THTMLSnippetDoc.RenderSourceCode(const SourceCode: string);
var
  Renderer: IHiliteRenderer;  // renders highlighted source as RTF
  HTMLBuilder: THTMLBuilder;  // constructs the HTML of the highlighted source
resourcestring
  sHeading = 'Source Code:';
begin
  fDocument.AppendLine(
    fTagGen.CompoundTag(
      ParaTag,
      fTagGen.CompoundTag(StrongTag, fTagGen.Entities(sHeading))
    )
  );
  fDocument.AppendLine(
    fTagGen.OpeningTag(DivTag, THTMLAttributes.Create(ClassAttr, IndentClass))
  );
  HTMLBuilder := THTML5Builder.Create;
  try
    Renderer := THTMLHiliteRenderer.Create(HTMLBuilder, fHiliteAttrs);
    TSyntaxHiliter.Hilite(SourceCode, Renderer);
    fDocument.AppendLine(HTMLBuilder.HTMLFragment);
  finally
    HTMLBuilder.Free;
  end;
  fDocument.AppendLine(fTagGen.ClosingTag(DivTag));
end;

procedure THTMLSnippetDoc.RenderTitledList(const Title: string;
  List: IStringList);
begin
  RenderTitledText(Title, CommaList(List));
end;

procedure THTMLSnippetDoc.RenderTitledText(const Title, Text: string);
begin
  fDocument.AppendLine(
    fTagGen.CompoundTag(
      ParaTag, fTagGen.CompoundTag(StrongTag, fTagGen.Entities(Title))
    )
  );
  fDocument.AppendLine(
    fTagGen.CompoundTag(
      ParaTag,
      THTMLAttributes.Create(ClassAttr, IndentClass),
      fTagGen.Entities(Text)
    )
  );
end;

procedure THTMLSnippetDoc.RenderVaultInfo(const Text: string);
begin
  fDocument.AppendLine(
    fTagGen.CompoundTag(
      ParaTag,
      THTMLAttributes.Create(ClassAttr, VaultInfoClass),
      fTagGen.Entities(Text)
    )
  );
end;

{ TXHTMLSnippetDoc }

function TXHTMLSnippetDoc.BuilderClass: THTMLBuilderClass;
begin
  Result := TXHTMLBuilder;
end;

{ THTML5SnippetDoc }

function THTML5SnippetDoc.BuilderClass: THTMLBuilderClass;
begin
  Result := THTML5Builder;
end;

end.
