{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class used to create content of an XHTML strict document.
}


unit UHTMLBuilder;


interface


uses
  // Delphi
  SysUtils,
  // Project
  UHTMLUtils;


type

  THTMLBuilderClass = class of THTMLBuilder;

  ///  <summary>Abstract base class for classes that create the content of
  ///  different types of HTML documents.</summary>
  THTMLBuilder = class abstract (TObject)
  strict private
    var
      ///  <summary>Value of CSS property.</summary>
      fCSS: string;
      ///  <summary>Value of Title property.</summary>
      fTitle: string;
      ///  <summary>Used to create inner HTML of document body.</summary>
      fBodyInner: TStringBuilder;

    ///  <summary>Creates an HTML attributes object containing class attribute
    ///  that references given CSS class name.</summary>
    function MakeClassAttr(const ClassName: string): IHTMLAttributes;

    ///  <summary>Generates inline &lt;style&gt; tag containing CSS.</summary>
    ///  <remarks>Returns empty string if there is no CSS.</remarks>
    function InlineStyleSheet: string;

    ///  <summary>Builds document's compound &lt;head&gt; tag and its sub-tags.
    ///  </summary>
    function HeadTag: string;

    ///  <summary>Build document's &lt;title&gt; tag and its content.</summary>
    function TitleTag: string;

    ///  <summary>Builds document's compound &lt;body&gt; tag and its content.
    ///  </summary>
    function BodyTag: string;

    ///  <summary>Builds document's compound &lt;html&gt; tag and all its sub
    ///  tags and content.</summary>
    function HTMLTag: string;

    ///  <summary>Getter for Title property.</summary>
    ///  <remarks>Returns default title if title is empty string.</remarks>
    function GetTitle: string;

  strict protected
    const
      // Various HTML tag names
      HTMLTagName = 'html';
      HeadTagName = 'head';
      TitleTagName = 'title';
      MetaTagName = 'meta';
      StyleTagName = 'style';
      BodyTagName = 'body';
      PreTagName = 'pre';
      SpanTagName = 'span';
  strict protected
    ///  <summary>Returns the class used to generate tags for the appropriate
    ///  type of HTML.</summary>
    function TagGenerator: THTMLClass; virtual; abstract;
    ///  <summary>Returns any preamble to be written to the HTML before the
    ///  opening &lt;html&gt; tag.</summary>
    function Preamble: string; virtual; abstract;
    ///  <summary>Returns the attributes of the document's &lt;html&gt; tag.
    ///  </summary>
    function HTMLTagAttrs: IHTMLAttributes; virtual; abstract;
    ///  <summary>Returns any &lt;meta&gt; tags to be included within the
    ///  document's &lt;head&gt; tag.</summary>
    function MetaTags: string; virtual; abstract;
  public
    ///  <summary>Object constructor. Initialises object with empty body.
    ///  </summary>
    constructor Create;

    ///  <summary>Object destructor. Tears down object.</summary>
    destructor Destroy; override;

    ///  <summary>Appends an opening &lt;pre&gt; tag with specified class to
    ///  document body.</summary>
    procedure OpenPre(const ClassName: string);

    ///  <summary>Appends a closing &lt;/pre&gt; tag to document body.</summary>
    procedure ClosePre;

    ///  <summary>Appends an opening &lt;span&gt; tag with specified class to
    ///  document body.</summary>
    procedure OpenSpan(const ClassName: string);

    ///  <summary>Appends a closing &lt;/span&gt; tag to document body.
    ///  </summary>
    procedure CloseSpan;

    ///  <summary>Appends given text to document body. Text is converted to
    ///  valid XHTML character entities if necessary.</summary>
    procedure AddText(const Text: string);

    ///  <summary>Appends a new line to document body.</summary>
    procedure NewLine;

    ///  <summary>Returns document fragment containing inner body XHTML.
    ///  </summary>
    ///  <remarks>Fragment does not include &lt;body&gt; tags.</remarks>
    function HTMLFragment: string;

    ///  <summary>Returns complete XHTML document.</summary>
    function HTMLDocument: string;

    ///  <summary>XHTML Document title.</summary>
    ///  <remarks>Default value used if Title is empty string.</remarks>
    property Title: string read GetTitle write fTitle;

    ///  <summary>CSS used for document's inline cascading style sheet.
    ///  </summary>
    property CSS: string read fCSS write fCSS;
  end;

  ///  <summary>Class used to create the content of a XHTML strict document.
  ///  </summary>
  TXHTMLBuilder = class sealed(THTMLBuilder)
  strict private
    const
      // XML processor instruction
      XMLProcInstruction = '<?xml version="1.0"?>';
      // XML document type
      XHTMLDocType = '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" '
        + '"https://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">';
  strict protected
    ///  <summary>Returns the class used to generate XHTML compliant tags.
    ///  </summary>
    function TagGenerator: THTMLClass; override;
    ///  <summary>Returns the XML processing instruction followed by the XHTML
    ///  doctype.</summary>
    function Preamble: string; override;
    ///  <summary>Returns the attributes required for an XHTML &lt;html&gt; tag.
    ///  </summary>
    function HTMLTagAttrs: IHTMLAttributes; override;
    ///  <summary>Returns a &lt;meta&gt; tag that specifies the text/html
    ///  content type and UTF-8 encodiing.</summary>
    function MetaTags: string; override;
  end;

  ///  <summary>Class used to create the content of a HTML 5 document.</summary>
  THTML5Builder = class sealed(THTMLBuilder)
  strict private
    const
      // HTML 5 document type
      HTML5DocType = '<!DOCTYPE HTML>';
  strict protected
    ///  <summary>Returns the class used to generate HTML 5 compliant tags.
    ///  </summary>
    function TagGenerator: THTMLClass; override;
    ///  <summary>Returns the HTML 5 doctype.</summary>
    function Preamble: string; override;
    ///  <summary>Returns the attributes required for an HTML 5 &lt;html&gt;
    ///  tag.</summary>
    function HTMLTagAttrs: IHTMLAttributes; override;
    ///  <summary>Returns a &lt;meta&gt; tag that specifies that the document
    ///  uses UTF-8 encoding.</summary>
    function MetaTags: string; override;
  end;


implementation


uses
  // Project
  UConsts;


resourcestring
  // Default document title used if none provided
  sUntitled = 'Untitled';


{ THTMLBuilder }

procedure THTMLBuilder.AddText(const Text: string);
begin
  fBodyInner.Append(TagGenerator.Entities(Text));
end;

function THTMLBuilder.BodyTag: string;
begin
  Result := TagGenerator.CompoundTag(BodyTagName, EOL + HTMLFragment + EOL);
end;

procedure THTMLBuilder.ClosePre;
begin
  fBodyInner.Append(TagGenerator.ClosingTag(PreTagName));
end;

procedure THTMLBuilder.CloseSpan;
begin
  fBodyInner.Append(TagGenerator.ClosingTag(SpanTagName));
end;

constructor THTMLBuilder.Create;
begin
  inherited Create;
  fBodyInner := TStringBuilder.Create;
end;

destructor THTMLBuilder.Destroy;
begin
  fBodyInner.Free;
  inherited;
end;

function THTMLBuilder.GetTitle: string;
begin
  if fTitle <> '' then
    Result := fTitle
  else
    Result := sUntitled;
end;

function THTMLBuilder.HeadTag: string;
begin
  Result := TagGenerator.CompoundTag(
    HeadTagName,
    EOL + MetaTags + EOL + TitleTag + EOL + InlineStyleSheet
  );
end;

function THTMLBuilder.HTMLDocument: string;
begin
  Result := Preamble + EOL + HTMLTag + EOL;
end;

function THTMLBuilder.HTMLFragment: string;
begin
  Result := fBodyInner.ToString;
end;

function THTMLBuilder.HTMLTag: string;
begin
  Result := TagGenerator.CompoundTag(
    HTMLTagName,
    HTMLTagAttrs,
    EOL + HeadTag + EOL + BodyTag + EOL
  );
end;

function THTMLBuilder.InlineStyleSheet: string;
var
  Attrs: IHTMLAttributes; // style tag's attributes
begin
  if fCSS <> '' then
  begin
    Attrs := THTMLAttributes.Create('type', 'text/css');
    Result := TagGenerator.CompoundTag(StyleTagName, Attrs, EOL + fCSS) + EOL;
  end
  else
    Result := '';
end;

function THTMLBuilder.MakeClassAttr(const ClassName: string): IHTMLAttributes;
begin
  Result := THTMLAttributes.Create;
  if ClassName <> '' then
    Result.Add('class', ClassName);
end;

procedure THTMLBuilder.NewLine;
begin
  fBodyInner.AppendLine;
end;

procedure THTMLBuilder.OpenPre(const ClassName: string);
begin
  fBodyInner.Append(
    TagGenerator.OpeningTag(PreTagName, MakeClassAttr(ClassName))
  );
end;

procedure THTMLBuilder.OpenSpan(const ClassName: string);
begin
  fBodyInner.Append(
    TagGenerator.OpeningTag(SpanTagName, MakeClassAttr(ClassName))
  );
end;

function THTMLBuilder.TitleTag: string;
begin
  Result := TagGenerator.CompoundTag(
    TitleTagName, TagGenerator.Entities(Title)
  );
end;

{ TXHTMLBuilder }

function TXHTMLBuilder.HTMLTagAttrs: IHTMLAttributes;
begin
  Result := THTMLAttributes.Create(
    [THTMLAttribute.Create('xmlns', 'https://www.w3.org/1999/xhtml'),
    THTMLAttribute.Create('xml:lang', 'en'),
    THTMLAttribute.Create('lang', 'en')]
  );
end;

function TXHTMLBuilder.MetaTags: string;
begin
  Result := TagGenerator.SimpleTag(
    MetaTagName,
    THTMLAttributes.Create([
      THTMLAttribute.Create('http-equiv', 'content-type'),
      THTMLAttribute.Create('content', 'text/html; UTF-8')
    ])
  );
end;

function TXHTMLBuilder.Preamble: string;
begin
  Result := XMLProcInstruction + EOL + XHTMLDocType;
end;

function TXHTMLBuilder.TagGenerator: THTMLClass;
begin
  Result := TXHTML;
end;

{ THTML5Builder }

function THTML5Builder.HTMLTagAttrs: IHTMLAttributes;
begin
  Result := THTMLAttributes.Create('lang', 'en');
end;

function THTML5Builder.MetaTags: string;
begin
  // <meta charset="UTF-8">
  Result := TagGenerator.SimpleTag(
    MetaTagName,
    THTMLAttributes.Create('charset', 'UTF-8')
  );
end;

function THTML5Builder.Preamble: string;
begin
  Result := HTML5DocType;
end;

function THTML5Builder.TagGenerator: THTMLClass;
begin
  Result := THTML5;
end;

end.

