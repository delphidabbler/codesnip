{
 * UHTMLBuilder.pas
 *
 * Implements a class used to create content of an XHTML strict document.
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
 * The Original Code is UHTMLBuilder.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UHTMLBuilder;


interface


uses
  // Delphi
  SysUtils,
  // Project
  UHTMLUtils;


type
  ///  <summary>
  ///  Class used to create content of a XHTML strict document.
  ///  </summary>
  THTMLBuilder = class(TObject)
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

    ///  <summary>Builds document's compound &lt;body&gt; tag and its content.
    ///  </summary>
    function BodyTag: string;

    ///  <summary>Builds document's compound &lt;html&gt; tag and all its sub
    ///  tags and content.</summary>
    function HTMLTag: string;

    ///  <summary>Getter for Title property.</summary>
    ///  <remarks>Returns default title if title is empty string.</remarks>
    function GetTitle: string;

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


implementation


uses
  // Project
  UConsts;


const
  // XHTML document elements
  // XML processor instruction
  cXMLProcInstruction = '<?xml version="1.0"?>';
  // XML document type
  cDocType = '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" '
    + '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">';
  // Various tag names
  cHTMLTag = 'html';
  cHeadTag = 'head';
  cTitleTag = 'title';
  cStyleTag = 'style';
  cBodyTag = 'body';
  cPreTag = 'pre';
  cSpanTag = 'span';


resourcestring
  // Default document title used if none provided
  sUntitled = 'Untitled';


{ THTMLBuilder }

procedure THTMLBuilder.AddText(const Text: string);
begin
  fBodyInner.Append(MakeSafeHTMLText(Text));
end;

function THTMLBuilder.BodyTag: string;
begin
  Result := MakeCompoundTag(cBodyTag, EOL + HTMLFragment + EOL);
end;

procedure THTMLBuilder.ClosePre;
begin
  fBodyInner.Append(MakeTag(cPreTag, ttClose));
end;

procedure THTMLBuilder.CloseSpan;
begin
  fBodyInner.Append(MakeTag(cSpanTag, ttClose));
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
  Result := MakeCompoundTag(
    cHeadTag,
    EOL
      + MakeCompoundTag(cTitleTag, MakeSafeHTMLText(Title))
      + EOL
      + InlineStyleSheet
  );
end;

function THTMLBuilder.HTMLDocument: string;
begin
  Result := cXMLProcInstruction
    + EOL
    + cDocType
    + EOL
    + HTMLTag
    + EOL;
end;

function THTMLBuilder.HTMLFragment: string;
begin
  Result := fBodyInner.ToString;
end;

function THTMLBuilder.HTMLTag: string;

  // ---------------------------------------------------------------------------
  ///  <summary>Builds object describing attributes of &lt;html&gt; tag.
  ///  </summary>
  function HTMLAttrs: IHTMLAttributes;
  begin
    Result := THTMLAttributes.Create(
      [THTMLAttribute.Create('xmlns', 'http://www.w3.org/1999/xhtml'),
      THTMLAttribute.Create('xml:lang', 'en'),
      THTMLAttribute.Create('lang', 'en')]
    );
  end;
  // ---------------------------------------------------------------------------

begin
  Result := MakeCompoundTag(
    cHTMLTag,
    HTMLAttrs,
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
    Result := EOL
      + MakeCompoundTag(cStyleTag, Attrs, EOL + fCSS + EOL)
      + EOL;
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
  fBodyInner.Append(MakeTag(cPreTag, ttOpen, MakeClassAttr(ClassName)));
end;

procedure THTMLBuilder.OpenSpan(const ClassName: string);
begin
  fBodyInner.Append(MakeTag(cSpanTag, ttOpen, MakeClassAttr(ClassName)));
end;

end.

