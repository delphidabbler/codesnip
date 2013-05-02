{
 * UHTMLBuilder.pas
 *
 * Implements a class used to create content of an XHTML document.
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
 * Portions created by the Initial Developer are Copyright (C) 2007-2009 Peter
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
  // Project
  UHTMLUtils;


type

  {
  THTMLBuilder:
    Class used to create content of a XHTML document.
  }
  THTMLBuilder = class(TObject)
  strict private
    fCSS: string;   // Value of CSS property
    fTitle: string; // Value of Title property
    fBody: string;  // Records XHTML of document body
    function MakeClassAttr(const ClassName: string): IHTMLAttributes;
      {Creates a HTML attributes object containing class attribute.
        @param ClassName [in] Name of CSS class specified in class attribute.
        @return HTML attributes object encapsulating class attribute.
      }
    procedure AppendBody(const S: string);
      {Appends a string to XHTML body text.
        @param S [in] String to be appended.
      }
    function InlineStyleSheet: string;
      {Generates <style> tag containing CSS.
        @return Required XHTML tag and content or '' if there is no CSS.
      }
    function HeadTag: string;
      {Builds the document's compound <head> tag and its sub-tags.
        @return Required tag text.
      }
    function BodyTag: string;
      {Builds document's compound <body> tag and its content.
        @return Required tag.
      }
    function HTMLTag: string;
      {Builds document's compound <html> tag and all its sub tags and content.
        @return Required tag.
      }
    function GetTitle: string;
      {Read accessor for Title property.
        @return Property value if not '' or default title otherwise.
      }
  public
    procedure OpenPre(const ClassName: string);
      {Appends a <pre> tag to document body.
        @param ClassName [in] Class of pre tag.
      }
    procedure ClosePre;
      {Appends a </pre> tag to document body.
      }
    procedure OpenSpan(const ClassName: string);
      {Appends a <span> tag to document body.
        @param ClassName [in] Class of span tag.
      }
    procedure CloseSpan;
      {Appends a </span> tag to document body.
      }
    procedure AddText(const Text: string);
      {Adds text to document body. Text is converted to valid XHTML characters
      if necessary.
        @param Text [in] Text to be added.
      }
    procedure NewLine;
      {Adds a new line to document body.
      }
    function HTMLFragment: string;
      {Gets XHTML document fragment written into body.
        @return Required XHTML.
      }
    function HTMLDocument: string;
      {Builds complete XHTML document.
        @return Required XHTML.
      }
    property Title: string read GetTitle write fTitle;
      {Document's title}
    property CSS: string read fCSS write fCSS;
      {Document's inline cascading style sheet}
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
  // Comment tags
  cOpenComment = '<!--';
  cCloseComment = '-->';
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
  {Adds text to document body. Text is converted to valid XHTML characters if
  necessary.
    @param Text [in] Text to be added.
  }
begin
  AppendBody(MakeSafeHTMLText(Text));
end;

procedure THTMLBuilder.AppendBody(const S: string);
  {Appends a string to XHTML body text.
    @param S [in] String to be appended.
  }
begin
  fBody := fBody + S;
end;

function THTMLBuilder.BodyTag: string;
  {Builds document's compound <body> tag and its content.
    @return Required tag.
  }
begin
  Result := MakeCompoundTag(cBodyTag, EOL + fBody + EOL);
end;

procedure THTMLBuilder.ClosePre;
  {Appends a </pre> tag to document body.
  }
begin
  AppendBody(MakeTag(cPreTag, ttClose));
end;

procedure THTMLBuilder.CloseSpan;
  {Appends a </span> tag to document body.
  }
begin
  AppendBody(MakeTag(cSpanTag, ttClose));
end;

function THTMLBuilder.GetTitle: string;
  {Read accessor for Title property.
    @return Property value if not '' or default title otherwise.
  }
begin
  if fTitle <> '' then
    Result := fTitle
  else
    Result := sUntitled;
end;

function THTMLBuilder.HeadTag: string;
  {Builds the document's compound <head> tag and its sub-tags.
    @return Required tag.
  }
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
  {Builds complete XHTML document.
    @return Required XHTML.
  }
begin
  Result := cXMLProcInstruction
    + EOL
    + cDocType
    + EOL
    + HTMLTag
    + EOL;
end;

function THTMLBuilder.HTMLFragment: string;
  {Gets XHTML document fragment written into body.
    @return Required XHTML.
  }
begin
  Result := fBody;
end;

function THTMLBuilder.HTMLTag: string;
  {Builds document's compound <html> tag and all its sub tags and content.
    @return Required tag.
  }

  // ---------------------------------------------------------------------------
  function HTMLAttrs: IHTMLAttributes;
    {Builds object describing attributes of <html> tag.
      @return Required attributes object.
    }
  begin
    Result := THTMLAttributes.Create;
    Result.Add('xmlns', 'http://www.w3.org/1999/xhtml');
    Result.Add('xml:lang', 'en');
    Result.Add('lang', 'en');
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
  {Generates <style> tag containing CSS.
    @return Required XHTML tag and content or '' if there is no CSS.
  }
var
  Attrs: IHTMLAttributes; // style tag's attributes
begin
  if fCSS <> '' then
  begin
    Attrs := THTMLAttributes.Create;
    Attrs.Add('type', 'text/css');
    Result := EOL
      + MakeCompoundTag(
        cStyleTag,
        Attrs,
        EOL + cOpenComment + EOL + fCSS + EOL + cCloseComment + EOL
      )
      + EOL;
  end
  else
    Result := '';
end;

function THTMLBuilder.MakeClassAttr(const ClassName: string): IHTMLAttributes;
  {Creates a HTML attributes object containing class attribute.
    @param ClassName [in] Name of CSS class specified in class attribute.
    @return HTML attributes object encapsulating class attribute.
  }
begin
  Result := THTMLAttributes.Create;
  if ClassName <> '' then
    Result.Add('class', ClassName);
end;

procedure THTMLBuilder.NewLine;
  {Adds a new line to document body.
  }
begin
  AppendBody(EOL);
end;

procedure THTMLBuilder.OpenPre(const ClassName: string);
  {Appends a <pre> tag to document body.
    @param ClassName [in] Class of pre tag.
  }
begin
  AppendBody(MakeTag(cPreTag, ttOpen, MakeClassAttr(ClassName)));
end;

procedure THTMLBuilder.OpenSpan(const ClassName: string);
  {Appends a <span> tag to document body.
    @param ClassName [in] Class of span tag.
  }
begin
  AppendBody(MakeTag(cSpanTag, ttOpen, MakeClassAttr(ClassName)));
end;

end.

