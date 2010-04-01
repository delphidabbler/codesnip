{
 * UHTMLUtils.pas
 *
 * Utility functions, interfaces and classes used to generate HTML.
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
 * The Original Code is UHTMLUtils.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).

 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UHTMLUtils;


interface


uses
  // Delphi
  Classes, Graphics,
  // Project
  UIStringList;


type

  {
  IHTMLAttributes:
    Interface to object that can build a list of HTML tag attributes and render
    them.
  }
  IHTMLAttributes = interface(IInterface)
    ['{2CE69ED2-9622-45A9-A800-5513443D1371}']
    function IsEmpty: Boolean;
      {Determines if attributes object is empty.
        @return True if there are no attributes, False otherwise.
      }
    function Render: string;
      {Renders attributes as plain text.
        @return Text representation of attributes.
      }
    function RenderSafe: string;
      {Renders attributes as HTML safe text.
        @return HTML safe representation of attributes.
      }
    procedure Add(const Name, Value: string); overload;
      {Adds a named attribute with its value.
        @param Name [in] Name of attribute.
        @param Value [in] Value of attribute. If '' attribute is not added.
      }
    procedure Add(const Name: string; Values: IStringList); overload;
      {Adds a named attribute and spaced separated list of values.
        @param Name [in] Name of attribute.
        @param Values [in] String list of attribute values. If not assigned or
          empty, attribute is not added.
      }
  end;

  {
  THTMLAttributes:
    Class that can build a list of HTML tag attributes and render them.
  }
  THTMLAttributes = class(TInterfacedObject, IHTMLAttributes)
  private
    fAttrs: TStringList;
      {Maintains list of attributes as name=value pairs}
  public
    constructor Create; overload;
      {Class constructor. Sets up object.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    { IHTMLAttributes methods }
    function IsEmpty: Boolean;
      {Determines if attributes object is empty.
        @return True if there are no attributes, False otherwise.
      }
    function Render: string;
      {Renders attributes as plain text.
        @return Text representation of attributes.
      }
    function RenderSafe: string;
      {Renders attributes as HTML safe text.
        @return HTML safe representation of attributes.
      }
    procedure Add(const Name, Value: string); overload;
      {Adds a named attribute with its value.
        @param Name [in] Name of attribute.
        @param Value [in] Value of attribute. If '' attribute is not added.
      }
    procedure Add(const Name: string; Values: IStringList); overload;
      {Adds a named attribute and spaced separated list of values.
        @param Name [in] Name of attribute.
        @param Values [in] String list of attribute values. If not assigned or
          empty, attribute is not added.
      }
  end;

function MakeResourceURL(const ModuleName: string; const ResName: PChar;
  const ResType: PChar = nil): string; overload;
  {Returns a res:// protocol URL that references a resource in a module.
    @param ModuleName [in] Name of module containing the resource.
    @param ResName [in] Name of resource.
    @param ResType [in] Type of resource (omitted from URL if nil or not
      specified).
    @return Required res:// protocol URL.
  }

function MakeResourceURL(const Module: HMODULE; const ResName: PChar;
  const ResType: PChar = nil): string; overload;
  {Returns a res:// protocol URL that references a resource a module.
    @param Module [in] Handle of module containing resource.
    @param ResName [in] Name of resource.
    @param ResType [in] Type of resource (omitted from URL if nil or not
      specified).
    @return Required res:// protocol URL.
  }

function MakeResourceURL(const ResName: string): string; overload;
  {Returns a res:// protocol URL that references a resource in a program's own
  RT_HTML resources.
    @param ResName [in] Name of resource.
    @return Required res:// protocol URL.
  }

function MakeSafeHTMLText(TheText: string): string;
  {Encodes a string so that any HTML-incompatible characters are replaced with
  suitable character entities.
    @param TheText [in] Text to be encoded.
    @return Encoded text.
  }

function URLEncode(const S: string; const InQueryString: Boolean): string;
  {Encodes a string, making it suitable for use in a URL. The function can
  encode strings for use in the main part of a URL (where spaces are encoded as
  '%20') or in URL query strings (where spaces are encoded as '+' characters).
    @param S [in] String to be encoded.
    @param InQueryString [in] Flag true if S is to be encoded for use in a query
      string or false if to be used in main body of URL.
    @return Encoded string.
  }

function URLDecode(const S: string; const FromQueryString: Boolean): string;
  {Decodes a URL-encoded string into plain text. The function can decode URLs
  from the main part of a URL (where spaces are encoded as '%20') or from URL
  query strings (where spaces are encoded as '+' characters).
    @param S [in] String to be decoded.
    @param FromQueryString [in] Flag true if S originates from a query string or
      false if it originates in the main body of URL.
    @return Decoded string.
    @except EBug raised if any URL attribute is invalid.
  }

type
  {
  THTMLTagType:
    Defines type of XHTML tags generated by MakeTag.
  }
  THTMLTagType = (
    ttOpen,       // opening compound tag
    ttClose,      // closing compound tag
    ttSimple      // simple tag
  );

function MakeTag(const TagName: string; const TagType: THTMLTagType;
  const Attrs: IHTMLAttributes = nil): string;
  {Generates an (X)HTML tag.
    @param TagName [in] Name of tag. Always output in lower case.
    @param TagType [in] Type of tag: open or close compound tag or simple tag.
    @param Attrs [in] Optional tag attributes of tag. Ignored for close tags.
      Any attributes that refer to URLs should be URL encoded before calling
      this routine.
    @return HTML safe tag.
  }

function MakeCompoundTag(const TagName: string; const Attrs: IHTMLAttributes;
  const InnerHTML: string): string; overload;
  {Generates a compound (X)HTML tag with its opening tag, inner HTML and closing
  tag.
    @param TagName [in] Name of tag. Always output in lower case.
    @param Attrs [in] Tag attributes of tag. Any attributes that refer to URLs
      should be URL encoded before calling this routine.
    @param InnerHTML [in] HTML that appears between opening and closing tags.
      Must be valid HTML.
    @return Required compound tag.
  }

function MakeCompoundTag(const TagName: string;
  const InnerHTML: string): string; overload;
  {Generates a compound (X)HTML tag with its opening tag, inner HTML and closing
  tag.
    @param TagName [in] Name of tag. Always output in lower case.
    @param InnerHTML [in] HTML that appears between opening and closing tags.
      Must be valid HTML.
    @return Required compound tag.
  }

function ImageTag(const Src, Title: string;
  const Width, Height: Integer; const Id: string = ''): string;
  {Returns <img> tag for image with given attributes. Image is top aligned.
    @param Src [in] URL of image file. Should be URL encoded by caller.
    @param Title [in] Image title.
    @param Width [in] Image width in pixels. Stored in "style" attribute.
    @param Height [in] Image height in pixels. Stored in "style" attribute.
    @param Id [in] Image's id (optional).
    @return Required image tag.
  }

function ColorToHTML(const Color: TColor): string;
  {Converts a Delphi TColor value into a string suitable for use in HTML or CSS
  code. Any system colors (like clBtnFace) are mapped to the actual colour
  according to the current Windows settings.
    @param Color [in] Colour value to be converted.
    @return HTML/CSS code for colour.
  }

function IsValidHTMLCode(const Content: string): Boolean;
  {Checks if document content is valid HTML.
    @param Content [in] Document content to be checked.
    @return True if valid HTML.
  }


implementation


uses
  // Delphi
  SysUtils, StrUtils, Windows,
  // Project
  UCSSUtils, UExceptions, UUnicodeHelper;


function IsValidHTMLCode(const Content: string): Boolean;
  {Checks if document content is valid HTML.
    @param Content [in] Document content to be checked.
    @return True if valid HTML.
  }
begin
  Result := AnsiContainsText(Content, '<html') and
    AnsiContainsText(Content, '</html>');
end;

function MakeResourceURL(const ModuleName: string; const ResName: PChar;
  const ResType: PChar = nil): string; overload;
  {Returns a res:// protocol URL that references a resource in a module.
    @param ModuleName [in] Name of module containing the resource.
    @param ResName [in] Name of resource.
    @param ResType [in] Type of resource (omitted from URL if nil or not
      specified).
    @return Required res:// protocol URL.
  }

  // ---------------------------------------------------------------------------
  function ResNameOrTypeToString(R: PChar): string;
    {Returns string representation of a resource name or type. If name or type
    is already a string it is returned unchanged. If it is a numeric value it's
    value is returned as a string, preceeded by '#'.
      @param R [in] Resource name or type.
      @return String representation of the resource name or type.
    }
  begin
    if HiWord(LongWord(R)) = 0 then
      // high word = 0 => numeric resource id
      // numeric value is stored in low word
      Result := Format('#%d', [LoWord(LongWord(R))])
    else
      // high word <> 0 => string value
      // PChar is implicitly converted to string
      Result := R;
  end;
  // ---------------------------------------------------------------------------

begin
  Assert(ModuleName <> '');
  Assert(Assigned(ResName));
  // Resource starts with module name
  Result := 'res://' + URLEncode(ModuleName, False);
  // Resource type follows if specified
  if Assigned(ResType) then
    Result := Result + '/' + URLEncode(ResNameOrTypeToString(ResType), False);
  // Resource name is last in URL
  Result := Result + '/' + URLEncode(ResNameOrTypeToString(ResName), False);
end;

function MakeResourceURL(const Module: HMODULE; const ResName: PChar;
  const ResType: PChar = nil): string; overload;
  {Returns a res:// protocol URL that references a resource a module.
    @param Module [in] Handle of module containing resource.
    @param ResName [in] Name of resource.
    @param ResType [in] Type of resource (omitted from URL if nil or not
      specified).
    @return Required res:// protocol URL.
  }
begin
  Result := MakeResourceURL(GetModuleName(Module), ResName, ResType);
end;

function MakeResourceURL(const ResName: string): string; overload;
  {Returns a res:// protocol URL that references a resource in a program's own
  RT_HTML resources.
    @param ResName [in] Name of resource.
    @return Required res:// protocol URL.
  }
begin
  Result := MakeResourceURL(HInstance, PChar(ResName));
end;

function MakeSafeHTMLText(TheText: string): string;
  {Encodes a string so that any HTML-incompatible characters are replaced with
  suitable character entities.
    @param TheText [in] Text to be encoded.
    @return Encoded text.
  }
var
  Ch: Char;     // each character in TheText
begin
  Result := '';
  for Ch in TheText do
    case Ch of
      '<':
        Result := Result + '&lt;';
      '>':
        Result := Result + '&gt;';
      '&':
        Result := Result + '&amp;';
      '"':
        Result := Result + '&quot;';
      else
      begin
        if (Ch < #32) or (Ch >= #127) then
          Result := Result + '&#' + IntToStr(Ord(Ch)) + ';'
        else
          Result := Result + Ch;
      end;
    end;
end;

function URLEncode(const S: string; const InQueryString: Boolean): string;
  {Encodes a string, making it suitable for use in a URL. The function can
  encode strings for use in the main part of a URL (where spaces are encoded as
  '%20') or in URL query strings (where spaces are encoded as '+' characters).
    @param S [in] String to be encoded.
    @param InQueryString [in] Flag true if S is to be encoded for use in a query
      string or false if to be used in main body of URL.
    @return Encoded string.
  }
var
  URL: Windows1252String;  // Windows-1252 encoding of S
  Ch: AnsiChar;            // each character in URL
begin
  URL := StringToWindows1252String(S);
  Result := '';
  for Ch in URL do
  begin
    case Ch of
      'A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.':
        Result := Result + Char(Ch);
      ' ':
        if InQueryString then
          Result := Result + '+'
        else
          Result := Result + '%20';
      else
        Result := Result + '%' + IntToHex(Ord(Ch), 2);
    end;
  end;
end;

function URLDecode(const S: string; const FromQueryString: Boolean): string;
  {Decodes a URL-encoded string into plain text. The function can decode URLs
  from the main part of a URL (where spaces are encoded as '%20') or from URL
  query strings (where spaces are encoded as '+' characters).
    @param S [in] String to be decoded.
    @param FromQueryString [in] Flag true if S originates from a query string or
      false if it originates in the main body of URL.
    @return Decoded string.
    @except EBug raised if any URL attribute is invalid.
  }
var
  Idx: Integer;     // loops through characters of S
  HexStr: string;   // hex digits from HTML attribute
  Hex: Integer;     // value of hex digits from HTML attribute
resourcestring
  // Error messages
  sIncompleteAttr = 'URL attribute incomplete';
  sInvalidAttr = 'Invalid hex digits in URL attribute';
begin
  Result := '';
  Idx := 1;
  while Idx <= Length(S) do
  begin
    case S[Idx] of
      '%':
      begin
        // Decode hex attribute in form %XX where X is a valid hex digit
        if Idx > Length(S) - 2 then
          raise EBug.Create(sIncompleteAttr);
        HexStr := '$' + S[Idx + 1] + S[Idx + 2];
        if not TryStrToInt(HexStr, Hex) then
          raise EBug.Create(sInvalidAttr);
        Result := Result + Char(Hex);
        Inc(Idx, 2);
      end;
      '+':
      begin
        // Decode '+' only if from a query string
        if FromQueryString then
          Result := Result + ' '
        else
          Result := Result + '+';
      end;
      else
        // Pass other characters through unchanged
        Result := Result + S[Idx];
      // Next character
    end;
    Inc(Idx);
  end;
end;

function MakeTag(const TagName: string; const TagType: THTMLTagType;
  const Attrs: IHTMLAttributes = nil): string;
  {Generates an (X)HTML tag.
    @param TagName [in] Name of tag. Always output in lower case.
    @param TagType [in] Type of tag: open or close compound tag or simple tag.
    @param Attrs [in] Optional tag attributes of tag. Ignored for close tags.
      Any attributes that refer to URLs should be URL encoded before calling
      this routine.
    @return HTML safe tag.
  }
begin
  if TagType = ttClose then
    Result := '</' + AnsiLowerCase(TagName) + '>'
  else
  begin
    Result := '<' + AnsiLowerCase(TagName);
    if Assigned(Attrs) and (not Attrs.IsEmpty) then
      Result := Result + ' ' + Attrs.RenderSafe;
    if TagType = ttOpen then
      Result := Result + '>'
    else
      Result := Result + ' />';
  end;
end;

function MakeCompoundTag(const TagName: string; const Attrs: IHTMLAttributes;
  const InnerHTML: string): string;
  {Generates a compound (X)HTML tag with its opening tag, inner HTML and closing
  tag.
    @param TagName [in] Name of tag. Always output in lower case.
    @param Attrs [in] Tag attributes of tag. Any attributes that refer to URLs
      should be URL encoded before calling this routine.
    @param InnerHTML [in] HTML that appears between opening and closing tags.
      Must be valid HTML.
    @return Required compound tag.
  }
begin
  Result := MakeTag(TagName, ttOpen, Attrs)
    + InnerHTML
    + MakeTag(TagName, ttClose);
end;

function MakeCompoundTag(const TagName: string;
  const InnerHTML: string): string; overload;
  {Generates a compound (X)HTML tag with its opening tag, inner HTML and closing
  tag.
    @param TagName [in] Name of tag. Always output in lower case.
    @param InnerHTML [in] HTML that appears between opening and closing tags.
      Must be valid HTML.
    @return Required compound tag.
  }
begin
  Result := MakeCompoundTag(TagName, nil, InnerHTML);
end;

function ImageTag(const Src, Title: string;
  const Width, Height: Integer; const Id: string = ''): string;
  {Returns <img> tag for image with given attributes. Image is top aligned.
    @param Src [in] URL of image file. Should be URL encoded by caller.
    @param Title [in] Image title.
    @param Width [in] Image width in pixels. Stored in "style" attribute.
    @param Height [in] Image height in pixels. Stored in "style" attribute.
    @param Id [in] Image's id (optional).
    @return Required image tag.
  }
var
  Attrs: IHTMLAttributes; // image's attributes
begin
  // Create attributes
  Attrs := THTMLAttributes.Create;
  Attrs.Add('src', Src);
  Attrs.Add(
    'style',
    TIStringList.Create(
      [CSSVerticalAlignProp(cvaTop), CSSWidthProp(Width), CSSHeightProp(Height)]
    )
  );
  Attrs.Add('title', Title);
  if Id <> '' then
    Attrs.Add('id', Id);
  // Create tag
  Result := MakeTag('img', ttSimple, Attrs);
end;

function ColorToHTML(const Color: TColor): string;
  {Converts a Delphi TColor value into a string suitable for use in HTML or CSS
  code. Any system colors (like clBtnFace) are mapped to the actual colour
  according to the current Windows settings.
    @param Color [in] Colour value to be converted.
    @return HTML/CSS code for colour.
  }
var
  ColorRGB: Integer;  // RGB code for the colour
begin
  ColorRGB := ColorToRGB(Color);  // this translates system colours to actual
  Result := Format(
    '#%0.2X%0.2X%0.2X',
    [GetRValue(ColorRGB), GetGValue(ColorRGB), GetBValue(ColorRGB)]
  );
end;

{ THTMLAttributes }

procedure THTMLAttributes.Add(const Name, Value: string);
  {Adds a named attribute with its value.
    @param Name [in] Name of attribute.
    @param Value [in] Value of attribute. If '' attribute is not added.
  }
begin
  fAttrs.Values[Name] := Value; // this deletes entry if Value is ''
end;

procedure THTMLAttributes.Add(const Name: string; Values: IStringList);
  {Adds a named attribute and spaced separated list of values.
    @param Name [in] Name of attribute.
    @param Values [in] String list of attribute values. If not assigned or
      empty, attribute is not added.
  }
begin
  if Assigned(Values) and (Values.Count > 0) then
    Add(Name, Values.GetText(' ', False));
end;

constructor THTMLAttributes.Create;
  {Class constructor. Sets up object.
  }
begin
  inherited Create;
  fAttrs := TStringList.Create;
end;

destructor THTMLAttributes.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fAttrs);
  inherited;
end;

function THTMLAttributes.IsEmpty: Boolean;
  {Determines if attributes object is empty.
    @return True if there are no attributes, False otherwise.
  }
begin
  Result := fAttrs.Count = 0;
end;

function THTMLAttributes.Render: string;
  {Renders attributes as plain text.
    @return Text representation of attributes.
  }
var
  Idx: Integer;
begin
  Result := '';
  for Idx := 0 to Pred(fAttrs.Count) do
    Result := Result + Format(
      ' %0:s="%1:s"', [fAttrs.Names[Idx], fAttrs.ValueFromIndex[Idx]]
    );
  Result := TrimLeft(Result);
end;

function THTMLAttributes.RenderSafe: string;
  {Renders attributes as HTML safe text.
    @return HTML safe representation of attributes.
  }
var
  Idx: Integer;
begin
  Result := '';
  for Idx := 0 to Pred(fAttrs.Count) do
    Result := Result + Format(
      ' %0:s="%1:s"',
      [
        MakeSafeHTMLText(fAttrs.Names[Idx]),
        MakeSafeHTMLText(fAttrs.ValueFromIndex[Idx])
      ]
    );
  Result := TrimLeft(Result);
end;

end.

