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

 * Portions created by the Initial Developer are Copyright (C) 2005-2011 Peter
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
  Classes, Graphics, Generics.Collections,
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

type
  // Type that stores the name / value pairs of an HTML attribute
  THTMLAttribute = TPair<string,string>;

  {
  THTMLAttributes:
    Class that can build a list of HTML tag attributes and render them.
  }
  THTMLAttributes = class(TInterfacedObject, IHTMLAttributes)
  strict private
    fAttrs: TStringList;
      {Maintains list of attributes as name=value pairs}
  public
    constructor Create; overload;
      {Object constructor. Sets up empty object.
      }
    constructor Create(const Name, Value: string); overload;
      {Object constructor. Sets up object containing a single named attribute.
        @param Name [in] Name of attribute.
        @param Value [in] Value of attribute. If '' attribute is not added.
      }
    constructor Create(Attrs: array of THTMLAttribute); overload;
      {Object constructor. Sets up object containing zero or more named
      attributes.
        @param Attrs [in] Array of attributes represented by THTMLAttribute
          records.
      }
    destructor Destroy; override;
      {Object destructor. Tears down object.
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

function MakeSafeHTMLText(TheText: string): string;
  {Encodes a string so that any HTML-incompatible characters are replaced with
  suitable character entities.
  NOTE: HTML returned by this method does not have non-ASCII characters from
  #127 converted to entities, therefore the HTML can only be saved to Unicode or
  UTF8 format files.
    @param TheText [in] Text to be encoded.
    @return Encoded text.
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
  Attrs: IHTMLAttributes = nil): string;
  {Generates an (X)HTML tag.
    @param TagName [in] Name of tag. Always output in lower case.
    @param TagType [in] Type of tag: open or close compound tag or simple tag.
    @param Attrs [in] Optional tag attributes of tag. Ignored for close tags.
      Any attributes that refer to URLs should be URL encoded before calling
      this routine.
    @return HTML safe tag.
  }

function MakeCompoundTag(const TagName: string; Attrs: IHTMLAttributes;
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


implementation


uses
  // Delphi
  SysUtils, Windows,
  // Project
  UCSSUtils, UExceptions, UStrUtils, UURIEncode;


function MakeSafeHTMLText(TheText: string): string;
  {Encodes a string so that any HTML-incompatible characters are replaced with
  suitable character entities.
  NOTE: HTML returned by this method does not have non-ASCII characters from
  #127 converted to entities, therefore the HTML can only be saved to Unicode or
  UTF8 format files.
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
        if (Ch < #32) and not CharInSet(Ch, [#10, #13]) then
          Result := Result + '&#' + IntToStr(Ord(Ch)) + ';'
        else
          Result := Result + Ch;
      end;
    end;
end;

function MakeTag(const TagName: string; const TagType: THTMLTagType;
  Attrs: IHTMLAttributes = nil): string;
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
    Result := '</' + StrToLower(TagName) + '>'
  else
  begin
    Result := '<' + StrToLower(TagName);
    if Assigned(Attrs) and (not Attrs.IsEmpty) then
      Result := Result + ' ' + Attrs.RenderSafe;
    if TagType = ttOpen then
      Result := Result + '>'
    else
      Result := Result + ' />';
  end;
end;

function MakeCompoundTag(const TagName: string; Attrs: IHTMLAttributes;
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
      [
        TCSS.VerticalAlignProp(cvaTop),
        TCSS.WidthProp(Width),
        TCSS.HeightProp(Height)
      ]
    )
  );
  Attrs.Add('title', Title);
  if Id <> '' then
    Attrs.Add('id', Id);
  // Create tag
  Result := MakeTag('img', ttSimple, Attrs);
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
  {Object constructor. Sets up empty object.
  }
begin
  inherited Create;
  fAttrs := TStringList.Create;
end;

constructor THTMLAttributes.Create(const Name, Value: string);
  {Object constructor. Sets up object containing a single named attribute.
    @param Name [in] Name of attribute.
    @param Value [in] Value of attribute. If '' attribute is not added.
  }
begin
  Create;
  Add(Name, Value);
end;

constructor THTMLAttributes.Create(Attrs: array of THTMLAttribute);
  {Object constructor. Sets up object containing zero or more named attributes.
    @param Attrs [in] Array of attributes represented by THTMLAttribute records.
  }
var
  Attr: THTMLAttribute; // each attribute
begin
  Create;
  for Attr in Attrs do
    Add(Attr.Key, Attr.Value);
end;

destructor THTMLAttributes.Destroy;
  {Object destructor. Tears down object.
  }
begin
  fAttrs.Free;
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
  Idx: Integer; // loops thru each attribute
begin
  Result := '';
  for Idx := 0 to Pred(fAttrs.Count) do
    Result := Result + Format(
      ' %0:s="%1:s"', [fAttrs.Names[Idx], fAttrs.ValueFromIndex[Idx]]
    );
  Result := StrTrimLeft(Result);
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
  Result := StrTrimLeft(Result);
end;

end.

