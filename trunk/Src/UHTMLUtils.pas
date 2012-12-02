{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Utility functions, interfaces and classes used to generate HTML.
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
    procedure Append(Attrs: IHTMLAttributes);
      {Appends all given attributes to these attributes. Any attributes with
      same name as an existing attribute overwrite the existing one.
        @param Attrs [in] Attributes to be appended.
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
    procedure Append(Attrs: IHTMLAttributes);
      {Appends all given attributes to these attributes. Any attributes with
      same name as an existing attribute overwrite the existing one.
        @param Attrs [in] Attributes to be appended.
      }
  end;

type
  ///  <summary>
  ///  Container for static methods that generate HTML tags and entities.
  ///  </summary>
  THTML = record
  strict private
    ///  <summary>Generates either an HTML start tag or a simple tag with given
    ///  name and attributes.</summary>
    ///  <param name="Name">string [in] Tag name.</param>
    ///  <param name="Attrs">IHTMLAttributes [in] Attributes to include in tag.
    ///  May be nil if no attributes are required.</param>
    ///  <param name="IsSimple">Boolean [in] Flag indicating whether tag is to
    ///  be simple (True) or the start of a compound tag (False).</param>
    ///  <returns>string. Required tag.</returns>
    class function TagWithAttrs(const Name: string; Attrs: IHTMLAttributes;
      const IsSimple: Boolean): string; static;

  public
    ///  <summary>Generates an opening HTML tag.</summary>
    ///  <param name="Name">string [in] Name of tag.</param>
    ///  <param name="Attrs">IHTMLAttributes [in] Optional tag attributes. Omit
    ///  or set to nil if no attributes are required.</param>
    ///  <returns>String. Required tag.</returns>
    ///  <remarks>Example tag: &lt;p class=&quot;ident&quot;&gt;</remarks>
    class function OpeningTag(const Name: string; Attrs: IHTMLAttributes = nil):
      string; static;

    ///  <summary>Generates a closing HTML tag.</summary>
    ///  <param name="Name">string [in] Name of tag.</param>
    ///  <returns>String. Required tag.</returns>
    ///  <remarks>Example tag: &lt;/p&gt;</remarks>
    class function ClosingTag(const Name: string): string; static;

    ///  <summary>Generates a simple HTML tag.</summary>
    ///  <param name="Name">string [in] Name of tag.</param>
    ///  <param name="Attrs">IHTMLAttributes [in] Optional tag attributes. Omit
    ///  or set to nil if no attributes are required.</param>
    ///  <returns>String. Required tag.</returns>
    ///  <remarks>Example tag: &lt;img class=&quot;glyph&quot; /&gt;</remarks>
    class function SimpleTag(const Name: string; Attrs: IHTMLAttributes = nil):
      string; static;

    ///  <summary>Surrounds the given HTML in a HTML tag pair.</summary>
    ///  <param name="Name">string [in] Name of tag.</param>
    ///  <param name="Attrs">IHTMLAttributes [in] Required attributes of tag.
    ///  </param>
    ///  <param name="InnerHTML">string [in] HTML that is to be enclosed within
    ///  the tag pair.</param>
    ///  <returns>String. Required tag.</returns>
    class function CompoundTag(const Name: string; Attrs: IHTMLAttributes;
      const InnerHTML: string): string; overload; static;

    ///  <summary>Surrounds the given HTML in a HTML tag pair. The opening tag
    ///  has no attributes.</summary>
    ///  <param name="Name">string [in] Name of tag.</param>
    ///  <param name="InnerHTML">string [in] HTML that is to be enclosed within
    ///  the tag pair.</param>
    ///  <returns>String. Required tag.</returns>
    class function CompoundTag(const Name, InnerHTML: string): string; overload;
      static;

    ///  <summary>Encodes the given string replacing any HTML-incompatible
    ///  characters with character entities.</summary>
    class function Entities(const Text: string): string; static;
  end;


// TODO: Replace all calls to MakeXXXX routines with calls to THTML methods

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

{ THTML }

class function THTML.ClosingTag(const Name: string): string;
begin
  Result := '</' + StrToLower(Name) + '>';
end;

class function THTML.CompoundTag(const Name: string; Attrs: IHTMLAttributes;
  const InnerHTML: string): string;
begin
  Result := OpeningTag(Name, Attrs) + InnerHTML + ClosingTag(Name);
end;

class function THTML.CompoundTag(const Name, InnerHTML: string): string;
begin
  Result := CompoundTag(Name, nil, InnerHTML);
end;

class function THTML.Entities(const Text: string): string;
var
  Ch: Char;           // each character in Text
  SB: TStringBuilder; // used to build the result string
begin
  SB := TStringBuilder.Create;
  try
    for Ch in Text do
    begin
      case Ch of
        '<':
          SB.Append('&lt;');
        '>':
          SB.Append('&gt;');
        '&':
          SB.Append('&amp;');
        '"':
          SB.Append('&quot;');
        #0..#9, #11, #12, #14..#31:
          SB.Append('&#' + IntToStr(Ord(Ch)) + ';')
        else
          SB.Append(Ch);
      end;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

class function THTML.OpeningTag(const Name: string;
  Attrs: IHTMLAttributes): string;
begin
  Result := TagWithAttrs(Name, Attrs, False);
end;

class function THTML.SimpleTag(const Name: string;
  Attrs: IHTMLAttributes): string;
begin
  Result := TagWithAttrs(Name, Attrs, True);
end;

class function THTML.TagWithAttrs(const Name: string; Attrs: IHTMLAttributes;
  const IsSimple: Boolean): string;
begin
  Result := '<' + StrToLower(Name);
  if Assigned(Attrs) and (not Attrs.IsEmpty) then
    Result := Result + ' ' + Attrs.RenderSafe;
  if IsSimple then
    Result := Result + ' />'
  else
    Result := Result + ' />';
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

procedure THTMLAttributes.Append(Attrs: IHTMLAttributes);
  {Appends all given attributes to these attributes. Any attributes with
  same name as an existing attribute overwrite the existing one.
    @param Attrs [in] Attributes to be appended.
  }
var
  Idx: Integer;
  AttrsObj: THTMLAttributes;
begin
  if not Assigned(Attrs) or Attrs.IsEmpty then
    Exit;
  AttrsObj := Attrs as THTMLAttributes;
  for Idx := 0 to Pred(AttrsObj.fAttrs.Count) do
    Add(AttrsObj.fAttrs.Names[Idx], AttrsObj.fAttrs.ValueFromIndex[Idx]);
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

