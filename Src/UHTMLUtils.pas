{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Helper interfaces and classes used to generate HTML.
}


unit UHTMLUtils;


interface


uses
  // Delphi
  Classes, Generics.Collections,
  // Project
  UIStringList;


type

  ///  <summary>Interface supported by objects that can build and render a list
  ///  of HTML tag attributes.</summary>
  IHTMLAttributes = interface(IInterface)
    ['{2CE69ED2-9622-45A9-A800-5513443D1371}']
    ///  <summary>Checks if the attributes list is empty.</summary>
    function IsEmpty: Boolean;
    ///  <summary>Renders the attributes in plain text.</summary>
    function Render: string;
    ///  <summary>Renders the attributes as HTML-safe text.</summary>
    function RenderSafe: string;
    ///  <summary>Adds a named attribute to the list with an associated value.
    ///  </summary>
    ///  <param name="Name">string [in] Attribute name.</param>
    ///  <param name="Value">string [in] Attribute vale.</param>
    procedure Add(const Name, Value: string); overload;
    ///  <summary>Adds a name attribute which has a space separated list of
    ///  values associated with it.</summary>
    ///  <param name="Name">string [in] Attribute name.</param>
    ///  <param name="Values">IStringList [in] List of values. If Values is not
    ///  assigned or is empty then the attribute is not added to the list.
    ///  </param>
    procedure Add(const Name: string; Values: IStringList); overload;
    ///  <summary>Appends a list of HTML attributes to the list.</summary>
    ///  <param name="Attrs">IHTMLAttributes [in] List of attributes to be
    ///  appended.</param>
    ///  <remarks>If any attribute in Attrs already exists in the list it will
    ///  be overwritten with the new value.</remarks>
    procedure Append(Attrs: IHTMLAttributes);
  end;

  ///  <summary>Type that stores the name / value pairs of an HTML attribute.
  ///  </summary>
  THTMLAttribute = TPair<string,string>;

  ///  <summary>Class that builds and renders a list of HTML tag attributes.
  ///  </summary>
  THTMLAttributes = class(TInterfacedObject, IHTMLAttributes)
  strict private
    var
      ///  <summary>Maintains list of attributes as name=value pairs.</summary>
      fAttrs: TStringList;
  public
    ///  <summary>Creates a new object instance with empty attribute list.
    ///  </summary>
    constructor Create; overload;

    ///  <summary>Creates a new object instance with an attribute list
    ///  containing a single attribute.</summary>
    ///  <param name="Name">string [in] Name of attribute.</param>
    ///  <param name="Value">string [in] Value of attribute.</param>
    constructor Create(const Name, Value: string); overload;

    ///  <summary>Creates a new object instance with an attribute list
    ///  containing zero or more attributes.</summary>
    ///  <param name="Attrs">array of THTMLAttribute [in] Array of attributes to
    ///  be stored in attribute list. The array may be empty.</param>
    constructor Create(Attrs: array of THTMLAttribute); overload;

    ///  <summary>Destroys object instance.</summary>
    destructor Destroy; override;

    ///  <summary>Checks if the attributes list is empty.</summary>
    ///  <remarks>Method of IHTMLAttributes.</remarks>
    function IsEmpty: Boolean;

    ///  <summary>Renders the attributes in plain text.</summary>
    ///  <remarks>Method of IHTMLAttributes.</remarks>
    function Render: string;

    ///  <summary>Renders the attributes as HTML-safe text.</summary>
    ///  <remarks>Method of IHTMLAttributes.</remarks>
    function RenderSafe: string;

    ///  <summary>Adds a named attribute to the list with an associated value.
    ///  </summary>
    ///  <param name="Name">string [in] Attribute name.</param>
    ///  <param name="Value">string [in] Attribute vale.</param>
    ///  <remarks>Method of IHTMLAttributes.</remarks>
    procedure Add(const Name, Value: string); overload;

    ///  <summary>Adds a name attribute which has a space separated list of
    ///  values associated with it.</summary>
    ///  <param name="Name">string [in] Attribute name.</param>
    ///  <param name="Values">IStringList [in] List of values. If Values is not
    ///  assigned or is empty then the attribute is not added to the list.
    ///  </param>
    ///  <remarks>Method of IHTMLAttributes.</remarks>
    procedure Add(const Name: string; Values: IStringList); overload;

    ///  <summary>Appends a list of HTML attributes to the list.</summary>
    ///  <param name="Attrs">IHTMLAttributes [in] List of attributes to be
    ///  appended.</param>
    ///  <remarks>
    ///  <para>If any attribute in Attrs already exists in the list it will be
    ///  overwritten with the new value.</para>
    ///  <para>Method of IHTMLAttributes.</para>
    ///  </remarks>
    procedure Append(Attrs: IHTMLAttributes);
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


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UConsts, UStrUtils;


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
        DOUBLEQUOTE:
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
    Result := Result + '>';
end;

{ THTMLAttributes }

procedure THTMLAttributes.Add(const Name, Value: string);
begin
  fAttrs.Values[Name] := Value; // this deletes entry if Value is ''
end;

procedure THTMLAttributes.Add(const Name: string; Values: IStringList);
begin
  if Assigned(Values) and not Values.IsEmpty then
    Add(Name, Values.GetText(' ', False));
end;

procedure THTMLAttributes.Append(Attrs: IHTMLAttributes);
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
begin
  inherited Create;
  fAttrs := TStringList.Create;
end;

constructor THTMLAttributes.Create(const Name, Value: string);
begin
  Create;
  Add(Name, Value);
end;

constructor THTMLAttributes.Create(Attrs: array of THTMLAttribute);
var
  Attr: THTMLAttribute; // each attribute
begin
  Create;
  for Attr in Attrs do
    Add(Attr.Key, Attr.Value);
end;

destructor THTMLAttributes.Destroy;
begin
  fAttrs.Free;
  inherited;
end;

function THTMLAttributes.IsEmpty: Boolean;
begin
  Result := fAttrs.Count = 0;
end;

function THTMLAttributes.Render: string;
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
var
  Idx: Integer;
begin
  Result := '';
  for Idx := 0 to Pred(fAttrs.Count) do
    Result := Result + Format(
      ' %0:s="%1:s"',
      [
        THTML.Entities(fAttrs.Names[Idx]),
        THTML.Entities(fAttrs.ValueFromIndex[Idx])
      ]
    );
  Result := StrTrimLeft(Result);
end;

end.

