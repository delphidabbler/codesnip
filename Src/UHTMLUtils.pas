{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Helper interfaces and classes used to generate HTML.
}


unit UHTMLUtils;


interface


uses
  // Delphi
  Classes, Graphics, Generics.Collections,
  // Project
  UBaseObjects,
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

  THTMLClass = class of THTML;

  ///  <summary>Abstract base classe for static classes that return valid tags
  ///  for different flavours of HTML.</summary>
  THTML = class abstract(TNoConstructObject)
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
      const IsSimple: Boolean): string;
  strict protected
    class function GetSimpleTagCloser: string; virtual; abstract;
  public
    ///  <summary>Generates an opening HTML tag.</summary>
    ///  <param name="Name">string [in] Name of tag.</param>
    ///  <param name="Attrs">IHTMLAttributes [in] Optional tag attributes. Omit
    ///  or set to nil if no attributes are required.</param>
    ///  <returns>String. Required tag.</returns>
    ///  <remarks>Example tag: &lt;p class=&quot;ident&quot;&gt;</remarks>
    class function OpeningTag(const Name: string; Attrs: IHTMLAttributes = nil):
      string;

    ///  <summary>Generates a closing HTML tag.</summary>
    ///  <param name="Name">string [in] Name of tag.</param>
    ///  <returns>String. Required tag.</returns>
    ///  <remarks>Example tag: &lt;/p&gt;</remarks>
    class function ClosingTag(const Name: string): string;

    ///  <summary>Generates a simple HTML tag.</summary>
    ///  <param name="Name">string [in] Name of tag.</param>
    ///  <param name="Attrs">IHTMLAttributes [in] Optional tag attributes. Omit
    ///  or set to nil if no attributes are required.</param>
    ///  <returns>String. Required tag.</returns>
    ///  <remarks>Example tag: &lt;img class=&quot;glyph&quot; /&gt;</remarks>
    class function SimpleTag(const Name: string; Attrs: IHTMLAttributes = nil):
      string;

    ///  <summary>Surrounds the given HTML in a HTML tag pair.</summary>
    ///  <param name="Name">string [in] Name of tag.</param>
    ///  <param name="Attrs">IHTMLAttributes [in] Required attributes of tag.
    ///  </param>
    ///  <param name="InnerHTML">string [in] HTML that is to be enclosed within
    ///  the tag pair.</param>
    ///  <returns>String. Required tag.</returns>
    class function CompoundTag(const Name: string; Attrs: IHTMLAttributes;
      const InnerHTML: string): string; overload;

    ///  <summary>Surrounds the given HTML in a HTML tag pair. The opening tag
    ///  has no attributes.</summary>
    ///  <param name="Name">string [in] Name of tag.</param>
    ///  <param name="InnerHTML">string [in] HTML that is to be enclosed within
    ///  the tag pair.</param>
    ///  <returns>String. Required tag.</returns>
    class function CompoundTag(const Name, InnerHTML: string): string; overload;

    ///  <summary>Encodes the given string replacing any HTML-incompatible
    ///  characters with character entities.</summary>
    class function Entities(const Text: string): string;
  end;


  ///  <summary>Contains static methods that generate XHTML tags and entities.
  ///  </summary>
  TXHTML = class sealed (THTML)
  strict protected
    class function GetSimpleTagCloser: string; override;
  end;

  ///  <summary>Contains static methods that generate HTML5 tags and entities.
  ///  </summary>
  THTML5 = class sealed (THTML)
  strict protected
    class function GetSimpleTagCloser: string; override;
  end;

implementation


uses
  // Delphi
  SysUtils, Windows,
  // Project
  UConsts, UCSSUtils, UExceptions, UStrUtils, UURIEncode;


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
        LT:           SB.Append('&lt;');
        GT:           SB.Append('&gt;');
        SINGLEQUOTE:  SB.Append('&apos;');
        DOUBLEQUOTE:  SB.Append('&quot;');
        AMPERSAND:    SB.Append('&amp;');
        else          SB.Append(Ch);
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
    Result := Result + GetSimpleTagCloser
  else
    Result := Result + '>';
end;

{ TXHTML }

class function TXHTML.GetSimpleTagCloser: string;
begin
  Result := ' />';
end;

{ THTML5 }

class function THTML5.GetSimpleTagCloser: string;
begin
  Result := '>';
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
        TXHTML.Entities(fAttrs.Names[Idx]),
        TXHTML.Entities(fAttrs.ValueFromIndex[Idx])
      ]
    );
  Result := StrTrimLeft(Result);
end;

end.

