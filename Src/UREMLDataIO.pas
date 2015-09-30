{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements classes, records and other types that encapsulate the tags and
 * entities of the REML mark-up language, including a language parser.
}


unit UREMLDataIO;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  UBaseObjects,
  UExceptions,
  UTaggedTextLexer;


type
  ///  <summary>Enumeration of the supported REML tags.</summary>
  ///  <remarks>
  ///  <para>-rtLink - Inline link element. Always has a url attribute.</para>
  ///  <para>-rtStrong - Inline element indicating strong formatting required.
  ///  </para>
  ///  <para>-rtEm - Inline element indicating text should be emphasised.</para>
  ///  <para>-rtVar - Inline element indicating text should be formatted as a
  ///  source code variable.</para>
  ///  <para>-rtPara - Block level element enclosing a normal paragraph.</para>
  ///  <para>-rtWarning - Inline element indicating text should be formatted as
  ///  a warning.</para>
  ///  <para>-rtHeading - Block level element enclosing a heading paragraph.
  ///  </para>
  ///  <para>-rtMono - Inline element indicating text should be formatted in a
  ///  mono-spaced font.</para>
  ///  </remarks>
  TREMLTagID = (
    rtLink, rtStrong, rtEm, rtVar, rtPara, rtWarning, rtHeading, rtMono
  );

  ///  <summary>Set containing members of the TREMLTagID enumeration.</summary>
  TREMLTagIDs = set of TREMLTagID;

  ///  <summary>Record that encapsulates an attribute of a REML tag.</summary>
  TREMLAttr = record
    ///  <summary>Attribute name or key.</summary>
    Key: string;
    ///  <summary>Attribute value.</summary>
    Value: string;
    ///  <summary>Constructs attribute with given key and value.</summary>
    constructor Create(const AKey, AValue: string);
    ///  <summary>Creates and returns a null attribute.</summary>
    ///  <remarks>A null attribute should never be included in mark-up.
    ///  </remarks>
    class function CreateNull: TREMLAttr; static;
    ///  <summary>Checks if the attribute is null.</summary>
    ///  <remarks>A null attribute has its key set to the empty string.
    ///  </remarks>
    function IsNull: Boolean;
  end;

  ///  <summary>Possible states or types of a REML tag.</summary>
  ///  <remarks>
  ///  <para>rtsOpen - An opening tag.</para>
  ///  <para>rtsClose - A closing tag.</para>
  ///  </remarks>
  TREMLTagState = (rtsOpen, rtsClose);

  ///  <summary>Record that provides inforamtion about a REML tag.</summary>
  TREMLTagInfo = record
    ///  <summary>Tag's ID.</summary>
    ID: TREMLTagID;
    ///  <summary>Any attribute associated with the tag.</summary>
    ///  <remarks>If Attr is null then the tag has no attribute.</remarks>
    Attr: TREMLAttr;
    ///  <summary>Indicates if this is an opening or closing tag.</summary>
    State: TREMLTagState;
    ///  <summary>Constructs a record with the given ID, state and attribute.
    ///  </summary>
    constructor Create(const AID: TREMLTagID; const AState: TREMLTagState;
      const AAttr: TREMLAttr); overload;
    ///  <summary>Constructs a record with the given ID and state and a null
    ///  attribute.</summary>
    constructor Create(const AID: TREMLTagID; const AState: TREMLTagState);
      overload;
  end;

  ///  <summary>Static class that provides information about the supported REML
  ///  tags.</summary>
  TREMLTags = class(TNoConstructObject)
  strict private
    const
      ///  <summary>Set of block level REML tags.</summary>
      BlockTags = [rtPara, rtHeading];
  strict private
    type
      ///  <summary>Record that provides information about a single REML tag.
      ///  </summary>
      TREMLTag = record
      public
        ///  <summary>The tag's unique identifier.</summary>
        Id: TREMLTagID;  // active text element kind
        ///  <summary>IDs of any tags that can't be nested inside this tag.
        ///  </summary>
        Exclusions: TREMLTagIDs;
        ///  <summary>The tag's name as it appears in REML code.</summary>
        TagName: string;
        ///  <summary>Name (or key) of any parameter associated with the tag.
        ///  </summary>
        ///  <remarks>No tag can have more than one parameter. An empty string
        ///  indicates the tag has no parameter.</remarks>
        ParamName: string;
        ///  <summary>Constructs a record with the given ID, exclusions, name
        ///  and, optionally, parameter name.</summary>
        constructor Create(const AId: TREMLTagID;
          const AExclusions: TREMLTagIDs; const ATagName: string;
          const AParamName: string = '');
      end;
  strict private
    class var
      ///  <summary>List of all supported REML tags.</summary>
      fTagMap: TArray<TREMLTag>;
  strict private
    ///  <summary>Gets the index of the tag with the given ID in fTagMap or -1
    ///  if the ID is not known.</summary>
    class function IndexOfTagId(const Id: TREMLTagID): Integer;
    ///  <summary>Read accessor for Count property.</summary>
    class function GetCount: Integer; static;
    ///  <summary>Read accessor for IDs[] property.</summary>
    class function GetId(Idx: Integer): TREMLTagID; static;
    ///  <summary>Read accessor for Names[] property.</summary>
    class function GetName(Idx: Integer): string; static;
    ///  <summary>Read accessor for Exclusions property.</summary>
    class function GetExclusions(Idx: Integer): TREMLTagIDs; static;
  public
    ///  <summary>Sets up list of supported REML tags.</summary>
    class constructor Create;
    ///  <summary>Clears tag list.</summary>
    class destructor Destroy;
    ///  <summary>Finds the name of a tag from its ID.</summary>
    ///  <param name="ID">TREMLTagID [in] ID of required tag.</param>
    ///  <param name="TagName">string [out] Set to name of tag, if found, or to
    ///  the empty string if the tag ID is not found.
    ///  </param>
    ///  <returns>Boolean. True if given tag ID is found, False if not.
    ///  </returns>
    class function LookupTagName(const Id: TREMLTagID; out TagName: string):
      Boolean;
    ///  <summary>Finds the name of any parameter associated with a given tag.
    ///  </summary>
    ///  <param name="ID">TREMLTagID [in] ID of required tag.</param>
    ///  <param name="ParamName">string [out] Set to name (key) of the
    ///  parameter, if any. Will be set to the empty string if the tag has
    ///  no parameter or if the tag could not be found.</param>
    ///  <returns>Boolean. True if the given tag ID is found, False if not.
    ///  </returns>
    class function LookupParamName(const Id: TREMLTagID; out ParamName: string):
      Boolean;
    ///  <summary>Number of supported tags.</summary>
    class property Count: Integer read GetCount;
    ///  <summary>Indexed list of tag IDs.</summary>
    class property Ids[Idx: Integer]: TREMLTagID read GetId;
    ///  <summary>Indexed list of tag names.</summary>
    class property Names[Idx: Integer]: string read GetName;
    ///  <summary>Indexed list of tags that can't be nested within the
    ///  associated tag.</summary>
    class property Exclusions[Idx: Integer]: TREMLTagIDs read GetExclusions;
  end;

  ///  <summary>Static class that provides information about supported REML
  ///  character entities.</summary>
  TREMLEntities = class(TNoConstructObject)
  strict private
    type
      ///  <summary>Record that associates a character with its mnemonic REML
      ///  entity.</summary>
      TREMLEntity = record
        ///  <summary>Name of mnemonic entity.</summary>
        Entity: string;
        ///  <summary>Character associated with mnemonic entity.</summary>
        Ch: Char;
        ///  <summary>Constructs a record with the given mnemonic entity and
        ///  assciated character.</summary>
        constructor Create(const AEntity: string; const ACh: Char);
      end;
    class var
      ///  <summary>List of all supported mnemonic entities.</summary>
      fEntityMap: TArray<TREMLEntity>;
  strict private
    ///  <summary>Returns the mnemonic entity representation of the given
    ///  character. Returns the empty string if there is no such entity.
    ///  </summary>
    class function CharToMnemonicEntity(const Ch: Char): string;
    ///  <summary>Read accessor for Count property.</summary>
    class function GetCount: Integer; static;
    ///  <summary>Read accessor for Entities[] property.</summary>
    class function GetEntity(Idx: Integer): string; static;
    ///  <summary>Read accessor for Chars[] property.</summary>
    class function GetChar(Idx: Integer): Char; static;
  public
    ///  <summary>Sets up list of supported mnemonic entities.</summary>
    class constructor Create;
    ///  <summary>Clears mnemonic entity list.</summary>
    class destructor Destroy;
    ///  <summary>Returns the character entity that represents the given
    ///  character, if appropriate.</summary>
    ///  <param name="Ch">Char [in] Character for which entity required.</param>
    ///  <returns>string. Character's mnemonic entity if one exists, the
    ///  character itself if it is printable and has ordinal value less than
    ///  127, or a numeric character entiry otherwise.</returns>
    class function MapToEntity(const Ch: Char): string;
    ///  <summary>Number of supported mnemonic entities.</summary>
    class property Count: Integer read GetCount;
    ///  <summary>Indexed list of supported mnemonic character entities.
    ///  </summary>
    class property Entities[Idx: Integer]: string read GetEntity;
    ///  <summary>Indexed list of characters that have corresponding mnemonic
    ///  entities.</summary>
    class property Chars[Idx: Integer]: Char read GetChar;
  end;

  ///  <summary>Class that parses REML mark-up.</summary>
  ///  <remarks>Events are fired for each logical token in the mark-up. Users
  ///  must handle these events.</remarks>
  TREMLParser = class(TObject)
  public
    type
      ///  <summary>Type of event triggered when plain text is encountered in
      ///  REML mark-up.</summary>
      ///  <param name="Sender">TObject [in] Reference to parser object that
      ///  triggered the event.</param>
      ///  <param name="Text">string [in] The text read from REML. Any REML
      ///  entities will have been resolved into their corresponding characters.
      ///  </param>
      TParseTextEvent = procedure(Sender: TObject; const Text: string)
        of object;
      ///  <summary>Type of event triggered when a tag is encountered in REML
      ///  mark-up.</summary>
      ///  <param name="Sender">TObject [in] Reference to parser object that
      ///  triggered the event.</param>
      ///  <param name="TagInfo">TREMLTagInfo [in] Structure providing
      ///  information about the tag.</param>
      TParseTagEvent = procedure (Sender: TObject; const TagInfo: TREMLTagInfo)
        of object;
  strict private
    type
      ///  <summary>Class that tracks currently open REML tags.</summary>
      ///  <remarks>Used to keep track of open tags while parsing REML.
      ///  </remarks>
      TOpenTagTracker = class(TObject)
      strict private
        var
          ///  <summary>Records the number of tags of each kind that are open.
          ///  </summary>
          fTagState: array[TREMLTagID] of Cardinal;
      public
        ///  <summary>Constructs new object instance.</summary>
        constructor Create;
        ///  <summary>Destroys current object instance.</summary>
        destructor Destroy; override;
        ///  <summary>Clears all tracking information.</summary>
        procedure Clear;
        ///  <summary>Records that a tag with the given ID has been opened.
        ///  </summary>
        procedure OpenTag(Tag: TREMLTagID);
        ///  <summary>Records that a tag with the given ID has been closed.
        ///  </summary>
        procedure CloseTag(Tag: TREMLTagID);
        ///  <summary>Checks if any tags with the given ID are open.</summary>
        function TagsOpen(Tags: TREMLTagIDs): Boolean;
      end;
  strict private
    var
      ///  <summary>Reference to any OnText event handler.</summary>
      fOnText: TParseTextEvent;
      ///  <summary>Reference to any OnTag event handler.</summary>
      fOnTag: TParseTagEvent;
      ///  <summary>Lexer used to analyse REML mark-up.</summary>
      fLexer: TTaggedTextLexer;
      ///  <summary>Object used to track number of open tags during parsing.
      ///  </summary>
      fOpenTagTracker: TOpenTagTracker;
      ///  <summary>Maintains a stack of tag parameters for currently open tags.
      ///  </summary>
      ///  <remarks>This stack lets us remember a tag's parameters so they can
      ///  also be associated with the matching closing tag.</remarks>
      fParamStack: TStack<TREMLAttr>;
    ///  <summary>Callback used by the lexer to obtain information about
    ///  supported tags.</summary>
    ///  <param name="TagIdx">Integer [in] Index of tag for which information is
    ///  requested.</param>
    ///  <param name="TagName">string [out] Set to name of tag with given index.
    ///  </param>
    ///  <param name="TagCode">Word [out] Set to unique code that identifies tag
    ///  with given index.</param>
    ///  <param name="IsContainer">Boolean [out] Set to a value that indicates
    ///  if the tag with the given index is a container. Always set to True
    ///  since all REML tags are containers.</param>
    ///  <returns>Boolean. True if tag index was in range and information was
    ///  provided or False to indicate that information has been provided for
    ///  all supported tags.</returns>
    ///  <remarks>The lexer calls this method repeatedly, starting with TagIdx
    ///  set to zero and increasing by 1 on each call until False is returned.
    ///  </remarks>
    function TagInfo(const TagIdx: Integer; out TagName: string;
      out TagCode: Word; out IsContainer: Boolean): Boolean;
    ///  <summary>Callback used by the lexer to obtain information about
    ///  supported mnemonic character entities.</summary>
    ///  <param name="EntityIdx">Integer [in] Index of mnemonic entity for which
    ///  information is requested.</param>
    ///  <param name="EntityName">string [out] Set to name of mnemonic entity
    ///  with given index.</param>
    ///  <param name="EntityChar">Char [out] Set to character associated with
    ///  mnemonic entity at given index.</param>
    ///  <returns>Boolean. True if entity index was in range and information was
    ///  provided or False to indicate that information has been provided for
    ///  all supported entities.</returns>
    ///  <remarks>The lexer calls this method repeatedly, starting with
    ///  EntityIdx set to zero and increasing by 1 on each call until False is
    ///  returned.</remarks>
    function EntityInfo(const EntityIdx: Integer; out EntityName: string;
      out EntityChar: Char): Boolean;
    ///  <summary>Calls any OnText event handler, passing given text to it.
    ///  </summary>
    procedure DoText(const Text: string);
    ///  <summary>Calls any OnTag event handler, passing given tag information
    ///  to it.</summary>
    procedure DoTag(const TagInfo: TREMLTagInfo);
  public
    ///  <summary>Constructs a new parser object instance.</summary>
    constructor Create;
    ///  <summary>Destroys current object instance.</summary>
    destructor Destroy; override;
    ///  <summary>Parses the given REML mark-up.</summary>
    procedure Parse(const REML: string);
    ///  <summary>Event fired whenever parser reads plain text from mark-up.
    ///  </summary>
    property OnText: TParseTextEvent read fOnText write fOnText;
    ///  <summary>Event fired whenever parser encounters a REML tag in mark-up.
    ///  </summary>
    property OnTag: TParseTagEvent read fOnTag write fOnTag;
  end;

  ///  <summary>Class of exception raised by TREMLParser when parsing REML
  ///  mark-up.</summary>
  EREMLParseError = class(ECodeSnip);


implementation


{
  About REML Markup
  -----------------

  REML is greatly simplified form of (X)HTML created to allow formatted text to
  be included in a snippet's Description and Notes properties.

  REML stands for "Routine Extra Markup Language". This was named in CodeSnip v2
  when all snippets were simple "routines" and the only field capable of storing
  rich text was the snippet's (or routine's) Extra property. From CodeSnip v4
  the Description field could now also contain formatted text and from CodeSnip
  v5 the "Extra" field was renamed to "Notes".

  REML comprises plain text with limited inline and block level formatting and
  hyperlink specified by HTML like tags.

  Supported tags are as follows. Unless otherwise specified, no tags may have
  any attributes:

  Inline:
    <a href="url">xxxx</a>  - Hyperlink: must have an href attribute that
                              specifies the link destination as a valid URL.
                              URLs must not be URL encoded. No other attributes
                              may be specified.
    <strong>..</strong>     - Renders enclosed text with strong emphasis.
    <em>..</em>             - Renders enclosed text emphasised.
    <var>..</var>           - Renders enclosed text as a programming variable.
    <warning>..</warning>   - Renders enclosed text as a warning.
    <mono>..</mono>         - Renders enclosed text as mono spaced.

  Block:
    <p>..</p>               - Enclosed text is formatted as a paragraph.
    <heading>..</heading>   - Enclosed text is formatted as a heading.

  Certain characters in plain text or in attribute values must be encoded as
  HTML-like character entities. Attribute names must not contain any of these
  characters. The characters that must be encoded are:

  Character       Entity
  >               &gt;
  <               &lt;
  "               &quot;
  &               &amp;
  ©               &copy;

  No other entities are supported. Any other character can be encoded using its
  unicode or ascii value. For example, the @ symbol (ascii 64) is encoded as
  &#64;

  Example:
    <heading>Hello</heading>
    <p>&quot;<strong>Hello</strong>&quot; to
    <a href="http://example.com">you</a></p>

  This example specifes a heading "Hello" followed by a single paragraph. In the
  paragraph, "Hello" will be bold, "to" should be plain text and "you" should
  hyperlink to "example.com".

  There are four versions of REML as follows:
  v1 - supported tags: <strong> and <a>.
     - supported entities: &gt;, &lt, &quot;, &amp;.
     - <a> tag supports only http:// protocol in href parameter.
     - (from CodeSnip v2.2.5)
  v2 - added tags: <em>, <var>, <warning>, <mono>, <p> and <heading>.
     - added entity: &copy;.
     - <a> tag supports only http:// protocol in href parameter.
     - (from CodeSnip v3.0.0)
  v3 - added support for file:// protocol in <a> tag's href parameter.
       (from CodeSnip v3.0.1)
  v4 - added support for https:// protocol in <a> tag's href parameter.
       (from CodeSnip v4.0 alpha 1)
}


uses
  // Delphi
  SysUtils,
  // Project
  UConsts,
  UStrUtils;


{ TREMLTags }

class constructor TREMLTags.Create;
begin
  // Record all supported tags
  SetLength(fTagMap, 8);
  fTagMap[0] := TREMLTag.Create(rtLink, [],  'a', 'href');
  fTagMap[1] := TREMLTag.Create(rtStrong, [],  'strong');
  fTagMap[2] := TREMLTag.Create(rtEm, [],  'em');
  fTagMap[3] := TREMLTag.Create(rtVar, [],  'var');
  fTagMap[4] := TREMLTag.Create(rtPara, BlockTags,  'p');
  fTagMap[5] := TREMLTag.Create(rtWarning, [],  'warning');
  fTagMap[6] := TREMLTag.Create(rtHeading, BlockTags, 'heading');
  fTagMap[7] := TREMLTag.Create(rtMono, [], 'mono');
end;

class destructor TREMLTags.Destroy;
begin
  SetLength(fTagMap, 0);
end;

class function TREMLTags.GetCount: Integer;
begin
  Result := Length(fTagMap);
end;

class function TREMLTags.GetExclusions(Idx: Integer): TREMLTagIDs;
begin
  Result := fTagMap[Idx].Exclusions;
end;

class function TREMLTags.GetId(Idx: Integer): TREMLTagID;
begin
  Result := fTagMap[Idx].Id;
end;

class function TREMLTags.GetName(Idx: Integer): string;
begin
  Result := fTagMap[Idx].TagName;
end;

class function TREMLTags.IndexOfTagId(const Id: TREMLTagID): Integer;
var
  Idx: Integer; // loops through tag map
begin
  Result := -1;
  for Idx := 0 to Pred(Length(fTagMap)) do
  begin
    if fTagMap[Idx].Id = Id then
    begin
      Result := Idx;
      Exit;
    end;
  end;
end;

class function TREMLTags.LookupParamName(const Id: TREMLTagID;
  out ParamName: string): Boolean;
var
  Idx: Integer; // Index of tag in map
begin
  Idx := IndexOfTagId(Id);
  Result := Idx >= 0;
  if Result then
    ParamName := fTagMap[Idx].ParamName
  else
    ParamName := '';
end;

class function TREMLTags.LookupTagName(const Id: TREMLTagID;
  out TagName: string): Boolean;
var
  Idx: Integer; // Index of tag in map
begin
  Idx := IndexOfTagId(Id);
  Result := Idx >= 0;
  if Result then
    TagName := fTagMap[Idx].TagName
  else
    TagName := '';
end;

{ TREMLTags.TREMLTag }

constructor TREMLTags.TREMLTag.Create(const AId: TREMLTagID;
  const AExclusions: TREMLTagIDs; const ATagName, AParamName: string);
begin
  Id := AId;
  Exclusions := AExclusions;
  TagName := ATagName;
  ParamName := AParamName;
end;

{ TREMLEntities }

class function TREMLEntities.CharToMnemonicEntity(const Ch: Char): string;
var
  Idx: Integer; // loops thru table of entity / characters
begin
  Result := '';
  for Idx := Low(fEntityMap) to High(fEntityMap) do
  begin
    if fEntityMap[Idx].Ch = Ch then
    begin
      Result := fEntityMap[Idx].Entity;
      Break;
    end;
  end;
end;

class constructor TREMLEntities.Create;
begin
  SetLength(fEntityMap, 5);
  // Record all supported character entities
  fEntityMap[0] := TREMLEntity.Create('amp',  '&');
  fEntityMap[1] := TREMLEntity.Create('quot', DOUBLEQUOTE);
  fEntityMap[2] := TREMLEntity.Create('gt',   '>');
  fEntityMap[3] := TREMLEntity.Create('lt',   '<');
  fEntityMap[4] := TREMLEntity.Create('copy', '©');
end;

class destructor TREMLEntities.Destroy;
begin
  SetLength(fEntityMap, 0);
end;

class function TREMLEntities.GetChar(Idx: Integer): Char;
begin
  Result := fEntityMap[Idx].Ch;
end;

class function TREMLEntities.GetCount: Integer;
begin
  Result := Length(fEntityMap);
end;

class function TREMLEntities.GetEntity(Idx: Integer): string;
begin
  Result := fEntityMap[Idx].Entity;
end;

class function TREMLEntities.MapToEntity(const Ch: Char): string;
begin
  Result := CharToMnemonicEntity(Ch);
  if (Result = '') and ( (Ord(Ch) <= 31) or (Ord(Ch) >= 127) ) then
    Result := '#' + IntToStr(Ord(Ch));
end;

{ TREMLEntities.TREMLEntity }

constructor TREMLEntities.TREMLEntity.Create(const AEntity: string;
  const ACh: Char);
begin
  Entity := AEntity;
  Ch := ACh;
end;

{ TREMLAttr }

constructor TREMLAttr.Create(const AKey, AValue: string);
begin
  Key := AKey;
  Value := AValue;
end;

class function TREMLAttr.CreateNull: TREMLAttr;
begin
  Result := TREMLAttr.Create(EmptyStr, EmptyStr);
end;

function TREMLAttr.IsNull: Boolean;
begin
  Result := Key = EmptyStr;
end;

{ TREMLTagInfo }

constructor TREMLTagInfo.Create(const AID: TREMLTagID;
  const AState: TREMLTagState; const AAttr: TREMLAttr);
begin
  ID := AID;
  Attr := AAttr;
  State := AState;
end;

constructor TREMLTagInfo.Create(const AID: TREMLTagID;
  const AState: TREMLTagState);
begin
  Create(AID, AState, TREMLAttr.CreateNull);
end;

{ TREMLParser }

constructor TREMLParser.Create;
begin
  fLexer := TTaggedTextLexer.Create(TagInfo, EntityInfo);
  fParamStack := TStack<TREMLAttr>.Create;
  fOpenTagTracker := TOpenTagTracker.Create;
end;

destructor TREMLParser.Destroy;
begin
  fOpenTagTracker.Free;
  fParamStack.Free;
  fLexer.Free;
  inherited;
end;

procedure TREMLParser.DoTag(const TagInfo: TREMLTagInfo);
begin
  if Assigned(fOnTag) then
    fOnTag(Self, TagInfo);
end;

procedure TREMLParser.DoText(const Text: string);
begin
  if Assigned(fOnText) then
    fOnText(Self, Text);
end;

function TREMLParser.EntityInfo(const EntityIdx: Integer;
  out EntityName: string; out EntityChar: Char): Boolean;
begin
  Result := EntityIdx < TREMLEntities.Count;
  if not Result then
    Exit;
  EntityName := TREMLEntities.Entities[EntityIdx];
  EntityChar := TREMLEntities.Chars[EntityIdx];
end;

procedure TREMLParser.Parse(const REML: string);
var
  ParamName: string;  // name of a parameter
  ParamValue: string; // value of a parameter
  TagId: TREMLTagID;  // id of a tag
  Attr: TREMLAttr;    // attributes of tag
resourcestring
  // Error message
  sErrMissingParam = 'Expected a "%0:s" parameter value in tag "%1:s"';
  sErrNesting = 'Illegal nesting of "%0:s" tag';
begin
  fOpenTagTracker.Clear;
  try
    // Nothing to do if there is no markup
    if REML = EmptyStr then
      Exit;
    // Use lexer to process REML
    fLexer.TaggedText := REML;
    // Scan REML a token at a time
    while fLexer.NextItem <> ttsEOF do
    begin
      case fLexer.Kind of
        ttsText:
        begin
          if fLexer.PlainText <> '' then
            // Plain text: add text element (lexer will have replaced character
            // entities with actual characters
            DoText(fLexer.PlainText);
        end;
        ttsCompoundStartTag:
        begin
          TagId := TREMLTagID(fLexer.TagCode);
          if fOpenTagTracker.TagsOpen(TREMLTags.Exclusions[fLexer.TagCode]) then
            raise EREMLParseError.CreateFmt(sErrNesting, [fLexer.TagName]);
          fOpenTagTracker.OpenTag(TagId);
          TREMLTags.LookupParamName(TagId, ParamName);
          if ParamName <> '' then
          begin
            ParamValue := fLexer.TagParams.Values[ParamName];
            if ParamValue = '' then
              raise EREMLParseError.CreateFmt(
                sErrMissingParam, [ParamName, fLexer.TagName]
              );
            Attr := TREMLAttr.Create(ParamName, ParamValue);
            // Record param for use by closing tag
            fParamStack.Push(Attr);
            DoTag(TREMLTagInfo.Create(TagId, rtsOpen, Attr));
          end
          else
            DoTag(TREMLTagInfo.Create(TagId, rtsOpen));
        end;
        ttsCompoundEndTag:
        begin
          TagId := TREMLTagID(fLexer.TagCode);
          fOpenTagTracker.CloseTag(TagId);
          TREMLTags.LookupParamName(TagId, ParamName);
          if ParamName <> '' then
          begin
            // We should have a param which must be stored in closing action
            // element, but closing REML tags have no parameters. We solve this
            // by popping the parameter value from the stack. This works because
            // we use a stack for params and opening and closing tags are
            // matched.
            Attr := fParamStack.Pop;
            DoTag(TREMLTagInfo.Create(TagId, rtsClose, Attr));
          end
          else
            // No parameter
            DoTag(TREMLTagInfo.Create(TagId, rtsClose));
        end;
      end;
    end;
  except
    on E: ETaggedTextLexer do
      raise EREMLParseError.Create(E);
    else
      raise;
  end;
end;

function TREMLParser.TagInfo(const TagIdx: Integer; out TagName: string;
  out TagCode: Word; out IsContainer: Boolean): Boolean;
begin
  Result := TagIdx < TREMLTags.Count;
  if Result then
  begin
    TagName := TREMLTags.Names[TagIdx];
    TagCode := Ord(TREMLTags.Ids[TagIdx]);
    IsContainer := True;
  end;
end;

{ TREMLParser.TOpenTagTracker }

procedure TREMLParser.TOpenTagTracker.Clear;
var
  TagId: TREMLTagID;
begin
  for TagId := Low(TREMLTagID) to High(TREMLTagID) do
    fTagState[TagId] := 0;
end;

procedure TREMLParser.TOpenTagTracker.CloseTag(Tag: TREMLTagID);
begin
  if fTagState[Tag] > 0 then
    Dec(fTagState[Tag]);
end;

constructor TREMLParser.TOpenTagTracker.Create;
begin
  inherited Create;
  Clear;
end;

destructor TREMLParser.TOpenTagTracker.Destroy;
begin
  inherited;
end;

procedure TREMLParser.TOpenTagTracker.OpenTag(Tag: TREMLTagID);
begin
  Inc(fTagState[Tag]);
end;

function TREMLParser.TOpenTagTracker.TagsOpen(Tags: TREMLTagIDs): Boolean;
var
  TagId: TREMLTagID;
begin
  for TagId in Tags do
    if fTagState[TagId] > 0 then
      Exit(True);
  Result := False;
end;

end.

