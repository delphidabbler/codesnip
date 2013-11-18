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
 * Implements classes that render and parse Routine Extra Markup Language (REML)
 * code. This markup is used to read and store active text objects as used by
 * the Extra property of a TSnippet object. Also includes helper classes.
}


unit UREMLDataIO;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  ActiveText.UMain,
  UBaseObjects,
  UExceptions,
  UTaggedTextLexer;


type
  TREMLTagID = (
    rtLink,         // link element: has a URL (inline)
    rtStrong,       // text formatted as strong (inline)
    rtEm,           // text formatted as emphasised (inline)
    rtVar,          // text formatted as variable (inline)
    rtPara,         // delimits a paragraph (block level)
    rtWarning,      // text formatted as a warning (inline)
    rtHeading,      // delimits a heading (block level)
    rtMono          // text formatted as mono spaced (inline)
  );

  TREMLTagIDs = set of TREMLTagID;

  TREMLAttr = record
    Key: string;
    Value: string;
    constructor Create(const AKey, AValue: string);
    class function CreateNull: TREMLAttr; static;
    function IsNull: Boolean;
  end;

  TREMLTagState = (rtsOpen, rtsClose);

  TREMLTagInfo = record
    ID: TREMLTagID;
    Attr: TREMLAttr;
    State: TREMLTagState;
    constructor Create(const AID: TREMLTagID; const AState: TREMLTagState;
      const AAttr: TREMLAttr); overload;
    constructor Create(const AID: TREMLTagID; const AState: TREMLTagState);
      overload;
  end;

  {
  TREMLTags:
    Class that provides information about REML tags.
  }
  TREMLTags = class(TNoConstructObject)
  strict private
    const
      BlockTags = [rtPara, rtHeading];
  strict private
    type
      {
      TREMLTag:
        Record that stores information a REML tag.
      }
      TREMLTag = record
      public
        Id: TREMLTagID;  // active text element kind
        // ids of tags that can't nest inside this tag
        Exclusions: TREMLTagIDs;
        TagName: string;                // corresponding REML tag name
        ParamName: string;              // name of any REML parameter
        constructor Create(const AId: TREMLTagID;
          const AExclusions: TREMLTagIDs; const ATagName: string;
          const AParamName: string = '');
          {Record contructor. Initialises fields.
            @param AId [in] Tag identifier.
            @param AExclusions [in] IDs of tags that can't be nested within this
              tag.
            @param ATagName [in] REML tag name.
            @param AParamName [in] Optional name of parameter.
          }
      end;
  strict private
    class var fTagMap: TArray<TREMLTag>;
      {Details of all supported tags}
    class function IndexOfTagId(const Id: TREMLTagID): Integer;
      {Finds index of a tag id in tag map.
        @param Id [in] Tag id to be found.
        @return Index of tag id or -1 if tag id not found.
      }
    class function GetCount: Integer; static;
      {Read accessor for Count property.
        @return Number of supported tags.
      }
    class function GetId(Idx: Integer): TREMLTagID; static;
      {Read accessor for Ids[] property.
        @param Idx [in] Zero based index of required id.
        @return Required id.
      }
    class function GetName(Idx: Integer): string; static;
      {Read accessor for Names[] property,
        @param Idx [in] Zero based index of required tag name.
        @return Required tag name.
      }
    class function GetExclusions(Idx: Integer): TREMLTagIDs; static;
  public
    class constructor Create;
      {Class constructor. Sets up map of REML tags.
      }
    class destructor Destroy;
      {Class destructor. Clears tag map.
      }
    class function LookupTagName(const Id: TREMLTagID; out TagName: string):
      Boolean;
      {Looks up name of a tag.
        @param Id [in] Id of tag.
        @param TagName [out] Name of tag or '' if unknown id.
        @return True if tag id is valid, False if not.
      }
    class function LookupParamName(const Id: TREMLTagID; out ParamName: string):
      Boolean;
      {Looks up a parameter name of an identified REML tag.
        @param Id [in] Id of required tag.
        @param ParamName [out] Set to name of parameter name. '' if tag has no
          parameter or if tag id is not valid.
        @return True if tag is valid, False if not.
      }
    class property Count: Integer read GetCount;
      {Number of supported tags}
    class property Ids[Idx: Integer]: TREMLTagID read GetId;
      {List of tag ids}
    class property Names[Idx: Integer]: string read GetName;
      {List of tag names}
    class property Exclusions[Idx: Integer]: TREMLTagIDs read GetExclusions;
      {Set of IDs of tags that can't be nested within this tag}
  end;

  {
  TREMLEntities:
    Static class that provides information about character entities.
  }
  TREMLEntities = class(TNoConstructObject)
  strict private
    type
      {
      TREMLEntity:
        Record that associates a character with its REML mnemonic entity.
      }
      TREMLEntity = record
        Entity: string;         // Mnemonic entity
        Ch: Char;               // Character equivalent
        constructor Create(const AEntity: string; const ACh: Char);
          {Record constructor. Initialises record.
            @param AEntity [in] Mnemonic entity.
            @param ACh [in] Equivalent character.
          }
      end;
    class var fEntityMap: TArray<TREMLEntity>; // Entity <=> character map
    class function CharToMnemonicEntity(const Ch: Char): string;
      {Gets the mnemonic character entity that represents a character.
        @param Entity [in] Character for which equivalent entity is required.
        @return Required entity or '' if character has no matching mnemonic
          entity.
      }
    class function GetCount: Integer; static;
      {Read accessor for Count property.
        @return Number of supported tags.
      }
    class function GetEntity(Idx: Integer): string; static;
      {Read accessor for Entities[] property.
        @param Idx [in] Zero based index of required entity.
        @return Required entity.
      }
    class function GetChar(Idx: Integer): Char; static;
      {Read accessor for Chars[] property.
        @param Idx [in] Zero based index of required character.
        @return Required character.
      }
  public
    class constructor Create;
      {Class constructor. Creates map of mnemonic entities to equivalent
      characters.
      }
    class destructor Destroy;
      {Class destructor. Clears entity map
      }
    class function MapToEntity(const Ch: Char): string;
      {Maps a character to a character entity if appropriate.
        @param Ch [in] Character to be mapped.
        @return Mnemonic entity if one exists, character itself if it is
          printable and has ascii value less than 127, or a numeric character
          otherwise.
      }
    class property Count: Integer read GetCount;
      {Number of supported tags}
    class property Entities[Idx: Integer]: string read GetEntity;
      {List of character entities}
    class property Chars[Idx: Integer]: Char read GetChar;
      {List of characters that match entities}
  end;

  TREMLParser = class(TObject)
  public
    type
      TParseTextEvent = procedure(Sender: TObject; const Text: string)
        of object;
      TParseTagEvent = procedure (Sender: TObject; const TagInfo: TREMLTagInfo)
        of object;
  strict private
    type
      TOpenTagTracker = class(TObject)
      strict private
        var
          fTagState: array[TREMLTagID] of Cardinal;
      public
        constructor Create;
        destructor Destroy; override;
        procedure Clear;
        procedure OpenTag(Tag: TREMLTagID);
        procedure CloseTag(Tag: TREMLTagID);
        function TagsOpen(Tags: TREMLTagIDs): Boolean;
      end;
  strict private
    var
      fOnText: TParseTextEvent;
      fOnTag: TParseTagEvent;
      fLexer: TTaggedTextLexer;     // Analysis REML markup
      fOpenTagTracker: TOpenTagTracker;
      // Stack of tag params for use in closing tags
      fParamStack: TStack<TREMLAttr>;
    function TagInfo(const TagIdx: Integer; out TagName: string;
      out TagCode: Word; out IsContainer: Boolean): Boolean;
      {Callback that provides lexer with information about supported tags. Lexer
      calls continually until False is returned.
        @param TagIdx [in] Index of tag for which information is requested.
        @param TagName [out] Set to name of tag.
        @param TagCode [out] Set to unique code that identifies tag.
        @param IsContainer [out] Always set to True to indicate container tag.
        @return True if tag information was provided or False to indicate no
          more tags.
      }
    function EntityInfo(const EntityIdx: Integer; out EntityName: string;
      out EntityChar: Char): Boolean;
      {Callback that provides lexer with information about supported character
      entities. Lexer calls continually until False is returned.
        @param EntityIdx [in] Index of entity for which information is
          requested.
        @param EntityName [out] Set to name of character entity.
        @param EntityChar [out] Set to character associated with entity.
        @return True if entity information was provided or False to indicate no
          more entities.
      }
    procedure DoText(const Text: string);
    procedure DoTag(const TagInfo: TREMLTagInfo);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const REML: string);
    property OnText: TParseTextEvent read fOnText write fOnText;
    property OnTag: TParseTagEvent read fOnTag write fOnTag;
  end;

  {
  TREMLWriter:
    Class that creates a REML markup representation of an active text object.
  }
  TREMLWriter = class(TNoPublicConstructObject)
  strict private
    function TextToREMLText(const Text: string): string;
      {Converts plain text to REML compatible text by replacing illegal
      characters with related character entities.
        @param Text [in] Plain text to be converted.
        @return Converted text.
      }
    function RenderTag(const TagElem: IActiveTextActionElem): string;
      {Renders an active text action element as a REML tag.
        @param TagElem [in] Active text action element to be rendered.
        @return Required REML tag.
      }
    function RenderText(const TextElem: IActiveTextTextElem): string;
      {Renders an active text text element. Illegal characters are converted to
      REML character entities.
        @param TextElem [in] Active text text element.
        @return REML-safe text containing necessary character entities.
      }
  strict protected
    constructor InternalCreate;
      {Internal class constructor. Sets up object to render active text document
      as REML.
      }
  public
    class function Render(const ActiveText: IActiveText): string;
      {Renders REML representation of an active text object.
        @param ActiveText [in] Active text to be rendered.
        @return String containing REML markup.
      }
  end;

  EREMLParseError = class(ECodeSnip);

implementation


{
  About REML (Routine Extra Markup Language)
  -----------------------------------------

  The markup is simplified form of (X)HTML.

  It comprises plain text with limited inline and block level formatting and
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
  UConsts, UStrUtils;


{ TREMLWriter }

constructor TREMLWriter.InternalCreate;
  {Internal class constructor. Sets up object to render active text document
  as REML.
  }
begin
  inherited InternalCreate;
end;

class function TREMLWriter.Render(const ActiveText: IActiveText): string;
  {Renders REML representation of an active text object.
    @param ActiveText [in] Active text to be rendered.
    @return String containing REML markup.
  }
var
  Elem: IActiveTextElem;          // each element in active text object
  TextElem: IActiveTextTextElem;  // an active text text element
  TagElem: IActiveTextActionElem; // an active text action element
begin
  with InternalCreate do
    try
      Result := '';
      for Elem in ActiveText do
      begin
        if Supports(Elem, IActiveTextTextElem, TextElem) then
          Result := Result + RenderText(TextElem)
        else if Supports(Elem, IActiveTextActionElem, TagElem) then
          Result := Result + RenderTag(TagElem);
      end;
    finally
      Free;
    end;
end;

function TREMLWriter.RenderTag(
  const TagElem: IActiveTextActionElem): string;
  {Renders an active text action element as a REML tag.
    @param TagElem [in] Active text action element to be rendered.
    @return Required REML tag.
  }
var
  TagName: string;    // name of tag
  ParamName: string;  // name of any parameter
begin
  // TODO: deal with this temp cast to TREMLTagID
  if not TREMLTags.LookupTagName(TREMLTagID(TagElem.Kind), TagName) then
    raise EBug.CreateFmt('%s.RenderTag: Invalid REML tag id', [ClassName]);
  Result := '';
  // TODO: deal with this temp cast to TREMLTagID
  TREMLTags.LookupParamName(TREMLTagID(TagElem.Kind), ParamName);
  case TagElem.State of
    fsClose:
    begin
      // closing tag
      Result := Format('</%s>', [TagName]);
      if TagElem.DisplayStyle = dsBlock then
        Result := Result + EOL;
    end;
    fsOpen:
    begin
      // opening tag: may have a parameter
      if ParamName ='' then
        Result := Format('<%s>', [TagName])
      else
        // have a parameter: value must be safely encoded
        Result := Format(
          '<%0:s %1:s="%2:s">',
          [
            TagName,
            ParamName,
            TextToREMLText(TagElem.Attrs[TActiveTextAttrNames.Link_URL])
          ]
        );
    end;
  end;
end;

function TREMLWriter.RenderText(
  const TextElem: IActiveTextTextElem): string;
  {Renders an active text text element. Illegal characters are converted to
  REML character entities.
    @param TextElem [in] Active text text element.
    @return REML-safe text containing necessary character entities.
  }
begin
  Result := TextToREMLText(TextElem.Text);
end;

function TREMLWriter.TextToREMLText(const Text: string): string;
  {Converts plain text to REML compatible text by replacing illegal characters
  with related character entities.
    @param Text [in] Plain text to be converted.
    @return Converted text.
  }
var
  Ch: Char;         // each character in plain text
  Entity: string;   // stores each required entity
begin
  Result := '';
  for Ch in Text do
  begin
    Entity := TREMLEntities.MapToEntity(Ch);
    if Entity = '' then
      Result := Result + Ch
    else
      Result := Result + '&' + Entity + ';';
  end;
end;

{ TREMLTags }

class constructor TREMLTags.Create;
  {Class constructor. Sets up map of REML tags.
  }
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
  {Class destructor. Clears tag map.
  }
begin
  SetLength(fTagMap, 0);
end;

class function TREMLTags.GetCount: Integer;
  {Read accessor for Count property.
    @return Number of supported tags.
  }
begin
  Result := Length(fTagMap);
end;

class function TREMLTags.GetExclusions(Idx: Integer): TREMLTagIDs;
begin
  Result := fTagMap[Idx].Exclusions;
end;

class function TREMLTags.GetId(Idx: Integer): TREMLTagID;
  {Read accessor for Ids[] property.
    @param Idx [in] Zero based index of required id.
    @return Required id.
  }
begin
  Result := fTagMap[Idx].Id;
end;

class function TREMLTags.GetName(Idx: Integer): string;
  {Read accessor for Names[] property,
    @param Idx [in] Zero based index of required tag name.
    @return Required tag name.
  }
begin
  Result := fTagMap[Idx].TagName;
end;

class function TREMLTags.IndexOfTagId(const Id: TREMLTagID):
  Integer;
  {Finds index of a tag id in tag map.
    @param Id [in] Tag id to be found.
    @return Index of tag id or -1 if tag id not found.
  }
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
  {Looks up a parameter name of an identified REML tag.
    @param Id [in] Id of required tag.
    @param ParamName [out] Set to name of parameter name. '' if tag has no
      parameter or if tag id is not valid.
    @return True if tag is valid, False if not.
  }
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
  {Looks up name of a tag.
    @param Id [in] Id of tag.
    @param TagName [out] Name of tag or '' if unknown id.
    @return True if tag id is valid, False if not.
  }
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
  {Record contructor. Initialises fields.
    @param AId [in] Active text element kind.
    @param ATagName [in] REML tag name.
    @param AParamName [in] Optional name of parameter.
  }
begin
  Id := AId;
  Exclusions := AExclusions;
  TagName := ATagName;
  ParamName := AParamName;
end;

{ TREMLEntities }

class function TREMLEntities.CharToMnemonicEntity(const Ch: Char): string;
  {Gets the mnemonic character entity that represents a character.
    @param Entity [in] Character for which equivalent entity is required.
    @return Required entity or '' if character has no matching mnemonic entity.
  }
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
  {Class constructor. Creates map of mnemonic entities to equivalent characters.
  }
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
  {Class destructor. Clears entity map.
  }
begin
  SetLength(fEntityMap, 0);
end;

class function TREMLEntities.GetChar(Idx: Integer): Char;
  {Read accessor for Chars[] property.
    @param Idx [in] Zero based index of required character.
    @return Required character.
  }
begin
  Result := fEntityMap[Idx].Ch;
end;

class function TREMLEntities.GetCount: Integer;
  {Read accessor for Count property.
    @return Number of supported tags.
  }
begin
  Result := Length(fEntityMap);
end;

class function TREMLEntities.GetEntity(Idx: Integer): string;
  {Read accessor for Entities[] property.
    @param Idx [in] Zero based index of required entity.
    @return Required entity.
  }
begin
  Result := fEntityMap[Idx].Entity;
end;

class function TREMLEntities.MapToEntity(const Ch: Char): string;
  {Maps a character to a character entity if appropriate.
    @param Ch [in] Character to be mapped.
    @return Mnemonic entity if one exists, character itself if it is printable
      and has ascii value less than 127, or a numeric character otherwise.
  }
begin
  Result := CharToMnemonicEntity(Ch);
  if (Result = '') and ( (Ord(Ch) <= 31) or (Ord(Ch) >= 127) ) then
    Result := '#' + IntToStr(Ord(Ch));
end;

{ TREMLEntities.TREMLEntity }

constructor TREMLEntities.TREMLEntity.Create(const AEntity: string;
  const ACh: Char);
  {Record constructor. Initialises record.
    @param AEntity [in] Mnemonic entity.
    @param ACh [in] Equivalent character.
  }
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

