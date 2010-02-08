{
 * UREMLDataIO.pas
 *
 * Implements classes that render and parse Routine Extra Markup Language (REML)
 * code. This markup is used to read and store active text objects as used by
 * the Extra property of a TRoutine object. Also includes helper classes.
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
 * The Original Code is UREMLDataIO.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UREMLDataIO;


interface


uses
  // Project
  UActiveText, UBaseObjects, UStacks, UTaggedTextLexer;


type

  {
  TREMLVersion:
    Range of valid REML version numbers.
  }
  TREMLVersion = 1..3;

  {
  TREMLReader:
    Class that parses markup used in Extra element read from snippets data
    files. Markup is translated into active text. The Extra element may occur in
    main database files and v2 of the user database and export files.
  }
  TREMLReader = class(TInterfacedObject, IActiveTextParser)
  strict private
    fLexer: TTaggedTextLexer;   // Analysis REML markup
    fParamStack: TStringStack;  // Stack of REML tag parameters for closing tags
    function TagInfo(const TagIdx: Integer; out TagName: string;
      out TagCode: Word; out IsContainer: WordBool): Boolean;
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
  protected // do not make strict
    { IActiveTextParser method }
    procedure Parse(const Markup: string; const ActiveText: IActiveText);
      {Parses markup and updates active text object with details.
        @param Markup [in] Markup containing definition of active text. Must be
          in format understood by parser.
        @param ActiveText [in] Active text object updated by parser.
      }
  public
    constructor Create;
      {Class constructor. Sets up object.
      }
    destructor Destroy; override;
      {Class destructor. Finalises object.
      }
  end;

  {
  TREMLWriter:
    Class that creates a REML markup representation of an active text object.
  }
  TREMLWriter = class(TNoPublicConstructObject)
  strict private
    fVersion: TREMLVersion; // Version of REML being written
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
    constructor InternalCreate(const Version: TREMLVersion);
      {Internal class constructor. Sets up object to render using a required
      version of REML.
        @param Version [in] Version of REML to write.
      }
  public
    class function Render(const ActiveText: IActiveText;
      const REMLVer: TREMLVersion): string;
      {Renders REML representation of an active text object.
        @param ActiveText [in] Active text to be rendered.
        @param REMLVer [in] Version of REML to be written.
        @return String containing REML markup.
      }
  end;

  {
  TREMLAnalyser:
    Static class that analyses active text and provides information about latest
    version and lowest possible version that can be used to render the REML.
  }
  TREMLAnalyser = class(TNoConstructObject)
  public
    const FIRST_VERSION = Low(TREMLVersion);    // First version of REML
    const LATEST_VERSION = High(TREMLVersion);  // Latest version of REML
    class function LowestWriterVersion(
      const ActiveText: IActiveText): TREMLVersion;
      {Determines lowest possible version REML that can be used to write some
      active text.
        @param ActiveText [in] Active text to be analysed.
        @return Minimum required REML version.
      }
  end;


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
  (C)             &copy;

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

  There are several versions of REML as follows:
  v1 - supported tags: <strong> and <a>. The href attribute of the <a> tag
       required the http:// protocol.
     - supported entities: &gt;, &lt, &quot;, &amp;.
  v2 - added tags: <em>, <var>, <warning>, <mono>, <p> and <heading>.
     - added entity: &copy;.
  v3 - changed <a> tag to accept file:// protocol in additions to http://
       protocol in href attribute.
}


uses
  // Delphi
  SysUtils, StrUtils,
  // Project
  UGC, UExceptions;


type

  {
  TREMLTags:
    Class that provides information about REML tags.
  }
  TREMLTags = class(TObject)
  strict private
    type
      {
      TREMLTag:
        Record that stores information a REML tag.
      }
      TREMLTag = record
        Id: TActiveTextElemKind;    // active text element kind
        Version: TREMLVersion;      // REML version where tag introduced
        TagName: string;            // corresponding REML tag name
        ParamName: string;          // name of any REML parameter
        constructor Create(const AId: TActiveTextElemKind;
          const AVersion: TREMLVersion; const ATagName: string;
          const AParamName: string = '');
          {Record contructor. Initialises fields.
            @param AId [in] Active text element kind.
            @param AVersion [in] REML version where tag introduced.
            @param ATagName [in] REML tag name.
            @param AParamName [in] Optional name of parameter.
          }
      end;
    var
      fTagMap: array of TREMLTag; // Details of all supported tags
    function IndexOfTagId(const Id: TActiveTextElemKind): Integer;
      {Finds index of a tag id in tag map.
        @param Id [in] Tag id to be found.
        @return Index of tag id or -1 if tag id not found.
      }
    function GetCount: Integer;
      {Read accessor for Count property.
        @return Number of supported tags.
      }
    function GetId(Idx: Integer): TActiveTextElemKind;
      {Read accessor for Ids[] property.
        @param Idx [in] Zero based index of required id.
        @return Required id.
      }
    function GetName(Idx: Integer): string;
      {Read accessor for Names[] property,
        @param Idx [in] Zero based index of required tag name.
        @return Required tag name.
      }
  public
    constructor Create;
      {Class constructor. Sets up map of REML tags.
      }
    destructor Destroy; override;
      {Class destructor. Clears tag map.
      }
    function LookupTagName(const Id: TActiveTextElemKind;
      out TagName: string): Boolean;
      {Looks up name of a tag.
        @param Id [in] Id of tag.
        @param TagName [out] Name of tag or '' if unknown id.
        @return True if tag id is valid, False if not.
      }
    function LookupParamName(const Id: TActiveTextElemKind;
      out ParamName: string): Boolean;
      {Looks up a parameter name of an identified REML tag.
        @param Id [in] Id of required tag.
        @param ParamName [out] Set to name of parameter name. '' if tag has no
          parameter or if tag id is not valid.
        @return True if tag is valid, False if not.
      }
    function LookupTagVersion(const Id: TActiveTextElemKInd;
      out Version: TREMLVersion): Boolean;
      {Looks up REML version when tag was introduced.
        @param Id [in] Id of tag.
        @param Version [out] REML version when tag was introduced.
        @return True if tag id is valid, False if not.
      }
    property Count: Integer read GetCount;
      {Number of supported tags}
    property Ids[Idx: Integer]: TActiveTextElemKind read GetId;
      {List of tag ids}
    property Names[Idx: Integer]: string read GetName;
      {List of tag names}
  end;

  {
  TREMLEntities:
    Class provides information about character entities.
  }
  TREMLEntities = class(TObject)
  strict private
    type
      {
      TREMLEntity:
        Record that associates a character with its REML mnemonic entity.
      }
      TREMLEntity = record
        Entity: string;         // Mnemonic entity
        Ch: Char;               // Character equivalent
        Version: TREMLVersion;  // REML version where entity introduced
        constructor Create(const AEntity: string; const ACh: Char;
          const AVersion: TREMLVersion);
          {Record constructor. Initialises record.
            @param AEntity [in] Mnemonic entity.
            @param ACh [in] Equivalent character.
            @param AVersion [in] REML version where entity introduced.
          }
      end;
    var
      fEntityMap: array of TREMLEntity; // Entities mapped to equivalent chars
    function CharToMnemonicEntity(const Ch: Char;
      const Ver: TREMLVersion): string;
      {Gets the mnemonic character entity that represents a character.
        @param Entity [in] Character for which equivalent entity is required.
        @param Ver [in] Version of REML for which entity is required.
        @return Required entity or '' if character has no matching mnemonic
          entity.
      }
    function GetCount: Integer;
      {Read accessor for Count property.
        @return Number of supported tags.
      }
    function GetEntity(Idx: Integer): string;
      {Read accessor for Entities[] property.
        @param Idx [in] Zero based index of required entity.
        @return Required entity.
      }
    function GetChar(Idx: Integer): Char;
      {Read accessor for Chars[] property.
        @param Idx [in] Zero based index of required character.
        @return Required character.
      }
  public
    constructor Create;
      {Class constructor. Creates map of mnemonic entities to equivalent
      characters.
      }
    destructor Destroy; override;
      {Class destructor. Tidies up object.
      }
    function MapToEntity(const Ch: Char; const Ver: TREMLVersion): string;
      {Maps a character to a character entity if appropriate.
        @param Ch [in] Character to be mapped.
        @param Ver [in] Version of REML for which entity is required.
        @return Mnemonic entity if one exists, character itself if it is
          printable and has ascii value less than 127, or a numeric character
          otherwise.
      }
    property Count: Integer read GetCount;
      {Number of supported tags}
    property Entities[Idx: Integer]: string read GetEntity;
      {List of character entities}
    property Chars[Idx: Integer]: Char read GetChar;
      {List of characters that match entities}
  end;

  {
  EREMLEntities:
    Class of exception raised when error encountered in TREMLEntities.
  }
  EREMLEntities = class(ECodeSnip);

  {
  TREMLInfo:
    Static helper class that provides information about REML tags and converts
    standard text to REML text and vice versa. This class simply maintains and
    provides easy access to TREMLTags and TREMLEntities.
  }
  TREMLInfo = class(TNoConstructObject)
  strict private
    class var fGC: IInterface;          // Garbage collector for class vars
    class var fTags: TREMLTags;         // Provides information about tags
    class var fEntities: TREMLEntities; // Converts REML char entities <=> chars
  public
    class function TagInfo: TREMLTags;
      {Provides a reference to object that provides information about a REML
      tag.
        @return Required object reference.
      }
    class function EntityInfo: TREMLEntities;
      {Provides a reference to object that provides information about REML
      character entities.
        @return Required object reference.
      }
  end;

{ TREMLReader }

constructor TREMLReader.Create;
  {Class constructor. Initialises object.
  }
begin
  inherited Create;
  fLexer := TTaggedTextLexer.Create(TagInfo, EntityInfo);
  fParamStack := TStringStack.Create;
end;

destructor TREMLReader.Destroy;
  {Class destructor. Finalises object.
  }
begin
  FreeAndNil(fParamStack);
  FreeAndNil(fLexer);
  inherited;
end;

function TREMLReader.EntityInfo(const EntityIdx: Integer;
  out EntityName: string; out EntityChar: Char): Boolean;
  {Callback that provides lexer with information about supported character
  entities. Lexer calls continually until False is returned.
    @param EntityIdx [in] Index of entity for which information is requested.
    @param EntityName [out] Set to name of character entity.
    @param EntityChar [out] Set to character associated with entity.
    @return True if entity information was provided or False to indicate no
      more entities.
  }
begin
  Result := EntityIdx < TREMLInfo.EntityInfo.Count;
  if not Result then
    Exit;
  EntityName := TREMLInfo.EntityInfo.Entities[EntityIdx];
  EntityChar := TREMLInfo.EntityInfo.Chars[EntityIdx];
end;

procedure TREMLReader.Parse(const Markup: string;
  const ActiveText: IActiveText);
  {Parses markup and updates active text object with details.
    @param Markup [in] Markup containing definition of active text. Must be in
      format understood by parser.
    @param ActiveText [in] Active text object updated by parser.
  }
var
  ParamName: string;            // name of a parameter
  ParamValue: string;           // value of a parameter
  TagId: TActiveTextElemKind;   // id of a tag
resourcestring
  // Error message
  sErrMissingParam = 'Expected a "%0:s" parameter value in tag "%1:s"';
begin
  Assert(Assigned(ActiveText), ClassName + '.Parse: ActiveText is nil');
  try
    // Nothing to do if there is no markup
    if Markup = '' then
      Exit;
    // Use lexer to process markup
    fLexer.TaggedText := Markup;
    // Scan REML a token at a time
    while fLexer.NextItem <> ttsEOF do
    begin
      case fLexer.Kind of
        ttsText:
        begin
          // Plain text: add text element (lexer will have replaced character
          // entities with actual characters
          ActiveText.AddElem(
            TActiveTextFactory.CreateTextElem(fLexer.PlainText)
          );
        end;
        ttsCompoundStartTag:
        begin
          // Start of an action element
          // Get tag id and any parameter
          TagId := TActiveTextElemKind(fLexer.TagCode);
          TREMLInfo.TagInfo.LookupParamName(TagId, ParamName);
          if ParamName <> '' then
          begin
            // We have a parameter: must not be empty
            ParamValue := fLexer.TagParams.Values[ParamName];
            if ParamValue = '' then
              raise EActiveTextParserError.CreateFmt(
                sErrMissingParam, [ParamName, fLexer.TagName]
              );
            // Record param for use by closing tag
            fParamStack.Push(ParamValue);
            // Add opening action element
            ActiveText.AddElem(
              TActiveTextFactory.CreateActionElem(TagId, ParamValue, fsOpen)
            );
          end
          else
          begin
            // No parameter: simply add opening parameterless action element
            ActiveText.AddElem(
              TActiveTextFactory.CreateActionElem(TagId, fsOpen)
            );
          end;
        end;
        ttsCompoundEndTag:
        begin
          // End of an action element
          // Get tag id and note if tag should have a parameter
          TagId := TActiveTextElemKind(fLexer.TagCode);
          TREMLInfo.TagInfo.LookupParamName(TagId, ParamName);
          if ParamName <> '' then
          begin
            // We should have a param which must be stored in closing action
            // element, but closing REML tags have no parameters. We solve this
            // by poppong the parameter value from the stack. This works because
            // we use a stack for params and opening and closing tags are
            // matched.
            ParamValue := fParamStack.Pop;
            // Add closing action element
            ActiveText.AddElem(
              TActiveTextFactory.CreateActionElem(TagId, ParamValue, fsClose)
            );
          end
          else
          begin
            // No parameter: simple add closing parameterless action element
            ActiveText.AddElem(
              TActiveTextFactory.CreateActionElem(TagId, fsClose)
            );
          end;
        end;
      end;
    end;
  except
    // Handle exceptions: convert expected exceptions to EActiveTextParserError
    on E: ETaggedTextLexer do
      raise EActiveTextParserError.Create(E.Message);
    on E: EREMLEntities do
      raise EActiveTextParserError.Create(E.Message);
    else
      raise;
  end;
end;

function TREMLReader.TagInfo(const TagIdx: Integer; out TagName: string;
  out TagCode: Word; out IsContainer: WordBool): Boolean;
  {Callback that provides lexer with information about supported tags. Lexer
  calls continually until False is returned.
    @param TagIdx [in] Index of tag for which information is requested.
    @param TagName [out] Set to name of tag.
    @param TagCode [out] Set to unique code that identifies tag.
    @param IsContainer [out] Always set to True to indicate container tag.
    @return True if tag information was provided or False to indicate no more
      tags.
  }
begin
  Result := TagIdx < TREMLInfo.TagInfo.Count;
  if Result then
  begin
    TagName := TREMLInfo.TagInfo.Names[TagIdx];
    TagCode := Ord(TREMLInfo.TagInfo.Ids[TagIdx]);
    IsContainer := True;
  end;
end;

{ TREMLWriter }

constructor TREMLWriter.InternalCreate(const Version: TREMLVersion);
  {Internal class constructor. Sets up object to render using a required
  version of REML.
    @param Version [in] Version of REML to write.
  }
begin
  inherited InternalCreate;
  fVersion := Version;
end;

class function TREMLWriter.Render(const ActiveText: IActiveText;
  const REMLVer: TREMLVersion): string;
  {Renders REML representation of an active text object.
    @param ActiveText [in] Active text to be rendered.
    @param REMLVer [in] Version of REML to be written.
    @return String containing REML markup.
  }
var
  Elem: IActiveTextElem;          // each element in active text object
  TextElem: IActiveTextTextElem;  // an active text text element
  TagElem: IActiveTextActionElem; // an active text action element
begin
  with InternalCreate(REMLVer) do
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
  if not TREMLInfo.TagInfo.LookupTagName(TagElem.Kind, TagName) then
    raise EBug.CreateFmt('%s.RenderTag: Invalid REML tag id', [ClassName]);
  Result := '';
  TREMLInfo.TagInfo.LookupParamName(TagElem.Kind, ParamName);
  case TagElem.State of
    fsClose:
      // closing tag
      Result := Format('</%s>', [TagName]);
    fsOpen:
    begin
      // opening tag: may have a parameter
      if ParamName ='' then
        Result := Format('<%s>', [TagName])
      else
        // have a parameter: value must be safely encoded
        Result := Format(
          '<%0:s %1:s="%2:s">',
          [TagName, ParamName, TextToREMLText(TagElem.Param)]
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
    Entity := TREMLInfo.EntityInfo.MapToEntity(Ch, fVersion);
    if Entity = '' then
      Result := Result + Ch
    else
      Result := Result + '&' + Entity + ';';
  end;
end;

{ TREMLInfo }

class function TREMLInfo.EntityInfo: TREMLEntities;
  {Provides a reference to object that provides information about REML character
  entities.
    @return Required object reference.
  }
begin
  if not Assigned(fEntities) then
  begin
    fEntities := TREMLEntities.Create;
    TGC.GCLocalObj(fGC, fEntities);
  end;
  Result := fEntities;
end;

class function TREMLInfo.TagInfo: TREMLTags;
  {Provides a reference to object that provides information about a REML tag.
    @return Required object reference.
  }
begin
  if not Assigned(fTags) then
  begin
    fTags := TREMLTags.Create;
    TGC.GCLocalObj(fGC, fTags);
  end;
  Result := fTags;
end;

{ TREMLTags }

constructor TREMLTags.Create;
  {Class constructor. Sets up map of REML tags.
  }
begin
  inherited Create;
  // Record all supported tags
  SetLength(fTagMap, 8);
  fTagMap[0] := TREMLTag.Create(ekLink,     1, 'a', 'href');
  fTagMap[1] := TREMLTag.Create(ekStrong,   1, 'strong');
  fTagMap[2] := TREMLTag.Create(ekEm,       2, 'em');
  fTagMap[3] := TREMLTag.Create(ekVar,      2, 'var');
  fTagMap[4] := TREMLTag.Create(ekPara,     2, 'p');
  fTagMap[5] := TREMLTag.Create(ekWarning,  2, 'warning');
  fTagMap[6] := TREMLTag.Create(ekHeading,  2, 'heading');
  fTagMap[7] := TREMLTag.Create(ekMono,     2, 'mono');
end;

destructor TREMLTags.Destroy;
  {Class destructor. Clears tag map.
  }
begin
  SetLength(fTagMap, 0);
  inherited;
end;

function TREMLTags.GetCount: Integer;
  {Read accessor for Count property.
    @return Number of supported tags.
  }
begin
  Result := Length(fTagMap);
end;

function TREMLTags.GetId(Idx: Integer): TActiveTextElemKind;
  {Read accessor for Ids[] property.
    @param Idx [in] Zero based index of required id.
    @return Required id.
  }
begin
  Result := fTagMap[Idx].Id;
end;

function TREMLTags.GetName(Idx: Integer): string;
  {Read accessor for Names[] property,
    @param Idx [in] Zero based index of required tag name.
    @return Required tag name.
  }
begin
  Result := fTagMap[Idx].TagName;
end;

function TREMLTags.IndexOfTagId(const Id: TActiveTextElemKind): Integer;
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

function TREMLTags.LookupParamName(const Id: TActiveTextElemKind;
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

function TREMLTags.LookupTagName(const Id: TActiveTextElemKind;
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

function TREMLTags.LookupTagVersion(const Id: TActiveTextElemKInd;
  out Version: TREMLVersion): Boolean;
  {Looks up REML version when tag was introduced.
    @param Id [in] Id of tag.
    @param Version [out] REML version when tag was introduced.
    @return True if tag id is valid, False if not.
  }
var
  Idx: Integer; // Index of tag in map
begin
  Idx := IndexOfTagId(Id);
  Result := Idx >= 0;
  if Result then
    Version := fTagMap[Idx].Version
  else
    Version := Low(TREMLVersion);
end;

{ TREMLTags.TREMLTag }

constructor TREMLTags.TREMLTag.Create(const AId: TActiveTextElemKind;
  const AVersion: TREMLVersion; const ATagName, AParamName: string);
  {Record contructor. Initialises fields.
    @param AId [in] Active text element kind.
    @param AVersion [in] REML version where tag introduced.
    @param ATagName [in] REML tag name.
    @param AParamName [in] Optional name of parameter.
  }
begin
  Id := AId;
  Version := AVersion;
  TagName := ATagName;
  ParamName := AParamName;
end;

{ TREMLEntities }

function TREMLEntities.CharToMnemonicEntity(const Ch: Char;
  const Ver: TREMLVersion): string;
  {Gets the mnemonic character entity that represents a character.
    @param Entity [in] Character for which equivalent entity is required.
    @param Ver [in] Version of REML for which entity is required.
    @return Required entity or '' if character has no matching mnemonic entity.
  }
var
  Idx: Integer; // loops thru table of entity / characters
begin
  Result := '';
  for Idx := Low(fEntityMap) to High(fEntityMap) do
  begin
    if (fEntityMap[Idx].Version <= Ver) and (fEntityMap[Idx].Ch = Ch) then
    begin
      Result := fEntityMap[Idx].Entity;
      Break;
    end;
  end;
end;

constructor TREMLEntities.Create;
  {Class constructor. Creates map of mnemonic entities to equivalent characters.
  }
begin
  inherited Create;
  SetLength(fEntityMap, 5);
  // Record all supported character entities
  fEntityMap[0] := TREMLEntity.Create('amp',  '&',  1);
  fEntityMap[1] := TREMLEntity.Create('quot', '"',  1);
  fEntityMap[2] := TREMLEntity.Create('gt',   '>',  1);
  fEntityMap[3] := TREMLEntity.Create('lt',   '<',  1);
  fEntityMap[4] := TREMLEntity.Create('copy', '�', 2);
end;

destructor TREMLEntities.Destroy;
  {Class destructor. Tidies up object.
  }
begin
  SetLength(fEntityMap, 0);
  inherited;
end;

function TREMLEntities.GetChar(Idx: Integer): Char;
  {Read accessor for Chars[] property.
    @param Idx [in] Zero based index of required character.
    @return Required character.
  }
begin
  Result := fEntityMap[Idx].Ch;
end;

function TREMLEntities.GetCount: Integer;
  {Read accessor for Count property.
    @return Number of supported tags.
  }
begin
  Result := Length(fEntityMap);
end;

function TREMLEntities.GetEntity(Idx: Integer): string;
  {Read accessor for Entities[] property.
    @param Idx [in] Zero based index of required entity.
    @return Required entity.
  }
begin
  Result := fEntityMap[Idx].Entity;
end;

function TREMLEntities.MapToEntity(const Ch: Char;
  const Ver: TREMLVersion): string;
  {Maps a character to a character entity if appropriate.
    @param Ch [in] Character to be mapped.
    @param Ver [in] Version of REML for which entity is required.
    @return Mnemonic entity if one exists, character itself if it is printable
      and has ascii value less than 127, or a numeric character otherwise.
  }
begin
  Result := CharToMnemonicEntity(Ch, Ver);
  if (Result = '') and ( (Ord(Ch) <= 31) or (Ord(Ch) >= 127) ) then
    Result := '#' + IntToStr(Ord(Ch));
end;

{ TREMLEntities.TREMLEntity }

constructor TREMLEntities.TREMLEntity.Create(const AEntity: string;
  const ACh: Char; const AVersion: TREMLVersion);
  {Record constructor. Initialises record.
    @param AEntity [in] Mnemonic entity.
    @param ACh [in] Equivalent character.
    @param AVersion [in] REML version where entity introduced.
  }
begin
  Entity := AEntity;
  Ch := ACh;
  Version := AVersion;
end;

{ TREMLAnalyser }

class function TREMLAnalyser.LowestWriterVersion(
  const ActiveText: IActiveText): TREMLVersion;
  {Determines lowest possible version REML that can be used to write some
  active text.
    @param ActiveText [in] Active text to be analysed.
    @return Minimum required REML version.
  }
var
  Elem: IActiveTextElem;          // each element in active text object
  TagElem: IActiveTextActionElem; // an active text action element
  TagVer: TREMLVersion;           // REML version when action element introduced
begin
  // Note: we can ignore checking for &copy; entity, introduced at ver 2, since
  // equivalent character in active text can be written in &#999; format by
  // ealier versions. Presence of &copy; is only critical when reading REML.
  Result := FIRST_VERSION;
  for Elem in ActiveText do
  begin
    if Supports(Elem, IActiveTextActionElem, TagElem) and
      (TagElem.State = fsOpen) then
    begin
      if not TREMLInfo.TagInfo.LookupTagVersion(Elem.Kind, TagVer) then
        raise EBug.Create(ClassName + '.LowestWriterVersion: TagVer not found');
      if TagVer > Result then
        Result := TagVer;
      // special case of <a href="file://...">
      if (Result < 3) and (TagElem.Kind = ekLink) and
        AnsiStartsText('file://', TagElem.Param) then
        Result := 3;
    end;
    if Result = LATEST_VERSION then
      Exit;
  end;
end;

end.

