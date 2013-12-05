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
 * Implements a main lexical analyser and subsdiary classes that can tokenise
 * code in a SGML like format. The lexer is customisable, the user providing the
 * valid tags and character entities. It checks the code for correctly nested
 * tags. Simple (<tag/>) and compound (<tag>..</tag>) tags are supported, as are
 * comments and script tags. Tags are not case sensitive. Symbolic character
 * entities are case sensitive.
}


unit UTaggedTextLexer;


interface


uses
  // Delphi
  Classes, Generics.Collections,
  // Project
  UExceptions;


type

  {
  TTaggedTextKind:
    Different kinds of tokens recognised and reported by the lexer.
  }
  TTaggedTextKind = (
    ttsNone,              // used internally to flag no token read
    ttsText,              // plain text
    ttsSimpleTag,         // a simple tag (form <tag />
    ttsCompoundStartTag,  // start of a compound tag (<tag>)
    ttsCompoundEndTag,    // end of a compound tag (</tag>)
    ttsComment,           // a comment (<! .. > or <!-- .. -->)
    ttsScript,            // a script <? .. ?>
    ttsEOF                // end of code: no more tokens
  );

  {
  TTaggedTextEntityHandler:
    Processes and translates character entities into the corresponding
    characters. Two types of entity are supported:
      1) Numeric entities of the form &#999; where 99 is a decimal number
         representing the ASCII or Unicode character code.
      2) Symbolic entities of form &entity_name;. The user specifies the names
         and character values of these entities. No symbolic entities are
         supported by default. The case of symbolic entities is significant, so
         that &AMP; is *not* the same as &amp;
  }
  TTaggedTextEntityHandler = class(TObject)
  strict private
    var
      fSymbolicEntities: TDictionary<string, Char>;
        {Map of entity names to the represented character code}
      fCurrentIdx: Integer;
        {Index in text of item (char or entity) currently being processed}
    function TranslateNumericEntity(const Entity: string): Char;
      {Translates a numeric entity into its corresponding character.
        @param Entity [in] Entity to be translated (without leading '&' and
          trailing ';' characters).
        @return Character corresponding to Entity.
        @except ETaggedTextEntityHandler raised if entity cannot be translated.
      }
    function TranslateSymbolicEntity(const Entity: string): Char;
      {Translates a symbolic entity into its corresponding character.
        @param Entity [in] Entity to be translated (without leading '&' and
          trailing ';' characters).
        @return Character corresponding to Entity.
        @except ETaggedTextEntityHandler raised if entity cannot be translated.
      }
    function TranslateEntity(const Entity: string): Char;
      {Translates an entity into the character it represents.
        @param Entity [in] Entity to be translated (without leading '&' and
          trailing ';' characters).
        @return Character corresponding to Entity.
        @except ETaggedTextEntityHandler raised if entity cannot be translated.
      }
  public
    constructor Create;
      {Constructor. Creates empty entity map.
      }
    destructor Destroy; override;
      {Destructor. Tears down object.
      }
    procedure AddEntity(const Entity: string; const Ch: Char);
      {Adds a symbolic entity and its corresponding character to the map. Set up
      this map before attempting to translate a symbolic entity.
        @param Entity [in] Character entity.
        @param Ch [in] Character corresponding to entity.
      }
    procedure TranslateTextEntities(const Text: string; out TransStr: string);
      {Finds and translates all entities in a string.
        @param Text [in] Text to be translated.
        @param TransStr [out] Translated text.
        @except ETaggedTextEntityHandler raised if entities cannot be
          translated.
      }
  end;

  {
  ETaggedTextEntityHandler:
    Class of exception raised by the TTaggedTextEntityHandler class.
  }
  ETaggedTextEntityHandler = class(EValidation);

  {
  TTaggedTextTagHandler:
    Processes tags and translates them into the corresponding tag codes. Also
    maintains a list of supported tags as supplied by user: no tags are
    supported by default. The case of tags is ignored so <TAG> and <tag> are
    taken to be the same tag.
  }
  TTaggedTextTagHandler = class(TObject)
  strict private
    type
      // Records information about a tag.
      TTagInfo = record
        Code: Word;           // Unique code associated with the tag
        IsCompound: Boolean;  // Flags whether tag is compound
        constructor Create(const ACode: Word; const AIsCompound: Boolean);
          {Create record with specified field values.
            @param ACode [in] Unique tag code.
            @param AIsCompound [in] Whether tag is compound.
          }
      end;
      // Class of exception raised when errors found in GetTagParams method.
      EAttrError = class(ECodeSnip);
    var
      fTags: TDictionary<string,TTagInfo>;
        {Maps the supported tags to information about the tag. Ignores tag case}
      fEntityHandler: TTaggedTextEntityHandler;
        {Reference to entity handler object used to translate entities appearing
        in tag values}
    function GetKind(var WorkTag: string): TTaggedTextKind;
      {Gets kind of a tag.
        @param WorkTag [in] Tag to be processed. [out] Tag stripped of
          characters that indicate tag kind, such as '/', '!' and '?'.
        @return Tag kind.
        @except ETaggedTextTagHandler raised if tag is empty
      }
    function GetTagName(const TagStr: string; out NextChPos: Integer): string;
      {Extracts the name of a tag
        @param TagStr [in] Tag contents excluding opening and closing markers.
        @param NextChPos [out] Character position following end of tag name.
        @return Tag name.
      }
    procedure LookupTagInfo(const TagName: string; out TagCode: Word;
      out IsCompound: Boolean);
      {Looks up the tag in the map of supported tags.
        @param TagName [in] Name of tag for which information is required.
        @param TagCode [out] Unique code representing tag.
        @param IsCompound [out] True if tag is compound, False if simple.
        @except Raises ETaggedTextTagHandler if tag not found.
      }
    function GetTagParams(const TagStr: string; var NextChPos: Integer;
      const Params: TStrings): Integer;
      {Gets details of a tag's parameters (attributes).
        @param TagStr [in] String containing tag information.
        @param NextChPos [in] Points to just after tag name. [out] Points to
          just after parameters.
        @param Params [in] Receives parameter information as name=value pairs.
          May be nul in which case parameter information is simply skipped.
        @return Number of parameters (attributes).
        @except Raises EAttrError if any attribute is malformed.
      }
  public
    constructor Create(const EH: TTaggedTextEntityHandler);
      {Constructor. Sets up object.
        @param EH [in] Method to call to translate entities.
      }
    destructor Destroy; override;
      {Destructor. Tears down object.
      }
    procedure AddTag(const Tag: string; const Code: Word;
      const IsCompound: Boolean);
      {Adds information about a tag to the map of tags recognised by the
      handler. Supported tags must be set up using this method before attempting
      to process them since the handler recognises no tags by default.
        @param Tag [in] Name of the tag without enclosing angle brackets.
        @param Code [in] Unique code associated with the tag - must not be
          $FFFF.
        @param IsCompound [in] True if the tag is compound (i.e. can contain
          other tags or text) or False if the tag stands on its own.
      }
    procedure ProcessTag(const Tag: string; out Text: string;
      out Kind: TTaggedTextKind; out Code: Word; const Params: TStrings);
      {Parses and extracts information from tag text.
        @param Tag [in] String containing tag information, delimited by '<' and
          '>'.
        @param Text [out] For start, end and simple tags this is name of tag,
          for comment and script tags this is the content of the tag.
        @param Kind [out] Kind of tag, or ttsNull if error.
        @param Code [out] Unique code associated with the tag ($FFFF for
          comments and scripts).
        @param Params [in] List of parameters to (attributes of) tag in
          name=value format. If nil then no parameter information is returned.
        @except Raises ETaggedTextTagHandler when errors in tag or its
          attributes encountered.
      }
  end;

  {
  ETaggedTextTagHandler:
    Class of exception raised by tagged text tag handler.
  }
  ETaggedTextTagHandler = class(ECodeSnip);

  {
  TTaggedTextTagInfoProc:
    Type of callback procedure that the lexer calls to get information about
    valid tags. The procedure is called repeatedly, getting information about a
    single tag with each call
      @param TagIdx [in] Index of tag being requested, starts at 0.
      @param TagName [out] Set to name of tag.
      @param TagCode [out] Set to a unique code representing tag. Do not use
        $FFFF.
      @param IsContainer [out] Set to true for a compound tag, False for simple
        tag.
      @return True to make lexer call this method again, False to terminate.
  }
  TTaggedTextTagInfoProc = function(const TagIdx: Integer; out TagName: string;
    out TagCode: Word; out IsContainer: Boolean): Boolean of object;

  {
  TTaggedTextEntityInfoProc:
    Type of callback procedure that lexer calls to get information about valid
    characters entities. The procedure is called repeatedly getting information
    about a single entity on each call.
      @param EntityIdx [in] Index of entity being requested, starts at 0.
      @param EntityName [out] Set to name of entity.
      @param EntityChar [out] Set to character equivalent of entity.
      @return True to make lexer call this method again, False to terminate.
  }
  TTaggedTextEntityInfoProc = function(const EntityIdx: Integer;
    out EntityName: string; out EntityChar: Char): Boolean of object;

  {
  TTaggedTextLexer:
    The class implements a lexical analyser for tagged text code (i.e. in a
    SGML like format. The lexer can detect compound (start <tag> and end </tag>)
    tags, simple (<tag/>) tags, script tags (<? .. ?>, comment tags (<!..>) and
    plain text (which may contain character entities). It provides relevant
    information about the the code element just read and only accepts predefined
    tags and entities.
  }
  TTaggedTextLexer = class(TObject)
  strict private
    fEntityHandler: TTaggedTextEntityHandler;
      {Object that parses entities and replaces entities in strings of text by
      their character values}
    fTagHandler: TTaggedTextTagHandler;
      {Object that parses tags, returning their type, id code and parameters}
    fTagStack: TStack<string>;
      {Stack of nested active compound tags}
    fNextCharPos: Integer;
      {The position of the next character to process in the tagged text}
    fTaggedText: string;
      {The text to be analysied}
    fKind: TTaggedTextKind;
      {Current token kind}
    fTagCode: Word;
      {Code representing current tag. $FFFF if a comment or script}
    fParams: TStringList;
      {Records parameters to current token if token is a tag}
    fCurText: string;
      {Storage for text for various properties}
    fInSpace: Boolean;
      {Indicates if lexer is currently processing white space}
    procedure SetTaggedText(const Value: string);
      {Setter for TaggedText property. Records new value and resets lexer ready
      to analyse the new tagged text.
        @param Value [in] New tagged text.
      }
    function GetTagName: string;
      {Getter for TagName property. Kind must be a tag.
        @return Name of current tag as string.
      }
    function GetPlainText: string;
      {Getter for PlainText property. Kind must be ttsText.
        @return Current text of a plain text token.
      }
    function GetTagParams: TStrings;
      {Getter for TagParams property. Kind must be start tag or simple tag.
        @return Reference to string list storing parameters for current tag.
      }
    function GetCommentText: string;
      {Getter for CommentText property. Kind must be ttsComment.
        @return Text of comment.
      }
    function GetScriptText: string;
      {Getter for ScriptText property. Kind must be ttsScript.
        @return Current script text.
      }
    function GetTagCode: Integer;
      {Getter for TagCode property. Kind must be a tag.
        @return Code of the current tag.
      }
    procedure GetTagInfo(const Callback: TTaggedTextTagInfoProc);
      {Gets information about valid tags by repeatedly calling a callback method
      until it returns False. The tag handler object is updated with this tag
      information.
        @param Callback [in] Callback function to call to get tag information.
      }
    procedure GetEntityInfo(const Callback: TTaggedTextEntityInfoProc);
      {Gets information about supported character entities by repeatedly calling
      a callback method until it returns False. The entity handler object is
      updated with this entity information.
        @param Callback [in] Callback function to call to get entity
          information.
      }
    procedure ProcessTag;
      {Gets and processes the next tag contained in the tagged text.
      }
    procedure ProcessPlainText;
      {Processes a sequence of characters comprising plain text.
      }
  public
    constructor Create(const TagInfoCallback: TTaggedTextTagInfoProc;
      const EntityInfoCallback: TTaggedTextEntityInfoProc);
      {Contructor. Sets up object and gets information about supported tags and
      character entities.
        @param TagInfoCallback [in] Method to call to get information about
          supported tags.
        @param EntityInfoCallback [in] Method to call to get information about
          supported character entities.
      }
    destructor Destroy; override;
      {Destructor. Tears down object.
      }
    procedure Reset;
      {Resets the lexer ready to restart the analysis of the TaggedText code.
      }
    function NextItem: TTaggedTextKind;
      {Fetches the next logical item from the tagged text. The TagCode, TagName,
      TagParams, CommentText, ScriptCode and PlainText properties are updated.
        @return Kind of next token or ttsEOF if at end of code.
      }
    property TaggedText: string read fTaggedText write SetTaggedText;
      {The code to be analysed by the lexer: setting this property calls Reset}
    property Kind: TTaggedTextKind read fKind;
      {The kind of the item read from the tagged text by the last call to
      NextItem. Kind determines which of the TagCode, TagName, TagParams,
      CommentText, ScriptCode and PlainText properties are valid when getting
      further information about the item read}
    property TagCode: Integer read GetTagCode;
      {The code number (range 0..$FFFF) of the tag read by NextItem: only valid
      when Kind is ttsCompoundStartTag, ttsCompoundEndTag or ttsSimpleTag}
    property TagName: string read GetTagName;
      {The name of the tag read by NextItem excluding tag delimiters: only valid
      when Kind is ttsCompoundStartTag, ttsCompoundEndTag or ttsSimpleTag}
    property TagParams: TStrings read GetTagParams;
      {List of parameters associated with the tag read by NextItem as Name=Value
      pairse: only valid when Kind is ttsCompoundStartTag or ttsSimpleTag}
    property CommentText: string read GetCommentText;
      {Text included in the comment read by NextItem: only valid when Kind is
      ttsComment}
    property ScriptText: string read GetScriptText;
      {Text of script enclosed by script tags read by NextItem: only valid when
      Kind is ttsScript}
    property PlainText: string read GetPlainText;
      {Plain text read by NextItem. This text includes all white space and any
      character entities know to lexer have neem translated. CRLF pairs are
      converted to LF. Only valid when Kind is ttsText}
  end;

  {
  ETaggedTextLexer:
    Class of exception raised when errors reported by the lexer.
  }
  ETaggedTextLexer = class(EValidation);


implementation


uses
  // Delphi
  SysUtils, Character,
  // Project
  UComparers, UConsts, UStructs, UStrUtils;


const
  // Character constants
  cEquals = '=';
  cQuotes = [SINGLEQUOTE, DOUBLEQUOTE];


resourcestring
  // Error messages
  sEntityEmpty = 'Empty entity';
  sEntityHasNoValue = 'Entity "#" has no numeric value';
  sEntityValueNotValid = 'Entity "%s" is not a valid numeric entity';
  sEntityOutOfRange = 'Numeric entity "%s" out of range';
  sEntityNotRecognised = 'Entity "%s" not recognised';
  sEntityUnterminated = 'Unterminated entity';
  sTagEmpty = 'Tag is empty';
  sTagNotRecognised = 'Tag "%s" not recognised';
  sSimpleTagInvalid = 'Tag "%s" is not a valid simple tag';
  sCompoundTagInvalid = 'Tag "%s" is not a valid compound tag';
  sTagNotCompound = 'Tag "%s" is not a compound tag';
  sEndTagHasParams = 'End tag "%s" should not have parameters';
  sNoMatchingStartTag = 'End tag "%s" encountered with no matching start tag';
  sNoMatchingEndTag = 'No end of tag marker found for tag beginning at %d';
  sErrorReadingTag = 'Error reading tag at character %0:d. %1:s';
  sStartAndEndTagMismatched =
    'End tag "%0:s" does not match opening tag "%1:s"';
  sErrorReadingEntities = 'Error reading entities in text. %s';
  sUnexpectedEOF = 'End of file found before all tags closed';
  sBadAttribute = 'malformed attribute';
  sBadTagAttribute = 'Tag "%0:s": %1:s';


{ TTaggedTextEntityHandler }

procedure TTaggedTextEntityHandler.AddEntity(const Entity: string;
  const Ch: Char);
  {Adds a symbolic entity and its corresponding character to the map. Set up
  this map before attempting to translate a symbolic entity.
    @param Entity [in] Character entity.
    @param Ch [in] Character corresponding to entity.
  }
begin
  if fSymbolicEntities.ContainsKey(Entity) then
    raise EBug.CreateFmt(
      '%0:s.AddEntity: Entity "%1:s" already registered', [ClassName, Entity]
    );
  fSymbolicEntities.Add(Entity, Ch)
end;

constructor TTaggedTextEntityHandler.Create;
  {Constructor. Creates empty entity map.
  }
begin
  inherited;
  fSymbolicEntities := TDictionary<string, Char>.Create(
    TStringEqualityComparer.Create
  );
end;

destructor TTaggedTextEntityHandler.Destroy;
  {Destructor. Tears down object.
  }
begin
  fSymbolicEntities.Free;
  inherited;
end;

function TTaggedTextEntityHandler.TranslateEntity(const Entity: string): Char;
  {Translates an entity into the character it represents.
    @param Entity [in] Entity to be translated (without leading '&' and trailing
      ';' characters).
    @return Character corresponding to Entity.
    @except ETaggedTextEntityHandler raised if entity cannot be translated.
  }
begin
  if Entity = '' then
    raise ETaggedTextEntityHandler.Create(
      sEntityEmpty, TSelection.Create(fCurrentIdx - 1)
    );
  if Entity[1] = '#' then
    Result := TranslateNumericEntity(Entity)
  else
    Result := TranslateSymbolicEntity(Entity);
end;

function TTaggedTextEntityHandler.TranslateNumericEntity(const Entity: string):
  Char;
  {Translates a numeric entity into its corresponding character.
    @param Entity [in] Entity to be translated (without leading '&' and trailing
      ';' characters).
    @return Character corresponding to Entity.
    @except ETaggedTextEntityHandler raised if entity cannot be translated.
  }
var
  EntityVal: Int64;     // value of a numeric entity
  EntityValStr: string; // value of a numeric entity as a string
begin
  Assert(Entity <> '',
    ClassName + '.TranslateNumericEntity: Entity is empty string');
  Assert(Entity[1] = '#',
    ClassName + '.TranslateNumericEntity: Entity[1] <> ''#''');
  if Entity = '#' then
    raise ETaggedTextEntityHandler.Create(
      sEntityHasNoValue, TSelection.Create(fCurrentIdx - 1)
    );
  // check for valid entity format
  EntityValStr := StrSliceRight(Entity, Length(Entity) - 1);
  if not TryStrToInt64(EntityValStr, EntityVal) or (EntityVal < 0) then
    raise ETaggedTextEntityHandler.CreateFmt(
      sEntityValueNotValid, [Entity], TSelection.Create(fCurrentIdx - 1)
    );
  // check if value is in range (already know >=0)
  if EntityVal > Ord(High(Char)) then
    raise ETaggedTextEntityHandler.CreateFmt(
      sEntityOutOfRange, [Entity], TSelection.Create(fCurrentIdx - 1)
    );
  // we have valid value
  Result := Char(EntityVal);
end;

function TTaggedTextEntityHandler.TranslateSymbolicEntity(
  const Entity: string): Char;
  {Translates a symbolic entity into its corresponding character.
    @param Entity [in] Entity to be translated (without leading '&' and trailing
      ';' characters).
    @return Character corresponding to Entity.
    @except ETaggedTextEntityHandler raised if entity cannot be translated.
  }
begin
  if not fSymbolicEntities.ContainsKey(Entity) then
    raise ETaggedTextEntityHandler.CreateFmt(
      sEntityNotRecognised, [Entity], TSelection.Create(fCurrentIdx - 1)
    );
  Result := fSymbolicEntities[Entity];
end;

procedure TTaggedTextEntityHandler.TranslateTextEntities(const Text: string;
  out TransStr: string);
  {Finds and translates all entities in a string.
    @param Text [in] Text to be translated.
    @param TransStr [out] Translated text.
    @except ETaggedTextEntityHandler raised if entities cannot be translated.
  }
var
  Idx: Integer;         // index used to scan text
  InsPos: Integer;      // index of insertion point in translated string
  Ch: Char;             // current char in text: used to check for entities
  EntityStart: Integer; // records start of entity in text
  Entity: string;       // stores any found entity
begin
  // Set up cursors into input and ouput strings
  Idx := 1;
  InsPos := 1;
  // Length of TransStr will be <= length of text: allocate max possible size
  SetLength(TransStr, Length(Text));
  // Scan thru each character of text to be translated
  while Idx <= Length(Text) do
  begin
    // Record current character in input for processing along with its index
    Ch := Text[Idx];
    fCurrentIdx := Idx;
    if Ch = '&' then
    begin
      // Entity
      // skip past opening '&' and record position as start of entity
      Inc(Idx);
      EntityStart := Idx;
      // scan through string looking for ';' that ends entity
      while (Idx <= Length(Text)) and (Text[Idx] <> ';') do
        Inc(Idx);
      if Idx > Length(Text) then
        raise ETaggedTextEntityHandler.Create(
          sEntityUnterminated, TSelection.Create(fCurrentIdx - 1)
        );
      // record entity excluding opening '&' and closing ';'
      Entity := StrSlice(Text, EntityStart, Idx - EntityStart);
      // skip over ending ';' in input
      Inc(Idx);
      // insert translated character in TransStr, and update its cursor
      TransStr[InsPos] := TranslateEntity(Entity);
      Inc(InsPos);
    end
    else
    begin
      // Ordinary character
      TransStr[InsPos] := Ch;
      Inc(InsPos);
      Inc(Idx);
    end;
  end;
  // If we have translated entities TransStr will be shorter than input string
  // so we reduce TransStr length accodingly
  if Idx <> InsPos then
    SetLength(TransStr, InsPos - 1);
end;

{ TTaggedTextTagHandler }

procedure TTaggedTextTagHandler.AddTag(const Tag: string; const Code: Word;
  const IsCompound: Boolean);
  {Adds information about a tag to the map of tags recognised by the handler.
  Supported tags must be set up using this method before attempting to process
  them since the handler recognises no tags by default.
    @param Tag [in] Name of the tag without enclosing angle brackets.
    @param Code [in] Unique code associated with the tag - must not be $FFFF.
    @param IsCompound [in] True if the tag is compound (i.e. can contain other
      tags or text) or False if the tag stands on its own.
  }
begin
  Assert(Code <> $FFFF, ClassName + '.AddTag: Code is reserved value $FFFF');
  // Check if tag already recorded: error if so
  if fTags.ContainsKey(Tag) then
    raise EBug.CreateFmt(
      '%0:s.AddTag: Tag "%s" already registered', [ClassName, Tag]
    );
  fTags.Add(StrToLower(Tag), TTagInfo.Create(Code, IsCompound));
end;

constructor TTaggedTextTagHandler.Create(const EH: TTaggedTextEntityHandler);
  {Constructor. Sets up object.
    @param EH [in] Method to call to translate entities.
  }
begin
  Assert(Assigned(EH), ClassName + '.Create: EH is not assigned');
  inherited Create;
  fTags := TDictionary<string, TTagInfo>.Create(TTextComparator.Create);
  fEntityHandler := EH;
end;

destructor TTaggedTextTagHandler.Destroy;
  {Destructor. Tears down object.
  }
begin
  fTags.Free;
  inherited;
end;

function TTaggedTextTagHandler.GetKind(var WorkTag: string): TTaggedTextKind;
  {Gets kind of a tag.
    @param WorkTag [in] Tag to be processed. [out] Tag stripped of characters
      that indicate tag kind, such as '/', '!' and '?'.
    @return Tag kind.
    @except ETaggedTextTagHandler raised if tag is empty
  }
var
  Len: Integer; // length of tag
begin
  Len := Length(WorkTag);
  Assert(WorkTag[1] = '<', ClassName + '.GetKind: Tag must begin with "<"');
  Assert(WorkTag[Len] = '>', ClassName + '.GetKind: Tag must end with ">"');
  if (WorkTag = '<>') or (WorkTag = '</>') then
    raise ETaggedTextTagHandler.Create(sTagEmpty);
  Assert(Len >= 3, ClassName + '.GetKind: Tag too short');
  if (Len >= 4) and (WorkTag[Len-1] = '/') then
  begin
    // tag of form <tag> => simple: we delete the / char at end of tag name
    Result := ttsSimpleTag;
    Delete(WorkTag, Len-1, 1);
  end
  else if WorkTag[2] = '/' then
  begin
    // tag of form </tag> => end of compound tag: we delete the / char
    Result := ttsCompoundEndTag;
    Delete(WorkTag, 2, 1);
  end
  else if WorkTag[2] = '!' then
  begin
    // tag is of form <!directive> or <!-- comment -->: we delete ! and any --
    Result := ttsComment;
    Delete(WorkTag, 2, 1);
    if StrPos('--', WorkTag) = 2 then
      Delete(WorkTag, 2, 2);
    if StrPos('--', WorkTag) = Length(WorkTag) - 2 then
      Delete(WorkTag, Length(WorkTag) - 2, 2);
  end
  else if WorkTag[2] = '?' then
  begin
    // tag is of form <?script> or <?script?>: we delete any ? chars
    Result := ttsScript;
    Delete(WorkTag, 2, 1);
    if WorkTag[Length(WorkTag) - 1] = '?' then
      Delete(WorkTag, Length(WorkTag) -1, 1);
  end
  else
    // tag is of form <tag>: start of compound tag: no changes to text
    Result := ttsCompoundStartTag;
  // Finally strip off delimiting < and > chars
  WorkTag := StrSlice(WorkTag, 2, Length(WorkTag) - 2);
end;

function TTaggedTextTagHandler.GetTagName(const TagStr: string;
  out NextChPos: Integer): string;
  {Extracts the name of a tag
    @param TagStr [in] Tag contents excluding opening and closing markers.
    @param NextChPos [out] Character position following end of tag name.
    @return Tag name.
  }
var
  StartPos: Integer;  // start position of tag in TagStr
begin
  Assert(Length(TagStr) >= 1, ClassName + '.GetTagName: Tag name too short');
  // Start at the beginning of the tag string
  NextChPos := 1;
  // Skip any white space before tag
  while (NextChPos <= Length(TagStr))
    and TCharacter.IsWhiteSpace(TagStr[NextChPos]) do
    Inc(NextChPos);
  // Now at start of tag name: read it up to next space or end of TagStr
  StartPos := NextChPos;
  while (NextChPos <= Length(TagStr))
    and not TCharacter.IsWhiteSpace(TagStr[NextChPos]) do
    Inc(NextChPos);
  // Copy the name from the string
  Result := StrSlice(TagStr, StartPos, NextChPos - StartPos);
  if StrIsBlank(Result) then
    raise ETaggedTextTagHandler.Create(sTagEmpty);
end;

function TTaggedTextTagHandler.GetTagParams(const TagStr: string;
  var NextChPos: Integer; const Params: TStrings): Integer;
  {Gets details of a tag's parameters (attributes).
    @param TagStr [in] String containing tag information.
    @param NextChPos [in] Points to just after tag name. [out] Points to just
      after parameters.
    @param Params [in] Receives parameter information as name=value pairs. May
      be nul in which case parameter information is simply skipped.
    @return Number of parameters (attributes).
    @except Raises EAttrError if any attribute is malformed.
  }

  // ---------------------------------------------------------------------------
  function GetNextParam(out Name, Value: string): Boolean;
    {Reads the next parameter from the current character position in the tag.
      @param Name [out] Receives parameter name.
      @param Value [out] Receives parameter value.
      @return True if a parameter was read, False if there are no more
        parameters.
      @except Raises EAttrError if the attribute is malformed.
    }
  var
    StartPos: Integer;        // start position of name or value in tag string
    AttrQuote: Char;          // kind of quote surroundin attribute values
    Len: Integer;             // length of whole tag
    EscapedValue: string;     // value before entities are translated
  begin
    // Set name & value to '' in case not found
    Name := '';
    Value := '';

    // Record length of whole tag
    Len := Length(TagStr);

    // Check to see if we have any attributes
    // skip white space
    while (NextChPos <= Len) and TCharacter.IsWhiteSpace(TagStr[NextChPos]) do
      Inc(NextChPos);
    // check if we've reached end of tag and get out if so: no params
    if NextChPos > Len then
    begin
      Result := False;
      Exit;
    end;

    // We have attribute: get it.
    // Must be in format name="value" or name='value'. "value" may contain
    // single quotes and 'value' may contain double quotes.

    // get attribute name
    StartPos := NextChPos;
    while (NextChPos <= Len)
      and not TCharacter.IsWhiteSpace(TagStr[NextChPos])
      and (TagStr[NextChPos] <> cEquals) do
      Inc(NextChPos);
    Name := StrSlice(TagStr, StartPos, NextChPos - StartPos);

    // skip any white space following name
    while (NextChPos <= Len) and TCharacter.IsWhiteSpace(TagStr[NextChPos]) do
      Inc(NextChPos);

    // MUST now have '=' character: skip over it if so, error if not
    if (NextChPos > Len) or (TagStr[NextChPos] <> cEquals) then
      raise EAttrError.Create(sBadAttribute);
    Inc(NextChPos);

    // skip white space between '=' and value
    while (NextChPos <= Len) and TCharacter.IsWhiteSpace(TagStr[NextChPos]) do
      Inc(NextChPos);

    // MUST now have a quote: record it and skip over
    if (NextChPos > Len) or not CharInSet(TagStr[NextChPos], cQuotes) then
      raise EAttrError.Create(sBadAttribute);
    AttrQuote := TagStr[NextChPos];
    Inc(NextChPos);

    // record attribute value
    StartPos := NextChPos;
    while (NextChPos <= Len) and (TagStr[NextChPos] <> AttrQuote) do
      Inc(NextChPos);
    if (NextChPos > Len) then
      raise EAttrError.Create(sBadAttribute);
    EscapedValue := StrSlice(TagStr, StartPos, NextChPos - StartPos);
    // translate any entities in value
    try
      fEntityHandler.TranslateTextEntities(EscapedValue, Value);
    except
      on E: ETaggedTextEntityHandler do
        raise EAttrError.Create(E);
    end;

    // skip over closing quote
    Inc(NextChPos);

    // Record that we found parameter
    Result := True;
  end;
  // ---------------------------------------------------------------------------

var
  Name, Value: string;  // name and value of parameter
begin
  Assert(Length(TagStr) >= 1,
    ClassName + '.GetTagParams: Tag string too short');
  // Set param count to zero
  Result := 0;
  // Loop while we have more parameters, getting the name and value of each
  while GetNextParam(Name, Value) do
  begin
    // record the name=value pair for the parameter just read, if required
    if Assigned(Params) then
      Params.Add(Name + '=' + Value);
    // count the parameter just read
    Inc(Result);
  end;
end;

procedure TTaggedTextTagHandler.LookupTagInfo(const TagName: string;
  out TagCode: Word; out IsCompound: Boolean);
  {Looks up the tag in the map of supported tags.
    @param TagName [in] Name of tag for which information is required.
    @param TagCode [out] Unique code representing tag.
    @param IsCompound [out] True if tag is compound, False if simple.
    @except Raises ETaggedTextTagHandler if tag not found.
  }
var
  Data: TTagInfo;   // information about tag
begin
  if not fTags.ContainsKey(TagName) then
    raise ETaggedTextTagHandler.CreateFmt(sTagNotRecognised, [TagName]);
  Data := fTags[TagName];
  TagCode := Data.Code;
  IsCompound := Data.IsCompound;
end;

procedure TTaggedTextTagHandler.ProcessTag(const Tag: string; out Text: string;
  out Kind: TTaggedTextKind; out Code: Word; const Params: TStrings);
  {Parses and extracts information from tag text.
    @param Tag [in] String containing tag information, delimited by '<' and '>'.
    @param Text [out] For start, end and simple tags this is name of tag, for
      comment and script tags this is the content of the tag.
    @param Kind [out] Kind of tag, or ttsNull if error.
    @param Code [out] Unique code associated with the tag ($FFFF for comments
      and scripts).
    @param Params [in] List of parameters to (attributes of) tag in name=value
      format. If nil then no parameter information is returned.
    @except Raises ETaggedTextTagHandler when errors in tag or its attributes
      encountered.
  }
var
  Len: Integer;         // length of the tag
  WorkingTag: string;   // string used to manipulate the given tag
  IsCompound: Boolean;  // true if tag is compound, false otherwise
  ChPos: Integer;       // indicates position of character to process in tag
begin
  // Clear any parameters list
  if Assigned(Params) then
    Params.Clear;
  // Store tag in working storage and record its length
  WorkingTag := Tag;
  Len := Length(WorkingTag);
  Assert(WorkingTag[1] = '<',
    ClassName + '.ProcessTag: Tag must begin with "<"');
  Assert(WorkingTag[Len] = '>',
    ClassName + '.ProcessTag: Tag must end with ">"');
  // Get kind of tag, stripping out all tag delimiting info leaving just tag
  // name and contents/parameters
  Kind := GetKind(WorkingTag);
  // Process tag content, depending on kind of tag
  try
    case Kind of
      ttsSimpleTag:
      begin
        // Simple tag
        // get tag's name: must always call this method before others then
        // extract information from a tag since this method sets the character
        // position ChPos ready to extract information following tag name
        Text := GetTagName(WorkingTag, ChPos);
        // get information about the tag from map
        LookupTagInfo(Text, Code, IsCompound);
        if IsCompound then
          raise ETaggedTextTagHandler.CreateFmt(sSimpleTagInvalid, [Text]);
        // get parameters for the tag
        GetTagParams(WorkingTag, ChPos, Params);
      end;
      ttsCompoundStartTag:
      begin
        // Compound start tag
        // get tag's name
        Text := GetTagName(WorkingTag, ChPos);
        // get information about tag from map
        LookupTagInfo(Text, Code, IsCompound);
        if not IsCompound then
          raise ETaggedTextTagHandler.CreateFmt(sCompoundTagInvalid, [Text]);
        // get parameters for the tag
        GetTagParams(WorkingTag, ChPos, Params);
      end;
      ttsCompoundEndTag:
      begin
        // Compound end tag
        // get tag's name
        Text := GetTagName(WorkingTag, ChPos);
        // get information about tag from map
        LookupTagInfo(Text, Code, IsCompound);
        if not IsCompound then
          raise ETaggedTextTagHandler.CreateFmt(sTagNotCompound, [Text]);
        // check if has params: not valid for end tags
        if GetTagParams(WorkingTag, ChPos, nil) > 0 then
          raise ETaggedTextTagHandler.CreateFmt(sEndTagHasParams, [Text]);
      end;
      ttsComment, ttsScript:
      begin
        // Comment or Script tag
        Code := $FFFF;      // not a tag in map
        Text := WorkingTag; // text to return is all that is left of tag
      end;
    end;
  except
    on E: EAttrError do
      raise ETaggedTextTagHandler.CreateFmt(
        sBadTagAttribute, [Text, E.Message]
      );
  end;
end;

{ TTaggedTextTagHandler.TTagInfo }

constructor TTaggedTextTagHandler.TTagInfo.Create(const ACode: Word;
  const AIsCompound: Boolean);
  {Create record with specified field values.
    @param ACode [in] Unique tag code.
    @param AIsCompound [in] Whether tag is compound.
  }
begin
  Code := ACode;
  IsCompound := AIsCompound;
end;

{ TTaggedTextLexer }

constructor TTaggedTextLexer.Create(
  const TagInfoCallback: TTaggedTextTagInfoProc;
  const EntityInfoCallback: TTaggedTextEntityInfoProc);
  {Contructor. Sets up object and gets information about supported tags and
  character entities.
    @param TagInfoCallback [in] Method to call to get information about
      supported tags.
    @param EntityInfoCallback [in] Method to call to get information about
      supported character entities.
  }
begin
  // Pre-conditions:
  Assert(Assigned(TagInfoCallback),
    ClassName + '.Create: TagInfoCallback is nil');
  Assert(Assigned(EntityInfoCallback),
    ClassName + '.Create: EntityInfoCallback is nil');
  inherited Create;
  // Create entity and tag handler objects used to parse tags and char entities
  fEntityHandler := TTaggedTextEntityHandler.Create;
  fTagHandler := TTaggedTextTagHandler.Create(fEntityHandler);
  // Create stack object to track nested compound tags
  fTagStack := TStack<string>.Create;
  // Create object to store a tag's parameters
  fParams := TStringList.Create;
  // Initialise ready to read tagged text
  Reset;
  // Get list of supported tags and entities using callback functions
  GetTagInfo(TagInfoCallback);
  GetEntityInfo(EntityInfoCallback);
end;

destructor TTaggedTextLexer.Destroy;
  {Destructor. Tears down object.
  }
begin
  fParams.Free;
  fEntityHandler.Free;
  fTagHandler.Free;
  fTagStack.Free;
  inherited;
end;

function TTaggedTextLexer.GetCommentText: string;
  {Getter for CommentText property. Kind must be ttsComment.
    @return Text of comment.
  }
begin
  Assert(fKind = ttsComment,
    ClassName + '.GetCommentText: current item not comment');
  Result := fCurText;
end;

procedure TTaggedTextLexer.GetEntityInfo(
  const Callback: TTaggedTextEntityInfoProc);
  {Gets information about supported character entities by repeatedly calling a
  callback method until it returns False. The entity handler object is updated
  with this entity information.
    @param Callback [in] Callback function to call to get entity information.
  }
var
  Idx: Integer;   // incrementing index number for each callback call
  Name: string;   // name of character entity
  Ch: Char;       // character associated with entity
begin
  Idx := 0;
  while Callback(Idx, Name, Ch) do
  begin
    fEntityHandler.AddEntity(Name, Ch);
    Inc(Idx);
  end;
end;

function TTaggedTextLexer.GetPlainText: string;
  {Getter for PlainText property. Kind must be ttsText.
    @return Current text of a plain text token.
  }
begin
  Assert(fKind = ttsText, ClassName + '.GetPlainText: current item not text');
  Result := fCurText;
end;

function TTaggedTextLexer.GetScriptText: string;
  {Getter for ScriptText property. Kind must be ttsScript.
    @return Current script text.
  }
begin
  Assert(fKind = ttsScript,
    ClassName + '.GetScriptText: current item not a script');
  Result := fCurText;
end;

function TTaggedTextLexer.GetTagCode: Integer;
  {Getter for TagCode property. Kind must be a tag.
    @return Code of the current tag.
  }
begin
  Assert(fKind in [ttsCompoundStartTag, ttsCompoundEndTag, ttsSimpleTag],
    ClassName + '.GetTagCode: current item not tag');
  Result := fTagCode;
end;

procedure TTaggedTextLexer.GetTagInfo(const Callback: TTaggedTextTagInfoProc);
  {Gets information about valid tags by repeatedly calling a callback method
  until it returns False. The tag handler object is updated with this tag
  information.
    @param Callback [in] Callback function to call to get tag information.
  }
var
  Idx: Integer;           // incrementing index number for each callback call
  Tag: string;            // name of supported tag
  Code: Word;             // unique code number associated with tag
  IsContainer: Boolean;   // whether the tag can contain text and/or other tags
begin
  Idx := 0;
  while Callback(Idx, Tag, Code, IsContainer) do
  begin
    fTagHandler.AddTag(Tag, Code, IsContainer);
    Inc(Idx);
  end;
end;

function TTaggedTextLexer.GetTagName: string;
  {Getter for TagName property. Kind must be a tag.
    @return Name of current tag as string.
  }
begin
  Assert(fKind in [ttsCompoundStartTag, ttsCompoundEndTag, ttsSimpleTag],
    ClassName + '.GetTagName: current item not tag');
  Result := fCurText;
end;

function TTaggedTextLexer.GetTagParams: TStrings;
  {Getter for TagParams property. Kind must be start tag or simple tag.
    @return Reference to string list storing parameters for current tag.
  }
begin
  Assert(fKind in [ttsCompoundStartTag, ttsSimpleTag],
    ClassName + '.GetTagParams: current item not a suitable tag');
  Result := fParams;
end;

function TTaggedTextLexer.NextItem: TTaggedTextKind;
  {Fetches the next logical item from the tagged text. The TagCode, TagName,
  TagParams, CommentText, ScriptCode and PlainText properties are updated.
    @return Kind of next token or ttsEOF if at end of code.
  }
begin
  // Clear any existing parameters
  fParams.Clear;
  // Scan through tagged text
  if fNextCharPos <= Length(fTaggedText) then
  begin
    if fTaggedText[fNextCharPos] = '<' then
      ProcessTag
    else
      ProcessPlainText;
  end
  else
  begin
    // At end of tagged text: it's an error if we still have unclosed tags
    if fTagStack.Count <> 0 then
      raise ETaggedTextLexer.Create(
        sUnexpectedEOF, TSelection.Create(Length(fTaggedText))
      );
    fKind := ttsEOF;
  end;
  // Return the kind of item just analysed
  Result := fKind;
end;

procedure TTaggedTextLexer.ProcessPlainText;
  {Processes a sequence of characters comprising plain text.
  }
var
  StartPos: Integer;  // index in tagged text of start of plain text
begin
  // We have plain text - process it
  fKind := ttsText;
  Assert(fNextCharPos <= Length(fTaggedText),
    ClassName + '.NextItem: Beyond end of tagged text');
  Assert(fTaggedText[fNextCharPos] <> '<',
    ClassName + '.NextItem: "<" character expected');
  // get extent of text before next tag or end of tagged text
  StartPos := fNextCharPos;
  Inc(fNextCharPos);
  while (fNextCharPos <= Length(fTaggedText))
    and (fTaggedText[fNextCharPos] <> '<') do
    Inc(fNextCharPos);
  // check the plain text for entities, replacing them with values
  try
    fEntityHandler.TranslateTextEntities(
      StrSlice(fTaggedText, StartPos, fNextCharPos - StartPos), fCurText
    );
  except
    on E: ETaggedTextEntityHandler do
    begin
      Assert(StartPos >= 0, ClassName + '.NextItem: StartPos < 0');
      raise ETaggedTextLexer.CreateFmt(
        sErrorReadingEntities,
        [E.Message],
        TSelection.Create(Cardinal(StartPos) - 1 + E.Selection.StartPos));
    end;
  end;
  // Process spaces: only first of a sequence of white space is used and
  // intervening tags have no effect on this process
  // compress all consecutive spaces down to single space
  fCurText := StrCompressWhiteSpace(fCurText);
  if fInSpace then
    // space has been emitted in current sequence, strip from start of fCurText
    fCurText := StrTrimLeft(fCurText);
  if fCurText = '' then
    Exit;
  // we are processing white space only if last char is a space: this space is
  // being handled here so any immediately following spaces must be ignored.
  fInSpace := TCharacter.IsWhiteSpace(fCurText[Length(fCurText)]);
end;

procedure TTaggedTextLexer.ProcessTag;
  {Gets and processes the next tag contained in the tagged text.
  }
var
  StartPos: Integer;          // index in tagged text of start of tag
  Tag: string;                // tag to be processed
  ExpectedClosingTag: string; // tag expected to close a compound tag
begin
  // get tag name from text
  StartPos := fNextCharPos;
  Assert(StartPos >= 1, ClassName + '.NextItem: StartPos < 1');
  while (fNextCharPos <= Length(fTaggedText))
    and (fTaggedText[fNextCharPos] <> '>') do
    Inc(fNextCharPos);
  if fNextCharPos > Length(fTaggedText) then
    raise ETaggedTextLexer.CreateFmt(
      sNoMatchingEndTag, [StartPos - 1], TSelection.Create(StartPos - 1)
    );
  Assert(fTaggedText[fNextCharPos] = '>', ClassName + 'NextTag: ">" expected');
  // skip over tag closer
  Inc(fNextCharPos);
  // get info about tag
  Tag := StrSlice(fTaggedText, StartPos, fNextCharPos - StartPos);
  try
    fTagHandler.ProcessTag(Tag, fCurText, fKind, fTagCode, fParams);
  except
    on E: ETaggedTextTagHandler do
      raise ETaggedTextLexer.CreateFmt(
        sErrorReadingTag,
        [StartPos - 1, E.Message],
        TSelection.Create(StartPos - 1)
      );
  end;
  // do any special processing relating to different types of tags
  case fKind of
    ttsCompoundStartTag:
      // compound start tag: push onto stack of open tags
      fTagStack.Push(fCurText);
    ttsCompoundEndTag:
    begin
      // compound end tag: check validity
      if fTagStack.Count = 0 then
        // tag stack empty => no matching opening tag
        raise ETaggedTextLexer.CreateFmt(
          sNoMatchingStartTag, [fCurText], TSelection.Create(StartPos - 1)
        );
      // check tag matches opening tag at top of stack
      ExpectedClosingTag := fTagStack.Pop;
      if not StrSameText(fCurText, ExpectedClosingTag) then
        raise ETaggedTextLexer.CreateFmt(
          sStartAndEndTagMismatched,
          [fCurText, ExpectedClosingTag],
          TSelection.Create(StartPos - 1)
        );
    end;
    ttsSimpleTag, ttsComment, ttsScript:
      // no special processing for these kinds
      ;
  end;
end;

procedure TTaggedTextLexer.Reset;
  {Resets the lexer ready to restart the analysis of the TaggedText code.
  }
begin
  fNextCharPos := 1;
  fTagStack.Clear;
  fKind := ttsNone;
end;

procedure TTaggedTextLexer.SetTaggedText(const Value: string);
  {Setter for TaggedText property. Records new value and resets lexer ready to
  analyse the new tagged text.
    @param Value [in] New tagged text.
  }
begin
  fTaggedText := Value;
  Reset;
end;

end.

