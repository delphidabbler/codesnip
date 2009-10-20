{
 * UTaggedTextLexer.pas
 *
 * Implements a main lexical analyser and subsdiary classes that can tokenise
 * code in a SGML like format. The lexer is customisable, the user providing the
 * valid tags and character entities. It checks the code for correctly nested
 * tags. Simple (<tag/>) and compound (<tag>..</tag>) tags are supported, as are
 * comments and script tags.
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
 * The Original Code is UTaggedTextLexer.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UTaggedTextLexer;


interface


uses
  // Delphi
  SysUtils, Classes,
  // Project
  UExceptions, UStacks;


const
  // Character constants made public
  cSingleQuote = '''';
  cDoubleQuote = '"';
  cWhiteSpace = [#0..#32];
  cQuotes = [cSingleQuote, cDoubleQuote];


type

  {
  ETaggedTextLexer:
    Class of exception raised when errors reported by the lexer and its
    supporting classes.
  }
  ETaggedTextLexer = class(ECodeSnip);

  {
  TTaggedTextKind:
    Different kinds of tokens recognised and reported by the lexer.
  }
  TTaggedTextKind = (
    ttsNull,              // used internally to flag errors
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
      1) Numeric entities of the form &#99; where 99 is a decimal number
         representing the ASCII or Unicode character code. These entities are
         supported by default.
      2) Symbolic entities of form &entity_name;. The user specifies the names
         and character values of these entities. No entities are supported by
         default.
    Errors are reported via the LastErrorMessage property. The case of entities
    is significant, so that &AMP; is *not* the same as &amp;
  }
  TTaggedTextEntityHandler = class(TObject)
  strict private
    fLastErrorMessage: string;
      {Value of LastErrorMessage property}
    fSymbolicEntities: TStringList;
      {String list storing entities with entity name stored as string item and
      the code of the represented character stored in Objects[]}
  public
    constructor Create;
      {Class constructor. Creates empty entity list.
      }
    destructor Destroy; override;
      {Class destructor. Frees owned entity list.
      }
    { TODO -oSelf -cNote : Unicode Fix: Changed AnsiChar to Char }
    function AddEntity(const Entity: string; const Ch: Char): Boolean;
      {Adds a symbolic entity and its corresponding character to the
      list of symbolic entities. Set up the list of entities before attempting
      to translate an entity.
        @param Entity [in] Character entity.
        @param Ch [in] Character corresponding to entity.
        @return True if entity added successfully, False if not.
      }
    { TODO -oSelf -cNote : Unicode Fix: Changed AnsiChar to Char }
    function TranslateEntity(const Entity: string; out Ch: Char): Boolean;
      {Translates entity into the character it represents.
        @param Entity [in] Entity to be translated (without leading '&' and
          trailing ';' characters).
        @param Ch [out] Character corresponding to Entity. and returns it via
          Ch.
        @return True if entity is translated or False if not.
      }
    function TranslateTextEntities(const Text: string;
      out TransStr: string): Boolean;
      {Finds and translates all entities in a string.
        @param Text [in] Text to be translated.
        @param TransStr [out] Translated text.
        @return True on success or False on error.
      }
    property LastErrorMessage: string read fLastErrorMessage;
      {Stores a description of the last reported error. The error message is
      *not* reset when methods execute successfully}
  end;

  {
  TTaggedTextEntityHandler:
    Processes tags and translates them into the corresponding tag codes. Also
    maintains a list of supported tags as supplied by user: no tags are
    supported by default. Errors are reported via the LastErrorMessage property.
  }
  TTaggedTextTagHandler = class(TObject)
  strict private
    fLastErrorMessage: string;
      {Value of LastErrorMessage property}
    fTagList: TStringList;
      {Stores details of the supported tags. The tag name is stored as a string
      item and the tag code and whether tag is compound are merged into a long
      word that is stored in Objects[]}
    fEntityHandler: TTaggedTextEntityHandler;
      {Reference to entity handler object used to translate entities appearing
      in tag values}
    function GetKind(var WorkTag: string): TTaggedTextKind;
      {Gets kind of a tag.
        @param WorkTag [in] Tag to be processed. [out] Tag stripped of
          characters that indicate tag kind, such as '/', '!' and '?'.
        @return Tag kind (ttsNull if kind is not recognised.
      }
    function GetTagName(const TagStr: string; out NextChPos: Integer): string;
      {Extracts the name of a tag
        @param TagStr [in] Tag contents excluding opening and closing markers.
        @param NextChPos [out] Character position following end of tag name.
        @return Tag name.
      }
    function LookupTagInfo(const TagName: string; out TagCode: Word;
      out IsCompound: WordBool): Boolean;
      {Looks up the tag in the table of supported tags.
        @param TagName [in] Name of tag for which information is required.
        @param TagCode [out] Unique code representing tag.
        @param IsCompound [out] True if tag is compound, False if simple.
        @return True if tag was found, False if not.
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
      }
    procedure Error(const Msg: string); overload;
      {Records an error message in the LastErrorMessage property.
        @param Msg [in] Message to be recorded.
      }
    procedure Error(const Fmt: string; const Args: array of const); overload;
      {Records an error message in the LastErrorMessage property.
        @param Fmt [in] Message template.
        @param Args [in] Arguments inserted in message template.
      }
  public
    constructor Create(const EH: TTaggedTextEntityHandler);
      {Class constructor. Sets up object.
        @param EH [in] Method to call to translate entities.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    function AddTag(const Tag: string; const Code: Word;
      const IsCompound: WordBool): Boolean;
      {Adds information about a tag to the list of tags recognised by the
      handler. A list of supported tags must be set up using this method before
      attempting to process them since the handler recognises no tags by
      default.
        @param Tag [in] Name of the tag without enclosing angle brackets.
        @param Code [in] Unique code associated with the tag - must not be
          $FFFF.
        @param IsCompound [in] True if the tag is compound (i.e. can contain
          other tags or text) or False if the tag stands on its own.
        @return True if tag is added successfully or False on error.
      }
    function ProcessTag(const Tag: string; out Text: string;
      out Kind: TTaggedTextKind; out Code: Word;
      const Params: TStrings): Boolean;
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
        @return True if the tag is processed successfully, False otherwise.
      }
    property LastErrorMessage: string read fLastErrorMessage;
      {Stores a description of the last reported error. The error message is
      *not* reset when methods execute successfully}
  end;

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
    out TagCode: Word; out IsContainer: WordBool): Boolean of object;

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
  { TODO -oSelf -cNote : Unicode Fix: Changed EntityChar from AnsiChar to Char }
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
    fTagStack: TStringStack;
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
    procedure SetTaggedText(const Value: string);
      {Setter for TaggedText property. Records new value and resets lexer ready
      to analyse the new tagged text.
        @param Value [in] New tagged text.
      }
    function GetTagName: string;
      {Getter for TagName property.
        @return Name of current tag as string.
        @except Raises exception if current tagged text item is not a tag.
      }
    function GetPlainText: string;
      {Getter for PlainText property.
        @return Current text of a plain text token.
        @except Raises exception if current tagged text item is not plain text.
      }
    function GetTagParams: TStrings;
      {Getter for TagParams property.
        @return Reference to string list storing parameters for current tag.
        @except Raises exception if current tagged text item is not a tag.
      }
    function GetCommentText: string;
      {Getter for CommentText property.
        @return Text of comment.
        @except Raises exception if current tagged text item is not a comment.
      }
    function GetScriptText: string;
      {Getter for ScriptText property.
        @return Current script text.
        @except Raises exception if current tagged text item is not a script.
      }
    function GetTagCode: Integer;
      {Getter for TagCode property.
        @return Code of the current tag.
        @except Raises exception if Kind of current item is not a tag.
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
  public
    constructor Create(const TagInfoCallback: TTaggedTextTagInfoProc;
      const EntityInfoCallback: TTaggedTextEntityInfoProc);
      {Class contructor. Sets up object and gets information about supported
      tags and character entities.
        @param TagInfoCallback [in] Method to call to get information about
          supported tags.
        @param EntityInfoCallback [in] Method to call to get information about
          supported character entities.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
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


implementation


uses
  // Delphi
  StrUtils, Windows {for inlining},
  // Project
  UUtils;


resourcestring
  // Error messages
  sEntityAlreadyReg = 'Entity "%s" already registered';
  sEntityEmpty = 'Empty entity';
  sEntityHasNoValue = 'Entity "#" has no numeric value';
  sEntityValueNotValid = 'Entity "%s" is not a valid non-negative number';
  sEntityOutOfRange = 'Numeric entity "%s" out of range';
  sEntityNotRecognised = 'Entity "%s" not recognised';
  sEntityUnterminated = 'Unterminated entity';
  sTagCodeInvalid = 'Tag "%s" cannot have code $FFFF';
  sTagAlreadyRegistered = 'Tag "%s" already registered';
  sTagEmpty = 'Tag is empty';
  sTagNotRecognised = 'Tag "%s" not recognised';
  sSimpleTagInvalid = 'Tag "%s" is not a valid simple tag';
  sCompoundTagInvalid = 'Tag "%s" is not a valid compound tag';
  sTagNotCompound = 'Tag "%s" is not a compound tag';
  sEndTagHasParams = 'End tag "%s" should not have parameters';
  sItemNotComment = 'Can''t read comment: current item not comment';
  sItemNotText = 'Can''t read text: current item not text';
  sItemNotScript = 'Can''t read script: current item not a script';
  sItemNotTagCode = 'Can''t read tag code: current item not tag';
  sItemNotTag = 'Can''t read tag name: current item not tag';
  sCantReadParams = 'Can''t read tag params: current item not a suitable tag';
  sNoMatchingStartTag = 'End tag "%s" encountered with no matching start tag';
  sNoMatchingEndTag = 'No end of tag marker found for tag beginning at %d';
  sErrorReadingTag = 'Error reading tag at character %0:d. %1:s';
  sStartAndEndTagMismatched =
    'End tag "%0:s" does not match opening tag "%1:s"';
  sErrorReadingEntities = 'Error reading entities in text. %s';
  sUnexpectedEOF = 'End of file found before all tags closed';


{ TTaggedTextEntityHandler }

function TTaggedTextEntityHandler.AddEntity(const Entity: string;
  const Ch: Char): Boolean;
  {Adds a symbolic entity and its corresponding character to the list of
  symbolic entities. Set up the list of entities before attempting to translate
  an entity.
    @param Entity [in] Character entity.
    @param Ch [in] Character corresponding to entity.
    @return True if entity added successfully, False if not.
  }
begin
  Result := fSymbolicEntities.IndexOf(Entity) = -1;
  if Result then
    fSymbolicEntities.AddObject(LowerCase(Entity), Pointer(Ch))
  else
    fLastErrorMessage := Format(sEntityAlreadyReg, [Entity]);
end;

constructor TTaggedTextEntityHandler.Create;
  {Class constructor. Creates empty entity list.
  }
begin
  inherited;
  fSymbolicEntities := TStringList.Create;
  fSymbolicEntities.CaseSensitive := True;
end;

destructor TTaggedTextEntityHandler.Destroy;
  {Class destructor. Frees owned entity list.
  }
begin
  fSymbolicEntities.Free;
  inherited;
end;

function TTaggedTextEntityHandler.TranslateEntity(
  const Entity: string; out Ch: Char): Boolean;
  {Translates entity into the character it represents.
    @param Entity [in] Entity to be translated (without leading '&' and trailing
      ';' characters).
    @param Ch [out] Character corresponding to Entity. and returns it via Ch.
    @return True if entity is translated or False if not.
  }
var
  AsciiVal: Integer;    // ascii value of a numeric entity
  SymbolIdx: Integer;   // index of symbolic entities in symbol list
begin
  // Assume failure
  Result := False;
  // Error if entity is empty string
  if Entity = '' then
  begin
    fLastErrorMessage := sEntityEmpty;
    Exit;
  end;
  // Check entity type
  if Entity[1] = '#' then
  begin
    // We have numeric entity: try to extract ascii value
    if Entity = '#' then
    begin
      // entity has no associated value
      fLastErrorMessage := sEntityHasNoValue;
      Exit;
    end;
    Assert(Length(Entity) >= 2,                            // ** do not localise
      ClassName + '.TranslateEntity: entity too short');
    // parse out the digits: only 0..9 accepted
    // we reject -ve numbers: use default of -1 so all conversion errors give
    // -ve number to indicate error
    AsciiVal := StrToIntDef(MidStr(Entity, 2, MaxInt), -1);
    if AsciiVal < 0 then
    begin
      fLastErrorMessage := Format(sEntityValueNotValid, [Entity]);
      Exit;
    end;
    // check if value is in range (already know >=0)
    if AsciiVal > 255 then
    begin
      fLastErrorMessage := Format(sEntityOutOfRange, [Entity]);
      Exit;
    end;
    // we have valid value: record it and return true
    Ch := Chr(AsciiVal);
    Result := True;
  end
  else
  begin
    // Symbolic entity
    // check if entity in list of supported entities
    SymbolIdx := fSymbolicEntities.IndexOf(Entity);
    if SymbolIdx = -1 then
    begin
      fLastErrorMessage := Format(sEntityNotRecognised, [Entity]);
      Exit;
    end;
    // entity is supported: record it's character value and return true
    { TODO -oSelf -cNote : Unicode Fix: Changed AnsiChar cast to Char }
    Ch := Char(fSymbolicEntities.Objects[SymbolIdx]);
    Result := True;
  end;
end;

function TTaggedTextEntityHandler.TranslateTextEntities(
  const Text: string; out TransStr: string): Boolean;
  {Finds and translates all entities in a string.
    @param Text [in] Text to be translated.
    @param TransStr [out] Translated text.
    @return True on success or False on error.
  }
var
  { TODO -oSelf -cNote : Unicode Fix: Changed Ch and EntityCh from AnsiChar to Char }
  Idx: Integer;         // index used to scan text
  InsPos: Integer;      // index of insertion point in translated string
  Ch: Char;             // current char in text: used to check for entities
  EntityStart: Integer; // records start of entity in text
  Entity: string;       // stores any found entity
  EntityCh: Char;       // stores character represented by entity
begin
  // Assume failure
  Result := False;
  // Set up cursors into input and ouput strings
  Idx := 1;
  InsPos := 1;
  // Length of TransStr will be <= length of text: allocate max possible size
  SetLength(TransStr, Length(Text));
  // Scan thru each character of text to be translated
  while Idx <= Length(Text) do
  begin
    // Record current character in input for processing
    Ch := Text[Idx];
    case Ch of
      '&':
      begin
        // We have start of entity
        // skip past opening '&' and record positiin as start of entity
        Inc(Idx);
        EntityStart := Idx;
        // scan through string looking for ';' that ends entity
        while (Idx <= Length(Text)) and (Text[Idx] <> ';') do
          Inc(Idx);
        if Idx > Length(Text) then
        begin
          // didn't find terminating ';': report errro
          fLastErrorMessage := sEntityUnterminated;
          Exit;
        end;
        // record entity excluding opening '&' and closing ';' in lower case
        Entity := LowerCase(MidStr(Text, EntityStart, Idx - EntityStart));
        // skip over ending ';' in input
        Inc(Idx);
        // try to translate entity: exit on error (LastErrorMessage set by
        // TranslateEntity)
        if not TranslateEntity(Entity, EntityCh) then
          Exit;
        // insert translated character in TransStr, and update its cursor
        TransStr[InsPos] := EntityCh;
        Inc(InsPos);
      end;
      else
      begin
        // We have ordinary character: copy into TransStr and move cursors on
        TransStr[InsPos] := Ch;
        Inc(InsPos);
        Inc(Idx);
      end;
    end;
  end;
  // If we have translated entities TransStr will be shorter than input string@
  // so we reduce TransStr length accodingly
  if Idx <> InsPos then
    SetLength(TransStr, InsPos - 1);
  Result := True;
end;

{ TTaggedTextTagHandler }

function TTaggedTextTagHandler.AddTag(const Tag: string;
  const Code: Word; const IsCompound: WordBool): Boolean;
  {Adds information about a tag to the list of tags recognised by the handler.
  A list of supported tags must be set up using this method before attempting to
  process them since the handler recognises no tags by default.
    @param Tag [in] Name of the tag without enclosing angle brackets.
    @param Code [in] Unique code associated with the tag - must not be $FFFF.
    @param IsCompound [in] True if the tag is compound (i.e. can contain other
      tags or text) or False if the tag stands on its own.
    @return True if tag is added successfully or False on error.
  }
var
  Data: LongWord; // stores both the tag code and the IsCompound flag
begin
  // Check that code is not reserved value $FFFF: error if so
  if Code = $FFFF then
  begin
    Result := False;
    Error(sTagCodeInvalid, [Tag]);
    Exit;
  end;
  // Check if tag already recorded: error if so
  Result := fTagList.IndexOf(Tag) = -1;
  if Result then
  begin
    // encode Code and a IsCompound into long word
    LongRec(Data).Lo := Code;
    LongRec(Data).Hi := Word(IsCompound);
    // add the tag and the data to the list
    fTagList.AddObject(LowerCase(Tag), Pointer(Data));
  end
  else
    Error(sTagAlreadyRegistered, [Tag]);
end;

constructor TTaggedTextTagHandler.Create(const EH: TTaggedTextEntityHandler);
  {Class constructor. Sets up object.
    @param EH [in] Method to call to translate entities.
  }
begin
  Assert(Assigned(EH),                                     // ** do not localise
    ClassName + '.Create: EH is not assigned');
  inherited Create;
  fTagList := TStringList.Create;
  fTagList.Sorted := True;
  fEntityHandler := EH;
end;

destructor TTaggedTextTagHandler.Destroy;
  {Class destructor. Tears down object.
  }
begin
  fTagList.Free;
  inherited;
end;

procedure TTaggedTextTagHandler.Error(const Msg: string);
  {Records an error message in the LastErrorMessage property.
    @param Msg [in] Message to be recorded.
  }
begin
  fLastErrorMessage := Msg;
end;

procedure TTaggedTextTagHandler.Error(const Fmt: string;
  const Args: array of const);
  {Records an error message in the LastErrorMessage property.
    @param Fmt [in] Message template.
    @param Args [in] Arguments inserted in message template.
  }
begin
  Error(Format(Fmt, Args));
end;

function TTaggedTextTagHandler.GetKind(var WorkTag: string): TTaggedTextKind;
  {Gets kind of a tag.
    @param WorkTag [in] Tag to be processed. [out] Tag stripped of characters
      that indicate tag kind, such as '/', '!' and '?'.
    @return Tag kind (ttsNull if kind is not recognised.
  }
var
  Len: Integer; // length of tag
begin
  Len := Length(WorkTag);
  Assert(WorkTag[1] = '<',                                 // ** do not localise
    ClassName + '.GetKind: Tag must begin with "<"');
  Assert(WorkTag[Len] = '>',                               // ** do not localise
    ClassName + '.GetKind: Tag must end with ">"');
  if (WorkTag = '<>') or (WorkTag = '</>') then
  begin
    // we have <> or </> => empty tag: delete all tag
    Result := ttsNull;
    Error(sTagEmpty);
    WorkTag := '';
    Exit;
  end;
  Assert(Len >= 3,                                         // ** do not localise
    ClassName + '.GetKind: Tag too short');
  if (Len >= 4) and (WorkTag[Len-1] = '/') then
  begin
    // tag of form <tag> => simple: we delete the / char
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
    if AnsiPos('--', WorkTag) = 2 then
      Delete(WorkTag, 2, 2);
    if AnsiPos('--', WorkTag) = Length(WorkTag) - 2 then
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
  WorkTag := MidStr(WorkTag, 2, Length(WorkTag) - 2);
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
  Assert(Length(TagStr) >= 1,                              // ** do not localise
    ClassName + '.GetTagName: Tag name too short');
  // Start at the beginning of the tag string
  NextChPos := 1;
  // Skip any white space before tag
  while (NextChPos <= Length(TagStr))
    and (TagStr[NextChPos] in cWhiteSpace) do
    Inc(NextChPos);
  // Now at start of tag name: read it up to next space or end of tag str
  StartPos := NextChPos;
  while (NextChPos <= Length(TagStr))
    and not (TagStr[NextChPos] in cWhiteSpace) do
    Inc(NextChPos);
  // Copy the name from the string
  Result := MidStr(TagStr, StartPos, NextChPos - StartPos);
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
  }

  // ---------------------------------------------------------------------------
  function GetNextParam(out Name, Value: string): Boolean;
    {Reads the next parameter from the current character position in the tag.
      @param Name [out] Receives parameter name.
      @param Value [out] Receives parameter value.
      @return True if a parameter was read, False if there are no more
        parameters.
    }
  var
    StartPos: Integer;        // start position of name or value in tag string
    ValDelims: set of Char;   // characters used to delimit values (e.g. quotes)
    Len: Integer;             // length of whole tag
  begin
    // Set name & value to '' in case not found
    Name := '';
    Value := '';
    // Record length of whole tag
    Len := Length(TagStr);

    // Check to see if we have any params
    // skip white space
    while (NextChPos <= Len) and (TagStr[NextChPos] in cWhiteSpace) do
      Inc(NextChPos);
    // check if we've reached end of tag and get out if so: no params
    if NextChPos > Len then
    begin
      Result := False;
      Exit;
    end;

    // We have attribute: get name
    StartPos := NextChPos;
    while (NextChPos <= Len)
      and not (TagStr[NextChPos] in cWhiteSpace + ['=']) do
      Inc(NextChPos);
    Name := MidStr(TagStr, StartPos, NextChPos - StartPos);
    // skip any white space following name
    while (NextChPos <= Len) and (TagStr[NextChPos] in cWhiteSpace) do
      Inc(NextChPos);

    // Check for value
    // if current character is '=' we have a value (else no value)
    if TagStr[NextChPos] = '=' then
    begin
      // skip '=' symbol
      Inc(NextChPos);
      // skip white space between '=' and value
      while (NextChPos <= Len) and (TagStr[NextChPos] in cWhiteSpace) do
        Inc(NextChPos);
      // if NextChPos > Len the there is no value: do nothing
      if NextChPos <= Len then
      begin
        // check to see if we have quoted param or not
        if TagStr[NextChPos] in cQuotes then
        begin
          // value is quoted: record quote as delimter and skip it
          ValDelims := [TagStr[NextChPos]];
          Inc(NextChPos);
        end
        else
          // value is not quoted: single word expected: white space delimits
          ValDelims := cWhiteSpace;
        // now get the value: it is between current pos and a delimter
        StartPos := NextChPos;
        while (NextChPos <= Len) and not (TagStr[NextChPos] in ValDelims) do
          Inc(NextChPos);
        // get the value: allows for closing quotes being missing
        Value := MidStr(TagStr, StartPos, NextChPos - StartPos);
        // translate any entities in value: we ignore any errors here
        fEntityHandler.TranslateTextEntities(
          MidStr(TagStr, StartPos, NextChPos - StartPos),
          Value
        );
        // if value was quoted, skip over any quote
        if (cQuotes * ValDelims <> [])
          and (NextChPos <= Len)
          and (TagStr[NextChPos] in cQuotes) then
          Inc(NextChPos);
      end;
    end;
    // Record that we found at least a name
    Result := True;
  end;
  // ---------------------------------------------------------------------------

var
  Name, Value: string;  // name and value of parameter
begin
  Assert(Length(TagStr) >= 1,                              // ** do not localise
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

function TTaggedTextTagHandler.LookupTagInfo(const TagName: string;
  out TagCode: Word; out IsCompound: WordBool): Boolean;
  {Looks up the tag in the table of supported tags.
    @param TagName [in] Name of tag for which information is required.
    @param TagCode [out] Unique code representing tag.
    @param IsCompound [out] True if tag is compound, False if simple.
    @return True if tag was found, False if not.
  }
var
  TagIdx: Integer;  // index of tag in table
  Data: LongWord;   // stores tag code and flag noting if tag is compound
begin
  // Lookup tag in table and record if found
  TagIdx := fTagList.IndexOf(TagName);
  Result := TagIdx >= 0;
  if Result then
  begin
    // Found tag: extract Code and IsCompound from string list's Objects[]
    Data := LongWord(fTagList.Objects[TagIdx]);
    TagCode := LongRec(Data).Lo;
    IsCompound := WordBool(LongRec(Data).Hi);
  end;
end;

function TTaggedTextTagHandler.ProcessTag(const Tag: string;
  out Text: string; out Kind: TTaggedTextKind; out Code: Word;
  const Params: TStrings): Boolean;
  {Parses and extracts information from tag text.
    @param Tag [in] String containing tag information, delimited by '<' and '>'.
    @param Text [out] For start, end and simple tags this is name of tag, for
      comment and script tags this is the content of the tag.
    @param Kind [out] Kind of tag, or ttsNull if error.
    @param Code [out] Unique code associated with the tag ($FFFF for comments
      and scripts).
    @param Params [in] List of parameters to (attributes of) tag in name=value
      format. If nil then no parameter information is returned.
    @return True if the tag is processed successfully, False otherwise.
  }
var
  Len: Integer;         // length of the tag
  WorkingTag: string;   // string used to manipulate the given tag
  IsCompound: WordBool; // true if tag is compound, false otherwise
  ChPos: Integer;       // indicates position of character to process in tag
begin
  // Assume failure
  Result := False;
  // Clear any parameters list
  if Assigned(Params) then
    Params.Clear;
  // Store tag in working storage and record its length
  WorkingTag := Tag;
  Len := Length(WorkingTag);
  Assert(WorkingTag[1] = '<',                              // ** do not localise
    ClassName + '.ProcessTag: Tag must begin with "<"');
  Assert(WorkingTag[Len] = '>',                            // ** do not localise
    ClassName + '.ProcessTag: Tag must end with ">"');
  // Get kind of tag, stripping out all tag delimiting info leavig just tag name
  // and contents/parameters
  Kind := GetKind(WorkingTag);
  // Process tag content, depending on kind of tag
  case Kind of
    ttsNull:
      // Error condition: error message set in GetKind
      Exit;
    ttsSimpleTag:
    begin
      // Simple tag
      // get tag's name: must always call this method before others than extract
      // information from a tag since this method sets the character position
      // ChPos ready to extract information following tag name
      Text := GetTagName(WorkingTag, ChPos);
      // get information about the tag from table of supported tags
      if not LookupTagInfo(Text, Code, IsCompound) then
      begin
        // tag is not in lookup table
        Error(sTagNotRecognised, [Text]);
        Exit;
      end;
      if IsCompound then
      begin
        // tag recognised but is a compound tag => not valid as simple tag
        Error(sSimpleTagInvalid, [Text]);
        Exit;
      end;
      // get parameters for the tag
      GetTagParams(WorkingTag, ChPos, Params);
      Result := True;
    end;
    ttsCompoundStartTag:
    begin
      // Compound start tag
      // get tag's name
      Text := GetTagName(WorkingTag, ChPos);
      // get information about tag from lookup table
      if not LookupTagInfo(Text, Code, IsCompound) then
      begin
        // tag is not in lookup table
        Error(sTagNotRecognised, [Text]);
        Exit;
      end;
      if not IsCompound then
      begin
        // tag not compound type: not valid here
        Error(sCompoundTagInvalid, [Text]);
        Exit;
      end;
      // get parameters for the tag
      GetTagParams(WorkingTag, ChPos, Params);
      Result := True;
    end;
    ttsCompoundEndTag:
    begin
      // Compound end tag
      // get tag's name
      Text := GetTagName(WorkingTag, ChPos);
      // get information about tag from lookup table
      if not LookupTagInfo(Text, Code, IsCompound) then
      begin
        // tag is not in lookup table
        Error(sTagNotRecognised, [Text]);
        Exit;
      end;
      if not IsCompound then
      begin
        // tag not compound type: not valid here
        Error(sTagNotCompound, [Text]);
        Exit;
      end;
      // check if has params: not valid for end tags
      if GetTagParams(WorkingTag, ChPos, nil) > 0 then
      begin
        Error(sEndTagHasParams, [Text]);
        Exit;
      end;
      Result := True;
    end;
    ttsComment, ttsScript:
    begin
      // Comment or Script tag
      Code := $FFFF;      // not a tag in lookup table
      Text := WorkingTag; // text to return is all that is left of tag
      Result := True;
    end;
  end;
end;

{ TTaggedTextLexer }

constructor TTaggedTextLexer.Create(
  const TagInfoCallback: TTaggedTextTagInfoProc;
  const EntityInfoCallback: TTaggedTextEntityInfoProc);
  {Class contructor. Sets up object and gets information about supported tags
  and character entities.
    @param TagInfoCallback [in] Method to call to get information about
      supported tags.
    @param EntityInfoCallback [in] Method to call to get information about
      supported character entities.
  }
begin
  // Pre-conditions:
  Assert(Assigned(TagInfoCallback),                        // ** do not localise
    ClassName + '.Create: TagInfoCallback is nil');
  Assert(Assigned(EntityInfoCallback),                     // ** do not localise
    ClassName + '.Create: EntityInfoCallback is nil');
  inherited Create;
  // Create entity and tag handler objects used to parse tags and char entities
  fEntityHandler := TTaggedTextEntityHandler.Create;
  fTagHandler := TTaggedTextTagHandler.Create(fEntityHandler);
  // Create stack object to track nested compound tags
  fTagStack := TStringStack.Create;
  // Create object to store a tag's parameters
  fParams := TStringList.Create;
  // Initialise ready to read tagged text
  Reset;
  // Get list of supported tags and entities using callback functions
  GetTagInfo(TagInfoCallback);
  GetEntityInfo(EntityInfoCallback);
end;

destructor TTaggedTextLexer.Destroy;
  {Class destructor. Tears down object.
  }
begin
  fParams.Free;
  fEntityHandler.Free;
  fTagHandler.Free;
  fTagStack.Free;
  inherited;
end;

function TTaggedTextLexer.GetCommentText: string;
  {Getter for CommentText property.
    @return Text of comment.
    @except Raises exception if current tagged text item is not a comment.
  }
begin
  if fKind <> ttsComment then
    raise ETaggedTextLexer.Create(sItemNotComment);
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
  Idx: Integer;           // inrementing index number for each callback call
  Name: string;           // name of character entity
  Ch: Char;               // character associated with entity
begin
  Idx := 0;
  while Callback(Idx, Name, Ch) do
  begin
    fEntityHandler.AddEntity(Name, Ch);
    Inc(Idx);
  end;
end;

function TTaggedTextLexer.GetPlainText: string;
  {Getter for PlainText property.
    @return Current text of a plain text token.
    @except Raises exception if current tagged text item is not plain text.
  }
begin
  if fKind <> ttsText then
    raise ETaggedTextLexer.Create(sItemNotText);
  Result := fCurText;
end;

function TTaggedTextLexer.GetScriptText: string;
  {Getter for ScriptText property.
    @return Current script text.
    @except Raises exception if current tagged text item is not a script.
  }
begin
  if fKind <> ttsScript then
    raise ETaggedTextLexer.Create(sItemNotScript);
  Result := fCurText;
end;

function TTaggedTextLexer.GetTagCode: Integer;
  {Getter for TagCode property.
    @return Code of the current tag.
    @except Raises exception if Kind of current item is not a tag.
  }
begin
  if not (fKind in [ttsCompoundStartTag, ttsCompoundEndTag, ttsSimpleTag]) then
    raise ETaggedTextLexer.Create(sItemNotTagCode);
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
  IsContainer: WordBool;  // whether the tag can contain text and/or other tags
begin
  Idx := 0;
  while Callback(Idx, Tag, Code, IsContainer) do
  begin
    fTagHandler.AddTag(Tag, Code, IsContainer);
    Inc(Idx);
  end;
end;

function TTaggedTextLexer.GetTagName: string;
  {Getter for TagName property.
    @return Name of current tag as string.
    @except Raises exception if current tagged text item is not a tag.
  }
begin
  if not (fKind in [ttsCompoundStartTag, ttsCompoundEndTag, ttsSimpleTag]) then
    raise ETaggedTextLexer.Create(sItemNotTag);
  Result := fCurText;
end;

function TTaggedTextLexer.GetTagParams: TStrings;
  {Getter for TagParams property.
    @return Reference to string list storing parameters for current tag.
    @except Raises exception if current tagged text item is not a tag.
  }
begin
  if not (fKind in [ttsCompoundStartTag, ttsSimpleTag]) then
    raise ETaggedTextLexer.Create(sCantReadParams);
  Result := fParams;
end;

function TTaggedTextLexer.NextItem: TTaggedTextKind;
  {Fetches the next logical item from the tagged text. The TagCode, TagName,
  TagParams, CommentText, ScriptCode and PlainText properties are updated.
    @return Kind of next token or ttsEOF if at end of code.
  }
var
  StartCh: Char;          // starting character of a string within tagged text
  StartPos: Integer;      // starting position of a string within tagged text
  Tag: string;            // name of tag read by lexer
  ExpectedTag: string;    // name of a tag expected by lexer
begin
  // Clear any existing parameters from param list
  fParams.Clear;
  // Scan through tagged text recognising various elements
  if fNextCharPos <= Length(fTaggedText) then
  begin
    // Record character starting this scan and check to see what next item is
    StartCh := fTaggedText[fNextCharPos];
    if StartCh = '<' then
    begin
      // We have start of a tag: get hold it and process it
      // record start of tag
      StartPos := fNextCharPos;
      // skip thru text until tag closer found
      while (fNextCharPos <= Length(fTaggedText))
        and (fTaggedText[fNextCharPos] <> '>') do
        Inc(fNextCharPos);
      // check if we have found end of tag: error if not
      if fNextCharPos > Length(fTaggedText) then
        raise ETaggedTextLexer.CreateFmt(sNoMatchingEndTag, [StartPos]);
      Assert(fTaggedText[fNextCharPos] = '>',              // ** do not localise
        ClassName + 'NextItem: ">" expected');
      // skip over tag close
      Inc(fNextCharPos);
      // record tag
      Tag := MidStr(fTaggedText, StartPos, fNextCharPos - StartPos);
      // Process the tag raising exception on error
      if not fTagHandler.ProcessTag(
        Tag, fCurText, fKind, fTagCode, fParams
      ) then
        raise ETaggedTextLexer.CreateFmt(
          sErrorReadingTag, [StartPos, fTagHandler.LastErrorMessage]
        );
      // Now act on kind of tag read
      case fKind of
        ttsCompoundStartTag:
          // we have compound start tag: push onto stack of currently open tags
          fTagStack.Push(fCurText);
        ttsCompoundEndTag:
        begin
          // we have compound end tag: check validity
          if fTagStack.IsEmpty then
            // .. tag stack empty => no matching opening tag
            raise ETaggedTextLexer.CreateFmt(sNoMatchingStartTag, [fCurText]);
          // .. tag we expect closes the one at top of stack
          //    pop stack to close tag
          ExpectedTag := fTagStack.Pop;
          if AnsiCompareText(fCurText, ExpectedTag) <> 0 then
            // .. error if tag is not the expected one
            raise ETaggedTextLexer.CreateFmt(
              sStartAndEndTagMismatched, [fCurText, ExpectedTag]
          );
        end;
        ttsSimpleTag:
        begin
          // we have a simple tag
          {No special processing for these tags};
        end;
        ttsComment, ttsScript:
          // we have script or comment
          {No special processing for these tags};
      end;
    end
    else
    begin
      // We have plain text - process it
      fKind := ttsText;
      Assert(fNextCharPos <= Length(fTaggedText),          // ** do not localise
        ClassName + '.NextItem: Beyond end of tagged text');
      Assert(fTaggedText[fNextCharPos] <> '<',             // ** do not localise
        ClassName + '.NextItem: "<" character expected');
      // get extent of text before ext tag or end of tagged text
      StartPos := fNextCharPos;
      Inc(fNextCharPos);
      while (fNextCharPos <= Length(fTaggedText))
        and (fTaggedText[fNextCharPos] <> '<') do
        Inc(fNextCharPos);
      // check the plain text for entities, replacing them with values
      if not fEntityHandler.TranslateTextEntities(
        MidStr(fTaggedText, StartPos, fNextCharPos - StartPos),
        fCurText
      ) then
        raise ETaggedTextLexer.CreateFmt(
          sErrorReadingEntities, [fEntityHandler.LastErrorMessage]
        );
      // replace all CR LF pairs with LF
      fCurText := UnixLineBreaks(fCurText);
    end;
  end
  else
  begin
    // We're at end of tagged text it's an error if we still have unclosed tags
    if not fTagStack.IsEmpty then
      raise ETaggedTextLexer.Create(sUnexpectedEOF);
    fKind := ttsEOF;
  end;
  // Return the kind of item just analysed
  Result := fKind;
end;

procedure TTaggedTextLexer.Reset;
  {Resets the lexer ready to restart the analysis of the TaggedText code.
  }
begin
  fNextCharPos := 1;
  fTagStack.Clear;
  fKind := ttsNull;
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

