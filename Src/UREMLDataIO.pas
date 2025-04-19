{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2025, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements classes that render and parse Routine Extra Markup Language (REML)
 * code. This markup is used to read and store active text objects as used by
 * some properties of a TSnippet object. Also includes helper classes.
}


unit UREMLDataIO;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  ActiveText.UMain, UBaseObjects, UTaggedTextLexer;


type

  {
  TREMLReader:
    Class that parses markup used in Extra element read from snippets data
    files. Markup is translated into active text. The Extra element may occur in
    main database files and v2 of the user database and export files.
  }
  TREMLReader = class(TInterfacedObject, IActiveTextParser)
  strict private
    type
      TBlockTagInfo = record
      strict private
        var
          fTag: TActiveTextActionElemKind;
          fHasContent: Boolean;
      public
        constructor Create(ATag: TActiveTextActionElemKind);
        property Tag: TActiveTextActionElemKind read fTag;
        property HasContent: Boolean read fHasContent write fHasContent;
      end;
    var
      fLexer: TTaggedTextLexer;     // Analyses REML markup
      // Stack of tag params for use in closing tags
      fParamStack: TStack<TActiveTextAttr>;
      // Stack of block level tags with record of whether block has yet got any
      // content.
      fBlockTagStack: TStack<TBlockTagInfo>;
      // Stack of all compound tags
      fTagStack: TStack<TActiveTextActionElemKind>;
      // Flag indicating whether parsing an implied block
      fIsImpliedBlock: Boolean;
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
    ///  <summary>Note that block tag at top of stack has had content written to
    ///  it.</summary>
    procedure NoteBlockTagHasContent;
    procedure StartElem(const AActiveText: IActiveText;
      const AElem: TActiveTextActionElemKind);
    procedure EndElem(const AActiveText: IActiveText;
      const AElem: TActiveTextActionElemKind);
    procedure StartInlineElem(const AActiveText: IActiveText;
      const AElem: TActiveTextActionElemKind);
    procedure EndInlineElem(const AActiveText: IActiveText;
      const AElem: TActiveTextActionElemKind);
    procedure StartBlockElem(const AActiveText: IActiveText;
      const AElem: TActiveTextActionElemKind);
    procedure EndBlockElem(const AActiveText: IActiveText;
      const AElem: TActiveTextActionElemKind);
    procedure StartImpliedBlockElem(const AActiveText: IActiveText);
    procedure EndImpliedBlockElem(const AActiveText: IActiveText);
    procedure WriteText(const AActiveText: IActiveText; const AText: string);
    procedure ParsePlainText(const AActiveText: IActiveText);
    procedure ParseStartTag(const AActiveText: IActiveText);
    procedure ParseEndTag(const AActiveText: IActiveText);
  public
    constructor Create;
      {Class constructor. Sets up object.
      }
    destructor Destroy; override;
      {Class destructor. Finalises object.
      }
    { IActiveTextParser method }
      {Parses markup and updates active text object with details.
        @param Markup [in] Markup containing definition of active text. Must be
          in format understood by parser.
        @param ActiveText [in] Active text object updated by parser.
      }
    procedure Parse(const Markup: string; const ActiveText: IActiveText);
  end;

  {
  TREMLWriter:
    Class that creates a REML markup representation of an active text object.
  }
  TREMLWriter = class(TNoPublicConstructObject)
  strict private
    const
      IndentMult = 2; // Number of spaces to indent for each block elem level
    var
      fLevel: Integer;  // Indent level based on block nesting level
      fIsStartOfTextLine: Boolean;  // flag true if we're at start of a line
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


implementation


{
  About REML (Routine Extra Markup Language)
  -----------------------------------------

  The markup is simplified form of (X)HTML.

  It comprises plain text with limited inline and block level formatting and
  hyperlink specified by HTML like tags.

  Valid REML tags and character entities are documented in the file reml.html in
  the Docs/Design directory.
}


uses
  // Delphi
  SysUtils,
  // Project
  UConsts, UExceptions, UIStringList, UStrUtils;


type

  {
  TREMLTags:
    Class that provides information about REML tags.
  }
  TREMLTags = class(TNoConstructObject)
  strict private
    type
      {
      TREMLTag:
        Record that stores information a REML tag.
      }
      TREMLTag = record
      public
        Id: TActiveTextActionElemKind;  // active text element kind
        TagName: string;                // corresponding REML tag name
        ParamName: string;              // name of any REML parameter
        constructor Create(const AId: TActiveTextActionElemKind;
          const ATagName: string;
          const AParamName: string = '');
          {Record contructor. Initialises fields.
            @param AId [in] Active text element kind.
            @param ATagName [in] REML tag name.
            @param AParamName [in] Optional name of parameter.
          }
      end;
  strict private
    class var fTagMap: array of TREMLTag;
      {Details of all supported tags}
    class function IndexOfTagId(const Id: TActiveTextActionElemKind): Integer;
      {Finds index of a tag id in tag map.
        @param Id [in] Tag id to be found.
        @return Index of tag id or -1 if tag id not found.
      }
    class function GetCount: Integer; static;
      {Read accessor for Count property.
        @return Number of supported tags.
      }
    class function GetId(Idx: Integer): TActiveTextActionElemKind; static;
      {Read accessor for Ids[] property.
        @param Idx [in] Zero based index of required id.
        @return Required id.
      }
    class function GetName(Idx: Integer): string; static;
      {Read accessor for Names[] property,
        @param Idx [in] Zero based index of required tag name.
        @return Required tag name.
      }
  public
    class constructor Create;
      {Class constructor. Sets up map of REML tags.
      }
    class destructor Destroy;
      {Class destructor. Clears tag map.
      }
    class function LookupTagName(const Id: TActiveTextActionElemKind;
      out TagName: string): Boolean;
      {Looks up name of a tag.
        @param Id [in] Id of tag.
        @param TagName [out] Name of tag or '' if unknown id.
        @return True if tag id is valid, False if not.
      }
    class function LookupParamName(const Id: TActiveTextActionElemKind;
      out ParamName: string): Boolean;
      {Looks up a parameter name of an identified REML tag.
        @param Id [in] Id of required tag.
        @param ParamName [out] Set to name of parameter name. '' if tag has no
          parameter or if tag id is not valid.
        @return True if tag is valid, False if not.
      }
    class property Count: Integer read GetCount;
      {Number of supported tags}
    class property Ids[Idx: Integer]: TActiveTextActionElemKind read GetId;
      {List of tag ids}
    class property Names[Idx: Integer]: string read GetName;
      {List of tag names}
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
    class var fEntityMap: array of TREMLEntity; // Entity <=> character map

    ///  <summary>Attempts to map a character to an associated mnemonic
    ///  character entity, without the surrounding <c>&amp;</c> and <c>;</c>
    ///  characters.</summary>
    ///  <param name="Ch"><c>Char</c> [in] Character to be mapped.</param>
    ///  <returns><c>string</c>. The associated mnemonic entity or an empty
    ///  string if not such entity exists.</returns>
    class function CharToMnemonicEntity(const Ch: Char): string;

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

    ///  <summary>Attempts to map a character to a character enitity, without
    ///  the surrounding <c>&amp;</c> and <c>;</c> characters.</summary>
    ///  <param name="Ch"><c>Char</c> [in] Character to be mapped.</param>
    ///  <returns><c>string</c>. A mnemonic entity if one exists for <c>Ch</c>.
    ///  Otherwise if <c>Ch</c> is not a printable ASCII character a numeric
    ///  entity is returned. If <c>Ch</c> is a printable ASCII character an
    ///  empty string is returned.</returns>
    class function MapToEntity(const Ch: Char): string;

    class property Count: Integer read GetCount;
      {Number of supported tags}
    class property Entities[Idx: Integer]: string read GetEntity;
      {List of character entities}
    class property Chars[Idx: Integer]: Char read GetChar;
      {List of characters that match entities}
  end;

{ TREMLReader }

constructor TREMLReader.Create;
  {Class constructor. Initialises object.
  }
begin
  inherited Create;
  fLexer := TTaggedTextLexer.Create(TagInfo, EntityInfo);
  fParamStack := TStack<TActiveTextAttr>.Create;
  fTagStack := TStack<TActiveTextActionElemKind>.Create;
  fBlockTagStack := TStack<TBlockTagInfo>.Create;
end;

destructor TREMLReader.Destroy;
  {Class destructor. Finalises object.
  }
begin
  fBlockTagStack.Free;
  fTagStack.Free;
  FreeAndNil(fParamStack);
  FreeAndNil(fLexer);
  inherited;
end;

procedure TREMLReader.EndBlockElem(const AActiveText: IActiveText;
  const AElem: TActiveTextActionElemKind);
resourcestring
  // Error message
  sMismatchedTag = 'Closing block tag does not match opening block tag.';
begin
  Assert(fBlockTagStack.Count > 0);
  if fBlockTagStack.Peek.Tag <> AElem then
    raise EActiveTextParserError.Create(sMismatchedTag);
  EndElem(AActiveText, AElem);
  fBlockTagStack.Pop;
end;

procedure TREMLReader.EndElem(const AActiveText: IActiveText;
  const AElem: TActiveTextActionElemKind);
var
  ParamName: string;                  // name of a parameter
  Attr: TActiveTextAttr;              // attributes of tag
resourcestring
  // Error message
  sMismatchedTag = 'Closing tag does not match opening tag.';
begin
  Assert(fTagStack.Count > 0);
  if AElem <> fTagStack.Peek then
    raise EActiveTextParserError.Create(sMismatchedTag);
  // Retrive any parameters and record element with any parameter
  TREMLTags.LookupParamName(AElem, ParamName);
  if ParamName <> '' then
  begin
    // We should have a param which must be stored in closing action
    // element, but closing REML tags have no parameters. We solve this
    // by popping the parameter value from the stack. This works because
    // we use a stack for params and opening and closing tags are
    // matched.
    Assert(fParamStack.Count > 0);
    Attr := fParamStack.Pop;
    // Add closing action element
    AActiveText.AddElem(
      TActiveTextFactory.CreateActionElem(
        AElem, TActiveTextFactory.CreateAttrs(Attr), fsClose
      )
    );
  end
  else
  begin
    // No parameter: simple add closing parameterless action element
    AActiveText.AddElem(
      TActiveTextFactory.CreateActionElem(AElem, fsClose)
    );
  end;
  // Pop tag from tag stack
  fTagStack.Pop;
end;

procedure TREMLReader.EndImpliedBlockElem(const AActiveText: IActiveText);
begin
  Assert(fIsImpliedBlock);
  Assert(fBlockTagStack.Peek.Tag = ekBlock);
  fIsImpliedBlock := False;
  EndBlockElem(AActiveText, ekBlock);
end;

procedure TREMLReader.EndInlineElem(const AActiveText: IActiveText;
  const AElem: TActiveTextActionElemKind);
begin
  EndElem(AActiveText, AElem);
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
  Result := EntityIdx < TREMLEntities.Count;
  if not Result then
    Exit;
  EntityName := TREMLEntities.Entities[EntityIdx];
  EntityChar := TREMLEntities.Chars[EntityIdx];
end;

procedure TREMLReader.NoteBlockTagHasContent;
var
  Block: TBlockTagInfo;
begin
  Assert(fBlockTagStack.Count > 0);
  if fBlockTagStack.Peek.HasContent then
    Exit;
  Block := fBlockTagStack.Pop;
  Block.HasContent := True;
  fBlockTagStack.Push(Block);
end;

procedure TREMLReader.Parse(const Markup: string;
  const ActiveText: IActiveText);
resourcestring
  sMismatchedTags = 'There is not a closing tag for each opening tag';
  sBadTagType = 'Unexpected tag type';
begin
  Assert(Assigned(ActiveText), ClassName + '.Parse: ActiveText is nil');

  // TODO: consider changing this so document tags are written
  if Markup = '' then
    Exit;

  fBlockTagStack.Clear;
  fTagStack.Clear;
  fParamStack.Clear;
  fIsImpliedBlock := False;

  try
    // Use lexer to process markup
    fLexer.TaggedText := Markup;

    StartBlockElem(ActiveText, ekDocument);

    while fLexer.NextItem <> ttsEOF do
    begin
      case fLexer.Kind of
        ttsText:
          ParsePlainText(ActiveText);
        ttsCompoundStartTag:
          ParseStartTag(ActiveText);
        ttsCompoundEndTag:
          ParseEndTag(ActiveText);
        else
          raise EActiveTextParserError.Create(sBadTagType);
      end;
    end;

    if fIsImpliedBlock then
      EndImpliedBlockElem(ActiveText);
    EndBlockElem(ActiveText, ekDocument);
    if fBlockTagStack.Count <> 0 then
      raise EActiveTextParserError.Create(sMismatchedTags);

  except
    // Handle exceptions: convert expected exceptions to EActiveTextParserError
    on E: ETaggedTextLexer do
      raise EActiveTextParserError.Create(E);
    else
      raise;
  end;
end;

procedure TREMLReader.ParseEndTag(const AActiveText: IActiveText);
var
  TagId: TActiveTextActionElemKind;   // id of a tag
begin
  Assert(flexer.Kind = ttsCompoundEndTag);
  // Get tag id
  TagId := TActiveTextActionElemKind(fLexer.TagCode);

  if TActiveTextElemCaps.DisplayStyleOf(TagId) = dsBlock then
  begin
    // Closing block tag
    if fIsImpliedBlock then
      // An implied block is being written: close it
      EndImpliedBlockElem(AActiveText);
    // End read closing block
    EndBlockElem(AActiveText, TagId)
  end
  else // TActiveTextElemCaps.DisplayStyleOf(TagId) = dsInline
    // Closing inline tag: just close it
    EndInlineElem(AActiveText, TagId);
end;

procedure TREMLReader.ParsePlainText(const AActiveText: IActiveText);
var
  Text: string;
resourcestring
  sNoTextPermitted = 'Text is not permitted in enclosing block';
begin
  Assert(fLexer.Kind = ttsText);
  Text := fLexer.PlainText;
  if TActiveTextElemCaps.CanContainText(fBlockTagStack.Peek.Tag) then
    // Parent block accepts text: write it
    WriteText(AActiveText, Text)
  else if TActiveTextElemCaps.IsPermittedChildElem(
    fBlockTagStack.Peek.Tag, ekBlock
  ) then
  begin
    // Parent block can contain an ekBlock:
    // create block if text is not strictly empty string, and add text to it
    if not StrIsEmpty(Text) then
    begin
      StartImpliedBlockElem(AActiveText);
      WriteText(AActiveText, Text);
    end;
  end
  else if not StrIsEmpty(Text, True) then
    // Unless text is just white space, report an error. We allow white space
    // since there may be white space between tags. If there is white space we
    // do nothing - we don't want to write white space.
    raise EActiveTextParserError.Create(sNoTextPermitted);
end;

procedure TREMLReader.ParseStartTag(const AActiveText: IActiveText);
var
  TagId: TActiveTextActionElemKind;   // id of a tag
resourcestring
  // Error message
  sErrMissingParam = 'Expected a "%0:s" parameter value in tag "%1:s"';
  sErrNesting = 'Illegal nesting of "%0:s" tag';
  sBadParentBlock = 'Invalid parent block for tag %0:s';
  sNoTextPermitted = 'Text is not permitted in enclosing block';
  sMismatchedCloser = 'Mismatching closing block tag %0:s';
  sErrDocEndExpected = 'End of document expected';
begin
  Assert(fLexer.Kind = ttsCompoundStartTag);
  // Get tag id
  TagId := TActiveTextActionElemKind(fLexer.TagCode);
  if TActiveTextElemCaps.DisplayStyleOf(TagId) = dsBlock then
  begin
    // Opening block tag found
    // If writing an implied block, close the block before processing new block
    if fIsImpliedBlock then
      EndImpliedBlockElem(AActiveText);
    // Output block tag if it is valid within parent block, else error
    if TActiveTextElemCaps.IsPermittedChildElem(
      fBlockTagStack.Peek.Tag, TagId
    ) then
      StartBlockElem(AActiveText, TagId)
    else
      raise EActiveTextParserError.CreateFmt(sBadParentBlock, [fLexer.TagName]);
  end
  else // TActiveTextElemCaps.DisplayStyleOf(TagId) = dsInline
  begin
    // Opeing inline tag found
    if TActiveTextElemCaps.IsPermittedChildElem(
        fBlockTagStack.Peek.Tag, TagId
      ) then
      // Tag is permitted within parent block: output it
      StartInlineElem(AActiveText, TagId)
    else if
      TActiveTextElemCaps.IsPermittedChildElem(
        fBlockTagStack.Peek.Tag, ekBlock
      ) and
      TActiveTextElemCaps.IsPermittedChildElem(ekBlock, TagId) then
      begin
        // Tag not directly permitted, but we can create an implied block iff:
        // 1. parent block permits an ekBlock child element
        // 2. ekBlock permits current tag as child element
        StartImpliedBlockElem(AActiveText);
        StartInlineElem(AActiveText, TagId);
      end
    else
      // Tag not permitted in parent block: error
      raise EActiveTextParserError.CreateFmt(sBadParentBlock, [fLexer.TagName]);
  end;
end;

procedure TREMLReader.StartBlockElem(const AActiveText: IActiveText;
  const AElem: TActiveTextActionElemKind);
begin
  Assert((TActiveTextElemCaps.DisplayStyleOf(AElem) = dsBlock));
  StartElem(AActiveText, AElem);
  fBlockTagStack.Push(TBlockTagInfo.Create(AElem));
end;

procedure TREMLReader.StartElem(const AActiveText: IActiveText;
  const AElem: TActiveTextActionElemKind);
var
  ParamName: string;                  // name of a parameter
  ParamValue: string;                 // value of a parameter
  Attr: TActiveTextAttr;              // attributes of tag
resourcestring
  // Error message
  sErrMissingParam = 'Expected a "%0:s" parameter value in tag "%1:s"';
begin
  // Find any parameters and record element with any parameter
  TREMLTags.LookupParamName(AElem, ParamName);
  if ParamName <> '' then
  begin
    // We have a parameter: must not be empty
    ParamValue := fLexer.TagParams.Values[ParamName];
    if ParamValue = '' then
      raise EActiveTextParserError.CreateFmt(
        sErrMissingParam, [ParamName, fLexer.TagName]
      );
    // Record param for use by closing tag
    Attr := TActiveTextAttr.Create(ParamName, ParamValue);
    fParamStack.Push(Attr);
    // Add opening action element
    AActiveText.AddElem(
      TActiveTextFactory.CreateActionElem(
        AElem, TActiveTextFactory.CreateAttrs(Attr), fsOpen
      )
    );
  end
  else
  begin
    // No parameter: simply add opening parameterless opening action element
    AActiveText.AddElem(
      TActiveTextFactory.CreateActionElem(AElem, fsOpen)
    );
  end;
  // Push tag onto tag stack
  fTagStack.Push(AElem);
end;

procedure TREMLReader.StartImpliedBlockElem(const AActiveText: IActiveText);
begin
  Assert(not fIsImpliedBlock);
  StartBlockElem(AActiveText, ekBlock);
  fIsImpliedBlock := True;
end;

procedure TREMLReader.StartInlineElem(const AActiveText: IActiveText;
  const AElem: TActiveTextActionElemKind);
begin
  Assert(TActiveTextElemCaps.DisplayStyleOf(AElem) = dsInline);
  StartElem(AActivetext, AElem);
  NoteBlockTagHasContent;
end;

function TREMLReader.TagInfo(const TagIdx: Integer; out TagName: string;
  out TagCode: Word; out IsContainer: Boolean): Boolean;
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
  Result := TagIdx < TREMLTags.Count;
  if Result then
  begin
    TagName := TREMLTags.Names[TagIdx];
    TagCode := Ord(TREMLTags.Ids[TagIdx]);
    IsContainer := True;
  end;
end;

procedure TREMLReader.WriteText(const AActiveText: IActiveText;
  const AText: string);
begin
  // Don't write anything if text is strictly empty string
  if not StrIsEmpty(AText) then
  begin
    AActiveText.AddElem(TActiveTextFactory.CreateTextElem(AText));
    NoteBlockTagHasContent;
  end;
end;

{ TREMLReader.TBlockTagInfo }

constructor TREMLReader.TBlockTagInfo.Create(ATag: TActiveTextActionElemKind);
begin
  fTag := ATag;
  fHasContent := False;
end;

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
  Text: string;
  SrcLines: IStringList;
  SrcLine: string;
  DestLines: IStringList;
  DestLine: string;
  RW: TREMLWriter;
begin
  if not ActiveText.HasContent then
    Exit('');
  RW := TREMLWriter.InternalCreate;
  try
    Text := '';
    RW.fLevel := 0;
    for Elem in ActiveText do
    begin
      if Supports(Elem, IActiveTextTextElem, TextElem) then
        Text := Text + RW.RenderText(TextElem)
      else if Supports(Elem, IActiveTextActionElem, TagElem) then
        Text := Text + RW.RenderTag(TagElem);
    end;
    SrcLines := TIStringList.Create(Text, EOL, False);
    DestLines := TIStringList.Create;
    for SrcLine in SrcLines do
    begin
      DestLine := StrTrimRight(SrcLine);
      if not StrIsEmpty(DestLine) then
        DestLines.Add(DestLine);
    end;
    Result := DestLines.GetText(EOL, False);
  finally
    RW.Free;
  end;
end;

function TREMLWriter.RenderTag(
  const TagElem: IActiveTextActionElem): string;
  {Renders an active text action element as a REML tag.
    @param TagElem [in] Active text action element to be rendered.
    @return Required REML tag.
  }
var
  TagName: string;      // name of tag
  ParamName: string;  // name of any parameter
begin
  TREMLTags.LookupTagName(TagElem.Kind, TagName);
  Result := '';
  TREMLTags.LookupParamName(TagElem.Kind, ParamName);
  case TagElem.State of
    fsClose:
    begin
      // closing tag
      if TagName <> '' then
      begin
        Result := Format('</%s>', [TagName]);
        if TActiveTextElemCaps.DisplayStyleOf(TagElem.Kind) = dsBlock then
        begin
          Dec(fLevel);
          Result := EOL + StrOfSpaces(IndentMult * fLevel) + Result + EOL;
          fIsStartOfTextLine := True;
        end;
      end
      else
      begin
        Result := '';
        fIsStartOfTextLine := True;
      end;
    end;
    fsOpen:
    begin
      // opening tag: may have a parameter
      if TagName <> '' then
      begin
        if ParamName ='' then
          Result := Format('<%s>', [TagName])
        else
          { TODO: revise to not assume parameter must be Link URL }
          // have a parameter: value must be safely encoded
          Result := Format(
            '<%0:s %1:s="%2:s">',
            [
              TagName,
              ParamName,
              TextToREMLText(TagElem.Attrs[TActiveTextAttrNames.Link_URL])
            ]
          );
        if TActiveTextElemCaps.DisplayStyleOf(TagElem.Kind) = dsBlock then
        begin
          Result := EOL + StrOfSpaces(IndentMult * fLevel) + Result + EOL;
          Inc(fLevel);
          fIsStartOfTextLine := True;
        end
        else if TActiveTextElemCaps.DisplayStyleOf(TagElem.Kind) = dsInline then
        begin
          if fIsStartOfTextLine then
          begin
            Result := StrOfSpaces(IndentMult * fLevel) + Result;
            fIsStartOfTextLine := False;
          end;
        end;
      end;
    end
    else
    begin
      if TagElem.Kind = ekBlock then
        fIsStartOfTextLine := True;
      // ekDocument is a no-op and there should be no other elems here
      Result := '';
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
  if fIsStartOfTextLine then
  begin
    Result := StrOfSpaces(IndentMult * fLevel);
    fIsStartOfTextLine := False;
  end
  else
    Result := '';
  Result := Result + TextToREMLText(TextElem.Text);
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
  SetLength(fTagMap, 11);
  // REML v1
  fTagMap[0] := TREMLTag.Create(ekLink, 'a', 'href'); // <a> has href param
  fTagMap[1] := TREMLTag.Create(ekStrong, 'strong');
  // REML v2
  fTagMap[2] := TREMLTag.Create(ekEm, 'em');
  fTagMap[3] := TREMLTag.Create(ekVar, 'var');
  fTagMap[4] := TREMLTag.Create(ekPara, 'p');
  fTagMap[5] := TREMLTag.Create(ekWarning, 'warning');
  fTagMap[6] := TREMLTag.Create(ekHeading, 'heading');
  fTagMap[7] := TREMLTag.Create(ekMono, 'mono');
  // REML v5
  fTagMap[8] := TREMLTag.Create(ekUnorderedList, 'ul');
  fTagMap[9] := TREMLTag.Create(ekOrderedList, 'ol');
  fTagMap[10] := TREMLTag.Create(ekListItem, 'li');
  // NOTE: ekBlock and ekDocument are not used REML
  //   content of ekBlock is rendered as text outside any block
  //   content of ekDocument is rendered without outputing a tag
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

class function TREMLTags.GetId(Idx: Integer): TActiveTextActionElemKind;
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

class function TREMLTags.IndexOfTagId(const Id: TActiveTextActionElemKind):
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

class function TREMLTags.LookupParamName(const Id: TActiveTextActionElemKind;
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

class function TREMLTags.LookupTagName(const Id: TActiveTextActionElemKind;
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

constructor TREMLTags.TREMLTag.Create(const AId: TActiveTextActionElemKind;
  const ATagName, AParamName: string);
  {Record contructor. Initialises fields.
    @param AId [in] Active text element kind.
    @param ATagName [in] REML tag name.
    @param AParamName [in] Optional name of parameter.
  }
begin
  Id := AId;
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
  {Class constructor. Creates map of mnemonic entities to equivalent characters.
  }
begin
  SetLength(fEntityMap, 35);
  // Supported character entities. All are optional unless otherwise stated
  // REML v1
  fEntityMap[0] := TREMLEntity.Create('amp', '&');   // required in REML
  fEntityMap[1] := TREMLEntity.Create('quot', DOUBLEQUOTE);
  fEntityMap[2] := TREMLEntity.Create('gt', '>');
  fEntityMap[3] := TREMLEntity.Create('lt', '<');   // required in REML
  // REML v2
  fEntityMap[4] := TREMLEntity.Create('copy', '©');
  // REML v5
  fEntityMap[5] := TREMLEntity.Create('times', '×');
  fEntityMap[6] := TREMLEntity.Create('divide', '÷');
  fEntityMap[7] := TREMLEntity.Create('div', '÷');
  fEntityMap[8] := TREMLEntity.Create('plusmn', '±');
  fEntityMap[9] := TREMLEntity.Create('ne', '≠');
  fEntityMap[10] := TREMLEntity.Create('neq', '≠');
  fEntityMap[11] := TREMLEntity.Create('sum', '∑');
  fEntityMap[12] := TREMLEntity.Create('infin', '∞');
  fEntityMap[13] := TREMLEntity.Create('pound', '£');
  fEntityMap[14] := TREMLEntity.Create('curren', '¤');
  fEntityMap[15] := TREMLEntity.Create('yen', '¥');
  fEntityMap[16] := TREMLEntity.Create('euro', '€');
  fEntityMap[17] := TREMLEntity.Create('dagger', '†');
  fEntityMap[18] := TREMLEntity.Create('ddagger', '‡');
  fEntityMap[19] := TREMLEntity.Create('Dagger', '‡');
  fEntityMap[20] := TREMLEntity.Create('hellip', '…');
  fEntityMap[21] := TREMLEntity.Create('para', '¶');
  fEntityMap[22] := TREMLEntity.Create('sect', '§');
  fEntityMap[23] := TREMLEntity.Create('reg', '®');
  fEntityMap[24] := TREMLEntity.Create('frac14', '¼');
  fEntityMap[25] := TREMLEntity.Create('frac12', '½');
  fEntityMap[26] := TREMLEntity.Create('half', '½');
  fEntityMap[27] := TREMLEntity.Create('frac34', '¾');
  fEntityMap[28] := TREMLEntity.Create('micro', 'µ');
  fEntityMap[29] := TREMLEntity.Create('deg', '°');
  fEntityMap[30] := TREMLEntity.Create('cent', '¢');
  fEntityMap[31] := TREMLEntity.Create('laquo', '«');
  fEntityMap[32] := TREMLEntity.Create('raquo', '»');
  fEntityMap[33] := TREMLEntity.Create('iquest', '¿');
  // REML v6
  fEntityMap[34] := TREMLEntity.Create('apos', SINGLEQUOTE);
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

end.

