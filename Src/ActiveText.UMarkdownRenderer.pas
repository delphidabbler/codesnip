{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2025, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements class that renders active text in Markdown format.
}


unit ActiveText.UMarkdownRenderer;

interface

uses
  // Delphi
  SysUtils,
  Generics.Collections,
  // Project
  ActiveText.UMain,
  UIStringList;


type
  ///  <summary>Renders active text in Markdown format.</summary>
  TActiveTextMarkdown = class(TObject)
  strict private
    type

      ///  <summary>Kinds of inline Markdown formatting.</summary>
      TInlineElemKind = (
        iekPlain,          // no formatting e.g. text => text
        iekWeakEmphasis,   // weak emphasis (italic) e.g. text => *text*
        iekStrongEmphasis, // strong emphasis (bold) e.g. text => **text**
        iekLink,           // link e.g. text,url => [text](url)
        iekInlineCode      // inline code e.g. text => `text`
      );

      ///  <summary>Representation of an inline Markdown element.</summary>
      TInlineElem = record
      strict private
        var
          fFormatterKind: TInlineElemKind;
          fMarkdown: string;
          fAttrs: IActiveTextAttrs;
          fCanRenderElem: TPredicate<TInlineElemKind>;
      public
        constructor Create(const AFormatterKind: TInlineElemKind;
          const ACanRenderElem: TPredicate<TInlineElemKind>;
          const AAttrs: IActiveTextAttrs);
        property Kind: TInlineElemKind read fFormatterKind;
        property Markdown: string read fMarkdown write fMarkdown;
        property Attrs: IActiveTextAttrs read fAttrs;
        property CanRenderElem: TPredicate<TInlineElemKind> read fCanRenderElem;
      end;

      ///  <summary>Stack of inline Markdown elements.</summary>
      ///  <remarks>Used in rendering all the inline elements within a block.
      ///  </remarks>
      TInlineElemStack = class (TStack<TInlineElem>)
      strict private
      public
        procedure Push(const AFmtKind: TInlineElemKind;
          const ACanRenderElem: TPredicate<TInlineElemKind>;
          const AAttrs: IActiveTextAttrs); reintroduce;
        function IsEmpty: Boolean;
        function IsOpen(const AFmtKind: TInlineElemKind): Boolean;
        function NestingDepthOf(const AFmtKind: TInlineElemKind): Integer;
        procedure AppendMarkdown(const AMarkdown: string);
        constructor Create;
        destructor Destroy; override;
      end;

      ///  <summary>Kinds of Markdown containers.</summary>
      TContainerKind = (
        ckPlain,        // represents main document
        ckBulleted,     // represents an unordered list item
        ckNumbered      // represents an ordered list item
      );

      ///  <summary>Encapsulates the state of a list (ordered or unordered).
      ///  </summary>
      TListState = record
      public
        ListNumber: Cardinal;
        ListKind: TContainerKind;
        constructor Create(AListKind: TContainerKind);
      end;

      ///  <summary>A stack of currently open lists, with the current, most
      ///  nested at the top of the stack.</summary>
      ///  <remarks>Used to keep track of list nesting.</remarks>
      TListStack = class(TStack<TListState>)
      public
        constructor Create;
        destructor Destroy; override;
        procedure IncTopListNumber;
      end;

      ///  <summary>Base class for classes that represent a chunk of a Markdown
      ///  document. A Markdown document contains a sequence of chunks, each of
      ///  which is either a block level element or a container of other chunks
      ///  at a deeper level.</summary>
      TContentChunk = class abstract
      strict private
        var
          fDepth: UInt8;
          fClosed: Boolean;
      public
        constructor Create(const ADepth: UInt8);
        procedure Close;
        function IsClosed: Boolean;
        procedure Render(const ALines: IStringList); virtual; abstract;
        property Depth: UInt8 read fDepth;
      end;

      ///  <summary>Base class for container chunks that hold a sequence of
      ///  other chunks at a given depth within a Markdown document.</summary>
      TContainer = class abstract (TContentChunk)
      strict private
        fContent: TObjectList<TContentChunk>;
      public
        constructor Create(const ADepth: UInt8);
        destructor Destroy; override;
        function IsEmpty: Boolean;
        procedure Add(const AChunk: TContentChunk);
        function LastChunk: TContentChunk;
        function Content: TArray<TContentChunk>;
        function TrimEmptyBlocks: TArray<TContentChunk>;
        procedure Render(const ALines: IStringList); override; abstract;
      end;

      ///  <summary>Encapsulate the Markdown document. Contains a sequence of
      ///  other chunks within the top level of the document.</summary>
      TDocument = class sealed (TContainer)
      public
        procedure Render(const ALines: IStringList); override;
      end;

      ///  <summary>Encapsulates a generalised list item, that is a container
      ///  for chunks at a deeper level within the document.</summary>
      TListItem = class abstract (TContainer)
      strict private
        fNumber: UInt8;
      public
        constructor Create(const ADepth: UInt8; const ANumber: UInt8);
        procedure Render(const ALines: IStringList); override; abstract;
        property Number: UInt8 read fNumber;
      end;

      ///  <summary>Encapsulates a bullet list item that contains a sequence of
      ///  chunks that belong to the list item.</summary>
      TBulletListItem = class sealed (TListItem)
      public
        constructor Create(const ADepth: UInt8; const ANumber: UInt8);
        procedure Render(const ALines: IStringList); override;
      end;

      ///  <summary>Encapsulates a numbered list item that contains a sequence
      ///  of chunks that belong to the list item.</summary>
      TNumberListItem = class sealed (TListItem)
      public
        constructor Create(const ADepth: UInt8; const ANumber: UInt8);
        procedure Render(const ALines: IStringList); override;
      end;

      ///  <summary>Encapsulates a generalised Markdown block level item.
      ///  </summary>
      TBlock = class abstract (TContentChunk)
      strict private
        var
          fMarkdownStack: TInlineElemStack;
      public
        constructor Create(const ADepth: UInt8);
        destructor Destroy; override;
        property MarkdownStack: TInlineElemStack read fMarkdownStack;
        function IsEmpty: Boolean;
        procedure Render(const ALines: IStringList); override; abstract;
        function RenderStr: string; virtual; abstract;
        function LookupElemKind(
          const AActiveTextKind: TActiveTextActionElemKind): TInlineElemKind;
      end;

      ///  <summary>Encapsulates a &quot;fake&quot; Markdown block that is used
      ///  to contain any active text that exists outside a block level tag or
      ///  whose direct parent is a list item.</summary>
      TSimpleBlock = class sealed (TBlock)
      public
        procedure Render(const ALines: IStringList); overload; override;
        function RenderStr: string; override;
      end;

      ///  <summary>Encapsulates a Markdown paragraph.</summary>
      TParaBlock = class sealed (TBlock)
      public
        procedure Render(const ALines: IStringList); overload; override;
        function RenderStr: string; override;
      end;

      ///  <summary>Encapsulates a markdown heading (assumed to be at level 2).
      ///  </summary>
      THeadingBlock = class sealed (TBlock)
      public
        procedure Render(const ALines: IStringList); overload; override;
        function RenderStr: string; override;
      end;

      ///  <summary>A stack of currently open containers.</summary>
      ///  <remarks>Used to track the parentage of the currently open container.
      ///  </remarks>
      TContainerStack = class(TStack<TContainer>);

  strict private
    var
      ///  <summary>Contains all the content chunks belonging to the top level
      ///  Markdown document.</summary>
      fDocument: TDocument;
      ///  <summary>Stack that tracks the parentage of any currently open list.
      ///  </summary>
      fListStack: TListStack;
      ///  <summary>Stack that tracks the parentage of the currently open
      ///  container.</summary>
      fContainerStack: TContainerStack;
    ///  <summary>Closes and renders the Markdown for the currently open inline
    ///  element in the given Markdown block.</summary>
    procedure CloseInlineElem(const Block: TBlock);
    procedure ParseTextElem(Elem: IActiveTextTextElem);
    procedure ParseBlockActionElem(Elem: IActiveTextActionElem);
    procedure ParseInlineActionElem(Elem: IActiveTextActionElem);
    procedure Parse(ActiveText: IActiveText);
  public
    constructor Create;
    destructor Destroy; override;
    ///  <summary>Parses the given active text and returns a Markdown
    ///  representation of it.</summary>
    function Render(ActiveText: IActiveText): string;
  end;


implementation

uses
  // Project
  UConsts,
  UExceptions,
  UMarkdownUtils,
  UStrUtils;


{ TActiveTextMarkdown }

procedure TActiveTextMarkdown.CloseInlineElem(const Block: TBlock);
var
  MElem: TInlineElem;
  Markdown: string;
begin
  MElem := Block.MarkdownStack.Peek;
  // Render markdown
  Markdown := '';
  if MElem.CanRenderElem(MElem.Kind) then
  begin
    // Element should be output, wrapping its markdown
    case MElem.Kind of
      iekWeakEmphasis:
        if not StrIsEmpty(MElem.Markdown) then
          Markdown := TMarkdown.WeakEmphasis(MElem.Markdown);
      iekStrongEmphasis:
        if not StrIsEmpty(MElem.Markdown) then
          Markdown := TMarkdown.StrongEmphasis(MElem.Markdown);
      iekLink:
        if StrIsEmpty(MElem.Attrs[TActiveTextAttrNames.Link_URL]) then
        begin
          Markdown := MElem.Markdown; // no URL: emit bare markdown
        end
        else
        begin
          // we have URL
          if not StrIsEmpty(MElem.Markdown) then
            // we have inner markdown: emit standard link
            Markdown := TMarkdown.Link(
              MElem.Markdown, MElem.Attrs[TActiveTextAttrNames.Link_URL]
            )
          else
            // no inner text: emit bare URL
            Markdown := TMarkdown.BareURL(
              MElem.Attrs[TActiveTextAttrNames.Link_URL]
            );
        end;
      iekInlineCode:
        if not StrIsEmpty(MElem.Markdown) then
        begin
          // Note: <mono>`foo`</mono> should be rendered as `` `foo` ``, not
          // ```foo```, but for any other leading or trailing character than `
          // don't prefix with space.
          // Also don't add space for other leading / trailing chars, so
          // <mono>[foo]</mono> is rendered as `[foo]` and <mono>[`foo`]</mono>
          // is rendered as ``[`foo`]``
          Markdown := MElem.Markdown;
          if Markdown[1] = '`' then
            Markdown := ' ' + Markdown;
          if Markdown[Length(Markdown)] = '`' then
            Markdown := Markdown + ' ';
          Markdown := TMarkdown.InlineCode(Markdown);
        end;
    end;
  end
  else
    // Ingoring element: keep its inner markdown
    Markdown := MElem.Markdown;
  // Pop stack & add markdown to that of new stack top
  Block.MarkdownStack.Pop;
  // stack should contain at least a block element below all inline elements
  Assert(not Block.MarkdownStack.IsEmpty);
  Block.MarkdownStack.AppendMarkdown(Markdown);
end;

constructor TActiveTextMarkdown.Create;
begin
  fDocument := TDocument.Create(0);
  fContainerStack := TContainerStack.Create;
  fListStack := TListStack.Create;
end;

destructor TActiveTextMarkdown.Destroy;
begin
  fListStack.Free;
  fContainerStack.Free;
  fDocument.Free;
  inherited;
end;

procedure TActiveTextMarkdown.Parse(ActiveText: IActiveText);
var
  Elem: IActiveTextElem;
  TextElem: IActiveTextTextElem;
  ActionElem: IActiveTextActionElem;
begin
  fContainerStack.Clear;
  fContainerStack.Push(fDocument);

  if ActiveText.IsEmpty then
    Exit;

  Assert(
    Supports(ActiveText[0], IActiveTextActionElem, ActionElem)
      and (ActionElem.Kind = ekDocument),
    ClassName + '.Parse: Expected ekDocument at start of active text'
  );

  for Elem in ActiveText do
  begin
    if Supports(Elem, IActiveTextTextElem, TextElem) then
      ParseTextElem(TextElem)
    else if Supports(Elem, IActiveTextActionElem, ActionElem) then
    begin
      if TActiveTextElemCaps.DisplayStyleOf(ActionElem.Kind) = dsBlock then
        ParseBlockActionElem(ActionElem)
      else
        ParseInlineActionElem(ActionElem);
    end;
  end;

end;

procedure TActiveTextMarkdown.ParseBlockActionElem(Elem: IActiveTextActionElem);
var
  CurContainer, NewContainer: TContainer;
begin

  CurContainer := fContainerStack.Peek;

  case Elem.State of

    fsOpen:
    begin
      case Elem.Kind of
        ekDocument:
          ; // do nothing
        ekUnorderedList:
          fListStack.Push(TListState.Create(ckBulleted));
        ekOrderedList:
          fListStack.Push(TListState.Create(ckNumbered));
        ekListItem:
        begin
          fListStack.IncTopListNumber;
          case fListStack.Peek.ListKind of
            ckBulleted:
              NewContainer := TBulletListItem.Create(
                fContainerStack.Peek.Depth + 1, fListStack.Peek.ListNumber
              );
            ckNumbered:
              NewContainer := TNumberListItem.Create(
                fContainerStack.Peek.Depth + 1, fListStack.Peek.ListNumber
              );
            else
              raise EBug.Create(
                ClassName + '.ParseBlockActionElem: Unknown list item type'
              );
          end;
          CurContainer.Add(NewContainer);
          fContainerStack.Push(NewContainer);
        end;
        ekBlock:
          CurContainer.Add(TSimpleBlock.Create(CurContainer.Depth));
        ekPara:
          CurContainer.Add(TParaBlock.Create(CurContainer.Depth));
        ekHeading:
          CurContainer.Add(THeadingBlock.Create(CurContainer.Depth));
      end;
    end;

    fsClose:
    begin
      case Elem.Kind of
        ekDocument:
          ; // do nothing
        ekUnorderedList, ekOrderedList:
          fListStack.Pop;
        ekListItem:
        begin
          fContainerStack.Pop;
          CurContainer.Close;
        end;
        ekBlock, ekPara, ekHeading:
          CurContainer.LastChunk.Close;
      end;
    end;
  end;
end;

procedure TActiveTextMarkdown.ParseInlineActionElem(
  Elem: IActiveTextActionElem);
var
  CurContainer: TContainer;
  Block: TBlock;
begin
  // Find last open block: create one if necessary
  CurContainer := fContainerStack.Peek;
  if not CurContainer.IsEmpty and (CurContainer.LastChunk is TBlock)
    and not CurContainer.LastChunk.IsClosed then
    Block := CurContainer.LastChunk as TBlock
  else
  begin
    Block := TSimpleBlock.Create(CurContainer.Depth);
    CurContainer.Add(Block);
  end;

  case Elem.State of
    fsOpen:
    begin

      CurContainer := fContainerStack.Peek;
      if not CurContainer.IsEmpty and (CurContainer.LastChunk is TBlock)
        and not CurContainer.LastChunk.IsClosed then
        Block := CurContainer.LastChunk as TBlock
      else
      begin
        Block := TSimpleBlock.Create(CurContainer.Depth);
        CurContainer.Add(Block);
      end;

      case Elem.Kind of

        ekLink, ekStrong, ekWarning, ekEm, ekVar:
        begin
          Block.MarkdownStack.Push(
            Block.LookupElemKind(Elem.Kind),
            function (AKind: TInlineElemKind): Boolean
            begin
              Assert(AKind in [iekWeakEmphasis, iekStrongEmphasis, iekLink]);
              Result := (Block.MarkdownStack.NestingDepthOf(AKind) = 0)
                and not Block.MarkdownStack.IsOpen(iekInlineCode);
            end,
            Elem.Attrs
          );
        end;

        ekMono:
          Block.MarkdownStack.Push(
            Block.LookupElemKind(Elem.Kind),
            function (AKind: TInlineElemKind): Boolean
            begin
              Assert(AKind = iekInlineCode);
              Result := Block.MarkdownStack.NestingDepthOf(AKind) = 0;
            end,
            Elem.Attrs
          );
      end;
    end;

    fsClose:
    begin
      CurContainer := fContainerStack.Peek;
      Assert(not CurContainer.IsEmpty or not (CurContainer.LastChunk is TBlock));
      Block := CurContainer.LastChunk as TBlock;
      CloseInlineElem(Block);
    end;
  end;
end;

procedure TActiveTextMarkdown.ParseTextElem(Elem: IActiveTextTextElem);
var
  CurContainer: TContainer;
  Block: TBlock;
begin
  CurContainer := fContainerStack.Peek;
  if not CurContainer.IsEmpty and (CurContainer.LastChunk is TBlock)
    and not CurContainer.LastChunk.IsClosed then
    Block := CurContainer.LastChunk as TBlock
  else
  begin
    Block := TSimpleBlock.Create(CurContainer.Depth);
    CurContainer.Add(Block);
  end;
  if not Block.MarkdownStack.IsOpen(iekInlineCode) then
    Block.MarkdownStack.AppendMarkdown(TMarkdown.EscapeText(Elem.Text))
  else
    Block.MarkdownStack.AppendMarkdown(Elem.Text);
end;

function TActiveTextMarkdown.Render(ActiveText: IActiveText): string;
var
  Document: IStringList;
begin
  Parse(ActiveText);
  Assert(fContainerStack.Count = 1);

  Document := TIStringList.Create;
  fContainerStack.Peek.Render(Document);
  Result := Document.GetText(EOL, True);
  while StrContainsStr(EOL2 + EOL, Result) do
    Result := StrReplace(Result, EOL2 + EOL, EOL2);
  Result := StrTrim(Result) + EOL;
end;

{ TActiveTextMarkdown.TInlineElem }

constructor TActiveTextMarkdown.TInlineElem.Create(
  const AFormatterKind: TInlineElemKind;
  const ACanRenderElem: TPredicate<TInlineElemKind>;
  const AAttrs: IActiveTextAttrs);
begin
  // Assign fields from parameters
  fFormatterKind := AFormatterKind;
  fMarkdown := '';
  fAttrs := AAttrs;
  fCanRenderElem := ACanRenderElem;

  // Set defaults for nil fields
  if not Assigned(AAttrs) then
    fAttrs := TActiveTextFactory.CreateAttrs;

  if not Assigned(ACanRenderElem) then
    fCanRenderElem :=
      function (AFmtKind: TInlineElemKind): Boolean
      begin
        Result := True;
      end;
end;

{ TActiveTextMarkdown.TInlineElemStack }

procedure TActiveTextMarkdown.TInlineElemStack.AppendMarkdown(
  const AMarkdown: string);
var
  Elem: TInlineElem;
begin
  Elem := Pop;
  Elem.Markdown := Elem.Markdown + AMarkdown;
  inherited Push(Elem);
end;

constructor TActiveTextMarkdown.TInlineElemStack.Create;
begin
  inherited Create;
  // Push root element onto stack that receives all rendered markdown
  // This element can always be rendered, has no attributes and no special chars
  Push(iekPlain, nil, {nil, }nil);
end;

destructor TActiveTextMarkdown.TInlineElemStack.Destroy;
begin
  inherited;
end;

function TActiveTextMarkdown.TInlineElemStack.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TActiveTextMarkdown.TInlineElemStack.IsOpen(
  const AFmtKind: TInlineElemKind): Boolean;
var
  Elem: TInlineElem;
begin
  Result := False;
  for Elem in Self do
    if Elem.Kind = AFmtKind then
      Exit(True);
end;

function TActiveTextMarkdown.TInlineElemStack.NestingDepthOf(
  const AFmtKind: TInlineElemKind): Integer;
var
  Elem: TInlineElem;
begin
  Result := -1;
  for Elem in Self do
    if (Elem.Kind = AFmtKind) then
      Inc(Result);
end;

procedure TActiveTextMarkdown.TInlineElemStack.Push(
  const AFmtKind: TInlineElemKind;
  const ACanRenderElem: TPredicate<TInlineElemKind>;
  const AAttrs: IActiveTextAttrs);
begin
  inherited Push(
    TInlineElem.Create(AFmtKind, ACanRenderElem, AAttrs)
  );
end;

{ TActiveTextMarkdown.TListState }

constructor TActiveTextMarkdown.TListState.Create(AListKind: TContainerKind);
begin
  ListKind := AListKind;
  ListNumber := 0;
end;

{ TActiveTextMarkdown.TListStack }

constructor TActiveTextMarkdown.TListStack.Create;
begin
  inherited Create;
end;

destructor TActiveTextMarkdown.TListStack.Destroy;
begin
  inherited;
end;

procedure TActiveTextMarkdown.TListStack.IncTopListNumber;
var
  State: TListState;
begin
  State := Pop;
  Inc(State.ListNumber);
  Push(State);
end;

{ TActiveTextMarkdown.TContentChunk }

procedure TActiveTextMarkdown.TContentChunk.Close;
begin
  fClosed := True;
end;

constructor TActiveTextMarkdown.TContentChunk.Create(const ADepth: UInt8);
begin
  inherited Create;
  fDepth := ADepth;
  fClosed := False;
end;

function TActiveTextMarkdown.TContentChunk.IsClosed: Boolean;
begin
  Result := fClosed;
end;

{ TActiveTextMarkdown.TContainer }

procedure TActiveTextMarkdown.TContainer.Add(const AChunk: TContentChunk);
begin
  fContent.Add(AChunk);
end;

function TActiveTextMarkdown.TContainer.Content: TArray<TContentChunk>;
begin
  Result := fContent.ToArray;
end;

constructor TActiveTextMarkdown.TContainer.Create(const ADepth: UInt8);
begin
  inherited Create(ADepth);
  fContent := TObjectList<TContentChunk>.Create(True);
end;

destructor TActiveTextMarkdown.TContainer.Destroy;
begin
  fContent.Free;
  inherited;
end;

function TActiveTextMarkdown.TContainer.IsEmpty: Boolean;
begin
  Result := fContent.Count = 0;
end;

function TActiveTextMarkdown.TContainer.LastChunk: TContentChunk;
begin
  Result := fContent.Last;
end;

function TActiveTextMarkdown.TContainer.TrimEmptyBlocks: TArray<TContentChunk>;
var
  TrimmedBlocks: TList<TContentChunk>;
  Chunk: TContentChunk;
begin
  TrimmedBlocks := TList<TContentChunk>.Create;
  try
    for Chunk in fContent do
    begin
      if (Chunk is TBlock) then
      begin
        if not (Chunk as TBlock).IsEmpty then
          TrimmedBlocks.Add(Chunk);
      end
      else
        TrimmedBlocks.Add(Chunk);
    end;
    Result := TrimmedBlocks.ToArray;
  finally
    TrimmedBlocks.Free;
  end;
end;

{ TActiveTextMarkdown.TDocument }

procedure TActiveTextMarkdown.TDocument.Render(const ALines: IStringList);
var
  Chunk: TContentChunk;
begin
  for Chunk in Self.TrimEmptyBlocks do
  begin
    Chunk.Render(ALines);
  end;
end;

{ TActiveTextMarkdown.TListItem }

constructor TActiveTextMarkdown.TListItem.Create(const ADepth: UInt8; const ANumber: UInt8);
begin
  inherited Create(ADepth);
  fNumber := ANumber;
end;

{ TActiveTextMarkdown.TBulletListItem }

constructor TActiveTextMarkdown.TBulletListItem.Create(const ADepth: UInt8; const ANumber: UInt8);
begin
  inherited Create(ADepth, ANumber);
end;

procedure TActiveTextMarkdown.TBulletListItem.Render(const ALines: IStringList);
var
  Idx: Integer;
  StartIdx: Integer;
  Trimmed: TArray<TContentChunk>;
  ItemText: string;

  procedure AddBulletItem(const AMarkdown: string);
  begin
    ALines.Add(TMarkdown.BulletListItem(AMarkdown, Depth - 1));
  end;

begin
  Trimmed := TrimEmptyBlocks;
  StartIdx := 0;
  if Length(Trimmed) > 0 then
  begin
    if (Trimmed[0] is TBlock) then
    begin
      ItemText := (Trimmed[0] as TBlock).RenderStr;
      if StrStartsStr(EOL, ItemText) then
        ALines.Add('');
      AddBulletItem(StrTrimLeft(ItemText));
      Inc(StartIdx);
    end
    else
    begin
      AddBulletItem('');
    end;
    for Idx := StartIdx to Pred(Length(Trimmed)) do
      Trimmed[Idx].Render(ALines);
  end
  else
  begin
    AddBulletItem('');
  end;
end;

{ TActiveTextMarkdown.TNumberListItem }

constructor TActiveTextMarkdown.TNumberListItem.Create(const ADepth: UInt8; const ANumber: UInt8);
begin
  inherited Create(ADepth, ANumber);
end;

procedure TActiveTextMarkdown.TNumberListItem.Render(const ALines: IStringList);
var
  Idx: Integer;
  StartIdx: Integer;
  Trimmed: TArray<TContentChunk>;
  ItemText: string;

  procedure AddNumberItem(const AMarkdown: string);
  begin
    ALines.Add(TMarkdown.NumberListItem(AMarkdown, Number, Depth - 1));
  end;

begin
  Trimmed := TrimEmptyBlocks;
  StartIdx := 0;
  if Length(Trimmed) > 0 then
  begin
    if (Trimmed[0] is TBlock) then
    begin
      ItemText := (Trimmed[0] as TBlock).RenderStr;
      if StrStartsStr(EOL, ItemText) then
        ALines.Add('');
      AddNumberItem(StrTrimLeft(ItemText));
      Inc(StartIdx);
    end
    else
    begin
      AddNumberItem('');
    end;
    for Idx := StartIdx to Pred(Length(Trimmed)) do
      Trimmed[Idx].Render(ALines);
  end
  else
  begin
    AddNumberItem('');
  end;
end;

{ TActiveTextMarkdown.TBlock }

constructor TActiveTextMarkdown.TBlock.Create(const ADepth: UInt8);
begin
  inherited Create(ADepth);
  fMarkdownStack := TInlineElemStack.Create;
end;

destructor TActiveTextMarkdown.TBlock.Destroy;
begin
  fMarkdownStack.Free;
  inherited;
end;

function TActiveTextMarkdown.TBlock.IsEmpty: Boolean;
var
  MDElem: TInlineElem;
begin
  Result := True;
  if fMarkdownStack.IsEmpty then
    Exit;
  for MDElem in fMarkdownStack do
    if not StrIsEmpty(MDElem.Markdown, True) then
      Exit(False);
end;

function TActiveTextMarkdown.TBlock.LookupElemKind(
  const AActiveTextKind: TActiveTextActionElemKind): TInlineElemKind;
begin
  case AActiveTextKind of
    ekLink: Result := iekLink;
    ekStrong, ekWarning: Result := iekStrongEmphasis;
    ekEm, ekVar: Result := iekWeakEmphasis;
    ekMono: Result := iekInlineCode;
    else
      raise EBug.Create(
        ClassName + '.LookupElemKind: Invalid inline active text element kind'
      );
  end;
end;

{ TActiveTextMarkdown.TSimpleBlock }

procedure TActiveTextMarkdown.TSimpleBlock.Render(const ALines: IStringList);
begin
  Assert(not MarkdownStack.IsEmpty);
  ALines.Add(RenderStr);
  ALines.Add('');
end;

function TActiveTextMarkdown.TSimpleBlock.RenderStr: string;
begin
  Result := TMarkdown.Paragraph(
    StrTrimLeft(MarkdownStack.Peek.Markdown), Depth
  );
end;

{ TActiveTextMarkdown.TParaBlock }

procedure TActiveTextMarkdown.TParaBlock.Render(const ALines: IStringList);
begin
  Assert(not MarkdownStack.IsEmpty);
  ALines.Add(RenderStr);
end;

function TActiveTextMarkdown.TParaBlock.RenderStr: string;
begin
  Result := EOL + TMarkdown.Paragraph(
    StrTrimLeft(MarkdownStack.Peek.Markdown), Depth
  ) + EOL;
end;

{ TActiveTextMarkdown.THeadingBlock }

procedure TActiveTextMarkdown.THeadingBlock.Render(const ALines: IStringList);
begin
  Assert(not MarkdownStack.IsEmpty);
  ALines.Add(RenderStr);
end;

function TActiveTextMarkdown.THeadingBlock.RenderStr: string;
begin
  Result := EOL + TMarkdown.Heading(
    StrTrimLeft(MarkdownStack.Peek.Markdown), 2, Depth
  ) + EOL;
end;

end.

