{
 * UActiveText.pas
 *
 * Provides interfaces, a factory class and implementation of "active text".
 * Active text is text that can have actions performed on it. Actions may
 * include formatting and clickable links.
 *
 * The active text object provides a list of a mixture of text and compound
 * action elements. Text elements text to be displayed in the current context.
 * Action elements occur in matched pairs and specify an action to be performed:
 * the first action element switches on the action and the second switches it
 * off.
 *
 * Active text does not define how it is rendered. It is up to the user to
 * determine how to render the text by examining the elements. However some
 * elements are defined as block level and some as inline.
 *
 * The active text object can be created by interpreting a textual markup
 * language. The object is language agnostic. The user must provide a parser
 * that can interpret the language and create the required elements.
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
 * The Original Code is UActiveText.pas
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


unit UActiveText;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  UBaseObjects, UExceptions;


type

  {
  TActiveTextElemKind:
    Supported types of active text element.
  }
  TActiveTextElemKind = (
    ekText,         // a plain text element
    ekLink,         // a link element: has a URL (inline)
    ekStrong,       // text formatted as strong (inline)
    ekEm,           // text formatted as emphasised (inline)
    ekVar,          // text formatted as variable (inline)
    ekPara,         // delimits a paragraph (block level)
    ekWarning,      // text formatted as a warning (inline)
    ekHeading,      // delimits a heading (block level)
    ekMono          // text formatted as mono spaced (inline)
  );

type
  {
  TActiveTextElemState:
    Indicates whether a active text action element is opening or closing.
  }
  TActiveTextElemState = (
    fsClose,        // element is closing
    fsOpen          // element is opening
  );

  {
  TActiveTextDisplayStyle:
    Indicates how an active text action element is displayed - either inline or
    as a block.
  }
  TActiveTextDisplayStyle = (
    dsInline,   // element is inline
    dsBlock     // element is block
  );

  {
  IActiveTextElem:
    Interface supported by all active text elements.
  }
  IActiveTextElem = interface(IInterface)
    ['{F08A9853-EDB6-4B14-8E21-F3AB10FAF7D9}']
    function GetKind: TActiveTextElemKind;
      {Gets kind of element this represents.
        @return Element kind.
      }
    property Kind: TActiveTextElemKind read GetKind;
      {Kind of element}
  end;

  {
  IActiveTextTextElem:
    Interface supported by plain text active text elements (i.e. with Kind =
    ekText).
  }
  IActiveTextTextElem = interface(IActiveTextElem)
    ['{B20C56D2-4ACC-48C8-AB30-9979A1B148B3}']
    function GetText: string;
      {Gets plain text represented by element.
        @return Element's text.
      }
    property Text: string read GetText;
      {Text associated with element}
  end;

  {
  IActiveTextActionElem:
    Interface supported by active text action elements, i.e. those that specify
    actions to be performed on text.
  }
  IActiveTextActionElem = interface(IActiveTextElem)
    ['{2956A28F-AED2-437E-A405-9A62077BD881}']
    function GetState: TActiveTextElemState;
      {Checks whether element is opening or closing: i.e. switching on or off
      the element's operation.
        @return Required state.
      }
    property State: TActiveTextElemState read GetState;
      {Indicates whether element is opening or closing operation. Operation is
      determined by inherited Kind property}
    function GetParam: string;
      {Gets value of any parameter associated with the element.
        @return Required parameter value or '' if there is no parameter.
      }
    property Param: string read GetParam;
      {Parameter associated with element. '' if element has no parameter}
    function GetDisplayStyle: TActiveTextDisplayStyle;
      {Checks whether element is displayed inline or as a block.
        @return Required display style.
      }
    property DisplayStyle: TActiveTextDisplayStyle read GetDisplayStyle;
      {Indicates whether element displays as a block or inline}
  end;

  {
  IActiveText:
    Defines operations of active text objects.
  }
  IActiveText = interface(IInterface)
    ['{230228FB-355F-4EC9-9EA9-F8A6DE628972}']
    function GetEnumerator: TEnumerator<IActiveTextElem>;
      {Gets object that can enumerate object's elements.
        @return Required enumerator object.
      }
    function AddElem(const Elem: IActiveTextElem): Integer;
      {Adds an element to the object.
        @param Elem [in] Element to be added.
        @return Index of new element in elements list.
      }
    procedure Append(const ActiveText: IActiveText);
      {Appends elements from another active text object to this one.
        @param ActiveText [in] Contains elements to be appended.
      }
    function IsEmpty: Boolean;
      {Checks if the active text object contains any elements.
        @return True if there are no elements, False otherwise.
      }
    function GetElem(Idx: Integer): IActiveTextElem;
      {Gets an element from the Elems property.
        @param Idx [in] Index of required element in Elems[].
        @return Required element.
      }
    function GetCount: Integer;
      {Gets number of elements in Elems property.
        @return Number of elements.
      }
    property Elems[Idx: Integer]: IActiveTextElem read GetElem; default;
      {List of elements in active text}
    property Count: Integer read GetCount;
      {Number of elements in Elems[]}
  end;

  {
  IActiveTextParser:
    Interface supported by objects that can build an active text object by
    parsing mark-up.
  }
  IActiveTextParser = interface(IInterface)
    procedure Parse(const Markup: string; const ActiveText: IActiveText);
      {Parses markup and updates active text object with details.
        @param Markup [in] Markup containing definition of active text. Must be
          in format understood by parser.
        @param ActiveText [in] Active text object to be updated by parser.
      }
  end;

  {
  EActiveTextParserError:
    Class of exception raised when parsing active text markup.
  }
  EActiveTextParserError = class(EValidation);

  {
  TActiveTextFactory:
    Static factory class that can create instances of active text objects and
    active text elements.
  }
  TActiveTextFactory = class(TNoConstructObject)
  public
    class function CloneActiveText(const Src: IActiveText): IActiveText;
      {Creates a cloned copy of an active text object.
        @param Src [in] Active text object to be cloned.
        @return Cloned copy.
      }
    class function CreateActiveText: IActiveText; overload;
      {Creates a new empty active text object.
        @return Empty object.
      }
    class function CreateActiveText(const Markup: string;
      Parser: IActiveTextParser): IActiveText; overload;
      {Create an active text object with contents obtained from parsing some
      markup.
        @param Markup [in] Markup that defines active text. "Language" used must
          be suitable for processing by provided parser.
        @param Parser [in] Object used to parse the markup and update active
          text document.
        @return New active text object.
      }
    class function CreateTextElem(const AText: string): IActiveTextTextElem;
      {Creates a new active text text element.
        @param AText [in] Text property of new element.
        @return Required new element.
      }
    class function CreateActionElem(const Kind: TActiveTextElemKind;
      const Param: string;
      const State: TActiveTextElemState): IActiveTextActionElem; overload;
      {Creates a new active text action element with a parameter.
        @param Kind [in] Identifies kind of element.
        @param Param [in] Value of element's Param property.
        @param State [in] State of elememt: opening or closing.
        @return Required new element.
      }
    class function CreateActionElem(const Kind: TActiveTextElemKind;
      const State: TActiveTextElemState): IActiveTextActionElem; overload;
      {Creates a new, parameterless, active text action element.
        @param Kind [in] Identifies kind of element.
        @param State [in] State of element: opening or closing.
        @return Required new element.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  IntfCommon;


type

  {
  TActiveText:
    Active text object implementation. Supports assignment and cloning and
    provides an enumerator.
  }
  TActiveText = class(TInterfacedObject,
    IActiveText, IAssignable, IClonable
  )
  strict private
    fElems: TList<IActiveTextElem>; // List of active text child elements
  protected // do not make strict
    { IActiveText methods }
    function GetEnumerator: TEnumerator<IActiveTextElem>;
      {Gets object that can enumerate object's elements.
        @return Required enumerator object.
      }
    function AddElem(const Elem: IActiveTextElem): Integer;
      {Adds an element to the object.
        @param Elem [in] Element to be added.
        @return Index of new element in elements list.
      }
    procedure Append(const ActiveText: IActiveText);
      {Appends elements from another active text object to this one.
        @param ActiveText [in] Contains elements to be appended.
      }
    function IsEmpty: Boolean;
      {Checks if the active text object contains any elements.
        @return True if there are no elements, False otherwise.
      }
    function GetElem(Idx: Integer): IActiveTextElem;
      {Gets an element from the Elems property.
        @param Idx [in] Index of required element in Elems[].
        @return Required element.
      }
    function GetCount: Integer;
      {Gets number of elements in Elems property.
        @return Number of elements.
      }
    { IAssignable method }
    procedure Assign(const Src: IInterface);
      {Assigns properties of another object to this object.
        @param Src [in] Active text object whose properties are to be copied.
          May be nil: clears this object's properties.
        @except EBug raised if Src is not not nil or an active text object.
      }
    { IClonable method }
    function Clone: IInterface;
      {Create a new instance of this object that is an extact copy.
        @return New object's IInterface interface.
      }
  public
    constructor Create;
      {Object constructor. Sets up object.
      }
    destructor Destroy; override;
      {Object destructor. Tears down object.
      }
  end;

  {
  TActiveTextElem:
    Base class for active text elements.
  }
  TActiveTextElem = class(TInterfacedObject,
    IActiveTextElem
  )
  strict private
    fKind: TActiveTextElemKind; // Kind of element
  protected // do not make strict
    { IActiveTextElem method }
    function GetKind: TActiveTextElemKind;
      {Gets element kind.
        @return Required kind.
      }
  public
    constructor Create(const Kind: TActiveTextElemKind);
      {Object constructor. Sets up object of correct kind.
        @param Kind [in] Element kind.
      }
  end;

  {
  TActiveTextTextElem:
    Implements an active text text element.
  }
  TActiveTextTextElem = class(TActiveTextElem,
    IActiveTextTextElem
  )
  strict private
    fText: string;  // Text of element
  protected // do not make strict
    function GetText: string;
      {Gets element's text.
        @return Required text.
      }
  public
    constructor Create(const Text: string);
      {Object constructor. Records element's text and specifies correct kind.
        @param Text [in] Element's text.
      }
  end;

  {
  TActiveTextActionElem:
    Implements an active text action element.
  }
  TActiveTextActionElem = class(TActiveTextElem,
    IActiveTextActionElem
  )
  strict private
    fState: TActiveTextElemState; // State of element: opening or closing
    fParam: string;               // Any parameter associated with element
  protected // do not make strict
    function GetState: TActiveTextElemState;
      {Gets element state.
        @return Required state.
      }
    function GetParam: string;
      {Gets element's parameter as text.
        @return Required parameter. '' if no parameter.
      }
    function GetDisplayStyle: TActiveTextDisplayStyle;
      {Checks whether element is displayed inline or as a block.
        @return Required display style.
      }
  public
    constructor Create(const Kind: TActiveTextElemKind;
      const Param: string; const State: TActiveTextElemState);
      {Object constructor. Creates required action element.
        @param Kind [in] Element kind.
        @param Param [in] Any parameter associated with element. May be ''.
        @param State [in] Element state: opening or closing.
      }
  end;

{ TActiveTextFactory }

class function TActiveTextFactory.CloneActiveText(
  const Src: IActiveText): IActiveText;
  {Creates a cloned copy of an active text object.
    @param Src [in] Active text object to be cloned.
    @return Cloned copy.
  }
begin
  Result := CreateActiveText;
  (Result as IAssignable).Assign(Src);
end;

class function TActiveTextFactory.CreateActiveText: IActiveText;
  {Creates a new empty active text object.
    @return Empty object.
  }
begin
  Result := TActiveText.Create;
end;

class function TActiveTextFactory.CreateActionElem(
  const Kind: TActiveTextElemKind; const Param: string;
  const State: TActiveTextElemState): IActiveTextActionElem;
  {Creates a new active text action element with a parameter.
    @param Kind [in] Identifies kind of element.
    @param Param [in] Value of element's Param property.
    @param State [in] State of elememt: opening or closing.
    @return Required new element.
  }
begin
  Result := TActiveTextActionElem.Create(Kind, Param, State);
end;

class function TActiveTextFactory.CreateActionElem(
  const Kind: TActiveTextElemKind;
  const State: TActiveTextElemState): IActiveTextActionElem;
  {Creates a new, parameterless, active text action element.
    @param Kind [in] Identifies kind of element.
    @param State [in] State of element: opening or closing.
    @return Required new element.
  }
begin
  Result := CreateActionElem(Kind, '', State);
end;

class function TActiveTextFactory.CreateActiveText(const Markup: string;
  Parser: IActiveTextParser): IActiveText;
  {Create an active text object with contents obtained from parsing some
  markup.
    @param Markup [in] Markup that defines active text. "Language" used must be
      suitable for processing by provided parser.
    @param Parser [in] Object used to parse the markup and update active text
      document.
    @return New active text object.
  }
begin
  Result := CreateActiveText;
  Parser.Parse(Markup, Result);
end;

class function TActiveTextFactory.CreateTextElem(
  const AText: string): IActiveTextTextElem;
  {Creates a new active text text element.
    @param AText [in] Text property of new element.
    @return Required new element.
  }
begin
  Result := TActiveTextTextElem.Create(AText);
end;

{ TActiveText }

function TActiveText.AddElem(const Elem: IActiveTextElem): Integer;
  {Adds an element to the object.
    @param Elem [in] Element to be added.
    @return Index of new element in elements list.
  }
begin
  Result := fElems.Add(Elem);
end;

procedure TActiveText.Append(const ActiveText: IActiveText);
  {Appends elements from another active text object to this one.
    @param ActiveText [in] Contains elements to be appended.
  }
var
  Elem: IActiveTextElem;  // references each element in elems
begin
  for Elem in ActiveText do
    AddElem(Elem);
end;

procedure TActiveText.Assign(const Src: IInterface);
  {Assigns properties of another object to this object.
    @param Src [in] Active text object whose properties are to be copied. May be
      nil: clears this object's properties.
    @except EBug raised if Src is not not nil or an active text object.
  }
begin
  if Assigned(Src) and not Supports(Src, IActiveText) then
    raise EBug.Create(ClassName + '.Assign: Src must support IActiveText');
  fElems.Clear;
  if Assigned(Src) then
    Append(Src as IActiveText);
end;

function TActiveText.Clone: IInterface;
  {Create a new instance of this object that is an extact copy.
    @return New object's IInterface interface.
  }
begin
  Result := Create;
  (Result as IAssignable).Assign(Self);
end;

constructor TActiveText.Create;
  {Object constructor. Sets up object.
  }
begin
  inherited Create;
  fElems := TList<IActiveTextElem>.Create;
end;

destructor TActiveText.Destroy;
  {Object destructor. Tears down object.
  }
begin
  fElems.Free;
  inherited;
end;

function TActiveText.GetCount: Integer;
  {Gets number of elements in Elems property.
    @return Number of elements.
  }
begin
  Result := fElems.Count;
end;

function TActiveText.GetElem(Idx: Integer): IActiveTextElem;
  {Gets an element from the Elems property.
    @param Idx [in] Index of required element in Elems[].
    @return Required element.
  }
begin
  Result := fElems[Idx];
end;

function TActiveText.GetEnumerator: TEnumerator<IActiveTextElem>;
  {Gets object that can enumerate object's elements.
    @return Required enumerator object.
  }
begin
  Result := fElems.GetEnumerator;
end;

function TActiveText.IsEmpty: Boolean;
  {Checks if the active text object contains any elements.
    @return True if there are no elements, False otherwise.
  }
begin
  Result := fElems.Count = 0;
end;

{ TActiveTextElem }

constructor TActiveTextElem.Create(const Kind: TActiveTextElemKind);
  {Object constructor. Sets up object of correct kind.
    @param Kind [in] Element kind.
  }
begin
  inherited Create;
  fKind := Kind;
end;

function TActiveTextElem.GetKind: TActiveTextElemKind;
  {Gets element kind.
    @return Required kind.
  }
begin
  Result := fKind;
end;

{ TActiveTextTextElem }

constructor TActiveTextTextElem.Create(const Text: string);
  {Object constructor. Records element's text and specifies correct kind.
    @param Text [in] Element's text.
  }
begin
  inherited Create(ekText);
  fText := Text;
end;

function TActiveTextTextElem.GetText: string;
  {Gets element's text.
    @return Required text.
  }
begin
  Result := fText;
end;

{ TActiveTextActionElem }

constructor TActiveTextActionElem.Create(const Kind: TActiveTextElemKind;
  const Param: string; const State: TActiveTextElemState);
  {Object constructor. Creates required action element.
    @param Kind [in] Element kind.
    @param Param [in] Any parameter associated with element. May be ''.
    @param State [in] Element state: opening or closing.
  }
begin
  Assert(Kind <> ekText,
    ClassName + '.Create: Kind is not valid for a compound element.');
  inherited Create(Kind);
  fParam := Param;
  fState := State;
end;

function TActiveTextActionElem.GetDisplayStyle: TActiveTextDisplayStyle;
  {Checks whether element is displayed inline or as a block.
    @return Required display style.
  }
begin
  if GetKind in [ekPara, ekHeading] then
    Result := dsBlock
  else
    Result := dsInline;
end;

function TActiveTextActionElem.GetParam: string;
  {Gets element's parameter as text.
    @return Required parameter. '' if no parameter.
  }
begin
  Result := fParam;
end;

function TActiveTextActionElem.GetState: TActiveTextElemState;
  {Gets element state.
    @return Required state.
  }
begin
  Result := fState;
end;

end.

