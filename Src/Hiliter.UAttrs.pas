{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2022, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements classes that define syntax highlighter attributes along with an
 * object that provides a list of named highlighter attributes.
}


unit Hiliter.UAttrs;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  Hiliter.UGlobals, IntfCommon, UBaseObjects;


type
  ///  <summary>Factory class used to create various highlight attributes
  ///  objects.</summary>
  THiliteAttrsFactory = class(TNoConstructObject)
  public
    ///  <summary>Creates and returns a null highlighter attributes object
    ///  instance.</summary>
    ///  <remarks>Null attributes use no styling other than default font name
    ///  and size.</remarks>
    class function CreateNulAttrs: IHiliteAttrs;
    ///  <summary>Creates and returns a highlighter attributes object instance
    ///  that uses the program's default syntax highlighting style.</summary>
    class function CreateDefaultAttrs: IHiliteAttrs;
    ///  <summary>Creates and returns a highlighter attributes object instance
    ///  that uses a syntax highlighting style defined by the user.</summary>
    class function CreateUserAttrs: IHiliteAttrs;
    ///  <summary>Creates and returns a copy of the given highlighter attributes
    ///  instance in a form suitable for printing. If UseColour is False, all
    ///  colour information is removed from the highlighter. If Attrs is nil a
    ///  null highlighter is returned.</summary>
    class function CreatePrintAttrs(const Attrs: IHiliteAttrs;
      const UseColour: Boolean): IHiliteAttrs;
    ///  <summary>Creates and returns a highlighter attributes object instance
    ///  with the given predefined style.</summary>
    class function CreatePredefinedAttrs(const Style: TPredefinedHiliteStyle):
      IHiliteAttrs;
    ///  <summary>Creates a returns a highlighter attributes object instance
    ///  that is a clone of the given highlighter attributes.</summary>
    class function CloneAttrs(Attrs: IHiliteAttrs): IHiliteAttrs;
    ///  <summary>Creates and returns named highlighter attributes object
    ///  instance.</summary>
    class function CreateNamedAttrs: INamedHiliteAttrs;
  end;


implementation


uses
  // Delphi
  SysUtils, Graphics,
  // Project
  UComparers, UExceptions, UFontHelper, UPreferences;


type
  ///  <summary>Class that records display attributes for use when syntax
  ///  highlighting source code.</summary>
  ///  <remarks>The class records the font face and size used for the whole
  ///  highighted along with font style and colour information of each
  ///  recognised source code element.</remarks>
  THiliteAttrs = class(TInterfacedObject,
    IHiliteAttrs, IAssignable
  )
  strict private
    var
      ///  <summary>List of highlight attributes for each source code element.
      ///  </summary>
      fElemAttrs: TList<IHiliteElemAttrs>;
      ///  <summary>Size of font in points.</summary>
      fFontSize: Integer;
      ///  <summary>Name of font.</summary>
      fFontName: string;
    const
      ///  <summary>Default font name.</summary>
      cDefFontName = 'Courier New';
      ///  <summary>Default font size.</summary>
      cDefFontSize = 9;
  public
    ///  <summary>Creates a new instance of the class with default values.
    ///  </summary>
    constructor Create;
    ///  <summary>Destroys this instance.</summary>
    destructor Destroy; override;
    ///  <summary>Gets name of highlighter font.</summary>
    ///  <remarks>Method of IHiliteAttrs.</remarks>
    function GetFontName: string;
    ///  <summary>Sets name of highlighter font.</summary>
    ///  <remarks>Method of IHiliteAttrs.</remarks>
    procedure SetFontName(const AFontName: string);
    ///  <summary>Gets size of highlighter font.</summary>
    ///  <remarks>Method of IHiliteAttrs.</remarks>
    function GetFontSize: Integer;
    ///  <summary>Sets size of highlighter font.</summary>
    ///  <remarks>
    ///  <para>If font size is out of range of supported sizes then the
    ///  highlighter font is reset to its default value.</para>
    ///  <para>Method of IHiliteAttrs.</para>
    ///  </remarks>
    procedure SetFontSize(const AFontSize: Integer);
    ///  <summary>Resets highlighter font name and size to default values.
    ///  </summary>
    ///  <remarks>Method of IHiliteAttrs.</remarks>
    procedure ResetDefaultFont;
    ///  <summary>Returns the highlighter attributes for the given source code
    ///  element.</summary>
    ///  <remarks>Method of IHiliteAttrs.</remarks>
    function GetElement(const Elem: THiliteElement): IHiliteElemAttrs;
    ///  <summary>Assigns the properties of the given highlighter attributes
    ///  object to this instance.</summary>
    ///  <remarks>
    ///  <para>Src must either support IHiliteAttrs or be nil. If nil then this
    ///  instance is reset to its default values.</para>
    ///  <para>Method of IAssignable.</para>
    ///  </remarks>
    procedure Assign(const Src: IInterface);
  end;

type
  ///  <summary>Class that records display attributes for use in highlighting a
  ///  source code element.</summary>
  ///  <remarks>The class records the font style and colour for a single source
  ///  code element.</remarks>
  THiliteElemAttrs = class(TInterfacedObject,
    IHiliteElemAttrs, IAssignable
  )
  strict private
    var
      ///  <summary>Foreground (font) colour of the source code element.
      ///  </summary>
      fForeColor: TColor;
      ///  <summary>Font style of the source code element.</summary>
      fFontStyle: TFontStyles;
  public
    ///  <summary>Creates a new instance of the class with default values.
    ///  </summary>
    constructor Create;
    ///  <summary>Checks if the element's attributes are "null", i.e. all
    ///  properties default values.</summary>
    ///  <remarks>
    ///  <para>This method is used to determines whether to output formatting
    ///  information for an element.</para>
    ///  <para>Method of IHiliteElemAttrs.</para>
    ///  </remarks>
    function IsNul: Boolean;
    ///  <summary>Returns the element's font style.</summary>
    ///  <remarks>Method of IHiliteElemAttrs.</remarks>
    function GetFontStyle: TFontStyles;
    ///  <summary>Sets the element's font style to the given value.</summary>
    ///  <remarks>Method of IHiliteElemAttrs.</remarks>
    procedure SetFontStyle(const AFontStyle: TFontStyles);
    ///  <summary>Gets the element's foreground (font) colour.</summary>
    ///  <remarks>Method of IHiliteElemAttrs.</remarks>
    function GetForeColor: TColor;
    ///  <summary>Sets the element's foreground (font) colour to the given
    ///  value.</summary>
    ///  <remarks>Method of IHiliteElemAttrs.</remarks>
    procedure SetForeColor(const AColor: TColor);
    ///  <summary>Assigns the properties of the given element attributes
    ///  object to this instance.</summary>
    ///  <remarks>
    ///  <para>Src must either support IHiliteElemAttrs or be nil. If nil then
    ///  this instance is reset to its default values.</para>
    ///  <para>Method of IAssignable.</para>
    ///  </remarks>
    procedure Assign(const Src: IInterface);
  end;

type
  ///  <summary>Implements a list of named syntax highlighter attributes where
  ///  attributes are accessed by name.</summary>
  TNamedHiliterAttrs = class(TInterfacedObject,
    INamedHiliteAttrs, IAssignable
  )
  strict private
    var
      ///  <summary>Map of names to syntax highlighter attributes.</summary>
      fMap: TDictionary<string,IHiliteAttrs>;
  public
    ///  <summary>Creates new instance with empty list.</summary>
    constructor Create;
    ///  <summary>Destroys instance.</summary>
    destructor Destroy; override;
    ///  <summary>Checks if highlighter attributes with given name exists.
    ///  </summary>
    ///  <remarks>Method of INamedHiliteAttrs.</remarks>
    function Contains(const Name: string): Boolean;
    ///  <summary>Checks if list is empty.</summary>
    ///  <remarks>Method of INamedHiliteAttrs.</remarks>
    function IsEmpty: Boolean;
    ///  <summary>Deletes highlighter attributes with given name.</summary>
    ///  <remarks>Method of INamedHiliteAttrs.</remarks>
    procedure Delete(const Name: string);
    ///  <summary>Clears list of highlighter attributes.</summary>
    ///  <remarks>Method of INamedHiliteAttrs.</remarks>
    procedure Clear;
    ///  <summary>Gets highlighter attributes with given name, which must exist.
    ///  </summary>
    ///  <remarks>Method of INamedHiliteAttrs.</remarks>
    function GetHiliter(const Name: string): IHiliteAttrs;
    ///  <summary>Stores given highlighter attributes in list with given name.
    ///  If name is already present its highighter attributes are overwritten.
    ///  </summary>
    ///  <remarks>Method of INamedHiliteAttrs.</remarks>
    procedure SetHiliter(const Name: string; Hiliter: IHiliteAttrs);
    ///  <summary>Returns an array of highlighter attribute names.</summary>
    ///  <remarks>Method of INamedHiliteAttrs.</remarks>
    function GetNames: TArray<string>;
    ///  <summary>Assigns properties of given Src object to this instance. Src
    ///  must support INamedHiliteAttrs.</summary>
    ///  <remarks>Method of IAssignable.</remarks>
    procedure Assign(const Src: IInterface);
  end;

{ THiliteAttrs }

procedure THiliteAttrs.Assign(const Src: IInterface);
var
  Elem: THiliteElement; // loops thru all highlight elements
begin
  if Assigned(Src) then
  begin
    if not Supports(Src, IHiliteAttrs) then
      raise EBug.Create(
        ClassName + '.Assign: Src does not support IHiliteAttrs'
      );
    // Src is assigned: copy its properties
    with Src as IHiliteAttrs do
    begin
      Self.SetFontName(FontName);
      Self.SetFontSize(FontSize);
      for Elem := Low(THiliteElement) to High(THiliteElement) do
        (Self.GetElement(Elem) as IAssignable).Assign(Elements[Elem]);
    end;
  end
  else
  begin
    // Src is nil: set defaults
    Self.SetFontName(cDefFontName);
    Self.SetFontSize(cDefFontSize);
    // Get each element attribute to sets its own default by assigning nil
    for Elem := Low(THiliteElement) to High(THiliteElement) do
      (Self.GetElement(Elem) as IAssignable).Assign(nil);
  end;
end;

constructor THiliteAttrs.Create;
var
  Elem: THiliteElement; // loops thru all highlight elements
begin
  inherited;
  // Set default font values
  fFontName := cDefFontName;
  fFontSize := cDefFontSize;
  // Create list that holds an null object for each highlight element
  // Low(THiliteElement) is at index 0 in list
  fElemAttrs := TList<IHiliteElemAttrs>.Create;
  for Elem := Low(THiliteElement) to High(THiliteElement) do
    fElemAttrs.Add(THiliteElemAttrs.Create);
end;

destructor THiliteAttrs.Destroy;
begin
  fElemAttrs.Free;  // releases each object in list
  inherited;
end;

function THiliteAttrs.GetElement(
  const Elem: THiliteElement): IHiliteElemAttrs;
begin
  // Note: Low(THiliteElement) is at index 0 in list. Following code does *not*
  // assume that Ord(Low(THiliteElement)) = 0.
  Result := fElemAttrs[Ord(Elem) - Ord(Low(THiliteElement))];
end;

function THiliteAttrs.GetFontName: string;
begin
  Result := fFontName;
end;

function THiliteAttrs.GetFontSize: Integer;
begin
  Result := fFontSize;
end;

procedure THiliteAttrs.ResetDefaultFont;
begin
  SetFontName(cDefFontName);
  SetFontSize(cDefFontSize);
end;

procedure THiliteAttrs.SetFontName(const AFontName: string);
begin
  fFontName := AFontName;
end;

procedure THiliteAttrs.SetFontSize(const AFontSize: Integer);
begin
  if TFontHelper.IsInCommonFontSizeRange(AFontSize) then
    fFontSize := AFontSize
  else
    fFontSize := cDefFontSize;
end;

{ THiliteElemAttrs }

procedure THiliteElemAttrs.Assign(const Src: IInterface);
begin
  if Assigned(Src) then
  begin
    if not Supports(Src, IHiliteElemAttrs) then
      raise EBug.Create(
        ClassName + '.Assign: Src does not support IHiliteElemAttrs'
      );
    // Src is assigned: copy its properties
    with Src as IHiliteElemAttrs do
    begin
      Self.SetForeColor(ForeColor);
      Self.SetFontStyle(FontStyle);
    end;
  end
  else
  begin
    // Src is nil: reset properties to default (null) values
    Self.SetForeColor(clNone);
    Self.SetFontStyle([]);
  end;
end;

constructor THiliteElemAttrs.Create;
begin
  inherited;
  // Intialise properties to default (null) values
  fForeColor := clNone;
  fFontStyle := [];
end;

function THiliteElemAttrs.GetFontStyle: TFontStyles;
begin
  Result := fFontStyle;
end;

function THiliteElemAttrs.GetForeColor: TColor;
begin
  Result := fForeColor;
end;

function THiliteElemAttrs.IsNul: Boolean;
begin
  Result := (fFontStyle = []) and (fForeColor = clNone);
end;

procedure THiliteElemAttrs.SetFontStyle(const AFontStyle: TFontStyles);
begin
  fFontStyle := AFontStyle;
end;

procedure THiliteElemAttrs.SetForeColor(const AColor: TColor);
begin
  fForeColor := AColor;
end;

{ THiliteAttrsFactory }

class function THiliteAttrsFactory.CloneAttrs(
  Attrs: IHiliteAttrs): IHiliteAttrs;
begin
  Result := THiliteAttrs.Create;
  (Result as IAssignable).Assign(Attrs);
end;

class function THiliteAttrsFactory.CreateDefaultAttrs: IHiliteAttrs;
begin
  Result := THiliteAttrsFactory.CreatePredefinedAttrs(hsRADStudio);
end;

class function THiliteAttrsFactory.CreateNamedAttrs: INamedHiliteAttrs;
begin
  Result := TNamedHiliterAttrs.Create;
end;

class function THiliteAttrsFactory.CreateNulAttrs: IHiliteAttrs;
begin
  // Just create highlighter object: null values are defaults
  Result := THiliteAttrs.Create;
end;

class function THiliteAttrsFactory.CreatePredefinedAttrs(
  const Style: TPredefinedHiliteStyle): IHiliteAttrs;
type
  // Table of highlighter attributes.
  TAttrsTable = array[THiliteElement] of record
    ForeColor: TColor;        // foreground (text) colour
    FontStyle: TFontStyles;   // set of font styles
  end;
const
  // Map of predefined styles to source code element attributes.
  cPredefinedStyle: array[TPredefinedHiliteStyle] of TAttrsTable =  (
    ( // hsNul
      (ForeColor: clNone;     FontStyle: [];),          // heWhitespace
      (ForeColor: clNone;     FontStyle: [];),          // heComment
      (ForeColor: clNone;     FontStyle: [];),          // heReserved
      (ForeColor: clNone;     FontStyle: [];),          // heIdentifier
      (ForeColor: clNone;     FontStyle: [];),          // heSymbol
      (ForeColor: clNone;     FontStyle: [];),          // heString
      (ForeColor: clNone;     FontStyle: [];),          // heNumber
      (ForeColor: clNone;     FontStyle: [];),          // heFloat
      (ForeColor: clNone;     FontStyle: [];),          // heHex
      (ForeColor: clNone;     FontStyle: [];),          // hePreProcessor
      (ForeColor: clNone;     FontStyle: [];),          // heAssembler
      (ForeColor: clNone;     FontStyle: [];)           // heError
    ),
    ( // hsCodeSnip
      (ForeColor: clNone;     FontStyle: [];),          // heWhitespace
      (ForeColor: clNavy;     FontStyle: [fsItalic];),  // heComment
      (ForeColor: clNone;     FontStyle: [fsBold];),    // heReserved
      (ForeColor: clNone;     FontStyle: [];),          // heIdentifier
      (ForeColor: clNone;     FontStyle: [];),          // heSymbol
      (ForeColor: clPurple;   FontStyle: [];),          // heString
      (ForeColor: clMaroon;   FontStyle: [];),          // heNumber
      (ForeColor: clMaroon;   FontStyle: [];),          // heFloat
      (ForeColor: clMaroon;   FontStyle: [];),          // heHex
      (ForeColor: clGreen;    FontStyle: [];),          // hePreProcessor
      (ForeColor: clNone;     FontStyle: [fsItalic];),  // heAssembler
      (ForeColor: clRed;      FontStyle: [];)           // heError
    ),
    ( // hsDelphi7
      (ForeColor: clNone;     FontStyle: [];),          // heWhitespace
      (ForeColor: clNavy;     FontStyle: [fsItalic];),  // heComment
      (ForeColor: clBlack;    FontStyle: [fsBold];),    // heReserved
      (ForeColor: clBlack;    FontStyle: [];),          // heIdentifier
      (ForeColor: clBlack;    FontStyle: [];),          // heSymbol
      (ForeColor: clNavy;     FontStyle: [];),          // heString
      (ForeColor: clNavy;     FontStyle: [];),          // heNumber
      (ForeColor: clNavy;     FontStyle: [];),          // heFloat
      (ForeColor: clNavy;     FontStyle: [];),          // heHex
      (ForeColor: clNavy;     FontStyle: [];),          // hePreProcessor
      (ForeColor: clBlack;    FontStyle: [];),          // heAssembler
      (ForeColor: clRed;      FontStyle: [];)           // heError
    ),
    ( // hsRADStudio
      (ForeColor: clNone;     FontStyle: [];),          // heWhitespace
      (ForeColor: clGreen;    FontStyle: [fsItalic];),  // heComment
      (ForeColor: clNavy;     FontStyle: [fsBold];),    // heReserved
      (ForeColor: clNone;     FontStyle: [];),          // heIdentifier
      (ForeColor: clNone;     FontStyle: [];),          // heSymbol
      (ForeColor: clBlue;     FontStyle: [];),          // heString
      (ForeColor: clBlue;     FontStyle: [];),          // heNumber
      (ForeColor: clBlue;     FontStyle: [];),          // heFloat
      (ForeColor: clBlue;     FontStyle: [];),          // heHex
      (ForeColor: clTeal;     FontStyle: [];),          // hePreProcessor
      (ForeColor: clNone;     FontStyle: [];),          // heAssembler
      (ForeColor: clRed;      FontStyle: [];)           // heError
    ),
    ( // hsVisualStudio
      (ForeColor: clNone;     FontStyle: [];),          // heWhitespace
      (ForeColor: clGreen;    FontStyle: [];),          // heComment
      (ForeColor: clBlue;     FontStyle: [];),          // heReserved
      (ForeColor: clBlack;    FontStyle: [];),          // heIdentifier
      (ForeColor: clNone;     FontStyle: [];),          // heSymbol
      (ForeColor: clNone;     FontStyle: [];),          // heString
      (ForeColor: clNone;     FontStyle: [];),          // heNumber
      (ForeColor: clNone;     FontStyle: [];),          // heFloat
      (ForeColor: clNone;     FontStyle: [];),          // heHex
      (ForeColor: clBlue;     FontStyle: [];),          // hePreProcessor
      (ForeColor: clBlack;    FontStyle: [];),          // heAssembler
      (ForeColor: clRed;      FontStyle: [];)           // heError
    )
  );
var
  Elem: THiliteElement;   // loops thru highlighter elements
begin
  // Create highlighter with default (null) values
  // (we accept default font name and size)
  Result := THiliteAttrs.Create;
  // Set required properties for highlight elements per table
  for Elem := Low(THiliteElement) to High(THiliteElement) do
  begin
    Result[Elem].FontStyle := cPredefinedStyle[Style][Elem].FontStyle;
    Result[Elem].ForeColor := cPredefinedStyle[Style][Elem].ForeColor;
  end;
end;

class function THiliteAttrsFactory.CreatePrintAttrs(
  const Attrs: IHiliteAttrs; const UseColour: Boolean): IHiliteAttrs;
var
  Elem: THiliteElement; // loops thru all highlighter elements
begin
  if not Assigned(Attrs) then
    // No highlighter: use null
    Result := CreateNulAttrs
  else
  begin
    // Copy provided Attr
    Result := THiliteAttrs.Create;
    (Result as IAssignable).Assign(Attrs);
    if not UseColour then
      // Remove colour information: not used
      for Elem := Low(THiliteElement) to High(THiliteElement) do
        Result[Elem].ForeColor := clNone;
  end;
end;

class function THiliteAttrsFactory.CreateUserAttrs: IHiliteAttrs;
begin
  Result := THiliteAttrs.Create;
  (Result as IAssignable).Assign(Preferences.HiliteAttrs);
end;

{ TNamedHiliterAttrs }

procedure TNamedHiliterAttrs.Assign(const Src: IInterface);
var
  Name: string;
  SrcHiliters: INamedHiliteAttrs;
begin
  Assert(Supports(Src, INamedHiliteAttrs), ClassName + '.Assign: Src is nil');
  Clear;
  SrcHiliters := Src as INamedHiliteAttrs;
  for Name in SrcHiliters.Names do
    fMap.Add(Name, SrcHiliters[Name]);
end;

procedure TNamedHiliterAttrs.Clear;
begin
  fMap.Clear;
end;

function TNamedHiliterAttrs.Contains(const Name: string): Boolean;
begin
  Result := fMap.ContainsKey(Name);
end;

constructor TNamedHiliterAttrs.Create;
begin
  inherited Create;
  fMap := TDictionary<string,IHiliteAttrs>.Create(
    TTextEqualityComparer.Create
  );
end;

procedure TNamedHiliterAttrs.Delete(const Name: string);
begin
  if fMap.ContainsKey(Name) then
    fMap.Remove(Name);
end;

destructor TNamedHiliterAttrs.Destroy;
begin
  fMap.Free;
  inherited;
end;

function TNamedHiliterAttrs.GetHiliter(const Name: string): IHiliteAttrs;
begin
  Assert(fMap.ContainsKey(Name), ClassName + '.GetHiliter: Name not found');
  Result := THiliteAttrsFactory.CloneAttrs(fMap[Name]);
end;

function TNamedHiliterAttrs.GetNames: TArray<string>;
var
  Idx: Integer;
  Enum: TEnumerator<string>;
begin
  SetLength(Result, fMap.Count);
  Enum := fMap.Keys.GetEnumerator;
  try
    Idx := 0;
    while Enum.MoveNext do
    begin
      Result[Idx] := Enum.Current;
      Inc(Idx);
    end;
  finally
    Enum.Free;
  end;
end;

function TNamedHiliterAttrs.IsEmpty: Boolean;
begin
  Result := fMap.Count = 0;
end;

procedure TNamedHiliterAttrs.SetHiliter(const Name: string;
  Hiliter: IHiliteAttrs);
var
  Clone: IHiliteAttrs;
begin
  Clone := THiliteAttrsFactory.CloneAttrs(Hiliter);
  if fMap.ContainsKey(Name) then
    fMap[Name] := Clone
  else
    fMap.Add(Name, Clone);
end;

end.

