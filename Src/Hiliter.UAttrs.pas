{
 * Hiliter.UAttrs.pas
 *
 * Implements classes that define attributes of syntax highlighters.
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
 * The Original Code is Hiliter.UAttrs.pas, formerly UHiliteAttrs.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit Hiliter.UAttrs;


interface


{
  NOTES:

  Syntax highlighter attribute objects are output-neutral objects that store the
  attributes of various syntax highlighters. These objects are used by the
  objects that format source code to determine the font and colour information
  to use.

  The objects define a font name and size used for the whole output. Each
  different highlight element (e.g. reserved words, comments etc) can have its
  own font colour and style (italic, bold, underline).

  Therefore the highlighter object records a single font name and size along
  with an array of objects that define the various highlight elements.
}


uses
  // Project
  Hiliter.UGlobals, UBaseObjects;


type

  {
  THiliteAttrsFactory:
    Factory class used to create various highlight attribute objects.
  }
  THiliteAttrsFactory = class(TNoConstructObject)
  public
    class function CreateNulAttrs: IHiliteAttrs;
      {Creates a nul highlighter object: one that provides no additional
      formatting information other than default font and size.
        @return Highlighter instance.
      }
    class function CreateDefaultAttrs: IHiliteAttrs;
      {Creates a highlighter object that uses program's default highlighting
      style.
        @return Highlighter instance.
      }
    class function CreateUserAttrs: IHiliteAttrs;
      {Creates a highlighter object that uses highlighting style defined by
      user.
        @return Highlighter instance.
      }
    class function CreatePrintAttrs(const Attrs: IHiliteAttrs;
      const UseColour: Boolean): IHiliteAttrs;
      {Creates a copy of a highlighter suitable for printing. Ensures font is
      Courier New and removes colours if mono printing required.
        @param Attrs [in] Highlighter attributes to be converted for printing.
          If nul then the nul highlighter is used.
        @param UseColour [in] Flag indicating whether colour required. When
          False all colour information is removed.
        @return New instance of highlighter adapted for printing.
      }
    class function CreateDisplayAttrs: IHiliteAttrs;
      {Creates a highlighter object to use to render source code in the main
      display. Based on user defined highlighter but with program's default mono
      font.
        @return Highlighter instance.
      }
    class function CreatePredefinedAttrs(
      const Style: TPredefinedHiliteStyle): IHiliteAttrs;
      {Creates a predefined highlighter object.
        @return Highlighter instance.
      }
  end;


implementation


uses
  // Delphi
  Generics.Collections, SysUtils, Graphics,
  // Project
  IntfCommon, UExceptions, UFontHelper, UPreferences;


type

  {
  THiliteAttrs:
    Object that stores display attributes that are used in a syntax highlighter.
    Supports assignment.
  }
  THiliteAttrs = class(TInterfacedObject,
    IHiliteAttrs, // defines highlight attributes and methods
    IAssignable   // defines object assignment
  )
  strict private
    var fElemAttrs: TList<IHiliteElemAttrs>;  // List of element attributes
    var fFontSize: Integer;                   // Size of font in points
    var fFontName: string;                    // Name of font
  protected // do not make strict
    { IHiliteAttrs methods }
    function GetFontName: string;
      {Gets name of font to use for all output.
        @return Name of font.
      }
    procedure SetFontName(const AFontName: string);
      {Sets name of font to use for all output.
        @param AFontName [in] Required font name.
      }
    function GetFontSize: Integer;
      {Gets size of font to use for all output.
        @return Size of font in points.
      }
    procedure SetFontSize(const AFontSize: Integer);
      {Sets size of font to use for all output.
        @param AFontSize [in] Required font size in points.
      }
    function GetElement(const Elem: THiliteElement): IHiliteElemAttrs;
      {Gets the attributes of a highlighter element.
        @param Elem [in] Required element.
        @return Highlight attributes for element.
      }
    { IAssignable method }
    procedure Assign(const Src: IInterface);
      {Assigns properties of a given object to this object.
        @param Src [in] Object whose properties are to be copied. If Src is nil
          object is reset to default values.
        @except EBug raised if Src is incompatible with this object.
      }
  public
    const cDefFontName = 'Courier New'; // Default font name
    const cDefFontSize = 9;             // Default font size
    constructor Create;
      {Object constructor. Sets up and intialises object.
      }
    destructor Destroy; override;
      {Object destructor. Tears down object.
      }
  end;

  {
  THiliteElemAttrs:
    Object that stores attributes applicable to various elements used in syntax
    highlighting. Supports assignment.
  }
  THiliteElemAttrs = class(TInterfacedObject,
    IHiliteElemAttrs, // defines element highlight attributes and methods
    IAssignable       // defines object assignment
  )
  strict private
    var fForeColor: TColor;       // Foreground (text) colour
    var fFontStyle: TFontStyles;  // Font styles
  protected // do not make strict
    { IHiliteElemAttrs methods }
    function IsNul: Boolean;
      {Checks whether element's attributes are "nul" - i.e. all properties have
      default values. Used to determine whether to output formatting information
      for an element.
        @return True if element's attributes are nul.
      }
    function GetFontStyle: TFontStyles;
      {Gets the font style to use for element.
        @return Set of font styles.
      }
    procedure SetFontStyle(const AFontStyle: TFontStyles);
      {Sets font style to use for element.
        @param AFontStyle [in] Required set of font styles.
      }
    function GetForeColor: TColor;
      {Gets the foreground colour (i.e. text colour) to use for element.
        @return Text colour.
      }
    procedure SetForeColor(const AColor: TColor);
      {Sets foreground colour (i.e. text colour) to use for element.
        @param AColor [in] Required colour.
      }
    { IAssignable methods }
    procedure Assign(const Src: IInterface);
      {Assigns properties of a given object to this object.
        @param Src [in] Object whose properties are to be copied. If Src is nil
          object is reset to default values.
        @except EBug raised if Src is incompatible with this object.
      }
  public
    constructor Create;
      {Object constructor. Sets up and initialises object.
      }
  end;


{ THiliteAttrs }

procedure THiliteAttrs.Assign(const Src: IInterface);
  {Assigns properties of a given object to this object.
    @param Src [in] Object whose properties are to be copied. If Src is nil
      object is reset to default values.
    @except EBug raised if Src is incompatible with this object.
  }
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
  {Object constructor. Sets up and intialises object.
  }
var
  Elem: THiliteElement; // loops thru all highlight elements
begin
  inherited;
  // Set default font values
  fFontName := cDefFontName;
  fFontSize := cDefFontSize;
  // Create list that holds an nul object for each highlight element
  // Low(THiliteElement) is at index 0 in list
  fElemAttrs := TList<IHiliteElemAttrs>.Create;
  for Elem := Low(THiliteElement) to High(THiliteElement) do
    fElemAttrs.Add(THiliteElemAttrs.Create);
end;

destructor THiliteAttrs.Destroy;
  {Object destructor. Tears down object.
  }
begin
  fElemAttrs.Free;  // releases each object in list
  inherited;
end;

function THiliteAttrs.GetElement(
  const Elem: THiliteElement): IHiliteElemAttrs;
  {Gets the attributes of a highlighter element.
    @param Elem [in] Required element.
    @return Highlight attributes for element.
  }
begin
  // Note: Low(THiliteElement) is at index 0 in list. Following code does *not*
  // assume that Ord(Low(THiliteElement)) = 0.
  Result := fElemAttrs[Ord(Elem) - Ord(Low(THiliteElement))];
end;

function THiliteAttrs.GetFontName: string;
  {Gets name of font to use for all output.
    @return Name of font.
  }
begin
  Result := fFontName;
end;

function THiliteAttrs.GetFontSize: Integer;
  {Gets size of font to use for all output.
    @return Size of font in points.
  }
begin
  Result := fFontSize;
end;

procedure THiliteAttrs.SetFontName(const AFontName: string);
  {Sets name of font to use for all output.
    @param AFontName [in] Required font name.
  }
begin
  fFontName := AFontName;
end;

procedure THiliteAttrs.SetFontSize(const AFontSize: Integer);
  {Sets size of font to use for all output.
    @param AFontSize [in] Required font size in points.
  }
begin
  fFontSize := AFontSize;
end;

{ THiliteElemAttrs }

procedure THiliteElemAttrs.Assign(const Src: IInterface);
  {Assigns properties of a given object to this object.
    @param Src [in] Object whose properties are to be copied. If Src is nil
      object is reset to default values.
    @except EBug raised if Src is incompatible with this object.
  }
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
    // Src is nil: reset properties to default (nul) values
    Self.SetForeColor(clNone);
    Self.SetFontStyle([]);
  end;
end;

constructor THiliteElemAttrs.Create;
  {Object constructor. Sets up and initialises object.
  }
begin
  inherited;
  // Intialise properties to default (nul) values
  fForeColor := clNone;
  fFontStyle := [];
end;

function THiliteElemAttrs.GetFontStyle: TFontStyles;
  {Gets the font style to use for element.
    @return Set of font styles.
  }
begin
  Result := fFontStyle;
end;

function THiliteElemAttrs.GetForeColor: TColor;
  {Gets the foreground colour (i.e. text colour) to use for element.
    @return Text colour.
  }
begin
  Result := fForeColor;
end;

function THiliteElemAttrs.IsNul: Boolean;
  {Checks whether element's attributes are "nul" - i.e. all properties have
  default values. Used to determine whether to output formatting information for
  an element.
    @return True if element's attributes are nul.
  }
begin
  Result := (fFontStyle = []) and (fForeColor = clNone);
end;

procedure THiliteElemAttrs.SetFontStyle(const AFontStyle: TFontStyles);
  {Sets font style to use for element.
    @param AFontStyle [in] Required set of font styles.
  }
begin
  fFontStyle := AFontStyle;
end;

procedure THiliteElemAttrs.SetForeColor(const AColor: TColor);
  {Sets foreground colour (i.e. text colour) to use for element.
    @param AColor [in] Required colour.
  }
begin
  fForeColor := AColor;
end;

{ THiliteAttrsFactory }

class function THiliteAttrsFactory.CreateDefaultAttrs: IHiliteAttrs;
  {Creates a highlighter object that uses program's default highlighting style.
    @return Highlighter instance.
  }
begin
  Result := THiliteAttrsFactory.CreatePredefinedAttrs(hsDelphi2006);
end;

class function THiliteAttrsFactory.CreateDisplayAttrs: IHiliteAttrs;
  {Creates a highlighter object to use to render source code in the main
  display. Based on user defined highlighter but with program's default mono
  font.
    @return Highlighter instance.
  }
var
  Font: TFont;  // display mono font
begin
  Result := CreateUserAttrs;
  // Ensure we use required mono display font
  Font := TFont.Create;
  try
    TFontHelper.SetDefaultMonoFont(Font, True);
    Result.FontName := Font.Name;
    Result.FontSize := Font.Size;
  finally
    Font.Free;
  end;
end;

class function THiliteAttrsFactory.CreateNulAttrs: IHiliteAttrs;
  {Creates a nul highlighter object: one that provides no additional formatting
  information other than default font and size.
    @return Highlighter instance.
  }
begin
  // Just create highlighter object: nul values are defaults
  Result := THiliteAttrs.Create;
end;

class function THiliteAttrsFactory.CreatePredefinedAttrs(
  const Style: TPredefinedHiliteStyle): IHiliteAttrs;
  {Creates a predefined highlighter object.
    @return Highlighter instance.
  }
type
  {
  TAttrsTable:
    Table of highlighter attributes.
  }
  TAttrsTable = array[THiliteElement] of record
    ForeColor: TColor;        // foreground (text) colour
    FontStyle: TFontStyles;   // set of font styles
  end;
const
  // Defines predefined styles
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
    ( // hsDelphi2006
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
  // Create highlighter with default (nul) values
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
  {Creates a copy of a highlighter suitable for printing. Ensures font is
  Courier New and removes colours if mono printing required.
    @param Attrs [in] Highlighter attributes to be converted for printing. If
      nul then the nul highlighter is used.
    @param UseColour [in] Flag indicating whether colour required. When False
      all colour information is removed.
    @return New instance of highlighter adapted for printing.
  }
var
  Elem: THiliteElement; // loops thru all highlighter elements
begin
  if not Assigned(Attrs) then
    // No highlighter: use nul
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
  // Ensure we use required printing fonts
  Result.FontName := THiliteAttrs.cDefFontName;
  Result.FontSize := THiliteAttrs.cDefFontSize;
end;

class function THiliteAttrsFactory.CreateUserAttrs: IHiliteAttrs;
  {Creates a highlighter object that uses highlighting style defined by user.
    @return Highlighter instance.
  }
begin
  Result := THiliteAttrs.Create;
  (Result as IAssignable).Assign(Preferences.HiliteAttrs);
end;

end.

