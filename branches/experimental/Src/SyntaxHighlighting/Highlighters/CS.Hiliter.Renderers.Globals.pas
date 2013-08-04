{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Declares various types that describe syntax hilighters and and defines
 * interfaces to various syntax highlighters and highlighter attributes.
}


unit CS.Hiliter.Renderers.Globals;


interface


uses
  // Delphi
  Classes, Graphics,
  // Project
  CS.Hiliter.Parser,
  UEncodings;


type
  ///  <summary>Enumeration defining the different elements that can be
  ///  highlighted in Pascal source code.</summary>
  THiliteElement = (
    heWhitespace,   // white space
    heComment,      // comments: in (* .. *), { .. } or // styles
    heReserved,     // reserved word (keyword or directives)
    heIdentifier,   // an identifier that is not "reserved"
    heSymbol,       // punctuation symbol or symbol group
    heString,       // string or character literal preceeded by #
    heNumber,       // whole number
    heFloat,        // floating point number (may be in scientific format)
    heHex,          // hexadecimal integer
    hePreProcessor, // compiler directive: {$..} and (*$..*) styles supported
    heAssembler,    // assembler code between asm ... end keywords
    heError         // an unrecognised piece of code (shouldn't happen)
  );

type
  ///  <summary>Interface supported by objects that store style attributes
  ///  applicable to various source code elements used in syntax highlighting.
  ///  </summary>
  IHiliteElemAttrs = interface(IInterface)
    ['{297A2F3D-77A8-45F9-A147-22A53791F114}']
    ///  <summary>Checks whether an element's attributes are "null" - i.e. all
    ///  properties have default values.</summary>
    ///  <remarks>Use to determine whether to output formatting information for
    ///  an element or not.</remarks>
    function IsNul: Boolean;
    ///  <summary>Returns the font style to use for a source code element.
    ///  </summary>
    function GetFontStyle: TFontStyles;
    ///  <summary>Sets the font style to use for a source code element.
    ///  </summary>
    ///  <param name="AFontStyle">TFontStyles [in] Required font style.</param>
    procedure SetFontStyle(const AFontStyle: TFontStyles);
    ///  <summary>Returns the foreground (i.e. text) colour to use for a source
    ///  code element.</summary>
    function GetForeColor: TColor;
    ///  <summary>Sets the foreground (i.e. text) colour to use for a source
    ///  code element.</summary>
    ///  <param name="AColor">TColor [in] Required colour.</param>
    procedure SetForeColor(const AColor: TColor);
    ///  <summary>Set of font styles to use for a source code element.</summary>
    property FontStyle: TFontStyles read GetFontStyle write SetFontStyle;
    ///  <summary>Foreground (i.e. Text) colour to use for a source code
    ///  element.</summary>
    property ForeColor: TColor read GetForeColor write SetForeColor;
  end;

type
  ///  <summary>Interface implemented by objects that store style attributes
  ///  that are used in a syntax highlighter.</summary>
  IHiliteAttrs = interface(IInterface)
    ['{25570AEE-3225-42A7-A534-3D27357EEA2E}']
    ///  <summary>Returns name of font to use for all source code.</summary>
    function GetFontName: string;
    ///  <summary>Sets name of font to use for all source code.</summary>
    ///  <param name="AFontName">string [in] Name of required font.</param>
    procedure SetFontName(const AFontName: string);
    ///  <summary>Returns size of font to use for all source code.</summary>
    ///  <remarks>Font size is in points.</remarks>
    function GetFontSize: Integer;
    ///  <summary>Sets size of font to use for all source code.</summary>
    ///  <param name="AFontSize">Integer [in] Required font size in points.
    ///  </param>
    procedure SetFontSize(const AFontSize: Integer);
    ///  <summary>Resets name and size of font used for all source code to
    ///  default value.</summary>
    procedure ResetDefaultFont;
    ///  <summary>Returns style attributes for a source code element.</summary>
    ///  <param name="Elem">THiliteElement [in] Specifies required attribute.
    ///  </param>
    ///  <returns>IHiliteElemAttrs [in] Interface to object providing required
    ///  style attributes.</returns>
    function GetElement(const Elem: THiliteElement): IHiliteElemAttrs;
    ///  <summary>Name of font used for all source code.</summary>
    property FontName: string read GetFontName write SetFontName;
    ///  <summary>Size of font used for all soure code.</summary>
    property FontSize: Integer read GetFontSize write SetFontSize;
    ///  <summary>List of style attributes for each source code element.
    ///  </summary>
    property Elements[const Elem: THiliteElement]: IHiliteElemAttrs
      read GetElement; default;
  end;

type
  ///  <summary>Enumeration of identifiers for the various predefined
  ///  highlighter styles.</summary>
  TPredefinedHiliteStyle = (
    hsNul,          // nul highlighter style
    hsCodeSnip,     // original codesnip default style
    hsDelphi7,      // Delphi 7 default style
    hsRADStudio,    // RAD Studio default style
    hsVisualStudio  // Microsoft Visual Studio default style
  );

type
  ///  <summary>Interface implemented by objects that format different source
  ///  code elements on behalf of syntax highlighter.</summary>
  ///  <remarks>Implement this interface for each required output format.
  ///  Syntax highlighter calls the methods of this interface.</remarks>
  IHiliteRenderer2 = interface(IInterface)
  { TODO: rename IHiliteRenderer2 back to IHiliteRenderer when original
          removed. }
    ['{20ED37E9-DE80-42B5-A920-2A62F1753866}']
    ///  <summary>Called by syntax highlighter before any source code is
    ///  processed.</summary>
    procedure Initialise;
    ///  <summary>Called by syntax highlighter after all source code has been
    ///  processed.</summary>
    procedure Finalise;
    ///  <summary>Called by syntax highlighter when a new line of source code
    ///  is started.</summary>
    procedure BeginLine;
    ///  <summary>Called by syntax highlighter after a line of souce code is
    ///  complete.</summary>
    procedure EndLine;
    ///  <summary>Called by syntax highlighter just before a source code
    ///  element is to be output.</summary>
    ///  <param name="Elem">THiliteElement [in] Type of element to be output.
    ///  </param>
    procedure BeforeElem(const ElemInfo: TSyntaxHiliteElemInfo);
    // TODO: revise comment for BeforeElem
    ///  <summary>Called by syntax highlighter for each element of source code
    ///  read. All the given text should be formatted in same style.</summary>
    ///  <remarks>Type of the element will have been specified in prior call to
    ///  BeforeElem.</remarks>
    procedure WriteElemText(const Text: string);
    ///  <summary>Called by syntax highlighter just after an element of source
    ///  code has been written.</summary>
    ///  <param name="Elem">THiliteElement [in] Type of element that has just
    ///  been output.</param>
    procedure AfterElem(const ElemInfo: TSyntaxHiliteElemInfo);
    // TODO: revise comment for AfterElem
  end;

type
  ///  <summary>Interface implemented by objects that manage a list of named
  ///  syntax highlighter attributes.</summary>
  INamedHiliteAttrs = interface(IInterface)
    ['{6F18CD62-111A-4CE8-924C-88DE7D82A19F}']
    ///  <summary>Getter for Hiliters property.</summary>
    function GetHiliter(const Name: string): IHiliteAttrs;
    ///  <summary>Setter for Hiliters property.</summary>
    procedure SetHiliter(const Name: string; Hiliter: IHiliteAttrs);
    ///  <summary>Getter for Names property.</summary>
    function GetNames: TArray<string>;
    ///  <summary>Deletes highlighter attributes with given name.</summary>
    procedure Delete(const Name: string);
    ///  <summary>Clears list of highlighter attributes.</summary>
    procedure Clear;
    ///  <summary>Checks if highlighter attributes with given name exists.
    ///  </summary>
    function Contains(const Name: string): Boolean;
    ///  <summary>Checks if list is empty.</summary>
    function IsEmpty: Boolean;
    ///  <summary>Map of highlighter names to associated highlighter attributes.
    ///  </summary>
    property Hiliters[const Name: string]: IHiliteAttrs
      read GetHiliter write SetHiliter; default;
    ///  <summary>Array of highlighter names.</summary>
    property Names: TArray<string>
      read GetNames;
  end;


implementation

end.

