{
 * Hiliter.UGlobals.pas
 *
 * Declares various types that describe syntax hilighters and and defines
 * interfaces to various syntax highlighters and highlighter attributes.
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
 * The Original Code is Hiliter.UGlobals.pas, formerly IntfHiliter.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit Hiliter.UGlobals;


interface


uses
  // Delphi
  Classes, Graphics,
  // Project
  UEncodings;


type
  {
  THiliteElement:
    Defines the different elements that can be highlighted in Pascal source
    code.
  }
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
  {
  IHiliteElemAttrs:
    Interface supported by objects that store display attributes applicable to
    various elements used in syntax highlighting.
  }
  IHiliteElemAttrs = interface(IInterface)
    ['{297A2F3D-77A8-45F9-A147-22A53791F114}']
    function IsNul: Boolean;
      {Checks whether an element's attributes are "nul" - i.e. all properties
      have default values. Used to determine whether to output formatting
      information for an element.
        @return True if element's attributes are nul.
      }
    function GetFontStyle: TFontStyles;
      {Gets the font style to use for element.
        @return Set of font styles.
      }
    procedure SetFontStyle(const AFontStyle: TFontStyles);
      {Sets style of font to use for element.
        @param AFontStyle Required set of font styles.
      }
    function GetForeColor: TColor;
      {Get the foreground colour (i.e. text colour) to use for element.
        @return The colour.
      }
    procedure SetForeColor(const AColor: TColor);
      {Sets foreground colour (i.e. text colour) to use for element.
        @param AColor Required colour.
      }
    property FontStyle: TFontStyles read GetFontStyle write SetFontStyle;
      {Set of font styles to use for element}
    property ForeColor: TColor read GetForeColor write SetForeColor;
      {Foreground (i.e. text) colour to use for element. clNone => use default
      colour}
  end;

type
  {
  IHiliteAttrs:
    Interface implemented by objects that store display attributes that are used
    in a syntax highlighter.
  }
  IHiliteAttrs = interface(IInterface)
    ['{25570AEE-3225-42A7-A534-3D27357EEA2E}']
    function GetFontName: string;
      {Gets name of font to use for all output.
        @return Name of font.
      }
    procedure SetFontName(const AFontName: string);
      {Sets name of font to use for all output.
        @param AFontName Required font name.
      }
    function GetFontSize: Integer;
      {Gets size of font to use for all output.
        @return Size of font in points.
      }
    procedure SetFontSize(const AFontSize: Integer);
      {Sets size of font to use for all output.
        @param AFontSize Required font size in points.
      }
    procedure ResetDefaultFont;
      {Resets font name and size to default value.
      }
    function GetElement(const Elem: THiliteElement): IHiliteElemAttrs;
      {Gets the highlight attributes of a highlighter element.
        @param Elem Required element.
        @return Highlight attributes for element.
      }
    property FontName: string read GetFontName write SetFontName;
      {Name of font used for all output}
    property FontSize: Integer read GetFontSize write SetFontSize;
      {Size of font used for all output in points}
    property Elements[const Elem: THiliteElement]: IHiliteElemAttrs
      read GetElement; default;
      {List of highlight attributes of each highlight element}
  end;

type
  {
  TPredefinedHiliteStyle:
    Enumeration that specifies the different perdefined highlighter styles.
  }
  TPredefinedHiliteStyle = (
    hsNul,          // nul highlighter style
    hsCodeSnip,     // original codesnip default style
    hsDelphi7,      // Delphi 7 default style
    hsDelphi2006,   // Delphi 2006 default style
    hsVisualStudio  // Microsoft Visual Studio default style
  );


type
  {
  ISyntaxHiliter:
    Interface implemented by highlighter classes. Provides a method used to
    highlight a document.
  }
  ISyntaxHiliter = interface(IInterface)
    ['{8FDE87E5-66AD-49AD-BDA8-2110F47C0F6C}']
    procedure Hilite(const RawCode: string);
      {Highlights source code using renderer passed to constructor. Output is
      dealt with by renderer.
        @param RawCode [in] Source code to be highlighted.
      }
  end;

type
  ///  <summary>Interface implemented by object that format different source
  ///  code elements on behalf of syntax highlighter.</summary>
  ///  <remarks>Implement this interface for each required output format.
  ///  Syntax highlighter calls the methods of this interface.</remarks>
  IHiliteRenderer = interface(IInterface)
    ['{791CE200-C614-40FC-B93D-744ED2984755}']
    ///  <summary>Called by syntax highlighter before any source code is
    ///  processed.</summary>
    procedure Initialise;
    ///  <summary>Called by syntax hgihlighter after all source code has been
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
    procedure BeforeElem(Elem: THiliteElement);
    ///  <summary>Called by syntax highlighter for each element of source code
    ///  read. All the given text should be formatted in same style.</summary>
    ///  <remarks>Type of the element will have been specified in prior call to
    ///  BeforeElem.</remarks>
    procedure WriteElemText(const Text: string);
    ///  <summary>Called by syntax highlighter just after an element of source
    ///  code has been written.</summary>
    ///  <param name="Elem">THiliteElement [in] Type of element that has just
    ///  been output.</param>
    procedure AfterElem(Elem: THiliteElement);
  end;


implementation

end.

