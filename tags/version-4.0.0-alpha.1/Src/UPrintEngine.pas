{
 * UPrintEngine.pas
 *
 * Implements a class that uses a rich edit control to print a rich text format
 * document.
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
 * The Original Code is UPrintEngine.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UPrintEngine;


interface


uses
  // Delphi
  Classes,
  // Project
  UHiddenRichEdit, URTFUtils;


type
  ///  <summary>
  ///  Record that defines the page margins on printed output, in pixels.
  ///  </summary>
  TPrintMargins = record
    Left, Top, Right, Bottom: Integer;  // the four margins
  end;

type
  ///  <summary>
  ///  Class that prints suitably formatted documents.
  ///  </summary>
  ///  <remarks>
  ///  "Suitable formatted" documents comprise RTF code. The class uses a
  ///  hidden rich edit control to these documents.
  ///  </remarks>
  TPrintEngine = class(THiddenRichEdit)
  strict private
    var
      ///  <summary>Value of Title property</summary>
      fTitle: string;
    ///  <summary>Gets print margins from page setup information.</summary>
    function GetPrintMargins: TPrintMargins;
  public
    ///  <summary>Prints given RTF document.</summary>
    procedure Print(const Document: TRTF);
    ///  <summary>Title of document that appears in print spooler.</summary>
    ///  <remarks>A default title is used if Title is not set or is set to
    ///  empty string.</remarks>
    property Title: string read fTitle write fTitle;
  end;


implementation


uses
  // Delphi
  Printers,
  // Project
  UMeasurement, UPrintInfo;


{ TPrintEngine }

function TPrintEngine.GetPrintMargins: TPrintMargins;

  // ---------------------------------------------------------------------------
  ///  <summary>Converts inches to horizontal pixels on printer's canvas.
  ///  </summary>
  function InchesToPixelsX(const Inches: Double): Integer;
  begin
    Result := InchesToPixels(Printer.Handle, Inches, axX);
  end;

  ///  <summary>Converts inches to vertical pixels on printer's canvas.
  ///  </summary>
  function InchesToPixelsY(const Inches: Double): Integer;
  begin
    Result := InchesToPixels(Printer.Handle, Inches, axY);
  end;
  // ---------------------------------------------------------------------------

begin
  // PageSetup object stores print margins in millimeters
  Result.Left := InchesToPixelsX(MMToInches(PrintInfo.PageMargins.Left));
  Result.Top := InchesToPixelsY(MMToInches(PrintInfo.PageMargins.Top));
  Result.Right := InchesToPixelsX(MMToInches(PrintInfo.PageMargins.Right));
  Result.Bottom := InchesToPixelsY(MMToInches(PrintInfo.PageMargins.Bottom));
end;

procedure TPrintEngine.Print(const Document: TRTF);
var
  PrintMargins: TPrintMargins;  // page margins
  DocTitle: string;             // document title for print spooler
resourcestring
  sDefTitle = 'CodeSnip document';  // default document title
begin
  // Load document into engine
  TRichEditHelper.Load(RichEdit, Document);
  // Set up page margins
  PrintMargins := GetPrintMargins;
  RichEdit.PageRect := Rect(
    PrintMargins.Left,
    PrintMargins.Top,
    Printer.PageWidth - PrintMargins.Right,
    Printer.PageHeight - PrintMargins.Bottom
  );
  // Set title
  if fTitle = '' then
    DocTitle := sDefTitle
  else
    DocTitle := fTitle;
  // Perform printing
  RichEdit.Print(DocTitle);
end;

end.

