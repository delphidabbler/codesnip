{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that uses a rich edit control to print a rich text format
 * document.
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
    procedure Print(const Document: TRTFMarkup);
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
  ClassHelpers.RichEdit,
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

procedure TPrintEngine.Print(const Document: TRTFMarkup);
var
  PrintMargins: TPrintMargins;  // page margins
  DocTitle: string;             // document title for print spooler
resourcestring
  sDefTitle = 'CodeSnip document';  // default document title
begin
  // Load document into engine
  RichEdit.Load(Document);
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

