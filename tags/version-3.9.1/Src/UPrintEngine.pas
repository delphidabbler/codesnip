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
 * Portions created by the Initial Developer are Copyright (C) 2007-2009 Peter
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
  UHiddenRichEdit;


type

  {
  TPrintMargins:
    Defines the page margins on printed output, in pixels.
  }
  TPrintMargins = record
    Left, Top, Right, Bottom: Integer;  // the four margins
  end;

  {
  TPrintEngine:
    Class that uses a rich edit control to print a rich text format document.
  }
  TPrintEngine = class(THiddenRichEdit)
  private
    fTitle: string;
      {Value of Title property}
    function GetPrintMargins: TPrintMargins;
      {Gets print margins from page setup information.
        @return Required margins.
      }
  public
    procedure Print(const Document: TStream);
      {Prints a document.
        @param Document [in] Stream containing document to be printed, in rich
          text format.
      }
    property Title: string read fTitle write fTitle;
      {Title that appears in print spooler}
  end;


implementation


uses
  // Delphi
  Printers,
  // Project
  UMeasurement, UPrintInfo, URTFUtils;


{ TPrintEngine }

function TPrintEngine.GetPrintMargins: TPrintMargins;
  {Gets print margins from page setup information.
    @return Required margins.
  }

  // ---------------------------------------------------------------------------
  function InchesToPixelsX(const Inches: Double): Integer;
    {Converts inches to horizontal pixels on printer's canvas.
      @param Inches [in] Measurement to covert.
    }
  begin
    Result := InchesToPixels(Printer.Handle, Inches, axX);
  end;

  function InchesToPixelsY(const Inches: Double): Integer;
    {Converts inches to vertical pixels on printer's canvas.
      @param Inches [in] Measurement to covert.
    }
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

procedure TPrintEngine.Print(const Document: TStream);
  {Prints a document.
    @param Document [in] Stream containing document to be printed, in rich text
      format.
  }
var
  PrintMargins: TPrintMargins;
begin
  // Load document into engine
  RTFLoadFromStream(RichEdit, Document);
  // Set up page margins
  PrintMargins := GetPrintMargins;
  RichEdit.PageRect := Rect(
    PrintMargins.Left,
    PrintMargins.Top,
    Printer.PageWidth - PrintMargins.Right,
    Printer.PageHeight - PrintMargins.Bottom
  );
  // Perform printing
  RichEdit.Print(fTitle);
end;

end.

