{
 * UPrintMgr.pas
 *
 * Implements a class that manages printing of a document providing information
 * about a routine.
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
 * The Original Code is UPrintMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UPrintMgr;


interface


uses
  // Delphi
  Classes,
  // Project
  UBaseObjects, UPrintInfo, USnippets, UView;


type

  {
  TPrintMgr:
    Class that manages printing of a document providing information about a
    routine. It generates a RTF formatted document and passes it to the print
    engine for printing.
  }
  TPrintMgr = class(TNoPublicConstructObject)
  strict private
    fRoutine: TRoutine;
      {Reference to routine whose information is to be printed}
    procedure GeneratePrintDocument(const Stm: TStream);
      {Generates document suitable for printing by print engine.
        @param Stm [in] Stream to receive generated document.
      }
  strict protected
    constructor InternalCreate(const ViewItem: TViewItem);
      {Class constructor. Sets up object to print a view item.
        @param ViewItem [in] View item to be printed. Must be a routine.
      }
    procedure DoPrint;
      {Prints document containing information about a routine. Uses print
      engine.
      }
  public
    class procedure Print(const ViewItem: TViewItem);
      {Prints details of a view item using print engine.
        @param ViewItem [in] View item to be printed. Must be a routine.
      }
    class function CanPrint(const ViewItem: TViewItem): Boolean;
      {Checks if a view item can be printed.
        @param ViewItem [in] View item to be checked.
        @return True if view item can be printed, False if not.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UPrintEngine, UPrintDocuments;


{ TPrintMgr }

class function TPrintMgr.CanPrint(const ViewItem: TViewItem): Boolean;
  {Checks if a view item can be printed.
    @param ViewItem [in] View item to be checked.
    @return True if view item can be printed, False if not.
  }
begin
  Result := ViewItem.Kind = vkRoutine;
end;

procedure TPrintMgr.DoPrint;
  {Prints document containing information about a routine. Uses print engine.
  }
var
  PrintEngine: TPrintEngine;  // object that prints the print document
  DocStm: TStream;            // stream containing generated print document
begin
  PrintEngine := TPrintEngine.Create;
  try
    PrintEngine.Title := fRoutine.Name;
    DocStm := TMemoryStream.Create;
    try
      GeneratePrintDocument(DocStm);
      PrintEngine.Print(DocStm);
    finally
      FreeAndNil(DocStm);
    end;
  finally
    FreeAndNil(PrintEngine);
  end;
end;

procedure TPrintMgr.GeneratePrintDocument(const Stm: TStream);
  {Generates document suitable for printing by print engine.
    @param Stm [in] Stream to receive generated document.
  }
var
  PrintDoc: IPrintDocument;   // generates print document
begin
  PrintDoc := TRoutinePrintDocument.Create(fRoutine);
  PrintDoc.Generate(Stm);
  Stm.Position := 0;
end;

constructor TPrintMgr.InternalCreate(const ViewItem: TViewItem);
  {Class constructor. Sets up object to print a view item.
    @param ViewItem [in] View item to be printed. Must be a routine.
  }
begin
  Assert(Assigned(ViewItem), ClassName + '.InternalCreate: ViewItem is nil');
  Assert(ViewItem.Kind = vkRoutine,
    ClassName + '.InternalCreate: ViewItem is not a routine');
  inherited InternalCreate;
  fRoutine := ViewItem.Routine;
end;

class procedure TPrintMgr.Print(const ViewItem: TViewItem);
  {Prints details of a view item using print engine.
    @param ViewItem [in] View item to be printed. Must be a routine.
  }
begin
  with InternalCreate(ViewItem) do
    try
      DoPrint;
    finally
      Free;
    end;
end;

end.

