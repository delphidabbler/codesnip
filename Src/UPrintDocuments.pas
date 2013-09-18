{
 * UPrintDocuments.pas
 *
 * Provides interface and classes that can generate output suitable for printing
 * using print engine.
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
 * The Original Code is UPrintDocuments.pas
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


unit UPrintDocuments;


interface


uses
  // Delphi
  Classes,
  // Project
  Hiliter.UGlobals, USnippets;


type

  {
  IPrintDocument:
    Interface supported by classes that can generate a print document suitable
    for processing by the print engine.
  }
  IPrintDocument = interface(IInterface)
    ['{56E4CA97-7F04-427A-A95F-03CE55910DC0}']
    procedure Generate(const Document: TStream);
      {Generates print document
        @param Document [in] Stream that receives document in format suitable
          for print engine.
      }
  end;

  {
  TRoutinePrintDocument:
    Class that generates a print document that describes a routine.
  }
  TRoutinePrintDocument = class(TInterfacedObject,
    IPrintDocument
  )
  strict private
    fRoutine: TRoutine;
      {Reference to routine that print document describes}
    function GetHiliteAttrs: IHiliteAttrs;
      {Gets highlighter attributes required to render source code. Object
      depends on various printer properties.
        @return Required highlighter attributes object.
      }
  protected // do not make strict
    { IPrintDocument method }
    procedure Generate(const Document: TStream);
      {Generates print document.
        @param Document [in] Stream that receives document in format suitable
          for print engine.
      }
  public
    constructor Create(const Routine: TRoutine);
      {Class constructor. Sets up object.
        @param Routine [in] Routine for which print document is to be
          generated.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  Hiliter.UAttrs, URTFRoutineDoc, UPrintInfo;


{ TRoutinePrintDocument }

constructor TRoutinePrintDocument.Create(const Routine: TRoutine);
  {Class constructor. Sets up object.
    @param Routine [in] Routine for which print document is to be generated.
  }
begin
  inherited Create;
  fRoutine := Routine;
end;

procedure TRoutinePrintDocument.Generate(const Document: TStream);
  {Generates print document.
    @param Document [in] Stream that receives document in format suitable for
      print engine.
  }
var
  Doc: TRTFRoutineDoc;  // object that renders routine document in RTF
begin
  Doc := TRTFRoutineDoc.Create(
    GetHiliteAttrs, poUseColor in PrintInfo.PrintOptions
  );
  try
    Doc.Generate(fRoutine, Document);
  finally
    FreeAndNil(Doc);
  end;
end;

function TRoutinePrintDocument.GetHiliteAttrs: IHiliteAttrs;
  {Gets highlighter attributes required to render source code. Object depends on
  various printer properties.
    @return Required highlighter attributes object.
  }
begin
  if not (poSyntaxPrint in PrintInfo.PrintOptions) then
    Result := THiliteAttrsFactory.CreatePrintAttrs(nil, False)
  else
    Result := THiliteAttrsFactory.CreatePrintAttrs(
      THiliteAttrsFactory.CreateUserAttrs,
      poUseColor in PrintInfo.PrintOptions
    );
end;

end.

