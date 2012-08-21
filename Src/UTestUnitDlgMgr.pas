{
 * UTestUnitDlgMgr.pas
 *
 * Implements a static class that manages and displays a test unit in a dialog
 * box.
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
 * The Original Code is UTestUnitDlgMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UTestUnitDlgMgr;


interface


uses
  // Delphi
  Classes,
  // Project
  UBaseObjects, USnippets;


type

  {
  TTestUnitDlgMgr:
    Static class that manages and displays a test unit in a dialog box.
  }
  TTestUnitDlgMgr = class(TNoConstructObject)
  public
    class procedure DisplayTestUnit(const Owner: TComponent;
      const Snippet: TRoutine);
      {Generates and displays a highlighted test compile unit in a dialog box.
        @param Owner [in] Component that owns the dialog box.
        @param Snippet [in] Snippet for which test unit is to be displayed.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  FmPreviewDlg, Hiliter.UAttrs, Hiliter.UGlobals, Hiliter.UHiliters, UTestUnit;


{ TTestUnitDlgMgr }

class procedure TTestUnitDlgMgr.DisplayTestUnit(const Owner: TComponent;
  const Snippet: TRoutine);
  {Generates and displays a highlighted test compile unit in a dialog box.
    @param Owner [in] Component that owns the dialog box.
    @param Snippet [in] Snippet for which test unit is to be displayed.
  }
var
  TestUnitSource: string;   // source code of test unit
  Hiliter: ISyntaxHiliter;  // highlighter object
resourcestring
  sDlgTitle = 'Test Unit for %s'; // caption of dialog box
begin
  // Generate unit source code
  with TTestUnit.Create(Snippet) do
    try
      TestUnitSource := GenerateUnitSource;
    finally
      Free;
    end;
  // Convert source to higlighted XHTML document and display it
  Hiliter := TSyntaxHiliterFactory.CreateHiliter(hkXHTML);
  TPreviewDlg.Execute(
    Owner,
    Hiliter.Hilite(TestUnitSource, THiliteAttrsFactory.CreateUserAttrs),
    Format(sDlgTitle, [Snippet.Name])
  );
end;

end.

