{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a static class that manages and displays a test unit in a dialog
 * box.
}


unit UTestUnitDlgMgr;


interface


uses
  // Delphi
  Classes,
  // Project
  DB.USnippet, UBaseObjects;


type

  {
  TTestUnitDlgMgr:
    Static class that manages and displays a test unit in a dialog box.
  }
  TTestUnitDlgMgr = class(TNoConstructObject)
  public
    class procedure DisplayTestUnit(const Owner: TComponent;
      const Snippet: TSnippet);
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
  const Snippet: TSnippet);
  {Generates and displays a highlighted test compile unit in a dialog box.
    @param Owner [in] Component that owns the dialog box.
    @param Snippet [in] Snippet for which test unit is to be displayed.
  }
var
  TestUnitSource: string;   // source code of test unit
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
  TPreviewDlg.Execute(
    Owner,
    TXHTMLDocumentHiliter.Hilite(
      TestUnitSource, THiliteAttrsFactory.CreateUserAttrs
    ),
    dtHTML,
    Format(sDlgTitle, [Snippet.DisplayName])
  );
end;

end.

