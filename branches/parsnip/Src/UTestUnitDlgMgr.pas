{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
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
  CS.Config,
  CS.SourceCode.Languages,
  CS.SourceCode.Hiliter.Brushes,
  CS.SourceCode.Hiliter.Renderers,
  CS.SourceCode.Hiliter.Themes,
  FmPreviewDlg,
  UEncodings,
  UPreferences,
  UTestUnit;


{ TTestUnitDlgMgr }

class procedure TTestUnitDlgMgr.DisplayTestUnit(const Owner: TComponent;
  const Snippet: TSnippet);
  {Generates and displays a highlighted test compile unit in a dialog box.
    @param Owner [in] Component that owns the dialog box.
    @param Snippet [in] Snippet for which test unit is to be displayed.
  }
var
  TestUnitSource: string;         // source code of test unit
  Language: TSourceCodeLanguage;  // source code language
  Brush: TSyntaxHiliterBrush;     // syntax highlighter brush for language
  Theme: TSyntaxHiliteTheme;      // syntax highlighter theme
  XHTMLDoc: TEncodedData;         // syntax highlighted source code XHTML
resourcestring
  sDlgTitle = 'Test Unit for %s'; // caption of dialog box
begin
  // TODO: apply extract method refactoring a couple of times
  // Generate unit source code
  with TTestUnit.Create(Snippet) do
    try
      TestUnitSource := GenerateUnitSource;
    finally
      Free;
    end;
  // Create XHTML document containing syntax highlighted source code.
  // Highlighting is performed using the current UI highlighter theme.
  Theme := TConfig.Instance.HiliterThemes[
    Preferences.CurrentHiliteThemeIds[htkUI]
  ];
  Language := TConfig.Instance.SourceCodeLanguages[Snippet.Language];
  Brush := TSyntaxHiliterBrushes.CreateBrush(Language.HiliterBrushID);
  try
    XHTMLDoc := TXHTMLDocumentHiliter.Hilite(TestUnitSource, Brush, Theme);
  finally
    Brush.Free;
  end;
  // Display dialogue box
  TPreviewDlg.Execute(
    Owner, XHTMLDoc, dtHTML, Format(sDlgTitle, [Snippet.DisplayName])
  );
end;

end.

