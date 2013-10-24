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
 * Implements a static class that creates and displays a test unit in a dialogue
 * box.
}


unit UTestUnitDlgMgr;


interface


uses
  // Delphi
  Classes,
  // Project
  CS.SourceCode.Languages,
  DB.USnippet,
  UBaseObjects,
  UEncodings;


type
  ///  <summary>Static class that creates and displays a test unit in a dialogue
  ///  box.</summary>
  TTestUnitDlgMgr = class(TNoConstructObject)
  strict private
    ///  <summary>Generates source code a test unit for the given snippet's
    ///  source code.</summary>
    class function GenerateTestUnit(const Snippet: TSnippet): string;
    ///  <summary>Syntax highlights the given source code with a highlighter
    ///  suitable for the given language and returns the highlighted code as
    ///  XHTML.</summary>
    class function HighlightSource(const SourceCode: string;
      const Language: TSourceCodeLanguage): TEncodedData;
  public
    ///  <summary>Generates and displays a syntax highlighted test compile unit
    ///  in a dialogue box.</summary>
    ///  <param name="Owner">TComponent [in] Component that owns the dialogue
    ///  box.</param>
    ///  <param name="Snippet">TSnippet [in] Snippet for which test unit is to
    ///  be displayed.</param>
    class procedure DisplayTestUnit(const Owner: TComponent;
      const Snippet: TSnippet);
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  CS.Config,
  CS.SourceCode.Hiliter.Brushes,
  CS.SourceCode.Hiliter.Renderers,
  CS.SourceCode.Hiliter.Themes,
  FmPreviewDlg,
  UPreferences,
  UTestUnit;


{ TTestUnitDlgMgr }

class procedure TTestUnitDlgMgr.DisplayTestUnit(const Owner: TComponent;
  const Snippet: TSnippet);
var
  XHTMLDoc: TEncodedData;         // syntax highlighted source code XHTML
resourcestring
  sDlgTitle = 'Test Unit for %s'; // caption of dialog box
begin
  XHTMLDoc := HighlightSource(
    GenerateTestUnit(Snippet),
    TConfig.Instance.SourceCodeLanguages[Snippet.Language]
  );
  TPreviewDlg.Execute(
    Owner, XHTMLDoc, dtHTML, Format(sDlgTitle, [Snippet.DisplayName])
  );
end;

class function TTestUnitDlgMgr.GenerateTestUnit(const Snippet: TSnippet):
  string;
begin
  with TTestUnit.Create(Snippet) do
    try
      Result := GenerateUnitSource;
    finally
      Free;
    end;
end;

class function TTestUnitDlgMgr.HighlightSource(const SourceCode: string;
  const Language: TSourceCodeLanguage): TEncodedData;
var
  Brush: TSyntaxHiliterBrush;     // syntax highlighter brush for language
  Theme: TSyntaxHiliteTheme;      // syntax highlighter theme
resourcestring
  sDlgTitle = 'Test Unit for %s'; // caption of dialog box
begin
  Theme := TConfig.Instance.HiliterThemes[
    Preferences.CurrentHiliteThemeIds[htkUI]
  ];
  Brush := TSyntaxHiliterBrushes.CreateBrush(Language.HiliterBrushID);
  try
    Result := TXHTMLDocumentHiliter.Hilite(SourceCode, Brush, Theme);
  finally
    Brush.Free;
  end;
end;

end.

