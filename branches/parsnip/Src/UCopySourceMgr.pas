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
 * Implements class that manages copying of a snippet's source code to the
 * clipboard. Code is copied as plain text and as rich text. Rich text version
 * is syntax highlighted.
}


unit UCopySourceMgr;


interface


uses
  // Project
  UCopyViewMgr, UEncodings, UView;


type
  ///  <summary>
  ///  Static abstract base class for objects that copy source code to the
  ///  clipboard.
  ///  </summary>
  TCopySourceCodeBase = class abstract(TCopyViewMgr)
  strict protected
    ///  <summary>Generates encoded data containing a Unicode plain text
    ///  document that provides information about the source code of the snippet
    ///  represented by the given view.</summary>
    class function GeneratePlainText(View: IView): TEncodedData; override;
    ///  <summary>Generates encoded data containing a RTF document that provides
    ///  information about the source code of the snippet represented by the
    ///  given view.</summary>
    class function GenerateRichText(View: IView): TEncodedData; override;
    ///  <summary>Generates source code for the snippet represented by the
    ///  given view. Source code is returned as a Unicode string.</summary>
    class function GenerateSourceCode(View: IView): string; virtual; abstract;
    class function HiliteBrushID(View: IView): string; virtual; abstract;
  public
    ///  <summary>Checks if a given view can be copied to the clipboard.
    ///  </summary>
    class function CanHandleView(View: IView): Boolean; override; abstract;
  end;

type
  ///  <summary>
  ///  Static class that manages copying of the raw source code of a single
  ///  snippet to the clipboard.
  ///  </summary>
  TCopySourceMgr = class sealed(TCopySourceCodeBase)
  strict protected
    ///  <summary>Returns the source code of the snippet represented by the
    ///  given view. Source code is returned as a Unicode string.</summary>
    class function GenerateSourceCode(View: IView): string; override;
    class function HiliteBrushID(View: IView): string; override;
  public
    ///  <summary>Checks if given view can be copied to the clipboard. Returns
    ///  True only if view represents a snippet.</summary>
    class function CanHandleView(View: IView): Boolean; override;
  end;

type
  ///  <summary>
  ///  Static class that manages creation and copying of annotated source code
  ///  of one or more code snippets to the clipboard.
  ///  </summary>
  TCopySnippetMgr = class sealed(TCopySourceCodeBase)
  strict protected
    ///  <summary>Returns an annotated code snippet generated from one or more
    ///  snippets represented by the given view. Source code is returned as a
    ///  Unicode string.</summary>
    class function GenerateSourceCode(View: IView): string; override;
    class function HiliteBrushID(View: IView): string; override;
  public
    ///  <summary>Checks if given view can be copied to the clipboard. Returns
    ///  True only if view contains one or more snippets that can be output as
    ///  annotated source code.</summary>
    class function CanHandleView(View: IView): Boolean; override;
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
  UPreferences,
  USnippetSourceGen;


{ TCopySourceCodeBase }

class function TCopySourceCodeBase.GeneratePlainText(View: IView): TEncodedData;
begin
  Result := TEncodedData.Create(GenerateSourceCode(View), etUnicode);
end;

class function TCopySourceCodeBase.GenerateRichText(View: IView): TEncodedData;
var
  Brush: TSyntaxHiliterBrush;
begin
  Brush := TSyntaxHiliterBrushes.CreateBrush(HiliteBrushID(View));
  try
    Result := TRTFDocumentHiliter.Hilite(
      GenerateSourceCode(View),
      Brush,
      TConfig.Instance.HiliterThemes[
        Preferences.CurrentHiliteThemeIds[htkExport]
      ]
    );
  finally
    Brush.Free;
  end;
end;

{ TCopySourceMgr }

class function TCopySourceMgr.CanHandleView(View: IView): Boolean;
begin
  Result := Supports(View, ISnippetView);
end;

class function TCopySourceMgr.GenerateSourceCode(View: IView): string;
begin
  Result := (View as ISnippetView).Snippet.SourceCode;
end;

class function TCopySourceMgr.HiliteBrushID(View: IView): string;
var
  Language: TSourceCodeLanguage;
begin
  Language := TConfig.Instance.SourceCodeLanguages[
    (View as ISnippetView).Snippet.LanguageID
  ];
  Result := Language.HiliterBrushID;
end;

{ TCopySnippetMgr }

class function TCopySnippetMgr.CanHandleView(View: IView): Boolean;
begin
  Result := TSnippetSourceGen.CanGenerate(View);
end;

class function TCopySnippetMgr.GenerateSourceCode(View: IView): string;
begin
  Result := TSnippetSourceGen.Generate(
    View, Preferences.SourceCommentStyle, Preferences.TruncateSourceComments
  );
end;

class function TCopySnippetMgr.HiliteBrushID(View: IView): string;
begin
  // Snippets can only be generated using TSnippetSourceGen if they are written
  // in Pascal
  Result := TSyntaxHiliterBrushes.PascalBrushID;
end;

end.

