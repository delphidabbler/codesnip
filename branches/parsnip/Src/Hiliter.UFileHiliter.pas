{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a class that generates hilighted and formatted source code for a
 * specified file type.
}


unit Hiliter.UFileHiliter;


interface


uses
  // Project
  UEncodings,
  USourceFileInfo;


type

  {
  TFileHiliter:
    Class that generates hilighted and formatted source code for a specified
    file type.
  }
  TFileHiliter = class(TObject)
  strict private
    fWantHiliting: Boolean;
      {Flag indicating whether output is to be highlighted. Ignored if target
      file type does not support highlighting}
    fFileType: TSourceFileType;
      {Type of source file to be targetted}
  public
    constructor Create(const WantHiliting: Boolean;
      const FileType: TSourceFileType);
      {Class contructor. Creates file highlighter object of requested kind.
        @param WantHighlighting [in] Whether user wants actual highlighting
          performed. When false source code output is correct kind for file
          type, but contains no special formatting.
        @param FileType [in] Specifies kind of file that highlighting is to
          target. This determines format of output.
      }
    function Hilite(const SourceCode, DocTitle: string): TEncodedData;
      {Highlights source code. Output is correctly formatted for file type.
        @param SourceCode [in] Source code to be highlighted.
        @param DocTitle [in] Title of document to be outputted. Ignored if
          file type does not support document title meta data.
        @return Highlighted source.
      }
    class function IsHilitingSupported(
      const FileType: TSourceFileType): Boolean;
      {Tests if a source file type supports highlighting.
        @param FileType [in] Source code file type.
        @return True if file type supports highlighting, false if not.
      }
  end;


implementation


uses
  // Project
  CS.Config,
  CS.SourceCode.Hiliter.Brushes,
  CS.SourceCode.Hiliter.Renderers,
  CS.SourceCode.Hiliter.Themes,
  UPreferences;


{ TFileHiliter }

constructor TFileHiliter.Create(const WantHiliting: Boolean;
  const FileType: TSourceFileType);
  {Class contructor. Creates file highlighter object of requested kind.
    @param WantHighlighting [in] Whether user wants actual highlighting
      performed. When false source code output is correct kind for file type,
      but contains no special formatting.
    @param FileType [in] Specifies kind of file that highlighting is to target.
      This determines format of output.
  }
begin
  inherited Create;
  fFileType := FileType;
  fWantHiliting := WantHiliting;
end;

function TFileHiliter.Hilite(const SourceCode, DocTitle: string): TEncodedData;
  {Highlights source code. Output is correctly formatted for file type.
    @param SourceCode [in] Source code to be highlighted.
    @param DocTitle [in] Title of document to be outputted. Ignored if file type
      does not support document title meta data.
    @return Highlighted source.
  }
var
  HilitedDocCls: TDocumentHiliterClass; // class used to create hilited document
  Theme: TSyntaxHiliteTheme;            // provides highlighting style
  Brush: TSyntaxHiliterBrush;           // highlights source code
begin
  case fFileType of
    sfRTF: HilitedDocCls := TRTFDocumentHiliter;
    sfHTML: HilitedDocCls := TXHTMLDocumentHiliter;
    else HilitedDocCls := TNulDocumentHiliter;
  end;
  if fWantHiliting and IsHilitingSupported(fFileType) then
    Theme := TConfig.Instance.HiliterThemes[
      Preferences.CurrentHiliteThemeIds[htkExport]
    ]
  else
    Theme := TSyntaxHiliteThemes.NullTheme;
  { TODO: revise to allow brush to be specified based on source code language.
          This will probably need extra Language ID parameter to be added.
          Alternatively see if this unit's code can be redistributed and the
          unit removed.
  }
  Brush := TSyntaxHiliterBrushes.CreateBrush('ObjectPascal');
  try
    Result := HilitedDocCls.Hilite(SourceCode, Brush, Theme, DocTitle);
  finally
    Brush.Free;
  end;
end;

class function TFileHiliter.IsHilitingSupported(
  const FileType: TSourceFileType): Boolean;
  {Tests if a source file type supports highlighting.
    @param FileType [in] Source code file type.
    @return True if file type supports highlighting, false if not.
  }
begin
  Result := FileType in [sfHTML, sfRTF];
end;

end.

