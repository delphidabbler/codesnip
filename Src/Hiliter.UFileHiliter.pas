{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that generates hilighted and formatted source code for a
 * specified file type.
}


unit Hiliter.UFileHiliter;


interface


uses
  // Project
  Hiliter.UGlobals, UEncodings, USourceFileInfo;


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
  Hiliter.UAttrs, Hiliter.UHiliters;


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
  HiliteAttrs: IHiliteAttrs;            // highlighter attributes
begin
  case fFileType of
    sfRTF: HilitedDocCls := TRTFDocumentHiliter;
    sfHTML: HilitedDocCls := TXHTMLDocumentHiliter;
    else HilitedDocCls := TNulDocumentHiliter;
  end;
  if fWantHiliting and IsHilitingSupported(fFileType) then
    HiliteAttrs := THiliteAttrsFactory.CreateUserAttrs
  else
    HiliteAttrs := nil;
  Result := HilitedDocCls.Hilite(SourceCode, HiliteAttrs, DocTitle);
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

