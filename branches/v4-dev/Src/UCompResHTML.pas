{
 * UCompResHTML.pas
 *
 * Static class that generate HTML of parts of tables used to display compiler
 * results in details pane.
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
 * The Original Code is UCompResHTML.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UCompResHTML;


interface


uses
  // Project
  Compilers.UGlobals, UBaseObjects;


type

  {
  TCompResHTML:
    Static class that provides ids and HTML of table cells for compilation
    result tables displayed in the detail pane.
  }
  TCompResHTML = class(TNoConstructObject)
  strict private
    class function ImageTag(const CompRes: TCompileResult;
      const Id: string = ''): string;
      {Provides HTML image tag used to represent a compilation result. Image has
      title describing result.
        @param CompRes [in] Required compilation result.
        @param Id [in] Optional id attribute for image tag.
      }
    class function CompileResultDesc(const CompRes: TCompileResult): string;
      {Provides description of a compiler result.
        @param CompRes [in] Compiler result for which description wanted.
        @return Required description.
      }
    class function ImageResURL(const CompRes: TCompileResult): string;
      {Provides URL of image representing a compilation result.
        @param CompRes [in] Compiler result for which URL is required.
        @return Required URL.
      }
  public
    class function NameCell(const Compiler: ICompiler): string;
      {Generates HTML of a table cell containing the name of a compiler.
        @param Compiler [in] Compiler whose name is to be displayed.
        @result Required HTML code.
      }
    class function ResultCell(const CompRes: TCompileResult): string;
      {Generates HTML of a table cell that displays an image that indicates a
      compiler result.
        @param CompRes [in] Compiler result to be displayed.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UConsts, UHTMLUtils, UHTMLDetailUtils, UIStringList, UJavaScriptUtils,
  UResourceUtils, UStrUtils;


resourcestring
  // Compiler result image titles
  sCompResImgTitleOK     = 'Compiles OK';
  sCompResImgTitleWarn   = 'Compiles with warnings';
  sCompResImgTitleError  = 'Does not compile';
  sCompResImgTitleQuery  = 'Not tested';

const
  // Compiler result image sizes
  CompResImgWidth   = 18;
  CompResImgHeight  = 18;

  // Table of compiler result images and associated titles
  CompResImgInfo: array[TCompileResult] of record
    ResName: string;    // name of image resource
    Title: string;      // value of image tag title attribute
  end =(
    (ResName: 'led-green.png';  Title: sCompResImgTitleOK),    // dcOK
    (ResName: 'led-yellow.png'; Title: sCompResImgTitleWarn),  // dcWarn
    (ResName: 'led-red.png';    Title: sCompResImgTitleError), // dcError
    (ResName: 'led-off.png';    Title: sCompResImgTitleQuery)  // dcQuery
  );


{ TCompResHTML }

class function TCompResHTML.CompileResultDesc(
  const CompRes: TCompileResult): string;
  {Provides description of a compiler result.
    @param CompRes [in] Compiler result for which description wanted.
    @return Required description.
  }
begin
  Result := CompResImgInfo[CompRes].Title;
end;

class function TCompResHTML.ImageResURL(const CompRes: TCompileResult):
  string;
  {Provides URL of image representing a compilation result.
    @param CompRes [in] Compiler result for which URL is required.
    @return Required URL.
  }
begin
  Result := MakeResourceURL(CompResImgInfo[CompRes].ResName);
end;

class function TCompResHTML.ImageTag(const CompRes: TCompileResult;
  const Id: string): string;
  {Provides HTML image tag used to represent a compilation result. Image has
  title describing result.
    @param CompRes [in] Required compilation result.
    @param Id [in] Optional id attribute for image tag.
  }
begin
  Result := UHTMLUtils.ImageTag(
    ImageResURL(CompRes),
    CompileResultDesc(CompRes),
    CompResImgWidth,
    CompResImgHeight,
    Id
  );
end;

class function TCompResHTML.NameCell(
  const Compiler: ICompiler): string;
  {Generates HTML of a table cell containing the name of a compiler.
    @param Compiler [in] Compiler whose name is to be displayed.
    @result Required HTML code.
  }
var
  CompilerNameHTML: string; // HTML containing compiler name
begin
  // Any spaces in compiler name replaced by <br /> tags
  CompilerNameHTML := StrReplace(
    MakeSafeHTMLText(Compiler.GetName), ' ', MakeTag('br', ttSimple)
  );
  Result := MakeCompoundTag('th', CompilerNameHTML);
end;

class function TCompResHTML.ResultCell(
  const CompRes: TCompileResult): string;
  {Generates HTML of a table cell that displays an image that indicates a
  compiler result.
    @param CompRes [in] Compiler result to be displayed.
  }
begin
  Result := MakeCompoundTag('td', ImageTag(CompRes));
end;

end.

