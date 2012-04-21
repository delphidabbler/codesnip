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
  ///  <summary>
  ///  Static class that generates HTML of rows of table that displayed compiler
  ///  results.
  ///  </summary>
  TCompResHTML = class(TNoConstructObject)
  strict private
    ///  <summary>Returns HTML image tag for image used to represent given
    ///  compilation result.</summary>
    class function ImageTag(const CompRes: TCompileResult): string;
    ///  <summary>Returns a description of given compile result.</summary>
    class function CompileResultDesc(const CompRes: TCompileResult): string;
    ///  <summary>Returns URL of image representing given compile result.
    ///  </summary>
    class function ImageResURL(const CompRes: TCompileResult): string;
    ///  <summary>Returns HTML of a table cell containing name of given
    ///  compiler.</summary>
    class function NameCell(const Compiler: ICompiler): string;
    ///  <summary>Returns HTML of a table cell containing an image that
    ///  indicates given compile result.</summary>
    class function ResultCell(const CompRes: TCompileResult): string;
  public
    ///  <summary>Generates and returns rows of an HTML table that displays
    ///  given compiler results for each compiler.</summary>
    class function TableRows(const CompileResults: TCompileResults): string;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  Compilers.UCompilers, UConsts, UHTMLUtils, UHTMLDetailUtils, UIStringList,
  UJavaScriptUtils, UResourceUtils, UStrUtils;


resourcestring
  // Compiler result image titles
  sCompResImgTitleOK     = 'Compiles OK';
  sCompResImgTitleWarn   = 'Compiles with warnings';
  sCompResImgTitleError  = 'Does not compile';
  sCompResImgTitleQuery  = 'Not tested';

const
  ///  Compiler result image width.
  CompResImgWidth   = 18;
  ///  Compiler result image height.
  CompResImgHeight  = 18;
  /// Table of compiler result images and associated titles.
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

class function TCompResHTML.CompileResultDesc(const CompRes: TCompileResult):
  string;
begin
  Result := CompResImgInfo[CompRes].Title;
end;

class function TCompResHTML.ImageResURL(const CompRes: TCompileResult):
  string;
begin
  Result := MakeResourceURL(CompResImgInfo[CompRes].ResName);
end;

class function TCompResHTML.ImageTag(const CompRes: TCompileResult): string;
begin
  Result := UHTMLUtils.ImageTag(
    ImageResURL(CompRes),
    CompileResultDesc(CompRes),
    CompResImgWidth,
    CompResImgHeight
  );
end;

class function TCompResHTML.NameCell(const Compiler: ICompiler): string;
var
  CompilerNameHTML: string; // HTML containing compiler name
begin
  // Any spaces in compiler name replaced by <br /> tags
  CompilerNameHTML := StrReplace(
    MakeSafeHTMLText(Compiler.GetName), ' ', MakeTag('br', ttSimple)
  );
  Result := MakeCompoundTag('th', CompilerNameHTML);
end;

class function TCompResHTML.ResultCell(const CompRes: TCompileResult): string;
begin
  Result := MakeCompoundTag('td', ImageTag(CompRes));
end;

class function TCompResHTML.TableRows(const CompileResults: TCompileResults):
  string;
var
  Compilers: ICompilers;
  Compiler: ICompiler;
  Row1, Row2: string;     // HTML for two rows in HTML table
begin
  Compilers := TCompilersFactory.CreateAndLoadCompilers;
  // Initialise HTML for two rows of table and resulting table HTML
  Row1 := MakeTag('tr', ttOpen);
  Row2 := MakeTag('tr', ttOpen);
  // Add to each table row for each compiler: compiler name in row 1 and LED
  // image representing compile result in row 2
  for Compiler in Compilers do
  begin
    Row1 := Row1 + NameCell(Compiler) + EOL;
    Row2 := Row2 + ResultCell(CompileResults[Compiler.GetID]) + EOL;
  end;
  // Close the two rows
  Row1 := Row1 + MakeTag('tr', ttClose);
  Row2 := Row2 + MakeTag('tr', ttClose);
  // Return HTML of two rows
  Result := Row1 + Row2;
end;

end.

