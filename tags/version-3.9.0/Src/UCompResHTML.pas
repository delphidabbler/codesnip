{
 * UCompResHTML.pas
 *
 * Static classes that generate HTML of parts of tables used to display compiler
 * results.
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
 * Portions created by the Initial Developer are Copyright (C) 2006-2010 Peter
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
    Static class that provides information and HTML used in compiler tables that
    is common to information and compilers check panes.
  }
  TCompResHTML = class(TNoConstructObject)
  strict protected
    class function ImageTag(const CompRes: TCompileResult;
      const Id: string = ''): string;
      {Provides HTML image tag used to represent a compilation result. Image has
      title describing result.
        @param CompRes [in] Required compilation result.
        @param Id [in] Optional id attribute for image tag.
      }
  public
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
  end;

  {
  TInfoCompResHTML:
    Static class that provides ids and HTML of table cells for the compiler
    result table displayed in the routine view of the information pane.
  }
  TInfoCompResHTML = class(TCompResHTML)
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

  {
  TCompCheckResHTML:
    Static class that provides ids, classes and HTML of table cells and links
    for the compiler result table displayed in the routine view of the compiler
    check pane.
  }
  TCompCheckResHTML = class(TCompResHTML)
  strict private
    class function NameSpanClass(const Compiler: ICompiler): string;
      {Provides name of CSS class used to format a compiler name. This CSS class
      is used in the span tag that surrounds the compiler name. The class is
      used to indicates whether compiler is available or not.
        @param Compiler [in] Compiler for which class is required.
        @return Required class name (empty if compiler available).
      }
    class function NameSpanTitle(const Compiler: ICompiler): string;
      {Provides a title attribute for span tag that surrounds a compiler name.
      If compiler is note available title notes this. If compiler is availabele
      title is blank.
        @param Compiler [in] Compiler for which title is required.
        @return Required title or empty string.
      }
  public
    class function NameCell(const Compiler: ICompiler): string;
      {Generates HTML of a table cell that displays a compiler name. If compiler
      is not installed the name is greyed and has title indicating the case.
        @param Compiler [in] Compiler whose name is to be displayed.
      }
    class function ResultCell(const CompRes: TCompileResult): string;
      {Generates HTML of a table cell that displays an image that indicates a
      compiler result.
        @param CompRes [in] Compiler result to be displayed.
      }
    class function TestCellPlaceholder(const Compiler: ICompiler): string;
      {Generates HTML of a placeholder table cell for a test compilation result.
      The cell contains and image tag that displays an un-tested result and has
      a unique id attribute.
        @param Compiler [in] Compiler to be identified by the id attribute.
        @result Required HTML code.
      }
    class function TestImgId(const Compiler: ICompiler): string;
      {Provides a unique ID for an image used to a test display compilation
      result for a specific compiler.
        @param Compiler [in] Compiler for which id is required.
      }
    class function ErrCellPlaceholder(const Compiler: ICompiler): string;
      {Generates HTML of an empty placeholder table cell for use in displaying
      any error or warning links. The cell is given a unique id attribute.
        @param Compiler [in] Compiler to be identified by the id attribute.
        @result Required HTML code.
      }
    class function ErrCellId(const Compiler: ICompiler): string;
      {Provides a unique ID for table cell used to display errors and warnings
      from a test compilation for a specific compiler.
        @param Compiler [in] Compiler for which id is required.
      }
    class function LogLink(const Compiler: ICompiler): string;
      {Generates HTML of a link used to display error or warning logs created as
      a result of a test compilation by a specific compiler. If there are no
      errors or warnings no link is created.
        @param Compiler [in] Compiler for which the link is required.
        @param Required HTML.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, StrUtils,
  // Project
  UConsts, UHTMLUtils, UHTMLDetailUtils, UIStringList, UJavaScriptUtils,
  UResourceUtils;


resourcestring
  // Compiler image titles
  sCompImgTitleOK     = 'Compiles OK';
  sCompImgTitleWarn   = 'Compiles with warnings';
  sCompImgTitleError  = 'Does not compile';
  sCompImgTitleQuery  = 'Not tested';

const
  // Compiler image sizes
  cCompImgFileWidth   = 18;
  cCompImgFileHeight  = 18;

  // Table of compiler result images and associated titles
  cLEDInfo: array[TCompileResult] of record
    LED: string;    // name of LED image "file" resource
    Title: string;  // value of image tag title attribute
  end =(
    (LED: 'led-green.png';  Title: sCompImgTitleOK),    // dcOK
    (LED: 'led-yellow.png'; Title: sCompImgTitleWarn),  // dcWarn
    (LED: 'led-red.png';    Title: sCompImgTitleError), // dcError
    (LED: 'led-off.png';    Title: sCompImgTitleQuery)  // dcQuery
  );


{ TCompResHTML }

class function TCompResHTML.CompileResultDesc(
  const CompRes: TCompileResult): string;
  {Provides description of a compiler result.
    @param CompRes [in] Compiler result for which description wanted.
    @return Required description.
  }
begin
  Result := cLEDInfo[CompRes].Title;
end;

class function TCompResHTML.ImageResURL(const CompRes: TCompileResult): string;
  {Provides URL of image representing a compilation result.
    @param CompRes [in] Compiler result for which URL is required.
    @return Required URL.
  }
begin
  Result := MakeResourceURL(cLEDInfo[CompRes].LED);
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
    cCompImgFileWidth,
    cCompImgFileHeight,
    Id
  );
end;

{ TInfoCompResHTML }

class function TInfoCompResHTML.NameCell(
  const Compiler: ICompiler): string;
  {Generates HTML of a table cell containing the name of a compiler.
    @param Compiler [in] Compiler whose name is to be displayed.
    @result Required HTML code.
  }
var
  CompilerNameHTML: string; // HTML containing compiler name
begin
  // Any spaces in compiler name replaced by <br /> tags
  CompilerNameHTML := ReplaceStr(
    MakeSafeHTMLText(Compiler.GetName), ' ', MakeTag('br', ttSimple)
  );
  Result := MakeCompoundTag('th', CompilerNameHTML);
end;

class function TInfoCompResHTML.ResultCell(
  const CompRes: TCompileResult): string;
  {Generates HTML of a table cell that displays an image that indicates a
  compiler result.
    @param CompRes [in] Compiler result to be displayed.
  }
begin
  Result := MakeCompoundTag('td', ImageTag(CompRes));
end;

{ TCompCheckResHTML }

class function TCompCheckResHTML.ErrCellId(const Compiler: ICompiler): string;
  {Provides a unique ID for table cell used to display errors and warnings from
  a test compilation for a specific compiler.
    @param Compiler [in] Compiler for which id is required.
  }
begin
  Result := Format('errcell%d', [Ord(Compiler.GetID)]);
end;

class function TCompCheckResHTML.ErrCellPlaceholder(
  const Compiler: ICompiler): string;
  {Generates HTML of an empty placeholder table cell for use in displaying any
  error or warning links. The cell is given a unique id attribute.
    @param Compiler [in] Compiler to be identified by the id attribute.
    @result Required HTML code.
  }
var
  TableCellAttrs: IHTMLAttributes;  // attributes of table cell tag
begin
  TableCellAttrs := THTMLAttributes.Create;
  TableCellAttrs.Add('class', 'testerr');
  TableCellAttrs.Add('id', ErrCellId(Compiler));
  Result := MakeCompoundTag('td', TableCellAttrs, '&nbsp;');
end;

class function TCompCheckResHTML.LogLink(const Compiler: ICompiler): string;
  {Generates HTML of a link used to display error or warning logs created as
  a result of a test compilation by a specific compiler. If there are no errors
  or warnings no link is created.
    @param Compiler [in] Compiler for which the link is required.
    @param Required HTML.
  }
resourcestring
  // Compiler results strings
  sWarningStatus    = 'warnings';
  sErrorStatus      = 'errors';
  sViewLinkText     = 'View %s';
  sHint             = 'View the %0:s compilation %1:s';
var
  LogStatus: string;  // status text
begin
  // Set up status and hints according to compilation result
  case Compiler.GetLastCompileResult of
    crWarning:
      LogStatus := sWarningStatus;
    crError:
      LogStatus := sErrorStatus;
  end;
  // If we have some log text return HTML for link that accesses it
  if Compiler.HasErrorsOrWarnings then
    // We have warnings or errors: link causes dialog to be displayed
    Result := '&nbsp;&raquo;&nbsp;' + JSLink(
      JSLiteralFunc('viewCompilerLog', [Ord(Compiler.GetID)]),
      '|' + Format(sHint, [Compiler.GetName, LogStatus]),
      TIStringList.Create('command-link'),
      Format(sViewLinkText, [LogStatus])
    )
  else
    // No warnings or error: nothing to display
    Result := '&nbsp';
end;

class function TCompCheckResHTML.NameCell(const Compiler: ICompiler): string;
  {Generates HTML of a table cell that displays a compiler name. If compiler is
  not installed the name is greyed and has title indicating the case.
    @param Compiler [in] Compiler whose name is to be displayed.
  }
var
  TableCellAttrs: IHTMLAttributes;  // attributes of table cell tag
  SpanAttrs: IHTMLAttributes;       // attributes of span tag
begin
  TableCellAttrs := THTMLAttributes.Create;
  TableCellAttrs.Add('class', 'compiler');
  SpanAttrs := THTMLAttributes.Create;
  SpanAttrs.Add('class', NameSpanClass(Compiler));
  SpanAttrs.Add('title', NameSpanTitle(Compiler));
  Result := MakeCompoundTag(
    'td',
    TableCellAttrs,
    MakeCompoundTag('span', SpanAttrs, MakeSafeHTMLText(Compiler.GetName))
  );
end;

class function TCompCheckResHTML.NameSpanClass(
  const Compiler: ICompiler): string;
  {Provides name of CSS class used to format a compiler name. This CSS class is
  used in the span tag that surrounds the compiler name. The class is used to
  indicates whether compiler is available or not.
    @param Compiler [in] Compiler for which class is required.
    @return Required class name (empty if compiler available).
  }
const
  // Map of compiler availability to span class
  cSpanClass: array[Boolean] of string = ('disabled', '');
begin
  Result := cSpanClass[Compiler.IsAvailable];
end;

class function TCompCheckResHTML.NameSpanTitle(
  const Compiler: ICompiler): string;
  {Provides a title attribute for span tag that surrounds a compiler name. If
  compiler is note available title notes this. If compiler is availabele title
  is blank.
    @param Compiler [in] Compiler for which title is required.
    @return Required title or empty string.
  }
resourcestring
  // Hint string for uninstalled compilers
  sCompNotInstalled = '%s not installed';
begin
  if Compiler.IsAvailable then
    Result := ''
  else
    Result := MakeSafeHTMLText(Format(sCompNotInstalled, [Compiler.GetName]));
end;

class function TCompCheckResHTML.ResultCell(
  const CompRes: TCompileResult): string;
  {Generates HTML of a table cell that displays an image that indicates a
  compiler result.
    @param CompRes [in] Compiler result to be displayed.
  }
var
  TableCellAttrs: IHTMLAttributes;  // attributes of table cell tag
begin
  TableCellAttrs := THTMLAttributes.Create;
  TableCellAttrs.Add('class', 'dbres');
  Result := MakeCompoundTag('td', TableCellAttrs, ImageTag(CompRes));
end;

class function TCompCheckResHTML.TestCellPlaceholder(
  const Compiler: ICompiler): string;
  {Generates HTML of a placeholder table cell for a test compilation result. The
  cell contains and image tag that displays an un-tested result and has a unique
  id attribute.
    @param Compiler [in] Compiler to be identified by the id attribute.
    @result Required HTML code.
  }
var
  TableCellAttrs: IHTMLAttributes;  // attributes of table cell tag
begin
  TableCellAttrs := THTMLAttributes.Create;
  TableCellAttrs.Add('class', 'testres');
  Result := MakeCompoundTag(
    'td', TableCellAttrs, ImageTag(crQuery, TestImgId(Compiler))
  );
end;

class function TCompCheckResHTML.TestImgId(const Compiler: ICompiler): string;
  {Provides a unique ID for an image used to a test display compilation result
  for a specific compiler.
    @param Compiler [in] Compiler for which id is required.
  }
begin
  Result := Format('testimg%d', [Ord(Compiler.GetID)]);
end;

end.

