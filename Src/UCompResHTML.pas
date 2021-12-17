{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Static class that generate HTML of parts of tables used to display compiler
 * results in details pane.
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
    class function NameCell(const Compiler: ICompiler): string;
    ///  <summary>Returns HTML of a table cell containing an image that
    ///  indicates given compile result.</summary>
    class function ResultCell(const CompRes: TCompileResult): string;
    ///  <summary>Generates and returns rows of an HTML table that displays
    ///  given compiler result for each displayable compiler in Compilers.
    ///  </summary>
    class function CompileResultsTableRows(Compilers: ICompilers;
      const CompileResults: TCompileResults): string;
    ///  <summary>Generates and returns rows of an HTML table containing no
    ///  compiler result information.</summary>
    ///  <remarks>Table contains an explanation of the problem.</remarks>
    class function EmptyTableRows: string;
  public
    ///  <summary>Generates and returns rows of an HTML table that displays
    ///  given compiler results for each compiler.</summary>
    ///  <remarks>If there are no displayable compilers, an 'error table' is
    ///  generated that contains no compile results but instead has an
    ///  explanation of the problem.</remarks>
    class function TableRows(const CompileResults: TCompileResults): string;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  Compilers.UCompilers, UConsts, UCSSUtils, UHTMLUtils, UIStringList,
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

class function TCompResHTML.CompileResultsTableRows(Compilers: ICompilers;
  const CompileResults: TCompileResults): string;
var
  Row1, Row2: string;   // collect HTML of first and second table rows
  Compiler: ICompiler;  // each supported compiler
begin
  // Initialise HTML for two rows of table and resulting table HTML
  Row1 := THTML.OpeningTag('tr');
  Row2 := THTML.OpeningTag('tr');
  // Add to each table row for each compiler: compiler name in row 1 and LED
  // image representing compile result in row 2
  for Compiler in Compilers do
  begin
    if not Compiler.GetDisplayable then
      Continue;
    Row1 := Row1 + NameCell(Compiler) + EOL;
    Row2 := Row2 + ResultCell(CompileResults[Compiler.GetID]) + EOL;
  end;
  // Close the two rows
  Row1 := Row1 + THTML.ClosingTag('tr');
  Row2 := Row2 + THTML.ClosingTag('tr');
  // Return HTML of two rows
  Result := Row1 + Row2;
end;

class function TCompResHTML.EmptyTableRows: string;
resourcestring
  sHeading = 'No compiler results available';
  sMessage = 'Results for all compilers have been hidden.';
  sHelpText = 'More information';
begin
  Result := THTML.CompoundTag(
    'tr',
    THTML.CompoundTag(
      'th',
      THTML.CompoundTag(
        'span',
        THTMLAttributes.Create('class', 'warning'),
        THTML.Entities(sHeading)
      )
    )
  ) +
  THTML.CompoundTag(
    'tr',
    THTML.CompoundTag(
      'td',
      THTML.Entities(sMessage)
      + ' ' +
      THTML.CompoundTag(
        'a',
        THTMLAttributes.Create([
          THTMLAttribute.Create('href', 'help:AllCompilersHidden'),
          THTMLAttribute.Create('class', 'help-link')
        ]),
        THTML.Entities(sHelpText)
      )
      + '.'
    )
  );
end;

class function TCompResHTML.ImageTag(const CompRes: TCompileResult): string;
var
  Attrs: IHTMLAttributes; // image's attributes
begin
  // Create attributes
  Attrs := THTMLAttributes.Create;
  Attrs.Add('src', MakeResourceURL(CompResImgInfo[CompRes].ResName));
  Attrs.Add(
    'style',
    TIStringList.Create(
      [
        TCSS.VerticalAlignProp(cvaTop),
        TCSS.WidthProp(CompResImgWidth),
        TCSS.HeightProp(CompResImgHeight)
      ]
    )
  );
  Attrs.Add('title', CompResImgInfo[CompRes].Title);
  // Create tag
  Result := THTML.SimpleTag('img', Attrs);
end;

class function TCompResHTML.NameCell(const Compiler: ICompiler): string;
var
  CompilerNameHTML: string; // HTML containing compiler name
begin
  // Any spaces in compiler name replaced by <br /> tags
  CompilerNameHTML := StrReplace(
    THTML.Entities(Compiler.GetName), ' ', THTML.SimpleTag('br')
  );
  Result := THTML.CompoundTag('th', CompilerNameHTML);
end;

class function TCompResHTML.ResultCell(const CompRes: TCompileResult): string;
begin
  Result := THTML.CompoundTag('td', ImageTag(CompRes));
end;

class function TCompResHTML.TableRows(const CompileResults: TCompileResults):
  string;
var
  Compilers: ICompilers;     // HTML for two rows in HTML table
begin
  Compilers := TCompilersFactory.CreateAndLoadCompilers;
  if Compilers.HaveDisplayable then
    Result := CompileResultsTableRows(Compilers, CompileResults)
  else
    Result := EmptyTableRows;
end;

end.

