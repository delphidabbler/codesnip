{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2024, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that renders a document that describes a snippet as rich
 * text.
}


unit URTFSnippetDoc;


interface


uses
  // Project
  DB.UCollections,
  ActiveText.UMain, ActiveText.URTFRenderer, Hiliter.UGlobals, UEncodings,
  UIStringList, USnippetDoc, URTFBuilder, URTFStyles;


type
  ///  <summary>Renders a document that describes a snippet as rich text.
  ///  </summary>
  ///  <remarks>Highlighting of document's source code can be customised.
  ///  </remarks>
  TRTFSnippetDoc = class(TSnippetDoc)
  strict private
    var
      ///  <summary>Attributes that determine formatting of highlighted source
      ///  code formatting.</summary>
      fHiliteAttrs: IHiliteAttrs;
      ///  <summary>Object used to build rich text document.</summary>
      fBuilder: TRTFBuilder;
      ///  <summary>Flag indicates whether to output in colour.</summary>
      fUseColour: Boolean;
      ///  <summary>Styles to apply to snippet description active text.
      ///  </summary>
      fDescStyles: TActiveTextRTFStyleMap;
      ///  <summary>Styles to apply to snippet extra information active text.
      ///  </summary>
      fExtraStyles: TActiveTextRTFStyleMap;
      ///  <summary>Styling applied to URLs.</summary>
      fURLStyle: TRTFStyle;
    const
      ///  <summary>Name of main document font.</summary>
      MainFontName = 'Tahoma';
      ///  <summary>Name of mono font.</summary>
      MonoFontName = 'Courier New';
      ///  <summary>Size of font used for database information in points.
      ///  </summary>
      DBInfoFontSize = 9; // points
      ///  <summary>Size of heading font in points.</summary>
      HeadingFontSize = 16; // points
      ///  <summary>Size of sub-heading font in points.</summary>
      ///  <remarks>Used in descripton and extra active text.</remarks>
      SubHeadingFontSize = 12;
      ///  <summary>Size of paragraph font in points.</summary>
      ParaFontSize = 10;
      ///  <summary>Paragraph spacing in points.</summary>
      ParaSpacing = 6.0;
      ///  <summary>Spacing for non-paragrap blocks in points.</summary>
      NoParaBlockSpacing = 0.0;
      ///  <summary>Spacing of list blocks in points.</summary>
      ListSpacing = ParaSpacing;
      ///  <summary>Step size of indents and tabs in twips.</summary>
      IndentDelta = TRTFStyle.DefaultIndentDelta;
  strict private
    ///  <summary>Initialises RTF style used when rendering active text as RTF.
    ///  </summary>
    procedure InitStyles;
  strict protected
    ///  <summary>Initialises rich text document.</summary>
    procedure InitialiseDoc; override;

//    ///  <summary>Adds given heading (i.e. snippet name) to document. Can be
//    ///  user defined or from main database.</summary>
//    ///  <remarks>Heading is coloured according to whether user defined or not.
//    ///  </remarks>
//    procedure RenderHeading(const Heading: string; const UserDefined: Boolean);
//      override;

    ///  <summary>Output given heading, i.e. snippet name for snippet from a
    ///  given collection..</summary>
    ///  <remarks>Heading is coloured according the the snippet's collection.
    ///  </remarks>
    procedure RenderHeading(const Heading: string;
      const ACollectionID: TCollectionID); override;
    ///  <summary>Adds given snippet description to document.</summary>
    ///  <remarks>Active text formatting is observed and styled to suit
    ///  document.</remarks>
    procedure RenderDescription(const Desc: IActiveText); override;
    ///  <summary>Hilights given source code and adds to document.</summary>
    procedure RenderSourceCode(const SourceCode: string); override;
    ///  <summary>Adds given title followed by given text to document.</summary>
    procedure RenderTitledText(const Title, Text: string); override;
    ///  <summary>Adds a comma-separated list of text, preceded by given title,
    ///  to document.</summary>
    procedure RenderTitledList(const Title: string; List: IStringList);
      override;
    ///  <summary>Output given compiler test info, preceded by given heading.
    ///  </summary>
    procedure RenderCompilerInfo(const Heading: string;
      const Info: TCompileDocInfoArray); override;
    ///  <summary>Output message stating that there is no compiler test info,
    ///  preceded by given heading.</summary>
    procedure RenderNoCompilerInfo(const Heading, NoCompileTests: string);
      override;
    ///  <summary>Interprets and adds given extra information to document.
    ///  </summary>
    ///  <remarks>Active text formatting is observed and styled to suit
    ///  document.</remarks>
    procedure RenderExtra(const ExtraText: IActiveText); override;
    ///  <summary>Adds given information about code snippets database to
    ///  document.</summary>
    procedure RenderDBInfo(const Text: string); override;
    ///  <summary>Finalises document and returns content as encoded data.
    ///  </summary>
    function FinaliseDoc: TEncodedData; override;
  public
    ///  <summary>Constructs object to render a snippet.</summary>
    ///  <param name="HiliteAttrs">IHiliteAttrs [in] Defines style of syntax
    ///  highlighting used for source code.</param>
    ///  <param name="UseColour">Boolean [in] Flag that whether document is
    ///  printed in colour (True) or black and white (False).</param>
    constructor Create(const HiliteAttrs: IHiliteAttrs;
      const UseColour: Boolean = True);
    ///  <summary>Destroys object.</summary>
    destructor Destroy; override;
  end;


implementation


uses
  // Delphi
  Graphics,
  // Project
  Hiliter.UHiliters, UColours, UConsts, UGraphicUtils, UPreferences;


{ TRTFSnippetDoc }

constructor TRTFSnippetDoc.Create(const HiliteAttrs: IHiliteAttrs;
  const UseColour: Boolean = True);
begin
  inherited Create;
  fHiliteAttrs := HiliteAttrs;
  fUseColour := UseColour;
  fDescStyles := TActiveTextRTFStyleMap.Create;
  fExtraStyles := TActiveTextRTFStyleMap.Create;
  InitStyles;
end;

destructor TRTFSnippetDoc.Destroy;
begin
  fExtraStyles.Free;
  fDescStyles.Free;
  inherited;
end;

function TRTFSnippetDoc.FinaliseDoc: TEncodedData;
begin
  Result := TEncodedData.Create(fBuilder.Render.ToBytes, etASCII);
  fBuilder.Free;
end;

procedure TRTFSnippetDoc.InitialiseDoc;
begin
  // Create object used to build main rich text document
  fBuilder := TRTFBuilder.Create(0);  // Use default code page
  // Set up font table
  fBuilder.FontTable.Add(MainFontName, rgfSwiss, 0);
  fBuilder.FontTable.Add(fHiliteAttrs.FontName, rgfModern, 0);
  // set up colour table
  fBuilder.ColourTable.Add(clWarningText);
  fBuilder.ColourTable.Add(clVarText);
  fBuilder.ColourTable.Add(clExternalLink);
  fBuilder.ColourTable.Add(Preferences.DBHeadingColours[False]);
  fBuilder.ColourTable.Add(Preferences.DBHeadingColours[True]);
end;

procedure TRTFSnippetDoc.InitStyles;
begin
  fURLStyle := TRTFStyle.Create(
    [scColour], TRTFFont.CreateNull, 0.0, [], clExternalLink
  );

  // Active text styles

  // -- Active text block styles

  fDescStyles.Add(
    ekHeading,
    TRTFStyle.Create(
      [scParaSpacing, scFontStyles, scFontSize],
      TRTFParaSpacing.Create(0.0, 0.0),
      TRTFFont.CreateNull,
      SubHeadingFontSize,
      [fsBold],
      clNone
    )
  );
  fExtraStyles.Add(
    ekHeading,
    TRTFStyle.Create(
      [scParaSpacing, scFontStyles, scFontSize],
      TRTFParaSpacing.Create(ParaSpacing, 0.0),
      TRTFFont.CreateNull,
      SubHeadingFontSize,
      [fsBold],
      clNone
    )
  );

  fDescStyles.Add(
     ekPara,
     TRTFStyle.Create(TRTFParaSpacing.Create(ParaSpacing, 0.0))
  );
  fExtraStyles.Add(ekPara, fDescStyles[ekPara]);

  fDescStyles.Add(
    ekBlock,
    TRTFStyle.Create(TRTFParaSpacing.Create(NoParaBlockSpacing, 0.0))
  );
  fExtraStyles.Add(ekBlock, fDescStyles[ekBlock]);

  fDescStyles.Add(
    ekUnorderedList,
    TRTFStyle.Create(TRTFParaSpacing.Create(ListSpacing, 0.0))
  );
  fExtraStyles.Add(ekUnorderedList, fDescStyles[ekUnorderedList]);

  fDescStyles.Add(ekOrderedList, fDescStyles[ekUnorderedList]);
  fExtraStyles.Add(ekOrderedList, fDescStyles[ekOrderedList]);

  fDescStyles.Add(
    ekListItem,
    TRTFStyle.Create(
      [scIndentDelta],
      TRTFParaSpacing.CreateNull,
      TRTFFont.CreateNull,
      0.0,
      [],
      clNone,
      360
    )
  );
  fExtraStyles.Add(ekListItem, fDescStyles[ekListItem]);

  // -- Active text inline styles

  fDescStyles.Add(
    ekStrong,
    TRTFStyle.Create(
      [scFontStyles],
      TRTFFont.CreateNull,
      0.0,
      [fsBold],
      clNone
    )
  );
  fExtraStyles.Add(ekStrong, fDescStyles[ekStrong]);

  fDescStyles.Add(
    ekEm,
    TRTFStyle.Create(
      [scFontStyles],
      TRTFFont.CreateNull,
      0.0,
      [fsItalic],
      clNone
    )
  );
  fExtraStyles.Add(ekEm, fDescStyles[ekEm]);

  fDescStyles.Add(
    ekVar,
    TRTFStyle.Create(
      [scFontStyles, scColour],
      TRTFFont.CreateNull,
      0.0,
      [fsItalic],
      clVarText
    )
  );
  fExtraStyles.Add(ekVar, fDescStyles[ekVar]);

  fDescStyles.Add(
    ekWarning,
    TRTFStyle.Create(
      [scFontStyles, scColour],
      TRTFFont.CreateNull,
      0.0,
      [fsBold],
      clWarningText
    )
  );
  fExtraStyles.Add(ekWarning, fDescStyles[ekWarning]);

  fDescStyles.Add(
    ekMono,
    TRTFStyle.Create(
      [scFont],
      TRTFFont.Create(MonoFontName, rgfModern),
      0.0,
      [],
      clNone
    )
  );
  fExtraStyles.Add(ekMono, fDescStyles[ekMono]);

  // Fixes for monochrome

  if not fUseColour then
  begin
    fDescStyles.MakeMonochrome;
    fExtraStyles.MakeMonochrome;
    fURLStyle.MakeMonochrome;
  end;
end;

procedure TRTFSnippetDoc.RenderCompilerInfo(const Heading: string;
  const Info: TCompileDocInfoArray);

  // Calculate length of longest compiler name, in twips, when rendered on font
  // to be used to display them
  function MaxCompilerNameLenInTwips: SmallInt;
  var
    CompilerInfo: TCompileDocInfo;  // info about each compiler
    CompilerNames: IStringList;     // list of all compiler names
    Font: TFont;                    // font in which compile info displayed
  begin
    Font := TFont.Create;
    try
      Font.Name := MainFontName;
      Font.Size := ParaFontSize;
      CompilerNames := TIStringList.Create;
      for CompilerInfo in Info do
        CompilerNames.Add(CompilerInfo.Compiler);
      Result := MaxStringWidthTwips(CompilerNames.ToArray, Font);
    finally
      Font.Free;
    end;
  end;

var
  CompilerInfo: TCompileDocInfo;  // info about each compiler
  TabStop: SmallInt;              // tab stop where compile result displayed
begin
  // Calculate tab stop where compile results are displayed
  TabStop := (MaxCompilerNameLenInTwips div IndentDelta) * IndentDelta
    + 2 * IndentDelta;
  // Display heading
  fBuilder.SetFontStyle([fsBold]);
  fBuilder.SetParaSpacing(
    TRTFParaSpacing.Create(ParaSpacing, ParaSpacing / 3)
  );
  fBuilder.AddText(Heading);
  fBuilder.ResetCharStyle;
  fBuilder.EndPara;
  fBuilder.ClearParaFormatting;
  fBuilder.SetFontSize(ParaFontSize);
  // Display compiler table
  fBuilder.SetTabStops([TabStop]);
  for CompilerInfo in Info do
  begin
    fBuilder.AddText(CompilerInfo.Compiler);
    fBuilder.AddText(TAB);
    fBuilder.BeginGroup;
    fBuilder.SetFontStyle([fsItalic]);
    fBuilder.AddText(CompilerInfo.Result);
    fBuilder.EndGroup;
    fBuilder.EndPara;
  end;
end;

procedure TRTFSnippetDoc.RenderDBInfo(const Text: string);
begin
  fBuilder.SetParaSpacing(TRTFParaSpacing.Create(ParaSpacing, 0.0));
  fBuilder.SetFontSize(DBInfoFontSize);
  fBuilder.SetFontStyle([fsItalic]);
  fBuilder.AddText(Text);
  fBuilder.EndPara;
  fBuilder.ClearParaFormatting;
  fBuilder.ResetCharStyle;
end;

procedure TRTFSnippetDoc.RenderDescription(const Desc: IActiveText);
var
  RTFWriter: TActiveTextRTF;  // Object that generates RTF from active text
begin
  fBuilder.ResetCharStyle;
  fBuilder.SetFontSize(ParaFontSize);
  RTFWriter := TActiveTextRTF.Create;
  try
    RTFWriter.ElemStyleMap := fDescStyles;
    RTFWriter.DisplayURLs := True;
    RTFWriter.URLStyle := fURLStyle;
    RTFWriter.Render(Desc, fBuilder);
  finally
    RTFWriter.Free;
  end;
end;

procedure TRTFSnippetDoc.RenderExtra(const ExtraText: IActiveText);
var
  RTFWriter: TActiveTextRTF;  // Object that generates RTF from active text
begin
  Assert(ExtraText.HasContent,
    ClassName + '.RenderExtra: ExtraText has no content');
  RTFWriter := TActiveTextRTF.Create;
  try
    RTFWriter.ElemStyleMap := fExtraStyles;
    RTFWriter.DisplayURLs := True;
    RTFWriter.URLStyle := fURLStyle;
    RTFWriter.Render(ExtraText, fBuilder);
  finally
    RTFWriter.Free;
  end;
end;

//procedure TRTFSnippetDoc.RenderHeading(const Heading: string;
//  const UserDefined: Boolean);
//begin
//  fBuilder.SetFontStyle([fsBold]);
//  fBuilder.SetFontSize(HeadingFontSize);
//  if fUseColour then
//    fBuilder.SetColour(Preferences.DBHeadingColours[UserDefined]);
//  fBuilder.SetParaSpacing(TRTFParaSpacing.Create(0.0, ParaSpacing));
//  fBuilder.AddText(Heading);
//  fBuilder.EndPara;
//end;

procedure TRTFSnippetDoc.RenderHeading(const Heading: string;
  const ACollectionID: TCollectionID);
begin
  fBuilder.SetFontStyle([fsBold]);
  fBuilder.SetFontSize(HeadingFontSize);
  if fUseColour then
    fBuilder.SetColour(Preferences.DBHeadingColours[ACollectionID <> TCollectionID.__TMP__MainDBCollectionID]);
  fBuilder.SetParaSpacing(TRTFParaSpacing.Create(0.0, ParaSpacing));
  fBuilder.AddText(Heading);
  fBuilder.EndPara;
end;

procedure TRTFSnippetDoc.RenderNoCompilerInfo(const Heading,
  NoCompileTests: string);
begin
  // Display heading
  fBuilder.SetFontStyle([fsBold]);
  fBuilder.SetParaSpacing(
    TRTFParaSpacing.Create(ParaSpacing, ParaSpacing / 3)
  );
  fBuilder.AddText(Heading);
  fBuilder.ResetCharStyle;
  fBuilder.EndPara;
  fBuilder.ClearParaFormatting;
  fBuilder.SetFontSize(ParaFontSize);
  fBuilder.AddText(NoCompileTests);
  fBuilder.EndPara;
end;

procedure TRTFSnippetDoc.RenderSourceCode(const SourceCode: string);
var
  Renderer: IHiliteRenderer;  // renders highlighted source as RTF
resourcestring
  sHeading = 'Source Code:';
begin
  fBuilder.ResetCharStyle;
  fBuilder.SetFont(MainFontName);
  fBuilder.SetFontSize(ParaFontSize);
  fBuilder.SetFontStyle([fsBold]);
  fBuilder.SetParaSpacing(TRTFParaSpacing.Create(ParaSpacing, ParaSpacing));
  fBuilder.AddText(sHeading);
  fBuilder.ResetCharStyle;
  fBuilder.EndPara;
  fBuilder.ClearParaFormatting;
  Renderer := TRTFHiliteRenderer.Create(fBuilder, fHiliteAttrs);
  TSyntaxHiliter.Hilite(SourceCode, Renderer);
  fBuilder.EndPara;
end;

procedure TRTFSnippetDoc.RenderTitledList(const Title: string;
  List: IStringList);
begin
  RenderTitledText(Title, CommaList(List));
end;

procedure TRTFSnippetDoc.RenderTitledText(const Title, Text: string);
begin
  fBuilder.ClearParaFormatting;
  fBuilder.ResetCharStyle;
  fBuilder.SetFont(MainFontName);
  fBuilder.SetFontSize(ParaFontSize);
  fBuilder.SetParaSpacing(TRTFParaSpacing.Create(ParaSpacing, 0.0));
  fBuilder.BeginGroup;
  fBuilder.SetFontStyle([fsBold]);
  fBuilder.AddText(Title);
  fBuilder.EndGroup;
  fBuilder.AddText(' ' + Text);
  fBuilder.EndPara;
end;

end.

