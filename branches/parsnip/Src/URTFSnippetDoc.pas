{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a class that renders a document that describes a snippet as rich
 * text.
}


unit URTFSnippetDoc;


interface


uses
  // Delphi
  Graphics,
  // Project
  CS.ActiveText.Renderers.RTF,
  CS.SourceCode.Hiliter.Brushes,
  CS.SourceCode.Hiliter.Themes,
  ActiveText.UMain,
  UEncodings,
  UIStringList,
  USnippetDoc,
  URTFBuilder,
  URTFStyles,
  URTFUtils;


type
  ///  <summary>Renders a document that describes a snippet as rich text.
  ///  </summary>
  ///  <remarks>Highlighting of document's source code can be customised.
  ///  </remarks>
  TRTFSnippetDoc = class(TSnippetDoc)
  strict private
    var
      ///  <summary>Theme used for styling syntax highlighting.</summary>
      fTheme: TSyntaxHiliteTheme;
      ///  <summary>Brush used to perform syntax highlighting.</summary>
      fBrush: TSyntaxHiliterBrush;
      ///  <summary>Object used to build rich text document.</summary>
      fBuilder: TRTFBuilder;
      ///  <summary>Flag indicates whether to output in colour.</summary>
      fUseColour: Boolean;
      ///  <summary>RTF styles used for snippet's description.</summary>
      fDescStyles: TActiveTextRTFStyleMap;
      ///  <summary>RTF styles used for snippet's extra information.</summary>
      fExtraStyles: TActiveTextRTFStyleMap;
      ///  <summary>Styling applied to URLs.</summary>
      fURLStyle: TRTFStyle;
    const
      ///  <summary>Name of main document font.</summary>
      MainFontName = 'Tahoma';
      ///  <summary>Name of mono font.</summary>
      MonoFontName = 'Courier New';
      ///  <summary>Size of heading font.</summary>
      HeadingFontSize = 16;
      ///  <summary>Size of paragraph font.</summary>
      ParaFontSize = 10;
      ///  <summary>Paragraph spacing in points.</summary>
      ParaSpacing = 12.0;
      ///  <summary>Size of font used for database information.</summary>
      DBInfoFontSize = 9;
  strict private
    ///  <summary>Initialises RTF style used when rendering active text as RTF.
    ///  </summary>
    procedure InitStyles;
  strict protected
    ///  <summary>Initialises rich text document.</summary>
    procedure InitialiseDoc; override;
    ///  <summary>Adds given heading to document.</summary>
    procedure RenderHeading(const Heading: string); override;
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
    ///  <summary>Adds given compiler info, preceeded by given heading, to
    ///  document.</summary>
    procedure RenderCompilerInfo(const Heading: string;
      const Info: TCompileDocInfoArray); override;
    ///  <summary>Interprets and adds given extra information to document.
    ///  </summary>
    ///  <remarks>Active text formatting is observed and styled to suit
    ///  document.</remarks>
    procedure RenderExtra(const ExtraText: IActiveText); override;
    ///  <summary>Finalises document and returns content as encoded data.
    ///  </summary>
    function FinaliseDoc: TEncodedData; override;

    function CreateActiveTextRenderer(const Styles: TActiveTextRTFStyleMap):
      IActiveTextRenderer;
  public
    ///  <summary>Constructs object to render a snippet.</summary>
    ///  <param name="ATheme">TSyntaxHiliteTheme [in] Theme to be used when
    ///  syntax highlighting source code.</param>
    ///  <param name="ABrush">TSyntaxHiliterBrush [in] Brush to be used to
    ///  syntax highlight source code.</param>
    ///  <param name="UseColour">Boolean [in] Flag that whether document is
    ///  printed in colour (True) or black and white (False).</param>
    constructor Create(const ATheme: TSyntaxHiliteTheme;
      const ABrush: TSyntaxHiliterBrush;
      const UseColour: Boolean = True);
    ///  <summary>Destroys object.</summary>
    destructor Destroy; override;
  end;


implementation


uses
  // Delphi
  SysUtils,
  Generics.Collections,
  // Project
  CS.SourceCode.Hiliter.Renderers,
  UColours,
  UConsts,
  UStrUtils;


{ TRTFSnippetDoc }

constructor TRTFSnippetDoc.Create(const ATheme: TSyntaxHiliteTheme;
  const ABrush: TSyntaxHiliterBrush; const UseColour: Boolean = True);
begin
  Assert(Assigned(ATheme), ClassName + '.Create: ATheme is nil');
  Assert(Assigned(ABrush), ClassName + '.Create: ABrush is nil');
  inherited Create;
  fTheme := ATheme.Clone(not UseColour);
  fBrush := ABrush;
  fUseColour := UseColour;
  fDescStyles := TActiveTextRTFStyleMap.Create;
  fExtraStyles := TActiveTextRTFStyleMap.Create;
  InitStyles;
end;

function TRTFSnippetDoc.CreateActiveTextRenderer(
  const Styles: TActiveTextRTFStyleMap): IActiveTextRenderer;
var
  Renderer: TActiveTextRTFRenderer;
begin
  Renderer := TActiveTextRTFRenderer.Create(fBuilder);
  Renderer.Options.ElemStyleMap := Styles;
  Renderer.Options.DisplayURLs := True;
  Renderer.Options.URLStyle := fURLStyle;
  Result := Renderer;
end;

destructor TRTFSnippetDoc.Destroy;
begin
  fTheme.Free;
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
  fBuilder.FontTable.Add(fTheme.FontName, rgfModern, 0);
  // set up colour table
  fBuilder.ColourTable.Add(clWarningText);
  fBuilder.ColourTable.Add(clVarText);
  fBuilder.ColourTable.Add(clExternalLink);
end;

procedure TRTFSnippetDoc.InitStyles;
begin
  fURLStyle := TRTFStyle.Create(
    [scColour], TRTFFont.CreateNull, 0.0, [], clExternalLink
  );

  fExtraStyles.Add(
     ekPara,
     TRTFStyle.Create(
       TRTFParaSpacing.Create(ParaSpacing, 0.0)
     )
  );
  fDescStyles.Add(
     ekPara,
     TRTFStyle.Create(
       TRTFParaSpacing.Create(0.0, ParaSpacing)
     )
  );

  fExtraStyles.Add(
    ekHeading,
    TRTFStyle.Create(
      [scParaSpacing, scFontStyles],
      TRTFParaSpacing.Create(ParaSpacing, 0.0),
      TRTFFont.CreateNull,
      0.0,
      [fsBold],
      clNone
    )
  );
  fDescStyles.Add(
    ekHeading,
    TRTFStyle.Create(
      [scParaSpacing, scFontStyles],
      TRTFParaSpacing.Create(0.0, ParaSpacing),
      TRTFFont.CreateNull,
      0.0,
      [fsBold],
      clNone
    )
  );

  fExtraStyles.Add(
    ekStrong,
    TRTFStyle.Create(
      [scFontStyles],
      TRTFFont.CreateNull,
      0.0,
      [fsBold],
      clNone
    )
  );
  fDescStyles.Add(ekStrong, fExtraStyles[ekStrong]);

  fExtraStyles.Add(
    ekEm,
    TRTFStyle.Create(
      [scFontStyles],
      TRTFFont.CreateNull,
      0.0,
      [fsItalic],
      clNone
    )
  );
  fDescStyles.Add(ekEm, fExtraStyles[ekEm]);

  fExtraStyles.Add(
    ekVar,
    TRTFStyle.Create(
      [scFontStyles, scColour],
      TRTFFont.CreateNull,
      0.0,
      [fsItalic],
      clVarText
    )
  );
  fDescStyles.Add(ekVar, fExtraStyles[ekVar]);

  fExtraStyles.Add(
    ekWarning,
    TRTFStyle.Create(
      [scFontStyles, scColour],
      TRTFFont.CreateNull,
      0.0,
      [fsBold],
      clWarningText
    )
  );
  fDescStyles.Add(ekWarning, fExtraStyles[ekWarning]);

  fExtraStyles.Add(
    ekMono,
    TRTFStyle.Create(
      [scFont],
      TRTFFont.Create(MonoFontName, rgfModern),
      0.0,
      [],
      clNone
    )
  );
  fDescStyles.Add(ekMono, fExtraStyles[ekMono]);

  if not fUseColour then
  begin
    fDescStyles.MakeMonochrome;
    fExtraStyles.MakeMonochrome;
    fURLStyle.MakeMonochrome;
  end;
end;

procedure TRTFSnippetDoc.RenderCompilerInfo(const Heading: string;
  const Info: TCompileDocInfoArray);
var
  Idx: Integer; // loops compiler information table
begin
  fBuilder.SetFontStyle([fsBold]);
  fBuilder.SetParaSpacing(
    TRTFParaSpacing.Create(ParaSpacing, ParaSpacing / 3)
  );
  fBuilder.AddText(Heading);
  fBuilder.ResetCharStyle;
  fBuilder.EndPara;
  fBuilder.ClearParaFormatting;
  fBuilder.SetFontSize(ParaFontSize);
  for Idx := Low(Info) to High(Info) do
  begin
    fBuilder.AddText(Info[Idx].Compiler);
    fBuilder.AddText(TAB);
    fBuilder.BeginGroup;
    fBuilder.SetFontStyle([fsItalic]);
    fBuilder.AddText(Info[Idx].Result);
    fBuilder.EndGroup;
    fBuilder.EndPara;
  end;
end;

procedure TRTFSnippetDoc.RenderDescription(const Desc: IActiveText);
begin
  fBuilder.ResetCharStyle;
  fBuilder.SetFontSize(ParaFontSize);
  Desc.Render(CreateActiveTextRenderer(fDescStyles));
end;

procedure TRTFSnippetDoc.RenderExtra(const ExtraText: IActiveText);
begin
  Assert(not ExtraText.IsEmpty, ClassName + '.RenderExtra: ExtraText is empty');
  ExtraText.Render(CreateActiveTextRenderer(fExtraStyles));
end;

procedure TRTFSnippetDoc.RenderHeading(const Heading: string);
begin
  fBuilder.SetFontStyle([fsBold]);
  fBuilder.SetFontSize(HeadingFontSize);
  fBuilder.SetParaSpacing(TRTFParaSpacing.Create(0.0, ParaSpacing));
  fBuilder.AddText(Heading);
  fBuilder.EndPara;
end;

procedure TRTFSnippetDoc.RenderSourceCode(const SourceCode: string);
var
  Renderer: IHiliteRenderer;  // renders highlighted source as RTF
begin
  fBuilder.ClearParaFormatting;
  Renderer := TRTFHiliteRenderer.Create(fBuilder, fBrush, fTheme);
  TSyntaxHiliter.Hilite(SourceCode, fBrush, Renderer);
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

