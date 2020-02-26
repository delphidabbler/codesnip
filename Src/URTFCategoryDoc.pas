{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that renders a rich text document that lists the snippets
 * in a category.
}


unit URTFCategoryDoc;


interface


uses
  // Delphi
  Graphics,
  // Project
  ActiveText.URTFRenderer, DB.UCategory, DB.USnippet, UEncodings, URTFBuilder,
  URTFStyles;


type
  ///  <summary>Renders a rich text document that lists the snippets in a
  ///  category.</summary>
  TRTFCategoryDoc = class(TObject)
  strict private
    const
      ///  <summary>Name of main document font.</summary>
      MainFontName = 'Tahoma';
      ///  <summary>Name of mono font.</summary>
      MonoFontName = 'Courier New';
      ///  <summary>Size of heading font.</summary>
      HeadingFontSize = 16.0;
      ///  <summary>Heading spacing in points</summary>
      HeadingSpacing = 0.0;
      ///  <summary>Size of sub-heading font.</summary>
      SubHeadingFontSize = 10.0;
      ///  <summary>Sub-heading spacing in points.</summary>
      SubHeadingSpacing = 14.0;
      ///  <summary>Size of paragraph font.</summary>
      ParaFontSize = 10.0;
      ///  <summary>Paragraph spacing in points.</summary>
      ParaSpacing = 12.0;
  strict private
    var
      ///  <summary>Object used to build rich text document.</summary>
      fBuilder: TRTFBuilder;
      ///  <summary>Flag indicates whether to output in colour.</summary>
      fUseColour: Boolean;
      ///  <summary>Map of active text action elements to their RTF styling.
      ///  </summary>
      fDescStyles: TActiveTextRTFStyleMap;
      ///  <summary>Styling applied to URLs.</summary>
      fURLStyle: TRTFStyle;
    ///  <summary>Outputs description of given category as a main heading as
    ///  RTF.</summary>
    procedure OutputCategoryHeading(const Category: TCategory);
    ///  <summary>Outputs name of given snippet as sub-heading as RTF.</summary>
    procedure OutputSnippetSubHeading(const Snippet: TSnippet);
    ///  <summary>Outputs description of given snippet as RTF.</summary>
    procedure OutputSnippetText(const Snippet: TSnippet);
    ///  <summary>Uses given font colour for subsequent text unless caller has
    ///  specified that colour is not to be used.</summary>
    ///  <remarks>Font colour is used until next call to this method.</remarks>
    procedure SetColour(const Colour: TColor);
    ///  <summary>Initialises RTF styles used when writing active text of
    ///  a snippet description.</summary>
    procedure InitStyles;
  public
    ///  <summary>Constructs and configures object.</summary>
    ///  <param name="UseColour">Boolean [in] Flag that whether document is
    ///  printed in colour (True) or black and white (False).</param>
    constructor Create(const UseColour: Boolean);
    ///  <summary>Destroys object.</summary>
    destructor Destroy; override;
    ///  <summary>Generates a document that lists contents of given category and
    ///  returns as encoded data using an encoding suitable for RTF.
    ///  </summary>
    function Generate(const Category: TCategory): TEncodedData;
  end;


implementation


uses
  // Project
  ActiveText.UMain, UColours, UPreferences;


{ TRTFCategoryDoc }

constructor TRTFCategoryDoc.Create(const UseColour: Boolean);
begin
  inherited Create;
  fUseColour := UseColour;
  fBuilder := TRTFBuilder.Create(0);  // use default code page for RTF document
  // Set up font table
  fBuilder.FontTable.Add(MainFontName, rgfSwiss, 0);
  fBuilder.FontTable.Add(MonoFontName, rgfModern, 0);
  // Set up colour table
  fBuilder.ColourTable.Add(Preferences.DBHeadingColours[False]);
  fBuilder.ColourTable.Add(Preferences.DBHeadingColours[True]);
  fBuilder.ColourTable.Add(clExternalLink);
  fDescStyles := TActiveTextRTFStyleMap.Create;
  InitStyles;
end;

destructor TRTFCategoryDoc.Destroy;
begin
  fDescStyles.Free;
  fBuilder.Free;
  inherited;
end;

function TRTFCategoryDoc.Generate(const Category: TCategory): TEncodedData;
var
  Snippet: TSnippet;
begin
  OutputCategoryHeading(Category);
  for Snippet in Category.Snippets do
  begin
    OutputSnippetSubHeading(Snippet);
    OutputSnippetText(Snippet);
  end;
  Result := TEncodedData.Create(fBuilder.Render.ToBytes, etASCII);
end;

procedure TRTFCategoryDoc.InitStyles;
begin
  fURLStyle := TRTFStyle.Create(
    [scColour], TRTFFont.CreateNull, 0.0, [], clExternalLink
  );
  fDescStyles.Add(
    ekPara, TRTFStyle.Create(TRTFParaSpacing.Create(ParaSpacing, 0.0))
  );
  fDescStyles.Add(
    ekHeading,
    TRTFStyle.Create(
      [scParaSpacing, scFontStyles],
      TRTFParaSpacing.Create(ParaSpacing, 0.0),
      TRTFFont.CreateNull,
      0.0,
      [fsUnderline],
      clNone
    )
  );
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
  if not fUseColour then
  begin
    fDescStyles.MakeMonochrome;
    fURLStyle.MakeMonochrome;
  end;
end;

procedure TRTFCategoryDoc.OutputCategoryHeading(const Category: TCategory);
begin
  fBuilder.BeginGroup;
  fBuilder.SetParaSpacing(TRTFParaSpacing.Create(HeadingSpacing, 0.0));
  fBuilder.SetFont(MainFontName);
  fBuilder.SetFontSize(HeadingFontSize);
  fBuilder.SetFontStyle([fsBold]);
  SetColour(Preferences.DBHeadingColours[Category.UserDefined]);
  fBuilder.AddText(Category.Description);
  fBuilder.EndPara;
  fBuilder.EndGroup;
end;

procedure TRTFCategoryDoc.OutputSnippetSubHeading(const Snippet: TSnippet);
begin
  fBuilder.BeginGroup;
  fBuilder.SetParaSpacing(TRTFParaSpacing.Create(SubHeadingSpacing, 0.0));
  fBuilder.SetFont(MainFontName);
  fBuilder.SetFontSize(SubHeadingFontSize);
  fBuilder.SetFontStyle([fsBold]);
  SetColour(Preferences.DBHeadingColours[Snippet.UserDefined]);
  fBuilder.AddText(Snippet.DisplayName);
  fBuilder.EndPara;
  fBuilder.EndGroup;
end;

procedure TRTFCategoryDoc.OutputSnippetText(const Snippet: TSnippet);
var
  RTFWriter: TActiveTextRTF;  // Object that generates RTF from active text
begin
  fBuilder.BeginGroup;
  fBuilder.SetParaSpacing(TRTFParaSpacing.Create(ParaSpacing, 0.0));
  fBuilder.SetFont(MainFontName);
  fBuilder.SetFontSize(ParaFontSize);
  fBuilder.SetFontStyle([]);
  RTFWriter := TActiveTextRTF.Create;
  try
    RTFWriter.ElemStyleMap := fDescStyles;
    RTFWriter.DisplayURLs := True;
    RTFWriter.URLStyle := fURLStyle;
    RTFWriter.Render(Snippet.Description, fBuilder);
  finally
    RTFWriter.Free;
  end;
  fBuilder.EndGroup;
end;

procedure TRTFCategoryDoc.SetColour(const Colour: TColor);
begin
  if fUseColour then
    fBuilder.SetColour(Colour);
end;

end.

