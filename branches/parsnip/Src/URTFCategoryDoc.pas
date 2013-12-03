{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
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
  CS.ActiveText.Renderers.RTF,
  CS.Database.Types,
  DB.USnippet,
  UEncodings,
  URTFBuilder,
  URTFStyles;


type
  ///  <summary>Renders a rich text document that lists the snippets associated
  ///  with a tag, or those snippets that have no tag.</summary>
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
    ///  <summary>Outputs description of tag as a main heading as RTF.</summary>
    ///  <remarks>If the tag is null then a special "no tags" heading is output.
    ///  </remarks>
    procedure OutputTagHeading(const Tag: TTag);
    ///  <summary>Outputs name of given snippet as sub-heading as RTF.</summary>
    procedure OutputSnippetSubHeading(const Snippet: TSnippet);
    ///  <summary>Outputs description of given snippet as RTF paragraph.
    ///  </summary>
    procedure OutputSnippetText(const Snippet: TSnippet);
    ///  <summary>Output given text as RTF paragraph.</summary>
    procedure OutputPlainTextPara(const Text: string);
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
    ///  <summary>Generates a document that lists snippets associated with given
    ///  tag and returns as encoded data using an encoding suitable for RTF.
    ///  </summary>
    function Generate(const Tag: TTag): TEncodedData;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  CS.ActiveText,
  DB.UMain,
  UColours,
  UPreferences,
  UStrUtils;


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

function TRTFCategoryDoc.Generate(const Tag: TTag): TEncodedData;
var
  Snippet: TSnippet;
  SnippetID: TSnippetID;
  SnippetIDs: ISnippetIDList;
resourcestring
  sEmptySnippetPara = 'There are no snippets with this tag.';
  sEmptySnippetNullPara = 'All snippets have at least one tag.';
begin
  // TODO: move following selection code into a TDatabase method
  { TODO: change this to print only the currently selected snippets in the tag:
          this will be a change from v4. }
  if not Tag.IsNull then
    SnippetIDs := _Database.Select(
      function (const Snippet: TSnippet): Boolean
      begin
        Result := Snippet.Tags.Contains(Tag)
      end
    )
  else
    SnippetIDs := _Database.Select(
      function (const Snippet: TSnippet): Boolean
      begin
        Result := Snippet.Tags.IsEmpty;
      end
    );
  OutputTagHeading(Tag);
  if not SnippetIDs.IsEmpty then
  begin
    for SnippetID in SnippetIDs do
    begin
      Snippet := _Database.Lookup(SnippetID);
      OutputSnippetSubHeading(Snippet);
      OutputSnippetText(Snippet);
    end;
  end
  else
    OutputPlainTextPara(
      StrIf(Tag.IsNull, sEmptySnippetNullPara, sEmptySnippetPara)
    );
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

procedure TRTFCategoryDoc.OutputPlainTextPara(const Text: string);
begin
  fBuilder.BeginGroup;
  fBuilder.SetParaSpacing(TRTFParaSpacing.Create(ParaSpacing, 0.0));
  fBuilder.SetFont(MainFontName);
  fBuilder.SetFontSize(ParaFontSize);
  fBuilder.SetFontStyle([]);
  fBuilder.AddText(Text);
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
  // TODO: reconsider following code in light of synch spaces
  SetColour(Preferences.DBHeadingColours[True]);
  fBuilder.AddText(Snippet.Title);
  fBuilder.EndPara;
  fBuilder.EndGroup;
end;

procedure TRTFCategoryDoc.OutputSnippetText(const Snippet: TSnippet);
var
  RTFRenderer: TActiveTextRTFRenderer;
begin
  fBuilder.BeginGroup;
  fBuilder.SetParaSpacing(TRTFParaSpacing.Create(ParaSpacing, 0.0));
  fBuilder.SetFont(MainFontName);
  fBuilder.SetFontSize(ParaFontSize);
  fBuilder.SetFontStyle([]);
  // RTFRenderer is freed automatically since it is reference counted and an
  // interface reference is passed to Snippet.Description.Render()
  RTFRenderer := TActiveTextRTFRenderer.Create(fBuilder);
  RTFRenderer.Options.ElemStyleMap := fDescStyles;
  RTFRenderer.Options.DisplayURLs := True;
  RTFRenderer.Options.URLStyle := fURLStyle;
  Snippet.Description.Render(RTFRenderer);
  fBuilder.EndGroup;
end;

procedure TRTFCategoryDoc.OutputTagHeading(const Tag: TTag);
resourcestring
  sNormalHeading = 'Snippets With "%s" Tag';
  sNoTagsHeading = 'Snippets That Have No Tags';
begin
  fBuilder.BeginGroup;
  fBuilder.SetParaSpacing(TRTFParaSpacing.Create(HeadingSpacing, 0.0));
  fBuilder.SetFont(MainFontName);
  fBuilder.SetFontSize(HeadingFontSize);
  fBuilder.SetFontStyle([fsBold]);
  if Tag.IsNull then
    fBuilder.AddText(sNoTagsHeading)
  else
    fBuilder.AddText(Format(sNormalHeading, [Tag.ToString]));
  fBuilder.EndPara;
  fBuilder.EndGroup;
end;

procedure TRTFCategoryDoc.SetColour(const Colour: TColor);
begin
  if fUseColour then
    fBuilder.SetColour(Colour);
end;

end.

