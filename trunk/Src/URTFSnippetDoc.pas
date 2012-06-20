{
 * URTFSnippetDoc.pas
 *
 * Implements a class that renders a document that describes a snippet as rich
 * text.
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
 * The Original Code is URTFSnippetDoc.pas, formerly URTFRoutineDoc.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit URTFSnippetDoc;


interface


uses
  // Delphi
  Graphics,
  // Project
  Hiliter.UGlobals, UActiveText, UActiveTextRTF, UEncodings, UIStringList,
  USnippetDoc, URTFBuilder, URTFStyles, URTFUtils;


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

      fExtraStyles: TRTFStyleMap;
      fExtraURLStyle: TRTFStyle;
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
    ///  <summary>Adds given heading (i.e. snippet name) to document.</summary>
    procedure RenderHeading(const Heading: string); override;
    ///  <summary>Adds given snippet description to document.</summary>
    procedure RenderDescription(const Desc: string); override;
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
  SysUtils,
  // Project
  Hiliter.UHiliters, UColours, UConsts;


{ TRTFSnippetDoc }

constructor TRTFSnippetDoc.Create(const HiliteAttrs: IHiliteAttrs;
  const UseColour: Boolean = True);
begin
  inherited Create;
  fHiliteAttrs := HiliteAttrs;
  fUseColour := UseColour;
  fExtraStyles := TRTFStyleMap.Create;
  InitStyles;
end;

destructor TRTFSnippetDoc.Destroy;
begin
  fExtraStyles.Free;
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
  fBuilder.FontTable.Add(MonoFontName, rgfModern, 0);
  // set up colour table
  fBuilder.ColourTable.Add(clWarningText);
  fBuilder.ColourTable.Add(clVarText);
  fBuilder.ColourTable.Add(clLinkText);
end;

procedure TRTFSnippetDoc.InitStyles;

  // Removes colour capability from style if colour not enabled.
  function AdjustCaps(const Caps: TRTFStyleCaps): TRTFStyleCaps;
  begin
    Result := Caps;
    if not fUseColour then
      Exclude(Result, scColour);
  end;

begin
  fExtraURLStyle := TRTFStyle.Create(
    AdjustCaps([scColour]), TRTFFont.CreateNull, 0.0, [], clLinkText
  );
  fExtraStyles.Add(
     ekPara,
     TRTFStyle.Create(
       TRTFParaSpacing.Create(ParaSpacing, 0.0)
     )
  );
  fExtraStyles.Add(
    ekHeading,
    TRTFStyle.Create(
      AdjustCaps([scParaSpacing, scFontStyles]),
      TRTFParaSpacing.Create(ParaSpacing, 0.0),
      TRTFFont.CreateNull,
      0.0,
      [fsBold],
      clNone
    )
  );
  fExtraStyles.Add(
    ekStrong,
    TRTFStyle.Create(
      AdjustCaps([scFontStyles]),
      TRTFFont.CreateNull,
      0.0,
      [fsBold],
      clNone
    )
  );
  fExtraStyles.Add(
    ekEm,
    TRTFStyle.Create(
      AdjustCaps([scFontStyles]),
      TRTFFont.CreateNull,
      0.0,
      [fsItalic],
      clNone
    )
  );
  fExtraStyles.Add(
    ekVar,
    TRTFStyle.Create(
      AdjustCaps([scFontStyles, scColour]),
      TRTFFont.CreateNull,
      0.0,
      [fsItalic],
      clVarText
    )
  );
  fExtraStyles.Add(
    ekWarning,
    TRTFStyle.Create(
      AdjustCaps([scFontStyles, scColour]),
      TRTFFont.CreateNull,
      0.0,
      [fsBold],
      clWarningText
    )
  );
  fExtraStyles.Add(
    ekMono,
    TRTFStyle.Create(
      AdjustCaps([scFont]),
      TRTFFont.Create(MonoFontName, rgfModern),
      0.0,
      [],
      clNone
    )
  );
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

procedure TRTFSnippetDoc.RenderDescription(const Desc: string);
begin
  fBuilder.ResetCharStyle;
  fBuilder.SetParaSpacing(TRTFParaSpacing.Create(ParaSpacing, ParaSpacing));
  fBuilder.SetFontStyle([]);
  fBuilder.SetFontSize(ParaFontSize);
  fBuilder.SetColour(clNone);
  fBuilder.AddText(Desc);
  fBuilder.EndPara;
end;

procedure TRTFSnippetDoc.RenderExtra(const ExtraText: IActiveText);
var
  RTFWriter: TActiveTextRTF;  // Object that generates RTF from active text
begin
  Assert(not ExtraText.IsEmpty, ClassName + '.RenderExtra: ExtraText is empty');
  RTFWriter := TActiveTextRTF.Create;
  try
    RTFWriter.ElemStyleMap := fExtraStyles;
    RTFWriter.DisplayURLs := True;
    RTFWriter.URLStyle := fExtraURLStyle;
    RTFWriter.Render(ExtraText, fBuilder);
  finally
    RTFWriter.Free;
  end;
end;

procedure TRTFSnippetDoc.RenderHeading(const Heading: string);
begin
  fBuilder.SetFontStyle([fsBold]);
  fBuilder.SetFontSize(HeadingFontSize);
  fBuilder.AddText(Heading);
  fBuilder.EndPara;
end;

procedure TRTFSnippetDoc.RenderSourceCode(const SourceCode: string);
var
  Renderer: IHiliteRenderer;  // renders highlighted source as RTF
begin
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

