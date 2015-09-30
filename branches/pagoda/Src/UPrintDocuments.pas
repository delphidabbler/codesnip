{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Provides interface and classes that can generate output suitable for printing
 * using print engine.
}


unit UPrintDocuments;


interface


uses
  // Delphi
  Classes,
  // Project
  CS.Database.Types,
  URTFUtils;


type
  ///  <summary>Interface supported by classes that can generate a print
  ///  document suitable for processing by the print engine.</summary>
  ///  <remarks>The print engine prints documents rendered in rich text format.
  ///  </remarks>
  IPrintDocument = interface(IInterface)
    ['{56E4CA97-7F04-427A-A95F-03CE55910DC0}']
    ///  <summary>Generates and returns print document.</summary>
    function Generate: TRTF;
  end;

type
  ///  <summary>Class that generates a print document that describes a snippet.
  ///  </summary>
  TSnippetPrintDocument = class(TInterfacedObject,
    IPrintDocument
  )
  strict private
    var
      ///  <summary>Reference to snippet described by print document.</summary>
      fSnippet: ISnippet;
  public
    ///  <summary>Constructs object to create print document for given snippet.
    ///  </summary>
    constructor Create(Snippet: ISnippet);
    ///  <summary>Generates and returns print document.</summary>
    ///  <remarks>Method of IPrintDocument.</remarks>
    function Generate: TRTF;
  end;

type
  ///  <summary>Class that generates a print document that describes a tag and
  ///  the snippets it contains.</summary>
  TTagPrintDocument = class(TInterfacedObject,
    IPrintDocument
  )
  strict private
    var
      ///  <summary>Reference to tag described by print document.</summary>
      fTag: TTag;
  public
    ///  <summary>Constructs object to create print document for given tag.
    ///  </summary>
    constructor Create(const Tag: TTag);
    ///  <summary>Generates and returns print document.</summary>
    ///  <remarks>Method of IPrintDocument.</remarks>
    function Generate: TRTF;
  end;

implementation


uses
  // Project
  CS.Config,
  CS.Docs.TagInfo.RTF,
  CS.SourceCode.Languages,
  CS.SourceCode.Hiliter.Brushes,
  URTFSnippetDoc,
  UPreferences,
  UPrintInfo;


{ TSnippetPrintDocument }

constructor TSnippetPrintDocument.Create(Snippet: ISnippet);
begin
  inherited Create;
  fSnippet := Snippet;
end;

function TSnippetPrintDocument.Generate: TRTF;
var
  Doc: TRTFSnippetDoc;            // object that renders snippet document in RTF
  Language: TSourceCodeLanguage;  // programming language used by snippet
  Brush: TSyntaxHiliterBrush;     // brush used to syntax highlight snippet
begin
  Language := TConfig.Instance.SourceCodeLanguages[fSnippet.LanguageID];
  if (poSyntaxHilite in PrintInfo.PrintOptions) then
    Brush := TSyntaxHiliterBrushes.CreateBrush(Language.HiliterBrushID)
  else
    Brush := TSyntaxHiliterBrushes.CreateNullBrush;
  try
    Doc := TRTFSnippetDoc.Create(
      TConfig.Instance.HiliterThemes[
        Preferences.CurrentHiliteThemeIds[htkPrint]
      ],
      Brush,
      poUseColour in PrintInfo.PrintOptions
    );
    try
      Result := TRTF.Create(Doc.Generate(fSnippet));
    finally
      Doc.Free;
    end;
  finally
    Brush.Free;
  end;
end;

{ TTagPrintDocument }

constructor TTagPrintDocument.Create(const Tag: TTag);
begin
  inherited Create;
  fTag := Tag;
end;

function TTagPrintDocument.Generate: TRTF;
var
  Doc: TTagInfoRTFDoc; // object that renders tag document in RTF
begin
  Doc := TTagInfoRTFDoc.Create(poUseColour in PrintInfo.PrintOptions);
  try
    Result := TRTF.Create(Doc.Generate(fTag));
  finally
    Doc.Free;
  end;
end;

end.

