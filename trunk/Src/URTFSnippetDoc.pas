{
 * URTFSnippetDoc.pas
 *
 * Implements a class that renders a document that describes a snippet as rich
 * text. Source code highlighting can be customised.
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
 * Portions created by the Initial Developer are Copyright (C) 2008-2011 Peter
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
  Hiliter.UGlobals, UActiveText, UEncodings, UIStringList, USnippetDoc,
  URTFBuilder, URTFUtils;


type

  {
  TRTFSnippetDoc:
    Class that renders a document that describes a snippet as rich text.
  }
  TRTFSnippetDoc = class(TSnippetDoc)
  strict private
    var
      fHiliteAttrs: IHiliteAttrs; // Attributes determine source code formatting
      fBuilder: TRTFBuilder;      // Object used to build rich text document
      fUseColour: Boolean;        // Flag indicates whether to output in colour
    const
      cMainFontName = 'Tahoma';                     // main font name
      cMonoFontName = 'Courier New';                // mono font name
      cHeadingFontSize = 16;                        // heading font size
      cParaFontSize = 10;                           // paragraph font size
      cParaSpacing = 12;                            // paragraph spacing
      cDBInfoFontSize = 9;                          // codesnip db font size
    procedure SetColour(const Colour: TColor);
      {Sets specified font colour in RTF, unless user specifies that colour is
      not to be used.
        @param Colour [in] Required colour. Ignored if fUseColour is false.
      }
  strict protected
    procedure InitialiseDoc; override;
      {Initialises rich text document.
      }
    procedure RenderHeading(const Heading: string); override;
      {Adds heading (snippet name) to rich text document.
        @param Heading [in] Heading to be written.
      }
    procedure RenderDescription(const Desc: string); override;
      {Adds snippet's description to rich text document.
        @param Desc [in] Description to be written.
      }
    procedure RenderSourceCode(const SourceCode: string); override;
      {Adds a placeholder for snippet's source code to rich text document.
      Placeholder is replaced later when source code is generated.
        @param SourceCode [in] Source code to be written.
      }
    procedure RenderTitledText(const Title, Text: string); override;
      {Outputs text preceded by a title.
        @param Title [in] Text title.
        @param Text [in] Text to be written.
      }
    procedure RenderTitledList(const Title: string; List: IStringList);
      override;
      {Adds a comma-separated list preceded by a title to rich text document.
        @param Title [in] List title.
        @param List [in] List of text to be written.
      }
    procedure RenderCompilerInfo(const Heading: string;
      const Info: TCompileDocInfoArray); override;
      {Adds details of compiler information to rich edit document.
        @param Heading [in] Heading for compiler information.
        @param Info [in] Array of compiler results (name and result as text).
      }
    procedure RenderExtra(const ExtraText: IActiveText); override;
      {Adds snippet's extra information to rich text document.
        @param ExtraText [in] Text to be written.
      }
    procedure RenderDBInfo(const Text: string); override;
      {Adds information about code snippets database to rich text document.
        @param Text [in] Text to be written.
      }
    function FinaliseDoc: TEncodedData; override;
      {Merges hilited source code document into main document and returns final
      document. Releases RTF builder object.
        @return ASCII encoded RTF document.
      }
  public
    constructor Create(const HiliteAttrs: IHiliteAttrs;
      const UseColour: Boolean = True);
      {Class constructor. Sets up object with customised source code
      highlighting.
        @param HiliteAttrs [in] Defines source code syntax highlighting.
        @param UseColour [in] Determines if output is to be rendered in colour.
      }
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
  {Class constructor. Sets up object with customised source code highlighting.
    @param HiliteAttrs [in] Defines source code syntax highlighting.
    @param UseColour [in] Determines if output is to be rendered in colour.
  }
begin
  inherited Create;
  fHiliteAttrs := HiliteAttrs;
  fUseColour := UseColour;
end;

function TRTFSnippetDoc.FinaliseDoc: TEncodedData;
  {Merges hilited source code document into main document and returns final
  document. Releases RTF builder object.
    @return ASCII encoded RTF document.
  }
begin
  Result := TEncodedData.Create(fBuilder.Render.ToBytes, etASCII);
  fBuilder.Free;
end;

procedure TRTFSnippetDoc.InitialiseDoc;
  {Initialises rich text document.
  }
begin
  // Create object used to build main rich text document
  fBuilder := TRTFBuilder.Create(0);  // Use default code page
  // Set up font table
  fBuilder.FontTable.Add(cMainFontName, rgfSwiss, 0);
  fBuilder.FontTable.Add(cMonoFontName, rgfModern, 0);
  // set up colour table
  fBuilder.ColourTable.Add(clWarningText);
  fBuilder.ColourTable.Add(clVarText);
  fBuilder.ColourTable.Add(clLinkText);
end;

procedure TRTFSnippetDoc.RenderCompilerInfo(const Heading: string;
  const Info: TCompileDocInfoArray);
  {Adds details of compiler information to rich edit document.
    @param Heading [in] Heading for compiler information.
    @param Info [in] Array of compiler results (name and result as text).
  }
var
  Idx: Integer; // loops compiler information table
begin
  fBuilder.SetFontStyle([fsBold]);
  fBuilder.SetParaSpacing(cParaSpacing, cParaSpacing div 3);
  fBuilder.AddText(Heading);
  fBuilder.ResetCharStyle;
  fBuilder.EndPara;
  fBuilder.ClearParaFormatting;
  fBuilder.SetFontSize(cParaFontSize);
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
  {Adds information about code snippets database to rich text document.
    @param Text [in] Text to be written.
  }
begin
  fBuilder.SetParaSpacing(cParaSpacing, 0);
  fBuilder.SetFontSize(cDBInfoFontSize);
  fBuilder.SetFontStyle([fsItalic]);
  fBuilder.AddText(Text);
  fBuilder.EndPara;
  fBuilder.ClearParaFormatting;
  fBuilder.ResetCharStyle;
end;

procedure TRTFSnippetDoc.RenderDescription(const Desc: string);
  {Adds snippet's description to rich text document.
    @param Desc [in] Description to be written.
  }
begin
  fBuilder.ResetCharStyle;
  fBuilder.SetParaSpacing(cParaSpacing, cParaSpacing);
  fBuilder.SetFontStyle([]);
  fBuilder.SetFontSize(cParaFontSize);
  fBuilder.SetColour(clNone);
  fBuilder.AddText(Desc);
  fBuilder.EndPara;
end;

procedure TRTFSnippetDoc.RenderExtra(const ExtraText: IActiveText);
  {Adds snippet's extra information to rich text document.
    @param ExtraText [in] Text to be written.
  }
var
  Elem: IActiveTextElem;              // each active text element
  TextElem: IActiveTextTextElem;      // refers to active text text elements
  ActionElem: IActiveTextActionElem;  // refers to active text action elements
  InBlock: Boolean;                   // flag true if inside a block level tag
resourcestring
  sURL = ' (%s)';                     // formatting for URLs from hyperlinks
begin
  Assert(not ExtraText.IsEmpty, ClassName + '.RenderExtra: ExtraText is empty');
  Assert(Supports(ExtraText[0], IActiveTextActionElem) and
    ((ExtraText[0] as IActiveTextActionElem).DisplayStyle = dsBlock),
    ClassName + '.RenderExtra: ExtraText must begin with a block tag');
  Assert(Supports(ExtraText[Pred(ExtraText.Count)], IActiveTextActionElem) and
    ((ExtraText[Pred(ExtraText.Count)] as IActiveTextActionElem).DisplayStyle
      = dsBlock),
    ClassName + '.RenderExtra: ExtraText must end with a block tag');
  InBlock := False;
  for Elem in ExtraText do
  begin
    if Supports(Elem, IActiveTextTextElem, TextElem) then
    begin
      if InBlock then
        fBuilder.AddText(TextElem.Text);
    end
    else if Supports(Elem, IActiveTextActionElem, ActionElem) then
    begin
      case ActionElem.Kind of
        ekPara:
        begin
          // begin or end a paragraph
          case ActionElem.State of
            fsOpen:
            begin
              fBuilder.SetParaSpacing(cParaSpacing, 0);
              InBlock := True;
            end;
            fsClose:
            begin
              fBuilder.EndPara;
              InBlock := False;
            end;
          end;
        end;
        ekHeading:
        begin
          // begin or end a heading
          case ActionElem.State of
            fsOpen:
            begin
              fBuilder.SetParaSpacing(cParaSpacing, 0);
              fBuilder.BeginGroup;
              fBuilder.SetFontStyle([fsBold]);
              InBlock := True;
            end;
            fsClose:
            begin
              fBuilder.EndGroup;
              fBuilder.EndPara;
              InBlock := False;
            end;
          end;
        end;
        ekLink:
        begin
          // write a link (write url in brackets after text)
          if ActionElem.State = fsClose then
          begin
            fBuilder.BeginGroup;
            SetColour(clLinkText);
            fBuilder.AddText(
              Format(sURL, [ActionElem.Attrs[TActiveTextAttrNames.Link_URL]])
            );
            fBuilder.EndGroup;
          end;
        end;
        ekStrong:
        begin
          // begin or end strong emphasis
          case ActionElem.State of
            fsOpen:
            begin
              fBuilder.BeginGroup;
              fBuilder.SetFontStyle([fsBold]);
            end;
            fsClose:
              fBuilder.EndGroup;
          end;
        end;
        ekEm:
        begin
          // begin or end emphasis
          case ActionElem.State of
            fsOpen:
            begin
              fBuilder.BeginGroup;
              fBuilder.SetFontStyle([fsItalic]);
            end;
            fsClose:
              fBuilder.EndGroup;
          end;
        end;
        ekVar:
        begin
          // begin or end variable
          case ActionElem.State of
            fsOpen:
            begin
              fBuilder.BeginGroup;
              fBuilder.SetFontStyle([fsItalic]);
              SetColour(clVarText);
            end;
            fsClose:
              fBuilder.EndGroup;
          end;
        end;
        ekWarning:
        begin
          // begin or end warning text
          case ActionElem.State of
            fsOpen:
            begin
              fBuilder.BeginGroup;
              fBuilder.SetFontStyle([fsBold]);
              SetColour(clWarningText);
            end;
            fsClose:
              fBuilder.EndGroup;
          end;
        end;
        ekMono:
        begin
          // begin or end mono text
          case ActionElem.State of
            fsOpen:
            begin
              fBuilder.BeginGroup;
              fBuilder.SetFont(cMonoFontName);
            end;
            fsClose:
              fBuilder.EndGroup;
          end;
        end;
      end;
    end;
  end;
end;

procedure TRTFSnippetDoc.RenderHeading(const Heading: string);
  {Adds heading (snippet name) to rich text document.
    @param Heading [in] Heading to be written.
  }
begin
  fBuilder.SetFontStyle([fsBold]);
  fBuilder.SetFontSize(cHeadingFontSize);
  fBuilder.AddText(Heading);
  fBuilder.EndPara;
end;

procedure TRTFSnippetDoc.RenderSourceCode(const SourceCode: string);
  {Adds a placeholder for snippet's source code to rich text document.
  Placeholder is replaced later when source code is generated.
    @param SourceCode [in] Source code to be written.
  }
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
  {Adds a comma-separated list preceded by a title to rich text document.
    @param Title [in] List title.
    @param List [in] List of text to be written.
  }
begin
  RenderTitledText(Title, CommaList(List));
end;

procedure TRTFSnippetDoc.RenderTitledText(const Title, Text: string);
  {Outputs text preceded by a title.
    @param Title [in] Text title.
    @param Text [in] Text to be written.
  }
begin
  fBuilder.ClearParaFormatting;
  fBuilder.ResetCharStyle;
  fBuilder.SetFont(cMainFontName);
  fBuilder.SetFontSize(cParaFontSize);
  fBuilder.SetParaSpacing(cParaSpacing, 0);
  fBuilder.BeginGroup;
  fBuilder.SetFontStyle([fsBold]);
  fBuilder.AddText(Title);
  fBuilder.EndGroup;
  fBuilder.AddText(' ' + Text);
  fBuilder.EndPara;
end;

procedure TRTFSnippetDoc.SetColour(const Colour: TColor);
  {Sets specified font colour in RTF, unless user specifies that colour is not
  to be used.
    @param Colour [in] Required colour. Ignored if fUseColour is false.
  }
begin
  if fUseColour then
    fBuilder.SetColour(Colour);
end;

end.

