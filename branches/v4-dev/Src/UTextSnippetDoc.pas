{
 * UTextSnippetDoc.pas
 *
 * Implements a class that renders a document that describes a snippet as plain
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
 * The Original Code is UTextSnippetDoc.pas, formerly UTextRoutineDoc.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UTextSnippetDoc;


interface


uses
  // Delphi
  Classes,
  // Project
  UActiveText, UEncodings, UIStringList, USnippetDoc;


type
  {
  TTextSnippetDoc:
    Class that renders a document that describes a snippet as plain text.
  }
  TTextSnippetDoc = class(TSnippetDoc)
  strict private
    var fWriter: TStringWriter;   // Used to write plain text to stream
    const cPageWidth = 80;        // Width of output in characters
    const cIndent = 2;            // Size of indentation in characters
  strict protected
    procedure InitialiseDoc; override;
      {Create writer object for output stream.
      }
    procedure RenderHeading(const Heading: string); override;
      {Writes heading (snippet name) to output stream.
        @param Heading [in] Heading to be written.
      }
    procedure RenderDescription(const Desc: string); override;
      {Writes snippet description to output stream.
        @param Desc [in] Description to be written.
      }
    procedure RenderSourceCode(const SourceCode: string); override;
      {Writes snippet's source code to output stream.
        @param SourceCode [in] Source code to be written.
      }
    procedure RenderTitledText(const Title, Text: string); override;
      {Outputs text preceded by a title.
        @param Title [in] Text title.
        @param Text [in] Text to be written.
      }
    procedure RenderTitledList(const Title: string; List: IStringList);
      override;
      {Writes a comma-separated list preceded by a title to output stream.
        @param Title [in] List title.
        @param List [in] List of text to be written.
      }
    procedure RenderCompilerInfo(const Heading: string;
      const Info: TCompileDocInfoArray); override;
      {Writes details of compiler information to output stream.
        @param Heading [in] Heading for compiler information.
        @param Info [in] Array of compiler results (name and result as text).
      }
    procedure RenderExtra(const ExtraText: IActiveText); override;
      {Writes snippet's extra information to output stream.
        @param ExtraText [in] Text to be written.
      }                 
    procedure RenderDBInfo(const Text: string); override;
      {Writes information about code snippets database to output stream.
        @param Text [in] Text to be written.
      }
    function FinaliseDoc: TEncodedData; override;
      {Renders text document as Unicode encoded data. Releases write object.
        @return Unicode encoded text document.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UStrUtils, UUtils;


{ TTextSnippetDoc }

function TTextSnippetDoc.FinaliseDoc: TEncodedData;
  {Renders text document as Unicode encoded data. Releases write object.
    @return Unicode encoded text document.
  }
begin
  Result := TEncodedData.Create(fWriter.ToString, etUnicode);
  fWriter.Free;
end;

procedure TTextSnippetDoc.InitialiseDoc;
  {Create writer object to build up text.
  }
begin
  fWriter := TStringWriter.Create;
end;

procedure TTextSnippetDoc.RenderCompilerInfo(const Heading: string;
  const Info: TCompileDocInfoArray);
  {Writes details of compiler information to output stream.
    @param Heading [in] Heading for compiler information.
    @param Info [in] Array of compiler results (name and result as text).
  }
var
  Idx: Integer; // loops compiler information table
begin
  fWriter.WriteLine;
  fWriter.WriteLine(Heading);
  for Idx := Low(Info) to High(Info) do
    fWriter.WriteLine('%-20s%s', [Info[Idx].Compiler, Info[Idx].Result]);
end;

procedure TTextSnippetDoc.RenderDBInfo(const Text: string);
  {Writes information about code snippets database to output stream.
    @param Text [in] Text to be written.
  }
begin
  fWriter.WriteLine;
  fWriter.WriteLine(StrWrap(Text, cPageWidth, 0));
end;

procedure TTextSnippetDoc.RenderDescription(const Desc: string);
  {Writes snippet description to output stream.
    @param Desc [in] Description to be written.
  }
begin
  fWriter.WriteLine;
  fWriter.WriteLine(StrWrap(Desc, cPageWidth, 0));
end;

procedure TTextSnippetDoc.RenderExtra(const ExtraText: IActiveText);
  {Writes snippet's extra information to output stream.
    @param ExtraText [in] Text to be written.
  }
var
  Elem: IActiveTextElem;              // each active text element
  ActionElem: IActiveTextActionElem;  // refers to active text action elements
  Text: string;                       // text to be output
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
  Text := '';
  InBlock := False;
  for Elem in ExtraText do
  begin
    case Elem.Kind of
      ekText:
        // text element: record it if in block, ignore if not
        if InBlock then
          Text := Text + (Elem as IActiveTextTextElem).Text;
      ekPara, ekHeading:
      begin
        // paragraphs and headings start new lines, and are separated by
        GetIntf(Elem, IActiveTextActionElem, ActionElem);
        case ActionElem.State of
          fsOpen:
          begin
            // open block: reset text - writing deferred until end of block
            Text := '';
            InBlock := True;
          end;
          fsClose:
          begin
            // close block: emit blank line then write any pending text only if
            // there is any text
            // note that we are guaranteed that a block close will be last thing
            // in active text, so all text will be flushed
            if Text <> '' then
            begin
              fWriter.WriteLine;
              fWriter.WriteLine(StrWrap(Text, cPageWidth, 0));
              Text := '';
              InBlock := False;
            end;
          end;
        end;
      end;
      ekLink:
      begin
        // hyperlink element: output in brackets only if it's a closing element
        GetIntf(Elem, IActiveTextActionElem, ActionElem);
        if InBlock and (ActionElem.State = fsClose) then
          Text := Text
            + Format(sURL, [ActionElem.Attrs[TActiveTextAttrNames.Link_URL]]);
      end;
    end;
  end;
end;

procedure TTextSnippetDoc.RenderHeading(const Heading: string);
  {Writes heading (snippet name) to output stream.
    @param Heading [in] Heading to be written.
  }
begin
  fWriter.WriteLine(Heading);
end;

procedure TTextSnippetDoc.RenderSourceCode(const SourceCode: string);
  {Writes snippet's source code to output stream.
    @param SourceCode [in] Source code to be written.
  }
begin
  fWriter.WriteLine;
  fWriter.WriteLine(SourceCode);
  fWriter.WriteLine;
end;

procedure TTextSnippetDoc.RenderTitledList(const Title: string;
  List: IStringList);
  {Writes a comma-separated list preceded by a title to output stream.
    @param Title [in] List title.
    @param List [in] List of text to be written.
  }
begin
  RenderTitledText(Title, CommaList(List));
end;

procedure TTextSnippetDoc.RenderTitledText(const Title, Text: string);
  {Outputs text preceded by a title.
    @param Title [in] Text title.
    @param Text [in] Text to be written.
  }
begin
  fWriter.WriteLine(Title);
  fWriter.WriteLine(StrWrap(Text, cPageWidth - cIndent, cIndent));
end;

end.

