{
 * UTextRoutineDoc.pas
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
 * The Original Code is UTextRoutineDoc.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UTextRoutineDoc;


interface


uses
  // Project
  UActiveText, UIStringList, URoutineDoc, UStrStreamWriter;


type
  {
  TTextRoutineDoc:
    Class that renders a document that describes a snippet as plain text.
  }
  TTextRoutineDoc = class(TRoutineDoc)
  strict private
    var
      fWriter: TStrStreamWriter;  // Object used to write plain text to stream
    const
      cPageWidth = 80;    // width of output in characters
      cIndent = 2;        // size of indentation in characters
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
    procedure FinaliseDoc; override;
      {Releases stream writer object.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UUtils;


{ TTextRoutineDoc }

procedure TTextRoutineDoc.FinaliseDoc;
  {Releases stream writer object.
  }
begin
  FreeAndNil(fWriter);
end;

procedure TTextRoutineDoc.InitialiseDoc;
  {Create writer object for output stream.
  }
begin
  fWriter := TStrStreamWriter.Create(DocStream);
end;

procedure TTextRoutineDoc.RenderCompilerInfo(const Heading: string;
  const Info: TCompileDocInfoArray);
  {Writes details of compiler information to output stream.
    @param Heading [in] Heading for compiler information.
    @param Info [in] Array of compiler results (name and result as text).
  }
var
  Idx: Integer; // loops compiler information table
begin
  fWriter.WriteStrLn;
  fWriter.WriteStrLn(Heading);
  for Idx := Low(Info) to High(Info) do
    fWriter.WriteStrLn('%-20s%s', [Info[Idx].Compiler, Info[Idx].Result]);
end;

procedure TTextRoutineDoc.RenderDBInfo(const Text: string);
  {Writes information about code snippets database to output stream.
    @param Text [in] Text to be written.
  }
begin
  fWriter.WriteStrLn;
  fWriter.WriteStrLn(TextWrap(Text, cPageWidth, 0));
end;

procedure TTextRoutineDoc.RenderDescription(const Desc: string);
  {Writes snippet description to output stream.
    @param Desc [in] Description to be written.
  }
begin
  fWriter.WriteStrLn;
  fWriter.WriteStrLn(TextWrap(Desc, cPageWidth, 0));
end;

procedure TTextRoutineDoc.RenderExtra(const ExtraText: IActiveText);
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
              fWriter.WriteStrLn;
              fWriter.WriteStrLn(TextWrap(Text, cPageWidth, 0));
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
          Text := Text + Format(sURL, [ActionElem.Param]);
      end;
    end;
  end;
end;

procedure TTextRoutineDoc.RenderHeading(const Heading: string);
  {Writes heading (snippet name) to output stream.
    @param Heading [in] Heading to be written.
  }
begin
  fWriter.WriteStrLn(Heading);
end;

procedure TTextRoutineDoc.RenderSourceCode(const SourceCode: string);
  {Writes snippet's source code to output stream.
    @param SourceCode [in] Source code to be written.
  }
begin
  fWriter.WriteStrLn;
  fWriter.WriteStrLn(SourceCode);
  fWriter.WriteStrLn;
end;

procedure TTextRoutineDoc.RenderTitledList(const Title: string;
  List: IStringList);
  {Writes a comma-separated list preceded by a title to output stream.
    @param Title [in] List title.
    @param List [in] List of text to be written.
  }
begin
  RenderTitledText(Title, CommaList(List));
end;

procedure TTextRoutineDoc.RenderTitledText(const Title, Text: string);
  {Outputs text preceded by a title.
    @param Title [in] Text title.
    @param Text [in] Text to be written.
  }
begin
  fWriter.WriteStrLn(Title);
  fWriter.WriteStrLn(TextWrap(Text, cPageWidth - cIndent, cIndent));
end;

end.

