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
 * Portions created by the Initial Developer are Copyright (C) 2009-2012 Peter
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
  ///  <summary>Renders a document that describes a snippet as plain text.
  ///  </summary>
  TTextSnippetDoc = class(TSnippetDoc)
  strict private
    var
      ///  <summary>Object used to build plain text document.</summary>
      fWriter: TStringWriter;
    const
      ///  <summary>Width of output in characters.</summary>
      cPageWidth = 80;
      ///  <summary>Size of a single level of indentation in characters.
      ///  </summary>
      cIndent = 2;
  strict protected
    ///  <summary>Initialises plain text document.</summary>
    procedure InitialiseDoc; override;
    ///  <summary>Adds given heading (i.e. snippet name) to document.</summary>
    procedure RenderHeading(const Heading: string); override;
    ///  <summary>Interprets and adds given snippet description to document.
    ///  </summary>
    ///  <remarks>Active text is converted to plain text with only block level
    ///  formatting observed.</remarks>
    procedure RenderDescription(const Desc: IActiveText); override;
    ///  <summary>Adds given source code to document.</summary>
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
    ///  <remarks>Active text is converted to plain text with only block level
    ///  formatting observed.</remarks>
    procedure RenderExtra(const ExtraText: IActiveText); override;
    ///  <summary>Adds given information about code snippets database to
    ///  document.</summary>
    procedure RenderDBInfo(const Text: string); override;
    ///  <summary>Finalises document and returns content as encoded data.
    ///  </summary>
    function FinaliseDoc: TEncodedData; override;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UStrUtils;


{ TTextSnippetDoc }

function TTextSnippetDoc.FinaliseDoc: TEncodedData;
begin
  Result := TEncodedData.Create(fWriter.ToString, etUnicode);
  fWriter.Free;
end;

procedure TTextSnippetDoc.InitialiseDoc;
begin
  fWriter := TStringWriter.Create;
end;

procedure TTextSnippetDoc.RenderCompilerInfo(const Heading: string;
  const Info: TCompileDocInfoArray);
var
  Idx: Integer; // loops compiler information table
begin
  fWriter.WriteLine;
  fWriter.WriteLine(Heading);
  for Idx := Low(Info) to High(Info) do
    fWriter.WriteLine('%-20s%s', [Info[Idx].Compiler, Info[Idx].Result]);
end;

procedure TTextSnippetDoc.RenderDBInfo(const Text: string);
begin
  fWriter.WriteLine;
  fWriter.WriteLine(StrWrap(Text, cPageWidth, 0));
end;

procedure TTextSnippetDoc.RenderDescription(const Desc: IActiveText);
begin
  fWriter.WriteLine;
  { TODO -cURGENT: Revise to be like RenderExtra: pull out common code to
         generic class like TActiveTextRTF }
  fWriter.WriteLine(StrWrap(StrTrim(Desc.ToString), cPageWidth, 0));
end;

procedure TTextSnippetDoc.RenderExtra(const ExtraText: IActiveText);
var
  Elem: IActiveTextElem;              // each active text element
  TextElem: IActiveTextTextElem;      // refers to active text text elements
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
    if Supports(Elem, IActiveTextTextElem, TextElem) then
    begin
      if InBlock then
        Text := Text + TextElem.Text;
    end
    else if Supports(Elem, IActiveTextActionElem, ActionElem) then
    begin
      case ActionElem.Kind of
        ekPara, ekHeading:
        begin
          // paragraphs and headings start new lines, and are separated by
          case ActionElem.State of
            fsOpen:
            begin
              // open block: reset text - writing deferred until end of block
              Text := '';
              InBlock := True;
            end;
            fsClose:
            begin
              // close block: emit blank line then write any pending text only
              // if there is any text
              // note that we are guaranteed that a block close will be last
              // thing in active text, so all text will be flushed
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
          // hyperlink element: output in brackets only if closing element
          if InBlock and (ActionElem.State = fsClose) then
            Text := Text
              + Format(sURL, [ActionElem.Attrs[TActiveTextAttrNames.Link_URL]]);
        end;
      end;
    end;
  end;
end;

procedure TTextSnippetDoc.RenderHeading(const Heading: string);
begin
  fWriter.WriteLine(Heading);
end;

procedure TTextSnippetDoc.RenderSourceCode(const SourceCode: string);
begin
  fWriter.WriteLine;
  fWriter.WriteLine(SourceCode);
  fWriter.WriteLine;
end;

procedure TTextSnippetDoc.RenderTitledList(const Title: string;
  List: IStringList);
begin
  RenderTitledText(Title, CommaList(List));
end;

procedure TTextSnippetDoc.RenderTitledText(const Title, Text: string);
begin
  fWriter.WriteLine(Title);
  fWriter.WriteLine(StrWrap(Text, cPageWidth - cIndent, cIndent));
end;

end.

