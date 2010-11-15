{
 * URTFMerger.pas
 *
 * Implements a class that can merge RTF documents.
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
 * The Original Code is URTFMerger.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit URTFMerger;


interface


uses
  // Delphi
  Classes,
  // Project
  UEncodings, UHiddenRichEdit;


type

  {
  TRTFMerger:
    Class that can merge RTF documents into a master document.
  }
  TRTFMerger = class(THiddenRichEdit)
  private
    procedure Merge(const RTF: ASCIIString);
      {Merges RTF code into master document at current position. Any selected
      text is deleted.
        @param RTF [in] RTF code to be merged in.
      }
    procedure SelectText(const Text: string);
      {Selects first occurence of text in rich edit control.
        @param Text [in] Text to be selected.
      }
  public
    constructor Create(const MasterRTF: ASCIIString);
      {Class constructor. Sets up object.
        @param MasterRTF [in] RTF code of document to receive merges.
      }
    procedure ReplacePlaceholder(const Placeholder: string;
      const RTF: ASCIIString);
      {Replaces placeholder text in rich edit with new content.
        @param Placeholder [in] Place holder text to be replaced.
        @param RTF [in] RTF code to replace place holder.
      }
    procedure SaveToStream(const Stream: TStream);
      {Saves merged document to stream.
        @param Stream [in] Stream that receives RTF code of document.
      }
  end;


implementation


uses
  // Project
  URTFUtils;


{ TRTFMerger }

constructor TRTFMerger.Create(const MasterRTF: ASCIIString);
  {Class constructor. Sets up object.
    @param MasterRTF [in] RTF code of document to receive merges.
  }
begin
  inherited Create;
  RTFLoadFromString(RichEdit, MasterRTF);
end;

procedure TRTFMerger.Merge(const RTF: ASCIIString);
  {Merges RTF code into master document at current position. Any selected text
  is deleted.
    @param RTF [in] RTF code to be merged in.
  }
begin
  RTFInsertString(RichEdit, RTF);
end;

procedure TRTFMerger.ReplacePlaceholder(const Placeholder: string;
  const RTF: ASCIIString);
  {Replaces placeholder text in rich edit with new content.
    @param Placeholder [in] Place holder text to be replaced.
    @param RTF [in] RTF code to replace place holder.
  }
begin
  SelectText(Placeholder);
  Merge(RTF);
end;

procedure TRTFMerger.SaveToStream(const Stream: TStream);
  {Saves merged document to stream.
    @param Stream [in] Stream that receives RTF code of document.
  }
begin
  RTFSaveToStream(RichEdit, Stream);
end;

procedure TRTFMerger.SelectText(const Text: string);
  {Selects first occurence of text in rich edit control.
    @param Text [in] Text to be selected.
  }
begin
  RichEdit.SelStart := RichEdit.FindText(Text, 0, MaxInt, []);
  RichEdit.SelLength := Length(Text);
end;

end.

