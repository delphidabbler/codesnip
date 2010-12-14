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
  // Project
  UHiddenRichEdit, URTFUtils;


type
  ///  <summary>
  ///  Class that can merge the RTF code of complete documents.
  ///  </summary>
  TRTFMerger = class(THiddenRichEdit)
  public
    ///  <summary>Object constructor. Sets up object with master RTF document
    ///  code into which other document code can be merged.</summary>
    constructor Create(const MasterRTF: TRTF);
    ///  <summary>Replaces first occurence of given placeholder text in master
    ///  document with given RTF code.</summary>
    ///  <remarks>If placeholder text can't be found no action is taken.
    ///  </remarks>
    procedure ReplacePlaceholder(const Placeholder: string;
      const RTF: TRTF);
    ///  <summary>Renders and returns merged document.</summary>
    function Render: TRTF;
  end;


implementation


{ TRTFMerger }

constructor TRTFMerger.Create(const MasterRTF: TRTF);
begin
  inherited Create;
  TRichEditHelper.Load(RichEdit, MasterRTF);
end;

function TRTFMerger.Render: TRTF;
begin
  Result := TRichEditHelper.Save(RichEdit);
end;

procedure TRTFMerger.ReplacePlaceholder(const Placeholder: string;
  const RTF: TRTF);
begin
  TRichEditHelper.Insert(RichEdit, RTF, Placeholder);
end;

end.

