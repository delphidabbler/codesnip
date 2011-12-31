{
 * UCopyViewMgr.pas
 *
 * Implements an abstract base class for objects that copy a representation of a
 * view to the clipboard.
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
 * The Original Code is UCopyViewMgr.pas
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


unit UCopyViewMgr;


interface


uses
  // Project
  UBaseObjects, UEncodings, UView;


type

  ///  <summary>
  ///  Static abstract base class for objects that copy a representation of a
  ///  view to the clipboard in both plain (Unicode) text format and rich text
  ///  format.
  ///  </summary>
  TCopyViewMgr = class abstract(TNoConstructObject)
  strict protected
    ///  <summary>Returns encoded data containing a Unicode plain text
    ///  representation of the given view that is to be copied to the clipboard.
    ///  </summary>
    class function GeneratePlainText(View: IView): TEncodedData;
      virtual; abstract;
    ///  <summary>Returns encoded data containing a RTF representation of the
    ///  given view that is to be copied to the clipboard.</summary>
    class function GenerateRichText(View: IView): TEncodedData;
      virtual; abstract;
  public
    ///  <summary>Checks if a given view can be copied to the clipboard.
    ///  </summary>
    class function CanHandleView(View: IView): Boolean;
      virtual; abstract;
    ///  <summary>Copies information about a given view to the clipboard in both
    ///  plain (Unicode) text and rich text formats.</summary>
    ///  <remarks>The view must be one that is supported by the concrete
    ///  subclass.</remarks>
    class procedure Execute(View: IView);
  end;


implementation


uses
  // Project
  UClipboardHelper, URTFUtils;


{ TCopyViewMgr }

class procedure TCopyViewMgr.Execute(View: IView);
var
  Clip: TClipboardHelper;     // object used to update clipboard
  UnicodeText: UnicodeString; // Unicode plain text representation of view
  RTF: TRTF;                  // rich text representation of view
begin
  Assert(Assigned(View), ClassName + '.Execute: View is nil');
  Assert(CanHandleView(View), ClassName + '.Execute: View not supported');
  // Generate plain text and rich text representation of view
  UnicodeText := GeneratePlainText(View).ToString;
  RTF := TRTF.Create(GenerateRichText(View));
  // Open clipboard and add both plain and rich text representations of snippet
  Clip := TClipboardHelper.Create;
  try
    Clip.Open;
    try
      Clip.AddUnicodeText(UnicodeText);
      Clip.AddRTF(RTF.ToRTFCode);
    finally
      Clip.Close;
    end;
  finally
    Clip.Free;
  end;
end;

end.

