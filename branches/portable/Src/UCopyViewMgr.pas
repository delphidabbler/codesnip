{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements an abstract base class for objects that copy a representation of a
 * view to the clipboard.
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

