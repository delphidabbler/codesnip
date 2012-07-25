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
  UBaseObjects, UView;


type

  {
  TCopyViewMgr:
    Static abstract base class for objects that copy a representation of a view
    to the clipboard.
  }
  TCopyViewMgr = class abstract(TNoConstructObject)
  strict protected
    class function GeneratePlainText(const View: TViewItem): string;
      virtual; abstract;
      {Generates rich text representation of a view that is to be copied to
      clipboard.
        @param View [in] View to be represented in plain text.
      }
    class function GenerateRichText(const View: TViewItem): string;
      virtual; abstract;
      {Generates rich text representation of a view that is to be copied to
      clipboard.
        @param View [in] View to be represented in rich text.
      }
  public
    class function CanHandleView(const View: TViewItem): Boolean;
      virtual; abstract;
      {Checks if view can be copied to clipboard.
        @param View [in] View to be checked.
        @return True if view can be copied, False otherwise.
      }
    class procedure Execute(const View: TViewItem);
      {Copies the information about the view to the clipboard. Information is
      copied in both plain text and rich text formats.
        @param View [in] View to be copied. Must be supported by concrete
          subclass.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UClipboardHelper;


{ TCopyViewMgr }

class procedure TCopyViewMgr.Execute(const View: TViewItem);
  {Copies the information about the view to the clipboard.
    @param View [in] View to be copied. Must be supported by concrete subclass.
  }
var
  Clip: TClipboardHelper;   // object used to update clipboard
  PlainText: string;        // plain text representation of snippet
  RTF: string;              // rich text representation of snippet
begin
  Assert(Assigned(View), ClassName + '.Execute: View is nil');
  Assert(CanHandleView(View), ClassName + '.Execute: View not supported');
  // Generate plain text and rich text representation of snipper
  PlainText := GeneratePlainText(View);
  RTF := GenerateRichText(View);
  // Open clipboard and add both plain and rich text representations of snippet
  Clip := TClipboardHelper.Create;
  try
    Clip.Open;                                 
    try
      Clip.Add(CF_UNICODETEXT, PlainText);
      Clip.Add(CF_RTF, BytesOf(RTF));         // convert RTF to default encoding
    finally
      Clip.Close;
    end;
  finally
    FreeAndNil(Clip);
  end;
end;

end.

