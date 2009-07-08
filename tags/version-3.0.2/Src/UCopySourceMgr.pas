{
 * UCopySourceMgr.pas
 *
 * Implements class that manages copying of a snippet's source code to the
 * clipboard. Code is copied as plain text and as rich text. Rich text version
 * is syntax highlighted.
 *
 * v1.0 of 06 Jun 2009  - Original version
 *
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
 * The Original Code is UCopySourceMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UCopySourceMgr;


interface


uses
  // Project
  UBaseObjects, UView;


type
  {
  TCopySourceMgr:
    Static class that manages copying of a snippet's source code to clipboard as
    plain text and syntax highlighted rich text.
  }
  TCopySourceMgr = class(TNoConstructObject)
  strict private
    class function GenerateRTF(const Source: string): string;
      {Generates a syntax highlighted version of source code in rich text
      format.
        @param Source [in] Raw source code to be highlighted.
        @return Required RTF code.
      }
  public
    class function CanHandleView(const View: TViewItem): Boolean;
      {Checks if a view can be copied to clipboard.
        @param View [in] View to be checked.
        @return True if view is a snippet, False otherwise.
      }
    class procedure Execute(const View: TViewItem);
      {Copies source code of a snippet represented by a view to the clipboard.
        @param View [in] View that defines snippet whose source code to be
          copied. Must be supported snippet type.
      }
  end;


implementation


uses
  // Project
  IntfHiliter, UClipboardHelper, UHiliteAttrs, USyntaxHiliters;


{ TCopySourceMgr }

class function TCopySourceMgr.CanHandleView(const View: TViewItem): Boolean;
  {Checks if a view can be copied to clipboard.
    @param View [in] View to be checked.
    @return True if view is a snippet, False otherwise.
  }
begin
  Result := View.Kind = vkRoutine;
end;

class procedure TCopySourceMgr.Execute(const View: TViewItem);
  {Copies source code of a snippet represented by a view to the clipboard.
    @param View [in] View that defines snippet whose source code to be copied.
      Must be supported snippet type.
  }
var
  Source: string; // plain text source code
begin
  Assert(View.Kind = vkRoutine,
    ClassName + '.Execute: View kind must be vkRoutine');
  Source := View.Routine.SourceCode;
  with TClipboardHelper.Create do
    try
      Open;
      Add(CF_TEXT, Source);
      Add(CF_RTF, GenerateRTF(Source));
    finally
      Close;
      Free;
    end;
end;

class function TCopySourceMgr.GenerateRTF(const Source: string): string;
  {Generates a syntax highlighted version of source code in rich text
  format.
    @param Source [in] Raw source code to be highlighted.
    @return Required RTF code.
  }
var
  Hiliter: ISyntaxHiliter;  // object that performs highlighting
begin
  Hiliter := TSyntaxHiliterFactory.CreateHiliter(hkRTF);
  Result := Hiliter.Hilite(Source, THiliteAttrsFactory.CreateUserAttrs, '');
end;

end.
