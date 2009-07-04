{
 * UCopySnippetMgr.pas
 *
 * Implements class that manages creation and copying of a code snippet to the
 * clipboard.
 *
 * v0.1 of 06 Jan 2006  - Original version.
 * v1.0 of 24 May 2006  - Removed unused unit reference.
 * v1.1 of 29 Oct 2006  - Changed to use renamed IPreferences properties.
 * v1.2 of 03 Oct 2008  - Changed to override new DoExecute method of base class
 *                        instead of Execute method.
 *                      - Made protected section strict.
 * v1.3 of 03 Jan 2009  - Revised to use UClipboardHelper unit instead of
 *                        ClipBrd.
 * v1.4 of 16 Jan 2009  - Nows copies snippet to clipboard as syntax highlighted
 *                        rich text in addition to plain text.
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
 * The Original Code is UCopySnippetMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UCopySnippetMgr;


interface


uses
  // Project
  USnippetMgr;


type

  {
  TCopySnippetMgr:
    Manages creation and copying of a code snippet to the clipboard.
  }
  TCopySnippetMgr = class(TSnippetMgr)
  strict private
    function GenerateRTF(const Source: string): string;
      {Generates a syntax highlighted version of source code in rich text
      format.
        @param Source [in] Raw source code to be highlighted.
        @return Required RTF code.
      }
  strict protected
    procedure DoExecute; override;
      {Copies required snippet to clipboard.
      }
  end;


implementation


uses
  // Project
  IntfHiliter, UClipboardHelper, UHiliteAttrs, USyntaxHiliters, UPreferences;


{ TCopySnippetMgr }

procedure TCopySnippetMgr.DoExecute;
  {Copies required snippet to clipboard.
  }
var
  Source: string; // raw source code of snippet
begin
  Source := SourceCode(Preferences.SourceCommentStyle);
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

function TCopySnippetMgr.GenerateRTF(const Source: string): string;
  {Generates a syntax highlighted version of source code in rich text format.
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

