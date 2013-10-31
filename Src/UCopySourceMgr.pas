{
 * UCopySourceMgr.pas
 *
 * Implements class that manages copying of a snippet's source code to the
 * clipboard. Code is copied as plain text and as rich text. Rich text version
 * is syntax highlighted.
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
 * The Original Code is UCopySourceMgr.pas
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


unit UCopySourceMgr;


interface


uses
  // Project
  UCopyViewMgr, UView;


type

  {
  TCopySourceCodeBase:
    Static abstract base class for objects that copy source code to the
    clipboard.
  }
  TCopySourceCodeBase = class abstract(TCopyViewMgr)
  strict protected
    class function GeneratePlainText(const View: TViewItem): string;
      override;
      {Generates a plain text document providing information about a snippet's
      source code.
        @param View [in] View representing snippet.
        @return Plain text document as a string.
      }
    class function GenerateRichText(const View: TViewItem): string;
      override;
      {Generates a RTF document providing information about a snippet's source
      code.
        @param View [in] View representing snippet.
        @return RTF document as a string.
      }
    class function GenerateSourceCode(const View: TViewItem): string;
      virtual; abstract;
      {Generates source code in required format.
        @param View [in] View for which source code is required.
        @return Source code as string.
      }
  public
    class function CanHandleView(const View: TViewItem): Boolean;
      override; abstract;
      {Checks if view can be copied to clipboard.
        @param View [in] View to be checked.
        @return True if view can be copied, False otherwise.
      }
  end;

  {
  TCopySourceMgr:
    Static class that manages copying of a snippet's source code to clipboard.
  }
  TCopySourceMgr = class sealed(TCopySourceCodeBase)
  strict protected
    class function GenerateSourceCode(const View: TViewItem): string; override;
      {Generates source code in required format.
        @param View [in] View for which source code is required.
        @return Snippet's source code as string.
      }
  public
    class function CanHandleView(const View: TViewItem): Boolean; override;
      {Checks if a view can be copied to clipboard.
        @param View [in] View to be checked.
        @return True if view is a snippet, False otherwise.
      }
  end;

  {
  TCopySnippetMgr:
    Statis class that manages creation and copying of one or more code snippets
    to the clipboard.
  }
  TCopySnippetMgr = class sealed(TCopySourceCodeBase)
  strict protected
    class function GenerateSourceCode(const View: TViewItem): string; override;
      {Generates source code in required format.
        @param View [in] View for which source code is required.
        @return Generated code snippet(s) as string.
      }
  public
    class function CanHandleView(const View: TViewItem): Boolean; override;
      {Checks if a view can be copied to clipboard.
        @param View [in] View to be checked.
        @return True if view contains code that can be output as a compilable
          snippet, False otherwise.
      }
  end;


implementation


uses
  // Project
  Hiliter.UAttrs, Hiliter.UGlobals, Hiliter.UHiliters, UPreferences,
  USnippetSourceGen;


{ TCopySourceCodeBase }

class function TCopySourceCodeBase.GeneratePlainText(
  const View: TViewItem): string;
  {Generates a plain text document providing information about a snippet's
  source code.
    @param View [in] View representing snippet.
    @return Plain text document as a string.
  }
begin
  Result := GenerateSourceCode(View);
end;

class function TCopySourceCodeBase.GenerateRichText(
  const View: TViewItem): string;
  {Generates a RTF document providing information about a snippet's source code.
    @param View [in] View representing snippet.
    @return RTF document as a string.
  }
var
  Hiliter: ISyntaxHiliter;  // object that performs highlighting
begin
  Hiliter := TSyntaxHiliterFactory.CreateHiliter(hkRTF);
  Result := Hiliter.Hilite(
    GenerateSourceCode(View), THiliteAttrsFactory.CreateUserAttrs, ''
  );
end;

{ TCopySourceMgr }

class function TCopySourceMgr.CanHandleView(const View: TViewItem): Boolean;
  {Checks if a view can be copied to clipboard.
    @param View [in] View to be checked.
    @return True if view is a snippet, False otherwise.
  }
begin
  Result := View.Kind = vkRoutine;
end;

class function TCopySourceMgr.GenerateSourceCode(const View: TViewItem): string;
  {Generates source code in required format.
    @param View [in] View for which source code is required.
    @return Snippet's source code as string.
  }
begin
  Result := View.Routine.SourceCode;
end;

{ TCopySnippetMgr }

class function TCopySnippetMgr.CanHandleView(const View: TViewItem): Boolean;
  {Checks if a view can be copied to clipboard.
    @param View [in] View to be checked.
    @return True if view contains code that can be output as a compilable
      snippet, False otherwise.
  }
begin
  Result := TSnippetSourceGen.CanGenerate(View);
end;

class function TCopySnippetMgr.GenerateSourceCode(
  const View: TViewItem): string;
  {Generates source code in required format.
    @param View [in] View for which source code is required.
    @return Generated code snippet(s) as string.
  }
begin
  Result := TSnippetSourceGen.Generate(View, Preferences.SourceCommentStyle);
end;

end.

