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
 * Portions created by the Initial Developer are Copyright (C) 2009-2011 Peter
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
  UCopyViewMgr, UEncodings, UView;


type
  ///  <summary>
  ///  Static abstract base class for objects that copy source code to the
  ///  clipboard.
  ///  </summary>
  TCopySourceCodeBase = class abstract(TCopyViewMgr)
  strict protected
    ///  <summary>Generates encoded data containing a Unicode plain text
    ///  document that provides information about the source code of the snippet
    ///  represented by the given view.</summary>
    class function GeneratePlainText(View: IView): TEncodedData; override;
    ///  <summary>Generates encoded data containing a RTF document that provides
    ///  information about the source code of the snippet represented by the
    ///  given view.</summary>
    class function GenerateRichText(View: IView): TEncodedData; override;
    ///  <summary>Generates source code for the snippet represented by the
    ///  given view. Source code is returned as a Unicode string.</summary>
    class function GenerateSourceCode(View: IView): string; virtual; abstract;
  public
    ///  <summary>Checks if a given view can be copied to the clipboard.
    ///  </summary>
    class function CanHandleView(View: IView): Boolean; override; abstract;
  end;

type
  ///  <summary>
  ///  Static class that manages copying of the raw source code of a single
  ///  snippet to the clipboard.
  ///  </summary>
  TCopySourceMgr = class sealed(TCopySourceCodeBase)
  strict protected
    ///  <summary>Returns the source code of the snippet represented by the
    ///  given view. Source code is returned as a Unicode string.</summary>
    class function GenerateSourceCode(View: IView): string; override;
  public
    ///  <summary>Checks if given view can be copied to the clipboard. Returns
    ///  True only if view represents a snippet.</summary>
    class function CanHandleView(View: IView): Boolean; override;
  end;

type
  ///  <summary>
  ///  Static class that manages creation and copying of annotated source code
  ///  of one or more code snippets to the clipboard.
  ///  </summary>
  TCopySnippetMgr = class sealed(TCopySourceCodeBase)
  strict protected
    ///  <summary>Returns an annotated code snippet generated from one or more
    ///  snippets represented by the given view. Source code is returned as a
    ///  Unicode string.</summary>
    class function GenerateSourceCode(View: IView): string; override;
  public
    ///  <summary>Checks if given view can be copied to the clipboard. Returns
    ///  True only if view contains one or more snippets that can be output as
    ///  annotated source code.</summary>
    class function CanHandleView(View: IView): Boolean; override;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  Hiliter.UAttrs, Hiliter.UGlobals, Hiliter.UHiliters, UPreferences,
  USnippetSourceGen;


{ TCopySourceCodeBase }

class function TCopySourceCodeBase.GeneratePlainText(View: IView): TEncodedData;
begin
  Result := TEncodedData.Create(GenerateSourceCode(View), etUnicode);
end;

class function TCopySourceCodeBase.GenerateRichText(View: IView): TEncodedData;
begin
  Result := TRTFDocumentHiliter.Hilite(
    GenerateSourceCode(View), THiliteAttrsFactory.CreateUserAttrs
  );
end;

{ TCopySourceMgr }

class function TCopySourceMgr.CanHandleView(View: IView): Boolean;
begin
  Result := Supports(View, ISnippetView);
end;

class function TCopySourceMgr.GenerateSourceCode(View: IView): string;
begin
  Result := (View as ISnippetView).Snippet.SourceCode;
end;

{ TCopySnippetMgr }

class function TCopySnippetMgr.CanHandleView(View: IView): Boolean;
begin
  Result := TSnippetSourceGen.CanGenerate(View);
end;

class function TCopySnippetMgr.GenerateSourceCode(View: IView): string;
begin
  Result := TSnippetSourceGen.Generate(View, Preferences.SourceCommentStyle);
end;

end.

