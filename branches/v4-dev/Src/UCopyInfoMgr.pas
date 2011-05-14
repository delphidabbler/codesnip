{
 * UCopyInfoMgr.pas
 *
 * Implements a class that copies information about a snippet to clipboard in
 * plain text and rich text format. Only routines are supported..
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
 * The Original Code is UCopyInfoMgr.pas
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


unit UCopyInfoMgr;


interface


uses
  // Project
  UCopyViewMgr, UEncodings, USnippetDoc, UView;


type
  ///  <summary>
  ///  Class that copies information about a snippet to clipboard as plain
  ///  Unicode text and rich text formats. Snippet is obtained from a view.
  ///  Only snippet views are supported.
  ///  </summary>
  TCopyInfoMgr = class sealed(TCopyViewMgr)
  strict private
    ///  <summary>Generates a document that describes a snippet.</summary>
    ///  <param name="View">IView [in] View that represents the snippet to be
    ///  described.</param>
    ///  <param name="Doc">TSnippetDoc [in] Object that renders document. Format
    ///  depends on concrete class of object.</param>
    ///  <returns>TEncodedData - Document in form suitable for copying to
    ///  clipboard.</returns>
    class function GenerateDoc(View: IView; const Doc: TSnippetDoc):
      TEncodedData;
  strict protected
    ///  <summary>Returns encoded data containing a Unicode plain text
    ///  representation of information about the snippet represented by the
    ///  given view that is to be copied to the clipboard.</summary>
    class function GeneratePlainText(View: IView): TEncodedData; override;
    ///  <summary>Returns encoded data containing a RTF representation of
    ///  information about the snippet represented by the given view that is to
    ///  be copied to the clipboard.</summary>
    class function GenerateRichText(View: IView): TEncodedData; override;
  public
    ///  <summary>Checks if a given view can be copied to the clipboard. Returns
    ///  True only if the view represents a snippet.</summary>
    class function CanHandleView(View: IView): Boolean; override;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  Hiliter.UAttrs, URTFSnippetDoc, UTextSnippetDoc;


{ TCopyInfoMgr }

class function TCopyInfoMgr.CanHandleView(View: IView): Boolean;
begin
  Result := Supports(View, ISnippetView);
end;

class function TCopyInfoMgr.GenerateDoc(View: IView; const Doc: TSnippetDoc):
  TEncodedData;
begin
  Result := Doc.Generate((View as ISnippetView).Snippet);
end;

class function TCopyInfoMgr.GeneratePlainText(View: IView): TEncodedData;
var
  Doc: TTextSnippetDoc; // object that generates plain text document
begin
  Doc := TTextSnippetDoc.Create;
  try
    // TTextSnippetDoc generates stream of Unicode bytes
    Result := GenerateDoc(View, Doc);
    Assert(Result.EncodingType = etUnicode,
      ClassName + '.GeneratePlainText: Unicode encoded data expected');
  finally
    Doc.Free;
  end;
end;

class function TCopyInfoMgr.GenerateRichText(View: IView): TEncodedData;
var
  Doc: TRTFSnippetDoc;  // object that generates RTF document
begin
  Doc := TRTFSnippetDoc.Create(THiliteAttrsFactory.CreateUserAttrs);
  try
    // TRTFSnippetDoc generates stream of ASCII bytes
    Result := GenerateDoc(View, Doc);
    Assert(Result.EncodingType = etASCII,
      ClassName + '.GenerateRichText: ASCII encoded data expected');
  finally
    Doc.Free;
  end;
end;

end.

