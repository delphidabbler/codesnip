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
 * Implements a class that copies information about a snippet to clipboard in
 * plain text and rich text format. Only routines are supported..
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
  Hiliter.UAttrs, Hiliter.UGlobals, URTFSnippetDoc, UTextSnippetDoc;


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
  Doc: TRTFSnippetDoc;        // object that generates RTF document
  HiliteAttrs: IHiliteAttrs;  // syntax highlighter formatting attributes
begin
  Assert(Supports(View, ISnippetView),
    ClassName + '.GenerateRichText: View is not a snippet view');
  if (View as ISnippetView).Snippet.HiliteSource then
    HiliteAttrs := THiliteAttrsFactory.CreateUserAttrs
  else
    HiliteAttrs := THiliteAttrsFactory.CreateNulAttrs;
  Doc := TRTFSnippetDoc.Create(HiliteAttrs);
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

