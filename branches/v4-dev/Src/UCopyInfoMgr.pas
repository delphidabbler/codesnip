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
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
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
  // Delphi
  SysUtils,
  // Project
  UCopyViewMgr, URoutineDoc, UView;


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
    ///  <param name="Doc">TRoutineDoc [in] Object that renders document. Format
    ///  depends on concrete class of object.</param>
    ///  <returns>TBytes - Byte array containing document in form suitable for
    ///  copying to clipboard.</returns>
    class function GenerateDoc(View: IView; const Doc: TRoutineDoc): TBytes;
  strict protected
    ///  <summary>Returns a byte array containing a Unicode plain text
    ///  representation of information about the snippet represented by the
    ///  given view that is to be copied to the clipboard.</summary>
    class function GeneratePlainText(View: IView): TBytes; override;
    ///  <summary>Returns a byte array containing a RTF representation of
    ///  information about the snippet represented by the given view that is to
    ///  be copied to the clipboard.</summary>
    class function GenerateRichText(View: IView): TBytes; override;
  public
    ///  <summary>Checks if a given view can be copied to the clipboard. Returns
    ///  True only if the view represents a snippet.</summary>
    class function CanHandleView(View: IView): Boolean; override;
  end;


implementation


uses
  // Delphi
  Classes,
  // Project
  Hiliter.UAttrs, URTFRoutineDoc, UTextRoutineDoc;


{ TCopyInfoMgr }

class function TCopyInfoMgr.CanHandleView(View: IView): Boolean;
begin
  Result := Supports(View, ISnippetView);
end;

class function TCopyInfoMgr.GenerateDoc(View: IView; const Doc: TRoutineDoc):
  TBytes;
var
  Stm: TBytesStream;  // stream that receives document
begin
  Stm := TBytesStream.Create;
  try
    Doc.Generate((View as ISnippetView).Snippet, Stm);
    Result := Stm.Bytes;
    SetLength(Result, Stm.Size);
  finally
    Stm.Free;
  end;
end;

class function TCopyInfoMgr.GeneratePlainText(View: IView): TBytes;
var
  Doc: TTextRoutineDoc; // object that generates plain text document
begin
  Doc := TTextRoutineDoc.Create;
  try
    // TTextRoutineDoc generates stream of Unicode bytes
    Result := GenerateDoc(View, Doc);
  finally
    Doc.Free;
  end;
end;

class function TCopyInfoMgr.GenerateRichText(View: IView): TBytes;
var
  Doc: TRTFRoutineDoc;  // object that generates RTF document
begin
  Doc := TRTFRoutineDoc.Create(THiliteAttrsFactory.CreateUserAttrs);
  try
    // TRTFRoutineDoc generates stream of ASCII bytes
    Result := GenerateDoc(View, Doc);
  finally
    Doc.Free;
  end;
end;

end.

