{
 * UCopyInfoMgr.pas
 *
 * Implements a class that copies information about a snippet to clipboard in
 * plain text and rich text format. Only routines are supported..
 *
 * v1.0 of 04 Jan 2009  - Original version.
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
 * The Original Code is UCopyInfoMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UCopyInfoMgr;


interface


uses
  // Project
  UBaseObjects, URoutineDoc, UView;


type

  {
  TCopyInfoMgr:
    Class that copies information about a snippet to clipboard in plain text and
    rich text format. Only routines are supported.
  }
  TCopyInfoMgr = class(TNoConstructObject)
  strict private
    class function GeneratePlainText(const View: TViewItem): string;
      {Generates a plain text document providing information about routine.
        @param View [in] View representing routine to be generated.
        @return Generated text document as a string.
      }
    class function GenerateRTF(const View: TViewItem): string;
      {Generates a RTF document providing information about routine.
        @param View [in] View representing routine to be generated.
        @return Generated RTF document as a string.
      }
    class function GenerateDoc(const View: TViewItem;
      const Doc: TRoutineDoc): string;
      {Generate a document that describes a routine.
        @param View [in] View that defines routine to be generated.
        @param Doc [in] Object used to render document in required format.
        @return Generated document as a string.
      }
  public
    class function CanHandleView(const View: TViewItem): Boolean;
      {Checks if snippet can be copied to clipboard.
        @param View [in] View to be checked.
        @return True if view is a routine, False otherwise.
      }
    class procedure Execute(const View: TViewItem);
      {Copies the information about a snippet to the clipboard.
        @param View [in] View that defines snippet to be copied. Must be
          supported snippet type.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Classes,
  // Project
  UClipboardHelper, UHiliteAttrs, URTFRoutineDoc, UTextRoutineDoc;


{ TCopyInfoMgr }

class function TCopyInfoMgr.CanHandleView(const View: TViewItem): Boolean;
  {Checks if snippet can be copied to clipboard.
    @param View [in] View to be checked.
    @return True if view is a routine, False otherwise.
  }
begin
  Result := View.Kind = vkRoutine;
end;

class procedure TCopyInfoMgr.Execute(const View: TViewItem);
  {Copies the information about a snippet to the clipboard.
    @param View [in] View that defines snippet to be copied. Must be supported
      snippet type.
  }
var
  Clip: TClipboardHelper; // object used to update clipboard
begin
  Assert(Assigned(View),                                   // ** do not localise
    ClassName + '.Execute: View is nil');
  Assert(CanHandleView(View),                              // ** do not localise
    ClassName + '.Execute: View not supported');
  // Open clipboard and add both plain and RTF representations of snippet
  Clip := TClipboardHelper.Create;
  try
    Clip.Open;
    try
      Clip.Add(CF_TEXT, GeneratePlainText(View));
      Clip.Add(CF_RTF, GenerateRTF(View));
    finally
      Clip.Close;
    end;
  finally
    FreeAndNil(Clip);
  end;
end;

class function TCopyInfoMgr.GenerateDoc(const View: TViewItem;
  const Doc: TRoutineDoc): string;
  {Generate a document that describes a routine.
    @param View [in] View that defines routine to be generated.
    @param Doc [in] Object used to render document in required format.
    @return Generated document as a string.
  }
var
  SS: TStringStream;  // stream that receives document
begin
  SS := TStringStream.Create('');
  try
    Doc.Generate(View.Routine, SS);
    Result := SS.DataString;
  finally
    FreeAndNil(SS);
  end;
end;

class function TCopyInfoMgr.GeneratePlainText(const View: TViewItem): string;
  {Generates a plain text document providing information about routine.
    @param View [in] View representing routine to be generated.
    @return Generated text document as a string.
  }
var
  Doc: TTextRoutineDoc; // object that generates plain text document
begin
  Doc := TTextRoutineDoc.Create;
  try
    Result := GenerateDoc(View, Doc);
  finally
    FreeAndNil(Doc);
  end;
end;

class function TCopyInfoMgr.GenerateRTF(const View: TViewItem): string;
  {Generates a RTF document providing information about routine.
    @param View [in] View representing routine to be generated.
    @return Generated RTF document as a string.
  }
var
  Doc: TRTFRoutineDoc;  // object that generates RTF document
begin
  Doc := TRTFRoutineDoc.Create(THiliteAttrsFactory.CreateUserAttrs);
  try
    Result := GenerateDoc(View, Doc);
  finally
    FreeAndNil(Doc);
  end;
end;

end.

