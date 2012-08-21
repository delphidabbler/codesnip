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

  {
  TCopyInfoMgr:
    Class that copies information about a snippet to clipboard in plain text and
    rich text format. Only routines are supported.
  }
  TCopyInfoMgr = class sealed(TCopyViewMgr)
  strict private
    class function GenerateDoc(const View: TViewItem; const Doc: TRoutineDoc;
      const Encoding: TEncoding): string;
      {Generates a document that describes a snippet.
        @param View [in] View that defines snippet to be generated.
        @param Doc [in] Object used to render document in required format.
        @param Encoding [in] Encoding to use on string stream that receives
          generated document.
        @return Generated document as a string.
      }
  strict protected
    class function GeneratePlainText(const View: TViewItem): string; override;
      {Generates a plain text document providing information about a snippet.
        @param View [in] View representing snippet.
        @return Plain text document as a string.
      }
    class function GenerateRichText(const View: TViewItem): string; override;
      {Generates a RTF document providing information about snippet.
        @param View [in] View representing snippet.
        @return RTF document as a string.
      }
  public
    class function CanHandleView(const View: TViewItem): Boolean; override;
      {Checks if snippet can be copied to clipboard.
        @param View [in] View to be checked.
        @return True if view is a snippet, False otherwise.
      }
  end;


implementation


uses
  // Delphi
  Classes,
  // Project
  Hiliter.UAttrs, URTFRoutineDoc, UTextRoutineDoc;


{ TCopyInfoMgr }

class function TCopyInfoMgr.CanHandleView(const View: TViewItem): Boolean;
  {Checks if snippet can be copied to clipboard.
    @param View [in] View to be checked.
    @return True if view is a snippet, False otherwise.
  }
begin
  Result := View.Kind = vkRoutine;
end;

class function TCopyInfoMgr.GenerateDoc(const View: TViewItem;
  const Doc: TRoutineDoc; const Encoding: TEncoding): string;
  {Generates a document that describes a snippet.
    @param View [in] View that defines snippet to be generated.
    @param Doc [in] Object used to render document in required format.
    @param Encoding [in] Encoding to use on string stream that receives
      generated document.
    @return Generated document as a string.
  }
var
  SS: TStringStream;  // stream that receives document
begin
  SS := TStringStream.Create('', Encoding);
  try
    Doc.Generate(View.Routine, SS);
    Result := SS.DataString;
  finally
    FreeAndNil(SS);
  end;
end;

class function TCopyInfoMgr.GeneratePlainText(const View: TViewItem): string;
  {Generates a plain text document providing information about a snippet.
    @param View [in] View representing snippet.
    @return Plain text document as a string.
  }
var
  Doc: TTextRoutineDoc; // object that generates plain text document
begin
  Doc := TTextRoutineDoc.Create;
  try
    Result := GenerateDoc(View, Doc, TEncoding.Unicode);
  finally
    FreeAndNil(Doc);
  end;
end;

class function TCopyInfoMgr.GenerateRichText(const View: TViewItem): string;
  {Generates a RTF document providing information about snippet.
    @param View [in] View representing snippet.
    @return RTF document as a string.
  }
var
  Doc: TRTFRoutineDoc;  // object that generates RTF document
begin
  Doc := TRTFRoutineDoc.Create(THiliteAttrsFactory.CreateUserAttrs);
  try
    Result := GenerateDoc(View, Doc, TEncoding.Default);
  finally
    FreeAndNil(Doc);
  end;
end;

end.

