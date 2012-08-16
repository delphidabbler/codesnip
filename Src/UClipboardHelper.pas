{
 * UClipboardHelper.pas
 *
 * Implements a class that assists in working with the clipboard.
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
 * The Original Code is UClipboardHelper.pas
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


unit UClipboardHelper;


interface


uses
  // Delphi
  SysUtils,
  // Project
  UExceptions;


type

  {
  TClipboardHelper:
    Class that assists in working with the clipboard.
  }
  TClipboardHelper = class(TObject)
  public
    procedure Open;
      {Opens clipboard. Must be called before calling methods that modify
      clipboard.
      }
    procedure Close;
      {Closes clipboard. Calls must be matched with calls to Open.
      }
    procedure Add(const Fmt: Word; const Data; const DataSize: Integer);
      overload;
      {Adds data to the clipboard as a specified format. Clipboard must be open
      before calling this method. Call repeatedly with different formats and
      data to add multiple formats to clipboard.
        @param Fmt [in] Clipboard format.
        @param Data [in] Data to be added to clipboard.
        @param Datasize [in] Size of data to be added to clipboard.
      }
    procedure Add(const Fmt: Word; const Str: string); overload;
      {Adds a string, with terminating #0 character to clipboard in a specified
      format.
        @param Fmt [in] Clipboard format.
        @param Str [in] String to be added to clipboard.
      }
    procedure Add(const Fmt: Word; const Bytes: TBytes); overload;
      {Adds an array of bytes to the clipboard in a specified format.
        @param Fmt [in] Clipboard format.
        @param Bytes [in] Array of bytes to be added to clipboard.
      }
  end;

  {
  EClipboard:
    Class of exception raised by TClipboardHelper.
  }
  EClipboard = class(ECodeSnip);


var
  // Global vars that provide clipboard formats: assigned in initialization
  CF_TEXT: Word;        // plain text clipboard format
  CF_UNICODETEXT: Word; // unicode text clipboard format
  CF_RTF: Word;         // rich text clipboard formats


implementation


uses
  // Delphi
  Windows, Clipbrd, RichEdit;


{ TClipboardHelper }

procedure TClipboardHelper.Add(const Fmt: Word; const Data;
  const DataSize: Integer);
  {Adds data to the clipboard as a specified format. Clipboard must be open
  before calling this method. Call repeatedly with different formats and data
  to add multiple formats to clipboard.
    @param Fmt [in] Clipboard format.
    @param Data [in] Data to be added to clipboard.
    @param Datasize [in] Size of data to be added to clipboard.
  }
var
  GH: HGLOBAL;    // global memory handle for clipboard data block
  Ptr: Pointer;   // pointer to buffer used to pass data to clipboard
resourcestring
  sCantAlloc = 'Can''t allocate memory block for clipboard format %0.8X';
  sCantGetPtr = 'Can''t get pointer to memory block for clipboard format %0.8X';
begin
  // Allocate buffer to store data to on clipboard
  GH := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, DataSize);
  if GH = 0 then
    raise EClipboard.CreateFmt(sCantAlloc, [Fmt]);
  // Lock block to get pointer: raise exception & free mem if can't do so
  Ptr := GlobalLock(GH);
  if not Assigned(Ptr) then
  begin
    GlobalFree(GH);
    raise EClipboard.CreateFmt(sCantGetPtr, [Fmt]);
  end;
  // Copy string to clipboard memory block
  try
    Move(Data, Ptr^, DataSize); // this copies data + terminating #0
    Clipboard.SetAsHandle(Fmt, GH);
  finally
    // Unlock block - don't free mem since clipboard now owns it
    GlobalUnlock(GH);
  end;
end;

procedure TClipboardHelper.Add(const Fmt: Word; const Str: string);
  {Adds a string, with terminating #0 character to clipboard in a specified
  format.
    @param Fmt [in] Clipboard format.
    @param Str [in] String to be added to clipboard.
  }
begin
  Add(Fmt, Pointer(Str)^, SizeOf(Char) * (Length(Str) + 1));
end;

procedure TClipboardHelper.Add(const Fmt: Word; const Bytes: TBytes);
  {Adds an array of bytes to the clipboard in a specified format.
    @param Fmt [in] Clipboard format.
    @param Bytes [in] Array of bytes to be added to clipboard.
  }
begin
  Add(Fmt, Pointer(Bytes)^, Length(Bytes));
end;

procedure TClipboardHelper.Close;
  {Closes clipboard. Calls must be matched with calls to Open.
  }
begin
  Clipboard.Close;
end;

procedure TClipboardHelper.Open;
  {Opens clipboard. Must be called before calling methods that modify clipboard.
  }
begin
  Clipboard.Open;
end;

initialization

// Set clipboard format identifiers
CF_TEXT := Windows.CF_TEXT;
CF_UNICODETEXT := Windows.CF_UNICODETEXT;
CF_RTF := Windows.RegisterClipboardFormat(RichEdit.CF_RTF);

end.

