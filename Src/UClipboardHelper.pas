{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that manages copying data to the clipboard in multiple
 * formats.
}


unit UClipboardHelper;


interface


uses
  // Project
  UEncodings, UExceptions;


type

  ///  <summary>
  ///  Class that manages copying data to the clipboard in multiple formats.
  ///  </summary>
  TClipboardHelper = class(TObject)
  strict private
    ///  <summary>Adds data to the clipboard in a specified format.</summary>
    ///  <param name="Fmt">Word [in] Required clipboard format.</param>
    ///  <param name="Data">Untyped [in] Data to be placed on clipboard.</param>
    ///  <param name="DataSize">Integer [in] Size of Data in bytes.</param>
    ///  <remarks>
    ///  <para>Clipboard must be open before calling this method.</para>
    ///  <para>Call repeatedly with different formats and data to add more than
    ///  one format.</para>
    ///  </remarks>
    procedure Add(const Fmt: Word; const Data; const DataSize: Integer);
  public
    ///  <summary>Opens clipboard.</summary>
    ///  <remarks>Must be called before calling any of the AddXXX methods.
    ///  </remarks>
    procedure Open;
    ///  <summary>Closes clipboard.</summary>
    ///  <remarks>Calls must be balanced with calls to Open.</remarks>
    procedure Close;
    ///  <summary>Adds plain Unicode text to clipboard in CF_UNICODETEXT format.
    ///  </summary>
    ///  <param name="AText">UnicodeString [in] Text to be placed on clipboard.
    ///  </param>
    ///  <remarks>
    ///  <para>String is stored with terminating #0#0.</para>
    ///  <para>Windows also automatically creates CF_TEXT, CF_OEMTEXT and
    ///  CF_LOCALE clipboard formats.</para>
    ///  </remarks>
    procedure AddUnicodeText(const AText: UnicodeString);
    ///  <summary>Adds RTF code to the clipboard in 'Rich Text Format' format.
    ///  </summary>
    ///  <param name="ARTF">ASCIIString [in] ASCII string containing RTF code to
    ///  be placed on clipboard.</param>
    ///  <remarks>Code string is stored with terminating #0.</remarks>
    procedure AddRTF(const ARTF: ASCIIString);
  end;

type
  ///  <summary>
  ///  Class of exception raised by TClipboardHelper.
  ///  </summary>
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

procedure TClipboardHelper.AddRTF(const ARTF: ASCIIString);
begin
  Add(CF_RTF, Pointer(ARTF)^, (Length(ARTF) + 1) * SizeOf(AnsiChar));
end;

procedure TClipboardHelper.AddUnicodeText(const AText: UnicodeString);
begin
  Add(CF_UNICODETEXT, Pointer(AText)^, (Length(AText) + 1) * SizeOf(WideChar));
end;

procedure TClipboardHelper.Close;
begin
  Clipboard.Close;
end;

procedure TClipboardHelper.Open;
begin
  Clipboard.Open;
end;

initialization

// Set clipboard format identifiers
CF_TEXT := Windows.CF_TEXT;
CF_UNICODETEXT := Windows.CF_UNICODETEXT;
CF_RTF := Windows.RegisterClipboardFormat(RichEdit.CF_RTF);

end.

