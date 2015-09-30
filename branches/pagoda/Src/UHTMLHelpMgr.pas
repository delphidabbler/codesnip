{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Provides a class that implements the IHelpMgr interface using the HTML Help
 * system and registers an instantiation of the class as the program's
 * help manager.
}


unit UHTMLHelpMgr;


interface


implementation


uses
  // Delphi
  Windows,
  // Project
  UAppInfo, UEncodings, UHelpMgr;


type
  ///  <summary>Structure used to specify one or more ALink names or KLink
  ///  keywords to be searched for.</summary>
  ///  <remarks>The LPCTSTR fields behave strangely when using the Unicode API.
  ///  In this case LPCTSTR is defined as PWideChar, but casting a Unicode
  ///  string to a PChar (=PWideChar) causes HTML Help to see only the first
  ///  character of the keyword, implying HTML Help is treating the string an
  ///  ANSI string. To get this to work a Unicode Delphi string must first be
  ///  cast to an ANSI string and then to a pointer before assiging to the
  ///  LPCTSTR valued field.</remarks>
  THHAKLink = packed record
    ///  <summary>Size of record in bytes.</summary>
    cbStruct: Integer;
    ///  <summary>Reserved. Must be False.</summary>
    fReserved: BOOL;
    ///  <summary>Semi-colon separated keywords.</summary>
    pszKeywords: LPCTSTR;
    ///  <summary>URL to jump to if none of the keywords are found (may be nil).
    ///  </summary>
    pszUrl: LPCTSTR;
    ///  <summary>Text to be displayed in a message box on failure (may be nil).
    ///  </summary>
    pszMsgText: LPCTSTR;
    ///  <summary>Title of any failure message box.</summary>
    pszMsgTitle: LPCTSTR;
    ///  <summary>Name of window where pszUrl is to be displayed.</summary>
    pszWindow: LPCTSTR;
    ///  <summary>Flag determining if help index is displayed if keyword lookup
    ///  fails.</summary>
    fIndexOnFail: BOOL;
  end;

  ///  <summary>Class that implements the help manager interface for the HTML
  ///  help system.</summary>
  THTMLHelpMgr = class(TInterfacedObject, IHelpMgr)
  strict private
    ///  <summary>Calls HtmlHelp API function with given command, help page and
    ///  parameters.</summary>
    ///  <param name="Command">UINT [in] Command to send to HTML Help.</param>
    ///  <param name="HelpPage">string [in] Name of required HTML topic "file"
    ///  within help file. May be empty string if no specific page is required.
    ///  </param>
    ///  <param name="Data">DWORD [in] Data to pass to HTML Help. Meaning
    ///  depends on Command.</param>
    procedure DoAppHelp(const Command: UINT; const HelpPage: string;
      const Data: DWORD);
  public
    ///  <summary>Displays any help topics that match the given A-Link keyword.
    ///  </summary>
    procedure ShowHelp(const AKeyword: string);
    ///  <summary>Displays HTML help contents tab and default help page.
    ///  </summary>
    procedure ShowContents;
  end;

{ THTMLHelpMgr }

procedure THTMLHelpMgr.DoAppHelp(const Command: UINT; const HelpPage: string;
  const Data: DWORD);
var
  HelpSpec: string; // Help file name followed by any required topic page
begin
  HelpSpec := TAppInfo.HelpFileName;
  if HelpPage <> '' then
    HelpSpec := HelpSpec + '::/' + HelpPage;
  HtmlHelp(GetDesktopWindow(), HelpSpec, Command, Data);
end;

procedure THTMLHelpMgr.ShowContents;
begin
  DoAppHelp(HH_DISPLAY_TOC, '', 0);
end;

procedure THTMLHelpMgr.ShowHelp(const AKeyword: string);
var
  ALink: THHAKLink; // structure containing details of A-Link
begin
  ZeroMemory(@ALink, SizeOf(THHAKLink));
  ALink.cbStruct := SizeOf(THHAKLink);
  // See remarks for THHAKLink for explanation of the following string casts.
  ALink.pszKeywords := Pointer(StringToASCIIString(AKeyword));
  ALink.fIndexOnFail := True;
  // We pass "pointer" to ALink as command's data
  DoAppHelp(HH_ALINK_LOOKUP, '', DWORD(@ALink));
end;

initialization

RegisterHelpMgr(THTMLHelpMgr.Create);

end.

