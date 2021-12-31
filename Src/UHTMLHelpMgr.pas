{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that implements the IHelpMgr interface and registers an
 * instantiation of it as the program's help manager. The object provides help
 * using the HTML help system.
}


unit UHTMLHelpMgr;


interface


implementation


uses
  // Delphi
  Windows,
  // Project
  UAppInfo, UEncodings, UHTMLHelp, UHelpMgr;


type
  {
  THTMLHelpMgr:
    Class that interacts with and controls HTML Help and implements IHelpMgr.
  }
  THTMLHelpMgr = class(TInterfacedObject,
    IHelpMgr
  )
  strict private
    procedure DoAppHelp(const Command: LongWord; const HelpPage: string;
      const Data: LongWord);
      {Calls HtmlHelp API with specified command and parameters.
        @param Command [in] Command to send to HTML Help.
        @param HelpPage [in] Name of HTML file containing required help page
          within help file. May be '' if no specific page required.
        @param Data [in] Data to pass to HTML Help. Format depends on command.
      }
  public
    procedure ShowHelp(const AKeyword: string);
      {Displays HTML help topic specified by an A-Link keyword.
        @param AKeyword [in] Required A-Link keyword.
      }
    procedure ShowContents;
      {Displays HTML help contents tab and default help page.
      }
  end;

{ THTMLHelpMgr }

procedure THTMLHelpMgr.DoAppHelp(const Command: LongWord;
  const HelpPage: string; const Data: LongWord);
  {Calls HtmlHelp API with specified command and parameters.
    @param Command [in] Command to send to HTML Help.
    @param HelpPage [in] Name of HTML file containing required help page within
      help file. May be '' if no specific page required.
    @param Data [in] Data to pass to HTML Help. Format depends on command.
  }
var
  HelpSpec: string; // Help file followed by any required page
begin
  HelpSpec := TAppInfo.HelpFileName;
  if HelpPage <> '' then
    HelpSpec := HelpSpec + '::/' + HelpPage;
  HtmlHelp(GetDesktopWindow(), PChar(HelpSpec), Command, Data);
end;

procedure THTMLHelpMgr.ShowContents;
  {Displays HTML help contents tab and default help page.
  }
begin
  DoAppHelp(HH_DISPLAY_TOC, '', 0);
end;

procedure THTMLHelpMgr.ShowHelp(const AKeyword: string);
  {Displays HTML help topic specified by an A-Link keyword.
    @param AKeyword [in] Required A-Link keyword.
  }
var
  ALink: THHAKLink; // structure containing details of A-Link
begin
  ZeroMemory(@ALink, SizeOf(THHAKLink));
  // Fill in A link structure
  ALink.cbStruct := SizeOf(THHAKLink);
  // *** NOTE:
  // This one is weird: when using the unicode API just casting the keyword to
  // PChar causes HTML Help to see only the first character of the keyword. We
  // have to cast to ASCII string and then to a pointer to get this to work,
  // even though pszKeywords is declared as PWideChar
  ALink.pszKeywords := Pointer(StringToASCIIString(AKeyword));
  ALink.fIndexOnFail := True;
  // Display help
  DoAppHelp(HH_ALINK_LOOKUP, '', LongWord(@ALink));
end;

initialization

// Create instance of help manager and register it for use as program's help
// system
RegisterHelpMgr(THTMLHelpMgr.Create);

end.

