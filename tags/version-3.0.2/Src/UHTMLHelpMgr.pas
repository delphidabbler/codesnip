{ ##
  @FILE                     UHTMLHelpMgr.pas
  @COMMENTS                 Implements a class that implements the IHelpMgr
                            interface and registers an instantiation of it as
                            the program's help manager. The object provides
                            help using the HTML help system.
  @PROJECT_NAME             CodeSnip
  @PROJECT_DESC             Offline viewer for routines from the online
                            DelphiDabbler CodeSnip database.
  @DEPENDENCIES             None
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 04/02/2007
      @COMMENTS             Original version.
    )
  )
}


{
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
 * The Original Code is UHelpMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s): None
 *
 * ***** END LICENSE BLOCK *****
}


unit UHTMLHelpMgr;

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}

interface


implementation


uses
  // Delphi
  Classes, Windows,
  // Project
  UAppInfo, UHTMLHelp, UHelpMgr;


type

  {
  THTMLHelpMgr:
    Class that interacts with and controls HTML Help and implements IHelpMgr.
  }
  THTMLHelpMgr = class(TInterfacedObject,
    IHelpMgr
  )
  private
    procedure DoAppHelp(const Command: LongWord; const HelpPage: string;
      const Data: LongWord);
      {Calls HtmlHelp API with specified command and parameters.
        @param Command Command to send to HTML Help.
        @param HelpPage Name of HTML file containing required help page within
          help file. May be '' if no specific page required.
        @param Data Data to pass to HTML Help. Format depends on command.
      }
  public
    procedure ShowHelp(const AKeyword: string); overload;
      {Displays HTML help topic specified by an A-Link keyword.
        @param AKeyword Required A-Link keyword.
      }
    procedure ShowHelp(const HelpContext: THelpContext); overload;
      {Displays HTML help topic per a help context number.
        @param HelpContext Required topic's help context number.
      }
    procedure ShowContents;
      {Displays HTML help contents tab and default help page.
      }
  end;


{ THTMLHelpMgr }

procedure THTMLHelpMgr.DoAppHelp(const Command: LongWord;
  const HelpPage: string; const Data: LongWord);
  {Calls HtmlHelp API with specified command and parameters.
    @param Command Command to send to HTML Help.
    @param HelpPage Name of HTML file containing required help page within help
      file. May be '' if no specific page required.
    @param Data Data to pass to HTML Help. Format depends on command.
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

procedure THTMLHelpMgr.ShowHelp(const HelpContext: THelpContext);
  {Displays HTML help topic per a help context number.
    @param HelpContext Required topic's help context number.
  }
begin
  DoAppHelp(HH_HELP_CONTEXT, '', HelpContext);
end;

procedure THTMLHelpMgr.ShowHelp(const AKeyword: string);
  {Displays HTML help topic specified by an A-Link keyword.
    @param AKeyword Required A-Link keyword.
  }
var
  ALink: THHAKLink; // structure containing details of A-Link
begin
  // Fill in A link structure
  ZeroMemory(@ALink, SizeOf(ALink));
  ALink.cbStruct := SizeOf(ALink);      // size of structure
  ALink.pszKeywords := PChar(AKeyword); // required keyword
  ALink.fIndexOnFail := True;           // display index page if kwd not found
  // Display help
  DoAppHelp(HH_ALINK_LOOKUP, '', LongWord(@ALink));
end;

initialization

// Create instance of help manager and register it for use as program's help system
RegisterHelpMgr(THTMLHelpMgr.Create);

end.
