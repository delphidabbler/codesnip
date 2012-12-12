{
 * UHelpProtocol.pas
 *
 * Implements a handler for the fake "help" URL protocol that displays a help
 * topic specified by an a-link keyword included in the URL.
 *
 * v1.0 of 14 Nov 2006  - Original version.
 * v1.1 of 04 Nov 2007  - Changed to use help manager directly to display help
 *                        topic rather than using THelpTopicAction.
 * v1.2 of 04 Jul 2009  - Added new THelpProtocol.SupportsProtocol class method
 *                        that overrides abstact method added to base class.
 *                      - Protocol name is now a class constant.
 *                      - Sealed THelpProtocol class.
 *                      - Changed call to register protocol to pass only class
 *                        name: protocol prefix no longer required.
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
 * The Original Code is UHelpProtocol.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UHelpProtocol;


interface


implementation


uses
  // Delphi
  StrUtils,
  // Project
  UHelpMgr, UHTMLUtils, UProtocols;


type

  {
  THelpProtocol:
    Implements a handler for the fake "help" URL protocol that has special
    meaning within the program. The "help" protocol causes a help topic to be
    displayed to corresponds to an a-link keyword that is specified as part of
    the URL. Format of protocol is "help:alink-keyword".
  }
  THelpProtocol = class sealed(TProtocol)
  strict private
    const
      cHelpProtocol = 'help:';  // URL prefix identifying help: protocol
  public
    class function SupportsProtocol(const URL: string): Boolean; override;
      {Checks if a URL uses the help: protocol.
        @param URL [in] URL whose protocol is to be checked.
        @return True if URL's protocol is help:, False if not.
      }
    function Execute: Boolean; override;
      {Displays a-link help topic identified by URL.
        @return True.
      }
  end;

{ THelpProtocol }


function THelpProtocol.Execute: Boolean;
  {Displays a-link help topic identified by URL.
    @return True.
  }
var
  ALink: string;  // a-link help keyword
begin
  ALink := URLDecode(
    AnsiRightStr(URL, Length(URL) - Length(cHelpProtocol)), False
  );
  HelpMgr.ShowHelp(ALink);
  Result := True;
end;


class function THelpProtocol.SupportsProtocol(const URL: string): Boolean;
begin
  Result := AnsiStartsStr(cHelpProtocol, URL);
end;

initialization

// Register the protocol with the protocol factory
TProtocolFactory.RegisterProtocol(THelpProtocol);

end.
