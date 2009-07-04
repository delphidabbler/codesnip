{
 * UHelpProtocol.pas
 *
 * Implements a handler for the fake "help" URL protocol that displays a help
 * topic specified by a-link keyword included in the URL. Format of protocol is
 * "help:a-link-keyword".
 *
 * v1.0 of 14 Nov 2006  - Original version.
 * v1.1 of 04 Nov 2007  - Changed to use help manager directly to display help
 *                        topic rather than using THelpTopicAction.
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
 * Portions created by the Initial Developer are Copyright (C) 2006-2007 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s): None
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
    meaning within the program. The "help" protocol causes the help topic whose
    a-link follows the protocol in the URL to be displayed. Format of protocol
    is "help:help_topic_alink".
  }
  THelpProtocol = class(TProtocol)
  public
    function Execute: Boolean; override;
      {Displays a-link help topic identified by URL.
        @return True.
      }
  end;

{ THelpProtocol }

const
  cHelpProtocol = 'help:';  // URL prefix identifying "help" protocol

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


initialization

// Register the protocol with the protocol factory
TProtocolFactory.RegisterProtocol(cHelpProtocol, THelpProtocol);

end.

