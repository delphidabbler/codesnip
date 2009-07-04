{
 * UHTTPProtocol.pas
 *
 * Implements a handler for the "http" URL protocol that displays the URL in the
 * default browser.
 *
 * v1.0 of 14 Nov 2006  - Original version based on code extracted from former
 *                        UProtocolHandler (now UProtocols) unit.
 * v1.1 of 04 Nov 2007  - Changed to use TBrowseURL action rather than
 *                        TShellExecAction to display URL in browser.
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
 * The Original Code is UHTTPProtocol.pas
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


unit UHTTPProtocol;


interface


implementation


uses
  // Delphi
  ExtActns,
  // Project
  UProtocols;


type

  {
  THTTPProtocol:
    Implements a handler for the "HTTP" protocol that causes URL to be displayed
    in the default web browser.
  }
  THTTPProtocol = class(TProtocol)
  public
    function Execute: Boolean; override;
      {Executes resource using shell to display URL in default browser.
        @return True.
        @except raises EShellExecAction if shell cannot execute the resource.
      }
  end;


{ THTTPProtocol }

const
  cHTTPProtocol = 'http://';  // URL prefix identifying "HTTP" protocol

function THTTPProtocol.Execute: Boolean;
  {Executes resource using shell to display URL in default browser.
    @return True.
    @except raises EShellExecAction if shell cannot execute the resource.
  }
begin
  // We execute the resource using an action
  with TBrowseURL.Create(nil) do
    try
      URL := Self.URL;
      Execute;
      Result := True;
    finally
      Free;
    end;
end;


initialization

// Register the protocol with the protocol factory
TProtocolFactory.RegisterProtocol(cHTTPProtocol, THTTPProtocol);

end.

