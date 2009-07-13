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
 * v2.0 of 04 Jul 2009  - THTTPProtocol heavily revised:
 *                        - Now implements new abstract SupportsProtocol method
 *                          inherited from revised TProtocol.
 *                        - Now descends from TBrowseProtocol instead of
 *                          directly from TProtocol. Execute method removed as
 *                          inherited method from TBrowseProtocol now used.
 *                        - Class now sealed.
 *                        - Protocol name is now a class constant.
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
 * The Original Code is UHTTPProtocol.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UHTTPProtocol;


interface


implementation


uses
  // Delphi
  StrUtils, ExtActns,
  // Project
  UBrowseProtocol, UProtocols;


type

  {
  THTTPProtocol:
    Implements a handler for the "HTTP" protocol that causes URL to be displayed
    in the default web browser.
  }
  THTTPProtocol = class sealed(TBrowseProtocol)
  strict private
    const
      cHTTPProtocol = 'http://';  // URL prefix identifying http: protocol
  public
    class function SupportsProtocol(const URL: string): Boolean; override;
      {Checks if a URL uses the http: protocol.
        @param URL [in] URL whose protocol is to be checked.
        @return True if URL's protocol is http:, False if not.
      }
  end;


{ THTTPProtocol }

class function THTTPProtocol.SupportsProtocol(const URL: string): Boolean;
  {Checks if a URL uses the http: protocol.
    @param URL [in] URL whose protocol is to be checked.
    @return True if URL's protocol is http:, False if not.
  }
begin
  Result := AnsiStartsStr(cHTTPProtocol, URL);
end;


initialization

// Register the protocol with the protocol factory
TProtocolFactory.RegisterProtocol(THTTPProtocol);

end.

