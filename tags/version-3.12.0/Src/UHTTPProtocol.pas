{
 * UHTTPProtocol.pas
 *
 * Implements a handlers for the "http" and "https:" URL protocols that displays
 * the URL in the default browser.
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
 * The Original Code is UHTTPProtocol.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
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

  {
  THTTPSProtocol:
    Implements a handler for the "HTTPS" protocol that causes URL to be
    displayed in the default web browser.
  }
  THTTPSProtocol = class sealed(TBrowseProtocol)
  strict private
    const
      cHTTPSProtocol = 'https://';  // URL prefix identifying http: protocol
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

{ THTTPSProtocol }

class function THTTPSProtocol.SupportsProtocol(const URL: string): Boolean;
begin
  Result := AnsiStartsStr(cHTTPSProtocol, URL);
end;

initialization

// Register the protocols with the protocol factory
TProtocolFactory.RegisterProtocol(THTTPProtocol);
TProtocolFactory.RegisterProtocol(THTTPSProtocol);

end.

