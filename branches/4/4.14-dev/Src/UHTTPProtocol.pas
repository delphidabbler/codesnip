{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a handlers for the "http" and "https:" URL protocols that displays
 * the URL in the default browser.
}


unit UHTTPProtocol;


interface


implementation


uses
  // Delphi
  ExtActns,
  // Project
  UBrowseProtocol, UProtocols, UStrUtils;


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
  Result := StrStartsStr(cHTTPProtocol, URL);
end;

{ THTTPSProtocol }

class function THTTPSProtocol.SupportsProtocol(const URL: string): Boolean;
begin
  Result := StrStartsStr(cHTTPSProtocol, URL);
end;

initialization

// Register the protocols with the protocol factory
TProtocolFactory.RegisterProtocol(THTTPProtocol);
TProtocolFactory.RegisterProtocol(THTTPSProtocol);

end.

