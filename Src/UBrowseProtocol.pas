{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a abstract base class for protocol handlers that access a URL
 * using a TBrowseURL action.
}


unit UBrowseProtocol;


interface


uses
  // Project
  UProtocols;


type

  {
  TBrowseProtocol:
    Abstract base class for protocol handlers that access a URL using a
    TBrowseURL action.
  }
  TBrowseProtocol = class abstract(TProtocol)
  strict protected
    class function NormaliseURL(const URL: string): string; virtual;
      {Converts URL into its normal form. Does nothing by default. Descendants
      may override.
        @param URL [in] URL to be normalised.
        @return URL unchanged.
      }
  public
    class function SupportsProtocol(const URL: string): Boolean;
      override; abstract;
      {Checks if this protocol handler handles a URL's protocol.
        @param URL [in] URL whose protocol is to be checked.
        @return True if URL's protocol is supported, False if not.
      }
    function Execute: Boolean; override;
      {Executes resource using a TBrowseAction to display URL in default
      application for file type.
        @return True.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, ExtActns;


{ TBrowseProtocol }

function TBrowseProtocol.Execute: Boolean;
  {Executes resource using shell to display URL in default application for file
  type.
    @return True.
    @except EProtocol raised if an exception occurs in browse action.
  }
var
  BrowseAction: TBrowseURL;
begin
  // We execute the resource using an action
  try
    BrowseAction := TBrowseURL.Create(nil);
    try
      BrowseAction.URL := NormaliseURL(Self.URL);
      BrowseAction.Execute;
      Result := True;
    finally
      BrowseAction.Free;
    end;
  except
    // any exceptions converted to EProtocol
    on E: Exception do
      raise EProtocol.Create(E);
  end;
end;

class function TBrowseProtocol.NormaliseURL(const URL: string): string;
  {Converts URL into its normal form. Does nothing by default. Descendants may
  override.
    @param URL [in] URL to be normalised.
    @return URL unchanged.
  }
begin
  Result := URL;
end;

end.

