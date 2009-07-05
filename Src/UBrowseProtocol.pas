{
 * UBrowseProtocol.pas
 *
 * Implements a abstract base class for protocol handlers that access a URL
 * using a TBrowseURL action.
 *
 * v1.0 of 04 Jul 2009  - Original version.
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
 * The Original Code is UBrowseProtocol.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
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
begin
  // We execute the resource using an action
  try
    with TBrowseURL.Create(nil) do
      try
        URL := Self.URL;
        Execute;
        Result := True;
      finally
        Free;
      end;
  except
    // any exceptions converted to EProtocol 
    on E: Exception do
      raise EProtocol.Create(E);
  end;
end;

end.

