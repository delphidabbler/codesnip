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
 * Implements a handler for the fake "help" URL protocol that displays a help
 * topic specified by an a-link keyword included in the URL.
}


unit UHelpProtocol;


interface


implementation


uses
  // Project
  UHelpMgr, UURIEncode, UProtocols, UStrUtils;


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
  ALink := URIDecode(StrSliceRight(URL, Length(URL) - Length(cHelpProtocol)));
  HelpMgr.ShowHelp(ALink);
  Result := True;
end;


class function THelpProtocol.SupportsProtocol(const URL: string): Boolean;
begin
  Result := StrStartsStr(cHelpProtocol, URL);
end;

initialization

// Register the protocol with the protocol factory
TProtocolFactory.RegisterProtocol(THelpProtocol);

end.

