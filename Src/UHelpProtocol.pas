{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a handler for the fake "help" URL protocol that displays a help
 * topic or contents depending on URL.
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
    meaning within the program. The "help" protocol causes the help system to
    display either a given topic or help contents depending on the content of
    the URL.
    If URL is 'help:#' then help contents is displayed.
    If URL is 'help:any-text' then a help topic with a-link keyword matching
    'any-text' is displayed.
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
      {Displays a-link help topic or help contents depending on content of URL.
        @return True.
      }
  end;

{ THelpProtocol }


function THelpProtocol.Execute: Boolean;
  {Displays a-link help topic or help contents depending on content of URL.
    @return True.
  }
var
  Param: string;  // parameter that follows protocol
begin
  Result := True;
  Param := URIDecode(StrSliceRight(URL, Length(URL) - Length(cHelpProtocol)));
  if Param = '' then
    Exit;
  if Param = '#' then
    HelpMgr.ShowContents
  else
    HelpMgr.ShowHelp(Param);
end;


class function THelpProtocol.SupportsProtocol(const URL: string): Boolean;
begin
  Result := StrStartsStr(cHelpProtocol, URL);
end;

initialization

// Register the protocol with the protocol factory
TProtocolFactory.RegisterProtocol(THelpProtocol);

end.

