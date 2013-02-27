{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a class that submits code snippets database contributions via a
 * web service.
}


unit Web.UCodeSubmitter;


interface


uses
  // Delphi
  SysUtils, Classes,
  // Project
  Web.UStdWebService;


type

  {
  TCodeSubmitter:
    Submits code to the Code Snippets Database using a web service via HTTP.
  }
  TCodeSubmitter = class sealed(TStdWebService)
  public
    constructor Create;
      {Class constructor. Initialises service.
      }
    procedure SubmitData(const Data: TBytes);
      {Submits data describing code to web service.
        @param Data [in] Byte array containing code submission information.
      }
  end;


implementation


uses
  // Project
  Web.UInfo;


const
  // Web service info
  cScriptName = 'codesnip-submitter.php';       // script name
  cUserAgent = 'DelphiDabbler-CSSubmitter-v1';  // user agent string
  cMediaType = 'text/xml';                      // media type


resourcestring
  // Error message
  sBadRespone = 'Invalid response from code snippets submitter web service.';


{ TCodeSubmitter }

constructor TCodeSubmitter.Create;
  {Class constructor. Initialises service.
  }
begin
  inherited Create(TWebServiceInfo.Create(cScriptName, cUserAgent, cMediaType));
end;

procedure TCodeSubmitter.SubmitData(const Data: TBytes);
  {Submits data describing code to web service.
    @param Data [in] Byte array containing code submission information.
  }
var
  Response: TStringList;  // valid response from web service
begin
  Assert(Assigned(Data), ClassName + '.Subscribe: Data stream is nil');
  // Send subscribe command to web service and gather response
  Response := TStringList.Create;
  try
    PostData(Data, Response);
  finally
    FreeAndNil(Response);
  end;
end;

end.

