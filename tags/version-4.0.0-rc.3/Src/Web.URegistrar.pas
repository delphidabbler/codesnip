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
 * Implements a class that interfaces with a web service to register the
 * application online.
}


unit Web.URegistrar;


interface


uses
  // Delphi
  Classes,
  // Project
  Web.UStdWebService;


type

  {
  TRegistrar
    Class that registers the application online by using the registration web
    service.
  }
  TRegistrar = class(TStdWebService)
  public
    constructor Create;
      {Class constructor. Initialises service.
      }
    function Submit(const Data: TStrings): string;
      {Sends application registration data to web service and interprets
      response.
        @param Data Application registration information as list of Name=Value
          data.
        @return Registration key.
      }
  end;


implementation


uses
  // Project
  UStrUtils, UURIParams, Web.UInfo;


{
  Web service notes: register-app.php
  -----------------------------------

  This web service enables CodeSnip to register the application with the
  DelphiDabbler website.

  The service accepts registration information as a series of name=value pairs
  POSTed to it. The required and optional data is as follows.

  +--------------------------------------------------------------------+
  | Name      | Required? | Description                                |
  +-----------+-----------+--------------------------------------------+
  | ProgId    | Yes       | Program id code (string)                   |
  +-----------+-----------+--------------------------------------------+
  | ProgName  | No        | Name of program (string)                   |
  +-----------+-----------+--------------------------------------------+
  | ProgVer   | No        | Program version number (dotted quad)       |
  +-----------+-----------+--------------------------------------------+
  | ProgKey   | Yes       | Unique program key (32 hex digits)         |
  +-----------+-----------+--------------------------------------------+
  | UserName  | Yes       | Name of user registering program           |
  +-----------+-----------+--------------------------------------------+
  | MailList  | No        | Whether to join any mailing list (0 or 1)  |
  +-----------+-----------+--------------------------------------------+
  | UserEmail | No*       | Email address of user joining mailing list |
  |           |           | * required only if MailList=1              |
  +-----------+-----------+--------------------------------------------+

  Responses from the web service comprise a numeric success code on a line on
  its own followed by some data, i.e.
    <success-code> CRLF <data>

  If registration succeeds the success code is '0' and the data is the program's
  registration key which is a string of 32 hex digits.

  If the registration fails the success code is a +ve error code and the data
  is an error message.

  If the client application accessing web service does not supply the required
  user agent then the service responds with a HTTP 403 (Forbidden) error.
}


const
  // Web service info
  cScriptName = 'register-app.php';           // script name
  cUserAgent = 'DelphiDabbler-Registrar-v2';  // user agent string


{ TRegistrar }

constructor TRegistrar.Create;
  {Class constructor. Initialises service.
  }
begin
  inherited Create(TWebServiceInfo.Create(cScriptName, cUserAgent));
end;

function TRegistrar.Submit(const Data: TStrings): string;
  {Sends application registration data to web service and interprets response.
    @param Data Application registration information as list of Name=Value data.
    @return Registration key.
  }
var
  Response: TStringList;  // response from server
  Query: TURIParams;
begin
  Query := nil;
  Response := TStringList.Create;
  try
    Query := TURIParams.Create(Data);
    PostQuery(Query, Response);
    Result := StrTrim(Response.Text);  // registration key
  finally
    Query.Free;
    Response.Free;
  end;
end;

end.
