{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a class that interfaces with the DelphiDabbler program
 * registration web service.
}


unit Web.URegistrar;


interface


uses
  // Delphi
  Classes,
  // Project
  Web.UStdWebService;


type
  ///  <summary>Provides an interface to the DelphiDabbler program registration
  ///  web service.</summary>
  TRegistrar = class(TStdWebService)
  public
    ///  <summary>Creates a new object instance with the correct URL and
    ///  suitable user agent for accessing the web service.</summary>
    constructor Create;
    ///  <summary>Sends regisration data to the web service and interprets its
    ///  response.</summary>
    ///  <param name="Data">TStrings [in] Registration information as a list of
    ///  Name=Value data items.</param>
    ///  <returns>string. Program's registration key returned from the web
    ///  service.</returns>
    function Submit(const Data: TStrings): string;
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
  cScriptURLTplt = 'http://%s/websvc/register-app';
  cUserAgent = 'DelphiDabbler-Registrar-v2';


{ TRegistrar }

constructor TRegistrar.Create;
begin
  inherited Create(TWebServiceInfo.Create(cScriptURLTplt, cUserAgent));
end;

function TRegistrar.Submit(const Data: TStrings): string;
var
  Response: TStringList;  // response from server
  Query: TURIParams;      // query made from Name=Value pairs in Data
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

