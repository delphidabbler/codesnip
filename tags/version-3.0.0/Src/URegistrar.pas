{
 * URegistrar.pas
 *
 * Class that registers the application online.
 *
 * v0.1 of 07 Apr 2006  - Original version.
 * v1.0 of 24 May 2006  - Corrected and greatly extended comments.
 * v1.1 of 13 May 2009  - Changed to use revised TWebService inherited
 *                        constructor and its single TWebServiceInfo parameter.
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
 * The Original Code is URegistrar.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit URegistrar;


interface


uses
  // Delphi
  Classes,
  // Project
  UWebService;


type

  {
  TRegistrar
    Class that registers the application online.
  }
  TRegistrar = class(TDDabStdWebService)
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
  // Delphi
  SysUtils,
  // Project
  UWebInfo;


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
  cUserAgent = 'DelphiDabbler-Registrar-v1';  // user agent string


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
begin
  Response := TStringList.Create;
  try
    PostQuery(Data, Response);
    Result := Trim(Response.Text);  // registration key
  finally
    Response.Free;
  end;
end;

end.

