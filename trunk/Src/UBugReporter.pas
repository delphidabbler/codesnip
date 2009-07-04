{
 * UBugReporter.pas
 *
 * Class that interacts with bug report web service via HTTP to submit bug
 * reports.
 *
 * v0.1 of 20 Nov 2005  - Original version.
 * v0.2 of 04 Jan 2006  - Changed re altered inherited constructor parameters.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 *                      - Removed unused unit reference.
 * v1.1 of 13 Jan 2009  - Replaced control char literals with constants.
 * v1.2 of 13 May 2009  - Changed to use revised TWebService inherited
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
 * The Original Code is UBugReporter.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UBugReporter;


interface


uses
  // Delphi
  Classes,
  // Project
  UWebService;


type

  {
  TBugReporter:
    Class that sends bug reports to website's bug report web service via HTTP
    and interprets responses.
  }
  TBugReporter = class(TWebService)
  public
    constructor Create;
      {Class constructor. Initialises service.
      }
    procedure Submit(const BugData: TStrings);
      {Sends bug report data server and interprets response.
        @param BugData Bug information as list of Name=Value pairs.
        @except Raises EWebServiceError exception if error response received
          from web service.
        @except Raises EWebServiceFailure exception if response from web service
          is not recognised.
      }
  end;


implementation

uses
  // Delphi (Windows unit for inline expansion in Delphi 2006)
  SysUtils, Windows,
  // Project
  UConsts, UWebInfo;


{
  Web service notes: bug-report.php
  ---------------------------------

  This web service enables CodeSnip to report a bug directly to the
  DelphiDabbler website.

  The service accepts bug reports as a sequence of name=value pairs POSTed to
  the service. What data is submitted is entirely application defined and the
  data is not validated in any way. It is simply passed on to the author. This
  may change in future versions and the service may validate the data.

  Responses from the service take one of two forms:
    "OK" - this word on its own informs of a successful submission.
    "ERROR: <error-message>" - informs that an error occured. The text following
      the word "ERROR" describes the error.

  There are no specific commands - simply post the data of the report.

  If the client application accessing web service does not supply the required
  user agent then the service responds with a HTTP 403 (Forbidden) error.
}

const
  // Web service info
  cScriptName = 'bug-report.php';           // name of script
  cUserAgent = 'DelphiDabbler-BugReporter'; // user agent string


resourcestring
  // Error message from bug report script
  sErrorResponse = 'The bug report web service reported the following error:'
    + EOL2 + '%0:s.';
  // Error message re unknown bug report script response
  sUnrecognizedResponse = 'The bug report web service returned an unrecognized '
    + 'response.';


{ TBugReporter }

constructor TBugReporter.Create;
  {Class constructor. Initialises service.
  }
begin
  inherited Create(TWebServiceInfo.Create(cScriptName, cUserAgent));
end;

procedure TBugReporter.Submit(const BugData: TStrings);
  {Sends bug report data server and interprets response.
    @param BugData Bug information as list of Name=Value pairs.
    @except Raises EWebServiceError exception if error response received from
      web service.
    @except Raises EWebServiceFailure exception if response from web service is
      not recognised.
  }
var
  Response: TStringList;  // Response from server.
  ResponseStr: string;    // Response from server as string;
begin
  // Send command to server and get response as string
  Response := TStringList.Create;
  try
    PostQuery(BugData, Response);
    ResponseStr := Trim(Response.Text);
  finally
    FreeAndNil(Response);
  end;
  // Interpret response
  if AnsiPos('ERROR:', ResponseStr) = 1 then
    // server reported error: extract message and raise exception using it
    raise EWebServiceError.CreateFmt(
      sErrorResponse, [Copy(ResponseStr, 8, MaxInt)]
    )
  else if not AnsiSameText('OK', ResponseStr) then
    // didn't understand server's response
    raise EWebServiceFailure.Create(sUnrecognizedResponse);
end;

end.

