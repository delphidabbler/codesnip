{
 * UMailListSubscriber.pas
 *
 * Implements class that interacts with mailing list subscription web service to
 * subscribe user to CodeSnip mailing list.
 *
 * v1.0 of 18 Nov 2006  - Original version.
 * v1.1 of 13 May 2009  - Changed to use revised TWebService inherited
 *                        constructor and its single TWebServiceInfo parameter.
 *                      - Assert now uses ClassName instead of string literal.
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
 * The Original Code is UMailListSubscriber.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UMailListSubscriber;


interface


uses
  // Project
  UWebService;


type

  {
  TMailListSubscriber:
    Class that interacts with mailing list subscription web service to subscribe
    user to CodeSnip mailing list.
  }
  TMailListSubscriber = class(TDDabStdWebService)
  public
    constructor Create;
      {Class constructor. Initialises service.
      }
    function Subscribe(const Email, Name: string): string;
      {Sends subscription request to web service and gathers response.
        @param Email [in] Email address to be subscribed.
        @param Name [in] Name of email account holder. May be ''.
        @return Message returned from web service in response to request.
        @except Raises EWebServiceFailure if response from web service has
          invalid format.
        @except Raises EWebServiceError on receipt of valid error response.
        @except Raises EMailListSubscriber if response not in expected format.
      }
  end;

  {
  EMailListSubscriber:
    Class of exception raised when errors detected in TMailListSubscriber.
  }
  EMailListSubscriber = class(EWebService);


implementation


uses
  // Delphi
  SysUtils, Classes,
  // Project
  UWebInfo;


const
  // Web service info
  cScriptName = 'maillist-subs.php';        // script name
  cUserAgent = 'DelphiDabbler-MLSubs-v1';   // user agent string


resourcestring
  // Error message
  sBadRespone = 'Invalid response from mail list web service "subscribe" '
    + 'command.';


{ TMailListSubscriber }

constructor TMailListSubscriber.Create;
  {Class constructor. Initialises service.
  }
begin
  inherited Create(TWebServiceInfo.Create(cScriptName, cUserAgent));
end;

function TMailListSubscriber.Subscribe(const Email, Name: string): string;
  {Sends subscription request to web service and gathers response.
    @param Email [in] Email address to be subscribed.
    @param Name [in] Name of email account holder. May be ''.
    @return Message returned from web service in response to request.
    @except Raises EWebServiceFailure if response from web service has invalid
      format.
    @except Raises EWebServiceError on receipt of valid error response.
    @except Raises EMailListSubscriber if response not in expected format.
  }
var
  Response: TStringList;  // valid response from web service
begin
  Assert(Email <> '', ClassName + '.Subscribe: Email required');
  // Send subscribe command to web service and gather response
  Response := TStringList.Create;
  try
    PostCommand(
      'subscribe',
      ['listid=codesnip', 'name=' + Name, 'email=' + Email],
      Response
    );
    // Response must be at least two lines: 1st line is success status code. 2nd
    // and subsequent lines are response message
    if Response.Count < 2 then
      raise EMailListSubscriber.Create(sBadRespone);
    // Discard success status code: we don't use this
    Response.Delete(0);
    // Return message that follows success status code
    Result := Trim(Response.Text);
  finally
    FreeAndNil(Response);
  end;
end;

end.

