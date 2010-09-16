{
 * Web.UStdWebService.pas
 *
 * Implements a class that provides basic interaction with DelphiDabbler
 * standard web services via HTTP.
 *
 * $Rev$
 * $Date$
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
 * The Original Code is Web.UStdWebService.pas, formerly UWebService.pas then
 * NsWebServices.UDDabStandard.pas.
 * .
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit Web.UStdWebService;


interface


uses
  // Delphi
  Classes,
  // Project
  UURIParams, Web.UBaseWebService;


type

  {
  TStdWebService:
    Base class for web services that interact with standard DelphiDabbler web
    services.
    These web services always place a numeric status code in first line of the
    response followed by any resulting data. The status code is zero for normal
    responses and non-zero when an error response is returned. This class
    outputs data for normal responses and generates EWebServiceError exceptions
    for error responses, using the response data as the error message. Malformed
    responses generate EWebServiceFailure exceptions.
  }
  TStdWebService = class(TBaseWebService)
  strict protected
    procedure ProcessResponse(const Response: TStrings);
      {Analyses lines of text returned from from web service and extracts data
      from successful responses. Raises exceptions based on error messages from
      error responses.
        @param Response [in] Lines of text from response from web service. After
          processing status code is removed from top of list, leaving only
          returned data.
        @except Raises EWebServiceError on receipt of valid error response.
        @except Raises [in] EWebServiceFailure if response from web service has
          invalid format.
      }
    procedure PostCommand(const Cmd: string; const Params: TURIParams;
      const Response: TStrings);
      {Sends command to server and returns data component of response in a
      string list.
        @param Cmd [in] Command to be executed. Passed to server as Cmd=CmdName
          parameter.
        @param Params [in] Query string parameters.
        @param Response [in] Server's response as string list where each line of
          response is a line of string list.
        @except EWebServiceError raised on receipt of valid error response.
        @except EWebServiceFailure raised if web service sends invalid response.
        @except EWebConnectionError raised if EIdSocketError encoutered.
        @except EWebTransmissionError raised if data is garbled in transmission.
        @except EHTTPError raised if EIdHTTPProtocolException encountered.
        @except EIdException or descendant re-raised for other exception types.
      }
    procedure PostQuery(const Query: TURIParams; const Response: TStrings);
      {Sends a query string to server and returns data component of response in
      a string list.
        @param Query [in] Query string parameters.
        @param Response [in] Server's response as string list where each line of
          response is a line of string list.
        @except EWebServiceError raised on receipt of valid error response.
        @except EWebServiceFailure raised if web service sends invalid response.
        @except EWebConnectionError raised if EIdSocketError encoutered.
        @except EWebTransmissionError raised if data is garbled in transmission.
        @except EHTTPError raised if EIdHTTPProtocolException encountered.
        @except EIdException or descendant re-raised for other exception types.
      }
    procedure PostData(const Data: TStream; const Response: TStrings);
      {Posts raw data to server and returns data component of response in a
      string list.
        @param Data [in] Stream containing raw data to be posted.
        @param Response [in] Server's response as string list where each line of
          response is a line of string list.
        @except EWebServiceError raised on receipt of valid error response.
        @except EWebServiceFailure raised if web service sends invalid response.
        @except EWebConnectionError raised if EIdSocketError encoutered.
        @except EWebTransmissionError raised if data is garbled in transmission.
        @except EHTTPError raised if EIdHTTPProtocolException encountered.
        @except EIdException or descendant re-raised for other exception types.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  Web.UExceptions;


{ TStdWebService }

resourcestring
  // Error messages
  sEmptyResponse = 'Empty response received from web service';
  sBadStatusCode = 'Web service returned response with bad status code';
  sUnrecognizedError = 'Unrecognized error notified by web service';

procedure TStdWebService.PostCommand(const Cmd: string;
  const Params: TURIParams; const Response: TStrings);
  {Sends command to server and returns data component of response in a string
  list.
    @param Cmd [in] Command to be executed. Passed to server as Cmd=CmdName
      parameter.
    @param Params [in] Query string parameters.
    @param Response [in] Server's response as string list where each line of
      response is a line of string list.
    @except EWebServiceError raised on receipt of valid error response.
    @except EWebServiceFailure raised if web service sends invalid response.
    @except EWebConnectionError raised if EIdSocketError encoutered.
    @except EWebTransmissionError raised if data is garbled in transmission.
    @except EHTTPError raised if EIdHTTPProtocolException encountered.
    @except EIdException or descendant re-raised for other exception types.
  }
begin
  Params.Add('cmd', Cmd);
  PostQuery(Params, Response);
end;

procedure TStdWebService.PostData(const Data: TStream;
  const Response: TStrings);
  {Posts raw data to server and returns data component of response in a string
  list.
    @param Data [in] Stream containing raw data to be posted.
    @param Response [in] Server's response as string list where each line of
      response is a line of string list.
    @except EWebServiceError raised on receipt of valid error response.
    @except EWebServiceFailure raised if web service sends invalid response.
    @except EWebConnectionError raised if EIdSocketError encoutered.
    @except EWebTransmissionError raised if data is garbled in transmission.
    @except EHTTPError raised if EIdHTTPProtocolException encountered.
    @except EIdException or descendant re-raised for other exception types.
  }
begin
  PostStrings(Data, Response);
  ProcessResponse(Response);
end;

procedure TStdWebService.PostQuery(const Query: TURIParams;
  const Response: TStrings);
  {Sends a query string to server and returns data component of response in a
  string list.
    @param Query [in] Query string parameters.
    @param Response [in] Server's response as string list where each line of
      response is a line of string list.
    @except EWebServiceError raised on receipt of valid error response.
    @except EWebServiceFailure raised if web service sends invalid response.
    @except EWebConnectionError raised if EIdSocketError encoutered.
    @except EWebTransmissionError raised if data is garbled in transmission.
    @except EHTTPError raised if EIdHTTPProtocolException encountered.
    @except EIdException or descendant re-raised for other exception types.
  }
begin
  PostStrings(Query, Response);
  ProcessResponse(Response);
end;

procedure TStdWebService.ProcessResponse(const Response: TStrings);
  {Analyses lines of text returned from from web service and extracts data from
  successful responses. Raises exceptions based on error messages from error
  responses.
    @param Response [in] Lines of text from response from web service. After
      processing status code is removed from top of list, leaving only returned
      data.
    @except Raises EWebServiceError on receipt of valid error response.
    @except Raises [in] EWebServiceFailure if response from web service has
      invalid format.
  }
var
  StatusCode: Integer; // status code from response
begin
  // All responses have content => empty reponse is error
  if Response.Count = 0 then
    raise EWebServiceFailure.Create(sEmptyResponse);
  // All valid responses have status code as first line of response
  if not TryStrToInt(Response[0], StatusCode) then
    raise EWebServiceFailure.Create(sBadStatusCode);
  // We have got status code: chop it off to leave data in Response
  Response.Delete(0);
  // Check status code in first line: 0 => OK, otherwise error
  if StatusCode <> 0 then
  begin
    // Error response: raise web service error exception unless data doesn't
    // contain expected error message when failure exception is raised
    if Trim(Response.Text) = '' then
      raise EWebServiceFailure.Create(sUnrecognizedError);
    raise EWebServiceError.Create(Trim(Response.Text), StatusCode);
  end
end;

end.

