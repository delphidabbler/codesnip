{
 * UWebService.pas
 *
 * A class that provides basic interaction with web services via HTTP, a
 * subclass that interacts with standard DelphiDabbler web services and some
 * exception classes raised by various kinds of web server error.
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
 * The Original Code is UWebService.pas
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


unit UWebService;


interface


uses
  // Delphi
  SysUtils, Classes,
  // Project
  NsWebServices.UBase, UURIParams;


type

  {
  TDDabStdWebService:
    Web service class that interacts with standard DelphiDabbler web services.
    These services always place a numeric status code in first line of response
    followed by any resulting data. Status code is zero for normal responses and
    non-zero when an error response is returned. This class outputs data for
    normal responses and generates EWebServiceError exceptions for error
    responses, using the response data as the error message. Malformed responses
    generate EWebServiceFailure exceptions.
  }
  TDDabStdWebService = class(TBaseWebService)
  strict protected
    function StringsToParams(const Strings: TStrings): TURIParams;
      // todo: comment this and note user must free returned object
    function CreateQueryStrings(const Cmd: string;
      const Params: array of string): TStrings;
      {Creates a string list containing query strings to be sent to server. Note
      that this TStrings object must be freed by caller.
        @param Cmd [in] Command to be executed. Passed to server as Cmd=CmdName
          parameter.
        @param Params [in] List of parameters in form ParamName=ParamValue. []
          if no parameters.
        @return String list containing query strings to be sent to server.
      }
    procedure ProcessResponse(const Response: TStrings);
      {Analyses lines of text returned from from web service and extracts data
      from successful responses. Raises exceptions based on error messages from
      error responses.
        @param Response [in] Receives lines of text received as response from
          web service. After processing error code is removed from top of list,
          leaving only returned data.
        @except Raises [in] EWebServiceFailure if response from web service has
          invalid format.
        @except Raises EWebServiceError on receipt of valid error response.
      }
    procedure PostCommand(const Cmd: string; const Params: array of string;
      const Response: TStrings);
      {Sends command to server and returns data component of response in a
      string list.
        @param Cmd [in] Command to be executed. Passed to server as Cmd=CmdName
          parameter.
        @param Params [in] List of parameters in form ParamName=ParamValue. []
          if no parameters.
        @param Response [in] Server's response as string list where each line of
          response is a line of string list.
        @except EWebServiceError raised on receipt of valid error response.
        @except EWebServiceFailure raised if web service sends invalid response.
        @except EWebConnectionError raised EIdSocketError encoutered.
        @except EHTTPError raised if EIdHTTPProtocolException encountered.
        @except EIdException or descendant re-raised for other exception types.
      }
    procedure PostQuery(const QS: TStrings; const Response: TStrings);
      {Sends a query string to server and returns data component of response in
      a string list.
        @param QS [in] List of parameters in Name=Value format.
        @param Response [in] Server's response as string list where each line of
          response is a line of string list.
        @except EWebServiceError raised on receipt of valid error response.
        @except EWebServiceFailure raised if web service sends invalid response.
        @except EWebConnectionError raised EIdSocketError encoutered.
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
        @except EWebConnectionError raised EIdSocketError encoutered.
        @except EHTTPError raised if EIdHTTPProtocolException encountered.
        @except EIdException or descendant re-raised for other exception types.
      }
  end;


implementation


uses
  // Project
  NsWebServices.UExceptions;


{ TDDabStdWebService }

resourcestring
  // Error messages
  sEmptyResponse = 'Empty response received from web service';
  sBadStatusCode = 'Web service returned response with bad status code';
  sUnrecognizedError = 'Unrecognized error notified by web service';

function TDDabStdWebService.CreateQueryStrings(const Cmd: string;
  const Params: array of string): TStrings;
var
  I: Integer; // loops through parameters
begin
  Result := TStringList.Create;
  Result.Add('cmd=' + Cmd);
  for I := Low(Params) to High(Params) do
    Result.Add(Params[I]);
end;

procedure TDDabStdWebService.PostCommand(const Cmd: string;
  const Params: array of string; const Response: TStrings);
  {Sends command to server and returns data component of response in a string
  list.
    @param Cmd [in] Command to be executed. Passed to server as Cmd=CmdName
      parameter.
    @param Params [in] List of parameters in form ParamName=ParamValue. [] if no
      parameters.
    @param Response [in] Server's response as string list where each line of
      response is a line of string list.
    @except EWebServiceError raised on receipt of valid error response.
    @except EWebServiceFailure raised if web service sends invalid response.
    @except EWebConnectionError raised EIdSocketError encoutered.
    @except EHTTPError raised if EIdHTTPProtocolException encountered.
    @except EIdException or descendant re-raised for other exception types.
  }
var
  QS: TStrings; // list of query strings
begin
  QS := CreateQueryStrings(Cmd, Params);
  try
    PostQuery(QS, Response);
  finally
    FreeAndNil(QS);
  end;
end;

procedure TDDabStdWebService.PostData(const Data: TStream;
  const Response: TStrings);
  {Posts raw data to server and returns data component of response in a string
  list.
    @param Data [in] Stream containing raw data to be posted.
    @param Response [in] Server's response as string list where each line of
      response is a line of string list.
    @except EWebServiceError raised on receipt of valid error response.
    @except EWebServiceFailure raised if web service sends invalid response.
    @except EWebConnectionError raised EIdSocketError encoutered.
    @except EHTTPError raised if EIdHTTPProtocolException encountered.
    @except EIdException or descendant re-raised for other exception types.
  }
begin
  inherited PostStrings(Data, Response);
  ProcessResponse(Response);
end;

procedure TDDabStdWebService.PostQuery(const QS, Response: TStrings);
  {Sends a query string to server and returns data component of response in a
  string list.
    @param QS [in] List of parameters in Name=Value format.
    @param Response [in] Server's response as string list where each line of
      response is a line of string list.
    @except EWebServiceError raised on receipt of valid error response.
    @except EWebServiceFailure raised if web service sends invalid response.
    @except EWebConnectionError raised EIdSocketError encoutered.
    @except EHTTPError raised if EIdHTTPProtocolException encountered.
    @except EIdException or descendant re-raised for other exception types.
  }
var
  Params: TURIParams;
begin
  Params := StringsToParams(QS);
  try
    inherited PostStrings(Params, Response);
  finally
    Params.Free;
  end;
  ProcessResponse(Response);
end;

procedure TDDabStdWebService.ProcessResponse(const Response: TStrings);
  {Analyses lines of text returned from from web service and extracts data from
  successful responses. Raises exceptions based on error messages from error
  responses.
    @param Response [in] Receives lines of text received as response from web
      service. After processing error code is removed from top of list, leaving
      only returned data.
    @except Raises [in] EWebServiceFailure if response from web service has
      invalid format.
    @except Raises EWebServiceError on receipt of valid error response.
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
    // Error response
    // data should contain error message
    if Trim(Response.Text) = '' then
      // no error message
      raise EWebServiceFailure.Create(sUnrecognizedError);
    // valid error message: raise exception using it
    raise EWebServiceError.Create(Trim(Response.Text), StatusCode);
  end
end;

function TDDabStdWebService.StringsToParams(
  const Strings: TStrings): TURIParams;
var
  Idx: Integer;
begin
  Result := TURIParams.Create;
  for Idx := 0 to Pred(Strings.Count) do
    Result.Add(Strings.Names[Idx], Strings.ValueFromIndex[Idx]);
end;

end.

