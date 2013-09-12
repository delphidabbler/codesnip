{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a class that provides basic interaction with DelphiDabbler
 * standard web services via HTTP.
}


unit Web.UStdWebService;


interface


uses
  // Delphi
  SysUtils, Classes,
  // Project
  UURIParams, Web.UBaseWebService;


type
  ///  <summary>Base class for web services that interact with standard
  ///  DelphiDabbler web services.</summary>
  ///  <remarks>
  ///  <para>Standard web services always place a numeric status code in first
  ///  line of the response followed by any resulting data. The status code is
  ///  zero for normal responses and non-zero when an error response is
  ///  returned.</para>
  ///  <para>This class outputs data for normal responses and generates
  ///  EWebServiceError exceptions for error responses, using the response data
  ///  as the error message. Malformed responses generate EWebServiceFailure
  ///  exceptions.</para>
  ///  </remarks>
  TStdWebService = class(TBaseWebService)
  strict protected

    ///  <summary>Converts string S into a form that is suitable for sending to
    ///  the a web service.</summary>
    class function SanitiseString(const S: string): string;

    ///  <summary>Analyses lines of text returned from web services and extracts
    ///  the data component of successful responses. Exceptions are raised for
    ///  error responses using the returned text as the exception message.
    ///  </summary>
    ///  <param name="Response">TStrings [in] When the method is called Response
    ///  must contain the lines of text received from the web service. After
    ///  processing the first line containing the status code is removed,
    ///  leaving only 0 or more lines of data.</param>
    ///  <exception>EWebServiceError raised on receipt of a valid error
    ///  response.</exception>
    ///  <exception>EWebServiceFailure raised if the format of the response is
    ///  invalid.</exception>
    procedure ProcessResponse(const Response: TStrings);

    ///  <summary>Posts a command to the web service and returns the data
    ///  component of the response in a string list.</summary>
    ///  <param name="Cmd">string [in] Command to be executed.</param>
    ///  <param name="Params">TURIParams [in] Parameters associated with the
    ///  command.</param>
    ///  <param name="Response">TStrings [in] String list set to reponse from
    ///  web service, stripped of the status code, where each line of the
    ///  response is a line of the string list.</param>
    ///  <exception>EWebServiceError raised on receipt of a valid error
    ///  response.</exception>
    ///  <exception>EWebServiceFailure raised if the web service returns an
    ///  invalid response.</exception>
    ///  <exception>EWebConnectionError raised if an EIdSocketError is
    ///  encountered.</exception>
    ///  <exception>EWebTransmissionError raised if response data is garbled in
    ///  transmission.</exception>
    ///  <exception>EHTTPError raised if an EIdHTTPProtocolException exception
    ///  is encountered.</exception>
    ///  <exception>Trapped EIdException or descendant exceptions are re-raised.
    ///  </exception>
    procedure PostCommand(const Cmd: string; const Params: TURIParams;
      const Response: TStrings);

    ///  <summary>Posts a query string to the web service and returns the data
    ///  component of the response in a string list.</summary>
    ///  <param name="Params">TURIParams [in] Parameters for inclusion in query
    ///  string.</param>
    ///  <param name="Response">TStrings [in] String list set to reponse from
    ///  web service, stripped of the status code, where each line of the
    ///  response is a line of the string list.</param>
    ///  <exception>EWebServiceError raised on receipt of a valid error
    ///  response.</exception>
    ///  <exception>EWebServiceFailure raised if the web service returns an
    ///  invalid response.</exception>
    ///  <exception>EWebConnectionError raised if an EIdSocketError is
    ///  encountered.</exception>
    ///  <exception>EWebTransmissionError raised if response data is garbled in
    ///  transmission.</exception>
    ///  <exception>EHTTPError raised if an EIdHTTPProtocolException exception
    ///  is encountered.</exception>
    ///  <exception>Trapped EIdException or descendant exceptions are re-raised.
    ///  </exception>
    procedure PostQuery(const Query: TURIParams; const Response: TStrings);

    ///  <summary>Posts raw data to the web service and returns the data
    ///  component of the response in a string list.</summary>
    ///  <param name="Data">TStream [in] Stream containing data to be posted.
    ///  </param>
    ///  <param name="Response">TStrings [in] String list set to reponse from
    ///  web service, stripped of the status code, where each line of the
    ///  response is a line of the string list.</param>
    ///  <exception>EWebServiceError raised on receipt of a valid error
    ///  response.</exception>
    ///  <exception>EWebServiceFailure raised if the web service returns an
    ///  invalid response.</exception>
    ///  <exception>EWebConnectionError raised if an EIdSocketError is
    ///  encountered.</exception>
    ///  <exception>EWebTransmissionError raised if response data is garbled in
    ///  transmission.</exception>
    ///  <exception>EHTTPError raised if an EIdHTTPProtocolException exception
    ///  is encountered.</exception>
    ///  <exception>Trapped EIdException or descendant exceptions are re-raised.
    ///  </exception>
    procedure PostData(const Data: TStream; const Response: TStrings); overload;

    ///  <summary>Posts raw data to the web service and returns the data
    ///  component of the response in a string list.</summary>
    ///  <param name="Data">TBytes [in] Byte array containing data to be posted.
    ///  </param>
    ///  <param name="Response">TStrings [in] String list set to reponse from
    ///  web service, stripped of the status code, where each line of the
    ///  response is a line of the string list.</param>
    ///  <exception>EWebServiceError raised on receipt of a valid error
    ///  response.</exception>
    ///  <exception>EWebServiceFailure raised if the web service returns an
    ///  invalid response.</exception>
    ///  <exception>EWebConnectionError raised if an EIdSocketError is
    ///  encountered.</exception>
    ///  <exception>EWebTransmissionError raised if response data is garbled in
    ///  transmission.</exception>
    ///  <exception>EHTTPError raised if an EIdHTTPProtocolException exception
    ///  is encountered.</exception>
    ///  <exception>Trapped EIdException or descendant exceptions are re-raised.
    ///  </exception>
    procedure PostData(const Data: TBytes; const Response: TStrings); overload;
  end;


implementation


uses
  // Project
  UStrUtils, Web.UExceptions;


{ TStdWebService }

resourcestring
  // Error messages
  sEmptyResponse = 'Empty response received from web service';
  sBadStatusCode = 'Web service returned response with bad status code';
  sUnrecognizedError = 'Unrecognized error notified by web service';

procedure TStdWebService.PostCommand(const Cmd: string;
  const Params: TURIParams; const Response: TStrings);
begin
  Params.Add('cmd', Cmd);
  PostQuery(Params, Response);
end;

procedure TStdWebService.PostData(const Data: TBytes; const Response: TStrings);
begin
  PostStrings(Data, Response);
  ProcessResponse(Response);
end;

procedure TStdWebService.PostData(const Data: TStream;
  const Response: TStrings);
begin
  PostStrings(Data, Response);
  ProcessResponse(Response);
end;

procedure TStdWebService.PostQuery(const Query: TURIParams;
  const Response: TStrings);
begin
  PostStrings(Query, Response);
  ProcessResponse(Response);
end;

procedure TStdWebService.ProcessResponse(const Response: TStrings);
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
    if StrTrim(Response.Text) = '' then
      raise EWebServiceFailure.Create(sUnrecognizedError);
    raise EWebServiceError.Create(StrTrim(Response.Text), StatusCode);
  end
end;

class function TStdWebService.SanitiseString(const S: string): string;
const
  IllegalChars = [#$00..#$1F, #$7F];
var
  Idx: Integer;
begin
  Result := S;
  for Idx := 1 to Length(S) do
    if CharInSet(Result[Idx], IllegalChars) then
      Result[Idx] := ' ';
end;

end.

