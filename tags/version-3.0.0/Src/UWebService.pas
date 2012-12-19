{
 * UWebService.pas
 *
 * A class that provides basic interaction with web services via HTTP, a
 * subclass that interacts with standard DelphiDabbler web services and some
 * exception classes raised by various kinds of web server error.
 *
 * v0.1 of 20 Nov 2005  - Original version.
 * v0.2 of 04 Jan 2006  - Modified to work with Indy components v10.1.1.
 *                      - Deleted now redundant Host parameter from constructor.
 * v0.3 of 10 Jan 2006  - Reverted to Indy components v9.
 * v0.4 of 10 Apr 2006  - Changed EHTTPError exception to use HTTP error code
 *                        and description as error message rather than content
 *                        of error pages returned from server.
 * v0.5 of 30 Apr 2006  - Added support for download progress reporting.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 * v1.1 of 25 Aug 2008  - Added override of ECodeSnip's new Assign method to
 *                        EWebServiceError and EHTTPError that updates custom
 *                        fields.
 * v1.2 of 30 Nov 2008  - Added new PostData method to both web service classes
 *                        to post raw data to a web service.
 *                      - Added new optional MediaType parameter to web service
 *                        constructor.
 * v1.3 of 13 Jan 2009  - Replaced control char literals with constants.
 *                      - Made private and protected sections strict.
 *                      - Now use ClassName in Assert statements.
 * v1.4 of 13 May 2009  - Changed TWebService constructor to take a single
 *                        TWebServiceInfo that encapsulates  information
 *                        previously provided by former string parameters.
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
 * The Original Code is UWebService.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UWebService;


interface


uses
  // Delphi
  SysUtils, Classes,
  // Indy
  IdHTTP, IdException,
  // Project
  UExceptions, UDownloadMonitor, UWebInfo;


type

  {
  TWebServiceProgressEvent:
    Type of event handler used to handle TWebService.OnProgress events. Used to
    monitor download progress.
      @param Sender [in] Reference to TWebService object triggering event.
      @param BytesToDate [in] Number of bytes downloaded.
      @param BytesExpected [in] Total number of bytes to be downloaded.
  }
  TWebServiceProgressEvent = procedure(Sender: TObject; const BytesToDate,
    BytesExpected: Integer) of object;

  {
  TWebService:
    Class that provides basic interaction with web services via HTTP. Assumes
    that all responses will have Mime type of 'text/*'.
  }
  TWebService = class(TObject)
  strict private
    fHTTP: TIdHTTP;
      {Component used to access server via HTTP}
    fScriptURI: string;
      {URI of web service script}
    fDownloadMonitor: TDownloadMonitor;
      {Object that monitors and notifies download progress}
    fOnProgress: TWebServiceProgressEvent;
      {References event handler of OnProgress event}
  strict protected
    procedure TriggerProgressEvent;
      {Triggers OnProgress event. Helper method provided to be called by
      descendant classes for operations where monitoring of download progress is
      required.
      }
    procedure DoProgress; virtual;
      {Called when download progress information changes. Does nothing.
      Descendants can trigger OnProgress events by calling the
      TriggerProgressEvent method.
      }
    procedure HandleException(const E: EIdException);
      {Handles likely exceptions generated by HTTP component and translates into
      own exception objects that are not detected as bugs and have more user
      friendly error messages.
        @param E [in] Exception to translate.
        @except EHTTPError raised if EIdHTTPProtocolException encountered.
        @except EWebConnectionError raised if EIdSocketError encoutered.
        @except EIdException or descendants re-raised for other exception types.
      }
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
    procedure PostCommand(const Cmd: string; const Params: array of string;
      const Response: TStrings); virtual;
      {Sends a command to server and returns lines of response in a string list.
        @param Cmd [in] Command to be executed. Passed to server as Cmd=CmdName
          parameter.
        @param Params [in] List of parameters in form ParamName=ParamValue. []
          if no parameters.
        @param Response [in] Server's response as string list where each line of
          response is a line of string list.
        @except EWebConnectionError raised if EIdSocketError encoutered.
        @except EHTTPError raised if EIdHTTPProtocolException encountered.
        @except EIdException or descendant re-raised for other exception types.
      }
    procedure PostQuery(const QS: TStrings; const Response: TStrings); virtual;
      {Sends a query string to server and returns lines of response in a string
      list.
        @param QS [in] List of parameters in Name=Value format.
        @param Response [in] Server's response as string list where each line of
          response is a line of string list.
        @except EWebConnectionError raised if EIdSocketError encoutered.
        @except EHTTPError raised if EIdHTTPProtocolException encountered.
        @except EIdException or descendant re-raised for other exception types.
      }
    procedure PostData(const Data: TStream; const Response: TStrings); virtual;
      {Posts raw data to server and returns lines of response in a string list.
        @param Data [in] Stream containing raw data to be posted.
        @param Response [in] Server's response as string list where each line of
          response is a line of string list.
        @except EWebConnectionError raised if EIdSocketError encoutered.
        @except EHTTPError raised if EIdHTTPProtocolException encountered.
        @except EIdException or descendant re-raised for other exception types.
      }
  public
    constructor Create(const Service: TWebServiceInfo);
      {Class constructor. Sets up object.
        @param Service [in] Description of web service.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    property OnProgress: TWebServiceProgressEvent
      read fOnProgress write fOnProgress;
      {Event that can be triggered to monitor progress of downloads. Not
      triggered by default. Descendant classes can trigger the event by
      overriding the DoProgress method and calling TriggerProgressEvent inside
      the overridden method}
  end;

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
  TDDabStdWebService = class(TWebService)
  strict protected
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
      const Response: TStrings); override;
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
      override;
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
    procedure PostData(const Data: TStream; const Response: TStrings); override;
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

  {
  EWebService:
    Base class for all errors generated by web services. None of these
    exceptions are treated as bugs.
  }
  EWebService = class(ECodeSnip);

  {
  EHTTPError:
    Exception raised when web server HTTP error is detected. Note these errors
    relate to the web server, not the web service.
  }
  EHTTPError = class(EWebService)
  strict private
    fHTTPErrorCode: Integer;
      {Stores value of HTTPErrorCode property}
  public
    constructor Create(const E: EIdHTTPProtocolException); overload;
      {Class constructor. Creates object from properties of given exception.
        @param E [in] Instance of exception from which to create this exception.
          E.ReplyErrorCode is stored in HTTPErrorCode property and E.Message is
          stored in Message property.
      }
    procedure Assign(const E: Exception); override;
      {Assigns properties of another exception to this one.
        @param E [in] Exception whose properties are to be copied. Must be an
          EHTTPError or an EIdHTTPProtocolException instance.
      }
    property HTTPErrorCode: Integer read fHTTPErrorCode;
      {HTTP error code from web server}
  end;

  {
  EWebConnectionError:
    Exception raised when there is a problem connecting to web server (i.e.
    socket error).
  }
  EWebConnectionError = class(EWebService);

  {
  EWebServiceFailure:
    Exception raised when the web service fails to response as expected.
  }
  EWebServiceFailure = class(EWebService);

  {
  EWebServiceError:
    Exception raised when a web service returns an error condition as a response
    to a command. It has a non-zero error code in addition to the error message.
  }
  EWebServiceError = class(EWebService)
  strict private
    fErrorCode: Integer;
      {Value of ErrorCode property}
  public
    constructor Create(const Msg: string; const ErrorCode: Integer = -1);
      overload;
      {Class constructor. Constructs exception object with an error code in
      addition to standard error message.
        @param Message [in] Error message.
        @param ErrorCode [in] Optional non-zero error code (defaults to -1).
      }
    constructor CreateFmt(const Fmt: string; const Args: array of const;
      const ErrorCode: Integer = -1); overload;
      {Class constructor. Constructs exception object with an error code in
      addition to message built from format string and arguments.
        @param Fmt [in] Format for message string.
        @param Args [in] Arguments to be included in formatted message string.
        @param ErrorCode [in] Optional non-zero error code (defaults to -1).
      }
    procedure Assign(const E: Exception); override;
      {Assigns properties of another exception to this one.
        @param E [in] Exception whose properties are to be copied. Must be an
          EWebServiceError instance.
      }
    property ErrorCode: Integer read fErrorCode;
      {Non-zero error code}
  end;


implementation


uses
  // Project
  UConsts;


resourcestring
  // Error messages
  sWebConnectionError = 'There was a problem accessing the internet. Please '
    + 'check your web connection.'
    + EOL2
    + 'The error reported by Windows was: %0:s';


{ TWebService }

constructor TWebService.Create(const Service: TWebServiceInfo);
  {Class constructor. Sets up object.
    @param Service [in] Description of web service.
  }
begin
  inherited Create;
  // Create and initialise HTTP client component
  fHTTP := TIdHTTP.Create(nil);
  fHTTP.Request.UserAgent := Service.UserAgent;
  fHTTP.Request.Accept := Service.MediaType;
  // Create object that monitors download progress
  fDownloadMonitor := TDownloadMonitor.Create(fHTTP, DoProgress);
  // Record script URI for future use
  fScriptURI := Service.ScriptURI;
end;

function TWebService.CreateQueryStrings(const Cmd: string;
  const Params: array of string): TStrings;
  {Creates a string list containing query strings to be sent to server. Note
  that this TStrings object must be freed by caller.
    @param Cmd [in] Command to be executed. Passed to server as Cmd=CmdName
      parameter.
    @param Params [in] List of parameters in form ParamName=ParamValue. [] if no
      parameters.
    @return String list containing query strings to be sent to server.
  }
var
  I: Integer; // loops through parameters
begin
  Result := TStringList.Create;
  Result.Add('cmd=' + Cmd);
  for I := Low(Params) to High(Params) do
    Result.Add(Params[I]);
end;

destructor TWebService.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fDownloadMonitor);
  FreeAndNil(fHTTP);
  inherited;
end;

procedure TWebService.DoProgress;
  {Called when download progress information changes. Does nothing. Descendants
  can trigger OnProgress events by calling the TriggerProgressEvent method.
  }
begin
  // Do nothing: descendant may call TriggerProgressEvent here if event required
  // (or do any other required processing)
end;

procedure TWebService.HandleException(const E: EIdException);
  {Handles likely exceptions generated by HTTP component and translates into own
  exception objects that are not detected as bugs and have more user friendly
  error messages.
    @param E [in] Exception to translate.
    @except EHTTPError raised if EIdHTTPProtocolException encountered.
    @except EWebConnectionError raised if EIdSocketError encoutered.
    @except EIdException or descendants re-raised for other exception types.
  }
begin
  if E is EIdHTTPProtocolException then
    raise EHTTPError.Create(E as EIdHTTPProtocolException)
  else if E is EIdSocketError then
    raise EWebConnectionError.CreateFmt(sWebConnectionError, [Trim(E.Message)])
  else
    raise E;
end;

procedure TWebService.PostCommand(const Cmd: string;
  const Params: array of string; const Response: TStrings);
  {Sends a command to server and returns lines of response in a string list.
    @param Cmd [in] Command to be executed. Passed to server as Cmd=CmdName
      parameter.
    @param Params [in] List of parameters in form ParamName=ParamValue. [] if no
      parameters.
    @param Response [in] Server's response as string list where each line of
      response is a line of string list.
    @except EWebConnectionError raised if EIdSocketError encoutered.
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

procedure TWebService.PostData(const Data: TStream; const Response: TStrings);
  {Posts raw data to server and returns lines of response in a string list.
    @param Data [in] Stream containing raw data to be posted.
    @param Response [in] Server's response as string list where each line of
      response is a line of string list.
    @except EWebConnectionError raised if EIdSocketError encoutered.
    @except EHTTPError raised if EIdHTTPProtocolException encountered.
    @except EIdException or descendant re-raised for other exception types.
  }
begin
  try
    Response.Text := fHTTP.Post(fScriptURI, Data);
  except
    on E: EIdException do
      HandleException(E);
  end;
end;

procedure TWebService.PostQuery(const QS, Response: TStrings);
  {Sends a query string to server and returns lines of response in a string
  list.
    @param QS [in] List of parameters in Name=Value format.
    @param Response [in] Server's response as string list where each line of
      response is a line of string list.
    @except EWebConnectionError raised if EIdSocketError encoutered.
    @except EHTTPError raised if EIdHTTPProtocolException encountered.
    @except EIdException or descendant re-raised for other exception types.
  }
begin
  try
    Response.Text := fHTTP.Post(fScriptURI, QS);
  except
    on E: EIdException do
      HandleException(E);
  end;
end;

procedure TWebService.TriggerProgressEvent;
  {Triggers OnProgress event. Helper method provided to be called by descendant
  classes for operations where monitoring of download progress is required.
  }
begin
  if Assigned(fOnProgress) then
    fOnProgress(
      Self, fDownloadMonitor.BytesReceived, fDownloadMonitor.BytesExpected
    );
end;

{ TDDabStdWebService }

resourcestring
  // Error messages
  sEmptyResponse = 'Empty response received from web service';
  sBadStatusCode = 'Web service returned response with bad status code';
  sUnrecognizedError = 'Unrecognized error notified by web service';

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
begin
  inherited; // Calls this class' version of PostQuery
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
  inherited;
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
begin
  inherited;
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


{ EHTTPError }

procedure EHTTPError.Assign(const E: Exception);
  {Assigns properties of another exception to this one.
    @param E [in] Exception whose properties are to be copied. Must be an
      EHTTPError or an EIdHTTPProtocolException instance.
  }
begin
  Assert(                                                  // ** do not localise
    (E is EHTTPError) or (E is EIdHTTPProtocolException),
    ClassName + '.Assign: E must be EHTTPError or EIdHTTPProtocolException');
  inherited;
  if E is EHTTPError then
    fHTTPErrorCode := (E as EHTTPError).fHTTPErrorCode
  else
    fHTTPErrorCode := (E as EIdHTTPProtocolException).ReplyErrorCode;
end;

constructor EHTTPError.Create(const E: EIdHTTPProtocolException);
  {Class constructor. Creates object from properties of given exception.
    @param E [in] Instance of exception from which to create this exception.
      E.ReplyErrorCode is stored in HTTPErrorCode property and E.Message is
      stored in Message property.
  }
begin
  inherited Create(E.Message);
  fHTTPErrorCode := E.ReplyErrorCode; // ** Indy 10: use E.ErrorCode;
end;


{ EWebServiceError }

procedure EWebServiceError.Assign(const E: Exception);
  {Assigns properties of another exception to this one.
    @param E [in] Exception whose properties are to be copied. Must be an
      EWebServiceError instance.
  }
begin
  Assert(E is EWebServiceError,                            // ** do not localise
    ClassName + '.Assign: E must be EWebServiceError');
  inherited;
  fErrorCode := (E as EWebServiceError).fErrorCode;
end;

constructor EWebServiceError.Create(const Msg: string;
  const ErrorCode: Integer);
  {Class constructor. Constructs exception object with an error code in addition
  to standard error message.
    @param Message [in] Error message.
    @param ErrorCode [in] Optional non-zero error code (defaults to -1).
  }
begin
  Assert(ErrorCode <> 0,                                   // ** do not localise
    ClassName + '.Create: zero error code');
  inherited Create(Msg);
  fErrorCode := ErrorCode;
end;

constructor EWebServiceError.CreateFmt(const Fmt: string;
  const Args: array of const; const ErrorCode: Integer);
  {Class constructor. Constructs exception object with an error code in addition
  to message built from format string and arguments.
    @param Fmt [in] Format for message string.
    @param Args [in] Arguments to be included in formatted message string.
    @param ErrorCode [in] Optional non-zero error code (defaults to -1).
  }
begin
  Assert(ErrorCode <> 0,                                   // ** do not localise
    ClassName + '.CreateFmt: zero error code');
  Create(Format(Fmt, Args), ErrorCode);
end;

end.
