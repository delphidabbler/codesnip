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
 * Implements a class that interfaces with a web service to update the local
 * copy of the Code Snippets Database.
}


unit Web.UDBDownloadMgr;


interface


uses
  // Delphi
  Classes,
  // Project
  UEncodings, UURIParams, Web.UExceptions, Web.UStdWebService;


type
  ///  <summary>Provides an interface to the DelphiDabbler Code Snippets
  ///  Database update web service.</summary>
  ///  <remarks>This class provides a public method for every command exposed by
  ///  the web service.</remarks>
  TDBDownloadMgr = class sealed(TStdWebService)
  strict private

    ///  <summary>Converts EWebError exceptions into EDBDownloadMgr exceptions
    ///  with both long and short descriptions.</summary>
    ///  <param name="E">EWebError [in] Exception to be converted.</param>
    ///  <exception>Raises an EDBDownloadMgr exception based on the information
    ///  provided by exception E</exception>.
    procedure HandleException(const E: EWebError);

    ///  <summary>Includes the standard parameters required by every call to the
    ///  web service in the given parameter list.</summary>
    procedure IncludeStdParams(const Params: TURIParams);

    ///  <summary>Posts a command to the web service that includes the standard
    ///  parameters and returns the web service's response in a string list.
    ///  </summary>
    ///  <param name="Cmd">string [in] Command to be sent to web service.
    ///  </param>
    ///  <param name="Response">TStrings [in] String list that receives the
    ///  response from the web service as lines of text.</param>
    ///  <exception>EDBDownloadMgr taised if an EWebError exception is
    ///  encountered.</exception>
    procedure PostStdCommand(const Cmd: string; const Response: TStrings);

    ///  <summary>Posts a command to the web service and returns the data
    ///  component of the web service's response in a string list.</summary>
    ///  <param name="Cmd">string [in] Command to be sent to web service.
    ///  </param>
    ///  <param name="Params">TURIParams [in] Parameters to be posted to the
    ///  web service.</param>
    ///  <param name="Response">TStrings [in] String list that receives the
    ///  response from the web service as lines of text.</param>
    ///  <exception>EDBDownloadMgr raised if an EWebError exception is
    ///  encountered.</exception>
    procedure SafePostCommand(const Cmd: string; const Params: TURIParams;
      const Response: TStrings);

  public

    ///  <summary>Creates a new object instance with the correct URL and
    ///  suitable user agent.</summary>
    constructor Create;

    ///  <summary>Logs on to the web service.</summary>
    ///  <param name="Caller">string [in] Specifies from where the web service
    ///  is called.</param>
    ///  <param name="WantProgress">Boolean [in] Flag that indicates if an
    ///  OnProgress event is to be triggered while the web service's response is
    ///  being downloaded.</param>
    ///  <exception>EDBDownloadMgr raised if an EWebError exception is
    ///  encountered.</exception>
    procedure LogOn(const Caller: string; const WantProgress: Boolean = False);

    ///  <summary>Logs off from the web server.</summary>
    ///  <exception>EDBDownloadMgr raised if an EWebError exception is
    ///  encountered.</exception>
    ///  <param name="WantProgress">Boolean [in] Flag that indicates if an
    ///  OnProgress event is to be triggered while the web service's response is
    ///  being downloaded.</param>
    procedure LogOff(const WantProgress: Boolean = False);

    ///  <summary>Gets the date of the last update to the online Code Snippets
    ///  Database from the web service.</summary>
    ///  <param name="WantProgress">Boolean [in] Flag that indicates if an
    ///  OnProgress event is to be triggered while the web service's response is
    ///  being downloaded.</param>
    ///  <returns>string. Last update date as a Unix date stamp converted to a
    ///  string.</returns>
    ///  <exception>EDBDownloadMgr raised if an EWebError exception is
    ///  encountered.</exception>
    function LastUpdate(const WantProgress: Boolean = False): string;

    ///  <summary>Gets the number of files on the online Code Snippets Database
    ///  from the web service.</summary>
    ///  <param name="WantProgress">Boolean [in] Flag that indicates if an
    ///  OnProgress event is to be triggered while the web service's response is
    ///  being downloaded.</param>
    ///  <returns>Integer. Required file count.</returns>
    ///  <exception>EDBDownloadMgr raised if an EWebError exception is
    ///  encountered.</exception>
    function FileCount(const WantProgress: Boolean = False): Integer;

    ///  <summary>Gets a block of data containing the whole of the online Code
    ///  Snippets Database from the web service.</summary>
    ///  <param name="WantProgress">Boolean [in] Flag that indicates if an
    ///  OnProgress event is to be triggered while the web service's response is
    ///  being downloaded.</param>
    ///  <returns>TEncodedData. The downloaded data as text in a specified
    ///  encoding.</returns>
    ///  <exception>EDBDownloadMgr raised if an EWebError exception is
    ///  encountered.</exception>
    function GetDatabase(const WantProgress: Boolean = False): TEncodedData;
  end;

type
  ///  <summary>Class of exception raised by TDBDownloadMgr.</summary>
  ///  <remarks>Contains both a full and abbreviated description of the
  ///  exception. The full description is in the inherited Message property and
  ///  the abbreviated version is in the ShortMsg property.</remarks>
  EDBDownloadMgr = class(EWebService)
  strict private
    var
      ///  <summary>Value of ShortMsg property.</summary>
      fShortMsg: string;
  public
    ///  <summary>Constructs a new exception instance.</summary>
    ///  <param name="ShortMsg">string [in] Abbreviated exception description.
    ///  </param>
    ///  <param name="Msg">string [in] Full exception description.</param>
    constructor Create(const ShortMsg, Msg: string); overload;
    ///  <summary>Constructs a new exception instance.</summary>
    ///  <param name="ShortMsg">string [in] Abbreviated exception description.
    ///  </param>
    ///  <param name="Fmt">string [in] Format string for full exception
    ///  description.</param>
    ///  <param name="Args">array of const [in] Parameters for Fmt.</param>
    constructor CreateFmt(const ShortMsg, Fmt: string;
      const Args: array of const); overload;
    ///  <summary>Abbreviated description of exception.</summary>
    property ShortMsg: string read fShortMsg;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  CS.Init.CommandLineOpts,
  UAppInfo,
  UConsts,
  UStrUtils,
  USystemInfo,
  Web.UCharEncodings,
  Web.UInfo;


{
  Web service notes: codesnip-updt.php v5
  =======================================

  This web service enables CodeSnip to check if updated files are available in
  on-line database and to update local database accordingly.

  The service accepts POSTed commands which must each have form:
    cmd=<command> [<params>]
  where <params> is a list of parameters in form param-name=param-value.

  All responses from the v5 service have two parts:
  + Successful responses have '0' on first line followed by optional lines of
    data resulting from command.
  + Error responses have +ve error code on first line and error message on 2nd
    line.

  The web service expects a user agent of "DelphiDabbler-CodeSnip-Updater-v5"
  and will return a 403 "Forbidden" error if this is not provided.

  Table of Commands
  +----------------------------------------------------------------------------+
  |Command    |Queries|Values                |Response                         |
  +-----------+-------|----------------------+---------------------------------|
  |logon      |cmd    |"logon"               |OK: Stream of news items which   |
  |           |progid |unique id of program  |is ignored                       |
  |           |version|program version number|ERROR: CSUPDT_ERR_STDPARAMS if   |
  |           |os     |operating system info |required params not provided     |
  |           |browser|version of IE browser |                                 |
  |           |caller |application defined   |                                 |
  |           |       |string                |                                 |
  +-----------+-------+----------------------+---------------------------------+
  |filecount  |cmd    |"filecount"           |OK: Integer indication number of |
  |           |progid |unique id of program  |files in remote database         |
  |           |version|program version number|ERROR: CSUPDT_ERR_STDPARAMS if   |
  |           |       |                      |required params not provided     |
  |           |       |                      |ERROR: CSUPDT_ERR_LIST if can't  |
  |           |       |                      |list files in database           |
  +-----------+-------+----------------------+---------------------------------+
  |lastupdate |cmd    |"lastupdate"          |OK: Unix time stamp representing |
  |           |progid |unique id of program  |date on which online database was|
  |           |version|program version number|last updated                     |
  |           |       |                      |ERROR: CSUPDT_ERR_STDPARAMS if   |
  |           |       |                      |required params not provided     |
  |           |       |                      |ERROR: CSUPDT_ERR_LIST if can't  |
  |           |       |                      |list files in database           |
  |           |       |                      |ERROR: CSUPDT_ERR_NOTFOUND if    |
  |           |       |                      |can't access a file in database  |
  +-----------+-------+----------------------+---------------------------------+
  |getdatabase|cmd    |"getdatabase"         |OK: All files from on-line       |
  |           |progid |unique id of program  |database combined in single      |
  |           |version|program version number|stream of data.                  |
  |           |       |                      |ERROR: CSUPDT_ERR_STDPARAMS if   |
  |           |       |                      |required params not provided     |
  |           |       |                      |ERROR: CSUPDT_ERR_LIST if can't  |
  |           |       |                      |list files in database           |
  |           |       |                      |ERROR: CSUPDT_ERR_NOTFOUND if    |
  |           |       |                      |can't access a file in database  |
  +-----------+-------+----------------------+---------------------------------+
  |logoff     |cmd    |"logoff"              |OK: No data                      |
  |           |progid |unique id of program  |ERRORS: CSUPDT_ERR_STDPARAMS if  |                          |
  |           |version|program version number|required params not provided     |
  +-----------+-------+----------------------+---------------------------------+
  If any other command (or no command) is specified then error code
  CSUPDT_ERR_CMD (1) is returned.
}


const
  // Web service info
  cScriptURLTplt = 'http://%s/websvc/codesnip-updt';
  cUserAgent = 'DelphiDabbler-CodeSnip-Updater-v5';


resourcestring
  // Error messages
  sShortHTTPError = 'HTTP Error';
  sLongHTTPError = 'The web server returned the following error: "%0:s"';
  sShortConnectionError = 'Connection Error';
  sShortTransmissionError = 'Transmission Error';
  sShortWebSvcFailure = 'Web Service Failure';
  sLongWebSvcFailure =  'The database update web service failed with the '
    + 'following error:' + EOL + '%0:s';
  sShortWebSvcError = 'Download Error';
  sLongWebSvcError = 'The database update web service returned the following '
    + 'error' + EOL + 'Error %0:d: %1:s';
  sBadFileCount = 'Web service returned an invalid value for the filecount '
    + 'command';


{ TDBDownloadMgr }

constructor TDBDownloadMgr.Create;
begin
  inherited Create(TWebServiceInfo.Create(cScriptURLTplt, cUserAgent));
end;

function TDBDownloadMgr.FileCount(const WantProgress: Boolean): Integer;
var
  Response: TStringList;  // response from server
begin
  Self.WantProgress := WantProgress;
  Response := TStringList.Create;
  try
    PostStdCommand('filecount', Response);
    if not TryStrToInt(StrTrim(Response.Text), Result) then
      raise EWebServiceFailure.Create(sBadFileCount);
  finally
    Response.Free;
  end;
end;

function TDBDownloadMgr.GetDatabase(const WantProgress: Boolean): TEncodedData;
var
  Response: TStringList;  // response from server
begin
  Self.WantProgress := WantProgress;
  Response := TStringList.Create;
  try
    PostStdCommand('getdatabase', Response);
    Result := TEncodedData.Create(
      Response.Text, TWebCharEncodings.GetEncodingType(ResponseCharSet)
    );
  finally
    Response.Free;
  end;
end;

procedure TDBDownloadMgr.HandleException(const E: EWebError);
begin
  if E is EHTTPError then
    raise EDBDownloadMgr.CreateFmt(
      sShortHTTPError, sLongHTTPError, [E.Message]
    );
  if E is EWebConnectionError then
    raise EDBDownloadMgr.Create(sShortConnectionError, E.Message);
  if E is EWebTransmissionError then
    raise EDBDownloadMgr.Create(sShortTransmissionError, E.Message);
  if E is EWebServiceFailure then
    raise EDBDownloadMgr.CreateFmt(
      sShortWebSvcFailure, sLongWebSvcFailure, [E.Message]
    );
  if E is EWebServiceError then
    // Include web service error code in long description
    raise EDBDownloadMgr.CreateFmt(
      sShortWebSvcError,
      sLongWebSvcError,
      [(E as EWebServiceError).ErrorCode, E.Message]
    );
end;

procedure TDBDownloadMgr.IncludeStdParams(const Params: TURIParams);
begin
  Params.Add('progid', TAppInfo.ProgramKey);
  Params.Add('version', TAppInfo.ProgramReleaseVersion);
end;

function TDBDownloadMgr.LastUpdate(const WantProgress: Boolean): string;
var
  Response: TStringList;  // response from server
begin
  Self.WantProgress := WantProgress;
  Response := TStringList.Create;
  try
    PostStdCommand('lastupdate', Response);
    Result := StrTrim(Response.Text);
  finally
    Response.Free;
  end;
end;

procedure TDBDownloadMgr.LogOff(const WantProgress: Boolean);
var
  Response: TStringList;  // response from server
begin
  Self.WantProgress := WantProgress;
  Response := TStringList.Create;
  try
    PostStdCommand('logoff', Response);   // No response data expected
  finally
    Response.Free;
  end;
end;

procedure TDBDownloadMgr.LogOn(const Caller: string;
  const WantProgress: Boolean);
var
  Response: TStringList;  // response from server
  Params: TURIParams;     // parameters to send with command
begin
  Self.WantProgress := WantProgress;
  Response := TStringList.Create;
  try
    Params := TURIParams.Create;
    try
      IncludeStdParams(Params);
      Params.Add('os', SanitiseString(TOSInfo.Description));
      Params.Add('browser', IntToStr(TIEInfo.MajorVersion));
      Params.Add(
        'caller',
        SanitiseString(Caller) + ',' +
          StrIf(TCommandLineOpts.IsPortable, 'Portable', 'Standard')
      );
      SafePostCommand('logon', Params, Response);
    finally
      Params.Free;
    end;
  finally
    Response.Free;
  end;
end;

procedure TDBDownloadMgr.PostStdCommand(const Cmd: string;
  const Response: TStrings);
var
  StdParams: TURIParams;
begin
  StdParams := TURIParams.Create;
  try
    IncludeStdParams(StdParams);
    SafePostCommand(Cmd, StdParams, Response);
  finally
    StdParams.Free;
  end;
end;

procedure TDBDownloadMgr.SafePostCommand(const Cmd: string;
  const Params: TURIParams; const Response: TStrings);
begin
  try
    PostCommand(Cmd, Params, Response);
  except
    on E: EWebError do
      HandleException(E);
  end;
end;

{ EDBDownloadMgr }

constructor EDBDownloadMgr.Create(const ShortMsg, Msg: string);
begin
  inherited Create(Msg);
  fShortMsg := ShortMsg;
end;

constructor EDBDownloadMgr.CreateFmt(const ShortMsg, Fmt: string;
  const Args: array of const);
begin
  inherited CreateFmt(Fmt, Args);
  fShortMsg := ShortMsg;
end;

end.

