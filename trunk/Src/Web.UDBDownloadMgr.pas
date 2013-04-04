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
 * Implements a class that interfaces with a web service to update the database.
}


unit Web.UDBDownloadMgr;


interface


uses
  // Delphi
  Classes,
  // Project
  UEncodings, UURIParams, Web.UExceptions, Web.UStdWebService;


type

  {
  TDBDownloadMgr:
    Manages downloads of updates to database from web service via HTTP.
  }
  TDBDownloadMgr = class sealed(TStdWebService)
  strict private
    procedure HandleException(const E: EWebError);
      {Converts EWebError exceptions into EDBDownloadMgr exceptions with both
      a long and a short description.
        @param E [in] EWebError or descendant exception to be converted.
        @except Raises EDBDownloadMgr exceptions based on given exception.
      }
    procedure IncludeStdParams(const Params: TURIParams);
      {Includes standard parameters in a parameter list.
        @param Params [in] Parameter list that receives standard parameters.
      }
    procedure PostStdCommand(const Cmd: string; const Response: TStrings);
      {Sends command to server that includes standard parameters that are sent
      with all commands, i.e. "progid" and "version".
        @param Cmd [in] Command to be sent.
        @param Response [in] String list to receive web server response.
        @except EDBDownloadMgr Raised if EWebError exception or sub class
          detected.
      }
    procedure SafePostCommand(const Cmd: string; const Params: TURIParams;
      const Response: TStrings);
      {Sends a command to server and returns data component of response in a
      string list. Converts recognised exceptions to EDBDownloadMgr.
        @param Cmd [in] Command to be executed. Passed to server as Cmd=CmdName
          parameter.
        @param Params [in] List of parameters in form ParamName=ParamValue. []
          if no parameters.
        @param Response [in] Server's response as string list where each line of
          response is a line of string list.
        @except EDBDownloadMgr Raised if EWebError exception or sub class
          detected.
      }
  public
    procedure LogOn(const Caller: string; const WantProgress: Boolean = False);
      {Logs on to web service.
        @param Caller [in] Specifies from where the web service is called.
        @param WantProgresss [in] Flag true if OnProgress event to be triggered
          for download.
      }
    procedure LogOff(const WantProgress: Boolean = False);
      {Logs off from web server.
        @param WantProgresss [in] Flag true if OnProgress event to be triggered
          for download.
      }
    function LastUpdate(const WantProgress: Boolean = False): string;
      {Gets date of last update to code snippets database on web server.
        @param WantProgresss [in] Flag true if OnProgress event to be triggered
          for download.
        @return String containing last update date as a Unix date stamp.
      }
    function FileCount(const WantProgress: Boolean = False): Integer;
      {Gets count of files in code snippets database on web server.
        @param WantProgresss [in] Flag true if OnProgress event to be triggered
          for download.
        @return File count.
      }
    function GetDatabase(const WantProgress: Boolean = False): TEncodedData;
      {Gets whole code snippets database from web server.
        @param WantProgresss [in] Flag true if OnProgress event to be triggered
          for download.
        @return Downloaded data with encoding info.
      }
    constructor Create;
      {Class constructor. Initialises service.
      }
  end;

  {
  EDBDownloadMgr:
    Class of exception raised by TDBDownloadMgr. Contains standard full
    description of exception and adds an abbreviated description in ShortMsg
    property.
  }
  EDBDownloadMgr = class(EWebService)
  strict private
    fShortMsg: string;
      {Value of ShortMsg property}
  public
    constructor Create(const ShortMsg, Msg: string); overload;
      {Class constructor. Creates exception that records abbreviated and full
      descriptions.
        @param ShortMsg [in] Abbreviated message for ShortMsg property.
        @param Msg [in] Standard exception Message property value.
      }
    constructor CreateFmt(const ShortMsg, Fmt: string;
      const Args: array of const); overload;
      {Class constructor. Creates exception that records abbreviated and full
      descriptions. Builds full description from format string and parameters.
        @param ShortMsg [in] Abbreviated message for ShortMsg property.
        @param Fmt [in] Format string for standard Message property.
        @param Args [in] Parameters for format string.
      }
    property ShortMsg: string read fShortMsg;
      {Abbreviated description of exception}
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UAppInfo, UConsts, UStrUtils, USystemInfo, Web.UCharEncodings, Web.UInfo;


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
  {Class constructor. Initialises service.
  }
begin
  inherited Create(TWebServiceInfo.Create(cScriptURLTplt, cUserAgent));
end;

function TDBDownloadMgr.FileCount(const WantProgress: Boolean): Integer;
  {Gets count of files in code snippets database on web server.
    @param WantProgresss [in] Flag true if OnProgress event to be triggered for
      download.
    @return File count.
  }
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
  {Gets whole code snippets database from web server.
    @param WantProgresss [in] Flag true if OnProgress event to be triggered for
      download.
    @return Downloaded data with encoding info.
  }
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
  {Converts EWebError exceptions into EDBDownloadMgr exceptions with both
  a long and a short description.
    @param E [in] EWebError or descendant exception to be converted.
    @except Raises EDBDownloadMgr exceptions based on given exception.
  }
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
  {Includes standard parameters in a parameter list.
    @param Params [in] Parameter list that receives standard parameters.
  }
begin
  Params.Add('progid', TAppInfo.ProgramKey);
  Params.Add('version', TAppInfo.ProgramReleaseVersion);
end;

function TDBDownloadMgr.LastUpdate(const WantProgress: Boolean): string;
  {Gets date of last update to code snippets database on web server.
    @param WantProgresss [in] Flag true if OnProgress event to be triggered for
      download.
    @return String containing last update date as a Unix date stamp.
  }
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
  {Logs off from web server.
    @param WantProgresss [in] Flag true if OnProgress event to be triggered for
      download.
  }
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
  {Logs on to web service.
    @param Caller [in] Specifies from where the web service is called.
    @param WantProgresss [in] Flag true if OnProgress event to be triggered for
      download.
  }
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
      Params.Add('browser', IntToStr(TOSInfo.BrowserVer));
      Params.Add('caller', SanitiseString(Caller));
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
  {Sends command to server that includes standard parameters that are sent with
  all commands, i.e. "progid" and "version".
    @param Cmd [in] Command to be sent.
    @param Response [in] String list to receive web server response.
    @except EDBDownloadMgr Raised if EWebError exception or sub class detected.
  }
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
  {Sends a command to server and returns data component of response in a
  string list. Converts recognised exceptions to EDBDownloadMgr.
    @param Cmd [in] Command to be executed. Passed to server as Cmd=CmdName
      parameter.
    @param Params [in] List of parameters in form ParamName=ParamValue. [] if no
      parameters.
    @param Response [in] Server's response as string list where each line of
      response is a line of string list.
    @except EDBDownloadMgr Raised if EWebError exception or sub class detected.
  }
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
  {Class constructor. Creates exception that records abbreviated and full
  descriptions.
    @param ShortMsg [in] Abbreviated message for ShortMsg property.
    @param Msg [in] Standard exception Message property value.
  }
begin
  inherited Create(Msg);
  fShortMsg := ShortMsg;
end;

constructor EDBDownloadMgr.CreateFmt(const ShortMsg, Fmt: string;
  const Args: array of const);
  {Class constructor: creates exception that records abbreviated and full
  descriptions. Builds full description from format string and parameters.
    @param ShortMsg [in] Abbreviated message for ShortMsg property.
    @param Fmt [in] Format string for standard Message property.
    @param Args [in] Parameters for format string.
  }
begin
  inherited CreateFmt(Fmt, Args);
  fShortMsg := ShortMsg;
end;

end.

