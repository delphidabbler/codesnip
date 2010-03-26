{
 * UDownloadMgr.pas
 *
 * Implements a class that updates database from webservice.
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
 * The Original Code is UDownloadMgr.pas
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


unit UDownloadMgr;


interface


uses
  // Delphi
  Classes,
  // Project
  NsWebServices.UExceptions, UWebService;


type

  {
  TDownloadMgr:
    Manages downloads of updates to database from web service via HTTP.
  }
  TDownloadMgr = class sealed(TDDabStdWebService)
  strict private
//    fWantProgressReport: Boolean;
//      {Flag true if OnProgress event to be triggered, false otherwise. Flag set
//      by each server command method to determine if OnProgress wanted for that
//      command}
    function ProductVersion: string;
      {Gets program's product version.
        @return String representation of product version number.
      }
    function ProgId: string;
      {Gets program's id code.
        @return Program id.
      }
    procedure HandleException(const E: EWebService);
      {Converts EWebService exceptions into EDownloadMgr exceptions with both
      a long and a short description.
        @param E [in] EWebService or descendant exception to be converted.
        @except Raises EDownloadMgr exceptions based on given exception.
      }
    procedure PostStdCommand(const Cmd: string; const Response: TStrings);
      {Sends command to server that includes standard parameters that are sent
      with all commands, i.e. "progid" and "version".
        @param Cmd [in] Command to be sent.
        @param Response [in] String list to receive web server response.
        @except EDownloadMgr Raised if EWebService exception or sub class
          detected.
      }
    procedure SafePostCommand(const Cmd: string; const Params: array of string;
      const Response: TStrings);
      {Sends a command to server and returns data component of response in a
      string list. Converts recognised exceptions to EDownloadMgr.
        @param Cmd [in] Command to be executed. Passed to server as Cmd=CmdName
          parameter.
        @param Params [in] List of parameters in form ParamName=ParamValue. []
          if no parameters.
        @param Response [in] Server's response as string list where each line of
          response is a line of string list.
        @except EDownloadMgr Raised if EWebService exception or sub class
          detected.
      }
//  strict protected
//    procedure DoProgress; override;
//      {Triggers OnProgress event if fWantProgress flag is true.
//      }
  public
    procedure LogOn(const Stream: TStream; const WantProgress: Boolean = False);
      {Logs on to web service and retrieves news items.
        @param Stream [in] Stream that receives news items.
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
    procedure GetDatabase(const Stream: TStream;
      const WantProgress: Boolean = False);
      {Gets whole code snippets database from web server.
        @param Stream [in] Stream to receive downloaded database. Database
          files are encoded into stream.
        @param WantProgresss [in] Flag true if OnProgress event to be triggered
          for download.
      }
    constructor Create;
      {Class constructor. Initialises service.
      }
  end;

  {
  EDownloadMgr:
    Class of exception raised by TDownloadMgr. Contains standard full
    description of exception and adds an abreviated description in ShortMsg
    property.
  }
  EDownloadMgr = class(EWebService)
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
  UAppInfo, UConsts, UUnicodeHelper, UWebInfo;


{
  Web service notes: codesnip-updt.php v4
  =======================================

  This web service enables CodeSnip to check if updated files are available in
  on-line database and to update local database accordingly.

  The service accepts POSTed commands which must each have form:
    cmd=<command> [<params>]
  where <params> is a list of parameters in form param-name=param-value.

  All responses from the v4 service have two parts:
  + Successful responses have '0' on first line followed by optional lines of
    data resulting from command.
  + Error responses have +ve error code on first line and error message on 2nd
    line.

  The web service expects a user agent of "DelphiDabbler-CodeSnip-Updater-v4"
  and will return a 403 "Forbidden" error if this is not provided.

  Table of Commands
  +----------------------------------------------------------------------------+
  |Command    |Queries|Values                |Response                         |
  +-----------+-------|----------------------+---------------------------------|
  |logon      |cmd    |"logon"               |OK: Stream of news items         |
  |           |progid |unique id of program  |ERROR: CSUPDT_ERR_STDPARAMS if   |
  |           |version|program version number|required params not provided     |
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

  The filecount command was only implemented from v1.9.3 of CodeSnip and v4.10
  of the web service.
}


const
  // Web service info
  cScriptName = 'codesnip-updt.php';                  // script name
  cUserAgent = 'DelphiDabbler-CodeSnip-Updater-v4';  // user agent string


resourcestring
  // Error messages
  sShortHTTPError = 'HTTP Error';
  sLongHTTPError = 'The web server returned the following error: "%0:s"';
  sShortConnectionError = 'Connection Error';
  sShortWebSvcFailure = 'Web Service Failure';
  sLongWebSvcFailure =  'The database update web service failed with the '
    + 'following error:' + EOL + '%0:s';
  sShortWebSvcError = 'Download Error';
  sLongWebSvcError = 'The database update web service returned the following '
    + 'error' + EOL + 'Error %0:d: %1:s';
  sBadFileCount = 'Web service returned an invalid value for the filecount '
    + 'command';


{ TDownloadMgr }

constructor TDownloadMgr.Create;
  {Class constructor. Initialises service.
  }
begin
  inherited Create(TWebServiceInfo.Create(cScriptName, cUserAgent));
end;

//procedure TDownloadMgr.DoProgress;
//  {Triggers OnProgress event if fWantProgress flag is true.
//  }
//begin
//  if fWantProgressReport then
//    TriggerProgressEvent;
//end;

function TDownloadMgr.FileCount(const WantProgress: Boolean): Integer;
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
    if not TryStrToInt(Trim(Response.Text), Result) then
      raise EWebServiceFailure.Create(sBadFileCount);
  finally
    FreeAndNil(Response);
  end;
end;

procedure TDownloadMgr.GetDatabase(const Stream: TStream;
  const WantProgress: Boolean);
  {Gets whole code snippets database from web server.
    @param Stream [in] Stream to receive downloaded database. Database files are
      encoded into stream.
    @param WantProgresss [in] Flag true if OnProgress event to be triggered for
      download.
  }
var
  Response: TStringList;  // response from server
  ResBytes: TBytes;       // response as latin-1 byte stream
begin
  Self.WantProgress := WantProgress;
  Response := TStringList.Create;
  try
    PostStdCommand('getdatabase', Response);
    ResBytes := Latin1BytesOf(Response.Text);
    Stream.WriteBuffer(ResBytes[0], Length(ResBytes));
  finally
    FreeAndNil(Response);
  end;
end;

procedure TDownloadMgr.HandleException(const E: EWebService);
  {Converts EWebService exceptions into EDownloadMgr exceptions with both
  a long and a short description.
    @param E [in] EWebService or descendant exception to be converted.
    @except Raises EDownloadMgr exceptions based on given exception.
  }
begin
  if E is EHTTPError then
    // Use HTTP error code and error description in long description
    raise EDownloadMgr.CreateFmt(
      sShortHTTPError, sLongHTTPError, [E.Message]
    );
  if E is EWebConnectionError then
    // Copy exception message to long description
    raise EDownloadMgr.Create(sShortConnectionError, E.Message);
  if E is EWebServiceFailure then
    // Insert exception's original message to long description
    raise EDownloadMgr.CreateFmt(
      sShortWebSvcFailure, sLongWebSvcFailure, [E.Message]
    );
  if E is EWebServiceError then
    // Include web service error code in long description
    raise EDownloadMgr.CreateFmt(
      sShortWebSvcError,
      sLongWebSvcError,
      [(E as EWebServiceError).ErrorCode, E.Message]
    );
end;

function TDownloadMgr.LastUpdate(const WantProgress: Boolean): string;
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
    Result := Trim(Response.Text);
  finally
    FreeAndNil(Response);
  end;
end;

procedure TDownloadMgr.LogOff(const WantProgress: Boolean);
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
    FreeAndNil(Response);
  end;
end;

procedure TDownloadMgr.LogOn(const Stream: TStream;
  const WantProgress: Boolean);
  {Logs on to web service and retrieves news items.
    @param Stream [in] Stream that receives news items.
    @param WantProgresss [in] Flag true if OnProgress event to be triggered for
      download.
  }
var
  Response: TStringList;  // response from server
begin
  Self.WantProgress := WantProgress;
  Response := TStringList.Create;
  try
    PostStdCommand('logon', Response);
    Response.SaveToStream(Stream);
  finally
    FreeAndNil(Response);
  end;
end;

procedure TDownloadMgr.PostStdCommand(const Cmd: string;
  const Response: TStrings);
  {Sends command to server that includes standard parameters that are sent with
  all commands, i.e. "progid" and "version".
    @param Cmd [in] Command to be sent.
    @param Response [in] String list to receive web server response.
    @except EDownloadMgr Raised if EWebService exception or sub class detected.
  }
var
  StdParams: array of string; // array to receive standard parameters
begin
  SetLength(StdParams, 2);
  StdParams[0] := 'progid=' + ProgId;
  StdParams[1] := 'version=' + ProductVersion;
  SafePostCommand(Cmd, StdParams, Response);
end;

function TDownloadMgr.ProductVersion: string;
  {Gets program's product version.
    @return String representation of product version number.
  }
begin
  Result := TAppInfo.ProgramReleaseVersion;
end;

function TDownloadMgr.ProgId: string;
  {Gets program's id code.
    @return Program id.
  }
begin
  Result := TAppInfo.ProgramKey;
end;

procedure TDownloadMgr.SafePostCommand(const Cmd: string;
  const Params: array of string; const Response: TStrings);
  {Sends a command to server and returns data component of response in a
  string list. Converts recognised exceptions to EDownloadMgr.
    @param Cmd [in] Command to be executed. Passed to server as Cmd=CmdName
      parameter.
    @param Params [in] List of parameters in form ParamName=ParamValue. [] if no
      parameters.
    @param Response [in] Server's response as string list where each line of
      response is a line of string list.
    @except EDownloadMgr Raised if EWebService exception or sub class detected.
  }
begin
  try
    PostCommand(Cmd, Params, Response);
  except
    on E: EWebService do
      HandleException(E);
  end;
end;


{ EDownloadMgr }

constructor EDownloadMgr.Create(const ShortMsg, Msg: string);
  {Class constructor. Creates exception that records abbreviated and full
  descriptions.
    @param ShortMsg [in] Abbreviated message for ShortMsg property.
    @param Msg [in] Standard exception Message property value.
  }
begin
  inherited Create(Msg);
  fShortMsg := ShortMsg;
end;

constructor EDownloadMgr.CreateFmt(const ShortMsg, Fmt: string;
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

