{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a class that interfaces with a web service that provides
 * information about available program updates.
}


unit Web.UProgramUpdateMgr;


interface


uses
  // Project
  UURIParams, Web.UStdWebService;


type
  ///  <summary>Provides an interface to the DelphiDabbler CodeSnip program
  ///  update web service using the v2 API.</summary>
  ///  <remarks>This class provides a public method for every command exposed by
  ///  the web service.</remarks>
  TProgramUpdateMgr = class sealed(TStdWebService)
  strict private
    const
      ///  <summary>Web service's URL template.</summary>
      ScriptURLTplt = 'http://codesnip.%s/websvc/prog-update';
      ///  <summary>User agent sent to web service.</summary>
      UserAgent = 'CodeSnip';
      ///  <summary>API version of web service.</summary>
      ApiVersion = '3';
      ///  <summary>API key required for all calls to web service.</summary>
      ApiKey = '9EE3A4D85A2F46F79AE2AAB1012A7678';
      { TODO -cPRERELEASE: Change "Channel" to have required channel for
                         previews, beta or final release as necessary. }
      ///  <summary>Release channel sent to web service.</summary>
      Channel = 'parsnip';
      { TODO -cPRERELEASE: Change "Stream" to have required stream to one of
                           "stable", "beta", "alpha" or "rc" as required. }
      ///  <summary>Release stream sent to web service.</summary>
      Stream = 'experimental';
      ///  <summary>Program edition sent to web service.</summary>
      Edition = 'main';
  strict private
    ///  <summary>Creates and returns a parameters object containing standard
    ///  parameters that are required on every call to the web service.
    ///  </summary>
    ///  <remarks>Callers must free the returned object.</remarks>
    function CreateParams: TURIParams;
    ///  <summary>Adds channel and edition parameters to the given parameters.
    ///  </summary>
    procedure AddUpdateStreamParams(Params: TURIParams);
  public
    ///  <summary>Creates a new object instance with the correct URL and
    ///  suitable user agent.</summary>
    constructor Create;
    ///  <summary>Signs on the the web service.</summary>
    ///  <param name="Caller">string [in] Specifies from where the web service
    ///  is called.</param>
    procedure SignOn(const Caller: string);
    ///  <summary>Gets the latest version of the program from the web service.
    ///  </summary>
    ///  <remarks>The version returned is the latest one for the program edition
    ///  specified by the ProgramMode constant.</remarks>
    function LatestProgramVersion: string;
    ///  <summary>Gets the URL to use to download the latest version of the
    ///  program.</summary>
    ///  <remarks>The URL returned is the correct one for the program edition
    ///  specified by the Edition constant.</remarks>
    function DownloadURL: string;
  end;


implementation


uses
  // Delphi
  SysUtils, Classes,
  // Project
  CS.Init.CommandLineOpts,
  UAppInfo,
  UStrUtils,
  USystemInfo,
  UVersionInfo,
  Web.UInfo;


{ TProgramUpdateMgr }

procedure TProgramUpdateMgr.AddUpdateStreamParams(Params: TURIParams);
begin
  Assert(Assigned(Params), ClassName + '.AddUpdateStreamParams: Params is nil');
  Params.Add('channel', Channel);
  Params.Add('stream', Stream);
  Params.Add('edition', Edition);
end;

constructor TProgramUpdateMgr.Create;
begin
  inherited Create(TWebServiceInfo.Create(ScriptURLTplt, UserAgent));
end;

function TProgramUpdateMgr.CreateParams: TURIParams;
begin
  Result := TURIParams.Create;
  Result.Add('api', ApiVersion);
  Result.Add('key', ApiKey);
  Result.Add('prog-id', TAppInfo.ProgramKey);
end;

function TProgramUpdateMgr.DownloadURL: string;
var
  Params: TURIParams;
  Response: TStringList;
begin
  Params := CreateParams;
  try
    AddUpdateStreamParams(Params);
    Response := TStringList.Create;
    try
      PostCommand('downloadurl', Params, Response);
      Result := StrTrim(Response.Text);
    finally
      Response.Free;
    end;
  finally
    Params.Free;
  end;
end;

function TProgramUpdateMgr.LatestProgramVersion: string;
var
  Params: TURIParams;
  Response: TStringList;
begin
  Params := CreateParams;
  try
    AddUpdateStreamParams(Params);
    Response := TStringList.Create;
    try
      PostCommand('version', Params, Response);
      Result := StrTrim(Response.Text);
    finally
      Response.Free;
    end;
  finally
    Params.Free;
  end;
end;

procedure TProgramUpdateMgr.SignOn(const Caller: string);
var
  Params: TURIParams;
  Response: TStringList;
begin
  Params := CreateParams;
  try
    Params.Add('prog-ver', TAppInfo.ProgramReleaseVersion);
    Params.Add('os', SanitiseString(TOSInfo.Description));
    Params.Add('ie-ver', IntToStr(TIEInfo.MajorVersion));
    Params.Add(
      'caller',
      SanitiseString(Caller) + ',' +
        StrIf(TCommandLineOpts.IsPortable, 'Portable', 'Standard')
    );
    Response := TStringList.Create;
    try
      PostCommand('stats', Params, Response);
      // do nothing with response
    finally
      Response.Free;
    end;
  finally
    Params.Free;
  end;
end;

end.

