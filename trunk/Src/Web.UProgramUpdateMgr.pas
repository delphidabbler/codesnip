{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2013, Peter Johnson (www.delphidabbler.com).
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
  TProgramUpdateMgr = class sealed(TStdWebService)
  strict private
    const
      ScriptURLTplt = 'http://codesnip.%s/websvc/prog-update';
      UserAgent = 'CodeSnip';
      ApiKey = '9EE3A4D85A2F46F79AE2AAB1012A7678';
      {$IFDEF PORTABLE}
      Edition = 'portable';
      {$ELSE}
      Edition = 'standard';
      {$ENDIF}
  strict private
    class function SanitiseString(const S: string): string;
    ///  <summary>Creates and returns a parameters object containing standard
    ///  parameters required on every call to web service.</summary>
    ///  <remarks>Callers must free the returned object.</remarks>
    function CreateParams: TURIParams;
  public
    constructor Create;
    procedure SignOn(const Caller: string);
    function LatestProgramVersion: string;
    function DownloadURL: string;
  end;


implementation


uses
  // Delphi
  SysUtils, Classes,
  // Project
  UAppInfo, UStrUtils, USystemInfo, Web.UInfo;


{ TProgramUpdateMgr }

constructor TProgramUpdateMgr.Create;
begin
  inherited Create(TWebServiceInfo.Create(ScriptURLTplt, UserAgent));
end;

function TProgramUpdateMgr.CreateParams: TURIParams;
begin
  Result := TURIParams.Create;
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
    Params.Add('edition', Edition);
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
    Params.Add('edition', Edition);
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

class function TProgramUpdateMgr.SanitiseString(const S: string): string;
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

procedure TProgramUpdateMgr.SignOn(const Caller: string);
var
  Params: TURIParams;
  Response: TStringList;
begin
  Params := CreateParams;
  try
    Params.Add('prog-ver', TAppInfo.ProgramReleaseVersion);
    Params.Add('os', SanitiseString(TOSInfo.Description));
    Params.Add('ie-ver', IntToStr(TOSInfo.BrowserVer));
    Params.Add('caller', SanitiseString(Caller));
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

