{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Class that provides information about the application.
}


unit Web.UProgramUpdateMgr;


interface


uses
  // Project
  Web.UStdWebService;


type
  TProgramUpdateMgr = class sealed(TStdWebService)
  strict private
    const
      ScriptName = 'prog-updater';
      UserAgent = 'DelphiDabbler-Program-Updater-v1';
  public
    constructor Create;
    function IsLatest: Boolean;
    function LatestProgramVersion: string;
  end;


implementation


uses
  // Delphi
  Classes,
  // Project
  UAppInfo, UStrUtils, UURIParams, Web.UInfo;


{ TProgramUpdateMgr }

constructor TProgramUpdateMgr.Create;
begin
  inherited Create(TWebServiceInfo.Create(ScriptName, UserAgent));
end;

function TProgramUpdateMgr.IsLatest: Boolean;
var
  Params: TURIParams;
  Response: TStringList;
begin
  Params := TURIParams.Create;
  try
    Params.Add('id', TAppInfo.ProgramID);
    Params.Add('ver', TAppInfo.ProgramReleaseVersion);
    Response := TStringList.Create;
    try
      PostCommand('islatest', Params, Response);
      Result := StrTrim(Response.Text) = '1';
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
  Params := TURIParams.Create;
  try
    Params.Add('id', TAppInfo.ProgramID);
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

end.

