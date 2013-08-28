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
  ///  <summary>Provides an interface to the DelphiDabbler CodeSnip program
  ///  update web service.</summary>
  ///  <remarks>This class provides a public method for every command exposed by
  ///  the web service.</remarks>
  TProgramUpdateMgr = class sealed(TStdWebService)
  strict private
    const
      ///  <summary>Web service's URL template.</summary>
      ScriptURLTplt = 'http://codesnip.%s/websvc/prog-update';
      ///  <summary>User agent sent to web service.</summary>
      UserAgent = 'CodeSnip';
      ///  <summary>API key required for all calls to web service.</summary>
      ApiKey = '9EE3A4D85A2F46F79AE2AAB1012A7678';
  strict private
      ///  <summary>Returns value to be passed to web service to specify the
      ///  edition of the program making the request.</summary>
    class function ProgramEdition: string;
    ///  <summary>Creates and returns a parameters object containing standard
    ///  parameters that are required on every call to the web service.
    ///  </summary>
    ///  <remarks>Callers must free the returned object.</remarks>
    function CreateParams: TURIParams;
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
    Params.Add('edition', ProgramEdition);
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
    Params.Add('edition', ProgramEdition);
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

class function TProgramUpdateMgr.ProgramEdition: string;
begin
  // NOTE: CodeSnip 4 used 'standard' in the standard edition and 'portable' in
  //       the portable edition. From CodeSnip 5 there was only one edition,
  //       labelled by its major version number.
  { TODO -cPRERELEASE: Change the following result to
    Result := Format('%d', [TVersionInfo.ProductVerNum.V1]);
    once the major version number in version information is changed to "5" }
  Result := '5';
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

