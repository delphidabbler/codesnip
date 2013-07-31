{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a class that submits code snippets database contributions via a
 * web service.
}


unit Web.UCodeSubmitter;


interface


uses
  // Delphi
  SysUtils, Classes,
  // Project
  Web.UStdWebService;


type
  ///  <summary>Submits snippets to the Code Snippets Database code submission
  ///  web service.</summary>
  TCodeSubmitter = class sealed(TStdWebService)
  public
    ///  <summary>Creates a new object instance with correct URL and user agent
    ///  for web service.</summary>
    constructor Create;
    ///  <summary>Submits data describing the snippets being submitted to the
    ///  cpde submission web service.</summary>
    ///  <param name="Data">TBytes [in] Byte array containing code submission
    ///  information.</param>
    procedure SubmitData(const Data: TBytes);
  end;


implementation


uses
  // Project
  Web.UInfo;


const
  // Web service info
  cScriptURLTplt = 'http://%s/websvc/codesnip-submitter';
  cUserAgent = 'DelphiDabbler-CSSubmitter-v1';
  cMediaType = 'text/xml';


resourcestring
  // Error message
  sBadRespone = 'Invalid response from code snippets submitter web service.';


{ TCodeSubmitter }

constructor TCodeSubmitter.Create;
begin
  inherited Create(
    TWebServiceInfo.Create(cScriptURLTplt, cUserAgent, cMediaType)
  );
end;

procedure TCodeSubmitter.SubmitData(const Data: TBytes);
var
  Response: TStringList;  // valid response from web service: content ignored
begin
  Assert(Assigned(Data), ClassName + '.Subscribe: Data stream is nil');
  Response := TStringList.Create;
  try
    PostData(Data, Response);
  finally
    FreeAndNil(Response);
  end;
end;

end.

