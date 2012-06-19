{
 * Web.UCodeSubmitter.pas
 *
 * Implements a class that submits code snippets database contributions via a
 * web service.
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
 * The Original Code is Web.UCodeSubmitter.pas, formerly UCodeSubmitter.pas then
 * NsWebServices.UCodeSubmitter.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit Web.UCodeSubmitter;


interface


uses
  // Delphi
  Classes,
  // Project
  Web.UStdWebService;


type

  {
  TCodeSubmitter:
    Submits code to the Code Snippets Database using a web service via HTTP.
  }
  TCodeSubmitter = class sealed(TStdWebService)
  public
    constructor Create;
      {Class constructor. Initialises service.
      }
    procedure SubmitData(const Data: TStream);
      {Submits data describing code to web service.
        @param Data [in] Stream containing code submission information.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  Web.UInfo;


const
  // Web service info
  cScriptName = 'codesnip-submitter.php';       // script name
  cUserAgent = 'DelphiDabbler-CSSubmitter-v1';  // user agent string
  cMediaType = 'text/xml';                      // media type


resourcestring
  // Error message
  sBadRespone = 'Invalid response from code snippets submitter web service.';


{ TCodeSubmitter }

constructor TCodeSubmitter.Create;
  {Class constructor. Initialises service.
  }
begin
  inherited Create(TWebServiceInfo.Create(cScriptName, cUserAgent, cMediaType));
end;

procedure TCodeSubmitter.SubmitData(const Data: TStream);
  {Submits data describing code to web service.
    @param Data [in] Stream containing code submission information.
  }
var
  Response: TStringList;  // valid response from web service
begin
  Assert(Assigned(Data), ClassName + '.Subscribe: Data stream is nil');
  // Send subscribe command to web service and gather response
  Response := TStringList.Create;
  try
    PostData(Data, Response);
  finally
    FreeAndNil(Response);
  end;
end;

end.

