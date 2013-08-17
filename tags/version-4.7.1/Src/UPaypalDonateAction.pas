{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Custom action used to display the DelphiDabbler Paypal donation page in
 * default web browser.
}


unit UPaypalDonateAction;


interface


uses
  // Delphi
  Classes;


type
  {
  TPaypalDonateAction:
    Custom action that displays the DelphiDabbler Paypal donation page in the
    default web browser.
  }
  TPaypalDonateAction = class(TBasicAction)
  public
    function Execute: Boolean; override;
      {Executes action by displaying the Paypal URL in the default browser. Any
      OnExcute event handler is ignored.
        @return False to indicate OnExecute event handler not called.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, ExtActns,
  // Project
  Web.UInfo;


{ TPaypalDonateAction }

function TPaypalDonateAction.Execute: Boolean;
  {Executes action by displaying the Paypal URL in the default browser. Any
  OnExcute event handler is ignored.
    @return False to indicate OnExecute event handler not called.
  }
var
  BrowseAction: TBrowseURL; // action that displays Paypal URL in browser
begin
  Result := False;
  // use a TBrowseAction to access URL
  BrowseAction := TBrowseURL.Create(nil);
  try
    BrowseAction.URL := TWebInfo.DonateURL;
    BrowseAction.Execute;
  finally
    FreeAndNil(BrowseAction);
  end;
end;

end.

