{
 * UPaypalDonateAction.pas
 *
 * Custom action used to display the DelphiDabbler Paypal donation page in
 * default web browser.
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
 * The Original Code is UPaypalDonateAction.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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

