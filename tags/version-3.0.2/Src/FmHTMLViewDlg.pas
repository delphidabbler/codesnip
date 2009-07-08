{ ##
  @FILE                     FmHTMLViewDlg.pas
  @COMMENTS                 Implements abstract base class for HTML view dialogs
                            that need to size themselves to their HTML content.
  @PROJECT_NAME             CodeSnip
  @PROJECT_DESC             Offline viewer for routines from the online
                            DelphiDabbler CodeSnip database.
  @DEPENDENCIES             None
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 28/05/2006
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 03/12/2006
      @COMMENTS             Changed to re-align form to owner after resizing
                            to fit HTML content.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 07/02/2007
      @COMMENTS             Rewrote to work with revised TGenericDlg base class
                            and new form customisation / aligment framework:
                            + Overrode new ConfigForm method to initialise the
                              HTML displayed in browser control before sizing
                              the body panel to fit the content.
                            + Added InitHTMLFrame abstract method that
                              subclasses override to initialise HTML in browser.
                            + Revised UpdateFormHeight method to remove call to
                              resize form. Renamed method as SizeBodyPanel.
    )
    @REVISION(
      @VERSION              2.1
      @DATE                 11/02/2007
      @COMMENTS             + Added new abstract GetBodyPanelHeight method to
                              return required height of body panel.
                            + Changed SizeBodyPanel method to simply set body
                              panel height to value returned from
                              GetBodyPanelHeight method. We no longer simply
                              assume the body panel should be the same height as
                              the HTML frame since there may be more that one
                              frame in a HTML dialog.
                            + Removed unused GetBrowser abstract method.
    )
  )
}


{
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
 * The Original Code is FmHTMLViewDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2007 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s): None
 *
 * ***** END LICENSE BLOCK *****
}


unit FmHTMLViewDlg;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericViewDlg;


type

  {
  THTMLViewDlg:
    Abstract base class for HTML view dialogs that need to size themselves to
    their HTML content.
  }
  THTMLViewDlg = class(TGenericViewDlg)
  protected
    procedure InitHTMLFrame; virtual; abstract;
      {Initialises HTML frame by loading required HTML in browser control.
      }
    function GetBodyPanelHeight: Integer; virtual; abstract;
      {Gets height of body panel.
        @return Required body panel height.
      }
    procedure SizeBodyPanel;
      {Sets size of body panel to value provided by descendant class.
      }
    procedure ConfigForm; override;
      {Initialises dialog's HTML content and sizes body panel to fit HTML
      content.
      }
  end;


implementation


{$R *.dfm}


{ THTMLViewDlg }

procedure THTMLViewDlg.ConfigForm;
  {Initialises dialog's HTML content and sizes body panel to fit HTML content.
  }
begin
  inherited;
  InitHTMLFrame;
  SizeBodyPanel;
end;

procedure THTMLViewDlg.SizeBodyPanel;
  {Sets size of body panel to value provided by descendant class.
  }
begin
  pnlBody.ClientHeight := GetBodyPanelHeight;
end;

end.

