{ ##
  @FILE                     FrTitled.pas
  @COMMENTS                 Base class for frames that display a title bar.
  @PROJECT_NAME             CodeSnip
  @PROJECT_DESC             Offline viewer for routines from the online
                            DelphiDabbler CodeSnip database.
  @DEPENDENCIES             None
  @HISTORY(
    @REVISION(
      @VERSION              0.1
      @DATE                 30/01/2005
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              0.2
      @DATE                 18/02/2005
      @COMMENTS             Deleted unused units from uses clause.
    )
    @REVISION(
      @VERSION              1.0
      @DATE                 24/05/2006
      @COMMENTS             Made minor commenting changes.
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
 * The Original Code is FrTitled.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2006 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s): None
 *
 * ***** END LICENSE BLOCK *****
}


unit FrTitled;


interface


uses
  // Delphi
  ExtCtrls, Controls, StdCtrls, Classes, Forms;


type

  {
  TTitledFrame:
    Base class for frames that display a title bar.
  }
  TTitledFrame = class(TFrame)
    pnlTitle: TPanel;
    lblTitle: TLabel;
    bvlTop: TBevel;
  end;


implementation

{$R *.dfm}

end.

