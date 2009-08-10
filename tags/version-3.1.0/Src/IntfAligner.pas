{ ##
  @FILE                     IntfAligner.pas
  @COMMENTS                 Declares an interface to object that can align a
                            form on screen.
  @PROJECT_NAME             CodeSnip
  @PROJECT_DESC             Offline viewer for routines from the online
                            DelphiDabbler CodeSnip database.
  @DEPENDENCIES             None
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 07/02/2007
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 26/09/2007
      @COMMENTS             Renamed IAligner interface to IFormAligner.
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
 * The Original Code is IntfAligner.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s): None
 *
 * ***** END LICENSE BLOCK *****
}


unit IntfAligner;


interface


uses
  // Delphi
  Windows, Forms;


type

  {
  IFormAligner:
    Interface to object that can align a form on screen.
  }
  IFormAligner = interface(IInterface)
    ['{1B2EDAFC-487B-4F8A-9921-84483B78DBBF}']
    procedure AlignForm(const AForm: TCustomForm);
      {Aligns a form on screen in implementation determined manner.
        @param AForm [in] Form to be aligned.
      }
  end;


implementation

end.

