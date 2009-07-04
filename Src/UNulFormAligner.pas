{ ##
  @FILE                     UNulFormAligner.pas
  @COMMENTS                 Defines a do nothing class that implements IAligner
                            for use with forms that do not support alignment.
  @PROJECT_NAME             CodeSnip
  @PROJECT_DESC             Offline viewer for routines from the online
                            DelphiDabbler CodeSnip database.
  @DEPENDENCIES             None
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 08/02/2007
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 26/07/2007
      @COMMENTS             Changed to use renamed IFormAligner interface.
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
 * The Original Code is UNulFormAligner.pas
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


unit UNulFormAligner;


interface


uses
  // Delphi
  Forms,
  // Project
  IntfAligner;


type

  {
  TNulAligner:
    Do nothing IAligner implementation. Used for forms that do not support
    alignment.
  }
  TNulAligner = class(TInterfacedObject,
    IFormAligner
  )
  protected
    procedure AlignForm(const AForm: TCustomForm);
      {Does nothing.
        @param AForm [in] Not used.
      }
  end;


implementation


{ TNulAligner }

procedure TNulAligner.AlignForm(const AForm: TCustomForm);
  {Does nothing.
    @param AForm [in] Not used.
  }
begin
  // Do nothing
end;

end.

