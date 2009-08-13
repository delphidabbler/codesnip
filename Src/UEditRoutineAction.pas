{
 * UEditRoutineAction.pas
 *
 * Custom action used to request editing of a named routine. Stores name of
 * required routine, which must be user defined, in properties.
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
 * The Original Code is UEditRoutineAction.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UEditRoutineAction;


interface


uses
  // Delphi
  Classes;


type

  {
  TEditRoutineAction:
    Custom action used to request editing of a named routine. Stores name of
    required routine, which must be user defined, in properties.
  }
  TEditRoutineAction = class(TBasicAction)
  private
    fRoutineName: string;
      {Name of routine}
  public
    property RoutineName: string
      read fRoutineName write fRoutineName;
      {Name of routine to be displayed}
  end;


implementation

end.

