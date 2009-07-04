{
 * URoutineAction.pas
 *
 * Custom action used to request display of a routine by name. Stores name of
 * required routine and its source database in properties.
 *
 * v0.1 of 30 Jan 2005  - Original version.
 * v0.2 of 18 Feb 2005  - Removed debug code.
 * v0.3 of 18 Feb 2005  - Deleted unused TRoutineAction contructor and
 *                        destructor.
 * v1.0 of 24 May 2006  - Made minor change to comments.
 * v1.1 of 31 Aug 2008  - Added new UserDefined property that indicates if
 *                        routine is from main or user database.
 *
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
 * The Original Code is URoutineAction.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit URoutineAction;


interface


uses
  // Delphi
  Classes;


type

  {
  TRoutineAction:
    Custom action used to request display of a named routine. Stores name of
    required routine, and whether it is user defined, in properties.
  }
  TRoutineAction = class(TBasicAction)
  private
    fRoutineName: string;
      {Name of routine}
    fUserDefined: Boolean;
      {Whether routine is user-defined}
  public
    property RoutineName: string
      read fRoutineName write fRoutineName;
      {Name of routine to be displayed}
    property UserDefined: Boolean
      read fUserDefined write fUserDefined;
      {Flag indicating whether routine is user defined, i.e. it comes from user
      database}
  end;


implementation

end.

