{
 * UEditSnippetAction.pas
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
 * The Original Code is UEditSnippetAction.pas, formerly UEditRoutineAction.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UEditSnippetAction;


interface


uses
  // Delphi
  Classes;


type
  ///  <summary>
  ///  Custom action used to request that a named user defined snippet is
  ///  edited.
  ///  </summary>
  TEditSnippetAction = class(TBasicAction)
  strict private
    var
      ///  <summary>Value of SnippetName property.</summary>
      fSnippetName: string;
  public
    ///  <sumary>Name of snippet to be edited.</summary>
    property SnippetName: string read fSnippetName write fSnippetName;
  end;


implementation

end.

