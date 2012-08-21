{
 * UCompLogAction.pas
 *
 * Custom action used to request display of a compiler log. Stores reference to
 * required compiler's id in a property.
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
 * The Original Code is UCompLogAction.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UCompLogAction;


interface


uses
  // Delphi
  Classes,
  // Project
  Compilers.UGlobals;


type

  {
  TCompLogAction:
    Custom action used to request display of a compiler log. Stores reference to
    id of required compiler.
  }
  TCompLogAction = class(TBasicAction)
  strict private
    fCompiler: TCompilerID;
      {Reference to compiler id}
  public
    property CompilerID: TCompilerID
      read fCompiler write fCompiler;
      {Id of compiler for which we are requesting display of log}
  end;


implementation

end.

