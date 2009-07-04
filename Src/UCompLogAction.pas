{
 * UCompLogAction.pas
 *
 * Custom action used to request display of a compiler log. Stores reference to
 * required compiler's id in a property.
 *
 * v0.1 of 30 Jan 2005  - Original version.
 * v0.2 of 18 Feb 2005  - Removed debug code.
 * v0.3 of 18 Feb 2005  - Removed TCompLogAction's unused constructor and
 *                        destructor.
 * v0.4 of 04 Mar 2005  - Changed to get compiler info from ICompiler instead
 *                        of TDelphiCompiler (ICompiler adds support for Free
 *                        Pascal).
 * v0.5 of 20 Apr 2005  - Changed to use renamed IntfCompilers unit.
 * v1.0 of 24 May 2006  - Made minor changes to comments.
 * v1.1 of 09 Jan 2009  - Changed to reference a compiler ID rather than a
 *                        compiler instance. Property name changes from Compiler
 *                        to CompilerID.
 *                      - Made private section strict.
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
 * The Original Code is UCompLogAction.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UCompLogAction;


interface


uses
  // Delphi
  Classes,
  // Project
  IntfCompilers;


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

