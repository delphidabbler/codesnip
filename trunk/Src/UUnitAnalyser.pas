{
 * UUnitAnalyser.pas
 *
 * Static class that analyses and provides information about unit source code.
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
 * The Original Code is UUnitAnalyser.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UUnitAnalyser;


interface


uses
  // Delphi
  SysUtils,
  // Project
  UBaseObjects;


type
  { TODO: Provide method that gets unit name from source code: use pascal
          lexer to do this.
          Use the method to get test unit file name from a provided complete
          unit and to suggest unit name for code imported from a file }
  TUnitAnalyser = class sealed(TNoConstructObject)
  public
    class function RequiredEncoding(const SourceCode: string): TEncoding;
  end;


implementation


uses
  // Project
  UEncodings;

{ TUnitAnalyser }

class function TUnitAnalyser.RequiredEncoding(const SourceCode: string):
  TEncoding;
begin
  if EncodingSupportsString(SourceCode, TEncoding.Default) then
    Result := TEncoding.Default
  else
    Result := TEncoding.UTF8;
end;

end.
