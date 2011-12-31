{
 * UConsts.pas
 *
 * Defines various character, string and resource id constants.
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
 * The Original Code is UConsts.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UConsts;


interface


uses
  // Delphi
  Windows;


const
  BACKSPACE =  #8;      // backspace character
  TAB       =  #9;      // tab character
  LF        = #10;      // line feed character
  VTAB      = #11;      // vertical tab character
  FF        = #12;      // form feed character
  CR        = #13;      // carriage return character
  ESC       = #27;      // escape character

  CRLF  = CR + LF;      // carriage return followed by line feed
  EOL   = CRLF;         // end of line character sequence for Windows systems
  EOL2  = EOL + EOL;    // 2 end of line sequences

  RT_HTML = MakeIntResource(23);    // HTML resource identifier


implementation

end.

