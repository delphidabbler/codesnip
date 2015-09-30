{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines various character, string and resource id constants.
}


unit UConsts;


interface


uses
  // Delphi
  Windows;


const
  BACKSPACE   =  #8;    // backspace character
  TAB         =  #9;    // tab character
  LF          = #10;    // line feed character
  VTAB        = #11;    // vertical tab character
  FF          = #12;    // form feed character
  CR          = #13;    // carriage return character
  ESC         = #27;    // escape character
  SINGLEQUOTE = '''';   // single quote character
  DOUBLEQUOTE = '"';    // double quote character

  CRLF  = CR + LF;      // carriage return followed by line feed
  EOL   = sLineBreak;   // end of line character sequence for OS
  EOL2  = EOL + EOL;    // 2 end of line sequences

  RT_HTML = MakeIntResource(23);    // HTML resource identifier


implementation

end.

