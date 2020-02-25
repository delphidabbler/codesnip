{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2020, Peter Johnson (gravatar.com/delphidabbler).
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
  SUB         = #26;    // ASCII SUB character
  ESC         = #27;    // escape character
  SINGLEQUOTE = '''';   // single quote character
  DOUBLEQUOTE = '"';    // double quote character

  CRLF  = CR + LF;      // carriage return followed by line feed
  EOL   = CRLF;         // end of line character sequence for Windows systems
  EOL2  = EOL + EOL;    // 2 end of line sequences
  EOF   = SUB;          // end of file character

  RT_HTML = MakeIntResource(23);    // HTML resource identifier


implementation

end.

