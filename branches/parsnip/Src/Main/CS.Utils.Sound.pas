{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Utility routines concerned with sound production.
}


unit CS.Utils.Sound;


interface


uses
  Windows;


///  <summary>Emits a sound indicating an erroneous key-press.</summary>
procedure KeyErrorBeep;


implementation


procedure KeyErrorBeep;
begin
  MessageBeep(MB_ICONERROR);
end;

end.
