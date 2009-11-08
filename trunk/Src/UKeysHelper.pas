{
 * UKeysHelper.pas
 *
 * Helper routines for working with key presses and shift states.
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
 * The Original Code is UKeysHelper.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UKeysHelper;


interface


uses
  // Delphi
  Classes;


function ExtractShiftKeys(const Shift: TShiftState): TShiftState;
  {Extracts any shift keys (ssShift, ssCtrl or ssAlt) from a shift state set.
    @param Shift [in] Shift state set.
    @return Set containing values for any of the shift keys contained in Shift.
  }

function HasShiftKeys(const Shift: TShiftState): Boolean;
  {Checks if a shift state set contains one or more of the ssShoft, ssCtrl or
  ssAlt shift keys.
    @param Shift [in] Shift state set.
    @return True if Shift contains shift keys, False if not.
  }


implementation


function ExtractShiftKeys(const Shift: TShiftState): TShiftState;
  {Extracts any shift keys (ssShift, ssCtrl or ssAlt) from a shift state set.
    @param Shift [in] Shift state set.
    @return Set containing values for any of the shift keys contained in Shift.
  }
begin
  Result := Shift * [ssShift, ssCtrl, ssAlt];
end;

function HasShiftKeys(const Shift: TShiftState): Boolean;
  {Checks if a shift state set contains one or more of the ssShoft, ssCtrl or
  ssAlt shift keys.
    @param Shift [in] Shift state set.
    @return True if Shift contains shift keys, False if not.
  }
begin
  Result := ExtractShiftKeys(Shift) <> [];
end;

end.

