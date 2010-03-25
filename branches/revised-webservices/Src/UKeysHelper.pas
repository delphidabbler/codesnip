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
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
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


function IsKeyCombination(const RequiredKey: Word;
  const RequiredShift: TShiftState; const ActualKey: Word;
  const ActualShift: TShiftState): Boolean;
  {Checks if a specified combination of keys has been pressed.
    @param RequiredKey [in] Code of required key.
    @param RequiredShift [in] Set of modifier keys required to be pressed with
      RequiredKey.
    @param ActualKey [in] Code of key actually pressed.
    @param ActualShift [in] Set of modifier keys actually pressed.
  }

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

function IsKeyCombination(const RequiredKey: Word;
  const RequiredShift: TShiftState; const ActualKey: Word;
  const ActualShift: TShiftState): Boolean;
  {Checks if a specified combination of keys has been pressed.
    @param RequiredKey [in] Code of required key.
    @param RequiredShift [in] Set of modifier keys required to be pressed with
      RequiredKey.
    @param ActualKey [in] Code of key actually pressed.
    @param ActualShift [in] Set of modifier keys actually pressed.
  }
begin
  Result := (ActualKey = RequiredKey) and
    (ExtractShiftKeys(ActualShift) = ExtractShiftKeys(RequiredShift));
end;

end.

