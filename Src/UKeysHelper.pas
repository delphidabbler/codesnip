{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Helper routines for working with key presses and shift states.
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
  {Checks if a shift state set contains one or more of the ssShift, ssCtrl or
  ssAlt shift keys.
    @param Shift [in] Shift state set.
    @return True if Shift contains shift keys, False if not.
  }

function IsValidDecimalNumberKey(const Text: string; var Key: Char): Boolean;
  {Checks if a key is permitted within a partially completed decimal number.
    @param Text [in] Text of partially completed decimal number. Used to test if
      for duplicate decimal points.
    @param Key [in/out] Key to test. Set to #0 if not valid.
    @return True if key is permitted, False if not.
  }


///  <summary>Returns set of any shift keys pressed when the function was
///  called.</summary>
///  <remarks>Shift keys are ssShift, ssCtrl and ssAlt.</remarks>
function ShiftKeysPressed: TShiftState;


implementation


uses
  // Delphi
  SysUtils, Character, Forms,
  // Project
  UConsts, UStrUtils;


function ShiftKeysPressed: TShiftState;
begin
  Result := ExtractShiftKeys(Forms.KeyboardStateToShiftState);
end;

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

function IsValidDecimalNumberKey(const Text: string; var Key: Char): Boolean;
  {Checks if a key is permitted within a partially completed decimal number.
    @param Text [in] Text of partially completed decimal number. Used to test if
      for duplicate decimal points.
    @param Key [in/out] Key to test. Set to #0 if not valid.
    @return True if key is permitted, False if not.
  }
begin
  Result := True;
  if (Key = FormatSettings.DecimalSeparator) then
  begin
    // Only allow decimal point if not already entered: can't have more than one
    if StrContainsStr(FormatSettings.DecimalSeparator, Text) then
      Result := False;
  end
  else if not Key.IsDigit and (Key <> BACKSPACE) then
    // Disallow any other characters other than backspace or digits
    Result := False;
  if not Result then
    Key := #0;
end;

end.

