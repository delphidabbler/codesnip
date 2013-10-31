{
 * UUnitsChkListMgr.pas
 *
 * Implements class that manages check list box controls that display lists of
 * Delphi units. Builds expandable list, sets check marks for specified units
 * and gets list of checked units.
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
 * The Original Code is UUnitsChkListMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UUnitsChkListMgr;


interface


uses
  // Delphi
  Classes, CheckLst,
  // Project
  UIStringList;


type

  {
  TUnitsChkListMgr:
    Manages check list box controls that display lists of Delphi units. Builds
    expandable list, sets check marks for specified units and gets list of
    checked units.
  }
  TUnitsChkListMgr = class(TObject)
  private
    fCLB: TCheckListBox;    // Check list box being managed
    procedure InitStandardUnits;
      {Initialises unit list with standard units that are always available.
      }
  public
    constructor Create(const CLB: TCheckListBox);
      {Class constructor. Sets up object to manage a specified check list box.
        @param CLB [in] Check list box to be managed.
      }
    function IsValidUnitName(const UnitName: string): Boolean;
      {Checks if a unit name is valid.
        @param UnitName [in] Unit name to be checked.
        @return True if UnitName is valid, False if not.
      }
    function ContainsUnit(const UnitName: string): Boolean;
      {Checks if a unit is contained in the list.
        @param UnitName [in] Name of unit to check.
        @return True if unit in list, False if not.
      }
    procedure IncludeUnit(const UnitName: string; const Checked: Boolean);
      {Ensures that a unit is present in the check list and sets or clears the
      associated check mark.
        @param UnitName [in] Name of unit that must be in list. Added if not
          already present.
        @param Checked [in] Flag indicating whether to check (True) or clear
          (False) the unit's check mark.
      }
    procedure IncludeUnits(const Units: TStrings; const Checked: Boolean);
      {Ensures that a list of units are all present in the check list and sets
      or clears the associated check marks.
        @param Units [in] List of names of units that must be in list. Units are
        added if not already present.
        @param Checked [in] Flag indicating whether to check (True) or clear
          (False) all the units' check marks.
      }
    procedure GetCheckedUnits(const Strings: IStringList);
      {Gets names of all checked units list box.
        @param Strings [in] Received list of checked unit names. List overwrites
          any previous entries in Strings.
      }
  end;


implementation


uses
  // Delphi
  SysUtils;


{ TUnitsChkListMgr }

function TUnitsChkListMgr.ContainsUnit(const UnitName: string): Boolean;
  {Checks if a unit is contained in the list.
    @param UnitName [in] Name of unit to check.
    @return True if unit in list, False if not.
  }
begin
  Result := fCLB.Items.IndexOf(UnitName) >= 0;
end;

constructor TUnitsChkListMgr.Create(const CLB: TCheckListBox);
  {Class constructor. Sets up object to manage a specified check list box.
    @param CLB [in] Check list box to be managed.
  }
begin
  inherited Create;
  fCLB := CLB;
  InitStandardUnits;
end;

procedure TUnitsChkListMgr.GetCheckedUnits(const Strings: IStringList);
  {Gets names of all checked units list box.
    @param Strings [in] Received list of checked unit names. List overwrites any
      previous entries in Strings.
  }
var
  Idx: Integer; // loops through all listbox items
begin
  Strings.Clear;
  for Idx := 0 to Pred(fCLB.Items.Count) do
    if fCLB.Checked[Idx] then
      Strings.Add(fCLB.Items[Idx]);
end;

procedure TUnitsChkListMgr.IncludeUnit(const UnitName: string;
  const Checked: Boolean);
  {Ensures that a unit is present in the check list and sets or clears the
  associated check mark.
    @param UnitName [in] Name of unit that must be in list. Added if not already
      present.
    @param Checked [in] Flag indicating whether to check (True) or clear (False)
      the unit's check mark.
  }
var
  UnitIndex: Integer; // index of unit name in list (-1 if not found)
begin
  Assert(UnitName <> '', ClassName + '.IncludeUnit: UnitName is empty');
  Assert(IsValidUnitName(UnitName),
    ClassName + '.IncludeUnit: UnitName is not valid');
  UnitIndex := fCLB.Items.IndexOf(UnitName);
  if UnitIndex = -1 then
    UnitIndex := fCLB.Items.Add(UnitName);
  fCLB.Checked[UnitIndex] := Checked;
end;

procedure TUnitsChkListMgr.IncludeUnits(const Units: TStrings;
  const Checked: Boolean);
  {Ensures that a list of units are all present in the check list and sets or
  clears the associated check marks.
    @param Units [in] List of names of units that must be in list. Units are
    added if not already present.
    @param Checked [in] Flag indicating whether to check (True) or clear (False)
      all the units' check marks.
  }
var
  UnitName: string; // name of each unit in list
begin
  for UnitName in Units do
    IncludeUnit(UnitName, Checked);
end;

procedure TUnitsChkListMgr.InitStandardUnits;
  {Initialises unit list with standard units that are always available.
  }
const
  // list of standard units
  cStdUnits: array[1..10] of string = (
    'SysUtils', 'Classes', 'Controls', 'Messages',
    'Windows', 'Graphics', 'Types', 'ShlObj', 'ShellAPI', 'ActiveX'
  );
var
  Idx: Integer; // loops thru all standard units
begin
  fCLB.Clear;
  for Idx := Low(cStdUnits) to High(cStdUnits) do
    fCLB.Items.Add(cStdUnits[Idx]);
end;

function TUnitsChkListMgr.IsValidUnitName(const UnitName: string): Boolean;
  {Checks if a unit name is valid.
    @param UnitName [in] Unit name to be checked.
    @return True if UnitName is valid, False if not.
  }
begin
  Result := IsValidIdent(UnitName, True); // allow dots in unit name
end;

end.

