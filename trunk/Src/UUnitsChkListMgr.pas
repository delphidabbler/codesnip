{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements class that manages check list box controls that display lists of
 * Delphi units. Builds expandable list, sets check marks for specified units
 * and gets list of checked units.
}


unit UUnitsChkListMgr;


interface


uses
  // Delphi
  Classes, CheckLst, Controls,
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
  strict private
    var
      fCLB: TCheckListBox;          // Check list box being managed
      ///  <summary>List of reserved unit names.</summary>
      ///  <remarks>Deafult units can't be deleted from list.</remarks>
      fReservedUnits: IStringList;
      ///  <summary>List of default unit names.</summary>
      ///  <remarks>Default units are those non-reserved units added to list
      ///  when user has not specified any units or requests restoration of
      ///  defaults.</remarks>
      fDefaultUnits: IStringList;
    ///  <summary>Traps check list box's OnMouseDown event to ensure right
    ///  click selects list item under mouse.</summary>
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    ///  <summary>Initialises unit list with reserved and any stored units.
    ///  </summary>
    procedure InitList;
    ///  <summary>Saves any non-reserved units to storage.</summary>
    procedure SaveList;
  public
    constructor Create(const CLB: TCheckListBox);
      {Class constructor. Sets up object to manage a specified check list box.
        @param CLB [in] Check list box to be managed.
      }
    ///  <summary>Destroys manager object and updates persistent storage.
    ///  </summary>
    destructor Destroy; override;
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
    ///  <summary>Checks if currently selected unit can be removed from list.
    ///  </summary>
    ///  <remarks>Returns False if there is no selection.</remarks>
    function CanDeleteSelectedItem: Boolean;
    ///  <summary>Deletes currently selected unot from list if possible.
    ///  </summary>
    ///  <remarks>Does nothing if no selection or if selected unit can't be
    ///  deleted.</remarks>
    procedure DeleteSelectedItem;
    ///  <summary>Restores default unit list.</summary>
    procedure RestoreDefaults;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  USettings;


{ TUnitsChkListMgr }

function TUnitsChkListMgr.CanDeleteSelectedItem: Boolean;
var
  Idx: Integer;
begin
  Idx := fCLB.ItemIndex;
  if (Idx < 0) or (Idx >= fCLB.Count) then
    Exit(False);
  Result := not fReservedUnits.Contains(fCLB.Items[Idx]);
end;

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
  fCLB.OnMouseDown := MouseDown;
  fReservedUnits := TIStringList.Create(
    ['SysUtils', 'Classes', 'Windows', 'Graphics']
  );
  fDefaultUnits := TIStringList.Create(
    ['Controls', 'Messages', 'Types', 'ShlObj', 'ShellAPI', 'ActiveX', 'Math']
  );
  InitList;
end;

procedure TUnitsChkListMgr.DeleteSelectedItem;
begin
  if not CanDeleteSelectedItem then
    Exit;
  fCLB.Items.Delete(fCLB.ItemIndex);
end;

destructor TUnitsChkListMgr.Destroy;
begin
  SaveList;
  inherited;
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

procedure TUnitsChkListMgr.InitList;
  {Initialises unit list with standard units that are always available.
  }
var
  StoredUnits: IStringList;
  Storage: ISettingsSection;
begin
  fReservedUnits.CopyTo(fCLB.Items, True);
  Storage := Settings.ReadSection(ssUnits);
  if Storage.ItemCount > 0 then
  begin
    StoredUnits := Storage.GetStrings('Count', 'Unit%d');
    if StoredUnits.Count > 0 then
      StoredUnits.CopyTo(fCLB.Items, False);
  end
  else
    fDefaultUnits.CopyTo(fCLB.Items, False);
end;

function TUnitsChkListMgr.IsValidUnitName(const UnitName: string): Boolean;
  {Checks if a unit name is valid.
    @param UnitName [in] Unit name to be checked.
    @return True if UnitName is valid, False if not.
  }
begin
  Result := IsValidIdent(UnitName, True); // allow dots in unit name
end;

procedure TUnitsChkListMgr.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then
    fCLB.ItemIndex := fCLB.ItemAtPos(Point(X, Y), True);
end;

procedure TUnitsChkListMgr.RestoreDefaults;
var
  CheckedUnits: IStringList;  // list of checked units before restoration
  SelectedUnit: string;       // name of any selected unit before restoration
  U: string;                  // each unit in CheckedUnits
  Idx: Integer;               // index of each U in restored list
begin
  CheckedUnits := TIStringList.Create;
  // record any checked and SelectedUnit units
  GetCheckedUnits(CheckedUnits);
  if fCLB.ItemIndex >= 0 then
    SelectedUnit := fCLB.Items[fCLB.ItemIndex]
  else
    SelectedUnit := '';
  // replace list box contents with default
  fReservedUnits.CopyTo(fCLB.Items, True);  // overwrites existing list
  fDefaultUnits.CopyTo(fCLB.Items, False);
  // restore any checks and selected item if still present
  for U in CheckedUnits do
  begin
    Idx := fCLB.Items.IndexOf(U);
    if Idx >= 0 then
      fCLB.Checked[Idx] := True;
  end;
  fCLB.ItemIndex := fCLB.Items.IndexOf(SelectedUnit);
end;

procedure TUnitsChkListMgr.SaveList;
var
  Storage: ISettingsSection;
  U: string;
  CustomUnits: IStringList;
begin
  CustomUnits := TIStringList.Create;
  for U in fCLB.Items do
    if not fReservedUnits.Contains(U) then
      CustomUnits.Add(U);
  Storage := Settings.EmptySection(ssUnits);
  Storage.SetStrings('Count', 'Unit%d', CustomUnits);
  Storage.Save;
end;

end.

