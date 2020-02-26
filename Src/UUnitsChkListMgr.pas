{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2020, Peter Johnson (gravatar.com/delphidabbler).
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
  ///  <summary>Manages check list box controls that display lists of Delphi
  ///  units. Unit list is persisted.</summary>
  TUnitsChkListMgr = class(TObject)
  strict private
    var
      ///  <summary>Check list box being managed.</summary>
      fCLB: TCheckListBox;
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
    ///  <summary>Constructs manager object for given check list box and loads
    ///  units from persistent storage.</summary>
    constructor Create(const CLB: TCheckListBox);
    ///  <summary>Destroys manager object and updates persistent storage.
    ///  </summary>
    destructor Destroy; override;
    ///  <summary>Checks if given unit name is valid.</summary>
    function IsValidUnitName(const UnitName: string): Boolean;
    ///  <summary>Checks if given unit name is contained in the list.</summary>
    function ContainsUnit(const UnitName: string): Boolean;
    ///  <summary>Ensures that a unit name is included in the list.</summary>
    ///  <param name="UnitName">string [in] Name of unit to be included. It is
    ///  added to the list unless already present.</param>
    ///  <param name="Checked">Boolean [in] Determines whether named unit is to
    ///  be checked.</param>
    procedure IncludeUnit(const UnitName: string; const Checked: Boolean);
    ///  <summary>Ensures all the given units are include in the list.</summary>
    ///  <param name="Units">TStrings [in] List of unit names to be included.
    ///  Each is added to the list unless already present.</param>
    ///  <param name="Checked">Boolean [in] Determines whether each unit is to
    ///  be checked.</param>
    procedure IncludeUnits(const Units: TStrings; const Checked: Boolean);
    ///  <summary>Returns a list of names of all checked units in list box.
    ///  </summary>
    function GetCheckedUnits: IStringList;
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
    ///  <summary>Clears checks from all items in list box.</summary>
    procedure ClearChecks;
    ///  <summary>Tests if any items in list box are checked.</summary>
    function HasCheckedItems: Boolean;
  end;


implementation


uses
  // Delphi
  SysUtils, Types {for inlining},
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

procedure TUnitsChkListMgr.ClearChecks;
var
  Idx: Integer; // loops through all items in check list box
begin
  for Idx := 0 to Pred(fCLB.Count) do
    fCLB.Checked[Idx] := False;
end;

function TUnitsChkListMgr.ContainsUnit(const UnitName: string): Boolean;
begin
  Result := fCLB.Items.IndexOf(UnitName) >= 0;
end;

constructor TUnitsChkListMgr.Create(const CLB: TCheckListBox);
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

function TUnitsChkListMgr.GetCheckedUnits: IStringList;
var
  Idx: Integer; // loops through all listbox items
begin
  Result := TIStringList.Create;
  for Idx := 0 to Pred(fCLB.Items.Count) do
    if fCLB.Checked[Idx] then
      Result.Add(fCLB.Items[Idx]);
end;

function TUnitsChkListMgr.HasCheckedItems: Boolean;
var
  Idx: Integer; // lopps thru each item in check list box
begin
  for Idx  := 0 to Pred(fCLB.Count) do
    if fCLB.Checked[Idx] then
      Exit(True);
  Result := False;
end;

procedure TUnitsChkListMgr.IncludeUnit(const UnitName: string;
  const Checked: Boolean);
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
var
  UnitName: string; // name of each unit in list
begin
  for UnitName in Units do
    IncludeUnit(UnitName, Checked);
end;

procedure TUnitsChkListMgr.InitList;
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
  // record any checked and SelectedUnit units
  CheckedUnits := GetCheckedUnits;
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

