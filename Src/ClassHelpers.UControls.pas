{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2024, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Class helper for TControl.
 *
 * Extracted in 2024 from original UClassHelpers unit (2012-2021).
}

unit ClassHelpers.UControls;

interface

uses
  // Delphi
  Controls, Menus;

type
  ///  <summary>Class helper that adds functionality to TControl.</summary>
  TControlHelper = class helper for TControl
  public
    ///  <summary>Gets reference to pop-up menu assigned to protected PopupMenu
    ///  property.</summary>
    function GetPopupMenu: TPopupMenu;
    ///  <summary>Checks if protected PopupMenu property is assigned.</summary>
    function HasPopupMenu: Boolean;
    ///  <summary>Refreshes control's action. Any changes in action that affect
    ///  state of control are reflected in control.</summary>
    procedure RefreshAction;
    ///  <summary>Refreshes all owned controls to reflect any changes in their
    ///  associated actions.</summary>
    procedure RefreshActions;
  end;

implementation

{ TControlHelper }

function TControlHelper.GetPopupMenu: TPopupMenu;
begin
  Result := PopupMenu;
end;

function TControlHelper.HasPopupMenu: Boolean;
begin
  Result := Assigned(PopupMenu);
end;

procedure TControlHelper.RefreshAction;
begin
  if Assigned(Action) then
    ActionChange(Action, False);
end;

procedure TControlHelper.RefreshActions;
var
  Idx: Integer; // loops through all controls
begin
  for Idx := 0 to Pred(ComponentCount) do
    if Components[Idx] is TControl then
      (Components[Idx] as TControl).RefreshAction;
end;

end.

