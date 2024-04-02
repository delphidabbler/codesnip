{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2024, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Class helper for TCustomActionList
 *
 * Extracted in 2024 from original UClassHelpers unit (2012-2021)
}

unit ClassHelpers.UActions;

interface

uses
  // Delphi
  ActnList;

type
  ///  <summary>Class helper that adds a method to TCustomActionList that can
  ///  update all the actions in the list.</summary>
  TActionListHelper = class helper for TCustomActionList
  public
    ///  <summary>Updates all actions in the action list by calling their Update
    ///  methods.</summary>
    procedure Update;
  end;

implementation

{ TActionListHelper }

procedure TActionListHelper.Update;
var
  Action: TContainedAction; // each action in list
begin
  for Action in Self do
    Action.Update;
end;

end.
