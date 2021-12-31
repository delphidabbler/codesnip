{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that can enable or disable all controls, actions and
 * menu items on a form depending on the form's Enabled property
}


unit UControlStateMgr;


interface


uses
  // Delphi
  Forms, Controls, Menus, ActnList, Classes;


type

  {
  TControlStateMgr:
    Class that can enable or disable all controls, actions and menu items on a
    form depending on the form's Enabled property.
  }
  TControlStateMgr = class(TObject)
  strict private
    fForm: TCustomForm;
      {Reference to form being managed}
    procedure UpdateControl(const Ctrl: TControl);
      {Updates a control if it has no associated action.
        @param Ctrl [in] Control to be updated.
      }
    procedure UpdateMenuItem(const MI: TMenuItem);
      {Updates a menu item if it has no action.
        @param MI [in] Menu item to be updated.
      }
    procedure UpdateActionList(const AL: TActionList);
      {Updates actions in an action list.
        @param AL [in] Action list.
      }
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
      {Handles OnUpdate event of an action list. Disabled all actions when form
      is disabled. Enables all actions without their own OnUpdate event handlers
      when form is enabled.
        @param Action [in] Action being updated.
        @param Handled [in/out] False when called. Set True to prevent further
          processing if action's Enabled property has been set.
      }
  public
    constructor Create(const Form: TCustomForm);
      {Class constructor. Sets up object.
        @param Form [in] Reference to form whose components are to be enabled /
          disabled.
      }
    procedure Update;
      {Updates state of form's components according to value of form's Enabled
      property.
      }
  end;


implementation


{ TControlStateMgr }

procedure TControlStateMgr.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
  {Handles OnUpdate event of an action list. Disabled all actions when form is
  disabled. Enables all actions without their own OnUpdate event handlers when
  form is enabled.
    @param Action [in] Action being updated.
    @param Handled [in/out] False when called. Set True to prevent further
      processing if action's Enabled property has been set.
  }
begin
  Handled := False;
  if Action is TCustomAction then
    if fForm.Enabled then
    begin
      // Form is enabled: we enabled any action without OnUpdate handler
      if not Assigned(Action.OnUpdate) then
      begin
        (Action as TCustomAction).Enabled := True;
        Handled := True;
      end;
    end
    else
    begin
      // Form is disabled: we disable all actions
      (Action as TCustomAction).Enabled := False;
      Handled := True;
    end;
end;

constructor TControlStateMgr.Create(const Form: TCustomForm);
  {Class constructor. Sets up object.
    @param Form [in] Reference to form whose components are to be enabled /
      disabled.
  }
begin
  Assert(Assigned(Form), ClassName + '.Create: Form is nil');
  inherited Create;
  fForm := Form;
end;

procedure TControlStateMgr.Update;
  {Updates state of form's components according to value of form's Enabled
  property.
  }
var
  Idx: Integer;     // loops through all components on form
  Cmp: TComponent;  // reference to each component
begin
  for Idx := 0 to Pred(fForm.ComponentCount) do
  begin
    Cmp := fForm.Components[Idx];
    if Cmp is TControl then
      UpdateControl(Cmp as TControl)
    else if Cmp is TMenuItem then
      UpdateMenuItem(Cmp as TMenuItem)
    else if Cmp is TActionList then
      UpdateActionList(Cmp as TActionList);
  end;
end;

procedure TControlStateMgr.UpdateActionList(const AL: TActionList);
  {Updates actions in an action list.
    @param AL [in] Action list.
  }
var
  Action: TContainedAction;   // refers to each action in list
  OldOnUpdate: TActionEvent;  // stores action list's existing OnUpdate handler
begin
  OldOnUpdate := AL.OnUpdate;
  try
    // use out own OnUpdate handler to update actions in list
    AL.OnUpdate := ActionListUpdate;
    for Action in AL do
      AL.UpdateAction(Action)
  finally
    AL.OnUpdate := OldOnUpdate;
  end;
end;

procedure TControlStateMgr.UpdateControl(const Ctrl: TControl);
  {Updates a control if it has no associated action.
    @param Ctrl [in] Control to be updated.
  }
begin
  if not Assigned(Ctrl.Action) then
    Ctrl.Enabled := fForm.Enabled;
end;

procedure TControlStateMgr.UpdateMenuItem(const MI: TMenuItem);
  {Updates a menu item if it has no action.
    @param MI [in] Menu item to be updated.
  }
begin
  if not Assigned(MI.Action) then
    MI.Enabled := fForm.Enabled;
end;

end.

