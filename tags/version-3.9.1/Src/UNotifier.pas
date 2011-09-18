{
 * UNotifier.pas
 *
 * Object that notifies the main application of certain user-initiated events.
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
 * The Original Code is UNotifier.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UNotifier;


interface


uses
  // Delphi
  Classes, ActiveX,
  // Project
  IntfNotifier, UView;


type

  {
  TNotifier:
    Notifies the main application of certain user-initiated events. Objects that
    receive the user interaction call methods using this object's INotifier
    interface which in turn trigger actions that have been registered with the
    object via its ISetActions interface. In effect the object is a central
    clearing house for user interaction with certain controls.
  }
  TNotifier = class(TInterfacedObject,
    INotifier,    // provides methods that trigger notifications
    ISetActions   // associates action objects with notifications
  )
  strict private
    fUpdateDbaseAction: TBasicAction;
      {Action that triggers database update}
    fDisplayRoutineAction: TBasicAction;
      {Action that causes a named snippet to be displayed}
    fCompileRoutineAction: TBasicAction;
      {Action that causes current snippet to be compiled}
    fViewCompilerLogAction: TBasicAction;
      {Action that causes a compiler log to be displayed}
    fShowHintAction: TBasicAction;
      {Action that causes a hint to be displayed}
    fConfigCompilersAction: TBasicAction;
      {Action that displays configure compiler dialog box}
    fShowViewItemAction: TBasicAction;
      {Action that causes a view item to be displayed}
    fOverviewStyleChangeActions: array of TBasicAction;
      {List of actions triggered when display style in overview pane changes}
    fDisplayPaneChangeActions: array of TBasicAction;
      {List of actions triggered when current pane in detail view changes}
    fShowTestUnitAction: TBasicAction;
      {Action that causes a test unit to be displayed}
    fEditRoutineAction: TBasicAction;
      {Action that causes a user defined snippet to be edited}
    fDonateAction: TBasicAction;
      {Action that displays donate dialog box}
    fDisplayCategoryAction: TBasicAction;
      {Action that causes a category to be displayed}
  protected // do not make strict
    { INotifier }
    procedure UpdateDbase;
      {Updates database.
      }
    procedure DisplayRoutine(const RoutineName: WideString;
      UserDefined: WordBool);
      {Displays a named snippet.
        @param RoutineName [in] Name of snippet to display.
        @param UserDefined [in] Whether snippet is user defined.
      }
    procedure DisplayCategory(const CatID: WideString);
      {Displays an identified category.
        @param CatID [in] Id of category to display.
      }
    procedure CompileRoutine;
      {Compiles the current snippet.
      }
    procedure ViewCompilerLog(CompID: SYSINT);
      {Displays a compiler log.
        @param CompID [in] Version of Delphi for which we need to display log.
          CompID is the ordinal value of the required compiler version
          enumerated type.
      }
    procedure ShowHint(const Hint: WideString);
      {Displays a hint.
        @param Hint [in] Hint to be displayed.
      }
    procedure ConfigCompilers;
      {Displays configure compilers dialog box.
      }
    procedure ShowViewItem(const ViewItem: TViewItem);
      {Displays a view item.
        @param ViewItem [in] View item to display.
      }
    procedure ChangeOverviewStyle(const Style: Integer);
      {Changes display style of overview pane.
        @param Style [in] Required display style.
      }
    procedure ChangeDetailPane(const Pane: Integer);
      {Changes displayed pane in detail display area.
        @param Pane [in] Required new pane.
      }
    procedure ShowTestUnit;
      {Displays test unit.
      }
    procedure EditRoutine(const RoutineName: WideString);
      {Edits a snippet.
        @param RoutineName [in] Name of snippet. Must be user defined.
      }
    procedure Donate;
      {Displays donate dialog box.
      }
    { ISetActions }
    procedure SetUpdateDbaseAction(const Action: TBasicAction);
      {Sets action triggered when user requests database update.
        @param Action [in] Required action.
      }
    procedure SetDisplayRoutineAction(const Action: TBasicAction);
      {Sets action triggered when a named snippet is requested to be displayed.
        @param Action [in] Required action.
      }
    procedure SetCompileRoutineAction(const Action: TBasicAction);
      {Sets action triggered when user wants to test-compile the current
      snippet.
        @param Action [in] Required action.
      }
    procedure SetViewCompilerLogAction(const Action: TBasicAction);
      {Sets action triggered when user wants to view a compiler log.
        @param Action [in] Required action.
      }
    procedure SetShowHintAction(const Action: TBasicAction);
      {Sets action triggered when user moves mouse over hot links etc. that
      cause hints to be displayed.
        @param Action [in] Required action.
      }
    procedure SetConfigCompilersAction(const Action: TBasicAction);
      {Sets action triggered when user requests that configure compilers dialog
      box is to be displayed.
        @param Action [in] Required action.
      }
    procedure SetShowViewItemAction(const Action: TBasicAction);
      {Sets action triggered when user requests a view item is displayed.
        @param Action [in] Required action.
      }
    procedure SetOverviewStyleChangeActions(
      const Actions: array of TBasicAction);
      {Sets actions that are triggered when different overview display styles
      are requested.
        @param Actions [in] Dynamic array of required actions: one per display
          style.
      }
    procedure SetDetailPaneChangeActions(const Actions: array of TBasicAction);
      {Sets actions that are triggered when different detail panes are required
      to be shown.
        @param Actions [in] Dynamic array of required actions: one per detail
          display tab.
      }
    procedure SetShowTestUnitAction(const Action: TBasicAction);
      {Sets action triggered where displays a test unit.
        @param Action [in] Required action.
      }
    procedure SetEditRoutineAction(const Action: TBasicAction);
      {Sets action triggered when user requests a user defined snippet is to be
      edited.
        @param Action [in] Required action.
      }
    procedure SetDonateAction(const Action: TBasicAction);
      {Sets action triggered when user request that the donate dialog box is
      displays.
        @param Action [in] Required action.
      }
    procedure SetDisplayCategoryAction(const Action: TBasicAction);
      {Sets actions triggered when a category is requested to be displayed.
        @param Action [in] Required action.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, StdActns,
  // Project
  Compilers.UGlobals, UCategoryAction, UCompLogAction, UEditRoutineAction,
  URoutineAction, UViewItemAction;


{ TNotifier }

procedure TNotifier.ChangeDetailPane(const Pane: Integer);
  {Changes displayed pane in detail display area.
    @param Pane [in] Required new pane.
  }
begin
  Assert((Pane >= 0) and (Pane < Length(fDisplayPaneChangeActions)),
    ClassName + '.ChangeDetailPane: Pane out of range');   
  if Assigned(fDisplayPaneChangeActions[Pane]) then
    fDisplayPaneChangeActions[Pane].Execute;
end;

procedure TNotifier.ChangeOverviewStyle(const Style: Integer);
  {Changes display style of overview pane.
    @param Style [in] Required display style.
  }
begin
  Assert((Style >= 0) and (Style < Length(fOverviewStyleChangeActions)),
    ClassName + '.ChangeOverviewStyle: Pane out of range');
  if Assigned(fOverviewStyleChangeActions[Style]) then
    fOverviewStyleChangeActions[Style].Execute;
end;

procedure TNotifier.CompileRoutine;
  {Compiles the current snippet.
  }
begin
  if Assigned(fCompileRoutineAction) then
    fCompileRoutineAction.Execute;
end;

procedure TNotifier.ConfigCompilers;
  {Displays configure compilers dialog box.
  }
begin
  if Assigned(fConfigCompilersAction) then
    fConfigCompilersAction.Execute;
end;

procedure TNotifier.DisplayCategory(const CatID: WideString);
  {Displays an identified category.
    @param CatID [in] Id of category to display.
  }
begin
  if Assigned(fDisplayCategoryAction) then
  begin
    (fDisplayCategoryAction as TCategoryAction).CatID := CatID;
    fDisplayCategoryAction.Execute;
  end;
end;

procedure TNotifier.DisplayRoutine(const RoutineName: WideString;
  UserDefined: WordBool);
  {Displays a named snippet.
    @param RoutineName [in] Name of snippet to display.
    @param UserDefined [in] Whether snippet is user defined.
  }
begin
  if Assigned(fDisplayRoutineAction) then
  begin
    (fDisplayRoutineAction as TRoutineAction).RoutineName := RoutineName;
    (fDisplayRoutineAction as TRoutineAction).UserDefined := UserDefined;
    fDisplayRoutineAction.Execute;
  end;
end;

procedure TNotifier.Donate;
  {Displays donate dialog box.
  }
begin
  if Assigned(fDonateAction) then
    fDonateAction.Execute;
end;

procedure TNotifier.EditRoutine(const RoutineName: WideString);
  {Edits a snippet.
    @param RoutineName [in] Name of snippet. Must be user defined.
  }
begin
  if Assigned(fEditRoutineAction) then
  begin
    (fEditRoutineAction as TEditRoutineAction).RoutineName := RoutineName;
    fEditRoutineAction.Execute;
  end;
end;

procedure TNotifier.SetCompileRoutineAction(
  const Action: TBasicAction);
  {Sets action triggered when user wants to test-compile the current snippet.
    @param Action [in] Required action.
  }
begin
  fCompileRoutineAction := Action;
end;

procedure TNotifier.SetConfigCompilersAction(const Action: TBasicAction);
  {Sets action triggered when user requests that configure compilers dialog box
  is to be displayed.
    @param Action [in] Required action.
  }
begin
  fConfigCompilersAction := Action;
end;

procedure TNotifier.SetDetailPaneChangeActions(
  const Actions: array of TBasicAction);
  {Sets actions that are triggered when different detail panes are required to
  be shown.
    @param Actions [in] Dynamic array of required actions: one per detail
      display tab.
  }
var
  Idx: Integer; // loops thru actions
begin
  SetLength(fDisplayPaneChangeActions, Length(Actions));
  for Idx := Low(Actions) to High(Actions) do
    fDisplayPaneChangeActions[Idx] := Actions[Idx];
end;

procedure TNotifier.SetDisplayCategoryAction(const Action: TBasicAction);
  {Sets actions triggered when a category is requested to be displayed.
    @param Action [in] Required action.
  }
begin
  Assert(Action is TCategoryAction,
    ClassName + '.SetDisplayCategoryAction: Action is not TCategoryAction');
  Assert(Supports(Action, ISetNotifier),
    ClassName + '.SetDisplayCategoryAction: Action must support ISetNotifier');
  fDisplayCategoryAction := Action;
  (fDisplayCategoryAction as ISetNotifier).SetNotifier(Self);
end;

procedure TNotifier.SetDisplayRoutineAction(
  const Action: TBasicAction);
  {Sets action triggered when a named snippet is requested to be displayed.
    @param Action [in] Required action.
  }
begin
  Assert(Action is TRoutineAction,
    ClassName + '.SetDisplayRoutineAction: Action is not TRoutineAction');
  Assert(Supports(Action, ISetNotifier),
    ClassName + '.SetDisplayRoutineAction: Action must support ISetNotifier');
  fDisplayRoutineAction := Action;
  (fDisplayRoutineAction as ISetNotifier).SetNotifier(Self);
end;

procedure TNotifier.SetDonateAction(const Action: TBasicAction);
  {Sets action triggered when user request that the donate dialog box is
  displays.
    @param Action [in] Required action.
  }
begin
  fDonateAction := Action;
end;

procedure TNotifier.SetEditRoutineAction(const Action: TBasicAction);
  {Sets action triggered when user requests a user defined snippet is to be
  edited.
    @param Action [in] Required action.
  }
begin
  Assert(Action is TEditRoutineAction,                     
    ClassName + '.SetEditRoutineAction: Action is not TEditRoutineAction');
  fEditRoutineAction := Action;
end;

procedure TNotifier.SetOverviewStyleChangeActions(
  const Actions: array of TBasicAction);
  {Sets actions that are triggered when different overview display styles are
  requested.
    @param Actions [in] Dynamic array of required actions: one per display
      style.
  }
var
  Idx: Integer; // loops thru actions
begin
  SetLength(fOverviewStyleChangeActions, Length(Actions));
  for Idx := Low(Actions) to High(Actions) do
    fOverviewStyleChangeActions[Idx] := Actions[Idx];
end;

procedure TNotifier.SetShowHintAction(const Action: TBasicAction);
  {Sets action triggered when user moves mouse over hot links etc. that cause
  hints to be displayed.
    @param Action [in] Required action.
  }
begin
  Assert(Action is THintAction,                            
    ClassName + '.SetShowHintAction: Action is not THintAction');
  fShowHintAction := Action;
end;

procedure TNotifier.SetShowTestUnitAction(const Action: TBasicAction);
begin
  fShowTestUnitAction := Action;
end;

procedure TNotifier.SetShowViewItemAction(const Action: TBasicAction);
  {Sets action triggered when user requests a view item is displayed.
    @param Action [in] Required action.
  }
begin
  fShowViewItemAction := Action;
end;

procedure TNotifier.SetUpdateDbaseAction(
  const Action: TBasicAction);
  {Sets action triggered when user requests database update.
    @param Action [in] Required action.
  }
begin
  fUpdateDbaseAction := Action;
end;

procedure TNotifier.SetViewCompilerLogAction(
  const Action: TBasicAction);
  {Sets action triggered when user wants to view a compiler log.
    @param Action [in] Required action.
  }
begin
  Assert(Action is TCompLogAction,
    ClassName + '.SetViewCompilerLogAction: Action is not TCompLogAction');
  fViewCompilerLogAction := Action;
end;

procedure TNotifier.ShowHint(const Hint: WideString);
  {Displays a hint.
    @param Hint [in] Hint to be displayed.
  }
begin
  if Assigned(fShowHintAction) then
  begin
    // record hint text in action
    (fShowHintAction as THintAction).Hint := Hint;
    fShowHintAction.Execute;
  end;
end;

procedure TNotifier.ShowTestUnit;
begin
  if Assigned(fShowTestUnitAction) then
    fShowTestUnitAction.Execute;
end;

procedure TNotifier.ShowViewItem(const ViewItem: TViewItem);
  {Displays a view item.
    @param ViewItem [in] View item to display.
  }
begin
  if Assigned(fShowViewItemAction) then
  begin
    (fShowViewItemAction as TViewItemAction).ViewItem := ViewItem;
    fShowViewItemAction.Execute;
  end;
end;

procedure TNotifier.UpdateDbase;
  {Updates database.
  }
begin
  if Assigned(fUpdateDbaseAction) then
    fUpdateDbaseAction.Execute;
end;

procedure TNotifier.ViewCompilerLog(CompID: SYSINT);
  {Displays a compiler log.
    @param CompID [in] Version of Delphi for which we need to display log.
      CompID is the ordinal value of the required compiler version enumerated
      type.
  }
begin
  if Assigned(fViewCompilerLogAction) then
  begin
    (fViewCompilerLogAction as TCompLogAction).CompilerID :=
      TCompilerID(CompID);
    fViewCompilerLogAction.Execute;
  end;
end;

end.

