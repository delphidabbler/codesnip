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
 * Portions created by the Initial Developer are Copyright (C) 2005-2011 Peter
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
    fDisplaySnippetAction: TBasicAction;
      {Action that causes a named snippet to be displayed}
    fCompileSnippetAction: TBasicAction;
      {Action that causes current snippet to be compiled}
    fShowHintAction: TBasicAction;
      {Action that causes a hint to be displayed}
    fConfigCompilersAction: TBasicAction;
      {Action that displays configure compiler dialog box}
    fShowViewItemAction: TBasicAction;
      {Action that causes a view item to be displayed}
    fOverviewStyleChangeActions: array of TBasicAction;
      {List of actions triggered when display style in overview pane changes}
    fDisplayPaneChangeAction: TBasicAction;
      {List of actions triggered when current pane in detail view changes}
    fEditSnippetAction: TBasicAction;
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
    procedure DisplaySnippet(const SnippetName: WideString;
      UserDefined: WordBool);
      {Displays a named snippet.
        @param SnippetName [in] Name of snippet to display.
        @param UserDefined [in] Whether snippet is user defined.
      }
    procedure DisplayCategory(const CatID: WideString);
      {Displays an identified category.
        @param CatID [in] Id of category to display.
      }
    procedure CompileSnippet;
      {Compiles the current snippet.
      }
    procedure ShowHint(const Hint: WideString);
      {Displays a hint.
        @param Hint [in] Hint to be displayed.
      }
    procedure ConfigCompilers;
      {Displays configure compilers dialog box.
      }
    procedure ShowViewItem(ViewItem: IView; const NewTab: Boolean);
      {Displays a view item.
        @param ViewItem [in] View item to display.
        @param NewTab [in] Flag indicates whether view is to be displayed in
          new tab.
      }
    procedure ChangeOverviewStyle(const Style: Integer);
      {Changes display style of overview pane.
        @param Style [in] Required display style.
      }
    procedure ChangeDetailPane(const Pane: Integer);
      {Changes displayed pane in detail display area.
        @param Pane [in] Required new pane.
      }
    procedure EditSnippet(const SnippetName: WideString);
      {Edits a snippet.
        @param SnippetName [in] Name of snippet. Must be user defined.
      }
    procedure Donate;
      {Displays donate dialog box.
      }
    { ISetActions }
    procedure SetUpdateDbaseAction(const Action: TBasicAction);
      {Sets action triggered when user requests database update.
        @param Action [in] Required action.
      }
    procedure SetDisplaySnippetAction(const Action: TBasicAction);
      {Sets action triggered when a named snippet is requested to be displayed.
        @param Action [in] Required action.
      }
    procedure SetCompileSnippetAction(const Action: TBasicAction);
      {Sets action triggered when user wants to test-compile the current
      snippet.
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
    procedure SetDetailPaneChangeAction(const Action: TBasicAction);
      {Sets action that us triggered when different detail panes are required
      to be shown.
        @param Action [in] Required action.
      }
    procedure SetEditSnippetAction(const Action: TBasicAction);
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
  Compilers.UGlobals, UCategoryAction, UEditSnippetAction, USnippetAction,
  UViewItemAction;


{ TNotifier }

procedure TNotifier.ChangeDetailPane(const Pane: Integer);
  {Changes displayed pane in detail display area.
    @param Pane [in] Required new pane.
  }
begin
  if Assigned(fDisplayPaneChangeAction) then
  begin
    // TODO: change this for a custom action?
    fDisplayPaneChangeAction.Tag := Pane;
    fDisplayPaneChangeAction.Execute;
  end;
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

procedure TNotifier.CompileSnippet;
  {Compiles the current snippet.
  }
begin
  if Assigned(fCompileSnippetAction) then
    fCompileSnippetAction.Execute;
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

procedure TNotifier.DisplaySnippet(const SnippetName: WideString;
  UserDefined: WordBool);
  {Displays a named snippet.
    @param SnippetName [in] Name of snippet to display.
    @param UserDefined [in] Whether snippet is user defined.
  }
begin
  if Assigned(fDisplaySnippetAction) then
  begin
    (fDisplaySnippetAction as TSnippetAction).SnippetName := SnippetName;
    (fDisplaySnippetAction as TSnippetAction).UserDefined := UserDefined;
    fDisplaySnippetAction.Execute;
  end;
end;

procedure TNotifier.Donate;
  {Displays donate dialog box.
  }
begin
  if Assigned(fDonateAction) then
    fDonateAction.Execute;
end;

procedure TNotifier.EditSnippet(const SnippetName: WideString);
  {Edits a snippet.
    @param SnippetName [in] Name of snippet. Must be user defined.
  }
begin
  if Assigned(fEditSnippetAction) then
  begin
    (fEditSnippetAction as TEditSnippetAction).SnippetName := SnippetName;
    fEditSnippetAction.Execute;
  end;
end;

procedure TNotifier.SetCompileSnippetAction(
  const Action: TBasicAction);
  {Sets action triggered when user wants to test-compile the current snippet.
    @param Action [in] Required action.
  }
begin
  fCompileSnippetAction := Action;
end;

procedure TNotifier.SetConfigCompilersAction(const Action: TBasicAction);
  {Sets action triggered when user requests that configure compilers dialog box
  is to be displayed.
    @param Action [in] Required action.
  }
begin
  fConfigCompilersAction := Action;
end;

procedure TNotifier.SetDetailPaneChangeAction(const Action: TBasicAction);
begin
  fDisplayPaneChangeAction := Action;
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

procedure TNotifier.SetDisplaySnippetAction(
  const Action: TBasicAction);
  {Sets action triggered when a named snippet is requested to be displayed.
    @param Action [in] Required action.
  }
begin
  Assert(Action is TSnippetAction,
    ClassName + '.SetDisplaySnippetAction: Action is not TSnippetAction');
  Assert(Supports(Action, ISetNotifier),
    ClassName + '.SetDisplaySnippetAction: Action must support ISetNotifier');
  fDisplaySnippetAction := Action;
  (fDisplaySnippetAction as ISetNotifier).SetNotifier(Self);
end;

procedure TNotifier.SetDonateAction(const Action: TBasicAction);
  {Sets action triggered when user request that the donate dialog box is
  displays.
    @param Action [in] Required action.
  }
begin
  fDonateAction := Action;
end;

procedure TNotifier.SetEditSnippetAction(const Action: TBasicAction);
  {Sets action triggered when user requests a user defined snippet is to be
  edited.
    @param Action [in] Required action.
  }
begin
  Assert(Action is TEditSnippetAction,
    ClassName + '.SetEditSnippetAction: Action is not TEditSnippetAction');
  fEditSnippetAction := Action;
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

procedure TNotifier.ShowViewItem(ViewItem: IView; const NewTab: Boolean);
  {Displays a view item.
    @param ViewItem [in] View item to display.
    @param NewTab [in] Flag indicates whether view is to be displayed in
      new tab.
  }
begin
  if Assigned(fShowViewItemAction) then
  begin
    (fShowViewItemAction as TViewItemAction).ViewItem := ViewItem;
    (fShowViewItemAction as TViewItemAction).NewTab := NewTab;
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

end.

