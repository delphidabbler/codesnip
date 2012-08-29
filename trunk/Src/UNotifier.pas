{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Object that notifies the main application of certain user-initiated events.
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
    fNewSnippetAction: TBasicAction;
      {Action that causes the Snippets Editor to be opened ready to create a new
      snippet}
    fNewsAction: TBasicAction;
      {Action that causes news items from CodeSnip news feed to be displayed}
    fCheckForUpdatesAction: TBasicAction;
      {Action that causes a check for program updates to be performed}
    fAboutBoxAction: TBasicAction;
      {Action that causes About box to be displayed}
  protected // do not make strict
    { INotifier }
    procedure UpdateDbase;
      {Updates database.
      }
    procedure DisplaySnippet(const SnippetName: WideString;
      UserDefined: WordBool; NewTab: WordBool);
      {Displays a named snippey.
        @param SnippetName [in] Name of snippet to display.
        @param UserDefined [in] Whether snippet is user defined.
        @param NewTab [in] Whether to display in new tab in detail pane.
      }
    procedure DisplayCategory(const CatID: WideString; NewTab: WordBool);
      {Displays an identified category.
        @param CatID [in] Id of category to display.
        @param NewTab [in] Whether to display in new tab in detail pane.
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
    procedure NewSnippet;
      {Opens Snippets Editor ready to create a new snippet.
      }
    procedure ShowNews;
      {Shows news items from CodeSnip news feed.
      }
    procedure CheckForUpdates;
      {Checks for program updates.
      }
    procedure ShowAboutBox;
      {Displays the program's About box.
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
    procedure SetNewSnippetAction(const Action: TBasicAction);
      {Sets action triggered when user requests that the Snippets Editor is
      opened ready to create a new snippet.
        @param Action [in] Required action.
      }
    procedure SetNewsAction(const Action: TBasicAction);
      {Sets action triggered when user requests that news items from CodeSnip
      news feed are displayed.
        @param Action [in] Required action.
      }
    procedure SetCheckForUpdatesAction(const Action: TBasicAction);
      {Sets action triggered when user requests a check for program updates.
        @param Action [in] Required action.
      }
    procedure SetAboutBoxAction(const Action: TBasicAction);
      {Sets action triggered when user requests that the About box is displayed.
        @param Action [in] Required action.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, StdActns,
  // Project
  Compilers.UGlobals, UCategoryAction, UDetailTabAction, UEditSnippetAction,
  USnippetAction, UViewItemAction;


{ TNotifier }

procedure TNotifier.ChangeDetailPane(const Pane: Integer);
  {Changes displayed pane in detail display area.
    @param Pane [in] Required new pane.
  }
begin
  if Assigned(fDisplayPaneChangeAction) then
  begin
    (fDisplayPaneChangeAction as TDetailTabAction).TabIndex := Pane;
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

procedure TNotifier.CheckForUpdates;
begin
  if Assigned(fCheckForUpdatesAction) then
    fCheckForUpdatesAction.Execute;
end;

procedure TNotifier.ConfigCompilers;
  {Displays configure compilers dialog box.
  }
begin
  if Assigned(fConfigCompilersAction) then
    fConfigCompilersAction.Execute;
end;

procedure TNotifier.DisplayCategory(const CatID: WideString; NewTab: WordBool);
  {Displays an identified category.
    @param CatID [in] Id of category to display.
    @param NewTab [in] Whether to display in new tab in detail pane.
  }
begin
  if Assigned(fDisplayCategoryAction) then
  begin
    (fDisplayCategoryAction as TCategoryAction).CatID := CatID;
    (fDisplayCategoryAction as TCategoryAction).NewTab := NewTab;
    fDisplayCategoryAction.Execute;
  end;
end;

procedure TNotifier.DisplaySnippet(const SnippetName: WideString;
  UserDefined: WordBool; NewTab: WordBool);
  {Displays a named snippet.
    @param SnippetName [in] Name of snippet to display.
    @param UserDefined [in] Whether snippet is user defined.
    @param NewTab [in] Whether to display in new tab in detail pane.
  }
begin
  if Assigned(fDisplaySnippetAction) then
  begin
    (fDisplaySnippetAction as TSnippetAction).SnippetName := SnippetName;
    (fDisplaySnippetAction as TSnippetAction).UserDefined := UserDefined;
    (fDisplaySnippetAction as TSnippetAction).NewTab := NewTab;
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

procedure TNotifier.NewSnippet;
begin
  if Assigned(fNewSnippetAction) then
    fNewSnippetAction.Execute;
end;

procedure TNotifier.SetAboutBoxAction(const Action: TBasicAction);
begin
  fAboutBoxAction := Action;
end;

procedure TNotifier.SetCheckForUpdatesAction(const Action: TBasicAction);
begin
  fCheckForUpdatesAction := Action;
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
  Assert(Action is TDetailTabAction,
    ClassName + '.SetDetailPaneChangeAction: Action is not TDetailTabAction');
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

procedure TNotifier.SetNewsAction(const Action: TBasicAction);
begin
  fNewsAction := Action;
end;

procedure TNotifier.SetNewSnippetAction(const Action: TBasicAction);
begin
  fNewSnippetAction := Action;
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

procedure TNotifier.ShowAboutBox;
begin
  if Assigned(fAboutBoxAction) then
    fAboutBoxAction.Execute;
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

procedure TNotifier.ShowNews;
begin
  if Assigned(fNewsAction) then
    fNewsAction.Execute;
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

