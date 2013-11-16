{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2013, Peter Johnson (www.delphidabbler.com).
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
  Classes,
  ActiveX,
  // Project
  IntfNotifier,
  USnippetIDs,
  UView;


type
  ///  <summary>Notifies the main application code of certain user-initiated
  ///  events.</summary>
  ///  <remarks>
  ///  <para>Objects that receive user input call methods of this object's
  ///  INotifier interface which in turn trigger actions that have been
  ///  registered via its ISetAction interface.</para>
  ///  <para>In effect this class is a central clearing house for user
  ///  interaction with certain controls, including the browser control.</para>
  ///  </remarks>
  TNotifier = class(TInterfacedObject, INotifier, ISetActions)
  strict private
    var
      ///  <summary>Action that triggers a database update.</summary>
      fUpdateDbaseAction: TBasicAction;
      ///  <summary>Action that causes a named snippet to be displayed.
      ///  </summary>
      fDisplaySnippetAction: TBasicAction;
      ///  <summary>Action that displays configure compiler dialogue box.
      ///  </summary>
      fConfigCompilersAction: TBasicAction;
      ///  <summary>Action that causes a view item to be displayed.</summary>
      fShowViewItemAction: TBasicAction;
      ///  <summary>List of actions triggered when display style in overview
      ///  pane changes.</summary>
      fOverviewStyleChangeActions: TArray<TBasicAction>;
      ///  <summary>List of actions triggered when current pane in detail view
      ///  changes.</summary>
      fDisplayPaneChangeAction: TBasicAction;
      ///  <summary>Action that causes a user defined snippet to be
      ///  edited.</summary>
      fEditSnippetAction: TBasicAction;
      ///  <summary>Action that displays donate dialogue box.</summary>
      fDonateAction: TBasicAction;
      ///  <summary>Action that causes a category to be displayed.</summary>
      fDisplayCategoryAction: TBasicAction;
      ///  <summary>Action that causes the Snippets Editor to be opened ready to
      ///  create a new snippet.</summary>
      fNewSnippetAction: TBasicAction;
      ///  <summary>Action that causes news items from CodeSnip news feed to be
      ///  displayed.</summary>
      fNewsAction: TBasicAction;
      ///  <summary>Action that causes a check for program updates to be
      ///  performed.</summary>
      fCheckForUpdatesAction: TBasicAction;
      ///  <summary>Action that causes About box to be displayed.</summary>
      fAboutBoxAction: TBasicAction;
      ///  <summary>Action that displays a specified page in the preferences
      ///  dialogue box.</summary>
      fShowPrefsPageAction: TBasicAction;

  public

    ///  <summary>Requests a database update.</summary>
    ///  <remarks>Methods of INotifier.</remarks>
    procedure UpdateDbase;

    ///  <summary>Displays a snippet.</summary>
    ///  <param name="SnippetID">TSnippetID [in] ID of required snippet.
    ///  </param>
    ///  <param name="NewTab">WordBool [in] Whether to display snippet in a new
    ///  detail pane tab.</param>
    ///  <remarks>Methods of INotifier.</remarks>
    procedure DisplaySnippet(const SnippetID: TSnippetID; NewTab: WordBool);

    ///  <summary>Displays a category.</summary>
    ///  <param name="CatId">WideString [in] ID of required category.</param>
    ///  <param name="NewTab">WordBool [in] Whether to display category in a new
    ///  detail pane tab.</param>
    ///  <remarks>Methods of INotifier.</remarks>
    procedure DisplayCategory(const CatID: WideString; NewTab: WordBool);

    ///  <summary>Displays Configure Compilers dialogue box.</summary>
    ///  <remarks>Methods of INotifier.</remarks>
    procedure ConfigCompilers;

    ///  <summary>Displays a view item.</summary>
    ///  <param name="View">IView [in] Required view item.</param>
    ///  <param name="NewTab">Boolean [in] Whether to display view item in a new
    ///  detail pane tab.</param>
    ///  <remarks>Methods of INotifier.</remarks>
    procedure ShowViewItem(ViewItem: IView; const NewTab: Boolean);

    ///  <summary>Changes display style of overview pane.</summary>
    ///  <param name="Style">Integer [in] Required display style.</param>
    ///  <remarks>
    ///  <para>Style is index of an overview pane tab.</para>
    ///  <para>Methods of INotifier.</para>
    ///  </remarks>
    procedure ChangeOverviewStyle(const Style: Integer);

    ///  <summary>Changes displayed pane in detail display area.</summary>
    ///  <param name="Pane">Integer [in] Index of required pane.</param>
    ///  <remarks>Methods of INotifier.</remarks>
    procedure ChangeDetailPane(const Pane: Integer);

    ///  <summary>Edits a snippet in Snippets Editor.</summary>
    ///  <param name="SnippetID">TSnippetID [in] ID of snippet.</param>
    ///  <remarks>
    ///  <para>Snippet must be user defined.</para>
    ///  <para>Methods of INotifier.</para>
    ///  </remarks>
    procedure EditSnippet(const SnippetID: TSnippetID);

    ///  <summary>Displays Donate dialogue box.</summary>
    ///  <remarks>Methods of INotifier.</remarks>
    procedure Donate;

    ///  <summary>Opens Snippets Editor ready to create a new snippet.</summary>
    ///  <remarks>Methods of INotifier.</remarks>
    procedure NewSnippet;

    ///  <summary>Displays news items from the CodeSnip news feed.</summary>
    ///  <remarks>Methods of INotifier.</remarks>
    procedure ShowNews;

    ///  <summary>Checks for program updates.</summary>
    ///  <remarks>Methods of INotifier.</remarks>
    procedure CheckForUpdates;

    ///  <summary>Displays the program's About Box.</summary>
    ///  <remarks>Methods of INotifier.</remarks>
    procedure ShowAboutBox;

    ///  <summary>Displays the Preferences dialogue box containing the specified
    ///  page.</summary>
    ///  <param name="ClsName">string [in] Class name of the frame that
    ///  implements the required preferences page.</param>
    ///  <remarks>Method of INotifier.</remarks>
    procedure ShowPrefsPage(const ClsName: string);

    ///  <summary>Sets action used to request a database update.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    ///  <remarks>Methods of ISetActions.</remarks>
    procedure SetUpdateDbaseAction(const Action: TBasicAction);

    ///  <summary>Sets action used to display a snippet.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    ///  <remarks>Methods of ISetActions.</remarks>
    procedure SetDisplaySnippetAction(const Action: TBasicAction);

    ///  <summary>Sets action used to display Configure Compilers dialogue
    ///  box.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    ///  <remarks>Methods of ISetActions.</remarks>
    procedure SetConfigCompilersAction(const Action: TBasicAction);

    ///  <summary>Sets action used to display a view item.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    ///  <remarks>Methods of ISetActions.</remarks>
    procedure SetShowViewItemAction(const Action: TBasicAction);

    ///  <summary>Sets actions used to change display style of overview pane.
    ///  </summary>
    ///  <param name="Actions">array of TBasicAction [in] Array of required
    ///  actions.</param>
    ///  <remarks>
    ///  <para>Actions array must have one action for each supported display
    ///  style.</para>
    ///  <para>Methods of ISetActions.</para>
    ///  </remarks>
    procedure SetOverviewStyleChangeActions(
      const Actions: array of TBasicAction);

    ///  <summary>Sets action used to change displayed pane in detail display
    ///  area.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    ///  <remarks>Methods of ISetActions.</remarks>
    procedure SetDetailPaneChangeAction(const Action: TBasicAction);

    ///  <summary>Sets action used to edit a snippet in Snippets Editor.
    ///  </summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    ///  <remarks>Methods of ISetActions.</remarks>
    procedure SetEditSnippetAction(const Action: TBasicAction);

    ///  <summary>Sets action used to display Donate dialogue box.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    ///  <remarks>Methods of ISetActions.</remarks>
    procedure SetDonateAction(const Action: TBasicAction);

    ///  <summary>Sets action used to display a category.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    ///  <remarks>Methods of ISetActions.</remarks>
    procedure SetDisplayCategoryAction(const Action: TBasicAction);

    ///  <summary>Sets action used to open snippets editor to create a new
    ///  snippet.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    ///  <remarks>Methods of ISetActions.</remarks>
    procedure SetNewSnippetAction(const Action: TBasicAction);

    ///  <summary>Sets action used to display news items from the CodeSnip news
    ///  feed.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    ///  <remarks>Methods of ISetActions.</remarks>
    procedure SetNewsAction(const Action: TBasicAction);

    ///  <summary>Sets action used to check for program updates.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    ///  <remarks>Methods of ISetActions.</remarks>
    procedure SetCheckForUpdatesAction(const Action: TBasicAction);

    ///  <summary>Sets action used to display the program's About Box.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    ///  <remarks>Methods of ISetActions.</remarks>
    procedure SetAboutBoxAction(const Action: TBasicAction);

    ///  <summary>Sets action used to display a given page of the Preferences
    ///  dialogue box.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    ///  <remarks>Method of ISetActions.</remarks>
    procedure SetShowPrefsPageAction(const Action: TBasicAction);
  end;


implementation


uses
  // Delphi
  SysUtils, StdActns,
  // Project
  Compilers.UGlobals, UCategoryAction, UContainers, UDetailTabAction,
  UEditSnippetAction, UShowPrefsPageAction, USnippetAction, UViewItemAction;


{ TNotifier }

procedure TNotifier.ChangeDetailPane(const Pane: Integer);
begin
  if Assigned(fDisplayPaneChangeAction) then
  begin
    (fDisplayPaneChangeAction as TDetailTabAction).TabIndex := Pane;
    fDisplayPaneChangeAction.Execute;
  end;
end;

procedure TNotifier.ChangeOverviewStyle(const Style: Integer);
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
begin
  if Assigned(fConfigCompilersAction) then
    fConfigCompilersAction.Execute;
end;

procedure TNotifier.DisplayCategory(const CatID: WideString; NewTab: WordBool);
begin
  if Assigned(fDisplayCategoryAction) then
  begin
    (fDisplayCategoryAction as TCategoryAction).CatID := CatID;
    (fDisplayCategoryAction as TCategoryAction).NewTab := NewTab;
    fDisplayCategoryAction.Execute;
  end;
end;

procedure TNotifier.DisplaySnippet(const SnippetID: TSnippetID;
  NewTab: WordBool);
begin
  if Assigned(fDisplaySnippetAction) then
  begin
    (fDisplaySnippetAction as TSnippetAction).SnippetID := SnippetID;
    (fDisplaySnippetAction as TSnippetAction).NewTab := NewTab;
    fDisplaySnippetAction.Execute;
  end;
end;

procedure TNotifier.Donate;
begin
  if Assigned(fDonateAction) then
    fDonateAction.Execute;
end;

procedure TNotifier.EditSnippet(const SnippetID: TSnippetID);
begin
  if Assigned(fEditSnippetAction) then
  begin
    (fEditSnippetAction as TEditSnippetAction).SnippetID := SnippetID;
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
begin
  Assert(Action is TSnippetAction,
    ClassName + '.SetDisplaySnippetAction: Action is not TSnippetAction');
  Assert(Supports(Action, ISetNotifier),
    ClassName + '.SetDisplaySnippetAction: Action must support ISetNotifier');
  fDisplaySnippetAction := Action;
  (fDisplaySnippetAction as ISetNotifier).SetNotifier(Self);
end;

procedure TNotifier.SetDonateAction(const Action: TBasicAction);
begin
  fDonateAction := Action;
end;

procedure TNotifier.SetEditSnippetAction(const Action: TBasicAction);
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
begin
  fOverviewStyleChangeActions := TArrayHelper.Copy<TBasicAction>(Actions);
end;

procedure TNotifier.SetShowPrefsPageAction(const Action: TBasicAction);
begin
  Assert(Action is TShowPrefsPageAction, ClassName
    + '.SetShowPreferencesAction: Action is not TShowPrefsPageAction');
  fShowPrefsPageAction := Action;
end;

procedure TNotifier.SetShowViewItemAction(const Action: TBasicAction);
begin
  fShowViewItemAction := Action;
end;

procedure TNotifier.SetUpdateDbaseAction(
  const Action: TBasicAction);
begin
  fUpdateDbaseAction := Action;
end;

procedure TNotifier.ShowAboutBox;
begin
  if Assigned(fAboutBoxAction) then
    fAboutBoxAction.Execute;
end;

procedure TNotifier.ShowNews;
begin
  if Assigned(fNewsAction) then
    fNewsAction.Execute;
end;

procedure TNotifier.ShowPrefsPage(const ClsName: string);
begin
  if Assigned(fShowPrefsPageAction) then
  begin
    (fShowPrefsPageAction as TShowPrefsPageAction).FrameClassName := ClsName;
    fShowPrefsPageAction.Execute;
  end;
end;

procedure TNotifier.ShowViewItem(ViewItem: IView; const NewTab: Boolean);
begin
  if Assigned(fShowViewItemAction) then
  begin
    (fShowViewItemAction as TViewItemAction).ViewItem := ViewItem;
    (fShowViewItemAction as TViewItemAction).NewTab := NewTab;
    fShowViewItemAction.Execute;
  end;
end;

procedure TNotifier.UpdateDbase;
begin
  if Assigned(fUpdateDbaseAction) then
    fUpdateDbaseAction.Execute;
end;

end.

