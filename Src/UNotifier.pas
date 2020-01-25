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
  CS.SourceCode.Languages,
  CS.Database.Types,
  IntfNotifier,
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
      ///  <summary>Action that causes a snippet to be edited.</summary>
      fEditSnippetAction: TBasicAction;
      ///  <summary>Action that displays donate dialogue box.</summary>
      fDonateAction: TBasicAction; // TODO -cwebsvc: remove this
      ///  <summary>Action that causes the Snippets Editor to be opened ready to
      ///  create a new snippet.</summary>
      fNewSnippetAction: TBasicAction;
      ///  <summary>Action that causes a check for program updates to be
      ///  performed.</summary>
      fCheckForUpdatesAction: TBasicAction; // TODO -cwebsvc: remove this
      ///  <summary>Action that causes About box to be displayed.</summary>
      fAboutBoxAction: TBasicAction;
      ///  <summary>Action that displays a specified page in the preferences
      ///  dialogue box.</summary>
      fShowPrefsPageAction: TBasicAction;
      ///  <summary>Action that causes a specified tag to be displayed.
      ///  </summary>
      fDisplayTagAction: TBasicAction;
      ///  <summary>Action that causes a specified tag to be removed from a
      ///  specified snippet's tag list.</summary>
      fRemoveTagAction: TBasicAction;
      ///  <summary>Action that causes a specified source code language to be
      ///  displayed.</summary>
      fDisplayLanguageAction: TBasicAction;
      ///  <summary>Action that changes the value of a specified snippet's
      ///  Starred property.</summary>
      fChangeSnippetStarAction: TBasicAction;
  public

    ///  <summary>Displays a snippet.</summary>
    ///  <param name="SnippetID">TSnippetID [in] ID of required snippet.
    ///  </param>
    ///  <param name="NewTab">WordBool [in] Whether to display snippet in a new
    ///  detail pane tab.</param>
    ///  <remarks>Methods of INotifier.</remarks>
    procedure DisplaySnippet(const SnippetID: TSnippetID; NewTab: WordBool);

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
    ///  <remarks>Methods of INotifier.</remarks>
    procedure EditSnippet(const SnippetID: TSnippetID);

    ///  <summary>Displays Donate dialogue box.</summary>
    ///  <remarks>Methods of INotifier.</remarks>
    procedure Donate; // TODO -cwebsvc: remove this

    ///  <summary>Opens Snippets Editor ready to create a new snippet.</summary>
    ///  <remarks>Methods of INotifier.</remarks>
    procedure NewSnippet;

    ///  <summary>Checks for program updates.</summary>
    ///  <remarks>Methods of INotifier.</remarks>
    procedure CheckForUpdates;  // TODO -cwebsvc: remove this

    ///  <summary>Displays the program's About Box.</summary>
    ///  <remarks>Methods of INotifier.</remarks>
    procedure ShowAboutBox;

    ///  <summary>Displays the Preferences dialogue box containing the specified
    ///  page.</summary>
    ///  <param name="ClsName">string [in] Class name of the frame that
    ///  implements the required preferences page.</param>
    ///  <remarks>Method of INotifier.</remarks>
    procedure ShowPrefsPage(const ClsName: string);

    ///  <summary>Displays the given tag.</summary>
    ///  <param name="Tag">TTag [in] Tag to be displayed.</param>
    ///  <param name="NewTab">WordBool [in] Whether to display tag in a new tab.
    ///  </param>
    ///  <remarks>Method of INotifier.</remarks>
    procedure DisplayTag(const Tag: TTag; NewTab: WordBool);

    ///  <summary>Removes a tag from a snippet's tag list.<summary>
    ///  <param name="SnippetID">TSnippetID [in] ID of snippet.</param>
    ///  <param name="Tag">TTag [in] Tag to be removed.</param>
    ///  <remarks>Method of INotifier.</remarks>
    procedure RemoveTag(const SnippetID: TSnippetID; const Tag: TTag);

    ///  <summary>Displays the source code language with the given ID.</summary>
    ///  <param name="LangID">TSourceCodeLanguageID [in] ID of language to be
    ///  displayed.</param>
    ///  <param name="NewTab">WordBool [in] Whether to display language in a new
    ///  tab.</param>
    ///  <remarks>Method of INotifier.</remarks>
    procedure DisplayLanguage(const LangID: TSourceCodeLanguageID;
      NewTab: WordBool);

    ///  <summary>Sets the the Starred property of the snippet with the given ID
    ///  to the given State.</summary>
    ///  <remarks>Method of INotifier.</remarks>
    procedure ChangeSnippetStar(const SnippetID: TSnippetID;
      const State: Boolean);

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
    procedure SetDonateAction(const Action: TBasicAction);  // TODO -cwebsvc: remove this

    ///  <summary>Sets action used to open snippets editor to create a new
    ///  snippet.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    ///  <remarks>Methods of ISetActions.</remarks>
    procedure SetNewSnippetAction(const Action: TBasicAction);

    ///  <summary>Sets action used to check for program updates.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    ///  <remarks>Methods of ISetActions.</remarks>
    procedure SetCheckForUpdatesAction(const Action: TBasicAction); // TODO -cwebsvc: remove this

    ///  <summary>Sets action used to display the program's About Box.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    ///  <remarks>Methods of ISetActions.</remarks>
    procedure SetAboutBoxAction(const Action: TBasicAction);

    ///  <summary>Sets action used to display a given page of the Preferences
    ///  dialogue box.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    ///  <remarks>Method of ISetActions.</remarks>
    procedure SetShowPrefsPageAction(const Action: TBasicAction);

    ///  <summary>Sets action used to display a tag.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    ///  <remarks>Method of ISetActions.</remarks>
    procedure SetDisplayTagAction(const Action: TBasicAction);

    ///  <summary>Sets action used to remove a tag from a snippet's tag list.
    ///  </summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    ///  <remarks>Method of ISetActions.</remarks>
    procedure SetRemoveTagAction(const Action: TBasicAction);

    ///  <summary>Sets action used to display a source code language.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    ///  <remarks>Method of ISetActions.</remarks>
    procedure SetDisplayLanguageAction(const Action: TBasicAction);

    ///  <summary>Sets action used to update the Starred property of a snippet.
    ///  </summary>
    ///  <remarks>Method of ISetActions.</remarks>
    procedure SetChangeSnippetStarAction(const Action: TBasicAction);

  end;


implementation


uses
  // Delphi
  SysUtils, StdActns,
  // Project
  CS.Actions.ChangeSnippetStar,
  CS.Actions.DisplayLanguage,
  CS.Actions.DisplayTag,
  CS.Actions.RemoveTag,
  Compilers.UGlobals,
  UContainers,
  UDetailTabAction,
  UEditSnippetAction,
  UShowPrefsPageAction,
  USnippetAction,
  UViewItemAction;


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

procedure TNotifier.ChangeSnippetStar(const SnippetID: TSnippetID;
  const State: Boolean);
begin
  if Assigned(fChangeSnippetStarAction) then
  begin
    (fChangeSnippetStarAction as TChangeSnippetStarAction).SnippetID :=
      SnippetID;
    (fChangeSnippetStarAction as TChangeSnippetStarAction).State := State;
    fChangeSnippetStarAction.Execute;
  end;
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

procedure TNotifier.DisplayLanguage(const LangID: TSourceCodeLanguageID;
  NewTab: WordBool);
begin
  if Assigned(fDisplayLanguageAction) then
  begin
    (fDisplayLanguageAction as TDisplayLanguageAction).LanguageID := LangID;
    (fDisplayLanguageAction as TDisplayLanguageAction).NewTab := NewTab;
    fDisplayLanguageAction.Execute;
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

procedure TNotifier.DisplayTag(const Tag: TTag; NewTab: WordBool);
begin
  if Assigned(fDisplayTagAction) then
  begin
    (fDisplayTagAction as TDisplayTagAction).Tag := Tag;
    (fDisplayTagAction as TDisplayTagAction).NewTab := NewTab;
    fDisplayTagAction.Execute;
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

procedure TNotifier.RemoveTag(const SnippetID: TSnippetID; const Tag: TTag);
begin
  if Assigned(fRemoveTagAction) then
  begin
    (fRemoveTagAction as TRemoveTagAction).SnippetID := SnippetID;
    (fRemoveTagAction as TRemoveTagAction).Tag := Tag;
    fRemoveTagAction.Execute;
  end;
end;

procedure TNotifier.SetAboutBoxAction(const Action: TBasicAction);
begin
  fAboutBoxAction := Action;
end;

procedure TNotifier.SetChangeSnippetStarAction(const Action: TBasicAction);
begin
  Assert(Action is TChangeSnippetStarAction, ClassName +
    '.SetChangeSnippetStarAction: Action is not TChangeSnippetStarAction');
  fChangeSnippetStarAction := Action;
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

procedure TNotifier.SetDisplayLanguageAction(const Action: TBasicAction);
begin
  Assert(Action is TDisplayLanguageAction, ClassName +
    '.SetDisplayLanguageAction: Action is not TDisplayLanguageAction');
  Assert(Supports(Action, ISetNotifier),
    ClassName + '.SetDisplayLanguageAction: Action must support ISetNotifier');
  fDisplayLanguageAction := Action;
  (fDisplayLanguageAction as ISetNotifier).SetNotifier(Self);
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

procedure TNotifier.SetDisplayTagAction(const Action: TBasicAction);
begin
  Assert(Action is TDisplayTagAction,
    ClassName + '.SetDisplayTagAction: Action is not TDisplayTagAction');
  Assert(Supports(Action, ISetNotifier),
    ClassName + '.SetDisplayTagAction: Action must support ISetNotifier');
  fDisplayTagAction := Action;
  (fDisplayTagAction as ISetNotifier).SetNotifier(Self);
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

procedure TNotifier.SetNewSnippetAction(const Action: TBasicAction);
begin
  fNewSnippetAction := Action;
end;

procedure TNotifier.SetOverviewStyleChangeActions(
  const Actions: array of TBasicAction);
begin
  fOverviewStyleChangeActions := TArrayHelper.Copy<TBasicAction>(Actions);
end;

procedure TNotifier.SetRemoveTagAction(const Action: TBasicAction);
begin
  Assert(Action is TRemoveTagAction,
    ClassName + '.SetRemoveTagAction: Action is not TRemoveTagAction');
  fRemoveTagAction := Action;
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

procedure TNotifier.ShowAboutBox;
begin
  if Assigned(fAboutBoxAction) then
    fAboutBoxAction.Execute;
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

end.

