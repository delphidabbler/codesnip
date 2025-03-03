{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Object that notifies the main application of certain user-initiated events.
}


unit UNotifier;


interface


uses
  // Delphi
  Classes, ActiveX,
  // Project
  DB.UCollections,
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
      fOverviewStyleChangeActions: array of TBasicAction;
      ///  <summary>List of actions triggered when current pane in detail view
      ///  changes.</summary>
      fDisplayPaneChangeAction: TBasicAction;
      ///  <summary>Action that causes a user defined snippet to be
      ///  edited.</summary>
      fEditSnippetAction: TBasicAction;
      ///  <summary>Action that causes a category to be displayed.</summary>
      fDisplayCategoryAction: TBasicAction;
      ///  <summary>Action that causes news items from CodeSnip news feed to be
      ///  displayed.</summary>
      fNewsAction: TBasicAction;
      ///  <summary>Action that causes About box to be displayed.</summary>
      fAboutBoxAction: TBasicAction;

  public

    ///  <summary>Displays a snippet.</summary>
    ///  <param name="Key">WideString [in] Required snippet's key.
    ///  </param>
    ///  <param name="ACollectionID"><c>TVaultID</c> [in] ID of the snippet's
    ///  vault.</param>
    ///  <param name="NewTab">WordBool [in] Whether to display snippet in a new
    ///  detail pane tab.</param>
    procedure DisplaySnippet(const Key: WideString;
      ACollectionID: TVaultID; NewTab: WordBool);

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
    ///  <param name="Key">WideString [in] Snippet's key.</param>
    ///  <param name="ACollectionID"><c>TVaultID</c> [in] ID of the snippet's
    ///  vault.</param>
    ///  <remarks>Method of INotifier.</remarks>
    procedure EditSnippet(const Key: WideString;
      const ACollectionID: TVaultID);

    ///  <summary>Displays news items from the CodeSnip news feed.</summary>
    ///  <remarks>Methods of INotifier.</remarks>
    procedure ShowNews;

    ///  <summary>Displays the program's About Box.</summary>
    ///  <remarks>Methods of INotifier.</remarks>
    procedure ShowAboutBox;

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

    ///  <summary>Sets action used to display a category.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    ///  <remarks>Methods of ISetActions.</remarks>
    procedure SetDisplayCategoryAction(const Action: TBasicAction);

    ///  <summary>Sets action used to display news items from the CodeSnip news
    ///  feed.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    ///  <remarks>Methods of ISetActions.</remarks>
    procedure SetNewsAction(const Action: TBasicAction);

    ///  <summary>Sets action used to display the program's About Box.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    ///  <remarks>Methods of ISetActions.</remarks>
    procedure SetAboutBoxAction(const Action: TBasicAction);

  end;


implementation


uses
  // Delphi
  SysUtils, StdActns,
  // Project
  Compilers.UGlobals,
  UCategoryAction,
  UDetailTabAction,
  UEditSnippetAction,
  USnippetAction,
  USnippetIDs,
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

procedure TNotifier.DisplaySnippet(const Key: WideString;
  ACollectionID: TVaultID; NewTab: WordBool);
begin
  if Assigned(fDisplaySnippetAction) then
  begin
    (fDisplaySnippetAction as TSnippetAction).Key := Key;
    (fDisplaySnippetAction as TSnippetAction).CollectionID := ACollectionID;
    (fDisplaySnippetAction as TSnippetAction).NewTab := NewTab;
    fDisplaySnippetAction.Execute;
  end;
end;

procedure TNotifier.EditSnippet(const Key: WideString;
  const ACollectionID: TVaultID);
begin
  if Assigned(fEditSnippetAction) then
  begin
    (fEditSnippetAction as TEditSnippetAction).ID := TSnippetID.Create(
      Key, ACollectionID
    );
    fEditSnippetAction.Execute;
  end;
end;

procedure TNotifier.SetAboutBoxAction(const Action: TBasicAction);
begin
  fAboutBoxAction := Action;
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

procedure TNotifier.SetOverviewStyleChangeActions(
  const Actions: array of TBasicAction);
var
  Idx: Integer; // loops thru actions
begin
  SetLength(fOverviewStyleChangeActions, Length(Actions));
  for Idx := Low(Actions) to High(Actions) do
    fOverviewStyleChangeActions[Idx] := Actions[Idx];
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

procedure TNotifier.ShowNews;
begin
  if Assigned(fNewsAction) then
    fNewsAction.Execute;
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

