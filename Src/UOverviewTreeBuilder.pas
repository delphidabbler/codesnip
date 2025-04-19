{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2025, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a set of classes that populate the overview treeview with a list
 * of snippets. Each class groups the snippets in different ways.
 *
 * ACKNOWLEDGEMENT: ViewStore property and its use implemented by @SirRufo
 * (GitHub PR #160 & Issue #158).
}


unit UOverviewTreeBuilder;


interface


uses
  // Delphu
  Generics.Collections,
  ComCtrls,
  // Project
  DB.USnippet, UGroups, UView, UViewItemTreeNode;


type
  {
  TOverviewTreeBuilder:
    Abstract base class for classes that populate the overview treeview with a
    list of snippets. Each subclass groups the snippets in different ways.
  }
  TOverviewTreeBuilder = class abstract(TObject)
  strict private
    var
      // Property values
      fTreeView: TTreeView;
      fSnippetList: TSnippetList;
      fViewStore: TList<IView>;
  strict protected
    property TreeView: TTreeView read fTreeView;
      {Reference to treeview populated by class}
    property SnippetList: TSnippetList read fSnippetList;
      {List of snippets to be displayed in treeview}
    ///  <summary>List of <c>IView</c> instances referenced by treeview nodes.
    ///  </summary>
    ///  <remarks>This list is required to maintain reference counting of
    ///  <c>IView</c>s because the tree nodes only store weak references.
    ///  </remarks>
    property ViewStore : TList<IView> read fViewStore;
      {List of IView instances referenced (weakly) by treeview nodes. This list
      maintains maintains reference counting}
    function AddViewItemNode(const ParentNode: TViewItemTreeNode;
      ViewItem: IView): TViewItemTreeNode;
      {Adds a new node to the tree view that represents a view item.
        @param ParentNode [in] Node that is parent of new node.
        @param ViewItem [in] View item for which we are adding node.
        @return New tree node.
      }
    function CreateGrouping: TGrouping; virtual; abstract;
      {Creates a grouping object of the required type.
        @return Required grouping object.
      }
    function CreateViewItemForGroup(const Group: TGroupItem): IView;
      virtual; abstract;
      {Creates a view item of a type that matches the type of a group item.
        @param Group [in] Group item for which view item is required.
        @return Required view item object.
      }
  public
    ///  <summary>Constructs an object to populate a tree view with a list of
    ///  snippets.</summary>
    ///  <param name="TV"><c>TTreeView</c> [in] Treeview control to be
    ///  populated.</param>
    ///  <param name="SnippetList"><c>TSnippetList</c> [in] List of snippets to
    ///  be added to the treeview.</param>
    ///  <param name="ViewStore"><c>TList&lt;IView&gt;</c> [in] Receives a list
    ///  of view items, one per tree node.</param>
    constructor Create(const TV: TTreeView; const SnippetList: TSnippetList;
      const ViewStore: TList<IView>);
    procedure Build;
      {Populates the treeview.
      }
  end;

  {
  TOverviewTreeBuilderClass:
    Class reference for TOverviewTreeBuilder sub-classes.
  }
  TOverviewTreeBuilderClass = class of TOverviewTreeBuilder;

  {
  TOverviewCategorisedTreeBuilder:
    Class that populates the overview treeview with a list of snippets grouped
    by category.
  }
  TOverviewCategorisedTreeBuilder = class sealed(TOverviewTreeBuilder)
  strict protected
    function CreateGrouping: TGrouping; override;
      {Creates a categorised grouping object.
        @return Required grouping object.
      }
    function CreateViewItemForGroup(const Group: TGroupItem): IView;
      override;
      {Creates a category view item from group item.
        @param Group [in] Group item containing category data.
        @return Required category view item for group.
      }
  end;

  {
  TOverviewAlphabeticTreeBuilder:
    Class that populates the overview treeview with a list of snippets grouped
    by initial letter of the snippet name.
  }
  TOverviewAlphabeticTreeBuilder = class sealed(TOverviewTreeBuilder)
  strict protected
    function CreateGrouping: TGrouping; override;
      {Creates an alphabetic grouping object.
        @return Required grouping object.
      }
    function CreateViewItemForGroup(const Group: TGroupItem): IView;
      override;
      {Creates an alpha view item from group item.
        @param Group [in] Group item containing alpha data.
        @return Required alpha view item for group.
      }
  end;

  {
  TOverviewSnipKindTreeBuilder:
    Class that populates the overview treeview with a list of snippets grouped
    by snippet kind.
  }
  TOverviewSnipKindTreeBuilder = class sealed(TOverviewTreeBuilder)
  strict protected
    function CreateGrouping: TGrouping; override;
      {Creates a snippet kind grouping object.
        @return Required grouping object.
      }
    function CreateViewItemForGroup(const Group: TGroupItem): IView;
      override;
      {Creates a snippet kind view item from group item.
        @param Group [in] Group item containing snippet kind data.
        @return Required snippet kind view item for group.
      }
  end;


implementation


uses
  // Project
  UPreferences;


{
  NOTE:

  Early version of this code, that were contained in the FrOverview unit used
  to add a section head tree node for every category and then delete empty ones.
  However, attempts to delete the first node from a tree causes an endless loop,
  freezing the program. This could be a bug in the Delphi treeview component.

  Therefore the code in this unit now builds or uses a list of snippets for
  each section and does not create a section header node for sections that
  contain no snippets.
}


{ TOverviewTreeBuilder }

function TOverviewTreeBuilder.AddViewItemNode(
  const ParentNode: TViewItemTreeNode; ViewItem: IView): TViewItemTreeNode;
  {Adds a new node to the tree view that represents a view item.
    @param ParentNode [in] Node that is parent of new node.
    @param ViewItem [in] View item for which we are adding node.
    @return New tree node.
  }
begin
  Result := TreeView.Items.AddChild(ParentNode, ViewItem.Description)
    as TViewItemTreeNode;
  Result.ViewItem := ViewItem;
end;

procedure TOverviewTreeBuilder.Build;
  {Populates the treeview.
  }
var
  Snippet: TSnippet;              // each snippet in a list
  ParentNode: TViewItemTreeNode;  // each section node in tree
  Grouping: TGrouping;            // groups snippets
  Group: TGroupItem;              // each group of snippets
  View: IView;
begin
  ViewStore.Clear;
  // Create required grouping of snippets
  Grouping := CreateGrouping;
  try
    // Create tree
    for Group in Grouping do
    begin
      if not Group.IsEmpty or Preferences.ShowEmptySections then
      begin
        View := CreateViewItemForGroup(Group);
        ParentNode := AddViewItemNode(nil, View);
        ViewStore.Add(View);
        for Snippet in Group.SnippetList do
        begin
          View := TViewFactory.CreateSnippetView(Snippet);
          AddViewItemNode(
            ParentNode, View
          );
          ViewStore.Add(View);
        end;
      end;
    end;
  finally
    Grouping.Free;
  end;
end;

constructor TOverviewTreeBuilder.Create(const TV: TTreeView;
  const SnippetList: TSnippetList; const ViewStore: TList<IView>);
begin
  inherited Create;
  fTreeView := TV;
  fSnippetList := SnippetList;
  fViewStore := ViewStore;
end;

{ TOverviewCategorisedTreeBuilder }

function TOverviewCategorisedTreeBuilder.CreateGrouping: TGrouping;
  {Creates a categorised grouping object.
    @return Required grouping object.
  }
begin
  Result := TCategoryGrouping.Create(SnippetList);
end;

function TOverviewCategorisedTreeBuilder.CreateViewItemForGroup(
  const Group: TGroupItem): IView;
  {Creates a category view item from group item.
    @param Group [in] Group item containing category data.
    @return Required category view item for group.
  }
begin
  Result := TViewFactory.CreateCategoryView(
    (Group as TCategoryGroupItem).Category
  );
end;

{ TOverviewAlphabeticTreeBuilder }

function TOverviewAlphabeticTreeBuilder.CreateGrouping: TGrouping;
  {Creates an alphabetic grouping object.
    @return Required grouping object.
  }
begin
  Result := TAlphaGrouping.Create(SnippetList);
end;

function TOverviewAlphabeticTreeBuilder.CreateViewItemForGroup(
  const Group: TGroupItem): IView;
  {Creates an alpha view item from group item.
    @param Group [in] Group item containing alpha data.
    @return Required alpha view item for group.
  }
begin
  Result := TViewFactory.CreateInitialLetterView(
    (Group as TAlphaGroupItem).Letter
  );
end;

{ TOverviewSnipKindTreeBuilder }

function TOverviewSnipKindTreeBuilder.CreateGrouping: TGrouping;
  {Creates a snippet kind grouping object.
    @return Required grouping object.
  }
begin
  Result := TSnipKindGrouping.Create(SnippetList);
end;

function TOverviewSnipKindTreeBuilder.CreateViewItemForGroup(
  const Group: TGroupItem): IView;
  {Creates a snippet kind view item from group item.
    @param Group [in] Group item containing snippet kind data.
    @return Required snippet kind view item for group.
  }
begin
  Result := TViewFactory.CreateSnippetKindView(
    (Group as TSnipKindGroupItem).SnipKindInfo
  );
end;

end.

