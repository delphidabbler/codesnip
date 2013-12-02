{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a set of classes that populate the overview treeview with a list
 * of snippets. Each class groups the snippets in different ways.
}


unit UOverviewTreeBuilder;


interface


uses
  // Delphu
  ComCtrls,
  // Project
  CS.Database.Types,
  DB.USnippet,
  UGroups,
  UView,
  UViewItemTreeNode;


type
  {
  TOverviewTreeBuilder:
    Abstract base class for classes that populate the overview treeview with a
    list of snippets. Each subclass groups the snippets in different ways.
  }
  TOverviewTreeBuilder = class abstract(TObject)
  strict private
    var
      fTreeView: TTreeView;           // Value of TreeView property
      fSnippetIDList: ISnippetIDList; // Value of SnippetList property
  strict protected
    property TreeView: TTreeView read fTreeView;
      {Reference to treeview populated by class}
    property SnippetIDList: ISnippetIDList read fSnippetIDList;
      {List of IDs of snippets to be displayed in treeview}
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
    constructor Create(const TV: TTreeView; SnippetIDList: ISnippetIDList);
      {Class constructor. Sets up object to populate a treeview with a list of
      snippets.
        @param TV [in] Treeview control to be populated.
        @param SnippetIDList [in] List of IDs of snippets to be added to TV.
      }
    procedure Build;
      {Populates the treeview.
      }
  end;

  {
  TOverviewTreeBuilderClass:
    Class reference for TOverviewTreeBuilder sub-classes.
  }
  TOverviewTreeBuilderClass = class of TOverviewTreeBuilder;


  ///  <summary>Class that populates the overview treeview with a list of
  ///  snippets grouped by tags.</summary>
  ///  <remarks>Snippets are repeated once per tag.</remarks>
  TOverviewTagTreeBuilder = class sealed(TOverviewTreeBuilder)
  strict protected
    ///  <summary>Creates and returns a tag grouping object.</summary>
    function CreateGrouping: TGrouping; override;
    ///  <summary>Createa and returns a tag view item from a group item.
    ///  </summary>
    function CreateViewItemForGroup(const Group: TGroupItem): IView;
      override;
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
begin
  // Create required grouping of snippets
  Grouping := CreateGrouping;
  try
    // Create tree
    for Group in Grouping do
    begin
      if not Group.IsEmpty or Preferences.ShowEmptySections then
      begin
        ParentNode := AddViewItemNode(nil, CreateViewItemForGroup(Group));
        for Snippet in Group.SnippetList do
          AddViewItemNode(
            ParentNode, TViewFactory.CreateSnippetView(Snippet)
          );
      end;
    end;
  finally
    Grouping.Free;
  end;
end;

constructor TOverviewTreeBuilder.Create(const TV: TTreeView;
  SnippetIDList: ISnippetIDList);
  {Class constructor. Sets up object to populate a treeview with a list of
  snippets.
    @param TV [in] Treeview control to be populated.
    @param SnippetIDList [in] List of IDs of snippets to be added to TV.
  }
begin
  inherited Create;
  fTreeView := TV;
  fSnippetIDList := SnippetIDList;
end;

{ TOverviewTagTreeBuilder }

function TOverviewTagTreeBuilder.CreateGrouping: TGrouping;
begin
  Result := TTagGrouping.Create(SnippetIDList);
end;

function TOverviewTagTreeBuilder.CreateViewItemForGroup(
  const Group: TGroupItem): IView;
begin
  Result := TViewFactory.CreateTagView(
    (Group as TTagGroupItem).Tag
  );
end;

{ TOverviewAlphabeticTreeBuilder }

function TOverviewAlphabeticTreeBuilder.CreateGrouping: TGrouping;
  {Creates an alphabetic grouping object.
    @return Required grouping object.
  }
begin
  Result := TAlphaGrouping.Create(SnippetIDList);
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
  Result := TSnipKindGrouping.Create(SnippetIDList);
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

