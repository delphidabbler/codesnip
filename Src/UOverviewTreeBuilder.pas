{
 * UOverviewTreeBuilder.pas
 *
 * Implements a set of classes that populate the overview treeview with a list
 * of snippets. Each class groups the snippets in different ways.
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
 * The Original Code is UOverviewTreeBuilder.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UOverviewTreeBuilder;


interface


uses
  // Delphu
  ComCtrls,
  // Project
  UGroups, USnippets, UView, UViewItemTreeNode;


type
  {
  TOverviewTreeBuilder:
    Abstract base class for classes that populate the overview treeview with a
    list of snippets. Each subclass groups the snippets in different ways.
  }
  TOverviewTreeBuilder = class abstract(TObject)
  strict private
    var
      fTreeView: TTreeView;       // Value of TreeView property
      fSnippetList: TRoutineList; // Value of SnippetList property
  strict protected
    property TreeView: TTreeView read fTreeView;
      {Reference to treeview populated by class}
    property SnippetList: TRoutineList read fSnippetList;
      {List of snippets to be displayed in treeview}
    function AddViewItemNode(const ParentNode: TViewItemTreeNode;
      const ViewItem: TViewItem): TViewItemTreeNode;
      {Adds a new node to the tree view that represents a view item.
        @param ParentNode [in] Node that is parent of new node.
        @param ViewItem [in] View item for which we are adding node.
        @return New tree node.
      }
    function CreateGrouping: TGrouping; virtual; abstract;
      {Creates a grouping object of the required type.
        @return Required grouping object.
      }
    function CreateViewItemForGroup(const Group: TGroupItem): TViewItem;
      virtual; abstract;
      {Creates a view item of a type that matches the type of a group item.
        @param Group [in] Group item for which view item is required.
        @return Required view item object.
      }
  public
    constructor Create(const TV: TTreeView; const SnippetList: TRoutineList);
      {Class constructor. Sets up object to populate a treeview with a list of
      snippets.
        @param TV [in] Treeview control to be populated.
        @param SnippetList [in] List of snippets to be added to TV.
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
    function CreateViewItemForGroup(const Group: TGroupItem): TViewItem;
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
    function CreateViewItemForGroup(const Group: TGroupItem): TViewItem;
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
    function CreateViewItemForGroup(const Group: TGroupItem): TViewItem;
      override;
      {Creates a snippet kind view item from group item.
        @param Group [in] Group item containing snippet kind data.
        @return Required snippet kind view item for group.
      }
  end;


implementation


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


uses
  // Delphi
  SysUtils;


{ TOverviewTreeBuilder }

function TOverviewTreeBuilder.AddViewItemNode(
  const ParentNode: TViewItemTreeNode;
  const ViewItem: TViewItem): TViewItemTreeNode;
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
  Snippet: TRoutine;              // each snippet in a list
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
      if not Group.IsEmpty then
      begin
        ParentNode := AddViewItemNode(nil, CreateViewItemForGroup(Group));
        for Snippet in Group.SnippetList do
          AddViewItemNode(ParentNode, TViewItem.Create(Snippet));
      end;
    end;
  finally
    FreeAndNil(Grouping);
  end;
end;

constructor TOverviewTreeBuilder.Create(const TV: TTreeView;
  const SnippetList: TRoutineList);
  {Class constructor. Sets up object to populate a treeview with a list of
  snippets.
    @param TV [in] Treeview control to be populated.
    @param SnippetList [in] List of snippets to be added to TV.
  }
begin
  inherited Create;
  fTreeView := TV;
  fSnippetList := SnippetList;
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
  const Group: TGroupItem): TViewItem;
  {Creates a category view item from group item.
    @param Group [in] Group item containing category data.
    @return Required category view item for group.
  }
begin
  Result := TViewItem.Create((Group as TCategoryGroupItem).Category);
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
  const Group: TGroupItem): TViewItem;
  {Creates an alpha view item from group item.
    @param Group [in] Group item containing alpha data.
    @return Required alpha view item for group.
  }
begin
  Result := TViewItem.Create((Group as TAlphaGroupItem).Letter);
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
  const Group: TGroupItem): TViewItem;
  {Creates a snippet kind view item from group item.
    @param Group [in] Group item containing snippet kind data.
    @return Required snippet kind view item for group.
  }
begin
  Result := TViewItem.Create((Group as TSnipKindGroupItem).SnipKindInfo);
end;

end.

