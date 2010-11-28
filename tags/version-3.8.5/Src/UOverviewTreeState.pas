{
 * UOverviewTreeState.pas
 *
 * Implements a class that maintains the expansion state of a set of section
 * nodes from the overview tree view. The class can record the current state and
 * restore it later.
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
 * The Original Code is UOverviewTreeState.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UOverviewTreeState;


interface


uses
  // Delphi
  ComCtrls, Generics.Collections,
  // Project
  UView;


type

  {
  TOverviewTreeSectionState:
    Class that records the state of an overview pane section node.
  }
  TOverviewTreeSectionState = class(TObject)
  strict private
    var
      fExpanded: Boolean;     // Value of Expanded property
      fViewItem: TViewItem;   // Value of ViewItem property
  public
    constructor Create(const ViewItem: TViewItem; const Expanded: Boolean);
      {Constructor. Sets up object.
        @param ViewItem [in] View item represented by section node.
        @param Expended [in] True if node is expanded, False if not.
      }
    destructor Destroy; override;
      {Destructor. Tears down object.
      }
    property Expanded: Boolean read fExpanded;
      {Whether section node is expanded}
    property ViewItem: TViewItem read fViewItem;
      {View item displayed by section node}
  end;

  {
  TOverviewTreeState:
    Class that maintains the expansion state of a set of section nodes from the
    overview tree view. The class can record the current state and restore it
    later.
  }
  TOverviewTreeState = class(TObject)
  strict private
    type
      // Enumeration indicating required state of tree at next restoration
      TResorationState = (
        rsFullExpand,    // tree must be fully expanded when restored
        rsFullCollapse,  // tree must be fully collapsed when restored
        rsAsSaved        // tree must be restored according to saved state
      );
      // Class that maintains a list of section state objects
      TSectionStates = TObjectList<TOverviewTreeSectionState>;
    var
      fTV: TTreeView;                   // Reference to treeview
      fSections: TSectionStates;        // List of section state objects
      fRestoreState: TResorationState;  // Current restoration state of tree
    function FindSection(const ViewItem: TViewItem;
      out FoundSection: TOverviewTreeSectionState): Boolean;
      {Finds a section object that references a specified view item.
        @param ViewItem [in] View item being searched for.
        @param FoundSection [in] Section object or nil if view item not found.
        @return True if view item found, False if not.
      }
    function IsSectionNode(const Node: TTreeNode): Boolean;
      {Checks if a node is a section node.
        @param Node [in] Node to be checked.
        @return True if node is a section node.
      }
  public
    constructor Create(const TV: TTreeView);
      {Constructor. Sets up object to work with a treeview.
        @param TV [in] Treeview whose state is to be managed.
      }
    destructor Destroy; override;
      {Destructor. Tears down object.
      }
    procedure SaveState;
      {Saves expansion state of all section nodes in treeview.
      }
    procedure RestoreState;
      {Restores state of all section nodes in overview. How tree is restored
      depends on required restoration state and if a previous state has been
      recorded.
      }
  end;


implementation


uses
  // Project
  UPreferences, UViewItemTreeNode;


{ TOverviewTreeSectionState }

constructor TOverviewTreeSectionState.Create(const ViewItem: TViewItem;
  const Expanded: Boolean);
  {Constructor. Sets up object.
    @param ViewItem [in] View item represented by section node.
    @param Expended [in] True if node is expanded, False if not.
  }
begin
  inherited Create;
  fViewItem := TViewItem.Create(ViewItem);
  fExpanded := Expanded;
end;

destructor TOverviewTreeSectionState.Destroy;
  {Destructor. Tears down object.
  }
begin
  fViewItem.Free;
  inherited;
end;

{ TOverviewTreeState }

constructor TOverviewTreeState.Create(const TV: TTreeView);
  {Constructor. Sets up object to work with a treeview.
    @param TV [in] Treeview whose state is to be managed.
  }
begin
  inherited Create;
  fSections := TSectionStates.Create(True);
  fTV := TV;
  case Preferences.OverviewStartState of
    ossExpanded: fRestoreState := rsFullExpand;
    ossCollapsed: fRestoreState := rsFullCollapse;
  end;
end;

destructor TOverviewTreeState.Destroy;
  {Destructor. Tears down object.
  }
begin
  fSections.Free;
  inherited;
end;

function TOverviewTreeState.FindSection(const ViewItem: TViewItem;
  out FoundSection: TOverviewTreeSectionState): Boolean;
  {Finds a section object that references a specified view item.
    @param ViewItem [in] View item being searched for.
    @param FoundSection [in] Section object or nil if view item not found.
    @return True if view item found, False if not.
  }
var
  Section: TOverviewTreeSectionState;  // each section state in list
begin
  Result := False;
  FoundSection := nil;
  for Section in fSections do
  begin
    if Section.ViewItem.IsEqual(ViewItem) then
    begin
      FoundSection := Section;
      Exit(True);
    end;
  end;
end;

function TOverviewTreeState.IsSectionNode(const Node: TTreeNode): Boolean;
  {Checks if a node is a section node.
    @param Node [in] Node to be checked.
    @return True if node is a section node.
  }
begin
  // A section node is a view item tree node at level 0
  Result := (Node.Level = 0) and (Node is TViewItemTreeNode);
end;

procedure TOverviewTreeState.RestoreState;
  {Restores state of all section nodes in overview. How tree is restored depends
  on required restoration state and if a previous state has been recorded.
  }
var
  Node: TTreeNode;                    // each node in treeview
  Section: TOverviewTreeSectionState; // object describing a section's state
begin
  case fRestoreState of
    rsFullExpand: fTV.FullExpand;
    rsFullCollapse: fTV.FullCollapse;
    rsAsSaved:
    begin
      for Node in fTV.Items do
      begin
        if IsSectionNode(Node)
          and FindSection((Node as TViewItemTreeNode).ViewItem, Section) then
          Node.Expanded := Section.Expanded;
      end;
    end;
  end;
end;

procedure TOverviewTreeState.SaveState;
  {Saves expansion state of all section nodes in treeview.
  }

  // ---------------------------------------------------------------------------
  procedure RecordSection(const Node: TViewItemTreeNode);
    {Records reference to a treeview section whose expansion state is to be
    maintained by the object.
      @param Node [in] Treeview node whose state is to be recorded.
    }
  begin
    fSections.Add(
      TOverviewTreeSectionState.Create(Node.ViewItem, Node.Expanded)
    );
  end;
  // ---------------------------------------------------------------------------

var
  Node: TTreeNode;  // each node in treeview
begin
  fSections.Clear;
  for Node in fTV.Items do
  begin
    if IsSectionNode(Node) then
      RecordSection(Node as TViewItemTreeNode);
  end;
  // note that tree must be restored from this saved state
  fRestoreState := rsAsSaved;
end;

end.

