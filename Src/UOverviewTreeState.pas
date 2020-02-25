{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that maintains the expansion state of a set of section
 * nodes from the overview tree view. The class can record the current state and
 * restore it later.
}


unit UOverviewTreeState;


interface


uses
  // Delphi
  ComCtrls, Generics.Collections,
  // Project
  UView;


type

  ///  <summary>
  ///  Class that records state of a section (level 0) node in the overview
  ///  pane's treeview.
  ///  </summary>
  TOverviewTreeSectionState = class(TObject)
  strict private
    var
      /// <summary>Value of Expanded property.</summary>
      fExpanded: Boolean;
      /// <summary>Value of ViewKey property.</summary>
      fViewKey: IViewKey;
  public
    constructor Create(ViewKey: IViewKey; const Expanded: Boolean);
      {Constructor. Sets up object.
        @param ViewItem [in] View item represented by section node.
        @param Expended [in] True if node is expanded, False if not.
      }
    destructor Destroy; override;
      {Destructor. Tears down object.
      }
    property Expanded: Boolean read fExpanded;
      {Whether section node is expanded}
    property ViewKey: IViewKey read fViewKey;
  end;

type

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
    function FindSection(ViewKey: IViewKey;
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

constructor TOverviewTreeSectionState.Create(ViewKey: IViewKey;
  const Expanded: Boolean);
  {Constructor. Sets up object.
    @param ViewItem [in] View item represented by section node.
    @param Expended [in] True if node is expanded, False if not.
  }
begin
  inherited Create;
  fViewKey := ViewKey;
  fExpanded := Expanded;
end;

destructor TOverviewTreeSectionState.Destroy;
  {Destructor. Tears down object.
  }
begin
  fViewKey := nil;
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

function TOverviewTreeState.FindSection(ViewKey: IViewKey;
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
    if Section.ViewKey.IsEqual(ViewKey) then
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
        if IsSectionNode(Node) and
          FindSection(
            (Node as TViewItemTreeNode).ViewItem.GetKey, Section
          ) then
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
      TOverviewTreeSectionState.Create(Node.ViewItem.GetKey, Node.Expanded)
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

