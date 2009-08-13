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
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
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
  ComCtrls, Contnrs,
  // Project
  UView, UViewItemTreeNode;


type

  {
  TOverviewTreeSectionState:
    Class that records the state of an overview pane section node.
  }
  TOverviewTreeSectionState = class(TObject)
  strict private
    fExpanded: Boolean;     // value of Expanded property
    fViewItem: TViewItem;   // value of ViewItem property
  public
    constructor Create(const ViewItem: TViewItem; const Expanded: Boolean);
      {Class constructor. Sets up object.
        @param ViewItem [in] View item represented by section node.
        @param Expended [in] True if node is expanded, False if not.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
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
    fTV: TTreeView;           // reference to treeview
    fSections: TObjectList;   // list of section objects
    function GetCount: Integer;
      {Number of sections in list.
        @return Section count.
      }
    function GetSection(Idx: Integer): TOverviewTreeSectionState;
      {Gets a section object by index.
        @param Idx [in] Index of required section.
        @return Reference to required section.
      }
    procedure Clear;
      {Clears all maintained objects.
      }
    procedure Add(const Node: TViewItemTreeNode);
      {Adds reference to a treeview section whose expansion state is to be
      maintained by the object.
        @param Node [in] Treeview node whose state is to be maintained.
      }
    function FindSection(const ViewItem: TViewItem;
      out Section: TOverviewTreeSectionState): Boolean;
      {Finds a section object that references a specified view item.
        @param ViewItem [in] View item being searched for.
        @param Section [in] Section object or nil if view item not found.
        @return True if view item found, False if not.
      }
    function IsSectionNode(const Node: TTreeNode): Boolean;
      {Checks if a node is a section node.
        @param Node [in] Node to be checked.
        @return True if node is a section node.
      }
  public
    constructor Create(const TV: TTreeView);
      {Class constructor. Sets up object to work with a treeview.
        @param TV [in] Treeview whose state is to be managed.
      }
    destructor Destroy; override;
      {Class desctuctor. Tears down object.
      }
    procedure SaveState;
      {Saves expansion state of all section nodes in treeview.
      }
    procedure RestoreState;
      {Restores state of all section nodes in overview for which expansion state
      has been previously recorded.
      }
  end;


implementation


uses
  // Delphi
  SysUtils;


{ TOverviewTreeSectionState }

constructor TOverviewTreeSectionState.Create(const ViewItem: TViewItem;
  const Expanded: Boolean);
  {Class constructor. Sets up object.
    @param ViewItem [in] View item represented by section node.
    @param Expended [in] True if node is expanded, False if not.
  }
begin
  inherited Create;
  fViewItem := TViewItem.Create(ViewItem);
  fExpanded := Expanded;
end;

destructor TOverviewTreeSectionState.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fViewItem);
  inherited;
end;

{ TOverviewTreeState }

procedure TOverviewTreeState.Add(const Node: TViewItemTreeNode);
  {Adds reference to a treeview section whose expansion state is to be
  maintained by the object.
    @param Node [in] Treeview node whose state is to be maintained.
  }
begin
  fSections.Add(TOverviewTreeSectionState.Create(Node.ViewItem, Node.Expanded));
end;

procedure TOverviewTreeState.Clear;
  {Clears all maintained objects.
  }
begin
  fSections.Clear;
end;

constructor TOverviewTreeState.Create(const TV: TTreeView);
  {Class constructor. Sets up object to work with a treeview.
    @param TV [in] Treeview whose state is to be managed.
  }
begin
  inherited Create;
  fSections := TObjectList.Create(True);
  fTV := TV;
end;

destructor TOverviewTreeState.Destroy;
  {Class desctuctor. Tears down object.
  }
begin
  FreeAndNil(fSections);
  inherited;
end;

function TOverviewTreeState.FindSection(const ViewItem: TViewItem;
  out Section: TOverviewTreeSectionState): Boolean;
  {Finds a section object that references a specified view item.
    @param ViewItem [in] View item being searched for.
    @param Section [in] Section object or nil if view item not found.
    @return True if view item found, False if not.
  }
var
  Idx: Integer; // loops through all recorded section objects
begin
  Result := False;
  Section := nil;
  for Idx := 0 to Pred(GetCount) do
  begin
    if GetSection(Idx).ViewItem.IsEqual(ViewItem) then
    begin
      Section := GetSection(Idx);
      Result := True;
      Break;
    end;
  end;
end;

function TOverviewTreeState.GetCount: Integer;
  {Number of sections in list.
    @return Section count.
  }
begin
  Result := fSections.Count;
end;

function TOverviewTreeState.GetSection(Idx: Integer): TOverviewTreeSectionState;
  {Gets a section object by index.
    @param Idx [in] Index of required section.
    @return Reference to required section.
  }
begin
  Result := fSections[Idx] as TOverviewTreeSectionState;
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
  {Restores state of all section nodes in overview for which expansion state has
  been previously recorded.
  }
var
  Node: TTreeNode;                    // each node in treeview
  Section: TOverviewTreeSectionState; // object describing a section's state
begin
  for Node in fTV.Items do
  begin
    if IsSectionNode(Node)
      and FindSection((Node as TViewItemTreeNode).ViewItem, Section) then
      Node.Expanded := Section.Expanded;
  end;
end;

procedure TOverviewTreeState.SaveState;
  {Saves expansion state of all section nodes in treeview.
  }
var
  Node: TTreeNode;  // each node in treeview
begin
  Clear;
  for Node in fTV.Items do
  begin
    // we record state of each section node in treeview.
    if IsSectionNode(Node) then
      Add(Node as TViewItemTreeNode);
  end;
end;

end.

