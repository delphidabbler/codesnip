{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Provides an abstract base class that can render tree nodes representing
 * snippets in a tree view.
}


unit USnippetsTVDraw;


interface


uses
  // Delphi
  ComCtrls,
  // Project
  DB.UCollections;


type

  {
  TSnippetsTVDraw:
    Abstract base class that can render tree nodes representing snippets objects
    in a tree view.
  }
  TSnippetsTVDraw = class abstract(TObject)
  strict protected
    ///  <summary>Gets the collection ID, if any, associated with a tree node.
    ///  </summary>
    ///  <param name="Node"><c>TTreeNode</c> [in] Node to be checked.</param>
    ///  <returns><c>TCollectionID</c>. Associated collection ID. If <c>Node</c>
    ///  has no associated collection then a null collection ID is returned.
    ///  </returns>
    function GetCollectionID(const Node: TTreeNode): TCollectionID;
      virtual; abstract;

//    function IsUserDefinedNode(const Node: TTreeNode): Boolean;
//      virtual; abstract;
//      {Checks if a node represents a user defined snippets object.
//        @param Node [in] Node to be checked.
//        @return True if node represents user defined object, False if not.
//      }

    function IsSectionHeadNode(const Node: TTreeNode): Boolean;
      virtual;
      {Checks if a node represents a section header.
        @param Node [in] Node to be checked.
        @return True if node is a section header, False if not.
      }
    function IsErrorNode(const Node: TTreeNode): Boolean;
      virtual;
      {Checks if a node represents an error condition.
        @param Node [in] Node to be checked.
        @return True if node represents error condition, False if not.
      }
  public
    procedure CustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
      {Handles event triggered when a snippets tree view item is to be
      displayed. Draws nodes depending on whether group heading, snippet, user
      defined, selected or focussed.
        @param Sender [in] Reference to treeview being drawn.
        @param Node [in] Node to be displayed.
        @param State [in] State of node.
        @param DefaultDraw [in/out] Not changed. Permits item to be drawn by
          system.
      }
  end;


implementation


uses
  // Delphi
  Graphics,
  // Project
  UColours,
  UPreferences;


{ TSnippetsTVDraw }

procedure TSnippetsTVDraw.CustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
  {Handles event triggered when a snippets tree view item is to be displayed.
  Draws nodes depending on whether group heading, snippet, user defined,
  selected or focussed.
    @param Sender [in] Reference to treeview being drawn.
    @param Node [in] Node to be displayed.
    @param State [in] State of node.
    @param DefaultDraw [in/out] Not changed. Permits item to be drawn by system.
  }
var
  TV: TTreeView;  // reference to treeview being rendered
begin
  Assert(Sender is TTreeView,
    ClassName + '.CustomDrawItem: Sender is not TTreeView');
  TV := Sender as TTreeView;
  TV.Font.Size := Preferences.OverviewFontSize;
  if Assigned(Node) then
  begin
    // Set font and background colour according to selected / focusses state
    if cdsSelected in State then
    begin
      if cdsFocused in State then
      begin
        // selected and focussed
        TV.Canvas.Brush.Color := clHighlight;
        TV.Canvas.Font.Color := clHighlightText;
      end
      else
      begin
        // selected but not focussed
        TV.Canvas.Brush.Color := clBtnFace;
        TV.Canvas.Font.Color := TV.Font.Color;
      end;
    end
    else
    begin
      // not selected
      if IsErrorNode(Node) then
        // colour unselected error nodes differently
        TV.Canvas.Font.Color := clWarningText
      else
        TV.Canvas.Font.Color :=
//          Preferences.DBHeadingColours[IsUserDefinedNode(Node)];
          Preferences.GetDBHeadingColour(GetCollectionID(Node));
      TV.Canvas.Brush.Color := TV.Color;
    end;
    if IsSectionHeadNode(Node) then
      // make header items bold
      TV.Canvas.Font.Style := [fsBold];
    DefaultDraw := True;
  end;
end;

function TSnippetsTVDraw.IsErrorNode(const Node: TTreeNode): Boolean;
  {Checks if a node represents an error condition.
    @param Node [in] Node to be checked.
    @return True if node represents error condition, False if not.
  }
begin
  // Default response
  Result := False;
end;

function TSnippetsTVDraw.IsSectionHeadNode(const Node: TTreeNode): Boolean;
  {Checks if a node represents a section header.
    @param Node [in] Node to be checked.
    @return True if node is a section header, False if not.
  }
begin
  // Default response
  Result := False;
end;

end.

