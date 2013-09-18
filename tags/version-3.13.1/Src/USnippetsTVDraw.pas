{
 * USnippetsTVDraw.pas
 *
 * Provides an abstract base class that can render tree nodes representing
 * snippets in a tree view.
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
 * The Original Code is USnippetsTVDraw.pas
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


unit USnippetsTVDraw;


interface


uses
  // Delphi
  ComCtrls;


type

  {
  TSnippetsTVDraw:
    Abstract base class that can render tree nodes representing snippets objects
    in a tree view.
  }
  TSnippetsTVDraw = class abstract(TObject)
  strict protected
    function IsUserDefinedNode(const Node: TTreeNode): Boolean;
      virtual; abstract;
      {Checks if a node represents a user defined snippets object.
        @param Node [in] Node to be checked.
        @return True if node represents user defined object, False if not.
      }
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
      displayed. Draws nodes depending on whether category, routine, user
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
  UColours;


{ TSnippetsTVDraw }

procedure TSnippetsTVDraw.CustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
  {Handles event triggered when a snippets tree view item is to be displayed.
  Draws nodes depending on whether category, routine, user defined, selected or
  focussed.
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
      else if IsUserDefinedNode(Node) then
        // colour unselected user defined snippets differently
        TV.Canvas.Font.Color := clUserRoutine
      else
        TV.Canvas.Font.Color := TV.Font.Color;
      TV.Canvas.Brush.Color := TV.Color;
    end;
    if IsSectionHeadNode(Node) then
      // make header (category) items bold
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

