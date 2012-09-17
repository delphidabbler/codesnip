{
 * USnippetsChkListMgr.pas
 *
 * Implements class that manages and draws check list box controls that display
 * lists of snippets.
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
 * The Original Code is USnippetsChkListMgr.pas
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


unit USnippetsChkListMgr;


interface


uses
  // Delphi
  Controls, CheckLst, Windows,
  // Project
  USnippetIDs, USnippets;


type

  {
  TSnippetsChkListMgr:
    Manages and draws check list box controls that display lists of snippets.
    Builds and clears list, sets check marks for specified snippets and
    maintains and restores snapshots of checked items.
  }
  TSnippetsChkListMgr = class(TObject)
  strict private
    fCLB: TCheckListBox;      // Reference to check list box being managed
    fSaveList: TRoutineList;  // Internal snaphot of checked snippets
    procedure CheckSnippet(const Snippet: TRoutine);
      {Checks entry corresponding to a snippet in check list box. Snippets not
      in check list box are ignored.
        @param Snippet [in] Snippet to be checked.
      }
    procedure ClearChecks;
      {Clears all checks from items in check list box.
      }
    procedure DrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
      {OnDrawItem event handler for associated check list box. Draws user
      defined snippet names in a different colour to main database snippets.
        @param Control [in] Check list box that triggered the event. Must be the
          managed control.
        @param Index [in] Index if item being drawn.
        @param Rect [in] Rectangle in check list box's canvas where item is to
          be drawn.
        @param State [in] State of list item.
      }
  public
    constructor Create(const CLB: TCheckListBox);
      {Class constructor. Sets up object to manage a specified check list box.
        @param CLB [in] Check list box to be managed.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    procedure Clear;
      {Clears the check box list.
      }
    procedure Save;
      {Takes an internal snapshot of the currently checked snippets in check
      list box.
      }
    procedure Restore;
      {Checks all snippets from any current snapshot in check list box. All
      other entries in check list box a cleared. Any snippets in snapshot that
      are not in the check list box are ignored.
      }
    procedure AddSnippet(const Snippet: TRoutine);
      {Adds a snippet to the check box list, unchecked.
        @param Snippet [in] Snippet to be added to list.
      }
    procedure CheckSnippets(const SnipList: TRoutineList);
      {Checks entries in list corresponding to each snippet in a list.
        @param SnipList [in] List of snippets to check. Snippets not in check
          list box are ignored.
      }
    procedure GetCheckedSnippets(const SnipList: TRoutineList); overload;
      {Gets all checked snippets in check list box.
        @param SnipList [in] List that receives checked snippets objects.
      }
    procedure GetCheckedSnippets(const SnipList: ISnippetIDList); overload;
      {Gets all checked snippets in check list box.
        @param SnipList [in] List that receives ids of checked snippets.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Graphics, StdCtrls,
  // Project
  UColours, UGraphicUtils;


{ TSnippetsChkListMgr }

procedure TSnippetsChkListMgr.AddSnippet(const Snippet: TRoutine);
  {Adds a snippet to the check box list, unchecked.
    @param Snippet [in] Snippet to be added to list.
  }
begin
  fCLB.Items.AddObject(Snippet.Name, Snippet);
end;

procedure TSnippetsChkListMgr.CheckSnippet(const Snippet: TRoutine);
  {Checks entry corresponding to a snippet in check list box. Snippets not in
  check list box are ignored.
    @param Snippet [in] Snippet to be checked.
  }
var
  Idx: Integer; // index of snippet in check list box
begin
  Idx := fCLB.Items.IndexOfObject(Snippet);
  if Idx >= 0 then
    fCLB.Checked[Idx] := True;
end;

procedure TSnippetsChkListMgr.CheckSnippets(const SnipList: TRoutineList);
  {Checks entries in list corresponding to each snippet in a list.
    @param SnipList [in] List of snippets to check. Snippets not in check list
      box are ignored.
  }
var
  Snippet: TRoutine;  // each snippet in list
begin
  for Snippet in SnipList do
    CheckSnippet(Snippet);
end;

procedure TSnippetsChkListMgr.Clear;
  {Clears the check box list.
  }
begin
  fCLB.Clear;
end;

procedure TSnippetsChkListMgr.ClearChecks;
  {Clears all checks from items in check list box.
  }
var
  Idx: Integer; // loops through all items in check list box
begin
  for Idx := 0 to Pred(fCLB.Count) do
    fCLB.Checked[Idx] := False;
end;

constructor TSnippetsChkListMgr.Create(const CLB: TCheckListBox);
  {Class constructor. Sets up object to manage a specified check list box.
    @param CLB [in] Check list box to be managed.
  }
begin
  Assert(Assigned(CLB), ClassName + '.Create: CLB is nil');
  inherited Create;
  fCLB := CLB;
  // make list box owner drawn and set correct height to allow for check boxes
  fCLB.OnDrawItem := DrawItem;
  fCLB.Style := lbOwnerDrawFixed;
  fCLB.ItemHeight := StringExtent('Xy', fCLB.Font).cy;
  fSaveList := TRoutineList.Create;
end;

destructor TSnippetsChkListMgr.Destroy;
  {Class destructor. Tears down object.
  }
begin
  fCLB.OnDrawItem := nil;
  fSaveList.Free;
  inherited;
end;

procedure TSnippetsChkListMgr.DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
  {OnDrawItem event handler for associated check list box. Draws user defined
  snippet names in a different colour to main database snippets.
    @param Control [in] Check list box that triggered the event. Must be the
      managed control.
    @param Index [in] Index if item being drawn.
    @param Rect [in] Rectangle in check list box's canvas where item is to be
      drawn.
    @param State [in] State of list item.
  }
var
  Canvas: TCanvas;      // check list box's canvas
begin
  inherited;
  Assert(fCLB = Control, ClassName + '.DrawItem: Control <> fCLB');
  Canvas := fCLB.Canvas;
  if not (odSelected in State)
    and (fCLB.Items.Objects[Index] as TRoutine).UserDefined then
    Canvas.Font.Color := clUserRoutine;
  Canvas.TextRect(
    Rect,
    Rect.Left + 2,
    (Rect.Top + Rect.Bottom - Canvas.TextHeight(fCLB.Items[Index])) div 2,
    fCLB.Items[Index]
  );
end;

procedure TSnippetsChkListMgr.GetCheckedSnippets(
  const SnipList: ISnippetIDList);
  {Gets all checked snippets in check list box.
    @param SnipList [in] List that receives ids of checked snippets.
  }
var
  Idx: Integer; // loops through all items in list box
begin
  SnipList.Clear;
  for Idx := 0 to Pred(fCLB.Count) do
    if fCLB.Checked[Idx] then
      SnipList.Add((fCLB.Items.Objects[Idx] as TRoutine).ID);
end;

procedure TSnippetsChkListMgr.GetCheckedSnippets(
  const SnipList: TRoutineList);
  {Gets all checked snippets in check list box.
    @param SnipList [in] List that receives checked snippets objects.
  }
var
  Idx: Integer; // loops through all items in list box
begin
  SnipList.Clear;
  for Idx := 0 to Pred(fCLB.Count) do
    if fCLB.Checked[Idx] then
      SnipList.Add(fCLB.Items.Objects[Idx] as TRoutine);
end;

procedure TSnippetsChkListMgr.Restore;
  {Checks all snippets from any current snapshot in check list box. All other
  entries in check list box a cleared. Any snippets in snapshot that are not
  in the check list box are ignored.
  }
begin
  ClearChecks;
  CheckSnippets(fSaveList);
end;

procedure TSnippetsChkListMgr.Save;
  {Takes an internal snapshot of the currently checked snippets in check list
  box.
  }
begin
  GetCheckedSnippets(fSaveList);
end;

end.

