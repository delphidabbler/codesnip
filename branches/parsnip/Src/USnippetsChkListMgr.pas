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
 * Implements class that manages and draws check list box controls that display
 * lists of snippets.
}


unit USnippetsChkListMgr;


interface


uses
  // Delphi
  Controls, CheckLst, Windows,
  // Project
  DB.USnippet, USnippetIDs;


type

  {
  TSnippetsChkListMgr:
    Manages and draws check list box controls that display lists of snippets.
    Builds and clears list, sets check marks for specified snippets and
    maintains and restores snapshots of checked items.
  }
  TSnippetsChkListMgr = class(TObject)
  strict private
    fCLB: TCheckListBox;        // Reference to check list box being managed
    fSaveList: ISnippetIDList;  // Internal snaphot of checked snippets
    procedure CheckSnippet(const Snippet: TSnippet);
      {Checks entry corresponding to a snippet in check list box. Snippets not
      in check list box are ignored.
        @param Snippet [in] Snippet to be checked.
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
    procedure AddSnippet(const Snippet: TSnippet);
      {Adds a snippet to the check box list, unchecked.
        @param Snippet [in] Snippet to be added to list.
      }
    procedure CheckSnippets(const SnipList: ISnippetIDList);
      {Checks entries in list corresponding to each snippet in a list.
        @param SnipList [in] List of snippets to check. Snippets not in check
          list box are ignored.
      }
    procedure ClearChecks;
      {Clears all checks from items in check list box.
      }
    procedure GetCheckedSnippets(const SnipList: TSnippetList); overload;
      {Gets all checked snippets in check list box.
        @param SnipList [in] List that receives checked snippets objects.
      }
    function GetCheckedSnippets: ISnippetIDList; overload;
      {Gets all checked snippets in check list box.
        @returns List that receives ids of checked snippets.
      }
    function HasCheckedItems: Boolean;
  end;


implementation


uses
  // Delphi
  Graphics, StdCtrls,
  // Project
  DB.UMain,
  UColours, UGraphicUtils, UPreferences;


{ TSnippetsChkListMgr }

procedure TSnippetsChkListMgr.AddSnippet(const Snippet: TSnippet);
  {Adds a snippet to the check box list, unchecked.
    @param Snippet [in] Snippet to be added to list.
  }
begin
  fCLB.Items.AddObject(Snippet.DisplayName, Snippet);
end;

procedure TSnippetsChkListMgr.CheckSnippet(const Snippet: TSnippet);
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

procedure TSnippetsChkListMgr.CheckSnippets(const SnipList: ISnippetIDList);
  {Checks entries in list corresponding to each snippet in a list.
    @param SnipList [in] List of snippets to check. Snippets not in check list
      box are ignored.
  }
var
  SnippetID: TSnippetID;  // each snippet in list
begin
  for SnippetID in SnipList do
    CheckSnippet(Database.Lookup(SnippetID));
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
  fSaveList := TSnippetIDList.Create;
end;

destructor TSnippetsChkListMgr.Destroy;
  {Class destructor. Tears down object.
  }
begin
  fCLB.OnDrawItem := nil;
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
  if not (odSelected in State) then
    // TODO: rethink this: DBHeadingColours[True] always used
    Canvas.Font.Color := Preferences.DBHeadingColours[True];
  Canvas.TextRect(
    Rect,
    Rect.Left + 2,
    (Rect.Top + Rect.Bottom - Canvas.TextHeight(fCLB.Items[Index])) div 2,
    fCLB.Items[Index]
  );
end;

procedure TSnippetsChkListMgr.GetCheckedSnippets(
  const SnipList: TSnippetList);
  {Gets all checked snippets in check list box.
    @param SnipList [in] List that receives checked snippets objects.
  }
var
  Idx: Integer; // loops through all items in list box
begin
  SnipList.Clear;
  for Idx := 0 to Pred(fCLB.Count) do
    if fCLB.Checked[Idx] then
      SnipList.Add(fCLB.Items.Objects[Idx] as TSnippet);
end;

function TSnippetsChkListMgr.GetCheckedSnippets: ISnippetIDList;
  {Gets all checked snippets in check list box.
    @returns List that receives ids of checked snippets.
  }
var
  Idx: Integer; // loops through all items in list box
begin
  Result := TSnippetIDList.Create;
  for Idx := 0 to Pred(fCLB.Count) do
    if fCLB.Checked[Idx] then
      Result.Add((fCLB.Items.Objects[Idx] as TSnippet).ID);
end;

function TSnippetsChkListMgr.HasCheckedItems: Boolean;
var
  Idx: Integer; // lopps thru each item in check list box
begin
  for Idx  := 0 to Pred(fCLB.Count) do
    if fCLB.Checked[Idx] then
      Exit(True);
  Result := False;
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
  fSaveList := GetCheckedSnippets;
end;

end.

