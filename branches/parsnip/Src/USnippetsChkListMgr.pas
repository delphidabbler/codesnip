{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements class that manages list box controls that display lists of
 * snippets.
}


unit USnippetsChkListMgr;


interface


uses
  // Delphi
  Controls,
  CheckLst,
  Windows,
  // Project
  CS.Database.Types,
  CS.UI.Helper.CollectionCtrlKVMgr;


type

  {
  TSnippetsChkListMgr:
    Manages check list box controls that display lists of snippets. Builds and
    clears list, sets check marks for specified snippets and maintains and
    restores snapshots of checked items.
  }
  TSnippetsChkListMgr = class(TObject)
  strict private
    var
      fCLB: TCheckListBox;        // Reference to check list box being managed
      /// <summary>Maps list box text to associated snippet and ensures the list
      ///  box is sorted.</summary>
      fCLBKVMgr: TSortedCollectionCtrlKVMgr<ISnippet>;
      fSaveList: ISnippetIDList;  // Internal snaphot of checked snippets
    procedure CheckSnippet(Snippet: ISnippet);
      {Checks entry corresponding to a snippet in check list box. Snippets not
      in check list box are ignored.
        @param Snippet [in] Snippet to be checked.
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
    procedure AddSnippet(Snippet: ISnippet);
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
    function GetCheckedSnippets: ISnippetIDList;
      {Gets all checked snippets in check list box.
        @returns List that receives ids of checked snippets.
      }
    ///  <summary>Checks any of the items in the check list box are checked.
    ///  </summary>
    function HasCheckedItems: Boolean;
  end;


implementation


uses
  // Delphi
  Graphics,
  StdCtrls,
  // Project
  CS.Database.Snippets,
  DB.UMain,
  UColours,
  UGraphicUtils;


{ TSnippetsChkListMgr }

procedure TSnippetsChkListMgr.AddSnippet(Snippet: ISnippet);
  {Adds a snippet to the check box list, unchecked.
    @param Snippet [in] Snippet to be added to list.
  }
begin
  fCLBKVMgr.Add(Snippet, Snippet.Title);
end;

procedure TSnippetsChkListMgr.CheckSnippet(Snippet: ISnippet);
  {Checks entry corresponding to a snippet in check list box. Snippets not in
  check list box are ignored.
    @param Snippet [in] Snippet to be checked.
  }
var
  Idx: Integer; // index of snippet in check list box
begin
  Idx := fCLBKVMgr.IndexOfKey(Snippet);
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
    CheckSnippet(Database.LookupSnippet(SnippetID));
end;

procedure TSnippetsChkListMgr.Clear;
  {Clears the check box list.
  }
begin
  fCLBKVMgr.Clear;
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
  fCLBKVMgr := TSortedCollectionCtrlKVMgr<ISnippet>.Create(
    TListBoxAdapter.Create(fCLB),
    True,
    function (const Left, Right: ISnippet): Boolean
      begin
        Result := Left.ID = Right.ID;
      end,
    stIgnoreCase
  );
  fSaveList := TSnippetIDList.Create;
end;

destructor TSnippetsChkListMgr.Destroy;
  {Class destructor. Tears down object.
  }
begin
  fCLBKVMgr.Free;
  inherited;
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
      Result.Add(fCLBKVMgr.GetKeyAt(Idx).ID);
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

