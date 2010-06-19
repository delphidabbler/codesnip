{
 * UHistory.pas
 *
 * Class that records and retrieves history of page accesses.
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
 * The Original Code is UHistory.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UHistory;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  UView;


type

  {
  THistory:
    Class that records and retrieves history of page accesses. It provides
    access to two lists: the "back" list of items that preceed the currently
    selected item in the history and the "forward" list of items that follow the
    currently selected item.
  }
  THistory = class(TObject)
  strict private
    type
      // Implements list of view items in history
      THistoryList = TObjectList<TViewItem>;
    var
      fItems: THistoryList;     // History list
      fCursor: Integer;         // Index of current history item in list
    const
      cMaxHistoryItems = 50;  // Max items stored in history
    function GetCurrent: TViewItem;
      {Gets the current view item in history.
        @return Reference to current view item or nil if there is no current
          item.
      }
  public
    constructor Create;
      {Constructor. Sets up and initialises object.
      }
    destructor Destroy; override;
      {Destructor. Tears down object.
      }
    procedure Clear;
      {Clears history list.
      }
    procedure NewItem(const ViewItem: TViewItem);
      {Creates new history item for a view item.
        @param ViewItem [in] View item to be added to history.
      }
    procedure SelectItem(const ViewItem: TViewItem);
      {Selects a view item in history list.
        @param ViewItem [in] View item to be selected.
        @except EBug raised if ViewItem not in history.
      }
    function GoBack: TViewItem;
      {Moves backward in history.
        @return Current item after moving back in history or nil if we were at
          start of list before method called.
      }
    procedure BackList(const List: TViewItemList);
      {Builds a list of items in the "back" list.
        @param List [in] Receives items in "back" list.
      }
    function BackListCount: Integer;
      {Counts items in "back" list.
        @return Number of items in list.
      }
    function GoForward: TViewItem;
      {Moves forward in history.
        @return Current item after moving forward in history or nil if we were
          at end of list before method called.
      }
    procedure ForwardList(const List: TViewItemList);
      {Builds a list of items in the "forward" list.
        @param List [in] Receives items in "forward" list.
      }
    function ForwardListCount: Integer;
      {Counts items in "forward" list.
        @return Number of items in list.
      }
    property Current: TViewItem
      read GetCurrent;
      {Reference to current view item in history or nil if there is no current
      item}
  end;


implementation


uses
  // Project
  UExceptions;


{ THistory }

procedure THistory.BackList(const List: TViewItemList);
  {Builds a list of items in the "back" list.
    @param List [in] Receives items in "back" list.
  }
var
  Idx: Integer; // loops thru "back" list
begin
  List.Clear;
  // Loop backward thru history list from just before current position, adding
  // each item to given list
  for Idx := Pred(fCursor) downto 0 do
    List.Add(fItems[Idx]);
end;

function THistory.BackListCount: Integer;
  {Counts items in "back" list.
    @return Number of items in list.
  }
begin
  // We have entries in back list if current item index > 0
  if fCursor > 0 then
    Result := fCursor
  else
    Result := 0;
end;

procedure THistory.Clear;
  {Clears history list.
  }
begin
  fItems.Clear;   // frees objects in list
  fCursor := -1;  // there is no current item
end;

constructor THistory.Create;
  {Constructor. Sets up and initialises object.
  }
begin
  inherited;
  fItems := THistoryList.Create(True);
  Clear;
end;

destructor THistory.Destroy;
  {Destructor. Tears down object.
  }
begin
  fItems.Free;  // frees all owned objects
  inherited;
end;

procedure THistory.ForwardList(const List: TViewItemList);
  {Builds a list of items in the "forward" list.
    @param List [in] Receives items in "forward" list.
  }
var
  Idx: Integer; // loops thru forward list
begin
  List.Clear;
  // Loop forward thru history list from just after current position, adding
  // each item to given list
  for Idx := Succ(fCursor) to Pred(fItems.Count) do
    List.Add(fItems[Idx]);
end;

function THistory.ForwardListCount: Integer;
  {Counts items in "forward" list.
    @return Number of items in list.
  }
begin
  // Number of items in list is count of those following cursor position
  Result := Pred(fItems.Count) - fCursor;
  if Result < 0 then
    Result := 0;
end;

function THistory.GetCurrent: TViewItem;
  {Gets the current view item in history.
    @return Reference to current view item or nil if there is no current item.
  }
begin
  if (fCursor >= 0) and (fCursor < fItems.Count) then
    Result := fItems[fCursor]
  else
    Result := nil;
end;

function THistory.GoBack: TViewItem;
  {Moves backward in history.
    @return Current item after moving back in history or nil if we were at start
      of list before method called.
  }
begin
  if fCursor >= 0 then
    Dec(fCursor);
  Result := Current;
end;

function THistory.GoForward: TViewItem;
  {Moves forward in history.
    @return Current item after moving forward in history or nil if we were at
      end of list before method called.
  }
begin
  if fCursor < fItems.Count then
    Inc(fCursor);
  Result := Current;
end;

procedure THistory.NewItem(const ViewItem: TViewItem);
  {Creates new history item for a view item.
    @param ViewItem [in] View item to be added to history.
  }
var
  I: Integer;             // loops thru history list
  ClonedItem: TViewItem;  // cloned copy of view item
begin
  Assert(fCursor <= fItems.Count, ClassName + '.NewItem: fCursor too large');
  Assert(fCursor >= -1, ClassName + '.NewItem: fCursor too small');
  // Create copy of given view item
  ClonedItem := TViewItem.Create(ViewItem);
  // Increment cursor if possible - it will reference new item
  if fCursor < fItems.Count then
    Inc(fCursor);
  // Store new item in list at required position
  if fCursor = fItems.Count then
    // cursor beyond end of list so append item to list
    fCursor := fItems.Add(ClonedItem)
  else
  begin
    // cursor within list so store new item at current position ...
    fItems[fCursor] := ClonedItem;
    // ... and delete all items in list after new position
    for I := Pred(fItems.Count) downto Succ(fCursor) do
      fItems.Delete(I); // this frees associated view item
  end;
  // Delete first item in list if list is too large and adjust cursor
  if fItems.Count > cMaxHistoryItems then
  begin
    fItems.Delete(0);
    Dec(fCursor);
  end;
end;

procedure THistory.SelectItem(const ViewItem: TViewItem);
  {Selects a view item in history list.
    @param ViewItem [in] View item to be selected.
    @except EBug raised if ViewItem not in history.
  }
var
  Idx: Integer; // index of view item in history list
begin
  Idx := fItems.IndexOf(ViewItem);
  if Idx = -1 then
    raise EBug.Create(
      ClassName + '.SelectItem: Selected view item not in history list'
    );
  fCursor := Idx;
end;

end.

