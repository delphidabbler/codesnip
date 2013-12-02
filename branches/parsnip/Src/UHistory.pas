{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Class that records and retrieves history of page accesses.
}


unit UHistory;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  UView;


type

  ///  <summary>Class that records and retrieves history of displayed view
  ///  items.</summary>
  ///  <remarks>Access to two lists is provided: the 'back' list of items that
  ///  preceed the currently selected item in the history and the 'forward' list
  ///  of items that follow the currently selected item.</remarks>
  THistory = class(TObject)
  strict private
    type
      ///  <summary>Class that implementes list of views in history.</summary>
      THistoryList = TViewList;
    var
      ///  <summary>List of views in history.</summary>
      fItems: THistoryList;
      ///  <summary>Index of current history item in list</summary>
      fCursor: Integer;
    const
      ///  <summary>Max number if items stored in history.</summary>
      cMaxHistoryItems = 50;
  strict private
    ///  <summary>Returns current view item in history.</summary>
    function GetCurrent: IView;

    ///  <summary>Handles database change events by updating the history as
    ///  necessary.</summary>
    ///  <param name="Sender">TObject [in] Object that triggered event. Not
    ///  used.</param>
    ///  <param name="EvtInfo">IInterface [in] Object that carries information
    ///  about the database change event.</param>
    procedure DBChangeHandler(Sender: TObject; const EvtInfo: IInterface);

  public
    ///  <summary>Constructs and initialises history object.</summary>
    constructor Create;

    ///  <summary>Destroys history object.</summary>
    destructor Destroy; override;

    ///  <summary>Clears history.</summary>
    procedure Clear;

    ///  <summary>Creates new history item for given view and adds it to history
    ///  list.</summary>
    ///  <remarks>Any items in history after insertion position for this new
    ///  item are discarded.</remarks>
    procedure NewItem(ViewItem: IView);

    ///  <summary>Selects given view item in history list.</summary>
    procedure SelectItem(ViewItem: IView);

    ///  <summary>Moves backward in history and returns new current item or nil
    ///  if at start of list when method called.</summary>
    function GoBack: IView;

    ///  <summary>Stores list of views in 'back' list in given list object.
    ///  </summary>
    procedure BackList(const List: TViewList);

    ///  <summary>Returns number of items in 'back' list.</summary>
    function BackListCount: Integer;

    ///  <summary>Moves forward in history and returns new current item or nil
    ///  if at end of list when method called.</summary>
    function GoForward: IView;

    ///  <summary>Stores list of views in 'forward' list in given list object.
    ///  </summary>
    procedure ForwardList(const List: TViewList);

    ///  <summary>Returns number of items in 'forward' list.</summary>
    function ForwardListCount: Integer;

    ///  <summary>Reference to currently selected view item in history or nil if
    ///  there is no current item.</summary>
    property Current: IView read GetCurrent;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  DB.UMain, UExceptions;


{ THistory }

procedure THistory.BackList(const List: TViewList);
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
begin
  // We have entries in back list if current item index > 0
  if fCursor > 0 then
    Result := fCursor
  else
    Result := 0;
end;

procedure THistory.Clear;
begin
  fItems.Clear;   // frees objects in list
  fCursor := -1;  // there is no current item
end;

constructor THistory.Create;
begin
  inherited;
  fItems := THistoryList.Create;
  Clear;
  _Database.AddChangeEventHandler(DBChangeHandler);
end;

procedure THistory.DBChangeHandler(Sender: TObject; const EvtInfo: IInterface);
var
  EventInfo: IDatabaseChangeEventInfo;  // information about the event
begin
  EventInfo := EvtInfo as IDatabaseChangeEventInfo;
  // Clear history if snippet or category changed or removed
  case EventInfo.Kind of
    evSnippetDeleted, evSnippetChanged,
    evCategoryDeleted, evCategoryChanged:
      Clear;
    evSnippetAdded, evCategoryAdded:
      NewItem(TViewFactory.CreateDBItemView(EventInfo.Info));
  end;
end;

destructor THistory.Destroy;
begin
  _Database.RemoveChangeEventHandler(DBChangeHandler);
  fItems.Free;
  inherited;
end;

procedure THistory.ForwardList(const List: TViewList);
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
begin
  // Number of items in list is count of those following cursor position
  Result := Pred(fItems.Count) - fCursor;
  if Result < 0 then
    Result := 0;
end;

function THistory.GetCurrent: IView;
begin
  if (fCursor >= 0) and (fCursor < fItems.Count) then
    Result := fItems[fCursor]
  else
    Result := nil;
end;

function THistory.GoBack: IView;
begin
  if fCursor >= 0 then
    Dec(fCursor);
  Result := Current;
end;

function THistory.GoForward: IView;
begin
  if fCursor < fItems.Count then
    Inc(fCursor);
  Result := Current;
end;

procedure THistory.NewItem(ViewItem: IView);
var
  I: Integer;         // loops thru history list
  ClonedItem: IView;  // cloned copy of view item
begin
  Assert(fCursor <= fItems.Count, ClassName + '.NewItem: fCursor too large');
  Assert(fCursor >= -1, ClassName + '.NewItem: fCursor too small');
  // Create copy of given view item
  if Supports(ViewItem, INullView) then
    Exit; // don't record nul views
  ClonedItem := TViewFactory.Clone(ViewItem);
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

procedure THistory.SelectItem(ViewItem: IView);
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

