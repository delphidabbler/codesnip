{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Defines types used to encapsulate a list of a user's favourite snippets.
}


unit Favourites.UFavourites;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  UMultiCastEvents, USnippetIDs;


type
  ///  <summary>Record encapsulating a favourite.</summary>
  TFavourite = record
  public
    ///  <summary>Id of favourite snippet.</summary>
    SnippetID: TSnippetID;
    ///  <summary>Date favourite was last accessed.</summary>
    LastAccessed: TDateTime;
    ///  <summary>Constructs a TFavourite record with given snippet ID and last
    ///  access date.</summary>
    constructor Create(const ASnippetID: TSnippetID;
      const ALastAccessed: TDateTime);
  end;

type
  ///  <summary>Interface supported by objects passed to a TFavourites object's
  ///  change listener that provides information about a change to the
  ///  TFavourites object.</summary>
  IFavouritesChangeEventInfo = interface(IInterface)
    ['{78F1BAAC-BEF3-4EFE-AEE8-70F67C671DEB}']
    ///  <summary>Read accessor for Action property.</summary>
    function GetAction: TCollectionNotification;
    ///  <summary>Type of action for which event was triggered.</summary>
    property Action: TCollectionNotification read GetAction;
    ///  <summary>Read accessor for Favourite property.</summary>
    function GetFavourite: TFavourite;
    ///  <summary>Favourite to which event's action applies.</summary>
    property Favourite: TFavourite read GetFavourite;
  end;

type
  ///  <summary>Encapsulates a list of favourite snippets and the operations on
  ///  it.</summary>
  TFavourites = class sealed(TObject)
  strict private
    type
      ///  <summary>Encapsulates an event containing information about a change
      ///  to the favourites list.</summary>
      ///  <remarks>Passed to objects that have registered a change listener
      ///  with a TFavourites object.</remarks>
      TEventInfo = class(TInterfacedObject, IFavouritesChangeEventInfo)
      strict private
        var
          ///  <summary>Change action performed on favourites list.</summary>
          fAction: TCollectionNotification;
          ///  <summary>Favourite affected by change action.</summary>
          fFavourite: TFavourite;
      public
        ///  <summary>Constructs object for given event action and favourite.
        ///  </summary>
        constructor Create(const AAction: TCollectionNotification;
          const AFavourite: TFavourite);
        ///  <summary>Returns change action performed on favourites list.
        ///  </summary>
        function GetAction: TCollectionNotification;
        ///  <summary>Returns favourite affected by change action.</summary>
        function GetFavourite: TFavourite;
      end;
  strict private
    var
      ///  <summary>List of favourites.</summary>
      fList: TList<TFavourite>;
      ///  <summary>List of change event listeners.</summary>
      fListeners: TMulticastEvents;
    ///  <summary>Handler for change notifications from favourites list.
    ///  </summary>
    procedure ChangeHandler(Sender: TObject; const Item: TFavourite;
      Action: TCollectionNotification);
    ///  <summary>Gets favourite associated with given snippet ID.</summary>
    ///  <exception>EArgumentOutOfRangeException exception raised if snippet ID
    ///  is not in the list.</exception>
    function GetFavourite(const SnippetID: TSnippetID): TFavourite;
  public
    ///  <summary>Constructs new, empty, favourites object.</summary>
    constructor Create;
    ///  <summary>Destroys favourites object.</summary>
    destructor Destroy; override;
    ///  <summary>Checks if a favourite for the snippet with the given ID is in
    ///  the favourites list.</summary>
    function IsFavourite(const SnippetID: TSnippetID): Boolean;
    ///  <summary>Checks if the favourites list is empty.</summary>
    function IsEmpty: Boolean;
    ///  <summary>Adds the snippet with the given ID to the favourites list,
    ///  with the given last access date stamp.</summary>
    procedure Add(const SnippetID: TSnippetID; const DateStamp: TDateTime);
    ///  <summary>Removes the favourite for the snippet with the given ID from
    ///  the favourites list.</summary>
    ///  <remarks>Does nothing if the given snippet is not a favourite.
    ///  </remarks>
    procedure Remove(const SnippetID: TSnippetID);
    ///  <summary>Replaces the favourite with snippet ID OldSnippetID with one
    ///  with snippet ID NewSnippetID. The last access date is not changed.
    ///  </summary>
    ///  <remarks>OldSnippetID must be in the favourites list and NewSnippetID
    ///  must not already be in the list.</remarks>
    procedure Replace(const OldSnippetID, NewSnippetID: TSnippetID);
    ///  <summary>Clears all favourites from the list.</summary>
    procedure Clear;
    ///  <summary>Sets the last access date for the favourite with the given
    ///  snippet ID to the current date and time.</summary>
    ///  <remarks>A favourite with the given snippet ID must be in the
    ///  favourites list.</remarks>
    procedure Touch(const SnippetID: TSnippetID);
    ///  <summary>Gets the favourites list enumerator.</summary>
    ///  <remarks>The caller is responsible for freeing the enumerator object.
    ///  </remarks>
    function GetEnumerator: TEnumerator<TFavourite>;
    ///  <summary>Adds the given change event handler to the list of listeners.
    ///  </summary>
    ///  <remarks>Each event handler in the listener list is called for each
    ///  change to the favourites list. Event handlers are passed an object
    ///  supporting IFavouritesChangeEventInfo that describes the event.
    ///  </remarks>
    procedure AddListener(AListener: TNotifyEventInfo);
    ///  <summary>Removes the given change event handler from the listeners
    ///  list.</summary>
    procedure RemoveListener(AListener: TNotifyEventInfo);
  end;


implementation


uses
  // Delphi
  SysUtils, Generics.Defaults;


{ TFavourites }

procedure TFavourites.Add(const SnippetID: TSnippetID;
  const DateStamp: TDateTime);
begin
  Assert(not IsFavourite(SnippetID),
    ClassName + '.Add: Snippet is already a favourite');
  fList.Add(TFavourite.Create(SnippetID, DateStamp));
end;

procedure TFavourites.AddListener(AListener: TNotifyEventInfo);
begin
  fListeners.AddHandler(AListener);
end;

procedure TFavourites.ChangeHandler(Sender: TObject; const Item: TFavourite;
  Action: TCollectionNotification);
var
  Evt: IFavouritesChangeEventInfo;
begin
  Evt := TEventInfo.Create(Action, Item);
  fListeners.TriggerEvents(Evt);
end;

procedure TFavourites.Clear;
begin
  fList.Clear;
end;

constructor TFavourites.Create;
begin
  inherited Create;
  fList := TList<TFavourite>.Create(
    TDelegatedComparer<TFavourite>.Create(
      function (const Left, Right: TFavourite): Integer
      begin
        Result := Left.SnippetID.CompareTo(Right.SnippetID);
      end
    )
  );
  fList.OnNotify := ChangeHandler;

  fListeners := TMultiCastEvents.Create(Self);
end;

destructor TFavourites.Destroy;
begin
  fList.OnNotify := nil;
  fListeners.Free;
  fList.Free;
  inherited;
end;

function TFavourites.GetEnumerator: TEnumerator<TFavourite>;
begin
  Result := fList.GetEnumerator;
end;

function TFavourites.GetFavourite(const SnippetID: TSnippetID): TFavourite;
begin
  Result := fList[fList.IndexOf(TFavourite.Create(SnippetID, 0.0))];
end;

function TFavourites.IsEmpty: Boolean;
begin
  Result := fList.Count = 0;
end;

function TFavourites.IsFavourite(const SnippetID: TSnippetID): Boolean;
begin
  Result := fList.Contains(TFavourite.Create(SnippetID, 0.0));
end;

procedure TFavourites.Remove(const SnippetID: TSnippetID);
begin
  fList.Remove(TFavourite.Create(SnippetID, 0.0));
end;

procedure TFavourites.RemoveListener(AListener: TNotifyEventInfo);
begin
  fListeners.RemoveHandler(AListener);
end;

procedure TFavourites.Replace(const OldSnippetID, NewSnippetID: TSnippetID);
var
  OldFav: TFavourite;
begin
  Assert(IsFavourite(OldSnippetID),
    ClassName + '.Replace: OldSnippetID is not a favourite');
  Assert(not IsFavourite(NewSnippetID),
    ClassName + '.Replace: NewSnippetID is already a favourite');

  if OldSnippetID = NewSnippetID then
    Exit;

  OldFav := GetFavourite(OldSnippetID);
  fList.Remove(OldFav);
  if not IsFavourite(NewSnippetID) then
    fList.Add(TFavourite.Create(NewSnippetID, OldFav.LastAccessed));
end;

procedure TFavourites.Touch(const SnippetID: TSnippetID);
var
  Idx: Integer;
begin
  Idx := fList.IndexOf(TFavourite.Create(SnippetID, 0.0));
  Assert(Idx >= 0, ClassName + '.Touch: Snippet is not a favourite');
  fList[Idx] := TFavourite.Create(SnippetID, Now);
end;

{ TFavourites.TEventInfo }

constructor TFavourites.TEventInfo.Create(
  const AAction: TCollectionNotification; const AFavourite: TFavourite);
begin
  inherited Create;
  fAction := AAction;
  fFavourite := AFavourite;
end;

function TFavourites.TEventInfo.GetAction: TCollectionNotification;
begin
  Result := fAction;
end;

function TFavourites.TEventInfo.GetFavourite: TFavourite;
begin
  Result := fFavourite;
end;

{ TFavourite }

constructor TFavourite.Create(const ASnippetID: TSnippetID;
  const ALastAccessed: TDateTime);
begin
  SnippetID := ASnippetID;
  LastAccessed := ALastAccessed;
end;

end.

