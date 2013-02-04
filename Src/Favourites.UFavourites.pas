{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines types used to encapsulate a list of a user's favourite snippets.
}


unit Favourites.UFavourites;

interface

uses
  Generics.Collections,
  UMultiCastEvents, USnippetIDs;

type
  TFavourite = record
  public
    SnippetID: TSnippetID;
    LastAccessed: TDateTime;
    constructor Create(const ASnippetID: TSnippetID;
      const ALastAccessed: TDateTime);
  end;

type
  IFavouritesChangeEventInfo = interface(IInterface)
    ['{78F1BAAC-BEF3-4EFE-AEE8-70F67C671DEB}']
    function GetAction: TCollectionNotification;
    property Action: TCollectionNotification read GetAction;
    function GetFavourite: TFavourite;
    property Favourite: TFavourite read GetFavourite;
  end;

type
  TFavourites = class sealed(TObject)
  strict private
    type
      TEventInfo = class(TInterfacedObject, IFavouritesChangeEventInfo)
      strict private
        var
          fAction: TCollectionNotification;
          fFavourite: TFavourite;
      public
        constructor Create(const AAction: TCollectionNotification;
          const AFavourite: TFavourite);
        function GetAction: TCollectionNotification;
        function GetFavourite: TFavourite;
      end;
  strict private
    var
      fList: TList<TFavourite>;
      fListeners: TMulticastEvents;
    procedure ChangeHandler(Sender: TObject; const Item: TFavourite;
      Action: TCollectionNotification);
    function GetFavourite(const SnippetID: TSnippetID): TFavourite;
  public
    constructor Create;
    destructor Destroy; override;
    function IsFavourite(const SnippetID: TSnippetID): Boolean;
    function IsEmpty: Boolean;
    procedure Add(const SnippetID: TSnippetID; const DateStamp: TDateTime);
    procedure Remove(const SnippetID: TSnippetID);
    procedure Replace(const OldSnippetID, NewSnippetID: TSnippetID);
    procedure Clear;
    procedure Touch(const SnippetID: TSnippetID);
    function GetEnumerator: TEnumerator<TFavourite>;
    procedure AddListener(AListener: TNotifyEventInfo);
    procedure RemoveListener(AListener: TNotifyEventInfo);
  end;

implementation

uses
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
