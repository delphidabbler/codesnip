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
 * Defines a class used to manage interaction with and updating of the list of
 * user favourites.
}


unit Favourites.UManager;

interface

uses
  Favourites.UFavourites, USnippetIDs, IntfNotifier, UView;

type
  TFavouritesManager = class(TObject)
  strict private
    var
      fFavourites: TFavourites;
      fChanging: Boolean;
      fChangingSnippetID: TSnippetID;
      fNotifier: INotifier;
    ///  <summary>Handles database change events by updating the favorites list
    ///  necessary.</summary>
    ///  <param name="Sender">TObject [in] Object that triggered event. Not
    ///  used.</param>
    ///  <param name="EvtInfo">IInterface [in] Object that carries information
    ///  about the database change event.</param>
    procedure DBChangeEventHandler(Sender: TObject; const EvtInfo: IInterface);
  public
    constructor Create(Notifier: INotifier);
    destructor Destroy; override;
    procedure ShowDialogue;
    function CanAddFavourite(View: IView): Boolean;
    procedure AddFavourite(View: IView);
  end;

implementation

uses
  SysUtils,
  DB.UMain, DB.USnippet, Favourites.UPersist, FmFavouritesDlg;

{ TFavouritesManager }

procedure TFavouritesManager.AddFavourite(View: IView);
begin
  Assert(CanAddFavourite(View),
    ClassName + '.AddFavourite: invalid view or already favourite');
  fFavourites.Add((View as ISnippetView).Snippet.ID, Now);
end;

function TFavouritesManager.CanAddFavourite(View: IView): Boolean;
var
  SnippetView: ISnippetView;
begin
  Result := Supports(View, ISnippetView, SnippetView)
    and not fFavourites.IsFavourite(SnippetView.Snippet.ID);
end;

constructor TFavouritesManager.Create(Notifier: INotifier);
begin
  inherited Create;
  fFavourites := TFavourites.Create;
  fNotifier := Notifier;
  TFavouritesPersist.Load(fFavourites);
  Database.AddChangeEventHandler(DBChangeEventHandler);
end;

procedure TFavouritesManager.DBChangeEventHandler(Sender: TObject;
  const EvtInfo: IInterface);
var
  EventInfo: IDatabaseChangeEventInfo;  // information about the event

  function EvtInfoToSnippetID: TSnippetID;
  begin
    Result := (EventInfo.Info as TSnippet).ID;
  end;

begin
  EventInfo := EvtInfo as IDatabaseChangeEventInfo;
  case EventInfo.Kind of
    evBeforeSnippetDelete:
    begin
      if fFavourites.IsFavourite(EvtInfoToSnippetID) then
        fFavourites.Remove(EvtInfoToSnippetID);
    end;
    evBeforeSnippetChange:
    begin
      fChangingSnippetID := EvtInfoToSnippetID;
      fChanging := fFavourites.IsFavourite(EvtInfoToSnippetID);
    end;
    evSnippetChanged:
    begin
      // update snippet id if original was in favourites list and has changed
      if fChanging and (fChangingSnippetID <> EvtInfoToSnippetID) then
        fFavourites.Replace(fChangingSnippetID, EvtInfoToSnippetID);
      fChanging := False;
    end;
  end;
end;

destructor TFavouritesManager.Destroy;
begin
  TFavouritesDlg.Close;
  TFavouritesPersist.Save(fFavourites);
  fFavourites.Free;
  inherited;
end;

procedure TFavouritesManager.ShowDialogue;
begin
  TFavouritesDlg.Display(nil, fFavourites, fNotifier);
end;

end.
