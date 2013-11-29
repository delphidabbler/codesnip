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
 * Defines a class used to manage interaction with, and updating of, the user's
 * favourites.
}


unit Favourites.UManager;


interface


uses
  // Delphi
  CS.Database.Types,
  Favourites.UFavourites,
  IntfNotifier,
  UView;


type
  ///  <summary>Class used to manage interaction with, and updating of, the
  ///  user's favourites list.</summary>
  TFavouritesManager = class(TObject)
  strict private
    var
      ///  <summary>The managed favourites object.</summary>
      fFavourites: TFavourites;
      ///  <summary>Flag that, when True, that one of the favourite snippets is
      ///  being updated by the database.</summary>
      fChanging: Boolean;
      ///  <summary>Stores id of any snippet currently being updated by the
      ///  database.</summary>
      fChangingSnippetID: TSnippetID;
      ///  <summary>Program notifier object.</summary>
      ///  <remarks>Passed to favourites dialogue box for notifying selected
      ///  actions to main window.</remarks>
      fNotifier: INotifier;
    ///  <summary>Handles database change events by updating the favorites list
    ///  as necessary.</summary>
    ///  <param name="Sender">TObject [in] Object that triggered event. Not
    ///  used.</param>
    ///  <param name="EvtInfo">IInterface [in] Object that carries information
    ///  about the database change event.</param>
    procedure DBChangeEventHandler(Sender: TObject; const EvtInfo: IInterface);
  public
    ///  <summary>Creates and initialises manager object.</summary>
    ///  <param name="Notifier">INotifier [in] Notifier object for passing to
    ///  Favourites dialogue box.</param>
    ///  <remarks>Constructs owned favourites list object and initialises it
    ///  from any stored favourites.</remarks>
    constructor Create(Notifier: INotifier);
    ///  <summary>Destroys manager object.</summary>
    destructor Destroy; override;
    ///  <summary>Displays or re-activates non-modal Favourites dialogue box.
    ///  </summary>
    procedure ShowDialogue;
    ///  <summary>Checks if data represented by given view can be added to
    ///  favourites list.</summary>
    ///  <remarks>To be added the view must be a snippet view and the snippet
    ///  must not already be a favourite.</remarks>
    function CanAddFavourite(View: IView): Boolean;
    ///  <summary>Adds snippet represented by given view as a favourite.
    ///  </summary>
    ///  <remarks>View must be a snippet view and the snippet must not already
    ///  be a favourite.</remarks>
    procedure AddFavourite(View: IView);
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
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
  _Database.AddChangeEventHandler(DBChangeEventHandler);
end;

procedure TFavouritesManager.DBChangeEventHandler(Sender: TObject;
  const EvtInfo: IInterface);
var
  EventInfo: IDatabaseChangeEventInfo;  // information about the event

  ///  <summary>Extracts snippet ID from EvtInfo object.</summary>
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

