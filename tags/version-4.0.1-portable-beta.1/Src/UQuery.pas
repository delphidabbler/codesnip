{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines a singleton object that encapsulates a query on the code snippets
 * database.
}


unit UQuery;


interface


uses
  // Project
  DB.UCategory, DB.USnippet, USearch;


type

  {
  IQuery:
    Interface to object that encapsulates a query on the code snippets database.
    It enables a search to be run against the database and provides access to
    the selected snippets.
  }
  IQuery = interface(IInterface)
    ['{10998D72-CD5D-482B-9626-D771A50C53BA}']
    function ApplySearch(Search: ISearch; const Refine: Boolean): Boolean;
      {Runs query by applying a search to the whole database. If search succeeds
      matching snippets and search are stored in query's Selection and Search
      properties. If search fails Selection and Search are left unchanged.
        @param Search [in] Search to apply.
        @param Refine [in] Flag indicating whether any existing search should
          be refined (True) or search should be of whole databas (False).
        @return True if search succeeds and False if it fails.
      }
    procedure Reset;
      {Resets query. Selection property is set to all snippets in database and
      Search property is set to nul search.
      }
    ///  <summary>Checks if there is an active search.</summary>
    function IsSearchActive: Boolean;
    function Refresh: Boolean;
      {Re-applies the latest search if one exists.
        @return True if search was re-applied, False if there was no search to
          apply.
      }
    procedure Update;
      {Updates query. Attempts to refresh query and, if that fails, resets it.
      }
    function GetLatestSearch: ISearch;
      {Gets value of LatestSearch property.
        @return Search object used to generate latest search.
      }
    function GetSelection: TSnippetList;
      {Gets value of Selection property.
        @return List of snippets matching current query.
      }
    procedure GetCatSelection(const Cat: TCategory;
      const Snippets: TSnippetList);
      {Provides list of snippets selected by last search that are in a specified
      category.
        @param Cat [in] Reference to required category.
        @param Snippets [in] Object to receive snippet list. List is emptied
          before snippets are copied in.
      }
    property LatestSearch: ISearch read GetLatestSearch;
      {Reference to search object used to generate current query}
    property Selection: TSnippetList read GetSelection;
      {List of snippets that match current query. This records all snippets in
      database if there is no search}
  end;


function Query: IQuery;
  {Returns singleton instance of object that encapsulates a query on the
  snippets database.
    @return Singleton object.
  }


implementation


uses
  // Delphi
  SysUtils, Generics.Collections,
  // Project
  DB.UMain, UBaseObjects, USingleton;


type

  {
  TQuery:
    Class that encapsulates a query on the code snippets database. It enables a
    search to be run against the database and makes the found snippets
    available. Must only be instantiated once as a singleton.
  }
  TQuery = class(TSingleton,
    IQuery
  )
  strict private
    var
      fSelection: TSnippetList;   // List of snippets selected by current query
      fActiveSearches: TList<ISearch>; // List of currently active searches
    class function GetInstance: IQuery; static;
      {Gets singleton instance of class, creating it if necessary
        @return Singleton instance.
      }
    function RunSearch(Search: ISearch): Boolean;
  protected // do not make strict
    { IQuery methods }
    function ApplySearch(Search: ISearch; const Refine: Boolean): Boolean;
      {Runs query by applying a search to the whole database. If search succeeds
      matching snippets and search are stored in query's Selection and Search
      properties. If search fails Selection and Search are left unchanged.
        @param Search [in] Search to apply.
        @param Refine [in] Flag indicating whether any existing search should
          be refined (True) or search should be of whole databas (False).
        @return True if search succeeds and False if it fails.
      }
    ///  <summary>Checks if there is an active search.</summary>
    ///  <remarks>A search is active if the latest search is not the null
    ///  search.</remarks>
    function IsSearchActive: Boolean;
    function Refresh: Boolean;
      {Re-applies the latest search if one exists.
        @return True if search was re-applied, False if there was no search to
          apply.
      }
    procedure Reset;
      {Resets query. Selection property is set to all snippets in database and
      Search property is set to nul search.
      }
    procedure Update;
      {Updates query. Attempts to refresh query and, if that fails, resets it.
      }
    function GetLatestSearch: ISearch;
      {Gets reference to latest search object.
        @return Required search object.
      }
    function GetSelection: TSnippetList;
      {Gets reference to list of snippets selected by last search.
        @return Reference to required list of snippets.
      }
    procedure GetCatSelection(const Cat: TCategory;
      const SnipList: TSnippetList);
      {Provides list of snippets selected by last search that are in a specified
      category.
        @param Cat [in] Reference to required category.
        @param SnipList [in] Object to receive snippet list. List is emptied
          before snippets are copied in.
      }
  strict protected
    procedure Initialize; override;
    procedure Finalize; override;
  public
    class property Instance: IQuery read GetInstance;
  end;

function Query: IQuery;
  {Returns singleton instance of object that encapsulates a query on the
  snippets database.
    @return Singleton object.
  }
begin
  Result := TQuery.Instance;
end;

{ TQuery }

function TQuery.ApplySearch(Search: ISearch; const Refine: Boolean):
  Boolean;
  {Runs query by applying a search to the whole database. If search succeeds
  matching snippets and search are stored in query's Selection and Search
  properties. If search fails Selection and Search are left unchanged.
    @param Search [in] Search to apply. Must not be nil or null search.
    @param Refine [in] Flag indicating whether any existing search should be
      refined (True) or search should be of whole databas (False).
    @return True if search succeeds and False if it fails.
  }
begin
  Assert(Assigned(Search), ClassName + '.ApplySearch: Search is nil');
  Assert(not Search.IsNul, ClassName + '.ApplySearch: Search can''t be null');

  if IsSearchActive and not Refine then
    Reset;
  Result := RunSearch(Search);
  if not Result then
    Exit;
  // Search was successful
  fActiveSearches.Add(Search);
end;

procedure TQuery.Finalize;
begin
  fSelection.Free;
  fActiveSearches.Free;
  inherited;
end;

procedure TQuery.GetCatSelection(const Cat: TCategory;
  const SnipList: TSnippetList);
  {Provides list of snippets selected by last search that are in a specified
  category.
    @param Cat [in] Reference to required category.
    @param SnipList [in] Object to receive snippet list. List is emptied before
      snippets are copied in.
  }
var
  Idx: Integer; // Loops thru all snippets in selection
begin
  SnipList.Clear;
  for Idx := 0 to Pred(fSelection.Count) do
  begin
    if Cat.Snippets.Contains(fSelection[Idx]) then
      SnipList.Add(fSelection[Idx]);
  end;
end;

function TQuery.GetLatestSearch: ISearch;
  {Gets reference to latest search object.
    @return Required search object.
  }
begin
  if fActiveSearches.Count > 0 then
    Result := fActiveSearches.Last
  else
    Result := TSearchFactory.CreateNulSearch;
end;

class function TQuery.GetInstance: IQuery;
  {Gets singleton instance of class, creating it if necessary
    @return Singleton instance.
  }
begin
  Result := TQuery.Create;  // OK since multiple calls return same instance
end;

function TQuery.GetSelection: TSnippetList;
  {Gets reference to list of snippets selected by last search.
    @return Reference to required list of snippets.
  }
begin
  Result := fSelection;
end;

procedure TQuery.Initialize;
begin
  inherited;
  fSelection := TSnippetList.Create;
  fActiveSearches := TList<ISearch>.Create;
  Reset;
end;

function TQuery.IsSearchActive: Boolean;
begin
  // Search is active if there is one or more in active search list
  Result := fActiveSearches.Count > 0;
end;

function TQuery.Refresh: Boolean;
  {Re-applies the latest search if one exists.
    @return True if search was re-applied, False if there was no search to
      apply.
  }
var
  Search: ISearch;
begin
  if not IsSearchActive then
    Exit(False);
  fSelection.Assign(Database.Snippets);
  for Search in fActiveSearches do
    RunSearch(Search);
  Result := True;
end;

procedure TQuery.Reset;
  {Resets query. Selection property is set to all snippets in database and
  Search property is set to nul search.
  }
begin
  fSelection.Assign(Database.Snippets);
  fActiveSearches.Clear;
end;

function TQuery.RunSearch(Search: ISearch): Boolean;
var
  FoundList: TSnippetList;
begin
  FoundList := TSnippetList.Create;
  try
    Result := Search.Execute(fSelection, FoundList);
    if not Result then
      Exit;
    fSelection.Assign(FoundList);
  finally
    FoundList.Free;
  end;
end;

procedure TQuery.Update;
  {Updates query. Attempts to refresh query and, if that fails, resets it.
  }
begin
  if not Refresh then
    Reset;
end;

end.

