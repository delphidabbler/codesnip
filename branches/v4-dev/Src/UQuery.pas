{
 * UQuery.pas
 *
 * Defines a singleton object that encapsulates a query on the code snippets
 * database.
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
 * The Original Code is UQuery.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UQuery;


interface


uses
  // Project
  DB.USnippet, USearch, USnippets;


type

  {
  IQuery:
    Interface to object that encapsulates a query on the code snippets database.
    It enables a search to be run against the database and provides access to
    the selected snippets.
  }
  IQuery = interface(IInterface)
    ['{10998D72-CD5D-482B-9626-D771A50C53BA}']
    function ApplySearch(const Search: ISearch): Boolean;
      {Runs query by applying a search to the whole database. If search succeeds
      matching snippets and search are stored in query's Selection and Search
      properties. If search fails Selection and Search are left unchanged.
        @param Search [in] Search to apply.
        @return True if search succeeds and False if it fails.
      }
    procedure Reset;
      {Resets query. Selection property is set to all snippets in database and
      Search property is set to nul search.
      }
    function Refresh: Boolean;
      {Re-applies the current search if one exists.
        @return True if search was re-applied, False if there was no search to
          apply.
      }
    function GetCurrentSearch: ISearch;
      {Gets value of CurrentSearch property.
        @return Search object used to generate current search.
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
    property CurrentSearch: ISearch read GetCurrentSearch;
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
  SysUtils,
  // Project
  UBaseObjects;


type

  {
  TQuery:
    Class that encapsulates a query on the code snippets database. It enables a
    search to be run against the database and makes the found snippets
    available. Must only be instantiated once as a singleton.
  }
  TQuery = class(TNoPublicConstructIntfObject,
    IQuery
  )
  strict private
    var
      fSelection: TSnippetList;   // List of snippets selected by current query
      fSearch: ISearch;           // Search object used by current query
    class var
      fInstance: IQuery;          // Singleton object instance of this class
    class function GetInstance: IQuery; static;
      {Gets singleton instance of class, creating it if necessary
        @return Singleton instance.
      }
  protected // do not make strict
    { IQuery methods }
    function ApplySearch(const Search: ISearch): Boolean;
      {Runs query by applying a search to the whole database. If search succeeds
      matching snippets and search are stored in query's Selection and Search
      properties. If search fails Selection and Search are left unchanged.
        @param Search [in] Search to apply.
        @return True if search succeeds and False if it fails.
      }
    function Refresh: Boolean;
      {Re-applies the current search if one exists.
        @return True if search was re-applied, False if there was no search to
          apply.
      }
    procedure Reset;
      {Resets query. Selection property is set to all snippets in database and
      Search property is set to nul search.
      }
    function GetCurrentSearch: ISearch;
      {Gets reference to current search object.
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
    constructor InternalCreate;
      {Internal class constructor. Sets up object with all snippets in database
      selected.
      }
  public
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
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

function TQuery.ApplySearch(const Search: ISearch): Boolean;
  {Runs query by applying a search to the whole database. If search succeeds
  matching snippets and search are stored in query's Selection and Search
  properties. If search fails Selection and Search are left unchanged.
    @param Search [in] Search to apply.
    @return True if search succeeds and False if it fails.
  }
var
  FoundList: TSnippetList;  // list receives found snippets
begin
  Assert(Assigned(Search), ClassName + '.ApplySearch: Search is nil');
  FoundList := TSnippetList.Create;
  try
    // Get list of snippets that match search
    // if there are no snippets found we leave current selection alone
    Result := (Search as ISearch).Execute(Database.Snippets, FoundList);
    if Result then
    begin
      // Search succeeded: record search and list of snippets
      fSearch := Search;
      fSelection.Assign(FoundList);
    end;
  finally
    FreeAndNil(FoundList);
  end;
end;

destructor TQuery.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fSelection);
  fSearch := nil;
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

function TQuery.GetCurrentSearch: ISearch;
  {Gets reference to current search object.
    @return Required search object.
  }
begin
  Result := fSearch;
end;

class function TQuery.GetInstance: IQuery;
  {Gets singleton instance of class, creating it if necessary
    @return Singleton instance.
  }
begin
  if not Assigned(fInstance) then
    fInstance := TQuery.InternalCreate; // "TQuery" reference required here
  Result := fInstance;
end;

function TQuery.GetSelection: TSnippetList;
  {Gets reference to list of snippets selected by last search.
    @return Reference to required list of snippets.
  }
begin
  Result := fSelection;
end;

constructor TQuery.InternalCreate;
  {Internal class constructor. Sets up object with all snippets in database
  selected. Must only be called once.
  }
begin
  Assert(not Assigned(fInstance),
    ClassName + '.InternalCreate: Must only call once - singleton object');
  inherited InternalCreate;
  fSelection := TSnippetList.Create;
  Reset;
end;

function TQuery.Refresh: Boolean;
  {Re-applies the current search if one exists.
    @return True if search was re-applied, False if there was no search to
      apply.
  }
begin
  if Assigned(fSearch) then
    Result := ApplySearch(fSearch)
  else
    Result := False;
end;

procedure TQuery.Reset;
  {Resets query. Selection property is set to all snippets in database and
  Search property is set to nul search.
  }
begin
  fSelection.Assign(Database.Snippets);
  fSearch := TSearchFactory.CreateNulSearch;
end;

end.

