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
 * Defines classes that group snippets together in different ways.
}


unit UGroups;


interface


uses
  // Delphi
  Generics.Collections,
  // 3rd party
  Collections.Base,
  Collections.Lists,
  // Project
  DB.UCategory, DB.USnippet, DB.USnippetKind, UInitialLetter;


type

  {
  TGroupItem:
    Abstract base class for classes that implement group "headings" within a
    grouping. Group items contain zero or more snippets sorted by name.
  }
  TGroupItem = class abstract(TObject)
  strict private
    type
      ///  <summary>Implements a sorted list of snippets.</summary>
      ///  <remarks>Using linked list because only operations on list are add
      ///  and enumeration: both are fast with linked list.</remarks>
      TSortedSnippetList = TObjectSortedLinkedList<TSnippet>;
    var
      ///  <summary>List of snippets belonging to group, sorted on display name.
      ///  </summary>
      fSnippetList: TSortedSnippetList;
  strict protected
    function GetTitle: string; virtual; abstract;
      {Read accessor for Title property.
        @return Required title.
      }
  public
    constructor Create;
      {Object constructor. Sets up object.
      }
    destructor Destroy; override;
      {Object destructor. Tears down object.
      }
    procedure AddSnippet(const Snippet: TSnippet);
      {Adds a snippet to the group's list.
        @param Snippet [in] Snippet to add.
      }
    function IsEmpty: Boolean;
      {Tests if snippet list is empty.
        @return True if list is empty, False if not.
      }
    function CompareTo(const Item: TGroupItem): Integer; virtual; abstract;
      {Compares this group item against another.
        @param Item [in] Group item to compare against.
        @return -ve if this item sorts before Item, 0 if same and +ve if this
          item sorts after Item.
      }
    property SnippetList: TSortedSnippetList read fSnippetList;
      {Sorted list of snippets associated with this group}
    property Title: string read GetTitle;
      {Title of group. Used for display}
  end;

  {
  TCategoryGroupItem:
    Defines a group heading for snippets organised by category. Contains all
    snippets that fall in a specified category.
  }
  TCategoryGroupItem = class(TGroupItem)
  strict private
    var fCategory: TCategory; // Snippet category associated with group
  strict protected
    function GetTitle: string; override;
      {Gets group title from category description.
        @return Required title.
      }
  public
    constructor Create(const Category: TCategory);
      {Object constructor. Sets up group for a category.
        @param Category [in] Category represented by the group.
      }
    function CompareTo(const Item: TGroupItem): Integer; override;
      {Compares this group item against another. Comparison is alphabetic and
      case insensitive based on associated category description. Categories with
      same description from main database sort before user defined versions.
        @param Item [in] Group item to compare against. Must be
          TCategoryGroupItem.
        @return -ve if this item sorts before Item, 0 if same and +ve if this
          item sorts after Item.
      }
    property Category: TCategory read fCategory;
      {Category represented by this group}
  end;

  {
  TAlphaGroupItem:
    Defines a group heading for snippets organised by initial letter of name.
    Contains all snippets sharing a specified initial letter.
  }
  TAlphaGroupItem = class(TGroupItem)
  strict private
    var fLetter: TInitialLetter; // Letter associated with group
  strict protected
    function GetTitle: string; override;
      {Gets group title as a letter.
        @return Required title.
      }
  public
    constructor Create(const Letter: TInitialLetter);
      {Object constructor. Sets up group for an initial letter of a snippet.
        @param Letter [in] Initial letter represented by the group.
      }
    function CompareTo(const Item: TGroupItem): Integer; override;
      {Compares this group item against another. Comparison is alphabetic based
      on the represented intial letters.
        @param Item [in] Group item to compare against. Must be TAlphaGroupItem.
        @return -ve if this item sorts before Item, 0 if same and +ve if this
          item sorts after Item.
      }
    property Letter: TInitialLetter read fLetter;
      {Initial letter of snippet represented by this group}
  end;

  {
  TSnipKindGroupItem:
    Defines a group heading for snippets organised by kind. Contains all
    snippets of a specified kind.
  }
  TSnipKindGroupItem = class(TGroupItem)
  strict private
    var fSnipKindInfo: TSnippetKindInfo;  // Snippet kind associated with group
  strict protected
    function GetTitle: string; override;
      {Gets group title from snippet kind description.
        @return Required title.
      }
  public
    constructor Create(const SnipKindInfo: TSnippetKindInfo);
      {Object constructor. Sets up group for a kind of snippet.
        @param SnippetKindInfo [in] Information about snippet kind represented
          by the group.
      }
    function CompareTo(const Item: TGroupItem): Integer; override;
      {Compares this group item against another. Comparison is alphabetic and
      case insensitive based on group titles.
        @param Item [in] Group item to compare against. Must be
          TSnipKindGroupItem.
        @return -ve if this item sorts before Item, 0 if same and +ve if this
          item sorts after Item.
      }
    property SnipKindInfo: TSnippetKindInfo read fSnipKindInfo;
      {Kind of snippet represented by this group}
  end;

  {
  TGrouping:
    Abstract base class for objects that group snippets in various ways.
    Groupings divide snippets amongst various "headings".
  }
  TGrouping = class abstract(TObject)
  strict private
    type
      // Sorted list of group item objects
      // Using linked list here because adding is faster than array base list
      TGroupItemList = TObjectSortedLinkedList<TGroupItem>;
    var fItems: TGroupItemList;     // List of items
    var fSnippetList: TSnippetList; // List of snippets to be grouped
    function GetCount: Integer;
      {Read accessor for Count property.
        @return Number of group items in grouping.
      }
  strict protected
    procedure AddItem(const Item: TGroupItem);
      {Adds a group item to grouping.
        @param Item [in] Group item to be added. Must be unique within grouping.
      }
    procedure Populate; virtual; abstract;
      {Populates grouping with group items and associated snippets.
      }
    property SnippetList: TSnippetList read fSnippetList;
      {List of snippets to be grouped}
  public
    constructor Create(const SnippetList: TSnippetList);
      {Object constructor. Sets up grouping object for a snippet list.
        @param SnippetList [in] List of snippets to be grouped.
      }
    destructor Destroy; override;
      {Object destructor. Tears down object.
      }
    function GetEnumerator: IEnumerator<TGroupItem>;
      {Creates an enumerator for this object.
        @return Reference to new enumerator. Caller is responsible for freeing
          this object.
      }
    property Count: Integer read GetCount;
      {Number of group items in grouping}
  end;

  {
  TCategoryGrouping:
    Class that groups snippets by category. Categories are sorted by
    description.
  }
  TCategoryGrouping = class(TGrouping)
  strict protected
    procedure Populate; override;
      {Populates grouping with sorted category group items and associated
      snippets.
      }
  end;

  {
  TAlphaGrouping:
    Class that groups snippets by initial letter of snippet name. Groups are
    sorted by initial letter.
  }
  TAlphaGrouping = class(TGrouping)
  strict private
    type
      // Sorted map of letter objects onto group items
      TLetterGroupMap = TObjectDictionary<TInitialLetter,TGroupItem>;
  strict protected
    procedure Populate; override;
      {Populates grouping with sorted alphabetic group items and associated
      snippets.
      }
  end;

  {
  TSnipKindGrouping:
    Class that groups snippets by kind. Groups are sorted by snippet kind
    description.
  }
  TSnipKindGrouping = class(TGrouping)
  strict protected
    procedure Populate; override;
      {Populates grouping with sorted snippet kind group items and associated
      snippets.
      }
  end;


implementation


uses
  // Delphi
  Generics.Defaults,
  // Project
  DB.UMain, UStrUtils;


{ TGrouping }

procedure TGrouping.AddItem(const Item: TGroupItem);
  {Adds a group item to grouping.
    @param Item [in] Group item to be added. Must be unique within grouping.
  }
begin
  fItems.Add(Item);
end;

constructor TGrouping.Create(const SnippetList: TSnippetList);
  {Object constructor. Sets up grouping object for a snippet list.
    @param SnippetList [in] List of snippets to be grouped.
  }
begin
  inherited Create;
  fSnippetList := SnippetList;
  fItems := TGroupItemList.Create(
    TRules<TGroupItem>.Create(
      TDelegatedComparer<TGroupItem>.Create(
        function (const Left, Right: TGroupItem): Integer
        begin
          Result := Left.CompareTo(Right);
        end
      ),
      TDelegatedEqualityComparer<TGroupItem>.Create(
        function (const Left, Right: TGroupItem): Boolean
        begin
          Result := Left.CompareTo(Right) = 0;
        end,
        function (const GroupItem: TGroupItem): Integer
        begin
          Result := Integer(GroupItem);
        end
      )
    )
  );
  fItems.OwnsObjects := True;
  Populate;
end;

destructor TGrouping.Destroy;
  {Object destructor. Tears down object.
  }
begin
  fItems.Free;  // frees owned objects
  inherited;
end;

function TGrouping.GetCount: Integer;
  {Read accessor for Count property.
    @return Number of group items in grouping.
  }
begin
  Result := fItems.Count;
end;

function TGrouping.GetEnumerator: IEnumerator<TGroupItem>;
  {Creates an enumerator for this object.
    @return Reference to new enumerator. Caller is responsible for freeing this
      object.
  }
begin
  Result := fItems.GetEnumerator;
end;

{ TGroupItem }

procedure TGroupItem.AddSnippet(const Snippet: TSnippet);
  {Adds a snippet to the group's list.
    @param Snippet [in] Snippet to add.
  }
begin
  Assert(Assigned(Snippet), ClassName + '.AddSnippet: Snippet is nil');
  fSnippetList.Add(Snippet);
end;

constructor TGroupItem.Create;
  {Object constructor. Sets up object.
  }
begin
  inherited Create;
  fSnippetList := TSortedSnippetList.Create(
    TRules<TSnippet>.Create(
      TSnippet.TDisplayNameComparer.Create,
      TSnippet.TDisplayNameEqualityComparer.Create
    )
  );
  fSnippetList.OwnsObjects := False;
end;

destructor TGroupItem.Destroy;
  {Object destructor. Tears down object.
  }
begin
  fSnippetList.Free;
  inherited;
end;

function TGroupItem.IsEmpty: Boolean;
  {Tests if snippet list is empty.
    @return True if list is empty, False if not.
  }
begin
  Result := SnippetList.Empty;
end;

{ TCategoryGrouping }

procedure TCategoryGrouping.Populate;
  {Populates grouping with sorted category group items and associated snippets.
  }
var
  Cat: TCategory;           // each category in databases
  Snippet: TSnippet;        // each snippet in a category
  Item: TCategoryGroupItem; // group item for each category
begin
  for Cat in Database.Categories do
  begin
    Item := TCategoryGroupItem.Create(Cat);
    AddItem(Item);
    for Snippet in Cat.Snippets do
      if SnippetList.Contains(Snippet) then
        Item.AddSnippet(Snippet);
  end;
end;

{ TCategoryGroupItem }

function TCategoryGroupItem.CompareTo(const Item: TGroupItem): Integer;
  {Compares this group item against another. Comparison is alphabetic and case
  insensitive based on associated category description. Categories with same
  description from main database sort before user defined versions.
    @param Item [in] Group item to compare against. Must be TCategoryGroupItem.
    @return -ve if this item sorts before Item, 0 if same and +ve if this
      item sorts after Item.
  }
var
  ItemCat: TCategory; // category which Item represents
begin
  ItemCat := (Item as TCategoryGroupItem).fCategory;
  Result := fCategory.CompareDescriptionTo(ItemCat);
end;

constructor TCategoryGroupItem.Create(const Category: TCategory);
  {Object constructor. Sets up group for a category.
    @param Category [in] Category represented by the group.
  }
begin
  inherited Create;
  fCategory := Category;
end;

function TCategoryGroupItem.GetTitle: string;
  {Gets group title from category description.
    @return Required title.
  }
begin
  Result := fCategory.Description;
end;

{ TAlphaGrouping }

procedure TAlphaGrouping.Populate;
  {Populates grouping with sorted alphabetic group items and associated
  snippets.
  }
  // ---------------------------------------------------------------------------
  function FirstCharOfName(const Name: string): Char;
    {Gets the first character of a name.
      @param Name [in] Name for which first character required.
      @return Required character in upper case.
    }
  begin
    Assert(Name <> '', ClassName + '.Populate:FirstCharOfName: Name is empty');
    Result := Name[1];
  end;
  // ---------------------------------------------------------------------------
var
  Letter: TInitialLetter;   // upper case initial letter of snippet name
  GroupItem: TGroupItem;    // a group item
  Snippet: TSnippet;        // each snippet in snippet list
  Map: TLetterGroupMap;     // map of initial letters to group items
begin
  Map := TLetterGroupMap.Create(
    [],
    TDelegatedEqualityComparer<TInitialLetter>.Create(
      function (const Left, Right: TInitialLetter): Boolean
      begin
        Result := Left = Right;
      end,
      function (const Letter: TInitialLetter): Integer
      begin
        Result := Ord(Letter.Letter);
      end
    )
  );
  // NOTE: We have to read all snippets in database to get all possible initial
  // letters in case user wants to display empty letter groups. We then add
  // only those snippets in given snippet list to the grouping.
  try
    for Snippet in Database._Snippets do
    begin
      Letter := TInitialLetter.Create(FirstCharOfName(Snippet.DisplayName));
      if Map.ContainsKey(Letter) then
        GroupItem := Map[Letter]
      else
      begin
        GroupItem := TAlphaGroupItem.Create(Letter);
        AddItem(GroupItem);
        Map.Add(Letter, GroupItem);
      end;
      if SnippetList.Contains(Snippet) then
        GroupItem.AddSnippet(Snippet);
    end;
  finally
    Map.Free;
  end;
end;

{ TAlphaGroupItem }

function TAlphaGroupItem.CompareTo(const Item: TGroupItem): Integer;
  {Compares this group item against another. Comparison is alphabetic based on
  the represented intial letters.
    @param Item [in] Group item to compare against. Must be TAlphaGroupItem.
    @return -ve if this item sorts before Item, 0 if same and +ve if this
      item sorts after Item.
  }
begin
  Result := TInitialLetter.Compare(fLetter, (Item as TAlphaGroupItem).fLetter);
end;

constructor TAlphaGroupItem.Create(const Letter: TInitialLetter);
  {Object constructor. Sets up group for an initial letter of a snippet.
    @param Letter [in] Initial letter represented by the group.
  }
begin
  inherited Create;
  fLetter := Letter;
end;

function TAlphaGroupItem.GetTitle: string;
  {Gets group title as a letter.
    @return Required title.
  }
begin
  Result := fLetter.Letter;
end;

{ TSnipKindGrouping }

procedure TSnipKindGrouping.Populate;
  {Populates grouping with sorted snippet kind group items and associated
  snippets.
  }
var
  SnipKind: TSnippetKind;           // each snippet kind
  Item: TGroupItem;                 // group item for each snippet kind
  Snippet: TSnippet;                // each snippet to be grouped
  Lookup: array[TSnippetKind]
    of TGroupItem;                  // lookup table of group kinds for searching
begin
  // Create all group items, one for each snippet kind along with a lookup table
  // to ease finding the required group item from a snippet's kind
  for SnipKind := Low(TSnippetKind) to High(TSnippetKind) do
  begin
    Item := TSnipKindGroupItem.Create(TSnippetKindInfoList.Items[SnipKind]);
    AddItem(Item);
    Lookup[SnipKind] := Item;
  end;
  // Add each snippet to required group
  for Snippet in SnippetList do
  begin
    // find group item and add snippet to it
    Item := Lookup[Snippet.Kind];
    Assert(Assigned(Item), ClassName + '.Populate: Item not found');
    Item.AddSnippet(Snippet);
  end;
end;

{ TSnipKindGroupItem }

function TSnipKindGroupItem.CompareTo(const Item: TGroupItem): Integer;
  {Compares this group item against another. Comparison is alphabetic and case
  insensitive based on group titles.
    @param Item [in] Group item to compare against. Must be TSnipKindGroupItem.
    @return -ve if this item sorts before Item, 0 if same and +ve if this
      item sorts after Item.
  }
begin
  Result := StrCompareText(GetTitle, (Item as TSnipKindGroupItem).GetTitle);
end;

constructor TSnipKindGroupItem.Create(const SnipKindInfo: TSnippetKindInfo);
  {Object constructor. Sets up group for a kind of snippet.
    @param SnippetKindInfo [in] Information about snippet kind represented by
      the group.
  }
begin
  inherited Create;
  fSnipKindInfo := SnipKindInfo;
end;

function TSnipKindGroupItem.GetTitle: string;
  {Gets group title from snippet kind description.
    @return Required title.
  }
begin
  Result := fSnipKindInfo.DisplayName;
end;

end.

