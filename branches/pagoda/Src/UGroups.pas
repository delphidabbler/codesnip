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
  Collections.MultiMaps,
  // Project
  CS.Database.Types,
  CS.SourceCode.Languages,
  UInitialLetter;


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
      TSortedSnippetList = TSortedLinkedList<ISnippet>;
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
    procedure AddSnippet(Snippet: ISnippet);
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

  ///  <summary>Defines a group heading for snippets organised by tag. Contains
  ///  all snippets that use the associated tag. In a special case the group may
  ///  contain snippets that have no tag.</summary>
  TTagGroupItem = class(TGroupItem)
  strict private
    var
      ///  <summary>Read accessor for Tag property.</summary>
      fTag: TTag;
  strict protected
    ///  <summary>Gets group title from tag's name.</summary>
    function GetTitle: string; override;
  public
    ///  <summary>Constructs a group item instance associated with the given
    ///  tag.</summary>
    ///  <remarks>If Tag is null then the group item is considered to represent
    ///  the special "no tags" group used for snippets with no tags.</remarks>
    constructor Create(const ATag: TTag);
    ///  <summary>Compares this group item against another. Comparison is
    ///  alphabetic and case insensitive based on associated tag name.</summary>
    ///  <param name="Item">TGroupItem [in] Group item to be compared against.
    ///  This must be a TTagGroupItem instance.</param>
    ///  <returns>Integer. -ve if this item sorts before Item, 0 if same and +ve
    ///  if this item sorts after Item.</returns>
    function CompareTo(const Item: TGroupItem): Integer; override;
    ///  <summary>Tag associated with group item.</summary>
    property Tag: TTag read fTag;
  end;

  ///  <summary>Defines a group heading for snippets organised by source code
  ///  language. Contains all snippets that use the associated language.
  ///  </summary>
  TSourceCodeLanguageGroupItem = class(TGroupItem)
  strict private
    var
      ///  <summary>Read accessor of language property.</summary>
      fLanguage: TSourceCodeLanguage;
  strict protected
    ///  <summary>Gets group title from language name.</summary>
    function GetTitle: string; override;
  public
    ///  <summary>Constructs a group item instance associated with the given
    ///  language.</summary>
    constructor Create(const ALanguage: TSourceCodeLanguage);
    ///  <summary>Compares this group item against another. Comparison is
    ///  alphabetic and case insensitive based on the associated language's
    ///  "friendly name".</summary>
    ///  <param name="Item">TGroupItem [in] Group item to be compared against.
    ///  This must be a TSourceCodeLanguageGroupItem instance.</param>
    ///  <returns>Integer. -ve if this item sorts before Item, 0 if same and +ve
    ///  if this item sorts after Item.</returns>
    function CompareTo(const Item: TGroupItem): Integer; override;
    ///  <summary>Source code language associated with group item.</summary>
    property Language: TSourceCodeLanguage read fLanguage;
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
    var
      fSnippetKind: TSnippetKind;  // Snippet kind associated with group
  strict protected
    function GetTitle: string; override;
      {Gets group title from snippet kind description.
        @return Required title.
      }
  public
    constructor Create(const ASnippetKind: TSnippetKind);
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
    property SnippetKind: TSnippetKind read fSnippetKind;
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
    var
      fItems: TGroupItemList;         // List of items
      fSnippetIDList: ISnippetIDList; // List of snippets to be grouped
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
    property SnippetIDList: ISnippetIDList read fSnippetIDList;
      {List of snippets to be grouped}
  public
    constructor Create(ASnippetList: ISnippetIDList);
      {Object constructor. Sets up grouping object for a snippet list.
        @param ASnippetList [in] List of snippets to be grouped.
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

  ///  <summary>Class that groups snippets by tag. Groups are sorted by tag
  ///  except that the special "no tags" group sorts first.</summary>
  ///  <remarks>The "no tags" group is used for snippets that have no tags.
  ///  </remarks>
  TTagGrouping = class(TGrouping)
  strict protected
    ///  <summary>Populates grouping with sorted tag group items and associated
    ///  snippets. Also adds a "no tags" group used for snippets which have no
    ///  tag.</summary>
    procedure Populate; override;
  end;

  ///  <summary>Class that groups snippets by source code language.</summary>
  TSourceCodeLanguageGrouping = class(TGrouping)
  strict protected
    ///  <summary>Populates grouping with sorted source code language group
    ///  items and associated snippets.</summary>
    procedure Populate; override;
  end;

  {
  TAlphaGrouping:
    Class that groups snippets by initial letter of snippet name. Groups are
    sorted by initial letter.
  }
  TAlphaGrouping = class(TGrouping)
  strict private
    type
      // Map of letter objects onto group items
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
  CS.Config,
  CS.Database.Snippets,
  DB.UMain,
  UComparers,
  UStrUtils;


{ TGrouping }

procedure TGrouping.AddItem(const Item: TGroupItem);
  {Adds a group item to grouping.
    @param Item [in] Group item to be added. Must be unique within grouping.
  }
begin
  fItems.Add(Item);
end;

constructor TGrouping.Create(ASnippetList: ISnippetIDList);
  {Object constructor. Sets up grouping object for a snippet list.
    @param ASnippetList [in] List of snippets to be grouped.
  }
begin
  inherited Create;
  fSnippetIDList := ASnippetList;
  fItems := TGroupItemList.Create(
    TRulesFactory<TGroupItem>.Construct(
      function (const Left, Right: TGroupItem): Integer
      begin
        Result := Left.CompareTo(Right);
      end,
      function (const GroupItem: TGroupItem): Integer
      begin
        Result := Integer(GroupItem);
      end
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

procedure TGroupItem.AddSnippet(Snippet: ISnippet);
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
    TRulesFactory<ISnippet>.CreateFromComparator(TSnippetTitleComparator.Create)
  );
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

{ TTagGrouping }

procedure TTagGrouping.Populate;

  procedure AddGroupItem(const ATag: TTag; SnippetIDs: ISnippetIDList);
  var
    GroupItem: TTagGroupItem;
    SnippetID: TSnippetID;
    Snippet: ISnippet;
  begin
    GroupItem := TTagGroupItem.Create(ATag);
    for SnippetID in SnippetIDs do
    begin
      Snippet := Database.LookupSnippet(SnippetID);
      GroupItem.AddSnippet(Snippet);
    end;
    AddItem(GroupItem);
  end;

var
  SelectedSnippetIDs: ISnippetIDList;
  AllTags: ITagSet;
  Tag: TTag;
begin
  SelectedSnippetIDs := Database.SelectSnippets(
    function (Snippet: ISnippet): Boolean
    begin
      Result := Snippet.Tags.IsEmpty
        and SnippetIDList.Contains(Snippet.ID);
    end
  );
  AddGroupItem(TTag.CreateNull, SelectedSnippetIDs);
  AllTags := Database.GetAllTags;
  for Tag in AllTags do
  begin
    SelectedSnippetIDs := Database.SelectSnippets(
      function (Snippet: ISnippet): Boolean
      begin
        Result := Snippet.Tags.Contains(Tag) and
          SnippetIDList.Contains(Snippet.ID);
      end
    );
    AddGroupItem(Tag, SelectedSnippetIDs);
  end;
end;

{ TTagGroupItem }

function TTagGroupItem.CompareTo(const Item: TGroupItem): Integer;
var
  ItemTag: TTag;
begin
  ItemTag := (Item as TTagGroupItem).fTag;
  Result := TTag.Compare(fTag, ItemTag);
end;

constructor TTagGroupItem.Create(const ATag: TTag);
begin
  inherited Create;
  fTag := ATag;
end;

function TTagGroupItem.GetTitle: string;
begin
  Result := fTag.ToString;
end;

{ TSourceCodeLanguageGrouping }

procedure TSourceCodeLanguageGrouping.Populate;
var
  Lang: TSourceCodeLanguage;
  GroupItem: TGroupItem;    // a group item
  SnippetID: TSnippetID;    // each snippet ID in database
  SelectedSnippetIDs: ISnippetIDList;
  Snippet: ISnippet;        // each snippet in database
begin
  for Lang in TConfig.Instance.SourceCodeLanguages do
  begin
    SelectedSnippetIDs := Database.SelectSnippets(
      function (Snippet: ISnippet): Boolean
      begin
        Result := (Snippet.LanguageID = Lang.ID) and
          SnippetIDList.Contains(Snippet.ID);
      end
    );
    GroupItem := TSourceCodeLanguageGroupItem.Create(Lang);
    for SnippetID in SelectedSnippetIDs do
    begin
      Snippet := Database.LookupSnippet(SnippetID);
      GroupItem.AddSnippet(Snippet);
    end;
    AddItem(GroupItem);
  end;
end;

{ TSourceCodeLanguageGroupItem }

function TSourceCodeLanguageGroupItem.CompareTo(const Item: TGroupItem):
  Integer;
var
  ThatLanguage: TSourceCodeLanguage;
begin
  ThatLanguage := (Item as TSourceCodeLanguageGroupItem).fLanguage;
  Result := fLanguage.CompareFriendlyNameTo(ThatLanguage);
end;

constructor TSourceCodeLanguageGroupItem.Create(
  const ALanguage: TSourceCodeLanguage);
begin
  inherited Create;
  fLanguage := ALanguage;
end;

function TSourceCodeLanguageGroupItem.GetTitle: string;
begin
  Result := fLanguage.FriendlyName;
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
  SnippetID: TSnippetID;    // each snippet ID in database
  Snippet: ISnippet;        // each snippet in database
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
    for SnippetID in Database.GetAllSnippets do
    begin
      Snippet := Database.LookupSnippet(SnippetID);
      Letter := TInitialLetter.Create(FirstCharOfName(Snippet.Title));
      if Map.ContainsKey(Letter) then
        GroupItem := Map[Letter]
      else
      begin
        GroupItem := TAlphaGroupItem.Create(Letter);
        AddItem(GroupItem);
        Map.Add(Letter, GroupItem);
      end;
      if SnippetIDList.Contains(Snippet.ID) then
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
  SnippetKind: TSnippetKind;        // each snippet kind
  Item: TGroupItem;                 // group item for each snippet kind
  SnippetID: TSnippetID;            // each of each snippet to be grouped
  Snippet: ISnippet;                // each snippet to be grouped
  Lookup: array[TSnippetKindID]
    of TGroupItem;                  // lookup table of group kinds for searching
begin
  // Create all group items, one for each snippet kind along with a lookup table
  // to ease finding the required group item from a snippet's kind
  for SnippetKind in Database.GetAllSnippetKinds do
  begin
    Item := TSnipKindGroupItem.Create(SnippetKind);
    AddItem(Item);
    Lookup[SnippetKind.ID] := Item;
  end;
  // Add each snippet to required group
  for SnippetID in SnippetIDList do
  begin
    // find group item and add snippet to it
    Snippet := Database.LookupSnippet(SnippetID);
    Item := Lookup[Snippet.KindID];
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

constructor TSnipKindGroupItem.Create(const ASnippetKind: TSnippetKind);
  {Object constructor. Sets up group for a kind of snippet.
    @param SnippetKindInfo [in] Information about snippet kind represented by
      the group.
  }
begin
  inherited Create;
  fSnippetKind := ASnippetKind;
end;

function TSnipKindGroupItem.GetTitle: string;
  {Gets group title from snippet kind description.
    @return Required title.
  }
begin
  Result := fSnippetKind.DisplayName;
end;

end.

