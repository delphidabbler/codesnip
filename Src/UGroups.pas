{
 * UGroups.pas
 *
 * Defines classes that group snippets together in different ways.
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
 * The Original Code is UGroups.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UGroups;


interface


uses
  // Project
  UAlphabet, UContainers, USnippetKindInfo, USnippets, Generics.Collections;


type

  {
  TGroupItem:
    Abstract base class for classes that implement group "headings" within a
    grouping. Group items contain zero or more snippets sorted by name.
  }
  TGroupItem = class abstract(TObject)
  strict private
    var fSnippetList: TRoutineList; // List of snippets in group
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
    procedure AddSnippet(const Snippet: TRoutine);
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
    property SnippetList: TRoutineList read fSnippetList;
      {List of snippets associated with this group}
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
    var fLetter: TLetter; // Letter associated with group
  strict protected
    function GetTitle: string; override;
      {Gets group title as a letter.
        @return Required title.
      }
  public
    constructor Create(const Letter: TLetter);
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
    property Letter: TLetter read fLetter;
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
      TGroupItemList = TSortedObjectList<TGroupItem>;
    var fItems: TGroupItemList;     // List of items
    var fSnippetList: TRoutineList; // List of snippets to be grouped
    function GetItem(Idx: Integer): TGroupItem;
      {Read accessor for Items[] property.
        @param Idx [in] Index of required object in list.
        @return Required object.
      }
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
    property SnippetList: TRoutineList read fSnippetList;
      {List of snippets to be grouped}
  public
    constructor Create(const SnippetList: TRoutineList);
      {Object constructor. Sets up grouping object for a snippet list.
        @param SnippetList [in] List of snippets to be grouped.
      }
    destructor Destroy; override;
      {Object destructor. Tears down object.
      }
    function GetEnumerator: TEnumerator<TGroupItem>;
      {Creates an enumerator for this object.
        @return Reference to new enumerator. Caller is responsible for freeing
          this object.
      }
    property Items[Idx: Integer]: TGroupItem read GetItem; default;
      {Indexed array of group items in grouping. Group items are sorted}
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
      TLetterGroupMap = TSortedObjectDictionary<TLetter,TGroupItem>;
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
  SysUtils, Windows {for inlining}, Character, Generics.Defaults;


{ TGrouping }

procedure TGrouping.AddItem(const Item: TGroupItem);
  {Adds a group item to grouping.
    @param Item [in] Group item to be added. Must be unique within grouping.
  }
begin
  fItems.Add(Item);
end;

constructor TGrouping.Create(const SnippetList: TRoutineList);
  {Object constructor. Sets up grouping object for a snippet list.
    @param SnippetList [in] List of snippets to be grouped.
  }
begin
  inherited Create;
  fSnippetList := SnippetList;
  fItems := TGroupItemList.Create(
    TDelegatedComparer<TGroupItem>.Create(
      function (const Left, Right: TGroupItem): Integer
      begin
        Result := Left.CompareTo(Right);
      end
    ),
    True
  );
  Populate;
end;

destructor TGrouping.Destroy;
  {Object destructor. Tears down object.
  }
begin
  FreeAndNil(fItems);   // frees owned objects
  inherited;
end;

function TGrouping.GetCount: Integer;
  {Read accessor for Count property.
    @return Number of group items in grouping.
  }
begin
  Result := fItems.Count;
end;

function TGrouping.GetEnumerator: TEnumerator<TGroupItem>;
  {Creates an enumerator for this object.
    @return Reference to new enumerator. Caller is responsible for freeing this
      object.
  }
begin
  Result := fItems.GetEnumerator;
end;

function TGrouping.GetItem(Idx: Integer): TGroupItem;
  {Read accessor for Items[] property.
    @param Idx [in] Index of required object in list.
    @return Required object.
  }
begin
  Result := fItems[Idx];
end;

{ TGroupItem }

procedure TGroupItem.AddSnippet(const Snippet: TRoutine);
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
  fSnippetList := TRoutineList.Create;
end;

destructor TGroupItem.Destroy;
  {Object destructor. Tears down object.
  }
begin
  FreeAndNil(fSnippetList);
  inherited;
end;

function TGroupItem.IsEmpty: Boolean;
  {Tests if snippet list is empty.
    @return True if list is empty, False if not.
  }
begin
  Result := SnippetList.Count = 0;
end;

{ TCategoryGrouping }

procedure TCategoryGrouping.Populate;
  {Populates grouping with sorted category group items and associated snippets.
  }
var
  Cat: TCategory;           // each category in databases
  Snippet: TRoutine;        // each snippet in a category
  Item: TCategoryGroupItem; // group item for each category
begin
  for Cat in Snippets.Categories do
  begin
    Item := TCategoryGroupItem.Create(Cat);
    AddItem(Item);
    for Snippet in Cat.Routines do
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
  Result := AnsiCompareText(fCategory.Description, ItemCat.Description);
  if Result = 0 then
    Result := Ord(fCategory.UserDefined) - Ord(ItemCat.UserDefined);
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
    Result := TCharacter.ToUpper(Name[1]);
    // must be 'A'..'Z' (not just any letter) or '_'
    Assert(CharInSet(Result, ['A'..'Z', '_']),
      ClassName +
        '.Populate:FirstCharOfName: Name must begin with A..Z or underscore');
  end;
  // ---------------------------------------------------------------------------
var
  LetterObj: TLetter;               // each letter object in alphabet object
  Item: TGroupItem;                 // found group item
  Snippet: TRoutine;                // each snippet to be grouped
  Lookup: TLetterGroupMap;  // lookup table of letters => group items
begin
  Lookup := TLetterGroupMap.Create(
    TDelegatedComparer<TLetter>.Create(
      function (const Left, Right: TLetter): Integer
      begin
        Result := Ord(Left.Letter) - Ord(Right.Letter);
      end
    ),
    []
  );
  try
    // Create all group items, one for each possible initial letter along with a
    // lookup table to ease finding the required group item from a snippet's
    // initial letter
    TAlphabet.Instance.InitEnum;
    while TAlphabet.Instance.NextLetter(LetterObj) do
    begin
      Item := TAlphaGroupItem.Create(LetterObj);
      AddItem(Item);
      Lookup.Add(LetterObj, Item);
    end;
    // Add each snippet to appropriate group
    for Snippet in SnippetList do
    begin
      // find group item from lookup
      Item := Lookup[
        TAlphabet.Instance.Letters[FirstCharOfName(Snippet.Name)]
      ];
      Assert(Assigned(Item), ClassName + '.Populate: Item not found');
      // add snippet to it
      Item.AddSnippet(Snippet);
    end;
  finally
    FreeAndNil(Lookup);
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
  Result := Ord(fLetter.Letter) - Ord((Item as TAlphaGroupItem).fLetter.Letter);
end;

constructor TAlphaGroupItem.Create(const Letter: TLetter);
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
  Snippet: TRoutine;                // each snippet to be grouped
  Lookup: array[TSnippetKind]
    of TGroupItem;                  // lookup table of group kinds for searching
begin
  // Create all group items, one for each snippet kind along with a lookup table
  // to ease finding the required group item from a snippet's kind
  for SnipKind := Low(TSnippetKind) to High(TSnippetKind) do
  begin
    Item := TSnipKindGroupItem.Create(TSnippetKindInfoList.Instance[SnipKind]);
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
  Result := AnsiCompareText(GetTitle, (Item as TSnipKindGroupItem).GetTitle);
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
  Result := fSnipKindInfo.Description;
end;

end.

