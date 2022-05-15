{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2011-2022, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Objects, records etc that encapsulate a code snippet, its data and lists of
 * code snippets.
}


unit DB.USnippet;


interface


uses
  // Delphi
  Classes, Generics.Collections, Generics.Defaults,
  // Project
  ActiveText.UMain, Compilers.UGlobals, DB.USnippetKind, UContainers,
  UIStringList, USnippetIDs;

type
  ///  <summary>Enumeration providing information about the level to which a
  ///  snippet has been tested.</summary>
  TSnippetTestInfo = (
    stiNone,              // snippet has not been tested
    stiBasic,             // snippet has had some basic testing
    stiAdvanced           // snippet has had advanced (unit) testing
  );

type

  {
  TSnippetData:
    Record that provides information about a snippet's properties.
  }
  TSnippetData = record
    Kind: TSnippetKind;                   // Kind of snippet
    Cat: string;                          // Category containing snippet
    Desc: IActiveText;                    // Description of snippet
    SourceCode: string;                   // Snippet's source code
    HiliteSource: Boolean;                // If syntax highlighter to be used
    DisplayName: string;                  // Snippet's display name
    Extra: IActiveText;                   // Extra text used to describe snippet
    CompilerResults: TCompileResults;     // Compilation results
    TestInfo: TSnippetTestInfo;           // Test information
    procedure Init;
      {Initialises record by setting default values for fields.
      }
    procedure Assign(const Src: TSnippetData);
      {Sets this record to be a copy of another record.
        @param Src [in] Record to be copied.
      }
  end;

  {
  TSnippetReferences:
    Record that details a snippet's references
  }
  TSnippetReferences = record
    Units: IStringList;         // List of required units
    Depends: ISnippetIDList;    // List of IDs of required snippets
    XRef: ISnippetIDList;       // List of IDs of cross referenced snippets
    procedure Init;
      {Initialises record by creating objects stored in fields.
      }
    procedure Assign(const Src: TSnippetReferences);
      {Sets this record to be a copy of another record.
        @param Src [in] Record to be copied.
      }
  end;

  {
  TSnippetEditData:
    Provides deatils of a snippet's data that can be edited.
  }
  TSnippetEditData = record
    Props: TSnippetData;        // Snippet's editable properties
    Refs: TSnippetReferences;   // Snippet's editable referfences
    procedure Init;
      {Initialises record by creating default values and field objects.
      }
    procedure Assign(const Src: TSnippetEditData);
      {Sets this record to be a copy of another record.
        @param Src [in] Record to be copied.
      }
  end;

  TSnippetList = class;

  {
  TSnippet:
    Encapsulates a snippet from the database. Can be routine, type, constant or
    free-form.
  }
  TSnippet = class(TObject)
  public
    ///  <summary>Comparer for snippets by display name.</summary>
    type TDisplayNameComparer = class(TComparer<TSnippet>)
    public
      ///  <summary>Compares snippets Left and Right. Returns -ve if Left's
      ///  display name sorts before Right's, 0 if the same or +ve if Left's
      ///  display name is greater than Right's.</summary>
      function Compare(const Left, Right: TSnippet): Integer; override;
    end;
  strict private
    fKind: TSnippetKind;                    // Kind of snippet this is
    fCategory: string;                      // Name of snippet's category
    fDescription: IActiveText;              // Description of snippet
    fSourceCode: string;                    // Snippet's source code
    fName: string;                          // Name of snippet
    fDisplayName: string;                   // Display name of snippet
    fUnits: TStringList;                    // List of required units
    fDepends: TSnippetList;                 // List of required snippets
    fXRef: TSnippetList;                    // List of cross-referenced snippets
    fExtra: IActiveText;                    // Further information for snippet
    fCompatibility: TCompileResults;        // Snippet's compiler compatibility
    fUserDefined: Boolean;                  // If this snippet is user-defined
    fHiliteSource: Boolean;                 // If source is syntax highlighted
    fTestInfo: TSnippetTestInfo;            // Level of testing of snippet
    function GetID: TSnippetID;
      {Gets snippet's unique ID.
        @return Required ID.
      }
    function GetDisplayName: string;
      {Gets snippet's display name, or name if no display name is set
        @return Required display name.
      }
  strict protected
    procedure SetName(const Name: string);
      {Sets Name property.
        @param Name [in] New name.
      }
    procedure SetProps(const Data: TSnippetData);
      {Sets snippet's properties.
        @param Data [in] Record containing property values.
      }
    function GetDisplayNameValue: string;
      {Get's value of snippet's display name field.
        @return Required field content.
      }
  public
    constructor Create(const Name: string; const UserDefined: Boolean;
      const Props: TSnippetData);
      {Class contructor. Sets up snippet object with given property values.
        @param Name [in] Name of snippet.
        @param UserDefined [in] Indicates if this is a user defined snippet.
        @param Props [in] Values of various snippet properties.
      }
    destructor Destroy; override;
      {Destructor. Tears down object.
      }
    function IsEqual(const Snippet: TSnippet): Boolean;
      {Checks if this snippet is same as another snippet. Snippets are
      considered equal if they have the same name and come from same database.
        @param Snippet [in] Snippet being compared.
        @return True if snippets are equal, False if not.
      }
    function CanCompile: Boolean;
      {Checks if snippet can be compiled.
        @return True if compilation supported and False if not.
      }
    property Kind: TSnippetKind read fKind;
      {Kind of snippet represented by this object}
    property ID: TSnippetID read GetID;
      {Snippet's unique ID}
    property Name: string read fName;
      {Name of snippet}
    property DisplayName: string read GetDisplayName;
      {Displat name of snippet}
    property Category: string read fCategory;
      {Category to which snippet belongs}
    property Description: IActiveText read fDescription;
      {Description of snippet}
    property SourceCode: string read fSourceCode;
      {Source code of snippet}
    property HiliteSource: Boolean read fHiliteSource;
      {Flags whether source code can be syntax highlighted}
    property Extra: IActiveText read fExtra;
      {Additional information about snippet}
    property Compatibility: TCompileResults read fCompatibility;
      {Compiler compatibilty of this snippet}
    property TestInfo: TSnippetTestInfo read fTestInfo;
      {Describes level of testing carried out on snippet}
    property Units: TStringList read fUnits;
      {List of units used by snippet}
    property Depends: TSnippetList read fDepends;
      {List of any other snippet in database on which this snippet depends}
    property XRef: TSnippetList read fXRef;
      {List of cross referenced snippets in database}
    property UserDefined: Boolean read fUserDefined;
      {Flag that indicates if this is a user defined snippet}
  end;

  {
  TSnippetEx:
    Extension of TSnippet for use internally by Snippets object.
  }
  TSnippetEx = class(TSnippet)
  public
    procedure UpdateRefs(const Refs: TSnippetReferences;
      const AllSnippets: TSnippetList);
      {Updates a snippet's references.
        @param Refs [in] Stores all snippet's references (XRef, Depends and
          Units).
        @param AllSnippets [in] List of all snippets in database.
      }
    procedure Update(const Data: TSnippetEditData;
      const AllSnippets: TSnippetList);
      {Updates snippet's properties and references.
        @param Data [in] New property values and references.
        @param AllSnippets [in] List of all snippets in database.
      }
    function GetEditData: TSnippetEditData;
      {Gets details of all editable data of snippet.
        @return Required editable properties and references.
      }
    function GetProps: TSnippetData;
      {Gets details of snippet's properties.
        @return Record containing property values.
      }
    function GetReferences: TSnippetReferences;
      {Gets details of snippet's references.
        @return Information sufficient to define references.
      }
  end;

  {
  TTempSnippet:
    Special subclass of TSnippetEx that can't be added to the Snippets object.
    Class does nothing, simply provides a class name for testing when a snippet
    is added to a TSnippetListEx. TTempSnippet can be added to a normal snippet
    list.
  }
  TTempSnippet = class(TSnippetEx);

  {
  TSnippetList:
    Class that implements a list of TSnippet objects.
  }
  TSnippetList = class(TObject)
  strict private
    function GetItem(Idx: Integer): TSnippet;
      {Read accessor for Items property.
        @param Idx [in] Index of required snippet in list.
        @return Snippet at specified index in list.
      }
    function Find(const SnippetName: string;
      const UserDefined: Boolean; out Index: Integer): Boolean; overload;
      {Finds a snippet in the list that has a specified name and user defined
      property. Uses a binary search.
        @param SnippetName [in] Name of snippet to be found.
        @param UserDefined [in] Whether required snippet is user defined or not.
        @param Index [out] Index of required snippet in list. Valid only if
          method returns True.
        @return True if snippet found, False if not.
      }
  strict protected
    var fList: TSortedObjectList<TSnippet>; // Sorted list of snippets
  public
    constructor Create(const OwnsObjects: Boolean = False);
      {Constructor. Creates a new empty list.
        @param OwnsObjects [in] Specifies whether list owns the snippet objects
          it contains. If True the snippets are freed when deleted from list.
      }
    destructor Destroy; override;
      {Destructor. Tears down object.
      }
    procedure Assign(const SrcList: TSnippetList);
      {Sets this list to be same as another list. List items are referenced, not
      copied.
        @param SrcList [in] List of snippets to be assigned.
      }
    function IsEqual(const AList: TSnippetList): Boolean;
      {Checks if this list contains same snippets as another list.
        @param AList [in] List of snippets to compare.
        @return True if lists are same, False if not.
      }
    function Add(const Snippet: TSnippet): Integer; virtual;
      {Adds new snippet to the list, maintaining list in alphabetical order.
        @param Snippet [in] Snippet being added.
        @return Index where item was inserted in list
        @except Raised if duplicate snippet added to list.
      }
    function Find(const SnippetID: TSnippetID): TSnippet; overload;
      {Finds a specified snippet in list.
        @param SnippetID [in] ID of snippet to find.
        @return Reference to required snippet or nil if not found.
      }
    function Find(const SnippetName: string;
      const UserDefined: Boolean): TSnippet; overload;
      {Finds a named snippet in list with a matching user defined property.
        @param SnippetName [in] Name of required snippet.
        @param UserDefined [in] Flag that determines if we are looking for a
          user defined snippet or one from main database.
        @return Reference to required snippet or nil if not found.
      }
    function Contains(const Snippet: TSnippet): Boolean;
      {Checks whether list contains a specified snippet.
        @param Snippet [in] Required snippet.
        @return True if snippet is in list, False otherwise.
      }
    function ContainsKinds(const Kinds: TSnippetKinds): Boolean;
      {Checks if the list contains specified kinds of snippets.
        @param Kinds [in] Set of kinds to test for.
        @return True if list contains at least one snippet of any of the
          specified kinds.
      }
    procedure Clear;
      {Clears the list.
      }
    function GetEnumerator: TEnumerator<TSnippet>;
      {Gets an intialised snippet list enumerator.
        @return Required enumerator.
      }
    function Count(const UserDefined: Boolean): Integer; overload;
      {Counts number of snippets in list that are either from or not from user
      defined database.
        @param UserDefined [in] Flags whether to count snippets in user database
          (True) or in main database (False).
        @return Number of snippets in specified database.
      }
    function Count: Integer; overload;
      {Counts number of snippets in list.
        @return Number of snippets in list.
      }
    function IsEmpty: Boolean; overload; inline;
      {Checks if list is empty.
        @return True if list is empty, False otehrwise.
      }
    function IsEmpty(const UserDefined: Boolean): Boolean; overload; inline;
      {Checks if sub-set of list from either from or not from use defined
      database is empty.
        @param UserDefined [in] Flags whether to check for snippets in user
          database (True) or in main database (False).
        @return True if required subset is empty, False if not empty.
      }
    property Items[Idx: Integer]: TSnippet read GetItem; default;
      {List of snippets}
  end;

  {
  TSnippetListEx:
    Private extension of TSnippetList for use internally by Snippets object.
  }
  TSnippetListEx = class(TSnippetList)
  public
    function Add(const Snippet: TSnippet): Integer; override;
      {Adds a snippet to list. Snippet must not be TTempSnippet class.
        @param Snippet [in] Snippet to be added.
        @return Index where snippet was added to list.
      }
    procedure Delete(const Snippet: TSnippet);
      {Deletes a snippet from list.
        @param Snippet [in] Snippet to be deleted. No action taken if snippet
          not in list.
      }
  end;

  {
  TSnippetIDListEx:
    Extension of TSnippetIDList that provides an additional constructor that can
    create a snippet ID list from a TSnippetList.
  }
  TSnippetIDListEx = class(TSnippetIDList)
  public
    constructor Create(const SnipList: TSnippetList); overload;
      {Constructor overload that creates a snippets ID list from a
      TSnippetList object.
        @param SnipList [in] List of snippets objects for which ID list is
          required.
      }
  end;

implementation

uses
  // Delphi
  SysUtils,
  // Project
  IntfCommon, UExceptions, UStrUtils;

{ TSnippet }

function TSnippet.CanCompile: Boolean;
  {Checks if snippet can be compiled.
    @return True if compilation supported and False if not.
  }
begin
  Result := Kind <> skFreeform;
end;

constructor TSnippet.Create(const Name: string; const UserDefined: Boolean;
  const Props: TSnippetData);
  {Class contructor. Sets up snippet object with given property values.
    @param Name [in] Name of snippet.
    @param UserDefined [in] Indicates if this is a user defined snippet.
    @param Props [in] Values of various snippet properties.
  }
begin
  Assert(ClassType <> TSnippet,
    ClassName + '.Create: must only be called from descendants.');
  inherited Create;
  // Record simple property values
  SetName(Name);
  SetProps(Props);
  // Create string list to store required units
  fUnits := TStringList.Create;
  // Create snippets lists for Depends and XRef properties
  fDepends := TSnippetListEx.Create;
  fXRef := TSnippetListEx.Create;
  // The following properties added to support user defined snippets
  fUserDefined := UserDefined;
end;

destructor TSnippet.Destroy;
  {Destructor. Tears down object.
  }
begin
  FreeAndNil(fXRef);
  FreeAndNil(fDepends);
  FreeAndNil(fUnits);
  fExtra := nil;
  fDescription := nil;
  inherited;
end;

function TSnippet.GetDisplayName: string;
begin
  if GetDisplayNameValue <> '' then
    Result := GetDisplayNameValue
  else
    Result := fName;
end;

function TSnippet.GetDisplayNameValue: string;
begin
  Result := fDisplayName;
end;

function TSnippet.GetID: TSnippetID;
  {Gets snippet's unique ID.
    @return Required ID.
  }
begin
  Result := TSnippetID.Create(fName, fUserDefined);
end;

function TSnippet.IsEqual(const Snippet: TSnippet): Boolean;
  {Checks if this snippet is same as another snippet. Snippets are considered
  equal if they have the same name and come from same database.
    @param Snippet [in] Snippet being compared.
    @return True if snippets are equal, False if not.
  }
begin
  Result := Snippet.ID = Self.ID;
end;

procedure TSnippet.SetName(const Name: string);
  {Sets Name property.
    @param Name [in] New name.
  }
begin
  fName := Name;
end;

procedure TSnippet.SetProps(const Data: TSnippetData);
  {Sets snippet's properties.
    @param Data [in] Record containing property values.
  }
begin
  fCategory := Data.Cat;
  fKind := Data.Kind;
  fDescription := Data.Desc;
  fSourceCode := StrWindowsLineBreaks(Data.SourceCode);
  fHiliteSource := Data.HiliteSource;
  fDisplayName := Data.DisplayName;
  fExtra := TActiveTextFactory.CloneActiveText(Data.Extra);
  fCompatibility := Data.CompilerResults;
  fTestInfo := Data.TestInfo;
end;

{ TSnippet.TDisplayNameComparer }

function TSnippet.TDisplayNameComparer.Compare(const Left,
  Right: TSnippet): Integer;
begin
  Result := StrCompareText(Left.DisplayName, Right.DisplayName);
  if Result = 0 then
    Result := Left.ID.CompareTo(Right.ID);
end;

{ TSnippetEx }

function TSnippetEx.GetEditData: TSnippetEditData;
  {Gets details of all editable data of snippet.
    @return Required editable properties and references.
  }
begin
  Result.Props := GetProps;
  Result.Refs := GetReferences;
end;

function TSnippetEx.GetProps: TSnippetData;
  {Gets details of snippet's properties.
    @return Record containing property values.
  }
begin
  Result.Cat := Category;
  Result.Kind := Kind;
  Result.Desc := Description;
  Result.SourceCode := SourceCode;
  Result.HiliteSource := HiliteSource;
  Result.DisplayName := GetDisplayNameValue;
  Result.Extra := TActiveTextFactory.CloneActiveText(Extra);
  Result.CompilerResults := Compatibility;
  Result.TestInfo := TestInfo;
end;

function TSnippetEx.GetReferences: TSnippetReferences;
  {Gets details of snippet's references.
    @return Information sufficient to define references.
  }
begin
  Result.Units := TIStringList.Create(Units);
  Result.Depends := TSnippetIDListEx.Create(Depends);
  Result.XRef := TSnippetIDListEx.Create(XRef);
end;

procedure TSnippetEx.Update(const Data: TSnippetEditData;
  const AllSnippets: TSnippetList);
  {Updates snippet's properties and references.
    @param Data [in] New property values and references.
    @param AllSnippets [in] List of all snippets in database.
  }
begin
  SetProps(Data.Props);
  UpdateRefs(Data.Refs, AllSnippets);
end;

procedure TSnippetEx.UpdateRefs(const Refs: TSnippetReferences;
  const AllSnippets: TSnippetList);
  {Updates a snippet's references.
    @param Refs [in] Stores all snippet's references (XRef, Depends and
      Units).
    @param AllSnippets [in] List of all snippets in database.
  }

  // ---------------------------------------------------------------------------
  procedure BuildSnippetList(const SL: TSnippetList;
    const IDList: ISnippetIDList);
    {Creates a snippets list from a snippets ID list. Looks up snippets in list
    of all snippets in database. Any snippets in ID list that do not exist in
    database are ignored.
      @param SL [in] Snippets list object to be updated.
      @param IDList [in] Snippets ID list that provides information used to
        create snippets list.
    }
  var
    ID: TSnippetID;     // refers to each ID in ID list
    Snippet: TSnippet;  // references each snippet identified by ID
  begin
    SL.Clear;
    for ID in IDList do
    begin
      Snippet := AllSnippets.Find(ID);
      if Assigned(Snippet) then
        SL.Add(Snippet);
    end;
  end;
  // ---------------------------------------------------------------------------

begin
  Refs.Units.CopyTo(Self.Units, True);            // copy units
  BuildSnippetList(Self.Depends, Refs.Depends);   // build Depends list
  BuildSnippetList(Self.XRef, Refs.XRef);         // build XRef list
end;

{ TSnippetList }

function TSnippetList.Add(const Snippet: TSnippet): Integer;
  {Adds new snippet to the list, maintaining list in alphabetical order.
    @param Snippet [in] Snippet being added.
    @return Index where item was inserted in list
    @except Raised if duplicate snippet added to list.
  }
begin
  Result := fList.Add(Snippet);
end;

procedure TSnippetList.Assign(const SrcList: TSnippetList);
  {Sets this list to be same as another list. List items are referenced, not
  copied.
    @param SrcList [in] List of snippets to be assigned.
  }
var
  Idx: Integer; // loops thru source list
begin
  Assert(not fList.OwnsObjects,
    ClassName + '.Assign: can''t assign to master list');
  Self.Clear;
  if Assigned(SrcList) then
  begin
    for Idx := 0 to Pred(SrcList.Count) do
      Self.Add(SrcList[Idx]);
  end;
end;

procedure TSnippetList.Clear;
  {Clears the list.
  }
begin
  fList.Clear;
end;

function TSnippetList.Contains(const Snippet: TSnippet): Boolean;
  {Checks whether list contains a specified snippet.
    @param Snippet [in] Required snippet.
    @return True if snippet is in list, False otherwise.
  }
begin
  Result := fList.Contains(Snippet);
end;

function TSnippetList.ContainsKinds(const Kinds: TSnippetKinds): Boolean;
  {Checks if the list contains specified kinds of snippets.
    @param Kinds [in] Set of kinds to test for.
    @return True if list contains at least one snippet of any of the specified
      kinds.
  }
var
  Snippet: TSnippet;  // reference to all snippets in list
begin
  Result := False;
  for Snippet in Self do
    if Snippet.Kind in Kinds then
    begin
      Result := True;
      Break;
    end;
end;

function TSnippetList.Count(const UserDefined: Boolean): Integer;
  {Counts number of snippets in list that are either from or not from user
  defined database.
    @param UserDefined [in] Flags whether to count snippets in user database
      (True) or in main database (False).
    @return Number of snippets in specified database.
  }
var
  Snippet: TSnippet;  // refers to all snippets in list
begin
  Result := 0;
  for Snippet in Self do
    if Snippet.UserDefined = UserDefined then
      Inc(Result);
end;

function TSnippetList.Count: Integer;
  {Counts number of snippets in list.
    @return Number of snippets in list.
  }
begin
  Result := fList.Count;
end;

constructor TSnippetList.Create(const OwnsObjects: Boolean = False);
  {Constructor. Creates a new empty list.
    @param OwnsObjects [in] Specifies whether list owns the snippet objects it
      contains. If True the snippets are freed when deleted from list.
  }
begin
  inherited Create;
  fList := TSortedObjectList<TSnippet>.Create(
    TDelegatedComparer<TSnippet>.Create(
      function (const Left, Right: TSnippet): Integer
      begin
        Result := Left.ID.CompareTo(Right.ID);
      end
    ),
    OwnsObjects
  );
  fList.PermitDuplicates := False;
end;

destructor TSnippetList.Destroy;
  {Destructor. Tears down object.
  }
begin
  fList.Free; // destroys owned snippets if OwnsObjects=True
  inherited;
end;

function TSnippetList.Find(const SnippetName: string;
  const UserDefined: Boolean; out Index: Integer): Boolean;
  {Finds a snippet in the list that has a specified name and user defined
  property. Uses a binary search.
    @param SnippetName [in] Name of snippet to be found.
    @param UserDefined [in] Whether required snippet is user defined or not.
    @param Index [out] Index of required snippet in list. Valid only if
      method returns True.
    @return True if snippet found, False if not.
  }
var
  TempSnippet: TSnippet;  // temp snippet used to perform search
  NulData: TSnippetData;  // nul data used to create snippet
begin
  // We need a temporary snippet object in order to perform binary search using
  // object list's built in search
  NulData.Init;
  TempSnippet := TTempSnippet.Create(SnippetName, UserDefined, NulData);
  try
    Result := fList.Find(TempSnippet, Index);
  finally
    TempSnippet.Free;
  end;
end;

function TSnippetList.Find(const SnippetName: string;
  const UserDefined: Boolean): TSnippet;
  {Finds a named snippet in list with a matching user defined property.
    @param SnippetName [in] Name of required snippet.
    @param UserDefined [in] Flag that determines if we are looking for a
      user defined snippet or one from main database.
    @return Reference to required snippet or nil if not found.
  }
var
  Idx: Integer; // index of snippet name in list
begin
  if Find(SnippetName, UserDefined, Idx) then
    Result := Items[Idx]
  else
    Result := nil;
end;

function TSnippetList.Find(const SnippetID: TSnippetID): TSnippet;
  {Finds a specified snippet in list.
    @param SnippetID [in] ID of snippet to find.
    @return Reference to required snippet or nil if not found.
  }
begin
  Result := Find(SnippetID.Name, SnippetID.UserDefined);
end;

function TSnippetList.GetEnumerator: TEnumerator<TSnippet>;
  {Gets an intialised snippet list enumerator.
    @return Required enumerator.
  }
begin
  Result := fList.GetEnumerator;
end;

function TSnippetList.GetItem(Idx: Integer): TSnippet;
  {Read accessor for Items property.
    @param Idx [in] Index of required snippet in list.
    @return Snippet at specified index in list.
  }
begin
  Result := fList[Idx];
end;

function TSnippetList.IsEmpty: Boolean;
  {Checks if list is empty.
    @return True if list is empty, False otehrwise.
  }
begin
  Result := Count = 0;
end;

function TSnippetList.IsEmpty(const UserDefined: Boolean): Boolean;
begin
  Result := Count(UserDefined) = 0;
end;

function TSnippetList.IsEqual(const AList: TSnippetList): Boolean;
  {Checks if this list contains same snippets as another list.
    @param AList [in] List of snippets to compare.
    @return True if lists are same, False if not.
  }
var
  Idx: Integer; // loops thru all snippets in list
begin
  // To be same comparison list must exist and have same number of snippets
  Result := Assigned(AList) and (Self.Count = AList.Count);
  if Result then
  begin
    // Same number of snippets: scan list checking snippet names same. We can
    // rely on items being in same order since lists are sorted
    for Idx := 0 to Pred(Self.Count) do
    begin
      if not Self[Idx].IsEqual(AList[Idx]) then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

{ TSnippetListEx }

function TSnippetListEx.Add(const Snippet: TSnippet): Integer;
  {Adds a snippet to list. Snippet must not be TTempSnippet class.
    @param Snippet [in] Snippet to be added.
    @return Index where snippet was added to list.
  }
begin
  Assert(not(Snippet is TTempSnippet),
    ClassName + '.Add: Can''t add temporary snippets to database');
  Result := inherited Add(Snippet);
end;

procedure TSnippetListEx.Delete(const Snippet: TSnippet);
  {Deletes a snippet from list.
    @param Snippet [in] Snippet to be deleted. No action taken if snippet not in
      list.
  }
var
  Idx: Integer; // index of snippet in list.
begin
  Idx := fList.IndexOf(Snippet);
  if Idx = -1 then
    Exit;
  fList.Delete(Idx);  // this frees snippet if list owns objects
end;

{ TSnippetData }

procedure TSnippetData.Assign(const Src: TSnippetData);
  {Sets this record to be a copy of another record.
    @param Src [in] Record to be copied.
  }
begin
  Kind := Src.Kind;
  Cat := Src.Cat;
  Desc := TActiveTextFactory.CloneActiveText(Src.Desc);
  SourceCode := Src.SourceCode;
  HiliteSource := Src.HiliteSource;
  DisplayName := Src.DisplayName;
  // we use cloning for Extra below because it deals uccessfully with both
  // Self.Extra = nil and Src.Extra = nil
  Extra := TActiveTextFactory.CloneActiveText(Src.Extra);
  CompilerResults := Src.CompilerResults;
  TestInfo := Src.TestInfo;
end;

procedure TSnippetData.Init;
  {Initialises record by setting default values for fields.
  }
var
  CompID: TCompilerID;  // loops thru compiler IDs
begin
  Kind := skFreeform;
  Cat := '';
  Desc := TActiveTextFactory.CreateActiveText;
  DisplayName := '';
  SourceCode := '';
  HiliteSource := True;
  Extra := TActiveTextFactory.CreateActiveText;
  for CompID := Low(TCompilerID) to High(TCompilerID) do
    CompilerResults[CompID] := crQuery;
  TestInfo := stiNone;
end;

{ TSnippetReferences }

procedure TSnippetReferences.Assign(const Src: TSnippetReferences);
  {Sets this record to be a copy of another record.
    @param Src [in] Record to be copied.
  }
begin
  Init;
  (Units as IAssignable).Assign(Src.Units);
  (Depends as IAssignable).Assign(Src.Depends);
  (XRef as IAssignable).Assign(Src.XRef);
end;

procedure TSnippetReferences.Init;
  {Initialises record by creating objects stored in fields.
  }
begin
  Units := TIStringList.Create;
  Depends := TSnippetIDList.Create;
  XRef := TSnippetIDList.Create;
end;

{ TSnippetEditData }

procedure TSnippetEditData.Assign(const Src: TSnippetEditData);
  {Sets this record to be a copy of another record.
    @param Src [in] Record to be copied.
  }
begin
  Props.Assign(Src.Props);
  Refs.Assign(Src.Refs);
end;

procedure TSnippetEditData.Init;
  {Initialises record by creating default values and field objects.
  }
begin
  Props.Init;
  Refs.Init;
end;

{ TSnippetIDListEx }

constructor TSnippetIDListEx.Create(const SnipList: TSnippetList);
  {Constructor overload that creates a snippets ID list from a TSnippetList
  object.
    @param SnipList [in] List of snippets objects for which ID list is
      required.
  }
var
  Snippet: TSnippet;  // references each snippet in list
begin
  inherited Create;
  for Snippet in SnipList do
    Add(Snippet.ID);
end;

end.
