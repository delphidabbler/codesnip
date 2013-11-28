{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2011-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Objects, records etc that encapsulate a code snippet, its data and lists of
 * code snippets.
}


unit DB.USnippet;


interface


uses
  // Delphi
  Classes, Generics.Defaults,
  // 3rd party
  Collections.Base,
  Collections.Lists,
  // Project
  CS.ActiveText,
  CS.Database.Types,
  CS.SourceCode.Languages,
  Compilers.UGlobals,
  DB.USnippetKind,
  UIStringList,
  USnippetIDs;


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
    Title: string;                        // Snippet's title
    Notes: IActiveText;                   // Additional notes about snippet
    CompilerResults: TCompileResults;     // Compilation results
    TestInfo: TSnippetTestInfo;           // Test information
    Tags: ITagSet;                        // Set of tags assigned to snippet
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
    RequiredModules: IStringList;     // List of required modules (e.g. units)
    RequiredSnippets: ISnippetIDList; // List of IDs of required snippets
    XRefs: ISnippetIDList;            // List of IDs of x-referenced snippets
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
    ///  <summary>Comparer for snippets by title.</summary>
    type
      TTitleComparer = class(TComparer<TSnippet>)
      public
        ///  <summary>Compares snippets Left and Right. Returns -ve if Left's
        ///  title sorts before Right's, 0 if the same or +ve if Left's title is
        ///  greater than Right's.</summary>
        function Compare(const Left, Right: TSnippet): Integer; override;
      end;
      TTitleEqualityComparer = class(TEqualityComparer<TSnippet>)
        function Equals(const Left, Right: TSnippet): Boolean; override;
        function GetHashCode(const Snippet: TSnippet): Integer; override;
      end;
  strict private
    fID: TSnippetID;                        // Snippet's ID.
    fKind: TSnippetKind;                    // Kind of snippet this is
    fCategory: string;                      // Name of snippet's category
    fDescription: IActiveText;              // Description of snippet
    fSourceCode: string;                    // Snippet's source code
    fTitle: string;                         // Title of snippet
    fRequiredModules: IStringList;          // List of required modules (units)
    fRequiredSnippets: ISnippetIDList;      // List of required snippets
    fXRefs: ISnippetIDList;                 // List of cross-referenced snippets
    fNotes: IActiveText;                    // Further information for snippet
    fCompatibility: TCompileResults;        // Snippet's compiler compatibility
    fHiliteSource: Boolean;                 // If source is syntax highlighted
    fTestInfo: TSnippetTestInfo;            // Level of testing of snippet
    fTags: ITagSet;                         // Set of snippet's tags
    function GetTitle: string;
      {Gets snippet's title, or ID string if no title is set
        @return Required title.
      }
  strict protected
    procedure SetProps(const Data: TSnippetData);
      {Sets snippet's properties.
        @param Data [in] Record containing property values.
      }
  public
    constructor Create(const ID: TSnippetID; const Props: TSnippetData);
      {Class contructor. Sets up snippet object with given property values.
        @param Name [in] Name of snippet.
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
    property ID: TSnippetID read fID;
      {Snippet's unique ID}
    property Kind: TSnippetKind read fKind;
      {Kind of snippet represented by this object}
    property Title: string read GetTitle;
      {Displat name of snippet}
    property Category: string read fCategory;
      {Category to which snippet belongs}
    property Description: IActiveText read fDescription;
      {Description of snippet}
    property SourceCode: string read fSourceCode;
      {Source code of snippet}
    property HiliteSource: Boolean read fHiliteSource;
      {Flags whether source code can be syntax highlighted}
    property Notes: IActiveText read fNotes;
      {Additional information about snippet}
    property Compatibility: TCompileResults read fCompatibility;
      {Compiler compatibilty of this snippet}
    property TestInfo: TSnippetTestInfo read fTestInfo;
      {Describes level of testing carried out on snippet}
    property Tags: ITagSet read fTags;
      {Set of tags associated with snippet}
    property RequiredModules: IStringList read fRequiredModules;
      {List of modules (e.g. units) used by snippet}
    property RequiredSnippets: ISnippetIDList read fRequiredSnippets;
      {List of any other snippet in database on which this snippet depends}
    property XRefs: ISnippetIDList read fXRefs;
      {List of cross referenced snippets in database}
    ///  <summary>Returns source code language used for snippet.</summary>
    ///  <remarks>Included to assist in testing syntax multi-language
    ///  highlighting. Revised database will include a similar snippet property.
    ///  </remarks>
    function Language: TSourceCodeLanguageID;
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
    function Find(const SnippetID: TSnippetID; out Index: Integer): Boolean;
      overload;
      {Finds a snippet in the list that has a specified name and user defined
      property. Uses a binary search.
        @param SnippetID [in] ID of snippet to be found.
        @param Index [out] Index of required snippet in list. Valid only if
          method returns True.
        @return True if snippet found, False if not.
      }
  strict protected
    var fList: TObjectSortedList<TSnippet>; // Sorted list of snippets
  public
    constructor Create(const OwnsObjects: Boolean = False);
      {Constructor. Creates a new empty list.
        @param OwnsObjects [in] Specifies whether list owns the snippet objects
          it contains. If True the snippets are freed when deleted from list.
      }
    destructor Destroy; override;
      {Destructor. Tears down object.
      }
    procedure Add(const Snippet: TSnippet); virtual;
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
    function Find(const SnippetName: string): TSnippet; overload;
      {Finds a named snippet in list with a matching user defined property.
        @param SnippetName [in] Name of required snippet.
        @return Reference to required snippet or nil if not found.
      }
    function Contains(const Snippet: TSnippet): Boolean;
      {Checks whether list contains a specified snippet.
        @param Snippet [in] Required snippet.
        @return True if snippet is in list, False otherwise.
      }
    procedure Clear;
      {Clears the list.
      }
    function GetEnumerator: IEnumerator<TSnippet>;
      {Gets an intialised snippet list enumerator.
        @return Required enumerator.
      }
    function Count: Integer;
      {Counts number of snippets in list.
        @return Number of snippets in list.
      }
    function IsEmpty: Boolean; inline;
      {Checks if list is empty.
        @return True if list is empty, False otehrwise.
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
    procedure Add(const Snippet: TSnippet); override;
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


implementation


uses
  // Delphi
  SysUtils,
  // Project
  CS.Database.Tags,
  CS.Utils.Hashes,
  IntfCommon,
  UExceptions,
  UStrUtils;

{ TSnippet }

function TSnippet.CanCompile: Boolean;
  {Checks if snippet can be compiled.
    @return True if compilation supported and False if not.
  }
begin
  Result := Kind <> skFreeform;
end;

constructor TSnippet.Create(const ID: TSnippetID; const Props: TSnippetData);
  {Class contructor. Sets up snippet object with given property values.
    @param Name [in] Name of snippet.
    @param Props [in] Values of various snippet properties.
  }
begin
  Assert(ClassType <> TSnippet,
    ClassName + '.Create: must only be called from descendants.');
  inherited Create;
  // Record simple property values
  fID := ID;
  fTags := TTagSet.Create;
  SetProps(Props);
  // Create string list to store required units
  fRequiredModules := TIStringList.Create;
  // Create snippets lists for Depends and XRef properties
  fRequiredSnippets := TSnippetIDList.Create;
  fXRefs := TSnippetIDList.Create;
end;

destructor TSnippet.Destroy;
  {Destructor. Tears down object.
  }
begin
  fNotes := nil;
  fDescription := nil;
  inherited;
end;

function TSnippet.GetTitle: string;
begin
  if not StrIsBlank(fTitle) then
    Result := fTitle
  else
    Result := fID.ToString;
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

function TSnippet.Language: TSourceCodeLanguageID;
begin
  if fHiliteSource then
    Result := TSourceCodeLanguageID.Create('Pascal')
  else
    Result := TSourceCodeLanguageID.Create('Text');
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
  fTitle := Data.Title;
  fNotes := TActiveTextFactory.CloneActiveText(Data.Notes);
  fCompatibility := Data.CompilerResults;
  fTestInfo := Data.TestInfo;
  fTags.Assign(Data.Tags);
end;

{ TSnippet.TTitleComparer }

function TSnippet.TTitleComparer.Compare(const Left, Right: TSnippet): Integer;
begin
  Result := StrCompareText(Left.Title, Right.Title);
  if Result = 0 then
    Result := TSnippetID.Compare(Left.ID, Right.ID);
end;

{ TSnippet.TTitleEqualityComparer }

function TSnippet.TTitleEqualityComparer.Equals(const Left, Right: TSnippet):
  Boolean;
begin
  Result := StrSameText(Left.Title, Right.Title);
  if Result then
    Result := Left.ID = Right.ID;
end;

function TSnippet.TTitleEqualityComparer.GetHashCode(const Snippet: TSnippet):
  Integer;
begin
  Result := TextHash(Snippet.Title);
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
  Result.Init;
  Result.Cat := Category;
  Result.Kind := Kind;
  Result.Desc := Description;
  Result.SourceCode := SourceCode;
  Result.HiliteSource := HiliteSource;
  Result.Title := Title;
  Result.Notes := TActiveTextFactory.CloneActiveText(Notes);
  Result.CompilerResults := Compatibility;
  Result.TestInfo := TestInfo;
  Result.Tags.Assign(Tags);
end;

function TSnippetEx.GetReferences: TSnippetReferences;
  {Gets details of snippet's references.
    @return Information sufficient to define references.
  }
begin
  Result.RequiredModules := TIStringList.Create(RequiredModules);
  Result.RequiredSnippets :=
    (RequiredSnippets as IClonable).Clone as ISnippetIDList;
  Result.XRefs := (XRefs as IClonable).Clone as ISnippetIDList;
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

  // Copies all snippet IDs from list Src to list Dest except that any IDs not
  // in AllSnippets are ignored.
  procedure StoreValidRefs(Src, Dest: ISnippetIDList);
  var
    ID: TSnippetID;
  begin
    Dest.Clear;
    for ID in Src do
    begin
      if AllSnippets.Find(ID) <> nil then
        Dest.Add(ID);
    end;
  end;

begin
  (Self.RequiredModules as IAssignable).Assign(Refs.RequiredModules);
  StoreValidRefs(Refs.RequiredSnippets, Self.RequiredSnippets);
  StoreValidRefs(Refs.XRefs, Self.XRefs);
end;

{ TSnippetList }

procedure TSnippetList.Add(const Snippet: TSnippet);
  {Adds new snippet to the list, maintaining list in alphabetical order.
    @param Snippet [in] Snippet being added.
    @return Index where item was inserted in list
    @except Raised if duplicate snippet added to list.
  }
begin
  fList.Add(Snippet);
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
  fList := TObjectSortedList<TSnippet>.Create(
    TRules<TSnippet>.Create(
      TDelegatedComparer<TSnippet>.Create(
        function (const Left, Right: TSnippet): Integer
        begin
          Result := TSnippetID.Compare(Left.ID, Right.ID);
        end
      ),
      TDelegatedEqualityComparer<TSnippet>.Create(
        function (const Left, Right: TSnippet): Boolean
        begin
          Result := Left.ID = Right.ID;
        end,
        function (const Snippet: TSnippet): Integer
        begin
          Result := Snippet.ID.Hash;
        end
      )
    )
  );
  fList.OwnsObjects := OwnsObjects;
end;

destructor TSnippetList.Destroy;
  {Destructor. Tears down object.
  }
begin
  fList.Free; // destroys owned snippets if OwnsObjects=True
  inherited;
end;

function TSnippetList.Find(const SnippetID: TSnippetID; out Index: Integer):
  Boolean;
  {Finds a snippet in the list that has a specified name and user defined
  property. Uses a binary search.
    @param SnippetID [in] ID of snippet to be found.
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
  TempSnippet := TTempSnippet.Create(SnippetID, NulData);
  try
    Index := fList.IndexOf(TempSnippet);
    Result := Index >= 0;
  finally
    TempSnippet.Free;
  end;
end;

function TSnippetList.Find(const SnippetName: string): TSnippet;
  {Finds a named snippet in list with a matching user defined property.
    @param SnippetName [in] Name of required snippet.
      user defined snippet or one from main database.
    @return Reference to required snippet or nil if not found.
  }
begin
  Result := Find(TSnippetID.Create(SnippetName));
end;

function TSnippetList.Find(const SnippetID: TSnippetID): TSnippet;
  {Finds a specified snippet in list.
    @param SnippetID [in] ID of snippet to find.
    @return Reference to required snippet or nil if not found.
  }
var
  Idx: Integer; // index of snippet name in list
begin
  if Find(SnippetID, Idx) then
    Result := Items[Idx]
  else
    Result := nil;
end;

function TSnippetList.GetEnumerator: IEnumerator<TSnippet>;
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

{ TSnippetListEx }

procedure TSnippetListEx.Add(const Snippet: TSnippet);
  {Adds a snippet to list. Snippet must not be TTempSnippet class.
    @param Snippet [in] Snippet to be added.
    @return Index where snippet was added to list.
  }
begin
  Assert(not(Snippet is TTempSnippet),
    ClassName + '.Add: Can''t add temporary snippets to database');
  inherited Add(Snippet);
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
  fList.RemoveAt(Idx);  // this frees snippet if list owns objects
end;

{ TSnippetData }

procedure TSnippetData.Assign(const Src: TSnippetData);
  {Sets this record to be a copy of another record.
    @param Src [in] Record to be copied.
  }
begin
  Init;
  Kind := Src.Kind;
  Cat := Src.Cat;
  Desc := TActiveTextFactory.CloneActiveText(Src.Desc);
  SourceCode := Src.SourceCode;
  HiliteSource := Src.HiliteSource;
  Title := Src.Title;
  Notes := TActiveTextFactory.CloneActiveText(Src.Notes);
  CompilerResults := Src.CompilerResults;
  TestInfo := Src.TestInfo;
  Tags.Assign(Src.Tags);
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
  Title := '';
  SourceCode := '';
  HiliteSource := True;
  Notes := TActiveTextFactory.CreateActiveText;
  for CompID := Low(TCompilerID) to High(TCompilerID) do
    CompilerResults[CompID] := crQuery;
  TestInfo := stiNone;
  Tags := TTagSet.Create;
end;

{ TSnippetReferences }

procedure TSnippetReferences.Assign(const Src: TSnippetReferences);
  {Sets this record to be a copy of another record.
    @param Src [in] Record to be copied.
  }
begin
  Init;
  (RequiredModules as IAssignable).Assign(Src.RequiredModules);
  (RequiredSnippets as IAssignable).Assign(Src.RequiredSnippets);
  (XRefs as IAssignable).Assign(Src.XRefs);
end;

procedure TSnippetReferences.Init;
  {Initialises record by creating objects stored in fields.
  }
begin
  RequiredModules := TIStringList.Create;
  RequiredSnippets := TSnippetIDList.Create;
  XRefs := TSnippetIDList.Create;
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

end.

