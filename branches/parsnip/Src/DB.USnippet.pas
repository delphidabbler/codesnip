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
  CS.Database.SnippetsTable,
  CS.Database.Types,
  CS.SourceCode.Languages,
  Compilers.UGlobals,
  DB.USnippetKind,
  UIStringList;


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
    LanguageID: TSourceCodeLanguageID;    // Source code language ID
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

  TSnippet = class;

  ///  <summary>Comparer for snippets by title.</summary>
  TSnippetTitleComparer = class(TComparer<TSnippet>)
  public
    ///  <summary>Compares snippets Left and Right. Returns -ve if Left's
    ///  title sorts before Right's, 0 if the same or +ve if Left's title is
    ///  greater than Right's.</summary>
    function Compare(const Left, Right: TSnippet): Integer; override;
  end;

  TSnippetTitleEqualityComparer = class(TEqualityComparer<TSnippet>)
    function Equals(const Left, Right: TSnippet): Boolean; override;
    function GetHashCode(const Snippet: TSnippet): Integer; override;
  end;

  {
  TSnippet:
    Encapsulates a snippet from the database. Can be routine, type, constant or
    free-form.
  }
  TSnippet = class(TDBSnippet)
  public
    procedure SetProps(const Data: TSnippetData);
      {Sets snippet's properties.
        @param Data [in] Record containing property values.
      }
  public
    function CanCompile: Boolean;
      {Checks if snippet can be compiled.
        @return True if compilation supported and False if not.
      }
    property ID: TSnippetID read GetID;
      {Snippet's unique ID}
    property Kind: TSnippetKind read GetKind;
      {Kind of snippet represented by this object}
    property Title: string read GetTitle;
      {Display name of snippet}
    //property Category  *** now INHERITED from TDBSnippet ***
    //  {Category to which snippet belongs}
    property Description: IActiveText read GetDescription;
      {Description of snippet}
    property SourceCode: string read GetSourceCode;
      {Source code of snippet}
    property LanguageID: TSourceCodeLanguageID read GetLanguageID;
      {Identifier of source code language}
    property Notes: IActiveText read GetNotes;
      {Additional information about snippet}
    property Compatibility: TCompileResults read GetCompileResults;
      {Compiler compatibilty of this snippet}
    property TestInfo: TSnippetTestInfo read GetTestInfo;
      {Describes level of testing carried out on snippet}
    property Tags: ITagSet read GetTags;
      {Set of tags associated with snippet}
    property RequiredModules: IStringList read GetRequiredModules;
      {List of modules (e.g. units) used by snippet}
    property RequiredSnippets: ISnippetIDList read GetRequiredSnippets;
      {List of any other snippet in database on which this snippet depends}
    property XRefs: ISnippetIDList read GetXRefs;
      {List of cross referenced snippets in database}
  public  // ex TSnippetEx
    procedure UpdateRefs(const Refs: TSnippetReferences);
      {Updates a snippet's references.
        @param Refs [in] Stores all snippet's references (XRef, Depends and
          Units).
      }
    procedure Update(const Data: TSnippetEditData);
      {Updates snippet's properties and references.
        @param Data [in] New property values and references.
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
  _TSnippetList:
    Class that implements a list of TSnippet objects.
  }
  _TSnippetList = class(TObject)
  strict private
    function GetItem(Idx: Integer): TSnippet;
      {Read accessor for Items property.
        @param Idx [in] Index of required snippet in list.
        @return Snippet at specified index in list.
      }
    function Find(const SnippetID: TSnippetID; out Index: Integer): Boolean;
      overload;
      {Finds a snippet in the list that has a specified ID. Uses a binary
      search.
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
    procedure Delete(const Snippet: TSnippet);
      {Deletes a snippet from list.
        @param Snippet [in] Snippet to be deleted. No action taken if snippet
          not in list.
      NOTE: formerly a method of TSnippetListEx
      }
    property Items[Idx: Integer]: TSnippet read GetItem; default;
      {List of snippets}
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  CS.Database.Snippets,
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

function TSnippet.GetEditData: TSnippetEditData;
  {Gets details of all editable data of snippet.
    @return Required editable properties and references.
  }
begin
  Result.Props := GetProps;
  Result.Refs := GetReferences;
end;

function TSnippet.GetProps: TSnippetData;
  {Gets details of snippet's properties.
    @return Record containing property values.
  }
begin
  Result.Init;
  Result.Cat := Category;
  Result.Kind := Kind;
  Result.Desc := Description;
  Result.SourceCode := SourceCode;
  Result.LanguageID := LanguageID;
  Result.Title := Title;
  Result.Notes := Notes;
  Result.CompilerResults := Compatibility;
  Result.TestInfo := TestInfo;
  Result.Tags := Tags;
end;

function TSnippet.GetReferences: TSnippetReferences;
  {Gets details of snippet's references.
    @return Information sufficient to define references.
  }
begin
  Result.RequiredModules := GetRequiredModules;
  Result.RequiredSnippets := GetRequiredSnippets;
  Result.XRefs := GetXRefs;
end;

procedure TSnippet.SetProps(const Data: TSnippetData);
  {Sets snippet's properties.
    @param Data [in] Record containing property values.
  }
begin
  _fCategory := Data.Cat;
  SetKind(Data.Kind);
  SetDescription(Data.Desc);
  SetSourceCode(StrWindowsLineBreaks(Data.SourceCode));
  SetLanguageID(Data.LanguageID);
  SetTitle(Data.Title);
  SetNotes(Data.Notes);
  SetCompileResults(Data.CompilerResults);
  SetTestInfo(Data.TestInfo);
  SetTags(Data.Tags);
end;

procedure TSnippet.Update(const Data: TSnippetEditData);
  {Updates snippet's properties and references.
    @param Data [in] New property values and references.
  }
begin
  SetProps(Data.Props);
  UpdateRefs(Data.Refs);
end;

procedure TSnippet.UpdateRefs(const Refs: TSnippetReferences);
  {Updates a snippet's references.
    @param Refs [in] Stores all snippet's references (XRef, Depends and
      Units).
  }
begin
  SetRequiredModules(Refs.RequiredModules);
  SetRequiredSnippets(Refs.RequiredSnippets);
  SetXRefs(Refs.XRefs);
end;

{ TSnippetTitleComparer }

function TSnippetTitleComparer.Compare(const Left, Right: TSnippet): Integer;
begin
  Result := StrCompareText(Left.Title, Right.Title);
  if Result = 0 then
    Result := TSnippetID.Compare(Left.ID, Right.ID);
end;

{ TSnippetTitleEqualityComparer }

function TSnippetTitleEqualityComparer.Equals(const Left, Right: TSnippet):
  Boolean;
begin
  Result := StrSameText(Left.Title, Right.Title);
  if Result then
    Result := Left.ID = Right.ID;
end;

function TSnippetTitleEqualityComparer.GetHashCode(const Snippet: TSnippet):
  Integer;
begin
  Result := TextHash(Snippet.Title);
end;

{ _TSnippetList }

procedure _TSnippetList.Add(const Snippet: TSnippet);
  {Adds new snippet to the list, maintaining list in alphabetical order.
    @param Snippet [in] Snippet being added.
    @return Index where item was inserted in list
    @except Raised if duplicate snippet added to list.
  }
begin
  fList.Add(Snippet);
end;

procedure _TSnippetList.Clear;
  {Clears the list.
  }
begin
  fList.Clear;
end;

function _TSnippetList.Contains(const Snippet: TSnippet): Boolean;
  {Checks whether list contains a specified snippet.
    @param Snippet [in] Required snippet.
    @return True if snippet is in list, False otherwise.
  }
begin
  Result := fList.Contains(Snippet);
end;

function _TSnippetList.Count: Integer;
  {Counts number of snippets in list.
    @return Number of snippets in list.
  }
begin
  Result := fList.Count;
end;

constructor _TSnippetList.Create(const OwnsObjects: Boolean = False);
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

procedure _TSnippetList.Delete(const Snippet: TSnippet);
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

destructor _TSnippetList.Destroy;
  {Destructor. Tears down object.
  }
begin
  fList.Free; // destroys owned snippets if OwnsObjects=True
  inherited;
end;

function _TSnippetList.Find(const SnippetID: TSnippetID; out Index: Integer):
  Boolean;
  {Finds a snippet in the list that has a specified ID. Uses a binary search.
    @param SnippetID [in] ID of snippet to be found.
    @param Index [out] Index of required snippet in list. Valid only if
      method returns True.
    @return True if snippet found, False if not.
  }
var
  TempSnippet: TSnippet;  // temp snippet used to perform search
  NullData: TSnippetData; // null data used to create snippet
begin
  // We need a temporary snippet object in order to perform binary search using
  // object list's built in search
  NullData.Init;
  TempSnippet := TSnippet.Create(SnippetID);
  try
    TempSnippet.SetProps(NullData);
    Index := fList.IndexOf(TempSnippet);
    Result := Index >= 0;
  finally
    TempSnippet.Free;
  end;
end;

function _TSnippetList.Find(const SnippetName: string): TSnippet;
  {Finds a named snippet in list with a matching user defined property.
    @param SnippetName [in] Name of required snippet.
      user defined snippet or one from main database.
    @return Reference to required snippet or nil if not found.
  }
begin
  Result := Find(TSnippetID.Create(SnippetName));
end;

function _TSnippetList.Find(const SnippetID: TSnippetID): TSnippet;
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

function _TSnippetList.GetEnumerator: IEnumerator<TSnippet>;
  {Gets an intialised snippet list enumerator.
    @return Required enumerator.
  }
begin
  Result := fList.GetEnumerator;
end;

function _TSnippetList.GetItem(Idx: Integer): TSnippet;
  {Read accessor for Items property.
    @param Idx [in] Index of required snippet in list.
    @return Snippet at specified index in list.
  }
begin
  Result := fList[Idx];
end;

function _TSnippetList.IsEmpty: Boolean;
  {Checks if list is empty.
    @return True if list is empty, False otehrwise.
  }
begin
  Result := Count = 0;
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
  LanguageID := Src.LanguageID;
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
  LanguageID := TSourceCodeLanguageID.CreateDefault;
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

