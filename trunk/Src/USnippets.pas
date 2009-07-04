{
 * USnippets.pas
 *
 * Defines a singleton object and subsidiary classes that encapsulate the
 * snippets and categories in the CodeSnip database and user defined databases.
 *
 * v0.1 of 30 Jan 2005  - Original version.
 * v0.2 of 18 Feb 2005  - Fixed bug where singletion Snippets object was not
 *                        being freed when application closes.
 * v0.3 of 19 Feb 2005  - Refactoring:
 *                        - Added check that requires only unique categories can
 *                          be added to a category list.
 *                        - Added new IsEqual methods to TCategory and TRoutine.
 * v0.4 of 26 Feb 2005  - Refactoring:
 *                        - Extracted code that reads back end "database" and
 *                          moved to new USnipData unit to decouple Snippets
 *                          object from data storage format. Constructors now
 *                          access database via a new reader object that defines
 *                          standard interface to database.
 *                        - Deleted unused properties.
 *                        - Deleted methods and constructors made redundant by
 *                          code changes.
 *                        - Redefined constructors for TRoutine and TCategory to
 *                          create object from Name=Value list of property
 *                          values.
 * v0.5 of 04 Mar 2005  - Changed to use new TCompilerID enumeration that
 *                        replaces TDelphiVersion.
 * v0.6 of 17 Mar 2005  - Source code read from database is now processed to
 *                        ensure that all end of line markers are CR LF pairs.
 * v0.7 of 20 Apr 2005  - Changed to use renamed IntfCompilers unit.
 * v0.8 of 02 Apr 2006  - Added new TSnippets.Clear method to remove all
 *                        snippets data.
 *                      - Renamed TSnippets.ReLoad method as Load and made clear
 *                        all data if Load raises exception.
 *                      - Changed so that TSnippets class has to be explicitly
 *                        loaded rather than being loaded in constructor.
 * v1.0 of 25 May 2006  - Improved and corrected comments.
 *                      - Removed redundant field from TCategory.
 * v1.1 of 26 Oct 2006  - Declared new ISnippets interface.
 *                      - Moved TSnippets to implementation section and changed
 *                        to descend from TInterfacedObject and implement
 *                        ISnippets.
 *                      - Changed private implementation variable to be of
 *                        ISnippets type.
 *                      - Changed TSnippets to implement property read / write
 *                        accessor methods of ISnippets rather than using direct
 *                        field access.
 * v1.2 of 24 Aug 2008  - Added Windows unit to enable inlining in Delphi 2006.
 * v2.0 of 15 Sep 2008  - Major update. Added support for user database:
 *                        - Extended TSnippets, TRoutine, TCategory and
 *                          TRoutineList to support editing, creation and
 *                          deletion of routines in the user database. TSnippets
 *                          now exposes editing facilities via the ISnippetsEdit
 *                          interface.
 *                        - Added numerous record and interface types to
 *                          facilitate exchange of editable data with classes
 *                          that control editing.
 *                        - Changed TRoutine to permit duplicate routine names
 *                          providing they one is in the user and other in main
 *                          database.
 *                        - Added Find method to TCategoryList that finds a
 *                          category by name.
 *                        - Moved database loader code to separate units.
 *                        - Added events triggered by Snippets user database is
 *                          modified.
 *                        - Added enumerators to list objects.
 *                        - Changed TRoutineList.Count property to be two
 *                          overloaded methods, one that returns total number of
 *                          routines in list and another that counts routines in
 *                          user or main databases.
 * v2.1 of 19 Sep 2008  - Modified routine update and deletion methods of
 *                        ISnippetsEdit to update Depends and XRef properties of
 *                        routines that refer to routine being modified.
 *                      - Added new methods to ISnippetsEdit that return lists
 *                        of routines that depend on and cross reference a
 *                        specified routine.
 * v2.2 of 21 Sep 2008  - Improved speed of routine searches in routine list by
 *                        changing to use binrary search.
 *                      - Added constructor, Compare method and overloads for
 *                        "=" and "<>" operators to TRoutineID record.
 *                      - Removed unused StrToBoolean helper routine.
 *                      - Prevented any routine from referencing itself in its
 *                        XRef or Depends properties.
 * v2.3 of 14 Dec 2008  - Added new Clone constructor to TRoutineID.
 *                      - Added new Assign methods to: TRoutineIDList
 *                        implementation, TRoutineData, TRoutineReferences,
 *                        and TRoutineEditData.
 *                      - Added new Init method to TRoutineReferences,
 *                        TRoutineEditData and TRoutineData records and modified
 *                        code that performed initialisation to use this method.
 *                      - Added new TRoutineInfo record and TRoutineInfoList
 *                        dynamic array types.
 * v2.4 of 30 Dec 2008  - Removed Credits, CreditsURL and Comments properties
 *                        from TRoutine and TRoutineData and replaced with new
 *                        Extra active text property.
 *                      - Fixed bug in TSnippets.UpdateRoutine where a possibly
 *                        deleted routine reference is passed to change event.
 * v2.5 of 04 Jan 2009  - Added new TTempRoutine subclass of TRoutine that is
 *                        designed for temporary use and can't be added to the
 *                        Snippets routine list.
 *                      - Added override of TRoutineList.Add in TRoutineListEx
 *                        that raises bug error if an attempt is made to add a
 *                        TTempRoutine.
 *                      - Added method to ISnippetsEdit and implementation that
 *                        creates a new user defined TTempRoutine instance.
 * v2.6 of 10 Jan 2009  - Added overload of ISnippetsEdit.CreateTempRoutine
 *                        and implementation that clones a routine as a
 *                        TTempRoutine.
 *                      - All Asserts now use ClassName method to report class.
 * v3.0 of 09 Jun 2009  - Added support for difference kinds of snippet.
 *                        TRoutine modified to have a Kind property that
 *                        determines if snippet is routine, type, constant or
 *                        freeform.
 *                      - Deleted TRoutine.StandardFormat property and
 *                        associated code.
 *                      - Added TRoutine.CanCompile method.
 *                      - Made Items proptery of IRoutineIDList writeable.
 *                      - Removed TRoutineID, IRoutineIDListEnum,
 *                        IRoutineIDList and TRoutineIDList. Renamed versions
 *                        now in USnippetIDs. Private TSnippetIDListEx added to
 *                        implementation to get required overloaded constructor
 *                        not available in USnippetIDs. Modified other classes
 *                        accordingly.
 *                      - Messages that referred to "routine" changed to refer
 *                        to "snippet"
 *
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
 * The Original Code is USnippets.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit USnippets;


interface


uses
  // Delphi
  Classes, Contnrs,
  // Project
  IntfCompilers, UActiveText, UBaseObjects, UIStringList, UMultiCastEvents,
  USnippetIDs;


type

  TRoutine = class;
  TRoutineList = class;
  TCategory = class;
  TCategoryList = class;

  {
  TSnippetKind:
    Enumeration of various supported kinds of snippets.
  }
  TSnippetKind = (
    skFreeform,   // free-form code - not in any of other supported formats
    skRoutine,    // procedure or function in standard format
    skConstant,   // constant definition in standard format
    skTypeDef     // type definition in standard format
  );

  {
  TRoutineData:
    Record that provides information about a snippet's properties.
  }
  TRoutineData = record
    Kind: TSnippetKind;               // Kind of snippet
    Cat: string;                      // Category containing snippet
    Desc: string;                     // Description of snippet
    SourceCode: string;               // Snippet's source code
    Extra: IActiveText;               // Extra text used to describe snippet
    CompilerResults: TCompileResults; // Compilation results
    procedure Init;
      {Initialises record by setting default values for fields.
      }
    procedure Assign(const Src: TRoutineData);
      {Sets this record to be a copy of another record.
        @param Src [in] Record to be copied.
      }
  end;

  {
  TRoutineReferences:
    Record that details a snippet's references
  }
  TRoutineReferences = record
    Units: IStringList;         // List of required units
    Depends: ISnippetIDList;    // List of IDs of required snippets
    XRef: ISnippetIDList;       // List of IDs of cross referenced snippets
    procedure Init;
      {Initialises record by creating objects stored in fields.
      }
    procedure Assign(const Src: TRoutineReferences);
      {Sets this record to be a copy of another record.
        @param Src [in] Record to be copied.
      }
  end;

  {
  TRoutineEditData:
    Provides deatils of a snippet's data that can be edited.
  }
  TRoutineEditData = record
    Props: TRoutineData;        // snippet's editable properties
    Refs: TRoutineReferences;   // snippet's editable referfences
    procedure Init;
      {Initialises record by creating default values and field objects.
      }
    procedure Assign(const Src: TRoutineEditData);
      {Sets this record to be a copy of another record.
        @param Src [in] Record to be copied.
      }
  end;

  {
  TRoutineInfo:
    Record that encapsulates data read from an import file that describes a
    snippet.
  }
  TRoutineInfo = record
    Name: string;           // snippet name
    Data: TRoutineEditData; // describes a snippet
    procedure Assign(const Src: TRoutineInfo);
      {Sets this record's fields to be same as another TRoutineInfo record.
      Object fields are copied appropriately.
        @param Src [in] Record containing fields to be copied.
      }
    procedure Init;
      {Initialises record to nul values.
      }
  end;

  {
  TRoutineInfoList:
    Dynamic array of TRoutineInfo records.
  }
  TRoutineInfoList = array of TRoutineInfo;

  {
  TCategoryData:
    Provides information about a category's properties.
  }
  TCategoryData = record
    Desc: string;               // description of categpry
  end;

  {
  TSnippetChangeEventKind:
    Enumeration that specifies the different kind of change events triggered by
    the user database.
  }
  TSnippetChangeEventKind = (
    evChangeBegin,      // a change to the database is about to take place
    evChangeEnd,        // a change to the database has completed
    evRoutineAdded,     // a snippet has been added
    evRoutineDeleted,   // a snippet has been deleted
    evRoutineChanged    // a snippet's properties and/or reference have changed
  );

  {
  ISnippetsDataProvider:
    Interface supported by objects that provides data about the categories and
    snippets in the database.
  }
  ISnippetsDataProvider = interface(IInterface)
    ['{D2D57A0D-DB29-4012-891E-E817E0EED8C8}']
    function GetCategoryProps(const Cat: TCategory): TCategoryData;
      {Retrieves all the properties of a category.
        @param Cat [in] Category for which data is requested.
        @return Record containing property data.
      }
    function GetCategoryRoutines(const Cat: TCategory): IStringList;
      {Retrieves names of all snippets that belong to a category.
        @param Cat [in] Category for which snippet names are requested.
        @return Required list of snippet names.
      }
    function GetRoutineProps(const Routine: TRoutine): TRoutineData;
      {Retrieves all the properties of a snippet.
        @param Routine [in] Snippet for which data is requested.
        @return Record containing property data.
      }
    function GetRoutineRefs(const Routine: TRoutine): TRoutineReferences;
      {Retrieves information about all the references of a snippet.
        @param Routine [in] Snippet for which information is requested.
        @return Record containing references.
      }
  end;

  {
  ISnippetChangeEventInfo:
    Interface supported by objects passed to Snippets object's change event
    handler that provides information about a change event. Some properites
    are not defined for certain event types. Kind property always defined.
  }
  ISnippetChangeEventInfo = interface(IInterface)
    ['{80DEE62F-DC23-4EE7-A0B1-5DE46F483CE1}']
    function GetKind: TSnippetChangeEventKind;
      {Gets kind (type) of event.
        @return Event kind.
      }
    function GetRoutine: TRoutine;
      {Gets reference to snippet referred to by event.
        @return Snippet reference. Not defined for some event kinds.
      }
    property Kind: TSnippetChangeEventKind read GetKind;
      {Identifies kind (type) of an event. Always defined}
    property Routine: TRoutine read GetRoutine;
      {References snippet that event refers to. Not defined for some event
      kinds}
  end;

  {
  ISnippetsFactory:
    Interface to factory object that creates snippet and category objects. For
    use by database loader objects.
  }
  ISnippetsFactory = interface(IInterface)
    ['{C6DD85BD-E649-4A90-961C-4011D2714B3E}']
    function CreateCategory(const CatID: string; const UserDefined: Boolean;
      const Data: TCategoryData): TCategory;
      {Creates a new category object.
        @param CatID [in] ID (name) of new category. Must be unique.
        @param UserDefined [in] True if category is user defined, False if not.
        @param Data [in] Record describing category's properties.
        @return Instance of new category object.
      }
    function CreateRoutine(const Name: string; const UserDefined: Boolean;
      const Props: TRoutineData): TRoutine;
      {Creates a new snippet object.
        @param Name [in] Name of new snippet. Must not exist in database
          specified by UserDefined parameter.
        @param UserDefined [in] True if snippet is user defined, False if not.
        @param Props [in] Record describing snippet's properties.
        @return Instance of new snippet with no references.
      }
  end;

  {
  TRoutine:
    Encapsulates a snippet from the database. Can be routine, type, constant or
    free-form.
  }
  TRoutine = class(TObject)
  strict private
    fKind: TSnippetKind;              // Kind of snippet this is
    fCategory: string;                // Name of snippet's category
    fDescription: string;             // Description of snippet
    fSourceCode: string;              // Snippet's source code}
    fName: string;                    // Name of snippet
    fUnits: TStringList;              // List of required units
    fDepends: TRoutineList;           // List of required snippets
    fXRef: TRoutineList;              // List of cross-referenced snippets
    fExtra: IActiveText;              // Additional information about snippet
    fCompatibility: TCompileResults;  // Routine's compiler compatibility
    fUserDefined: Boolean;            // Whether this snippet is user-defined
    function GetID: TSnippetID;
      {Gets snippet's unique ID.
        @return Required ID.
      }
  strict protected
    procedure SetName(const Name: string);
      {Sets Name property.
        @param Name [in] New name.
      }
    procedure SetProps(const Data: TRoutineData);
      {Sets snippet's properties.
        @param Data [in] Record containing property values.
      }
  public
    constructor Create(const Name: string; const UserDefined: Boolean;
      const Props: TRoutineData);
      {Class contructor. Sets up snippet object with given property values.
        @param Name [in] Name of snippet.
        @param UserDefined [in] Indicates if this is a user defined snippet.
        @param Props [in] Values of various snippet properties.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    function IsEqual(const Routine: TRoutine): Boolean;
      {Checks if this snippet is same as another snippet. Snippets are
      considered equal if they have the same name and come from same database.
        @param Routine [in] Snippet being compared.
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
    property Category: string read fCategory;
      {Category to which snippet belongs}
    property Description: string read fDescription;
      {Description of snippet}
    property SourceCode: string read fSourceCode;
      {Source code of snippet}
    property Extra: IActiveText read fExtra;
      {Additional information about snippet}
    property Compatibility: TCompileResults read fCompatibility;
      {Compiler compatibilty of this snippet}
    property Units: TStringList read fUnits;
      {List of units used by snippet}
    property Depends: TRoutineList read fDepends;
      {List of any other snippet in database on which this snippet depends}
    property XRef: TRoutineList read fXRef;
      {List of cross referenced snippets in database}
    property UserDefined: Boolean read fUserDefined;
      {Flag that indicates if this is a user defined snippet}
  end;

  {
  TRoutineList:
    Class that implements a list of TRoutine objects.
  }
  TRoutineList = class(TObject)
  strict private
    function GetItem(Idx: Integer): TRoutine;
      {Read accessor for Items property.
        @param Idx [in] Index of required snippet in list.
        @return Snippet at specified index in list.
      }
    function Find(const RoutineName: string;
      const UserDefined: Boolean; out Index: Integer): Boolean; overload;
      {Finds a snippet in the list that has a specified name and user defined
      property. Uses a binary search.
        @param RoutineName [in] Name of snippet to be found.
        @param UserDefined [in] Whether required snippet is user defined or not.
        @param Index [out] Index of required snippet in list. Valid only if
          method returns True.
        @return True if snippet found, False if not.
      }
    function CompareIDs(const SID1, SID2: TSnippetID): Integer;
      {Compares IDS of two snippets.
        @param SID1 [in] First ID to be compared.
        @param SID2 [in] Second ID to be compared.
        @return 0 if IDs are the same, -ve if SID1 < SID2 and +ve if
          SID1 > SID2.
      }
  strict protected
    var
      fList: TObjectList;   // Stores list of snippets
  public
    type
      {
      TEnumerator:
        Implements enumerator for TRoutineList.
      }
      TEnumerator = class(TObject)
      private
        fRoutineList: TRoutineList; // Reference to object being enumerated
        fIndex: Integer;            // Index of current item in enumeration
        function GetCurrent: TRoutine;
          {Gets current snippet in enumeration. Error if at end of enumeration.
            @return Current snippet.
          }
      public
        constructor Create(const RL: TRoutineList);
          {Class constructor. Sets up and initialises enumeration.
            @param RL [in] Reference to snippet list object to be enumerated.
          }
        function MoveNext: Boolean;
          {Moves to next item in enumeration.
            @return True if there is a next item, False if enumeration
              completed.
          }
        property Current: TRoutine read GetCurrent;
          {Current item in enumeration. Error if at end of enumeration}
      end;
  public
    constructor Create(const OwnsObjects: Boolean = False);
      {Class constructor. Creates a new empty list.
        @param OwnsObjects [in] Specifies whether list owns the snippet objects
          it contains. If True the snippets are freed when deleted from list.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    procedure Assign(const SrcList: TRoutineList);
      {Sets this list to be same as another list. List items are referenced, not
      copied.
        @param SrcList [in] List of snippets to be assigned.
      }
    function IsEqual(const AList: TRoutineList): Boolean;
      {Checks if this list contains same snippets as another list.
        @param AList [in] List of snippets to compare.
        @return True if lists are same, False if not.
      }
    function Add(const Routine: TRoutine): Integer; virtual;
      {Adds new snippet to the list, maintaining list in alphabetical order.
        @param Routine [in] Snippet being added.
        @return Index where item was inserted in list
        @except Raised if duplicate snippet added to list.
      }
    function Find(const RoutineName: string;
      const UserDefined: Boolean): TRoutine; overload;
      {Finds a named snippet in list with a matching user defined property.
        @param RoutineName [in] Name of required snippet.
        @param UserDefined [in] Flag that determines if we are looking for a
          user defined snippet or one from main database.
        @return Reference to object representing snippet in list or nil if not
          in list.
      }
    function Contains(const Routine: TRoutine): Boolean;
      {Checks whether list contains a specified snippet.
        @param Routine [in] Required snippet.
        @return True if snippet is in list, False otherwise.
      }
    procedure Clear;
      {Clears the list.
      }
    function GetEnumerator: TEnumerator;
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
    property Items[Idx: Integer]: TRoutine read GetItem; default;
      {List of snippets}
  end;

  {
  TCategory:
    Class that encapsulates a codesnip database category and list of all
    snippets in it.
  }
  TCategory = class(TObject)
  strict private
    fRoutines: TRoutineList;  // List of snippet objects in category
    fCategory: string;        // Category name
    fDescription: string;     // Category description
    fUserDefined: Boolean;    // Whether this is a user-defined snippet
  public
    constructor Create(const CatID: string; const UserDefined: Boolean;
      const Data: TCategoryData);
      {Class contructor. Sets up category object with given property values.
        @param Data [in] Contains required property values.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    function IsEqual(const Cat: TCategory): Boolean;
      {Checks if this category is same as another category. Categories are
      considered equal if they have the same name.
        @param Cat [in] Category being compared.
        @return True if categories are equal, False if not.
      }
    property Category: string read fCategory;
      {Category name. Must be unique}
    property Description: string read fDescription;
      {Description of category}
    property Routines: TRoutineList read fRoutines;
      {List of snippets in this category}
    property UserDefined: Boolean read fUserDefined;
      {Flag that indicates if this is a user defined category}
  end;

  {
  TCategoryList:
    Class that implements a list of TCategory objects.
  }
  TCategoryList = class(TObject)
  strict private
    fList: TObjectList;   // Stores list of categories
    function GetCount: Integer;
      {Read accessor for Count property.
        @return Number of categories in list.
      }
    function GetItem(Idx: Integer): TCategory;
      {Read accessor for Items property.
        @param Idx [in] Index of required category in list.
        @return Category at specified index in list.
      }
  public
    type
      {
      TEnumerator:
        Implements enumerator for TCategoryList.
      }
      TEnumerator = class(TObject)
      private
        fCategoryList: TCategoryList;   // Category list being enumerated
        fIndex: Integer;                // Index of current item in enumeration
        function GetCurrent: TCategory;
          {Gets current category in enumeration. Error if at end of enumeration.
            @return Current category.
          }
      public
        constructor Create(const CL: TCategoryList);
          {Class constructor. Sets up and initialises enumeration.
            @param CL [in] Reference to object to be enumerated.
          }
        function MoveNext: Boolean;
          {Moves to next item in enumeration.
            @return True if there is a next item, False if enumeration
              completed.
          }
        property Current: TCategory read GetCurrent;
          {Current item in enumeration. Error if at end of enumeration}
      end;
  public
    constructor Create;
      {Class constructor. Creates a new empty list that owns the category
      objects in it, so that detroying this object frees the category objects.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    procedure Clear;
      {Clears the list.
      }
    function Add(const Category: TCategory): Integer;
      {Adds new category to list.
        @param Category [in] Category to be added.
        @return Index where category inserted in list.
      }
    function Find(const CatName: string): TCategory;
      {Finds a named category in list.
        @param CatName [in] Name of required category.
        @return Reference to object representing category in list or nil if not
          in list.
      }
    function GetEnumerator: TEnumerator;
      {Gets an intialised category list enumerator.
        @return Required enumerator.
      }
    property Items[Idx: Integer]: TCategory read GetItem; default;
      {List of categories}
    property Count: Integer read GetCount;
      {Number of categories in list}
  end;

  {
  ISnippets:
    Interface to object that encapsulates the whole (main and user) databases
    and provides access to all snippets and all categories.
  }
  ISnippets = interface(IInterface)
    ['{A280DEEF-0336-4264-8BD0-7CDFBB207D2E}']
    procedure Load;
      {Loads data from main and user databases.
      }
    procedure Clear;
      {Clears all data.
      }
    procedure AddChangeEventHandler(const Handler: TNotifyEventInfo);
      {Adds a change event handler to list of listeners.
        @param Handler [in] Event handler to be added.
      }
    procedure RemoveChangeEventHandler(const Handler: TNotifyEventInfo);
      {Removes a change event handler from list of listeners.
        @param Handler [in] Handler to remove from list.
      }
    function GetRoutines: TRoutineList;
      {Gets list of snippets in main and user databases.
        @return Required list.
      }
    function GetCategories: TCategoryList;
      {Gets list of categories in main and user databases.
        @return Required list.
      }
    property Categories: TCategoryList read GetCategories;
      {List of categories in main and user databases}
    property Routines: TRoutineList read GetRoutines;
      {List of snippets in main and user databases}
  end;

  {
  ISnippetsEdit:
    Interface to object that can be used to edit the user database.
  }
  ISnippetsEdit = interface(IInterface)
    ['{CBF6FBB0-4C18-481F-A378-84BB09E5ECF4}']
    function GetEditableRoutineInfo(
      const Routine: TRoutine = nil): TRoutineEditData;
      {Provides details of all a snippet's data (properties and references) that
      may be edited.
        @param Routine [in] Snippet for which data is required. May be nil in
          which case a blank record is returned.
        @return Required data.
      }
    function GetDependents(const Routine: TRoutine): ISnippetIDList;
      {Builds an ID list of all snippets that depend on a specified snippet.
        @param Routine [in] Snippet for which dependents are required.
        @return List of IDs of dependent snippets.
      }
    function GetReferrers(const Routine: TRoutine): ISnippetIDList;
      {Builds an ID list of all snippets that cross reference a specified
      snippet.
        @param Routine [in] Snippet for which cross referers are required.
        @return List of IDs of referring snippets.
      }
    function UpdateRoutine(const Routine: TRoutine;
      const Data: TRoutineEditData; const NewName: string = ''): TRoutine;
      {Updates a user defined snippet's properties and references using provided
      data.
        @param Routine [in] Snippet to be updated. Must be user-defined.
        @param Data [in] Record containing revised data.
        @param NewName [in] New name of snippet. Set to '' or Routine.Name if
          name is not to change.
        @return Reference to updated snippet. Will have changed.
      }
    function AddRoutine(const RoutineName: string;
      const Data: TRoutineEditData): TRoutine;
      {Adds a new snippet to the user database.
        @param RoutineName [in] Name of new snippet.
        @param Data [in] Record storing new snippet's properties and references.
        @return Reference to new snippet.
      }
    function CreateTempRoutine(const RoutineName: string;
      const Data: TRoutineEditData): TRoutine; overload;
      {Creates a new temporary snippet without adding it to the Snippets
      object's snippets list. The new instance may not be added to the
      Snippets object.
        @param RoutineName [in] Name of new snippet.
        @param Data [in] Record storing new snippet's properties and references.
        @return Reference to new snippet.
      }
    function CreateTempRoutine(const Routine: TRoutine): TRoutine; overload;
      {Creates a new temporary copy of a snippet without adding it to the
      Snippets object's snippets list. The new instance may not be added to the
      Snippets object.
        @param Routine [in] Snippet to be copied.
        @return Reference to new copied snippet.
      }
    procedure DeleteRoutine(const Routine: TRoutine);
      {Deletes a snippet from the user database.
        @param Routine [in] Snippet to be deleted.
      }
    function Updated: Boolean;
      {Checks if user database has been updated since last save.
        @return True if database has been updated, False otherwise.
      }
    procedure Save;
      {Saves user database.
      }
  end;


function Snippets: ISnippets;
  {Returns singleton instance of object that encapsulates main and user
  databases.
    @return Singleton object.
  }


implementation


uses
  // Delphi
  SysUtils, Windows {for inlining},
  // Project
  IntfCommon, UExceptions, USnippetKindInfo, USnippetsIO, USnipData;


var
  // Private global snippets singleton object
  PvtSnippets: ISnippets = nil;


type

  {
  TSnippetsFactory:
    Class that can create category and snippet objects.
  }
  TSnippetsFactory = class(TInterfacedObject, ISnippetsFactory)
  public
    function CreateCategory(const CatID: string; const UserDefined: Boolean;
      const Data: TCategoryData): TCategory;
      {Creates a new category object.
        @param CatID [in] ID (name) of new category. Must be unique.
        @param UserDefined [in] True if category is user defined, False if not.
        @param Data [in] Record describing category's properties.
        @return Instance of new category object.
      }
    function CreateRoutine(const Name: string; const UserDefined: Boolean;
      const Props: TRoutineData): TRoutine;
      {Creates a new snippet object.
        @param Name [in] Name of new snippet. Must not exist in database
          specified by UserDefined parameter.
        @param UserDefined [in] True if snippet is user defined, False if not.
        @param Props [in] Record describing snippet's properties.
        @return Instance of new snippet with no references.
      }
  end;

  {
  TRoutineEx:
    Private extension of TRoutine for use internally by Snippets object.
  }
  TRoutineEx = class(TRoutine)
  public
    procedure UpdateRefs(const Refs: TRoutineReferences;
      const AllRoutines: TRoutineList);
      {Updates a snippet's references.
        @param Refs [in] Stores all snippet's references (XRef, Depends and
          Units).
        @param AllRoutines [in] List of all snippets in database.
      }
    procedure Update(const Data: TRoutineEditData;
      const AllRoutines: TRoutineList);
      {Updates snippet's properties and references.
        @param Data [in] New property values and references.
        @param AllRoutines [in] List of all snippets in database.
      }
    function GetEditData: TRoutineEditData;
      {Gets details of all editable data of snippet.
        @return Required editable properties and references.
      }
    function GetProps: TRoutineData;
      {Gets details of snippet's properties.
        @return Record containing property values.
      }
    function GetReferences: TRoutineReferences;
      {Gets details of snippet's references.
        @return Information sufficient to define references.
      }
  end;

  {
  TTempRoutine:
    Special subclass of TRoutineEx that can't be added to the Snippets object.
    Class does nothing, simply provides a class name for testing when a snippet
    is added to a TRoutineListEx. TTempRoutine can be added to a normal snippet
    list.
  }
  TTempRoutine = class(TRoutineEx);

  {
  TRoutineListEx:
    Private extension of TRoutineList for use internally by Snippets object.
  }
  TRoutineListEx = class(TRoutineList)
  public
    function Add(const Routine: TRoutine): Integer; override;
      {Adds a snippet to list. Prevents TTempRoutine instances from being added.
        @param Routine [in] Snippet to be added.
        @return Index where snippet was added to list.
        @except EBug raised if Routine is a TTempRoutine instance.
      }
    procedure Delete(const Routine: TRoutine);
      {Deletes a snippet from list.
        @param Routine [in] Snippet to be deleted. No action taken if snippet
          not in list.
      }
  end;

  {
  TCategoryEx:
    Private extension of TCategory for use internally by Snippets object. This
    version does nothing except provide a placeholder for any future extended
    object.
  }
  TCategoryEx = class(TCategory);

  {
  TSnippets:
    Class that encapsulates the main and user databases. Provides access to all
    snippets and all categories via the ISnippets interface. Also enables user
    defined database to be modified via ISnippetsEdit interface.
  }
  TSnippets = class(TInterfacedObject,
    ISnippets,
    ISnippetsEdit
  )
  strict private
    fUpdated: Boolean;                // Flags if user database has been updated
    fCategories: TCategoryList;       // List of categories
    fRoutines: TRoutineList;          // List of snippets
    fChangeEvents: TMulticastEvents;  // List of change event handlers
    type
      {
      TEventInfo:
        Class that provides information about a change event.
      }
      TEventInfo = class(TInterfacedObject, ISnippetChangeEventInfo)
      strict private
        fKind: TSnippetChangeEventKind; // Kind of event
        fRoutine: TRoutine;             // Reference to any affected snippet
      protected
        { ISnippetChangeEventInfo methods }
        function GetKind: TSnippetChangeEventKind;
          {Gets kind (type) of event.
            @return Event kind.
          }
        function GetRoutine: TRoutine;
          {Gets reference to snippet referred to by event.
            @return Snippet reference. Not defined for some event kinds.
          }
      public
        constructor Create(const Kind: TSnippetChangeEventKind;
          const Routine: TRoutine = nil);
          {Class constructor. Creates an event information object.
            @param Kind [in] Kind of event.
            @param Routine [in] Reference to any snippet affected by event. May
              be nil if event does not affect a snippet.
          }
      end;
    procedure TriggerEvent(const Kind: TSnippetChangeEventKind;
      const Routine: TRoutine = nil);
      {Triggers a change event. Notifies all registered listeners.
        @param Kind [in] Kind of event.
        @param Routine [in] Reference to any snippet affected by event. May be
          nil.
      }
    function InternalAddRoutine(const RoutineName: string;
      const Data: TRoutineEditData): TRoutine;
      {Adds a new snippet to the user database. Assumes snippet not already in
      user database.
        @param RoutineName [in] Name of new snippet.
        @param Data [in] Properties and references of new snippet.
        @return Reference to new snippet object.
        @except Exception raised if snippet's category does not exist.
      }
    procedure InternalDeleteRoutine(const Routine: TRoutine);
      {Deletes a snippet from the user database.
        @param Routine [in] Snippet to delete from database.
      }
    procedure GetDependentList(const ARoutine: TRoutine;
      const List: TRoutineList);
      {Builds a list of all snippets that depend on a specified snippet.
        @param ARoutine [in] Snippet for which dependents are required.
        @param List [in] Receives list of dependent snippets.
      }
    procedure GetReferrerList(const ARoutine: TRoutine;
      const List: TRoutineList);
      {Builds list of all snippets that cross reference a specified snippet.
        @param ARoutine [in] The cross referenced snippet.
        @param List [in] Receives list of cross referencing snippets.
      }
  protected
    { ISnippets methods }
    function GetCategories: TCategoryList;
      {Gets list of all categories in database.
        @return Required list.
      }
    function GetRoutines: TRoutineList;
      {Gets list of all snippets in database.
        @return Required list.
      }
    procedure Load;
      {Loads object's data from main and user defined databases.
      }
    procedure Clear;
      {Clears the object's data.
      }
    procedure AddChangeEventHandler(const Handler: TNotifyEventInfo);
      {Adds a change event handler to list of listeners.
        @param Handler [in] Event handler to be added.
      }
    procedure RemoveChangeEventHandler(const Handler: TNotifyEventInfo);
      {Removes a change event handler from list of listeners.
        @param Handler [in] Handler to remove from list.
      }
    { ISnippetsEdit methods }
    function GetEditableRoutineInfo(
      const Routine: TRoutine = nil): TRoutineEditData;
      {Provides details of all a snippet's data (properties and references) that
      may be edited.
        @param Routine [in] Snippet for which data is required. May be nil in
          which case a blank record is returned.
        @return Required data.
      }
    function GetDependents(const Routine: TRoutine): ISnippetIDList;
      {Builds an ID list of all snippets that depend on a specified snippet.
        @param Routine [in] Snippet for which dependents are required.
        @return List of IDs of dependent snippets.
      }
    function GetReferrers(const Routine: TRoutine): ISnippetIDList;
      {Builds an ID list of all snippets that cross reference a specified
      snippet.
        @param Routine [in] Snippet which is cross referenced.
        @return List of IDs of referring snippets.
      }
    function UpdateRoutine(const Routine: TRoutine;
      const Data: TRoutineEditData; const NewName: string = ''): TRoutine;
      {Updates a user defined snippet's properties and references using provided
      data.
        @param Routine [in] Snippet to be updated. Must be user-defined.
        @param Data [in] Record containing revised data.
        @param NewName [in] New name of snippet. Set to '' or Routine.Name if
          name is not to change.
        @return Reference to updated snippet. Will have changed.
      }
    function AddRoutine(const RoutineName: string;
      const Data: TRoutineEditData): TRoutine;
      {Adds a new snippet to the user database.
        @param RoutineName [in] Name of new snippet.
        @param Data [in] Record storing new snippet's properties and references.
        @return Reference to new snippet.
      }
    function CreateTempRoutine(const RoutineName: string;
      const Data: TRoutineEditData): TRoutine; overload;
      {Creates a new temporary user defined snippet without adding it to the
      Snippets object's snippets list. The new instance may not be added to the
      Snippets object.
        @param RoutineName [in] Name of new snippet.
        @param Data [in] Record storing new snippet's properties and references.
        @return Reference to new snippet.
      }
    function CreateTempRoutine(const Routine: TRoutine): TRoutine; overload;
      {Creates a new temporary copy of a snippet without adding it to the
      Snippets object's snippets list. The new instance may not be added to the
      Snippets object.
        @param Routine [in] Snippet to be copied.
        @return Reference to new snippet.
      }
    procedure DeleteRoutine(const Routine: TRoutine);
      {Deletes a snippet from the user database.
        @param Routine [in] Snippet to be deleted.
      }
    function Updated: Boolean;
      {Checks if user database has been updated since last save.
        @return True if database has been updated, False otherwise.
      }
    procedure Save;
      {Saves user defined snippets and all categories to user database.
      }
  public
    constructor Create;
      {Class constructor. Sets up new empty object.
      }
    destructor Destroy; override;
      {Class destructor. Tidies up and tears down object.
      }
  end;

  {
  TUserDataProvider:
    Class that provides data about the categories and snippets in the user-
    defined database.
  }
  TUserDataProvider = class(TInterfacedObject, ISnippetsDataProvider)
  strict private
    fRoutines: TRoutineList;    // All snippets in the whole database
    fCategories: TCategoryList; // All categories in the whole database
  public
    constructor Create(const Routines: TRoutineList;
      const Categories: TCategoryList);
      {Class constructor. Records list of all snippets and categories in both
      databases.
        @param Routines [in] List of all snippets in whole database.
        @param Categories [in] List of all categories in whole database.
      }
    { ISnippetsDataProvider methods }
    function GetCategoryProps(const Cat: TCategory): TCategoryData;
      {Retrieves all the properties of a category.
        @param Cat [in] Category for which data is requested.
        @return Record containing property data.
      }
    function GetCategoryRoutines(const Cat: TCategory): IStringList;
      {Retrieves names of all user-defined snippets that belong to a category.
        @param Cat [in] Category for which snippet names are requested.
        @return Required list of snippet names.
      }
    function GetRoutineProps(const Routine: TRoutine): TRoutineData;
      {Retrieves all the properties of a snippet.
        @param Routine [in] Snippet for which data is requested.
        @return Record containing property data.
      }
    function GetRoutineRefs(const Routine: TRoutine): TRoutineReferences;
      {Retrieves information about all the references of a snippet.
        @param Routine [in] Snippet for which information is requested.
        @return Record containing references.
      }
  end;

  {
  TSnippetIDListEx:
    Extension of TSnippetIDList that provides an additional constructor that can
    create a snippet ID list from a TRoutineList.
  }
  TSnippetIDListEx = class(TSnippetIDList)
  public
    constructor Create(const Routines: TRoutineList); overload;
      {Class constructor overload that creates a snippets ID list from a
      TRoutineList object.
        @param Routines [in] List of snippets objects for which ID list is
          required.
      }
  end;

function Snippets: ISnippets;
  {Returns singleton instance of object that encapsulates main and user
  databases.
    @return Singleton object.
  }
begin
  if not Assigned(PvtSnippets) then
    PvtSnippets := TSnippets.Create;
  Result := PvtSnippets;
end;

{ TSnippets }

procedure TSnippets.AddChangeEventHandler(const Handler: TNotifyEventInfo);
  {Adds a change event handler to list of listeners.
    @param Handler [in] Event handler to be added.
  }
begin
  fChangeEvents.AddHandler(Handler);
end;

function TSnippets.AddRoutine(const RoutineName: string;
  const Data: TRoutineEditData): TRoutine;
  {Adds a new snippet to the user database.
    @param RoutineName [in] Name of new snippet.
    @param Data [in] Record storing new snippet's properties and references.
    @return Reference to new snippet.
  }
resourcestring
  // Error message
  sNameExists = 'Snippet "%s" already exists in user database';
begin
  Result := nil;  // keeps compiler happy
  TriggerEvent(evChangeBegin);
  try
    // Check if snippet with same name exists in user database: error if so
    if fRoutines.Find(RoutineName, True) <> nil then
      raise ECodeSnip.CreateFmt(sNameExists, [RoutineName]);
    Result := InternalAddRoutine(RoutineName, Data);
    TriggerEvent(evRoutineAdded, Result);
  finally
    fUpdated := True;
    TriggerEvent(evChangeEnd);
  end;
end;

procedure TSnippets.Clear;
  {Clears the object's data.
  }
begin
  fCategories.Clear;
  fRoutines.Clear;
end;

constructor TSnippets.Create;
  {Class constructor. Sets up new empty object.
  }
begin
  inherited Create;
  fRoutines := TRoutineListEx.Create(True);
  fCategories := TCategoryList.Create;
  fChangeEvents := TMultiCastEvents.Create(Self);
end;

function TSnippets.CreateTempRoutine(const Routine: TRoutine): TRoutine;
  {Creates a new temporary copy of a snippet without adding it to the
  Snippets object's snippets list. The new instance may not be added to the
  Snippets object.
    @param Routine [in] Snippet to be copied.
    @return Reference to new snippet.
  }
var
  Data: TRoutineEditData; // data describing snippet's properties and references
begin
  Assert(Assigned(Routine), ClassName + '.CreateTempRoutine: Routine is nil');
  Assert(Routine is TRoutineEx,
    ClassName + '.CreateTempRoutine: Routine is not a TRoutineEx');
  Data := (Routine as TRoutineEx).GetEditData;
  Result := TTempRoutine.Create(
    Routine.Name, Routine.UserDefined, (Routine as TRoutineEx).GetProps);
  (Result as TTempRoutine).UpdateRefs(
    (Routine as TRoutineEx).GetReferences, fRoutines
  );
end;

function TSnippets.CreateTempRoutine(const RoutineName: string;
  const Data: TRoutineEditData): TRoutine;
  {Creates a new temporary user defined snippet without adding it to the
  Snippets object's snippets list. The new instance may not be added to the
  Snippets object.
    @param RoutineName [in] Name of new snippet.
    @param Data [in] Record storing new snippet's properties and references.
    @return Reference to new snippet.
  }
begin
  Result := TTempRoutine.Create(RoutineName, True, Data.Props);
  (Result as TTempRoutine).UpdateRefs(Data.Refs, fRoutines);
end;

procedure TSnippets.DeleteRoutine(const Routine: TRoutine);
  {Deletes a snippet from the user database.
    @param Routine [in] Snippet to be deleted.
  }
var
  Dependent: TRoutine;      // loops thru each snippet that depends on Routine
  Dependents: TRoutineList; // list of dependent snippets
  Referrer: TRoutine;       // loops thru snippets that cross references Routine
  Referrers: TRoutineList;  // list of referencing snippets
begin
  Assert(Routine.UserDefined,
    ClassName + '.DeleteRoutine: Routine is not user-defined');
  Assert(fRoutines.Contains(Routine),
    ClassName + '.DeleteRoutine: Routine is not in the database');
  TriggerEvent(evChangeBegin);
  // Get list of referencing and dependent routines
  Dependents := nil;
  Referrers := nil;
  try
    Dependents := TRoutineList.Create;
    GetDependentList(Routine, Dependents);
    Referrers := TRoutineList.Create;
    GetReferrerList(Routine, Referrers);
    // Delete routine for XRef or Depends list of referencing snippets
    for Referrer in Referrers do
      (Referrer.XRef as TRoutineListEx).Delete(Routine);
    for Dependent in Dependents do
      (Dependent.Depends as TRoutineListEx).Delete(Routine);
    // Delete routine itself
    InternalDeleteRoutine(Routine);
    TriggerEvent(evRoutineDeleted);
  finally
    FreeAndNil(Referrers);
    FreeAndNil(Dependents);
    fUpdated := True;
    TriggerEvent(evChangeEnd);
  end;
end;

destructor TSnippets.Destroy;
  {Class destructor. Tidies up and tears down object.
  }
begin
  FreeAndNil(fChangeEvents);
  FreeAndNil(fCategories);
  FreeAndNil(fRoutines);
  inherited;
end;

function TSnippets.GetCategories: TCategoryList;
  {Gets list of all categories in database.
    @return Required list.
  }
begin
  Result := fCategories;
end;

procedure TSnippets.GetDependentList(const ARoutine: TRoutine;
  const List: TRoutineList);
  {Builds a list of all snippets that depend on a specified snippet.
    @param ARoutine [in] Snippet for which dependents are required.
    @param List [in] Receives list of dependent snippets.
  }
var
  Routine: TRoutine;  // references each snippet in database
begin
  List.Clear;
  for Routine in fRoutines do
    if not Routine.IsEqual(ARoutine) and Routine.Depends.Contains(ARoutine) then
      List.Add(Routine);
end;

function TSnippets.GetDependents(const Routine: TRoutine): ISnippetIDList;
  {Builds an ID list of all snippets that depend on a specified snippet.
    @param Routine [in] Snippet for which dependents are required.
    @return List of IDs of dependent snippets.
  }
var
  List: TRoutineList; // list of dependent snippets
begin
  List := TRoutineList.Create;
  try
    GetDependentList(Routine, List);
    Result := TSnippetIDListEx.Create(List);
  finally
    FreeAndNil(List);
  end;
end;

function TSnippets.GetEditableRoutineInfo(
  const Routine: TRoutine): TRoutineEditData;
  {Provides details of all a snippet's data (properties and references) that may
  be edited.
    @param Routine [in] Snippet for which data is required. May be nil in which
      case a blank record is returned.
    @return Required data.
  }
begin
  Assert(not Assigned(Routine) or Routine.UserDefined,
    ClassName + '.GetEditableRoutineInfo: Routine is not user-defined');
  if Assigned(Routine) then
    Result := (Routine as TRoutineEx).GetEditData
  else
    Result.Init;
end;

function TSnippets.GetReferrers(const Routine: TRoutine): ISnippetIDList;
  {Builds an ID list of all snippets that cross reference a specified
  snippet.
    @param Routine [in] Snippet which is cross referenced.
    @return List of IDs of referring snippets.
  }
var
  List: TRoutineList; // list of referring snippets
begin
  List := TRoutineList.Create;
  try
    GetReferrerList(Routine, List);
    Result := TSnippetIDListEx.Create(List);
  finally
    FreeAndNil(List);
  end;
end;

procedure TSnippets.GetReferrerList(const ARoutine: TRoutine;
  const List: TRoutineList);
  {Builds list of all snippets that cross reference a specified snippet.
    @param ARoutine [in] The cross referenced snippet.
    @param List [in] Receives list of cross referencing snippets.
  }
var
  Routine: TRoutine;  // references each routine in database
begin
  List.Clear;
  for Routine in fRoutines do
    if not Routine.IsEqual(ARoutine) and Routine.XRef.Contains(ARoutine) then
      List.Add(Routine);
end;

function TSnippets.GetRoutines: TRoutineList;
  {Gets list of all snippets in database.
    @return Required list.
  }
begin
  Result := fRoutines;
end;

function TSnippets.InternalAddRoutine(const RoutineName: string;
  const Data: TRoutineEditData): TRoutine;
  {Adds a new snippet to the user database. Assumes snippet not already in user
  database.
    @param RoutineName [in] Name of new snippet.
    @param Data [in] Properties and references of new snippet.
    @return Reference to new snippet object.
    @except Exception raised if snippet's category does not exist.
  }
var
  Cat: TCategory; // category object containing new snippet
resourcestring
  // Error message
  sCatNotFound = 'Category "%0:s" for new snippet "%1:s" does not exist';
begin
  Result := TRoutineEx.Create(RoutineName, True, Data.Props);
  (Result as TRoutineEx).UpdateRefs(Data.Refs, fRoutines);
  Cat := fCategories.Find(Result.Category);
  if not Assigned(Cat) then
    raise ECodeSnip.CreateFmt(sCatNotFound, [Result.Category, Result.Name]);
  Cat.Routines.Add(Result);
  fRoutines.Add(Result);
end;

procedure TSnippets.InternalDeleteRoutine(const Routine: TRoutine);
  {Deletes a snippet from the user database.
    @param Routine [in] Snippet to delete from database.
  }
var
  Cat: TCategory; // category containing snippet
begin
  // Delete from category if found
  Cat := fCategories.Find(Routine.Category);
  if Assigned(Cat) then
    (Cat.Routines as TRoutineListEx).Delete(Routine);
  // Delete from "master" list: this frees Routine
  (fRoutines as TRoutineListEx).Delete(Routine);
end;

procedure TSnippets.Load;
  {Loads object's data from main and user defined databases.
  }
var
  Factory: ISnippetsFactory;  // object reader uses to create snippets objects
begin
  Clear;
  // Create factory that reader calls into to create category and snippet
  // objects. This is done to keep updating of snippet and categories private
  // to this unit
  Factory := TSnippetsFactory.Create;
  try
    // Load main database: MUST do this first since user database can
    // reference objects in main database
    with TSnippetsIOFactory.CreateLoader(dtMain) do
      Load(fRoutines, fCategories, Factory);
    // Load any user database
    with TSnippetsIOFactory.CreateLoader(dtUser) do
      Load(fRoutines, fCategories, Factory);
    fUpdated := False;
  except
    // If an exception occurs clear the database
    Clear;
    raise;
  end;
end;

procedure TSnippets.RemoveChangeEventHandler(const Handler: TNotifyEventInfo);
  {Removes a change event handler from list of listeners.
    @param Handler [in] Handler to remove from list.
  }
begin
  fChangeEvents.AddHandler(Handler);
end;

procedure TSnippets.Save;
  {Saves user defined snippets and all categories to user database.
  }
var
  Provider: ISnippetsDataProvider;  // object that supplies info to writer
begin
  // Create object that can provide required information about user database
  Provider := TUserDataProvider.Create(fRoutines, fCategories);
  // Use a writer object to write out the database
  with TSnippetsIOFactory.CreateWriter do
    Write(fRoutines, fCategories, Provider);
  fUpdated := False;
end;

procedure TSnippets.TriggerEvent(const Kind: TSnippetChangeEventKind;
  const Routine: TRoutine);
  {Triggers a change event. Notifies all registered listeners.
    @param Kind [in] Kind of event.
    @param Routine [in] Reference to any snippet affected by event. May be nil.
  }
var
  EvtInfo: ISnippetChangeEventInfo; // event information object
begin
  EvtInfo := TEventInfo.Create(Kind, Routine);
  fChangeEvents.TriggerEvents(EvtInfo);
end;

function TSnippets.Updated: Boolean;
  {Checks if user database has been updated since last save.
    @return True if database has been updated, False otherwise.
  }
begin
  Result := fUpdated;
end;

function TSnippets.UpdateRoutine(const Routine: TRoutine;
  const Data: TRoutineEditData; const NewName: string): TRoutine;
  {Updates a user defined snippet's properties and references using provided
  data.
    @param Routine [in] Snippet to be updated. Must be user-defined.
    @param Data [in] Record containing revised data.
    @param NewName [in] New name of snippet. Set to '' or Routine.Name if name
      is not to change.
    @return Reference to updated snippet. Will have changed.
  }
var
  RoutineName: string;      // name of snippet
  Dependent: TRoutine;      // loops thru each snippetthat depends on Routine
  Dependents: TRoutineList; // list of dependent snippets
  Referrer: TRoutine;       // loops thru snippets that cross references Routine
  Referrers: TRoutineList;  // list of referencing snippets
resourcestring
  // Error message
  sCantRename = 'Can''t rename snippet %0:s to %1:s: Snippet %1:s already '
    + 'exists in user database';
begin
  Result := Routine;      // keeps compiler happy
  Assert(Routine.UserDefined,
    ClassName + '.UpdateRoutine: Routine is not user-defined');
  TriggerEvent(evChangeBegin);
  try
    // Calculate new name
    if NewName <> '' then
      RoutineName := NewName
    else
      RoutineName := Routine.Name;
    // If name has changed then new name musn't exist in user database
    if RoutineName <> Routine.Name then
      if fRoutines.Find(RoutineName, True) <> nil then
        raise ECodeSnip.CreateFmt(sCantRename, [Routine.Name, RoutineName]);
    // We update by deleting old snippet and inserting new one
    // get lists of snippets that cross reference or depend on this routine
    Dependents := TRoutineList.Create;
    GetDependentList(Routine, Dependents);
    Referrers := TRoutineList.Create;
    GetReferrerList(Routine, Referrers);
    // remove invalid references from referring snippets
    for Referrer in Referrers do
      (Referrer.XRef as TRoutineListEx).Delete(Routine);
    for Dependent in Dependents do
      (Dependent.Depends as TRoutineListEx).Delete(Routine);
    // delete the snippet
    InternalDeleteRoutine(Routine);
    // add new routine
    Result := InternalAddRoutine(RoutineName, Data);
    // add new snippet to referrer list of referring snippets
    for Referrer in Referrers do
      Referrer.XRef.Add(Result);
    for Dependent in Dependents do
      Dependent.Depends.Add(Result);
    TriggerEvent(evRoutineChanged, Result);
  finally
    fUpdated := True;
    TriggerEvent(evChangeEnd);
  end;
end;

{ TSnippets.TEventInfo }

constructor TSnippets.TEventInfo.Create(const Kind: TSnippetChangeEventKind;
  const Routine: TRoutine);
  {Class constructor. Creates an event information object.
    @param Kind [in] Kind of event.
    @param Routine [in] Reference to any snippet affected by event. May be nil
      if event does not affect a snippet.
  }
begin
  inherited Create;
  fKind := Kind;
  fRoutine := Routine;
end;

function TSnippets.TEventInfo.GetKind: TSnippetChangeEventKind;
  {Gets kind (type) of event.
    @return Event kind.
  }
begin
  Result := fKind;
end;

function TSnippets.TEventInfo.GetRoutine: TRoutine;
  {Gets reference to snippet referred to by event.
    @return Snippet reference. Not defined for some event kinds.
  }
begin
  Result := fRoutine;
end;

{ TRoutine }

function TRoutine.CanCompile: Boolean;
  {Checks if snippet can be compiled.
    @return True if compilation supported and False if not.
  }
begin
  Result := Kind <> skFreeform;
end;

constructor TRoutine.Create(const Name: string; const UserDefined: Boolean;
  const Props: TRoutineData);
  {Class contructor. Sets up snippet object with given property values.
    @param Name [in] Name of snippet.
    @param UserDefined [in] Indicates if this is a user defined snippet.
    @param Props [in] Values of various snippet properties.
  }
begin
  Assert(ClassType <> TRoutine,
    ClassName + '.Create: must only be called from descendants.');
  inherited Create;
  // Record simple property values
  SetName(Name);
  SetProps(Props);
  // Create string list to store required units
  fUnits := TStringList.Create;
  // Create routine lists for Depends and XRef properties
  fDepends := TRoutineListEx.Create;
  fXRef := TRoutineListEx.Create;
  // The following properties added to support user defined snippets
  fUserDefined := UserDefined;
end;

destructor TRoutine.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fXRef);
  FreeAndNil(fDepends);
  FreeAndNil(fUnits);
  fExtra := nil;
  inherited;
end;

function TRoutine.GetID: TSnippetID;
  {Gets snippet's unique ID.
    @return Required ID.
  }
begin
  Result := TSnippetID.Create(fName, fUserDefined);
end;

function TRoutine.IsEqual(const Routine: TRoutine): Boolean;
  {Checks if this snippet is same as another snippet. Snippets are considered
  equal if they have the same name and come from same database.
    @param Routine [in] Snippet being compared.
    @return True if snippets are equal, False if not.
  }
begin
  Result := Routine.ID = Self.ID;
end;

procedure TRoutine.SetName(const Name: string);
  {Sets Name property.
    @param Name [in] New name.
  }
begin
  fName := Name;
end;

procedure TRoutine.SetProps(const Data: TRoutineData);
  {Sets snippet's properties.
    @param Data [in] Record containing property values.
  }
begin
  fCategory := Data.Cat;
  fKind := Data.Kind;
  fDescription := Data.Desc;
  fSourceCode := AdjustLineBreaks(Data.SourceCode); // ensure EOLs are CRLF
  fExtra := TActiveTextFactory.CloneActiveText(Data.Extra);
  fCompatibility := Data.CompilerResults;
end;

{ TRoutineEx }

function TRoutineEx.GetEditData: TRoutineEditData;
  {Gets details of all editable data of snippet.
    @return Required editable properties and references.
  }
begin
  Result.Props := GetProps;
  Result.Refs := GetReferences;
end;

function TRoutineEx.GetProps: TRoutineData;
  {Gets details of snippet's properties.
    @return Record containing property values.
  }
begin
  Result.Cat := Category;
  Result.Kind := Kind;
  Result.Desc := Description;
  Result.SourceCode := SourceCode;
  Result.Extra := TActiveTextFactory.CloneActiveText(Extra);
  Result.CompilerResults := Compatibility;
end;

function TRoutineEx.GetReferences: TRoutineReferences;
  {Gets details of snippet's references.
    @return Information sufficient to define references.
  }
begin
  Result.Units := TIStringList.Create(Units);
  Result.Depends := TSnippetIDListEx.Create(Depends);
  Result.XRef := TSnippetIDListEx.Create(XRef);
end;

procedure TRoutineEx.Update(const Data: TRoutineEditData;
  const AllRoutines: TRoutineList);
  {Updates snippet's properties and references.
    @param Data [in] New property values and references.
    @param AllRoutines [in] List of all snippets in database.
  }
begin
  SetProps(Data.Props);
  UpdateRefs(Data.Refs, AllRoutines);
end;

procedure TRoutineEx.UpdateRefs(const Refs: TRoutineReferences;
  const AllRoutines: TRoutineList);
  {Updates a snippet's references.
    @param Refs [in] Stores all snippet's references (XRef, Depends and
      Units).
    @param AllRoutines [in] List of all snippets in database.
  }

  // ---------------------------------------------------------------------------
  procedure BuildRoutineList(const SL: TRoutineList;
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
    Routine: TRoutine;  // references each snippet identified by ID
  begin
    SL.Clear;
    for ID in IDList do
    begin
      Routine := AllRoutines.Find(ID.Name, ID.UserDefined);
      if Assigned(Routine) then
        SL.Add(Routine);
    end;
  end;
  // ---------------------------------------------------------------------------

begin
  Refs.Units.CopyTo(Self.Units, True);            // copy units
  BuildRoutineList(Self.Depends, Refs.Depends);   // build Depends list
  BuildRoutineList(Self.XRef, Refs.XRef);         // build XRef list
end;

{ TRoutineList }

function TRoutineList.Add(const Routine: TRoutine): Integer;
  {Adds new snippet to the list, maintaining list in alphabetical order.
    @param Routine [in] Snippet being added.
    @return Index where item was inserted in list
    @except Raised if duplicate snippet added to list.
  }
var
  Idx: Integer; // loops thru list
  Cmp: Integer; // result of comparison of snippet names
begin
  // Assume adding to end of list
  Result := Count;
  // Loop thru existing items searching for location to insert
  for Idx := 0 to Pred(Count) do
  begin
    // compare new item to current item
    Cmp := CompareIDs(Routine.ID, Items[Idx].ID);
    if Cmp = 0 then
      // don't allow duplicates
      raise EBug.CreateFmt(
        ClassName + '.Add: Duplicate snippet name (%s) in same database',
        [Routine.Name]
      );
    if Cmp < 0 then
    begin
      // current item is greater than new one: insert before it
      Result := Idx;
      Break;
    end;
  end;
  // add the new item
  fList.Insert(Result, Routine);
end;

procedure TRoutineList.Assign(const SrcList: TRoutineList);
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

procedure TRoutineList.Clear;
  {Clears the list.
  }
begin
  fList.Clear;
end;

function TRoutineList.CompareIDs(const SID1, SID2: TSnippetID): Integer;
  {Compares IDS of two snippets.
    @param SID1 [in] First ID to be compared.
    @param SID2 [in] Second ID to be compared.
    @return 0 if IDs are the same, -ve if SID1 < SID2 and +ve if
      SID1 > SID2.
  }
begin
  Result := SID1.Compare(SID2);
end;

function TRoutineList.Contains(const Routine: TRoutine): Boolean;
  {Checks whether list contains a specified snippet.
    @param Routine [in] Required snippet.
    @return True if snippet is in list, False otherwise.
  }
begin
  Result := fList.IndexOf(Routine) >= 0;
end;

function TRoutineList.Count(const UserDefined: Boolean): Integer;
  {Counts number of snippets in list that are either from or not from user
  defined database.
    @param UserDefined [in] Flags whether to count snippets in user database
      (True) or in main database (False).
    @return Number of snippets in specified database.
  }
var
  Routine: TRoutine;  // refers to all snippets in list
begin
  Result := 0;
  for Routine in Self do
    if Routine.UserDefined = UserDefined then
      Inc(Result);
end;

function TRoutineList.Count: Integer;
  {Counts number of snippets in list.
    @return Number of snippets in list.
  }
begin
  Result := fList.Count;
end;

constructor TRoutineList.Create(const OwnsObjects: Boolean = False);
  {Class constructor. Creates a new empty list.
    @param OwnsObjects [in] Specifies whether list owns the snippet objects it
      contains. If True the snippets are freed when deleted from list.
  }
begin
  inherited Create;
  fList := TObjectList.Create(OwnsObjects);
end;

destructor TRoutineList.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fList);  // destroys owned snippets if OwnsObjects=True
  inherited;
end;

function TRoutineList.Find(const RoutineName: string;
  const UserDefined: Boolean; out Index: Integer): Boolean;
  {Finds a snippet in the list that has a specified name and user defined
  property. Uses a binary search.
    @param RoutineName [in] Name of snippet to be found.
    @param UserDefined [in] Whether required snippet is user defined or not.
    @param Index [out] Index of required snippet in list. Valid only if
      method returns True.
    @return True if snippet found, False if not.
  }
var
  Low, High: Integer;   // low and high bounds of search
  Cur: Integer;         // current item to be tested
  Cmp: Integer;         // result of comparing two items
  SearchID: TSnippetID; // ID of snippet being search for
begin
  // Uses binary search: based on TStringList.Find from Delphi Classes unit
  // Initialise
  Result := False;
  Low := 0;
  High := Count - 1;
  SearchID := TSnippetID.Create(RoutineName, UserDefined);
  // Perform binary search
  while Low <= High do
  begin
    // Choose item to be compared: mid point of range
    Cur := (Low + High) shr 1;
    // Compare chosen
    Cmp := CompareIDs(Items[Cur].ID, SearchID);
    if Cmp < 0 then
      // Current item < required: search above current item
      Low := Cur + 1
    else
    begin
      // Current item >= required: search below current item
      High := Cur - 1;
      if Cmp = 0 then
      begin
        // We found it: set Low to Cur because Cur > High => loop will terminate
        Result := True;
        Low := Cur;
      end;
    end;
  end;
  // Set index (Low = Cur if found)
  Index := Low;
end;

function TRoutineList.Find(const RoutineName: string;
  const UserDefined: Boolean): TRoutine;
  {Finds a named snippet in list with a matching user defined property.
    @param RoutineName [in] Name of required snippet.
    @param UserDefined [in] Flag that determines if we are looking for a
      user defined snippet or one from main database.
    @return Reference to object representing snippet in list or nil if not
      in list.
  }
var
  Idx: Integer; // index of snippet name in list
begin
  if Find(RoutineName, UserDefined, Idx) then
    Result := Items[Idx]
  else
    Result := nil;
end;

function TRoutineList.GetEnumerator: TEnumerator;
  {Gets an intialised snippet list enumerator.
    @return Required enumerator.
  }
begin
  Result := TEnumerator.Create(Self);
end;

function TRoutineList.GetItem(Idx: Integer): TRoutine;
  {Read accessor for Items property.
    @param Idx [in] Index of required snippet in list.
    @return Snippet at specified index in list.
  }
begin
  Result := fList[Idx] as TRoutine;
end;

function TRoutineList.IsEqual(const AList: TRoutineList): Boolean;
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
    // Same number of snippets: scan list checking routine names same. We can
    // rely on items being in same order since lists are sorted
    for Idx := 0 to Pred(Self.Count) do
    begin
      if Self[Idx].IsEqual(AList[Idx]) then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

{ TRoutineList.TEnumerator }

constructor TRoutineList.TEnumerator.Create(const RL: TRoutineList);
  {Class constructor. Sets up and initialises enumeration.
    @param RL [in] Reference to snippet list object to be enumerated.
  }
begin
  inherited Create;
  fRoutineList := RL;
  fIndex := -1;
end;

function TRoutineList.TEnumerator.GetCurrent: TRoutine;
  {Gets current snippet in enumeration. Error if at end of enumeration.
    @return Current snippet.
  }
begin
  Result := fRoutineList[fIndex];
end;

function TRoutineList.TEnumerator.MoveNext: Boolean;
  {Moves to next item in enumeration.
    @return True if there is a next item, False if enumeration
      completed.
  }
begin
  Result := fIndex < Pred(fRoutineList.Count);
  if Result then
    Inc(fIndex);
end;

{ TRoutineListEx }

function TRoutineListEx.Add(const Routine: TRoutine): Integer;
  {Adds a snippet to list. Prevents TTempRoutine instances from being added.
    @param Routine [in] Snippet to be added.
    @return Index where snippet was added to list.
    @except EBug raised if Routine is a TTempRoutine instance.
  }
begin
  if Routine is TTempRoutine then
    // don't allow temp routines to be added to list
    raise EBug.CreateFmt(
      ClassName + '.Add: Can''t add temporary snippets to database (%s)',
      [Routine.Name]
    );
  Result := inherited Add(Routine);
end;

procedure TRoutineListEx.Delete(const Routine: TRoutine);
  {Deletes a snippet from list.
    @param Routine [in] Snippet to be deleted. No action taken if snippet not in
      list.
  }
var
  Idx: Integer; // index of snippet in list.
begin
  Idx := fList.IndexOf(Routine);
  if Idx = -1 then
    Exit;
  fList.Delete(Idx);  // this frees snippet if list owns objects
end;

{ TCategory }

constructor TCategory.Create(const CatID: string; const UserDefined: Boolean;
  const Data: TCategoryData);
  {Class contructor. Sets up category object with given property values.
    @param Data [in] Contains required property values.
  }
begin
  Assert(ClassType <> TCategory,
    ClassName + '.Create: must only be called from descendants.');
  inherited Create;
  fCategory := CatID;
  fDescription := Data.Desc;
  fUserDefined := UserDefined;
  // Create list to store snippets in category
  fRoutines := TRoutineListEx.Create;
end;

destructor TCategory.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fRoutines);
  inherited;
end;

function TCategory.IsEqual(const Cat: TCategory): Boolean;
  {Checks if this category is same as another category. Categories are
  considered equal if they have the same name.
    @param Cat [in] Category being compared.
    @return True if categories are equal, False if not.
  }
begin
  Result := AnsiSameText(Self.Category, Cat.Category);
end;

{ TCategoryList }

function TCategoryList.Add(const Category: TCategory): Integer;
  {Adds new category to list.
    @param Category [in] Category to be added.
    @return Index where category inserted in list.
  }
begin
  Assert(fList.IndexOf(Category) = -1,
    ClassName + '.Add: Category already in list');
  Result := fList.Add(Category);
end;

procedure TCategoryList.Clear;
  {Clears the list.
  }
begin
  fList.Clear;
end;

constructor TCategoryList.Create;
  {Class constructor. Creates a new empty list that owns the category objects in
  it, so that detroying this object frees the category objects.
  }
begin
  inherited;
  fList := TObjectList.Create(True);
end;

destructor TCategoryList.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fList);
  inherited;
end;

function TCategoryList.Find(const CatName: string): TCategory;
  {Finds a named category in list.
    @param CatName [in] Name of required category.
    @return Reference to object representing category in list or nil if not in
      list.
  }
var
  Idx: Integer;   // loops thru categories
begin
  Result := nil;
  for Idx := 0 to Pred(Count) do
  begin
    if AnsiSameText(CatName, Items[Idx].Category) then
    begin
      Result := Items[Idx];
      Break;
    end;
  end;
end;

function TCategoryList.GetCount: Integer;
  {Read accessor for Count property.
    @return Number of categories in list.
  }
begin
  Result := fList.Count;
end;

function TCategoryList.GetEnumerator: TEnumerator;
  {Gets an intialised category list enumerator.
    @return Required enumerator.
  }
begin
  Result := TEnumerator.Create(Self);
end;

function TCategoryList.GetItem(Idx: Integer): TCategory;
  {Read accessor for Items property.
    @param Idx [in] Index of required category in list.
    @return Category at specified index in list.
  }
begin
  Result := fList[Idx] as TCategory;
end;

{ TCategoryList.TEnumerator }

constructor TCategoryList.TEnumerator.Create(const CL: TCategoryList);
  {Class constructor. Sets up and initialises enumeration.
    @param CL [in] Reference to object to be enumerated.
  }
begin
  inherited Create;
  fCategoryList := CL;
  fIndex := -1;
end;

function TCategoryList.TEnumerator.GetCurrent: TCategory;
  {Gets current category in enumeration. Error if at end of enumeration.
    @return Current category.
  }
begin
  Result := fCategoryList[fIndex];
end;

function TCategoryList.TEnumerator.MoveNext: Boolean;
  {Moves to next item in enumeration.
    @return True if there is a next item, False if enumeration completed.
  }
begin
  Result := fIndex < Pred(fCategoryList.Count);
  if Result then
    Inc(fIndex);
end;

{ TSnippetsFactory }

function TSnippetsFactory.CreateCategory(const CatID: string;
  const UserDefined: Boolean; const Data: TCategoryData): TCategory;
  {Creates a new category object.
    @param CatID [in] ID (name) of new category. Must be unique.
    @param UserDefined [in] True if category is user defined, False if not.
    @param Data [in] Record describing category's properties.
    @return Instance of new category object.
  }
begin
  Result := TCategoryEx.Create(CatID, UserDefined, Data);
end;

function TSnippetsFactory.CreateRoutine(const Name: string;
  const UserDefined: Boolean; const Props: TRoutineData): TRoutine;
  {Creates a new snippet object.
    @param Name [in] Name of new snippet. Must not exist in database specified
      by UserDefined parameter.
    @param UserDefined [in] True if snippet is user defined, False if not.
    @param Props [in] Record describing snippet's properties.
    @return Instance of new snippet with no references.
  }
begin
  Result := TRoutineEx.Create(Name, UserDefined, Props);
end;

{ TUserDataProvider }

constructor TUserDataProvider.Create(const Routines: TRoutineList;
  const Categories: TCategoryList);
  {Class constructor. Records list of all snippets and categories in both
  databases.
    @param Routines [in] List of all snippets in whole database.
    @param Categories [in] List of all categories in whole database.
  }
begin
  inherited Create;
  fRoutines := Routines;
  fCategories := Categories;
end;

function TUserDataProvider.GetCategoryProps(
  const Cat: TCategory): TCategoryData;
  {Retrieves all the properties of a category.
    @param Cat [in] Category for which data is requested.
    @return Record containing property data.
  }
begin
  Result.Desc := Cat.Description;
end;

function TUserDataProvider.GetCategoryRoutines(
  const Cat: TCategory): IStringList;
  {Retrieves names of all user-defined snippets that belong to a category.
    @param Cat [in] Category for which snippet names are requested.
    @return Required list of snippet names.
  }
var
  Routine: TRoutine;  // references each snippet in category
begin
  Result := TIStringList.Create;
  for Routine in Cat.Routines do
    if Routine.UserDefined then
      Result.Add(Routine.Name);
end;

function TUserDataProvider.GetRoutineProps(
  const Routine: TRoutine): TRoutineData;
  {Retrieves all the properties of a snippet.
    @param Routine [in] Snippet for which data is requested.
    @return Record containing property data.
  }
begin
  Result := (Routine as TRoutineEx).GetProps;
end;

function TUserDataProvider.GetRoutineRefs(
  const Routine: TRoutine): TRoutineReferences;
  {Retrieves information about all the references of a snippet.
    @param Routine [in] Snippet for which information is requested.
    @return Record containing references.
  }
begin
  Result := (Routine as TRoutineEx).GetReferences;
end;

{ TRoutineData }

procedure TRoutineData.Assign(const Src: TRoutineData);
  {Sets this record to be a copy of another record.
    @param Src [in] Record to be copied.
  }
begin
  Self.Kind := Src.Kind;
  Self.Cat := Src.Cat;
  Self.Desc := Src.Desc;
  Self.SourceCode := Src.SourceCode;
  (Self.Extra as IAssignable).Assign(Src.Extra);
  Self.CompilerResults := Src.CompilerResults;
end;

procedure TRoutineData.Init;
  {Initialises record by setting default values for fields.
  }
var
  CompID: TCompilerID;  // loops thru compiler IDs
begin
  Self.Kind := skFreeform;
  Self.Cat := '';
  Self.Desc := '';
  Self.SourceCode := '';
  Self.Extra := TActiveTextFactory.CreateActiveText;
  for CompID := Low(TCompilerID) to High(TCompilerID) do
    CompilerResults[CompID] := crQuery;
end;

{ TRoutineReferences }

procedure TRoutineReferences.Assign(const Src: TRoutineReferences);
  {Sets this record to be a copy of another record.
    @param Src [in] Record to be copied.
  }
begin
  Init;
  (Self.Units as IAssignable).Assign(Src.Units);
  (Self.Depends as IAssignable).Assign(Src.Depends);
  (Self.XRef as IAssignable).Assign(Src.XRef);
end;

procedure TRoutineReferences.Init;
  {Initialises record by creating objects stored in fields.
  }
begin
  Units := TIStringList.Create;
  Depends := TSnippetIDList.Create;
  XRef := TSnippetIDList.Create;
end;

{ TRoutineEditData }

procedure TRoutineEditData.Assign(const Src: TRoutineEditData);
  {Sets this record to be a copy of another record.
    @param Src [in] Record to be copied.
  }
begin
  Init;
  Self.Props.Assign(Src.Props);
  Self.Refs.Assign(Src.Refs);
end;

procedure TRoutineEditData.Init;
  {Initialises record by creating default values and field objects.
  }
begin
  Self.Props.Init;
  Self.Refs.Init;
end;

{ TRoutineInfo }

procedure TRoutineInfo.Assign(const Src: TRoutineInfo);
  {Sets this record's fields to be same as another TRoutineInfo record.
  Object fields are copied appropriately.
    @param Src [in] Record containing fields to be copied.
  }
begin
  Self.Name := Src.Name;
  Self.Data.Assign(Src.Data);
end;

procedure TRoutineInfo.Init;
  {Initialises record to nul values.
  }
begin
  Name := '';
  Data.Init;
end;

{ TSnippetIDListEx }

constructor TSnippetIDListEx.Create(const Routines: TRoutineList);
  {Class constructor overload that creates a snippets ID list from a
  TRoutineList object.
    @param Routines [in] List of snippets objects for which ID list is
      required.
  }
var
  Snippet: TRoutine;  // references each snippet in list
begin
  inherited Create;
  for Snippet in Routines do
    Add(Snippet.ID);
end;


initialization


finalization

// Free the singleton
PvtSnippets := nil;

end.

