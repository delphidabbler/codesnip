{
 * USnippetsIO.pas
 *
 * Implements objects that can load data into the Snippets object from both the
 * user and main databases. Also provides a class that can write the user
 * database to storage.
 *
 * v1.0 of 15 Sep 2008  - Original version.
 * v1.1 of 17 Sep 2008  - Fixed bug that caused a snippet in user database to be
 *                        ignored if it had same name as a snippet in main
 *                        database.
 * v1.2 of 04 Oct 2008  - Changed TSnippetsIOFactory to derive from
 *                        TNoConstructObject and hence prevented it from being
 *                        constructed.
 * v1.3 of 13 Jan 2009  - Replaced control char literals with constants.
 * v1.4 of 06 Jun 2009  - Added "imports" category as a default user-defined
 *                        category in addition to existing "user" category.
 *                      - Changed to use TSnippetID instead of TRoutineID and
 *                        ISnippetIDList instead of IRoutineIDList.
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
 * The Original Code is USnippetsIO.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit USnippetsIO;


interface


uses
  // Delphi
  SysUtils,
  // Project
  UBaseObjects, UExceptions, USnipData, USnippetIDs, USnippets;


type

  {
  ISnippetsLoader:
    Interface to object that can load data into the Snippets object from
    storage.
  }
  ISnippetsLoader = interface(IInterface)
    ['{C6AF94FC-F56F-44AE-9E79-3B0CD0BB21D4}']
    procedure Load(const Routines: TRoutineList;
      const Categories: TCategoryList; const SnippetsFactory: ISnippetsFactory);
      {Loads data from storage and updates snippets object.
        @param Routines [in] Receives information about each snippet in the
          database.
        @param Categories [in] Receives information about each category in the
          database.
        @param SnippetsFactory [in] Object used to create new categories and
          snippets.
      }
  end;

  {
  ISnippetsWriter:
    Interface to object that can write data from the user-defined component of
    the Snippets object to storage.
  }
  ISnippetsWriter = interface(IInterface)
    ['{F46EE2E3-68A7-4877-9E04-192D15D29BB1}']
    procedure Write(const Routines: TRoutineList;
      const Categories: TCategoryList;
      const Provider: ISnippetsDataProvider);
      {Writes data from Snippets object to storage.
        @param Routines [in] Contains information about each snippet in the
          database.
        @param Categories [in] Contains information about each category in the
          database.
        @param Privider [in] Object used to obtain details of the data to be
          written from the Snippets object
      }
  end;

  {
  TSnippetsLoaderFactory:
    Factory class that can create instances of writer and loader objects for the
    Snippets object.
  }
  TSnippetsIOFactory = class(TNoConstructObject)
  public
    class function CreateLoader(const DBType: TDatabaseType): ISnippetsLoader;
      {Creates an object that can load data into the Snippets object from
      storage. param DBType [in] Type of database to be loaded: main or user.
        @return Instance of loader object.
      }
    class function CreateWriter: ISnippetsWriter;
      {Create an object that can write user defined data from the Snippets
      object to storage.
        @return Instance of writer object.
      }
  end;

  {
  ESnippetsLoader:
    Class of exception raised by snippets loader objects.
  }
  ESnippetsLoader = class(ECodeSnip);


implementation


uses
  // Project
  UConsts, UIStringList;


type

  {
  TSnippetsLoaderClass:
    Class reference to TSnippetsLoader descendants.
  }
  TSnippetsLoaderClass = class of TSnippetsLoader;

  {
  TSnippetsLoader:
    Abstract base class for objects that can load data into the Snippets object
    from storage.
  }
  TSnippetsLoader = class(TInterfacedObject,
    ISnippetsLoader
  )
  strict private
    fReader: IDataReader;       // Object used to read data from storage
    fRoutines: TRoutineList;    // Receives list of snippets
    fCategories: TCategoryList; // Receives list of categories
    fFactory: ISnippetsFactory; // Object creates new categories and snippets
    procedure LoadRoutines(const Cat: TCategory);
      {Loads all snippets in a category.
        @param Cat [in] Category to be loaded.
      }
    procedure LoadReferences(const Routine: TRoutine);
      {Loads all of a snippet's references.
        @param Routine [in] Snippet for which references are required.
      }
    procedure HandleException(const E: Exception);
      {Handles exceptions generated by loader and converts ECodeSnip and
      descendant exceptions into ESnippetsLoader exceptions.
        @param E [in] Exception to be handled.
        @except Exception always raised.
      }
  strict protected
    function CreateReader: IDataReader; virtual; abstract;
      {Creates reader object for the database. If database doesn't exist a nul
      reader must be created.
        @return Reader object instance.
      }
    function FindRoutine(const RoutineName: string;
      const Routines: TRoutineList): TRoutine; virtual; abstract;
      {Finds the snippet object with a specified name.
        @param RoutineName [in] Name of required snippet.
        @return Reference to required snippet object or nil if snippet is not
          found.
      }
    function IsNativeRoutine(const Routine: TRoutine): Boolean;
      virtual; abstract;
      {Checks if a snippet is native (belongs) to the database being read.
        @param Routine [in] Snippet to test.
        @return True if snippet is native, False if not.
      }
    function IsUserDatabase: Boolean; virtual; abstract;
      {Checks if the database is the user database.
        @return True if the database is the user database, False if not.
      }
    function ErrorMessageHeading: string; virtual; abstract;
      {Returns heading to use in error messages. Should identify the database.
        @return Required heading.
      }
    procedure LoadCategories; virtual;
      {Loads all categories from storage.
      }
    procedure CreateCategory(const CatName: string;
      const CatData: TCategoryData);
      {Creates a new category and adds it to the categories list.
        @param CatName [in] Name of category.
        @param CatData [in] Properties of category.
      }
    property Categories: TCategoryList read fCategories;
      {Reference to category list}
  public
    { ISnippetsLoader method }
    procedure Load(const Routines: TRoutineList;
      const Categories: TCategoryList; const SnippetsFactory: ISnippetsFactory);
      {Loads data from storage and updates snippets object.
        @param Routines [in] Receives information about each snippet in the
          database.
        @param Categories [in] Receives information about each category in the
          database.
        @param SnippetsFactory [in] Object used to create new categories and
          snippets.
      }
  end;

  {
  TMainSnippetsLoader:
    Class that updates Snippets object with data read from main database.
  }
  TMainSnippetsLoader = class(TSnippetsLoader,
    ISnippetsLoader
  )
  strict protected
    function CreateReader: IDataReader; override;
      {Creates reader object. If main database doesn't exist a nul reader is
      created.
        @return Reader object instance.
      }
    function FindRoutine(const RoutineName: string;
      const Routines: TRoutineList): TRoutine; override;
      {Finds the snippet object with a specified name in the main database.
        @param RoutineName [in] Name of required snippet.
        @return Reference to required snippet object or nil if snippet is not
          found.
      }
    function IsNativeRoutine(const Routine: TRoutine): Boolean; override;
      {Checks if a snippet is native (belongs) to the main database.
        @param Routine [in] Snippet to test.
        @return True if snippet is native, False if not.
      }
    function IsUserDatabase: Boolean; override;
      {Checks if the database is the user database.
        @return False - this is not the user database.
      }
    function ErrorMessageHeading: string; override;
      {Returns heading to use in error messages. Identifies main database.
        @return Required heading.
      }
  end;

  {
  TUserSnippetsLoader:
    Class that updates Snippets object with data read from user database.
  }
  TUserSnippetsLoader = class(TSnippetsLoader,
    ISnippetsLoader
  )
  strict protected
    function CreateReader: IDataReader; override;
      {Creates reader object. If user database doesn't exist a nul reader is
      created.
        @return Reader object instance.
      }
    function FindRoutine(const RoutineName: string;
      const Routines: TRoutineList): TRoutine; override;
      {Finds the snippet object with a specified name. If snippet is not in this
      (user) database the main database is searched.
        @param RoutineName [in] Name of required snippet.
        @return Reference to required snippet object or nil if snippet is not
          found.
      }
    function IsNativeRoutine(const Routine: TRoutine): Boolean; override;
      {Checks if a snippet is native (belongs) to the user database.
        @param Routine [in] Snippet to test.
        @return True if snippet is native, False if not.
      }
    function IsUserDatabase: Boolean; override;
      {Checks if the database is the user database.
        @return True - this is the user database.
      }
    function ErrorMessageHeading: string; override;
      {Returns heading to use in error messages. Identifies main database.
        @return Required heading.
      }
    procedure LoadCategories; override;
      {Loads all categories from storage and adds user category if not present.
      }
  end;

  {
  TSnippetsWriter:
    Object used to write data from user database to storage.
  }
  TSnippetsWriter = class(TInterfacedObject,
    ISnippetsWriter
  )
  strict private
    fWriter: IDataWriter;             // Object used to write to storage
    fRoutines: TRoutineList;          // List of routines to be written
    fCategories: TCategoryList;       // List of categories to be written
    fProvider: ISnippetsDataProvider; // Object used to get data to be written
    function CreateWriter: IDataWriter;
      {Creates object that can write data for user-defined items from Snippets
      to storage.
        @return Requied writer object.
      }
    procedure WriteCategories;
      {Writes information about categories to storage.
      }
    procedure WriteRoutines;
      {Writes information about all snippets to storage.
      }
  public
    { ISnippetsWriter method }
    procedure Write(const Routines: TRoutineList;
      const Categories: TCategoryList;
      const Provider: ISnippetsDataProvider);
      {Writes data from Snippets object to storage.
        @param Routines [in] Contains information about each snippet in the
          database.
        @param Categories [in] Contains information about each category in the
          database.
        @param Privider [in] Object used to obtain details of the data to be
          written from the Snippets object
      }
  end;

{ TSnippetsIOFactory }

class function TSnippetsIOFactory.CreateLoader(
  const DBType: TDatabaseType): ISnippetsLoader;
  {Creates an object that can load data into the Snippets object from storage.
    @param DBType [in] Type of database to be loaded: main or user.
    @return Instance of loader object.
  }
const
  // Map of database types to classes used to read the database
  cLoaderClasses: array[TDatabaseType] of TSnippetsLoaderClass = (
    TMainSnippetsLoader, TUserSnippetsLoader
  );
begin
  Result := cLoaderClasses[DBType].Create;
end;

class function TSnippetsIOFactory.CreateWriter: ISnippetsWriter;
  {Create an object that can write user defined data from the Snippets object to
  storage.
    @return Instance of writer object.
  }
begin
  Result := TSnippetsWriter.Create;
end;

{ TSnippetsLoader }

procedure TSnippetsLoader.CreateCategory(const CatName: string;
  const CatData: TCategoryData);
  {Creates a new category and adds it to the categories list.
    @param CatName [in] Name of category.
    @param CatData [in] Properties of category.
  }
begin
  fCategories.Add(fFactory.CreateCategory(CatName, IsUserDatabase, CatData));
end;

procedure TSnippetsLoader.HandleException(const E: Exception);
  {Handles exceptions generated by loader and converts ECodeSnip and descendant
  exceptions into ESnippetsLoader exceptions.
    @param E [in] Exception to be handled.
    @except Exception always raised.
  }
begin
  if E is ECodeSnip then
    // add message header identifying database to existing message
    raise ESnippetsLoader.Create(ErrorMessageHeading + EOL2 + E.Message)
  else
    raise E;
end;

procedure TSnippetsLoader.Load(const Routines: TRoutineList;
  const Categories: TCategoryList; const SnippetsFactory: ISnippetsFactory);
  {Loads data from storage and updates snippets object.
    @param Routines [in] Receives information about each snippet in the
      database.
    @param Categories [in] Receives information about each category in the
      database.
    @param SnippetsFactory [in] Object used to create new categories and
      snippets.
  }
var
  Category: TCategory;  // a category
  Routine: TRoutine;    // a snippet
begin
  // Create reader object that can access data storage
  fReader := CreateReader;
  // Record snippets and categories list in fields
  // Do not clear snippet or category lists: may already contain data
  fRoutines := Routines;
  fCategories := Categories;
  fFactory := SnippetsFactory;
  try
    // Load categories
    LoadCategories;
    // Load snippets in each category
    for Category in fCategories do
      LoadRoutines(Category);
    // Build XRef, Depends and Units reference list of each snippet for this
    // database
    for Routine in fRoutines do
    begin
      if IsNativeRoutine(Routine) then
        LoadReferences(Routine);
    end;
  except
    on E: Exception do
      HandleException(E);
  end;
end;

procedure TSnippetsLoader.LoadCategories;
  {Loads all categories from storage
  }
var
  CatNames: IStringList;    // list of names of categories
  CatName: string;          // name of each category
  Category: TCategory;      // a category object
  CatData: TCategoryData;   // properties of a category
begin
  // Get name of all categories
  CatNames := fReader.GetAllCatNames;
  // Loop through each category by name
  for CatName in CatNames do
  begin
    // Check if category exists, creating it if not
    Category := fCategories.Find(CatName);
    if not Assigned(Category) then
    begin
      FillChar(CatData, SizeOf(CatData), 0);
      fReader.GetCatProps(CatName, CatData);
      CreateCategory(CatName, CatData);
    end;
  end;
end;

procedure TSnippetsLoader.LoadReferences(const Routine: TRoutine);
  {Loads all of a snippet's references.
    @param Routine [in] Snippet for which references are required.
  }

  // ---------------------------------------------------------------------------
  procedure LoadRoutineReferences(const RefList: TRoutineList;
    const RefNames: IStringList);
    {Creates a snippet list from names of snippets in a string list. If no
    snippet with a given name is found no matching entry is added to snippet
    list.
      @param RefList [in] List to receive referenced snippets.
      @param RefNames [in] List of snippet names.
    }
  var
    RefName: string;        // referenced snippet name
    Reference: TRoutine;    // referenced snippet object
  begin
    for RefName in RefNames do
    begin
      Reference := FindRoutine(RefName, fRoutines);
      if Assigned(Reference) then
        RefList.Add(Reference);
    end;
  end;
  // ---------------------------------------------------------------------------

begin
  LoadRoutineReferences(
    Routine.Depends, fReader.GetRoutineDepends(Routine.Name)
  );
  LoadRoutineReferences(
    Routine.XRef, fReader.GetRoutineXRefs(Routine.Name)
  );
  fReader.GetRoutineUnits(Routine.Name).CopyTo(Routine.Units);
end;

procedure TSnippetsLoader.LoadRoutines(const Cat: TCategory);
  {Loads all snippets in a category.
    @param Cat [in] Category to be loaded.
  }
var
  RoutineNames: IStringList;    // list of names of snippets in category
  RoutineProps: TRoutineData;   // properties of a snippet
  RoutineName: string;          // each name in name list
  Routine: TRoutine;            // references a snippet object
begin
  FillChar(RoutineProps, SizeOf(RoutineProps), 0);
  // Get names of all snippets in category
  RoutineNames := fReader.GetCatRoutines(Cat.Category);
  // Process each snippet name in list
  for RoutineName in RoutineNames do
  begin
    // Check if snippet exists in current database and add it to list if not
    // Note: do not use FindRoutine method here
    Routine := fRoutines.Find(RoutineName, IsUserDatabase);
    if not Assigned(Routine) then
    begin
      fReader.GetRoutineProps(RoutineName, RoutineProps);
      Routine := fFactory.CreateRoutine(
        RoutineName, IsUserDatabase, RoutineProps
      );
      fRoutines.Add(Routine);
    end;
    // Add snippet to database only if it belongs to this database
    if IsNativeRoutine(Routine) then
      Cat.Routines.Add(Routine);
  end;
end;

{ TMainSnippetsLoader }

function TMainSnippetsLoader.CreateReader: IDataReader;
  {Creates reader object. If main database doesn't exist a nul reader is
  created.
    @return Reader object instance.
  }
begin
  Result := TDataIOFactories.CreateDataReader('IniDataReader', dtMain);
  if not Result.DatabaseExists then
    Result := TDataIOFactories.CreateDataReader('NulDataReader', dtMain);
end;

function TMainSnippetsLoader.ErrorMessageHeading: string;
  {Returns heading to use in error messages. Identifies main database.
    @return Required heading.
  }
resourcestring
  sError = 'Error loading the CodeSnip database:';
begin
  Result := sError;
end;

function TMainSnippetsLoader.FindRoutine(const RoutineName: string;
  const Routines: TRoutineList): TRoutine;
  {Finds the snippet object with a specified name in the main database.
    @param RoutineName [in] Name of required snippet.
    @return Reference to required snippet object or nil if snippet is not found.
  }
begin
  // We only search main database
  Result := Routines.Find(RoutineName, False);
end;

function TMainSnippetsLoader.IsNativeRoutine(const Routine: TRoutine): Boolean;
  {Checks if a snippet is native (belongs) to the main database.
    @param Routine [in] Snippet to test.
    @return True if snippet is native, False if not.
  }
begin
  Result := not Routine.UserDefined;
end;

function TMainSnippetsLoader.IsUserDatabase: Boolean;
  {Checks if the database is the user database.
    @return False - this is not the user database.
  }
begin
  Result := False;
end;

{ TUserSnippetsLoader }

function TUserSnippetsLoader.CreateReader: IDataReader;
  {Creates reader object. If user database doesn't exist a nul reader is
  created.
    @return Reader object instance.
  }
begin
  Result := TDataIOFactories.CreateDataReader('XMLDataReader', dtUser);
  if not Result.DatabaseExists then
    Result := TDataIOFactories.CreateDataReader('NulDataReader', dtUser);
end;

function TUserSnippetsLoader.ErrorMessageHeading: string;
  {Returns heading to use in error messages. Identifies main database.
    @return Required heading.
  }
resourcestring
  sError = 'Error loading the user defined database:';
begin
  Result := sError;
end;

function TUserSnippetsLoader.FindRoutine(const RoutineName: string;
  const Routines: TRoutineList): TRoutine;
  {Finds the snippet object with a specified name. If snippet is not in this
  (user) database the main database is searched.
    @param RoutineName [in] Name of required snippet.
    @return Reference to required snippet object or nil if snippet is not found.
  }
begin
  // Search in user database
  Result := Routines.Find(RoutineName, True);
  if not Assigned(Result) then
    // Not in user database: try main database
    Result := Routines.Find(RoutineName, False);
end;

function TUserSnippetsLoader.IsNativeRoutine(const Routine: TRoutine): Boolean;
  {Checks if a snippet is native (belongs) to the user database.
    @param Routine [in] Snippet to test.
    @return True if snippet is native, False if not.
  }
begin
  Result := Routine.UserDefined;
end;

function TUserSnippetsLoader.IsUserDatabase: Boolean;
  {Checks if the database is the user database.
    @return True - this is the user database.
  }
begin
  Result := True;
end;

procedure TUserSnippetsLoader.LoadCategories;
  {Loads all categories from storage and adds user and imports categories if not
  present.
  }
resourcestring
  // Descriptions of default user-defined categories
  sUserCatDesc = 'User Defined Snippets';
  sImportCatDesc = 'Imported Snippets';
const
  // User defined category info
  cUserCatName = 'user';                                  // name
  cUserCatData: TCategoryData = (Desc: sUserCatDesc);     // properties
  // Imports category info
  cImportCatName = 'imports';                             // name
  cImportCatData: TCategoryData = (Desc: sImportCatDesc); // properties
begin
  // Get all categories from storage
  inherited;
  // Add default user-defined categories if not present
  if Categories.Find(cUserCatName) = nil then
    CreateCategory(cUserCatName, cUserCatData);
  if Categories.Find(cImportCatName) = nil then
    CreateCategory(cImportCatName, cImportCatData);
end;

{ TSnippetsWriter }

function TSnippetsWriter.CreateWriter: IDataWriter;
  {Creates object that can write data for user-defined items from Snippets to
  storage.
    @return Requied writer object.
  }
begin
  Result := TDataIOFactories.CreateDataWriter('XMLDataWriter', dtUser)
    as IDataWriter;
end;

procedure TSnippetsWriter.Write(const Routines: TRoutineList;
  const Categories: TCategoryList; const Provider: ISnippetsDataProvider);
  {Writes data from Snippets object to storage.
    @param Routines [in] Contains information about each snippet in the
      database.
    @param Categories [in] Contains information about each category in the
      database.
    @param Privider [in] Object used to obtain details of the data to be written
      from the Snippets object
  }
begin
  fRoutines := Routines;
  fCategories := Categories;
  fProvider := Provider;
  fWriter := CreateWriter;
  fWriter.Initialise;
  WriteCategories;
  WriteRoutines;
  fWriter.Finalise;
end;

procedure TSnippetsWriter.WriteCategories;
  {Writes information about categories to storage.
  }
var
  Cat: TCategory;         // loops through each category
  Props: TCategoryData;   // categpry properties
  Routines: IStringList;  // list of names of snippets in a category
begin
  for Cat in fCategories do
  begin
    Props := fProvider.GetCategoryProps(Cat);
    fWriter.WriteCatProps(Cat.Category, Props);
    Routines := fProvider.GetCategoryRoutines(Cat);
    fWriter.WriteCatRoutines(Cat.Category, Routines);
  end;
end;

procedure TSnippetsWriter.WriteRoutines;
  {Writes information about all snippets to storage.
  }

  // ---------------------------------------------------------------------------
  function IDListToStrings(const IDList: ISnippetIDList): IStringList;
    {Copies snippet names from a snippet ID list to a string list.
      @param IDList [in] Snippet ID List to be copied.
      @return String list containing names.
    }
  var
    ID: TSnippetID; // each id in snippet id list
  begin
    Result := TIStringList.Create;
    for ID in IDList do
      Result.Add(ID.Name);
  end;
  // ---------------------------------------------------------------------------

var
  Routine: TRoutine;        // loops through each snippet in list
  Props: TRoutineData;      // snippet properties
  Refs: TRoutineReferences; // snippet references
begin
  for Routine in fRoutines do
  begin
    // Only write user-defined snippets
    if Routine.UserDefined then
    begin
      // Get and write a snippet's properties
      Props := fProvider.GetRoutineProps(Routine);
      fWriter.WriteRoutineProps(Routine.Name, Props);
      // Get and write a snippet's references
      Refs := fProvider.GetRoutineRefs(Routine);
      fWriter.WriteRoutineUnits(Routine.Name, Refs.Units);
      fWriter.WriteRoutineDepends(Routine.Name, IDListToStrings(Refs.Depends));
      fWriter.WriteRoutineXRefs(Routine.Name, IDListToStrings(Refs.XRef));
    end;
  end;
end;

end.

