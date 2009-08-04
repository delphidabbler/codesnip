{
 * USnipData.pas
 *
 * Defines interface implemented by classes that access back end Code Snippets
 * database. Also implememts class that that registers factories that create
 * back end database access objects.
 *
 * v0.1 of 26 Feb 2005  - Original version.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 * v1.1 of 24 Aug 2008  - Added Windows unit to enable inlining in Delphi 2006.
 * v2.0 of 01 Sep 2008  - Major update:
 *                        - Modified to work with both user and main databases.
 *                        - Added function to IDataReader so that reader can
 *                          check database exists. Other methods were revised to
 *                          use record structures to return routine and category
 *                          data and IStringList for string list data.
 *                        - Added IDataWriter interface that is implemented by
 *                          objects that can write user database.
 *                        - Removed descriptions of readers and writers and
 *                          unused methods that list ids and descriptions.
 *                        - Added EDataReader exception definition.
 * v2.1 of 04 Oct 2008  - Changed TDataIOFactories and TDataIOFactory to derive
 *                        from TNoConstructObject and hence prevented them from
 *                        being constructed.
 *                      - Made private section of TDataIOFactories strict.
 *                      - Now use ClassName method in all raise EBug statements.
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
 * The Original Code is USnipData.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit USnipData;


interface


{*******************************************************************************
*                                                                              *
* Conceptually we treat the codesnip database as having the following tables:  *
*                                                                              *
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *
* Categories - details of each category of routines                            *
* ---------------------------------------------------------------------------- *
* Category        | Name or ID of category (primary key)                       *
* Description     | Description of category                                    *
* UserDefined     | Indicates if routine is user-defined                       *
* ---------------------------------------------------------------------------- *
* Note)   Categories has a one-many relationship with Routines                 *
* ============================================================================ *
*                                                                              *
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *
* Routines - details of each routine                                           *
* ---------------------------------------------------------------------------- *
* Name            | Pascal Name of routine (primary key)                       *
* Category        | Name of category routine belongs to                        *
* Description     | Description of purpose of routine                          *
* SourceCode      | Routine's source code                                      *
* Comments        | Additional comments about routines                         *
* Credits         | Any credits - any URL link text delimited by [ ]           *
* CreditsURL      | Any URL associated with credits                            *
* Compatibility   | String of characters describing compiler compatibility     *
* UserDefined     | Indicates if routine is user-defined                       *
* StandardFormat  | Indicates if routine is in standard format                 *
* ---------------------------------------------------------------------------- *
* Note 1) Routines have a many-many relationship with same table, both for     *
*         dependency lists and cross-reference lists.                          *
* Note 2) Routines have a many-many relationship with units.                   *
* ============================================================================ *
*                                                                              *
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *
* Units - names of required units                                              *
* ---------------------------------------------------------------------------- *
* Unit             | Name of routine (primary key)                             *
* ---------------------------------------------------------------------------- *
* Note)   There is no other information in this table - so we may omit it      *
* ============================================================================ *
*                                                                              *
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *
* RoutineUnits - map of routines to units                                      *
* ---------------------------------------------------------------------------- *
* RoutineName      | Name of routine                                           *
* UnitName         | Name of unit                                              *
* ============================================================================ *
*                                                                              *
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *
* RoutineXRef - map of routines to cross-referenced routines                   *
* ---------------------------------------------------------------------------- *
* RoutineName      | Name of routine                                           *
* XRefRoutineName  | Name of cross referenced routine                          *
* ============================================================================ *
*                                                                              *
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *
* RoutineDepends - map of routines to the routines they depend upon            *
* ---------------------------------------------------------------------------- *
* RoutineName      | Name of routine                                           *
* RequiredRoutine  | Name of required routine                                  *
* ============================================================================ *
*                                                                              *
* Any object that is to be used to read a database must implement the          *
* IDataReader interface.                                                       *
*                                                                              *
* Any object that is to be used to write the user database must implement the  *
* IDataWriter interface.                                                       *
*                                                                              *
*******************************************************************************}


uses
  // Delphi
  SysUtils, Classes,
  // Project
  UExceptions, UBaseObjects, USnippets, UIStringList;


type

  {
  IDataReader:
    Interface that defines operations that must be implemented by objects that
    read the CodeSnip and/or user database.
  }
  IDataReader = interface(IInterface)
    ['{72A8EAD4-05CE-41BF-AE0F-33495757BBFC}']
    function DatabaseExists: Boolean;
      {Check if the database exists. This method is always called first. No
      other methods are called if this method returns false.
        @return True if database exists, False if not.
      }
    function GetAllCatNames: IStringList;
      {Get names of all categories in database.
        @return List of category names.
      }
    procedure GetCatProps(const Cat: string; var Props: TCategoryData);
      {Get properties of a category.
        @param Cat [in] Name of required category.
        @param Props [in/out] Empty properties passed in. Record fields set to
          values of category properties by implementor.
      }
    function GetCatRoutines(const Cat: string): IStringList;
      {Get names of all routines in a category.
        @param Cat [in] Name of category containing routines.
        @return List of routines names.
      }
    procedure GetRoutineProps(const Routine: string; var Props: TRoutineData);
      {Get properties of a routine. These are the fields of the routine's
      record in the routines "table".
        @param Routine [in] Name of required routine.
        @param Props [in/out] Empty properties passed in. Record fields set to
          values of routine properties by implementor.
      }
    function GetRoutineXRefs(const Routine: string): IStringList;
      {Get list of all routines that are cross referenced by a routine.
        @param Routine [in] Name of routine we need cross references for.
        @return List of routine names.
      }
    function GetRoutineDepends(const Routine: string): IStringList;
      {Get list of all routines on which a given routine depends.
        @param Routine [in] Name of required routine.
        @return List of routine names.
      }
    function GetRoutineUnits(const Routine: string): IStringList;
      {Get list of all units referenced by a routine.
        @param Routine [in] Name of required routine.
        @return List of unit names.
      }
  end;

  {
  IDataWriter:
    Interface that defines operations that must be implemented by objects that
    write user database.
  }
  IDataWriter = interface(IInterface)
    ['{71E892C4-6E0F-480A-9DF4-70835F83A0CA}']
    procedure Initialise;
      {Initialise the database. Always called before any other methods.
      }
    procedure WriteCatProps(const CatName: string; const Props: TCategoryData);
      {Write the properties of a category. Always called before WriteCatRoutines
      for a given category, so can be used to perform any per-category
      initialisation.
        @param CatName [in] Name of category.
        @param Props [in] Properties of category.
      }
    procedure WriteCatRoutines(const CatName: string;
      const Routines: IStringList);
      {Write the list of routines belonging to a category. Always called after
      WriteCatProps for any given category.
        @param CatName [in] Name of category.
        @param Routines [in] List of name of routines.
      }
    procedure WriteRoutineProps(const RoutineName: string;
      const Props: TRoutineData);
      {Write the properties of a routine. Always called after all categories are
      written and before WriteRoutineUnits, so can be used to perform any per-
      routine intialisation.
        @param RoutineName [in] Name of routine.
        @param Props [in] Properties of routine.
      }
    procedure WriteRoutineUnits(const RoutineName: string;
      const Units: IStringList);
      {Write the list of units required by a routine.
        @param RoutineName [in] Name of routine.
        @param Units [in] List of names of required units.
      }
    procedure WriteRoutineDepends(const RoutineName: string;
      const Depends: IStringList);
      {Write the list of routines on which a routine depends.
        @param RoutineName [in] Name of routine.
        @param Depends [in] List of routine names.
      }
    procedure WriteRoutineXRefs(const RoutineName: string;
      const XRefs: IStringList);
      {Write the list of routines that a routine cross-references.
        @param RoutineName [in] Name of routine.
        @param XRefs [in] List of routine names.
      }
    procedure Finalise;
      {Finalises the database. Always called after all other methods.
      }
  end;

  {
  TDatabaseType:
    Type of database. Used by readers so they know whether to access the
    codesnip or user database.
  }
  TDatabaseType = (
    dtMain,   // main CodeSnip database
    dtUser    // user's database
  );

  {
  TDataIOFactoryClass:
    Class reference to TDataIOFactory classes.
  }
  TDataIOFactoryClass = class of TDataIOFactory;

  {
  TDataIOFactory:
    Pure abstract base class for classes that create objects that implement
    IDataReader or IDataWriter. Should not be directly overridden. Provided so
    that TDataReadFactory and TDataWriterFactory can be used polymorphically.
  }
  TDataIOFactory = class(TNoConstructObject)
  public
    class function Instance(const DBType: TDatabaseType): IInterface;
      virtual; abstract;
  end;

  {
  TDataReaderFactory:
    Pure abstract base class for classes that create objects that implement
    IDataReader and can access either the CodeSnip of user database. Each data
    reader object must provide a factory class that derives from this class and
    that creates an instance of the IDataReader object in its Instance method.
  }
  TDataReaderFactory = class(TDataIOFactory)
  public
    class function Instance(const DBType: TDatabaseType): IInterface;
      override; abstract;
  end;

  {
  TDataWriterFactory:
    Pure abstract base class for classes that create objects that implement
    IDataWriter and can write the user database. Each data writer object must
    provide a factory class that derives from this class and that creates an
    instance of the IDataWriter object in its Instance method.
  }
  TDataWriterFactory = class(TDataIOFactory)
  public
    class function Instance(const DBType: TDatabaseType): IInterface;
      override; abstract;
  end;

  {
  TDataIOFactories:
    Static class that enables factory classes for various database reader and
    writer objects to be registered and instantiated. Factories are registered
    with unique IDs. The class also permits the registered IDs to be listed and
    a description to be returned.
  }
  TDataIOFactories = class(TNoConstructObject)
  strict private
    class function FindEntry(const ID: string): TObject;
      {Finds an internal object that describes a specified factory.
        @param ID [in] Unique string that identifies a factory class.
        @return Reference to internal object that describes the factory or nil
          if no factory is registered with ID.
      }
  public
    class procedure RegisterFactory(const ID: string;
      const FactoryClass: TDataIOFactoryClass);
      {Registers a data reader or writer factory.
        @param ID [in] Unique string identifying the factory.
        @param FactoryClass [in] Factory's class reference.
        @except EBug raised if factory with given ID is already registered.
      }
    class function CreateDataReader(const ID: string;
      const DBType: TDatabaseType): IDataReader;
      {Creates an instance of a registered data reader object.
        @param ID [in] Identifies the factory class.
        @param DBType [in] Type of database to read.
        @return New data reader object instance.
        @except EBug raised if no factory with given ID is registered.
      }
    class function CreateDataWriter(const ID: string;
      const DBType: TDatabaseType): IDataWriter;
      {Creates an instance of a registered data writer object.
        @param ID [in] Identifies the factory class.
        @param DBType [in] Type of database to write.
        @return New data writer object instance.
        @except EBug raised if no factory with given ID is registered.
        @except EBug raised if dtMain is specified in DBType.
      }
  end;

  {
  EDataIO:
    Class of exception raised by IDataReader and IDataWriter objects.
  }
  EDataIO = class(ECodeSnip);


implementation


uses
  // Delphi
  Contnrs, Windows {for inlining};


var
  // Private global object storing registered data reader factory classes
  PvtFactoryClasses: TObjectList;


type

  {
  TDataFactoryEntry:
    Class that records information about a registered data reader factory.
  }
  TDataFactoryEntry = class(TObject)
  public
    ID: string;
      {ID of factory}
    FactoryClass: TDataIOFactoryClass;
      {Class reference to static factory class}
  end;


{ TDataIOFactories }

class function TDataIOFactories.CreateDataReader(const ID: string;
  const DBType: TDatabaseType): IDataReader;
  {Creates an instance of a registered data reader object.
    @param ID [in] Identifies the factory class.
    @param DBType [in] Type of database to read.
    @return New data reader object instance.
    @except EBug raised if no factory with given ID is registered.
  }
var
  Entry: TObject; // entry representing the the required factory
begin
  Entry := FindEntry(ID);
  if not Assigned(Entry) then                              // ** do not localise
    raise EBug.Create(ClassName + '.CreateDataReader: ID not found');
  // Create instance of data reader object using required factory
  Result := (Entry as TDataFactoryEntry).FactoryClass.Instance(DBType)
    as IDataReader;
end;

class function TDataIOFactories.CreateDataWriter(const ID: string;
  const DBType: TDatabaseType): IDataWriter;
  {Creates an instance of a registered data writer object.
    @param ID [in] Identifies the factory class.
    @param DBType [in] Type of database to write.
    @return New data writer object instance.
    @except EBug raised if no factory with given ID is registered.
    @except EBug raised if dtMain is specified in DBType.
  }
var
  Entry: TObject; // entry representing the the required factory
begin
  Entry := FindEntry(ID);
  if not Assigned(Entry) then                              // ** do not localise
    raise EBug.Create(ClassName + '.CreateDataWriter: ID not found');
  if DBType = dtMain then                                  // ** do not localise
    raise EBug.Create(ClassName + '.CreateDataWriter: ID can''t be mtMain');
  // Create instance of data writer object using required factory
  Result := (Entry as TDataFactoryEntry).FactoryClass.Instance(DBType)
    as IDataWriter;
end;

class function TDataIOFactories.FindEntry(const ID: string): TObject;
  {Finds an internal object that describes a specified factory.
    @param ID [in] Unique string that identifies a factory class.
    @return Reference to internal object that describes the factory or nil if no
      factory is registered with ID.
  }
var
  Idx: Integer; // loops thru list of registered factories
begin
  Result := nil;
  for Idx := 0 to Pred(PvtFactoryClasses.Count) do
  begin
    if AnsiSameText(
      (PvtFactoryClasses[Idx] as TDataFactoryEntry).ID, ID
    ) then
    begin
      Result := PvtFactoryClasses[Idx];
      Break;
    end;
  end;
end;

class procedure TDataIOFactories.RegisterFactory(const ID: string;
  const FactoryClass: TDataIOFactoryClass);
  {Registers a data reader or writer factory.
    @param ID [in] Unique string identifying the factory.
    @param FactoryClass [in] Factory's class reference.
    @except EBug raised if factory with given ID is already registered.
  }
var
  Entry: TDataFactoryEntry; // entry representing the the required factory
begin
  // Check if factory with given ID registered already
  if Assigned(FindEntry(ID)) then
    raise EBug.Create(                                     // ** do not localise
      ClassName + '.RegisterFactory: factory already registered'
    );
  // Create entry for new factory
  Entry := TDataFactoryEntry.Create;
  Entry.ID := ID;
  Entry.FactoryClass := FactoryClass;
  // Add entry to list
  PvtFactoryClasses.Add(Entry);
end;


initialization

// Create factory class registration list
PvtFactoryClasses := TObjectList.Create(True);


finalization

// Free registration list
FreeAndNil(PvtFactoryClasses);

end.

