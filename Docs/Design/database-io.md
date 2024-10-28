# Database IO Design

Both user defined and "main" snippets are stored in the same data structure.

## Loading

When the database is loaded the following happens:

### Top level

`TDatabaseIOFactory` is called twice, in a predefined, significant order:

1. `TDatabaseIOFactory.CreateMainDBLoader` is called to create a `IDatabaseLoader` object for the main database. That object's `Load` method is called.
2. `TDatabaseIOFactory.CreateUserDBLoader` is called to create another `IDatabaseLoader` object, this time for the user databse. That object's `Load` method is then called.

> ⭐ When adding further data sources, instead of hard wiring the `TDatabaseIOFactory.CreateXXX` methods, we will need to create `IDatabaseLoader` instances for each registered data source. It may be a good idea initially to hard wire the "main" and "user" databases at the start of the data source registry and ensure they are all called in the right order.

The order the loading is done is to ensure any references to snippets in the main database are valid when the user database is loaded. This is because the user database can reference snippets in the main database, but not vice versa.

> ⭐ When implementing additional data sources we may need to prevent references across data sources.

### Level 2: `IDatabaseLoader`

`IDatabaseLoader` is defined as:

```pascal
  IDatabaseLoader = interface(IInterface)
    ['{C6AF94FC-F56F-44AE-9E79-3B0CD0BB21D4}']
    procedure Load(const SnipList: TSnippetList;
      const Categories: TCategoryList;
      const DBDataItemFactory: IDBDataItemFactory);
  end;
```

Implementations of the single `Load` method create and store loaded snippets and categories in `SnipList` & `Categories` respectively. They use `IDBDataItemFactory` methods to create snippet and category objects.

There are the following `IDatabaseLoader` implementations:

1. `TMainDatabaseLoader` for loading the main database from the main database directory.
2. `TUserDatabaseLoader` for loading the user database from its directory.

Both the above classes use the template design pattern and descend from an abstract parent class named `TDatabaseLoader`. One of the overridden template methods requires an object to be returned that can read data from the correct database format and expose methods that are called from `TDatabaseLoader`. These reader object are of type `IDataReader`.

> ⭐ When implementing additional data formats we will need further `IDatabaseLoader` implementations that can read each supported format. If we choose to sub class `TDatabaseLoader` then we will need to provide a suitable `IDataReader` implementation.

In either case, if no database of either type is found a null object of type `TNulDataReader` (sic) is created that implements `IDataReader` to do nothing.

### Level 3: `IDataReader`

`IDataReader` is defined as:

```pascal
  IDataReader = interface(IInterface)
    ['{72A8EAD4-05CE-41BF-AE0F-33495757BBFC}']
    function DatabaseExists: Boolean;
    function GetAllCatIDs: IStringList;
    procedure GetCatProps(const CatID: string; var Props: TCategoryData);
    function GetCatSnippets(const CatID: string): IStringList;
    procedure GetSnippetProps(const Snippet: string; var Props: TSnippetData);
    function GetSnippetXRefs(const Snippet: string): IStringList;
    function GetSnippetDepends(const Snippet: string): IStringList;
    function GetSnippetUnits(const Snippet: string): IStringList;
  end;
```

Where:

* `DatabaseExists` -- Checks if the database exists. This method is always called first. No other methods of `IDataReader` are called if this method returns false.
* `GetAllCatIDs` -- Gets the names of all categories defined in the database.
* `GetCatProps` -- Gets the properties of the category identified by `CatID` and returns them in the `Props` record.
* `GetCatSnippets` -- Returns a list of the names of all snippets in category with ID `CatID`.
* `GetSnippetProps` -- Gets the properties if the snippet with unique `Snippet` and returns them in the `Props` record.
* `GetSnippetXRefs` -- Returns a list of the names of all snippets XRefd by snippet with name `Snippet`.
* `GetSnippetDepends` -- Returns a list of the names of all snippets on which the snippet with name `Snippet` depends.
* `GetSnippetUnits` -- Returns a list of the names of all the units referenced by the snippet with name `Snippet`.

There are the following `IDataReader` implementations:

1. `TIniDataReader` that reads the main database's .ini based data format. Instances of this class are created by `TMainDatabaseLoader`.
2. `TXMLDataReader` that read the user database's XML based data format. Instances of this class are created by `TUserDatabaseLoader`.

> ⭐ Note that, although there is a one-one correspondence between `TMainDatabaseLoader` & `TIniDataReader` and `TUserDatabaseLoader` and `TXMLDataReader`, this does not generalise to other data sources and data format readers. More than one data source could use the same data format.

> ⭐ When implementing additional data sources we will need `IDataReader` implementations that can read each supported data format.

## Saving

Only the user database gets saved, because only this database is editable. When the user database is saved, the following happens:

`TDatabaseIOFactory` is called to create a `IDatabaseWriter` database writer object. That object's `Write` method is then called.

> ⭐ When adding further data sources we will need to extend the factory class to create a suitable `IDatabaseWriter` object for the data format being written.

### Level 2: `IDatabaseWriter`

`IDatabaseWriter` is defined as:

```pascal
  IDatabaseWriter = interface(IInterface)
    ['{F46EE2E3-68A7-4877-9E04-192D15D29BB1}']
    procedure Write(const SnipList: TSnippetList;
      const Categories: TCategoryList; const Provider: IDBDataProvider);
  end;
```

The sole `Write` method writes user data from database object, where `SnipList` and `Categories` contain the snippets and categories to be saved, respectively, and the `Provider` object is used to get snippet and category properties to be written.

There is just the one `IDatabaseWriter` implementation: `TDatabaseWriter` that saves the user database. It creates and uses a `IDataWriter` instance to perform the actual output.

> ⭐ When adding further data formats we will need different `IDataWriter` implementations that can write data in the correct format.

### Level 3: `IDataWriter`

`IDataWriter` is defined as follows:

```pascal
  IDataWriter = interface(IInterface)
    ['{71E892C4-6E0F-480A-9DF4-70835F83A0CA}']
    procedure Initialise;
    procedure WriteCatProps(const CatID: string; const Props: TCategoryData);
    procedure WriteCatSnippets(const CatID: string;
      const SnipList: IStringList);
    procedure WriteSnippetProps(const SnippetName: string;
      const Props: TSnippetData);
    procedure WriteSnippetUnits(const SnippetName: string;
      const Units: IStringList);
    procedure WriteSnippetDepends(const SnippetName: string;
      const Depends: IStringList);
    procedure WriteSnippetXRefs(const SnippetName: string;
      const XRefs: IStringList);
    procedure Finalise;
  end;
```

Where:

* `Initialise` -- Initialise the output. Always called before any other methods.
* `WriteCatProps` -- Writes the properties per `Props` of the category with ID `CatID`.
* `WriteCatSnippets` -- Writes the list of snippets whose names are in `SnipList` belonging to category `CatID`. Always called after `WriteCatProps` for a given category.
* `WriteSnippetProps` -- Writes the properties from `Props` for snippet named `SnippetName`. Always called after all categories are written and before `WriteSnippetUnits`, so can be used to perform any per-snippet intialisation.
* `WriteSnippetUnits` -- Writes the list of units per `Units` referenced by the snippet named `SnippetName`.
* `WriteSnippetDepends` -- Writes the list of snippets names in `Depends` that are depended upon by the snippet named `SnippetName`.
* `WriteSnippetXRef` -- Writes the list of snippets names in `XRefs` that are cross referenced by the snippet named `SnippetName`.
* `Finalise` -- Finalises the writing. Always called after all other methods.

There is only the one `IDataWriter` implementation. That is `TXMLDataWriter` that writes the user database data in the required XML format.

> ⭐ When adding further data formats we will need further data writer implementations

> ⭐ There is not necessarily a one-one correspondence between data sources and formats, so the same `IDataWriter` implementation could be used for more than one data source.
