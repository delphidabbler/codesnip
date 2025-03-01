{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Defines interfaces implemented by classes that read and write physical code
 * snippets database files. Also provides an exception class for data I/O
 * errors.
}


unit DBIO.UFileIOIntf;


interface


uses
  // Project
  DB.MetaData,
  DB.UCategory,
  DB.USnippet,
  UExceptions,
  UIStringList;


type
  {
  IDataReader:
    Interface that defines operations that must be implemented by objects that
    read the CodeSnip and/or user database.
    NOTE: Any object that is to be used to read a database must implement this
    interface.
  }
  IDataReader = interface(IInterface)
    ['{72A8EAD4-05CE-41BF-AE0F-33495757BBFC}']
    function DatabaseExists: Boolean;
      {Check if the database exists. This method is always called first. No
      other methods are called if this method returns false.
        @return True if database exists, False if not.
      }
    function GetAllCatIDs: IStringList;
      {Get ids of all categories in database.
        @return List of category ids.
      }
    procedure GetCatProps(const CatID: string; var Props: TCategoryData);
      {Get properties of a category.
        @param CatID [in] Id of required category.
        @param Props [in/out] Empty properties passed in. Record fields set to
          values of category properties by implementor.
      }
    function GetCatSnippets(const CatID: string): IStringList;
      {Get keys of all snippets in a category.
        @param CatID [in] Id of category containing snippets.
        @return List of snippet IDs.
      }
    procedure GetSnippetProps(const SnippetKey: string; var Props: TSnippetData);
      {Get properties of a snippet. These are the fields of the snippet's
      record in the snippets "table".
        @param SnippetKey [in] Snippet's key.
        @param Props [in/out] Empty properties passed in. Record fields set to
          values of snippet's properties by implementor.
      }
    function GetSnippetXRefs(const SnippetKey: string): IStringList;
      {Get list of all snippets that are cross referenced by a snippet.
        @param SnippetKey [in] Keyof snippet we need cross references for.
        @return List of snippet keys.
      }
    function GetSnippetDepends(const SnippetKey: string): IStringList;
      {Get list of all snippet on which a given snippet depends.
        @param SnippetKey [in] Snippet's key.
        @return List of snippet keys.
      }
    function GetSnippetUnits(const SnippetKey: string): IStringList;
      {Get list of all units referenced by a snippet.
        @param SnippetKey [in] Snippet's key.
        @return List of unit names.
      }
  end;

  {
  IDataWriter:
    Interface that defines operations that must be implemented by objects that
    write user database.
    NOTE: Any object that is to be used to write the user database must
    implement this interface.
  }
  IDataWriter = interface(IInterface)
    ['{71E892C4-6E0F-480A-9DF4-70835F83A0CA}']
    procedure Initialise;
      {Initialise the database. Always called before any other methods.
      }
    procedure WriteCatProps(const CatID: string; const Props: TCategoryData);
      {Write the properties of a category. Always called before WriteCatSnippets
      for a given category, so can be used to perform any per-category
      initialisation.
        @param CatID [in] ID of category.
        @param Props [in] Properties of category.
      }
    procedure WriteCatSnippets(const CatID: string;
      const SnipList: IStringList);
      {Write the list of snippets belonging to a category. Always called after
      WriteCatProps for any given category.
        @param CatID [in] ID of category.
        @param SnipList [in] List of IDs of snippets.
      }
    procedure WriteSnippetProps(const SnippetKey: string;
      const Props: TSnippetData);
      {Write the properties of a snippet. Always called after all categories are
      written and before WriteSnippetUnits, so can be used to perform any per-
      snippet intialisation.
        @param SnippetKey [in] Snippet's key.
        @param Props [in] Properties of snippet.
      }
    procedure WriteSnippetUnits(const SnippetKey: string;
      const Units: IStringList);
      {Write the list of units required by a snippet.
        @param SnippetKey [in] Snippet's key.
        @param Units [in] List of names of required units.
      }
    procedure WriteSnippetDepends(const SnippetKey: string;
      const Depends: IStringList);
      {Write the list of snippets on which a snippet depends.
        @param SnippetKey [in] Snippet's key.
        @param Depends [in] List of snippet keys.
      }
    procedure WriteSnippetXRefs(const SnippetKey: string;
      const XRefs: IStringList);
      {Write the list of snippets that a snippet cross-references.
        @param SnippetKey [in] Snippet's key.
        @param XRefs [in] List of snippet keys.
      }

    ///  <summary>Write the collection's meta data.</summary>
    ///  <param name="AMetaData"><c>TMetaData</c> [in] Meta data to be written.
    ///  </param>
    ///  <remarks>Data formats may support all, some or no metadata.</remarks>
    procedure WriteMetaData(const AMetaData: TMetaData);

    procedure Finalise;
      {Finalises the database. Always called after all other methods.
      }
  end;

  {
  EDataIO:
    Class of exception raised by IDataReader and IDataWriter objects.
  }
  EDataIO = class(ECodeSnip);


implementation

end.

