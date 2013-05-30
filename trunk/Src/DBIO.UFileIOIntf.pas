{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines interfaces implemented by classes that read and write physical code
 * snippets database files. Also provides an exception class for data I/O
 * errors.
}


unit DBIO.UFileIOIntf;


interface


uses
  // Project
  DB.UCategory, DB.USnippet, UExceptions, UIStringList;


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
      {Get names of all categories in database.
        @return List of category ids.
      }
    procedure GetCatProps(const CatID: string; var Props: TCategoryData);
      {Get properties of a category.
        @param CatID [in] Id of required category.
        @param Props [in/out] Empty properties passed in. Record fields set to
          values of category properties by implementor.
      }
    function GetCatSnippets(const CatID: string): IStringList;
      {Get names of all snippets in a category.
        @param CatID [in] Id of category containing snippets.
        @return List of snippet names.
      }
    procedure GetSnippetProps(const Snippet: string; var Props: TSnippetData);
      {Get properties of a snippet. These are the fields of the snippet's
      record in the snippets "table".
        @param Snippet [in] Name of snippet.
        @param Props [in/out] Empty properties passed in. Record fields set to
          values of snippet's properties by implementor.
      }
    function GetSnippetXRefs(const Snippet: string): IStringList;
      {Get list of all snippets that are cross referenced by a snippet.
        @param Snippet [in] Name of snippet we need cross references for.
        @return List of snippet names.
      }
    function GetSnippetDepends(const Snippet: string): IStringList;
      {Get list of all snippet on which a given snippet depends.
        @param Snippet [in] Name of snippet.
        @return List of snippet names.
      }
    function GetSnippetUnits(const Snippet: string): IStringList;
      {Get list of all units referenced by a snippet.
        @param Snippet [in] Name of snippet.
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
        @param SnipList [in] List of names of snippets.
      }
    procedure WriteSnippetProps(const SnippetName: string;
      const Props: TSnippetData);
      {Write the properties of a snippet. Always called after all categories are
      written and before WriteSnippetUnits, so can be used to perform any per-
      snippet intialisation.
        @param SnippetName [in] Name of snippet.
        @param Props [in] Properties of snippet.
      }
    procedure WriteSnippetUnits(const SnippetName: string;
      const Units: IStringList);
      {Write the list of units required by a snippet.
        @param SnippetName [in] Name of snippet.
        @param Units [in] List of names of required units.
      }
    procedure WriteSnippetDepends(const SnippetName: string;
      const Depends: IStringList);
      {Write the list of snippets on which a snippet depends.
        @param SnippetName [in] Name of snippet.
        @param Depends [in] List of snippet names.
      }
    procedure WriteSnippetXRefs(const SnippetName: string;
      const XRefs: IStringList);
      {Write the list of snippets that a snippet cross-references.
        @param SnippetName [in] Name of snippet.
        @param XRefs [in] List of snippet names.
      }
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

