{
 * USnipData.pas
 *
 * Defines interface implemented by classes that read and write back end Code
 * Snippets database. Also provides an exception class for data I/O errors.
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
 * The Original Code is USnipData.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
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
* Categories - details of each category of snippets                            *
* ---------------------------------------------------------------------------- *
* Category        | Name or ID of category (primary key)                       *
* Description     | Description of category                                    *
* UserDefined     | Indicates if snippet is user-defined                       *
* ---------------------------------------------------------------------------- *
* Note)   Categories has a one-many relationship with Snippets                 *
* ============================================================================ *
*                                                                              *
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *
* Snippets - details of each snippet                                           *
* ---------------------------------------------------------------------------- *
* Name            | Pascal Name of snippet (primary key)                       *
* Category        | Name of category snippet belongs to                        *
* Description     | Description of purpose of snippet                          *
* SourceCode      | Snippet's source code                                      *
* Comments        | Additional comments about snippets                         *
* Credits         | Any credits - any URL link text delimited by [ ]           *
* CreditsURL      | Any URL associated with credits                            *
* Compatibility   | String of characters describing compiler compatibility     *
* UserDefined     | Indicates if snippet is user-defined                       *
* StandardFormat  | Indicates if snippet is in standard format                 *
* ---------------------------------------------------------------------------- *
* Note 1) Snippets have a many-many relationship with same table, both for     *
*         dependency lists and cross-reference lists.                          *
* Note 2) Snippets have a many-many relationship with units.                   *
* ============================================================================ *
*                                                                              *
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *
* Units - names of required units                                              *
* ---------------------------------------------------------------------------- *
* Unit             | Name of unit (primary key)                             *
* ---------------------------------------------------------------------------- *
* Note)   There is no other information in this table - so we may omit it      *
* ============================================================================ *
*                                                                              *
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *
* SnippetUnits - map of snippets to units                                      *
* ---------------------------------------------------------------------------- *
* SnippetName      | Name of snippet                                           *
* UnitName         | Name of unit                                              *
* ============================================================================ *
*                                                                              *
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *
* SnippetXRef - map of snippets to cross-referenced snippets                   *
* ---------------------------------------------------------------------------- *
* SnippetName      | Name of snippet                                           *
* XRefSnippetName  | Name of cross referenced snippet                          *
* ============================================================================ *
*                                                                              *
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *
* SnippetDepends - map of snippets to the snippets they depend upon            *
* ---------------------------------------------------------------------------- *
* SnippetName      | Name of snippet                                           *
* RequiredSnippet  | Name of required snippet                                  *
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
  // Project
  DB.USnippet, UExceptions, USnippets, UIStringList;


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
    function GetCatSnippets(const Cat: string): IStringList;
      {Get names of all snippets in a category.
        @param Cat [in] Name of category containing snippets.
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
  }
  IDataWriter = interface(IInterface)
    ['{71E892C4-6E0F-480A-9DF4-70835F83A0CA}']
    procedure Initialise;
      {Initialise the database. Always called before any other methods.
      }
    procedure WriteCatProps(const CatName: string; const Props: TCategoryData);
      {Write the properties of a category. Always called before WriteCatSnippets
      for a given category, so can be used to perform any per-category
      initialisation.
        @param CatName [in] Name of category.
        @param Props [in] Properties of category.
      }
    procedure WriteCatSnippets(const CatName: string;
      const SnipList: IStringList);
      {Write the list of snippets belonging to a category. Always called after
      WriteCatProps for any given category.
        @param CatName [in] Name of category.
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

