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
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
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
  // Project
  UExceptions, USnippets, UIStringList;


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
    procedure GetRoutineProps(const Routine: string; var Props: TSnippetData);
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
      const Props: TSnippetData);
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
  EDataIO:
    Class of exception raised by IDataReader and IDataWriter objects.
  }
  EDataIO = class(ECodeSnip);


implementation

end.

