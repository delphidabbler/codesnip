{
 * DB.UConsts.pas
 *
 * Common constant and type definitions for use in the database code in the DB
 * namespace.
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
 * The Original Code is DB.UConsts.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2010-2013 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit DB.UConsts;

interface

type
  ///  <summary>Enumeration of different data sources recorded in database.
  ///  </summary>
  TDBDataSource = (
    dsCodeSnip,   // DelphiDabbler Code Snippets database
    dsLocalUser   // local user defined database
  );

type
  ///  <summary>Enumeration of various supported kinds of snippets.</summary>
  TDBSnippetKind = (
    skFreeform,   // free-form code - not in any of other supported formats
    skRoutine,    // procedure or function in standard format
    skConstant,   // constant definition in standard format
    skTypeDef,    // simple type definition in standard format
    skUnit,       // complete source code unit
    skClass       // Delphi class or advanced record
  );

implementation

end.

