{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines records that encapsulate SWAG database categories and snippets.
}


unit SWAG.UCommon;


interface


type
  ///  <summary>Record that encapsulates the data of a SWAG category.</summary>
  TSWAGCategory = record
    ///  <summary>String that uniquely identifies a SWAG category.</summary>
    ID: string;
    ///  <summary>SWAG category title.</summary>
    Title: string;
  end;

type
  ///  <summary>Record that encapsulates the data that defines a SWAG snippet
  ///  </summary>
  TSWAGSnippet = record
    ///  <summary>Number that uniquely identifies a SWAG snippet.</summary>
    ID: Cardinal;
    ///  <summary>ID of SWAG category that snippet belongs to.</summary>
    Category: string;
    ///  <summary>File name of snippet in original SWAG archive.</summary>
    FileName: string;
    ///  <summary>Date and time snippet was added to or updated in the SWAG
    ///  archive.</summary>
    DateStamp: TDateTime;
    ///  <summary>SWAG snippet title.</summary>
    Title: string;
    ///  <summary>Name of author(s) of SWAG snippet.</summary>
    Author: string;
    ///  <summary>Source code of SWAG snippet.</summary>
    ///  <remarks>Strictly speaking this is the text of the snippet since not
    ///  all snippets are pure source code - some are text documents.</remarks>
    SourceCode: string;
    ///  <summary>Flag that indicates if SWAG snippet is a text document (True)
    ///  or is Pascal source code (False).</summary>
    IsDocument: Boolean;
  end;


implementation

end.

