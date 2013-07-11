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
  TSWAGCategory = record
    ID: string;
    Title: string;
  end;

type
  TSWAGSnippet = record
    ID: Cardinal;
    Category: string;
    FileName: string;
    DateStamp: TDateTime;
    Title: string;
    Author: string;
    SourceCode: string;
    IsDocument: Boolean;
  end;


implementation

end.

