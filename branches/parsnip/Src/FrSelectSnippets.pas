{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a frame that enables one or more snippets from both the user and
 * main databases to be selected via a tree view.
}


unit FrSelectSnippets;


interface


uses
  // Delphi
  ImgList, Controls, Classes, ComCtrls,
  // Project
  DB.UCategory, DB.USnippet, FrSelectSnippetsBase;


type

  {
  TSelectSnippetsFrame:
    Frame class that enables one or more snippets the database to be selected.
    Displays a two-level tree of snippets categories with their associated
    snippets. Each category and snippet has a check box that can be checked to
    select them. A property is exposed that gives access to selected snippets.
  }
  { TODO: Delete this frame and rename TSelectSnippetsBaseFrame as
          TSelectSnippetsFrame. Change code that uses this unit to use
          FrSelectSnippetsBase. }
  TSelectSnippetsFrame = class(TSelectSnippetsBaseFrame);


implementation


{$R *.dfm}

end.

