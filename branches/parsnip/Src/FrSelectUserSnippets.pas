{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a frame that enables one or more snippets in the user-defined
 * database to be selected via a tree view.
}


unit FrSelectUserSnippets;


interface


uses
  // Delphi
  ImgList, Controls, Classes, ComCtrls,
  // Project
  DB.UCategory, DB.USnippet, FrSelectSnippetsBase;


type

  {
  TSelectUserSnippetsFrame:
    Frame class that enables one or more snippets from the database to be
    selected. Displays a two-level tree of snippets categories with their
    associated user defined snippets. Each category and snippet has a check box
    that can be checked to select them. A property is exposed that gives access
    to selected snippets.
  }
  // TODO: Replace use of this frame with FrSelectSnippets and delete unit.
  TSelectUserSnippetsFrame = class(TSelectSnippetsBaseFrame);

implementation


{$R *.dfm}

end.

