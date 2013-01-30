{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2012, Peter Johnson (www.delphidabbler.com).
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
    Frame class that enables one or more snippets from the user defined
    database to be selected. Displays a two-level tree of snippets categories
    with their associated user defined snippets. Each category and snippet has a
    check box that can be checked to select them. A property is exposed that
    gives access to selected snippets.
  }
  TSelectUserSnippetsFrame = class(TSelectSnippetsBaseFrame)
  strict protected
    function CanAddCatNode(const Cat: TCategory): Boolean; override;
      {Checks if a category node should be added to treeview.
        @param Cat [in] Category to be checked.
        @return True if category contains any user-defined snippets.
      }
    function CanAddSnippetNode(const Snippet: TSnippet): Boolean; override;
      {Checks if a snippet node should be added to treeview.
        @param Snippet [in] Snippet to be checked.
        @return True if snippet is user-defined.
      }
  end;


implementation


{$R *.dfm}


{ TSelectUserSnippetsFrame }

function TSelectUserSnippetsFrame.CanAddCatNode(const Cat: TCategory): Boolean;
  {Checks if a category node should be added to treeview.
    @param Cat [in] Category to be checked.
    @return True if category contains any user-defined snippets.
  }
begin
  Result := Cat.Snippets.Count(True) > 0;
end;

function TSelectUserSnippetsFrame.CanAddSnippetNode(
  const Snippet: TSnippet): Boolean;
  {Checks if a snippet node should be added to treeview.
    @param Snippet [in] Snippet to be checked.
    @return True if snippet is user-defined.
  }
begin
  Result := Snippet.UserDefined;
end;

end.

