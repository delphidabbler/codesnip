{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2021, Peter Johnson (gravatar.com/delphidabbler).
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


uses
  DB.UCollections;


{$R *.dfm}


{TODO -cRefactor: TSelectUserSnippetsFrame is now identical to
        TSelectSnippetsFrame, so both can be collapsed into base class,
        TSelectSnippetsBaseFrame.

        Add an event that the owning form can handle to filter out any unwanted
        snippets from being displayed.

        TSelectUserSnippetsFrame and TSelectSnippetsFrame should then be removed
        and TSelectSnippetsBaseFrame should be renamed as TSelectSnippetsFrame.

        Finally FrSelectUserSnippets and FrSelectSnippets units can be removed
        and FrSelectSnippetsBase can be renamed as FrSelectSnippets.
}

{ TSelectUserSnippetsFrame }

function TSelectUserSnippetsFrame.CanAddCatNode(const Cat: TCategory): Boolean;
  {Checks if a category node should be added to treeview.
    @param Cat [in] Category to be checked.
    @return True if category contains any user-defined snippets.
  }
begin
  Result := not Cat.Snippets.IsEmpty;
end;

function TSelectUserSnippetsFrame.CanAddSnippetNode(
  const Snippet: TSnippet): Boolean;
  {Checks if a snippet node should be added to treeview.
    @param Snippet [in] Snippet to be checked.
    @return True if snippet is user-defined.
  }
begin
  Result := True;
end;

end.

