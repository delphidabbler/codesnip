{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a frame that enables one or more snippets from both the user and
 * main databases to be selected via a tree view.
}


unit FrSelectSnippets;


interface


uses
  // Delphi
  ImgList,
  Controls,
  Classes,
  ComCtrls,
  // Project
  DB.Categories,
  DB.Snippets,
  FrSelectSnippetsBase;


type

  {
  TSelectSnippetsFrame:
    Frame class that enables one or more snippets from both the user and main
    databases to be selected. Displays a two-level tree of snippets categories
    with their associated snippets. Each category and snippet has a check box
    that can be checked to select them. A property is exposed that gives access
    to selected snippets.
  }
  TSelectSnippetsFrame = class(TSelectSnippetsBaseFrame)
  strict protected
    function CanAddCatNode(const Cat: TCategory): Boolean; override;
      {Checks if a category node should be added to treeview.
        @param Cat [in] Category to be checked.
        @return True if category contains any snippets.
      }
    function CanAddSnippetNode(const Snippet: TSnippet): Boolean; override;
      {Checks if a snippet node should be added to treeview.
        @param Snippet [in] Snippet to be checked.
        @return True. All snippets should be added.
      }
  end;


implementation


{$R *.dfm}


{ TSelectSnippetsFrame }

function TSelectSnippetsFrame.CanAddCatNode(const Cat: TCategory): Boolean;
  {Checks if a category node should be added to treeview.
    @param Cat [in] Category to be checked.
    @return True if category contains any snippets.
  }
begin
  Result := not Cat.Snippets.IsEmpty;
end;

function TSelectSnippetsFrame.CanAddSnippetNode(
  const Snippet: TSnippet): Boolean;
  {Checks if a snippet node should be added to treeview.
    @param Snippet [in] Snippet to be checked.
    @return True. All snippets should be added.
  }
begin
  Result := True;
end;

end.

