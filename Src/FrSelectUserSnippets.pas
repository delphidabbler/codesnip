{
 * FrSelectUserSnippets.pas
 *
 * Implements a frame that enables one or more snippets in the user-defined
 * database to be selected via a tree view.
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
 * The Original Code is FrSelectUserSnippets.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FrSelectUserSnippets;


interface


uses
  // Delphi
  ImgList, Controls, Classes, ComCtrls,
  // Project
  FrSelectSnippetsBase, USnippets;


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
    function CanAddSnippetNode(const Snippet: TRoutine): Boolean; override;
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
  Result := Cat.Routines.Count(True) > 0;
end;

function TSelectUserSnippetsFrame.CanAddSnippetNode(
  const Snippet: TRoutine): Boolean;
  {Checks if a snippet node should be added to treeview.
    @param Snippet [in] Snippet to be checked.
    @return True if snippet is user-defined.
  }
begin
  Result := Snippet.UserDefined;
end;

end.

