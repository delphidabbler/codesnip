{
 * FrSelectSnippets.pas
 *
 * Implements a frame that enables one or more snippets from both the user and
 * main databases to be selected via a tree view.
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
 * The Original Code is FrSelectSnippets.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FrSelectSnippets;


interface


uses
  // Delphi
  ImgList, Controls, Classes, ComCtrls,
  // Project
  FrSelectSnippetsBase, USnippets;


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
    function CanAddSnippetNode(const Snippet: TRoutine): Boolean; override;
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
  Result := Cat.Routines.Count > 0;
end;

function TSelectSnippetsFrame.CanAddSnippetNode(
  const Snippet: TRoutine): Boolean;
  {Checks if a snippet node should be added to treeview.
    @param Snippet [in] Snippet to be checked.
    @return True. All snippets should be added.
  }
begin
  Result := True;
end;

end.

