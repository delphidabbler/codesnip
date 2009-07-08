{
 * FrSelectUserSnippets.pas
 *
 * Implements a frame that enables one or more user-defined routines in the
 * snippets database to be selected via a tree view.
 *
 * v1.0 of 30 Nov 2008  - Original version.
 *
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
 * Portions created by the Initial Developer are Copyright (C) 2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FrSelectUserSnippets;


interface


uses
  // Delphi
  ImgList, Controls, Classes, ComCtrls,
  // Project
  FrSelectSnippets;


type

  {
  TSelectUserSnippetsFrame:
    Implements a frame that enables one or more user-defined routines in the
    snippets database to be selected. It displays a two-level tree of snippets
    categories with their associated user-defined routines. Each category and
    routine has a check box that can be checked to select them. A property is
    exposed that gives access to selected routines.
  }
  TSelectUserSnippetsFrame = class(TSelectSnippetsFrame)
  strict protected
    procedure AddNodes; override;
      {Adds nodes for each category and the routines it contains to an empty
      tree view.
      }
  end;


implementation


uses
  // Project
  FrCheckedTV, USnippets;


{$R *.dfm}


{ TSelectUserSnippetsFrame }

procedure TSelectUserSnippetsFrame.AddNodes;
  {Adds nodes for each category and the routines it contains to an empty
  tree view.
  }
var
  Cat: TCategory;               // reference to a category
  CatNode: TCheckedTreeNode;    // tree node representing a category
  Routine: TRoutine;            // reference to routines in a category
begin
  // Add each category as top level node
  for Cat in Snippets.Categories do
  begin
    // Don't add category node if has no user defined routines
    if Cat.Routines.Count(True) = 0 then
      Continue;
    CatNode := AddNode(nil, Cat.Description, Cat);
    // Add each user defined routine in category as child of category node
    for Routine in Cat.Routines do
      if Routine.UserDefined then
        AddNode(CatNode, Routine.Name, Routine);
  end;
end;

end.

