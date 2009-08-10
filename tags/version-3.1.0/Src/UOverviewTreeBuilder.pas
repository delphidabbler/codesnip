{
 * UOverviewTreeBuilder.pas
 *
 * Implements a set of classes that populate the overview treeview with a list
 * of snippets. Each class groups the snippets in different ways.
 *
 * v1.0 of 27 Jun 2009  - Original version.
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
 * The Original Code is UOverviewTreeBuilder.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UOverviewTreeBuilder;


interface


uses
  // Delphu
  ComCtrls,
  // Project
  USnippets, UView, UViewItemTreeNode;


type
  {
  TOverviewTreeBuilder:
    Abstract base class for classes that populate the overview treeview with a
    list of snippets. Each subclass groups the snippets in different ways.
  }
  TOverviewTreeBuilder = class abstract(TObject)
  strict private
    fTreeView: TTreeView;       // Value of TreeView property
    fSnippetList: TRoutineList; // Value of SnippetList property
  strict protected
    property TreeView: TTreeView read fTreeView;
      {Reference to treeview populated by class}
    property SnippetList: TRoutineList read fSnippetList;
      {List of snippets to be displayed in treeview}
    function AddViewItemNode(const ParentNode: TViewItemTreeNode;
      const ViewItem: TViewItem): TViewItemTreeNode;
      {Adds a new node to the tree view that represents a view item.
        @param ParentNode [in] Node that is parent of new node.
        @param ViewItem [in] View item for which we are adding node.
        @return New tree node.
      }
  public
    constructor Create(const TV: TTreeView; const SnippetList: TRoutineList);
      {Class constructor. Sets up object to populate a treeview with a list of
      snippets.
        @param TV [in] Treeview control to be populated.
        @param SnippetList [in] List of snippets to be added to TV.
      }
    procedure Build; virtual; abstract;
      {Abstract method that descendants overrride to populate the treeview.
      }
  end;

  {
  TOverviewTreeBuilderClass:
    Class reference for TOverviewTreeBuilder sub-classes.
  }
  TOverviewTreeBuilderClass = class of TOverviewTreeBuilder;

  {
  TOverviewCategorisedTreeBuilder:
    Class that populates the overview treeview with a list of snippets grouped
    by category.
  }
  TOverviewCategorisedTreeBuilder = class sealed(TOverviewTreeBuilder)
  public
    procedure Build; override;
      {Populates treeview with snippets grouped by category.
      }
  end;

  {
  TOverviewAlphabeticTreeBuilder:
    Class that populates the overview treeview with a list of snippets grouped
    by initial letter of the snippet name.
  }
  TOverviewAlphabeticTreeBuilder = class sealed(TOverviewTreeBuilder)
  public
    procedure Build; override;
      {Populates treeview with snippets grouped by initial letter of snippet
      name.
      }
  end;

  {
  TOverviewSnipKindTreeBuilder:
    Class that populates the overview treeview with a list of snippets grouped
    by snippet kind.
  }
  TOverviewSnipKindTreeBuilder = class sealed(TOverviewTreeBuilder)
  public
    procedure Build; override;
      {Populates treeview with snippets grouped by snippet kind.
      }
  end;


implementation


{
  NOTE:

  Early version of this code, that were contained in the FrOverview unit used
  to add a section head tree node for every category and then delete empty ones.
  However, attempts to delete the first node from a tree causes an endless loop,
  freezing the program. This could be a bug in the Delphi treeview component.

  Therefore the code in this unit now builds or uses a list of routines in for
  each section and does not create a section header node for sections that
  contain no routines.
}


uses
  // Delphi
  SysUtils, Classes,
  // Project
  UAlphabet, ULists, USnippetKindInfo;


{ TOverviewTreeBuilder }

function TOverviewTreeBuilder.AddViewItemNode(
  const ParentNode: TViewItemTreeNode;
  const ViewItem: TViewItem): TViewItemTreeNode;
  {Adds a new node to the tree view that represents a view item.
    @param ParentNode [in] Node that is parent of new node.
    @param ViewItem [in] View item for which we are adding node.
    @return New tree node.
  }
begin
  Result := TreeView.Items.AddChild(ParentNode, ViewItem.Description)
    as TViewItemTreeNode;
  Result.ViewItem := ViewItem;
end;

constructor TOverviewTreeBuilder.Create(const TV: TTreeView;
  const SnippetList: TRoutineList);
  {Class constructor. Sets up object to populate a treeview with a list of
  snippets.
    @param TV [in] Treeview control to be populated.
    @param SnippetList [in] List of snippets to be added to TV.
  }
begin
  inherited Create;
  fTreeView := TV;
  fSnippetList := SnippetList;
end;

{ TOverviewCategorisedTreeBuilder }

procedure TOverviewCategorisedTreeBuilder.Build;
  {Populates treeview with snippets grouped by category.
  }
var
  CatNode: TViewItemTreeNode; // reference to a category section node
  Cat: TCategory;             // each category in database
  Snippet: TRoutine;          // each snippet in a category
  Sections: TStringList;      // maps category to list of snippets in it
  CatDescs: TStringList;      // alphabetical list of category descriptions
  CatDescIdx: Integer;        // loops thru category description list
  SectionIdx: Integer;        // index of a category in Sections
begin
  // Create string list that maps categories onto snippets
  CatDescs := nil;
  Sections := TStringList.Create;
  try
    Sections.Sorted := True;
    CatDescs := TStringList.Create;
    CatDescs.Sorted := True;
    // Create required lists: 1) Sections maps categories to routine lists and
    // 2) CatDescs maintains alphabetical list of section names
    for Cat in Snippets.Categories do
    begin
      Sections.AddObject(Cat.Category, TRoutineList.Create);
      CatDescs.AddObject(Cat.Description, Cat);
    end;
    // Add all snippets into correct category list
    for Snippet in SnippetList do
    begin
      SectionIdx := Sections.IndexOf(Snippet.Category);
      if SectionIdx >= 0 then
        (Sections.Objects[SectionIdx] as TRoutineList).Add(Snippet);
    end;
    // Create tree
    for CatDescIdx := 0 to Pred(CatDescs.Count) do
    begin
      Cat := CatDescs.Objects[CatDescIdx] as TCategory;
      SectionIdx := Sections.IndexOf(Cat.Category);
      Assert(SectionIdx >= 0,
        ClassName + '.Build: Category not in section list');
      if (Sections.Objects[SectionIdx] as TRoutineList).Count > 0 then
      begin
        CatNode := AddViewItemNode(nil, TViewItem.Create(vkCategory, Cat));
        for Snippet in (Sections.Objects[SectionIdx] as TRoutineList) do
          AddViewItemNode(CatNode, TViewItem.Create(Snippet));
      end;
    end;
  finally
    FreeAndNil(CatDescs);
    // Free section list and contained snippet lists
    for SectionIdx := Pred(Sections.Count) downto 0 do
      Sections.Objects[SectionIdx].Free;
    FreeAndNil(Sections);
  end;
end;

{ TOverviewAlphabeticTreeBuilder }

procedure TOverviewAlphabeticTreeBuilder.Build;
  {Populates treeview with snippets grouped by initial letter of snippet name.
  }

  function FirstCharOfName(const Name: string): Char;
    {Gets the first character of a name.
      @param Name [in] Name for which first character required.
      @return Required character in upper case.
    }
  begin
    Assert(Name <> '', ClassName + '.Build:FirstCharOfName: Name is empty');
    Result := UpCase(Name[1]);
    Assert(Result in ['_', 'A'..'Z'],
      ClassName +
        '.Build:FirstCharOfName: Name must begin with A..Z or underscore');
  end;

var
  Snippet: TRoutine;              // each snippet in a list
  Sections: TIntegerList;         // alphabetic (+ underscore) sections
  SectionSnippets: TRoutineList;  // snippets in in section
  LetterObj: TLetter;             // references each letter object
  Idx: Integer;                   // loops through all letters in Sections list
  ParentNode: TViewItemTreeNode;  // each section node in tree
begin
  Sections := TIntegerList.Create;
  try
    // Create a snippet list for each letter
    TAlphabet.Instance.InitEnum;
    while TAlphabet.Instance.NextLetter(LetterObj) do
      Sections.Add(Ord(LetterObj.Letter), TRoutineList.Create);
    // populate section snippet list depending on snippet's initial letter
    for Snippet in SnippetList do
    begin
      SectionSnippets := Sections.FindObject(
        Ord(FirstCharOfName(Snippet.Name))
      ) as TRoutineList;
      if Assigned(SectionSnippets) then
        SectionSnippets.Add(Snippet);
    end;
    // Create tree nodes only section contains snippets
    TAlphabet.Instance.InitEnum;
    while TAlphabet.Instance.NextLetter(LetterObj) do
    begin
      SectionSnippets := Sections.FindObject(Ord(LetterObj.Letter))
        as TRoutineList;
      if Assigned(SectionSnippets) and (SectionSnippets.Count > 0) then
      begin
        ParentNode := AddViewItemNode(nil, TViewItem.Create(LetterObj));
        for Snippet in SectionSnippets do
          AddViewItemNode(ParentNode, TViewItem.Create(Snippet));
      end;
    end;
  finally
    // Free all sections and owned snippet lists
    for Idx := 0 to Pred(Sections.Count) do
      Sections.Objects[Idx].Free;
    FreeAndNil(Sections);
  end;
end;

{ TOverviewSnipKindTreeBuilder }

procedure TOverviewSnipKindTreeBuilder.Build;
  {Populates treeview with snippets grouped by snippet kind.
  }
var
  Snippet: TRoutine;              // each snippet in a snippets list
  Kind: TSnippetKind;             // each snippet kind
  ParentNode: TViewItemTreeNode;  // tree node for each snippet kind
  Sections: array[TSnippetKind]   // snippets associated with each kind
    of TRoutineList;
begin
  // Create snippet lists for each snippet kind
  for Kind := Low(TSnippetKind) to High(TSnippetKind) do
    Sections[Kind] := nil;  // make safe in case exception occurs below
  try
    for Kind := Low(TSnippetKind) to High(TSnippetKind) do
      Sections[Kind] := TRoutineList.Create;
    // Allocate each snippet to appropriate snippet kind list
    for Snippet in SnippetList do
      Sections[Snippet.Kind].Add(Snippet);
    // Create tree nodes only if section contains snippets
    for Kind := Low(TSnippetKind) to High(TSnippetKind) do
    begin
      if Sections[Kind].Count > 0 then
      begin
        ParentNode := AddViewItemNode(
          nil, TViewItem.Create(TSnippetKindInfoList.Instance[Kind])
        );
        for Snippet in Sections[Kind] do
          AddViewItemNode(ParentNode, TViewItem.Create(Snippet));
      end;
    end;
  finally
    // Free each snippet list
    for Kind := Low(TSnippetKind) to High(TSnippetKind) do
      Sections[Kind].Free;
  end;
end;

end.

