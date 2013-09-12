{
 * FrCategoryList.pas
 *
 * Implements a frame that displays a list of categories and provides access to
 * the selected category.
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
 * The Original Code is FrCategoryList.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FrCategoryList;


interface


uses
  // Delphi
  Forms, Controls, StdCtrls, Classes,
  // Project
  UCategoryListAdapter, USnippets;


type
  {
  TCategoryListFrame:
    Frame that displays a list of categories and provides access to the selected
    category.
  }
  TCategoryListFrame = class(TFrame)
    lbCategories: TListBox;
    lblCategories: TLabel;
    procedure lbCategoriesClick(Sender: TObject);
    procedure FrameEnter(Sender: TObject);
  strict private
    fCatList: TCategoryListAdapter;
    fOnChange: TNotifyEvent;
    function GetPrompt: string;
      {Read accessor for Prompt property.
        @return Current prompt text.
      }
    procedure SetPrompt(const Value: string);
      {Write accessor for Prompt property.
        @param Value [in] New prompt text to display.
      }
    procedure DoChange;
      {Triggers OnChange event providing a category is selected.
      }
    function GetSelectedCategory: TCategory;
      {Read accessor for SelectedCategory property.
        @return Category selected in list box or nil if no category selected.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets up object.
        @param AOwner [in] Component that owns this frame.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    procedure SetCategories(const CatList: TCategoryList);
      {Displays the required categories in the list box, sorted alphabetically
      by description
        @param CatList [in] List of categories to be displayed.
      }
    procedure ArrangeFrame;
      {Arranges controls in frame and sizes it to fit the controls.
      }
    function IsValidEntry: Boolean;
      {Checks if data entered in frame is valid.
        @return True if entry is valid, False if not.
      }
    property Prompt: string read GetPrompt write SetPrompt;
      {Text that is displayed above categories list box}
    property SelectedCategory: TCategory read GetSelectedCategory;
      {Category currently selected in list box. Nil if no selection has been
      made}
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
      {Event triggered category selected in list box changes}
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UCtrlArranger;

{$R *.dfm}

{ TCategoryListFrame }

procedure TCategoryListFrame.ArrangeFrame;
  {Arranges controls in frame and sizes it to fit the controls.
  }
begin
  lbCategories.Top := TCtrlArranger.BottomOf(lblCategories, 4);
  Self.ClientHeight := TCtrlArranger.TotalControlHeight(Self);
  Self.ClientWidth := TCtrlArranger.TotalControlWidth(Self);
end;

constructor TCategoryListFrame.Create(AOwner: TComponent);
  {Class constructor. Sets up object.
    @param AOwner [in] Component that owns this frame.
  }
begin
  inherited Create(AOwner);
end;

destructor TCategoryListFrame.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fCatList);
  inherited;
end;

procedure TCategoryListFrame.DoChange;
  {Triggers OnChange event providing a category is selected.
  }
begin
  if Assigned(fOnChange) and Assigned(SelectedCategory) then
    fOnChange(Self);
end;

procedure TCategoryListFrame.FrameEnter(Sender: TObject);
  {Handles frame's OnEnter event. Sets focus on categories list box.
    @param Sender [in] Not used.
  }
begin
  lbCategories.SetFocus;
end;

function TCategoryListFrame.GetPrompt: string;
  {Read accessor for Prompt property.
    @return Current prompt text.
  }
begin
  Result := lblCategories.Caption;
end;

function TCategoryListFrame.GetSelectedCategory: TCategory;
  {Read accessor for SelectedCategory property.
    @return Category selected in list box or nil if no category selected.
  }
var
  CatID: string;  // id of selected category
begin
  if lbCategories.ItemIndex >= 0 then
  begin
    CatID := fCatList.CatName(lbCategories.ItemIndex);
    Result := Snippets.Categories.Find(CatID);
  end
  else
    Result := nil;
end;

function TCategoryListFrame.IsValidEntry: Boolean;
  {Checks if data entered in frame is valid.
    @return True if entry is valid, False if not.
  }
begin
  // Data considered valid if a category is selected in the list box
  Result := lbCategories.ItemIndex >= 0;
end;

procedure TCategoryListFrame.lbCategoriesClick(Sender: TObject);
  {Handles category list box OnClick events. Notifies that list box selection
  has changed.
    @param Sender [in] Not used.
  }
begin
  DoChange;
end;

procedure TCategoryListFrame.SetCategories(const CatList: TCategoryList);
  {Displays the required categories in the list box, sorted alphabetically by
  description
    @param CatList [in] List of categories to be displayed.
  }
begin
  // create category list object to order the categories
  FreeAndNil(fCatList);
  fCatList := TCategoryListAdapter.Create(CatList);
  // display categories in list box
  lbCategories.Clear;
  fCatList.ToStrings(lbCategories.Items);
  DoChange;
end;

procedure TCategoryListFrame.SetPrompt(const Value: string);
  {Write accessor for Prompt property.
    @param Value [in] New prompt text to display.
  }
begin
  lblCategories.Caption := Value;
end;

end.

