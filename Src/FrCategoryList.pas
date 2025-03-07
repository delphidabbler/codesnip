{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a frame that displays a list of categories and provides access to
 * the selected category.
}


unit FrCategoryList;


interface


uses
  // Delphi
  Forms,
  Controls,
  StdCtrls,
  Classes,
  // Project
  DB.Categories,
  UI.Adapters.CategoryList;


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
  DB.Main,
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
    CatID := fCatList.CatID(lbCategories.ItemIndex);
    Result := Database.Categories.Find(CatID);
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

