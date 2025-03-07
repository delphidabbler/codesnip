{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that permits user to select and delete a category.
}


unit FmDeleteCategoryDlg;


interface


uses
  // Delphi
  Forms,
  StdCtrls,
  Controls,
  ExtCtrls,
  Classes,
  // Project
  DB.Categories,
  FmCategoryEditDlg,
  FrCategoryList,
  UBaseObjects;


type

  ///  <summary>Dialogue box that permits the user to select and delete a
  ///  category.</summary>
  TDeleteCategoryDlg = class(TCategoryEditDlg, INoPublicConstruct)
    frmCategories: TCategoryListFrame;
    procedure btnOKClick(Sender: TObject);
  strict private
    fCategories: TCategoryList; // List of categories that can be deleted
    procedure SelectionChangeHandler(Sender: TObject);
      {Handles category list frame's change event. Updates state of OK button
      according to changes.
        @param Sender [in] Not used.
      }

    ///  <summary>Deletes a category from the database.</summary>
    ///  <param name="Cat"><c>TCategory</c> [in] Category to be deleted.</param>
    ///  <remarks>The category must be empty.</remarks>
    procedure DeleteCategory(const Cat: TCategory);

  strict protected
    procedure ConfigForm; override;
      {Configures form. Populates controls and supplies event handler to frame.
      }
    procedure ArrangeForm; override;
      {Sizes and arranges frame in dialog box.
      }
    procedure UpdateOKBtn; override;
      {Updates state of OK button depending on if user has entered valid data in
      dialog box.
      }
  public
    class function Execute(AOwner: TComponent;
      const CatList: TCategoryList): Boolean;
      {Displays dialog box with list of deletable categories. Performs deletion
      if user OKs.
        @param AOwner [in] Component that owns dialog box.
        @param CatList [in] List of categories available for deletion.
      }
  end;


implementation


uses
  // Project
  DB.Main,
  UColours,
  UCtrlArranger,
  UFontHelper;

{$R *.dfm}


{ TDeleteCategoryDlg }

procedure TDeleteCategoryDlg.ArrangeForm;
  {Sizes and arranges frame in dialog box.
  }
begin
  frmCategories.ArrangeFrame;
  inherited;
end;

procedure TDeleteCategoryDlg.btnOKClick(Sender: TObject);
  {OK button click event handler. Deletes category before closing dialog box.
  Confirms with user if category contains snippets.
    @param Sender [in] Not used.
  }
var
  Cat: TCategory; // selected category
begin
  inherited;
  Cat := frmCategories.SelectedCategory;
  Assert(Assigned(Cat), ClassName + '.btnOKClick: No category selected');
  Assert(Cat.CanDelete, ClassName + '.btnOKClick: Category can''t be deleted');
  DeleteCategory(Cat);
end;

procedure TDeleteCategoryDlg.ConfigForm;
  {Configures form. Sets up controls and supplies event handler to frame.
  }
resourcestring
  // Prompt text for frame
  sPrompt = 'Select &category to be deleted:';
begin
  inherited;
  frmCategories.OnChange := SelectionChangeHandler;
  frmCategories.Prompt := sPrompt;
  frmCategories.SetCategories(fCategories);
end;

procedure TDeleteCategoryDlg.DeleteCategory(const Cat: TCategory);
begin
  Assert(Cat.CanDelete, ClassName + '.DeleteCategory: Cat can''t be deleted');
  (Database as IDatabaseEdit).DeleteCategory(Cat);
end;

class function TDeleteCategoryDlg.Execute(AOwner: TComponent;
  const CatList: TCategoryList): Boolean;
  {Displays dialog box with list of deletable categories. Performs deletion if
  user OKs.
    @param AOwner [in] Component that owns dialog box.
    @param CatList [in] List of categories available for deletion.
  }
var
  Dlg: TDeleteCategoryDlg;
begin
  Dlg := InternalCreate(AOwner);
  try
    Dlg.fCategories := CatList;
    Result := Dlg.ShowModal = mrOK;
  finally
    Dlg.Free;
  end;
end;

procedure TDeleteCategoryDlg.SelectionChangeHandler(Sender: TObject);
  {Handles category list frame's change event. Updates state of OK button
  according to changes.
    @param Sender [in] Not used.
  }
begin
  UpdateOKBtn;
end;

procedure TDeleteCategoryDlg.UpdateOKBtn;
  {Updates state of OK button depending on if user has entered valid data in
  dialog box.
  }
begin
  btnOK.Enabled := frmCategories.IsValidEntry
    // following check is potentially redundant, but leaving in for safety
    and frmCategories.SelectedCategory.CanDelete;
end;

end.

