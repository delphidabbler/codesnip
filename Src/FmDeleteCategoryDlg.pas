{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that permits user to select and delete a user
 * defined category.
}


unit FmDeleteCategoryDlg;


interface


uses
  // Delphi
  Forms, StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  DB.UCategory, FmCategoryEditDlg, FrCategoryList, UBaseObjects;


type
  {
  TDeleteCategoryDlg:
    Dialog box that permits user to select and delete a user-defined category.
  }
  TDeleteCategoryDlg = class(TCategoryEditDlg, INoPublicConstruct)
    frmCategories: TCategoryListFrame;
    lblErrorMsg: TLabel;
    procedure btnOKClick(Sender: TObject);
  strict private
    fCategories: TCategoryList; // List of categories that can be deleted
    procedure SelectionChangeHandler(Sender: TObject);
      {Handles category list frame's change event. Updates state of OK button
      according to changes.
        @param Sender [in] Not used.
      }
    procedure UpdateErrorLabelState;
      {Shows or hides error state label depending of whether selected category
      can be deleted.
      }
    procedure DeleteCategory(const Cat: TCategory);
      {Deletes category and all its snippets from database.
        @param Cat [in] Category to be deleted.
      }
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
  DB.UMain, UColours, UCtrlArranger, UFontHelper;

{$R *.dfm}


{ TDeleteCategoryDlg }

procedure TDeleteCategoryDlg.ArrangeForm;
  {Sizes and arranges frame in dialog box.
  }
begin
  frmCategories.ArrangeFrame;
  TCtrlArranger.SetLabelHeight(lblErrorMsg);
  lblErrorMsg.Top := TCtrlArranger.BottomOf(frmCategories, 8);
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
  TFontHelper.SetDefaultFont(lblErrorMsg.Font);
  lblErrorMsg.Font.Color := clWarningText;
  lblErrorMsg.Visible := False;
end;

procedure TDeleteCategoryDlg.DeleteCategory(const Cat: TCategory);
  {Deletes category and all its snippets from database.
    @param Cat [in] Category to be deleted.
  }
begin
  (Database as IDatabaseEdit).DeleteCategory(Cat);
end;

class function TDeleteCategoryDlg.Execute(AOwner: TComponent;
  const CatList: TCategoryList): Boolean;
  {Displays dialog box with list of deletable categories. Performs deletion if
  user OKs.
    @param AOwner [in] Component that owns dialog box.
    @param CatList [in] List of categories available for deletion.
  }
begin
  with InternalCreate(AOwner) do
    try
      fCategories := CatList;
      Result := ShowModal = mrOK;
    finally
      Free;
    end;
end;

procedure TDeleteCategoryDlg.SelectionChangeHandler(Sender: TObject);
  {Handles category list frame's change event. Updates state of OK button
  according to changes.
    @param Sender [in] Not used.
  }
begin
  UpdateOKBtn;
  UpdateErrorLabelState;
end;

procedure TDeleteCategoryDlg.UpdateErrorLabelState;
  {Shows or hides error state label depending of whether selected category can
  be deleted.
  }
begin
  if not frmCategories.IsValidEntry then
    lblErrorMsg.Visible := False
  else
    lblErrorMsg.Visible := not frmCategories.SelectedCategory.CanDelete;
end;

procedure TDeleteCategoryDlg.UpdateOKBtn;
  {Updates state of OK button depending on if user has entered valid data in
  dialog box.
  }
begin
  btnOK.Enabled := frmCategories.IsValidEntry
    and frmCategories.SelectedCategory.CanDelete;
end;

end.

