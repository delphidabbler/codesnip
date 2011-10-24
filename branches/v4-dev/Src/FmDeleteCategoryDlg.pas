{
 * FmDeleteCategoryDlg.pas
 *
 * Implements a dialog box that permits user to select and delete a user-defined
 * category.
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
 * The Original Code is FmDeleteCategoryDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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
    function CanDelete(const Cat: TCategory): Boolean;
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
  Assert(CanDelete(Cat), ClassName + '.btnOKClick: Category can''t be deleted');
  DeleteCategory(Cat);
end;

function TDeleteCategoryDlg.CanDelete(const Cat: TCategory): Boolean;
begin
  { TODO: Make a method of TCategory that checks this, and add assertion to
          check this and remove snippet deletion code from delete category
          method of TDatabase. }
  Result := Cat.Snippets.Count = 0;
end;

procedure TDeleteCategoryDlg.ConfigForm;
  {Configures form. Sets up controls and supplies event handler to frame.
  }
resourcestring
  // Prompt text for frame
  sPrompt = 'Select category to be deleted:';
begin
  inherited;
  frmCategories.OnChange := SelectionChangeHandler;
  frmCategories.Prompt := sPrompt;
  frmCategories.SetCategories(fCategories);
  lblErrorMsg.Font.Color := clWarningText;
  TFontHelper.SetDefaultBaseFont(lblErrorMsg.Font, False);
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
begin
  if not frmCategories.IsValidEntry then
    lblErrorMsg.Visible := False
  else
    lblErrorMsg.Visible := not CanDelete(frmCategories.SelectedCategory);
end;

procedure TDeleteCategoryDlg.UpdateOKBtn;
  {Updates state of OK button depending on if user has entered valid data in
  dialog box.
  }
begin
  btnOK.Enabled := frmCategories.IsValidEntry
    and CanDelete(frmCategories.SelectedCategory);
end;

end.

