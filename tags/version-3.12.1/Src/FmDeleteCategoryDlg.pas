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
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
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
  FmCategoryEditDlg, FrCategoryList, UBaseObjects, USnippets;


type
  {
  TDeleteCategoryDlg:
    Dialog box that permits user to select and delete a user-defined category.
  }
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
  // Delphi
  SysUtils,
  // Project
  UConsts, UMessageBox;

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
resourcestring
  // Confirmation prompt
  sConfirmDelete = 'The "%0:2s" category contains %1:d snippet(s).'
    + EOL2
    + 'If you delete the category all the snippet(s) will also be deleted.'
    + EOL2 + EOL
    + 'Are you sure you want to delete "%0:2s"?';
begin
  inherited;
  Cat := frmCategories.SelectedCategory;
  Assert(Assigned(Cat), ClassName + '.btnOKClick: No category selected');
  if (Cat.Routines.Count = 0)
    or TMessageBox.Confirm(
      Self, Format(sConfirmDelete, [Cat.Description, Cat.Routines.Count])
    ) then
    DeleteCategory(Cat)
  else
    ModalResult := mrNone;
end;

procedure TDeleteCategoryDlg.ConfigForm;
  {Configures form. Populates controls and supplies event handler to frame.
  }
resourcestring
  // Prompt text for frame
  sPrompt = 'Select category to be deleted:';
begin
  inherited;
  frmCategories.OnChange := SelectionChangeHandler;
  frmCategories.Prompt := sPrompt;
  frmCategories.SetCategories(fCategories);
end;

procedure TDeleteCategoryDlg.DeleteCategory(const Cat: TCategory);
  {Deletes category and all its snippets from database.
    @param Cat [in] Category to be deleted.
  }
begin
  (Snippets as ISnippetsEdit).DeleteCategory(Cat);
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
end;

procedure TDeleteCategoryDlg.UpdateOKBtn;
  {Updates state of OK button depending on if user has entered valid data in
  dialog box.
  }
begin
  btnOK.Enabled := frmCategories.IsValidEntry;
end;

end.

