{
 * FmRenameCategoryDlg.pas
 *
 * Implements a dialog box that permits user to select and rename a user-defined
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
 * The Original Code is FmRenameCategoryDlg.pas
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


unit FmRenameCategoryDlg;


interface


uses
  // Delphi
  Forms, StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmCategoryEditDlg, FrCategoryList, FrCategoryDescEdit, UBaseObjects,
  USnippets;


type

  {
  TRenameCategoryDlg:
    Dialog box that permits user to select and rename a user-defined category.
  }
  TRenameCategoryDlg = class(TCategoryEditDlg, INoPublicConstruct)
    frmCategories: TCategoryListFrame;
    frmDescription: TCategoryDescEditFrame;
    procedure btnOKClick(Sender: TObject);
  strict private
    fCategories: TCategoryList; // List of categories that can be renamed
    procedure CategoryChangeHandler(Sender: TObject);
      {Handles category list frame's change event. Enters description of
      selected category in description edit then state of OK button accordingly.
        @param Sender [in] Not used.
      }
    procedure DescriptionChangeHandler(Sender: TObject);
      {Handles description frame's change event. Updates state of OK button
      according to entered description.
        @param Sender [in] Not used.
      }
    procedure DescriptionCheckHander(Sender: TObject; const Desc: string;
      var Valid: Boolean);
      {Event handler for description frame called to check the validity of a
      description. Accepts validity passed from caller unless description is
      that of the selected category, in which case it is not considered an
      error.
        @param Sender [in] Not used.
        @param Desc [in] Description being checked.
        @param Valid [in/out] True or false on calling depending if description
          considered valid. Value can be altered to override the decision.
      }
    procedure RenameCategory(const Category: TCategory; const NewDesc: string);
      {Renames category in database.
        @param Category [in] Category to be renamed.
        @param NewDesc [in] New category description.
      }
  strict protected
    procedure ConfigForm; override;
      {Configures form. Populates controls and supplies event handlers to
      frames.
      }
    procedure ArrangeForm; override;
      {Sizes and arranges frames in dialog box.
      }
    procedure UpdateOKBtn; override;
      {Updates state of OK button depending on if user has entered valid data in
      dialog box.
      }
  public
    class function Execute(AOwner: TComponent;
      const CatList: TCategoryList): Boolean;
      {Displays dialog box with list of renamable categories. Performs
      renamining if user OKs.
        @param AOwner [in] Component that owns dialog box.
        @param CatList [in] List of categories available for renaming.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Windows {for inlining},
  // Project
  UCtrlArranger;

{$R *.dfm}


{ TRenameCategoryDlg }

procedure TRenameCategoryDlg.ArrangeForm;
  {Sizes and arranges frames in dialog box.
  }
begin
  frmCategories.ArrangeFrame;
  frmDescription.ArrangeFrame;
  frmDescription.Top := TCtrlArranger.BottomOf(frmCategories, 8);
  inherited;
end;

procedure TRenameCategoryDlg.btnOKClick(Sender: TObject);
  {OK button click event handler. Renames category before closing dialog box.
    @param Sender [in] Not used.
  }
begin
  inherited;
  RenameCategory(
    frmCategories.SelectedCategory, frmDescription.Description
  );
end;

procedure TRenameCategoryDlg.CategoryChangeHandler(Sender: TObject);
  {Handles category list frame's change event. Enters description of selected
  category in description edit then state of OK button accordingly.
    @param Sender [in] Not used.
  }
begin
  frmDescription.Description := frmCategories.SelectedCategory.Description;
  UpdateOKBtn;
end;

procedure TRenameCategoryDlg.ConfigForm;
  {Configures form. Populates controls and supplies event handlers to frames.
  }
resourcestring
  // Prompts
  sCatPrompt = 'Select category to be renamed:';
  sDescPrompt = 'Enter a new description for the category:';
begin
  inherited;
  // Set the required prompt text in frames
  frmCategories.Prompt := sCatPrompt;
  frmDescription.Prompt := sDescPrompt;
  // Set the frames' event handlers
  frmCategories.OnChange := CategoryChangeHandler;
  frmDescription.OnChange := DescriptionChangeHandler;
  frmDescription.OnCheckDescription := DescriptionCheckHander;
  // Populate the categories list
  frmCategories.SetCategories(fCategories);
end;

procedure TRenameCategoryDlg.DescriptionChangeHandler(Sender: TObject);
  {Handles description frame's change event. Updates state of OK button
  according to entered description.
    @param Sender [in] Not used.
  }
begin
  UpdateOKBtn;
end;

procedure TRenameCategoryDlg.DescriptionCheckHander(Sender: TObject;
  const Desc: string; var Valid: Boolean);
  {Event handler for description frame called to check the validity of a
  description. Accepts validity passed from caller unless description is that of
  the selected category, in which case it is not considered an error.
    @param Sender [in] Not used.
    @param Desc [in] Description being checked.
    @param Valid [in/out] True or false on calling depending if description
      considered valid. Value can be altered to override the decision.
  }
begin
  if not Valid then
    if Assigned(frmCategories.SelectedCategory) then
      Valid := AnsiSameText(Desc, frmCategories.SelectedCategory.Description);
end;

class function TRenameCategoryDlg.Execute(AOwner: TComponent;
  const CatList: TCategoryList): Boolean;
  {Displays dialog box with list of renamable categories. Performs renamining if
  user OKs.
    @param AOwner [in] Component that owns dialog box.
    @param CatList [in] List of categories available for renaming.
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

procedure TRenameCategoryDlg.RenameCategory(const Category: TCategory;
  const NewDesc: string);
  {Renames category in database.
    @param Category [in] Category to be renamed.
    @param NewDesc [in] New category description.
  }
var
  EditData: TCategoryData;  // category properties
begin
  EditData := (Snippets as ISnippetsEdit).GetEditableCategoryInfo(Category);
  EditData.Desc := NewDesc;
  (Snippets as ISnippetsEdit).UpdateCategory(Category, EditData);
end;

procedure TRenameCategoryDlg.UpdateOKBtn;
  {Updates state of OK button depending on if user has entered valid data in
  dialog box.
  }
begin
  btnOK.Enabled := frmDescription.IsValidEntry and
    frmCategories.IsValidEntry and
    not AnsiSameStr(
      frmCategories.SelectedCategory.Description, frmDescription.Description
    );
end;

end.

