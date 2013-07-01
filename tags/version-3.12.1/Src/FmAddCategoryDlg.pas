{
 * FmAddCategoryDlg.pas
 *
 * Implements a dialog that permits user to add a new user defined category to
 * the database.
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
 * The Original Code is FmAddCategoryDlg.pas
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


unit FmAddCategoryDlg;


interface


uses
  // Delphi
  Forms, StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmCategoryEditDlg, FrCategoryDescEdit, UBaseObjects;


type

  {
  TAddCategoryDlg:
    Dialog box that permits user to add a new user defined category to the
    database.
  }
  TAddCategoryDlg = class(TCategoryEditDlg, INoPublicConstruct)
    frmDescription: TCategoryDescEditFrame;
    procedure btnOKClick(Sender: TObject);
  strict private
    procedure DescriptionChangeHandler(Sender: TObject);
      {Handles description frame's change event. Updates state of OK button
      according to entered description.
        @param Sender [in] Not used.
      }
    procedure AddCategory(const Desc: string);
      {Creates new category and adds to database.
        @param Desc [in] Description of new category.
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
    class function Execute(AOwner: TComponent): Boolean;
      {Displays dialog box and adds category if user OKs.
        @param AOwner [in] Component that owns dialog box.
        @param CatList [in] List of categories available for deletion.
      }
  end;


implementation


uses
  // Project
  USnippets, UUniqueID;

{$R *.dfm}


{ TAddCategoryDlg }

procedure TAddCategoryDlg.AddCategory(const Desc: string);
  {Creates new category and adds to database.
    @param Desc [in] Description of new category.
  }
var
  Data: TCategoryData;  // category properties
begin
  Data := (Snippets as ISnippetsEdit).GetEditableCategoryInfo;
  Data.Desc := Desc;
  // add category with a unique id string as name (name must be unique and is
  // for internal use only)
  (Snippets as ISnippetsEdit).AddCategory(TUniqueID.Generate, Data);
end;

procedure TAddCategoryDlg.ArrangeForm;
  {Sizes and arranges frame in dialog box.
  }
begin
  frmDescription.ArrangeFrame;
  inherited;
end;

procedure TAddCategoryDlg.btnOKClick(Sender: TObject);
  {OK button click event handler. Creates and adds new category before closing
  dialog box.
    @param Sender [in] Not used.
  }
begin
  inherited;
  AddCategory(frmDescription.Description);
end;

procedure TAddCategoryDlg.ConfigForm;
  {Configures form. Populates controls and supplies event handler to frame.
  }
resourcestring
  // Prompt text for frame
  sPrompt = 'Enter a category description:';
begin
  inherited;
  frmDescription.Prompt := sPrompt;
  frmDescription.OnChange := DescriptionChangeHandler;
end;

procedure TAddCategoryDlg.DescriptionChangeHandler(Sender: TObject);
  {Handles description frame's change event. Updates state of OK button
  according to entered description.
    @param Sender [in] Not used.
  }
begin
  UpdateOKBtn;
end;

class function TAddCategoryDlg.Execute(AOwner: TComponent): Boolean;
  {Displays dialog box and adds category if user OKs.
    @param AOwner [in] Component that owns dialog box.
    @param CatList [in] List of categories available for deletion.
  }
begin
  with InternalCreate(AOwner) do
    try
      Result := ShowModal = mrOK;
    finally
      Free;
    end;
end;

procedure TAddCategoryDlg.UpdateOKBtn;
  {Updates state of OK button depending on if user has entered valid data in
  dialog box.
  }
begin
  btnOK.Enabled := frmDescription.IsValidEntry;
end;

end.

