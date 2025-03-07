{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that permits user to add a new user defined
 * category to the database.
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
  DB.Categories,
  DB.Main,
  UUniqueID;

{$R *.dfm}


{ TAddCategoryDlg }

procedure TAddCategoryDlg.AddCategory(const Desc: string);
  {Creates new category and adds to database.
    @param Desc [in] Description of new category.
  }
var
  Data: TCategoryData;  // category properties
begin
  Data := Database.GetEditableCategoryInfo;
  Data.Desc := Desc;
  // add category with a unique id string as name (name must be unique and is
  // for internal use only)
  Database.AddCategory(TUniqueID.Generate, Data);
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
  sPrompt = 'Enter a category &description:';
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
var
  Dlg: TAddCategoryDlg;
begin
  Dlg := InternalCreate(AOwner);
  try
    Result := Dlg.ShowModal = mrOK;
  finally
    Dlg.Free;
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

