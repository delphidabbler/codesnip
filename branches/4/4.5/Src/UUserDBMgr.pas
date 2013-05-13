{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a static class that manages user's interaction with user database.
}


unit UUserDBMgr;


interface


uses
  // Project
  DB.UCategory, UBaseObjects, UView;


type

  ///  <summary>Static class that manages user's interaction with user database
  ///  and performs move and backup operations on it.</summary>
  TUserDBMgr = class(TNoConstructObject)
  strict private
    ///  <summary>OnClose event handler for open dialogue box. Checks if
    ///  dialogue box can close.</summary>
    ///  <param name="Sender">TObject [in] Reference to dialogue box in
    ///  question.</param>
    ///  <param name="CanClose">Boolean [in/out] Set to True to permit dialogue
    ///  to close or False to inhibit closure.</param>
    class procedure CanOpenDialogClose(Sender: TObject; var CanClose: Boolean);
    ///  <summary>OnClose event handler for save dialogue box. Checks if
    ///  dialogue box can close.</summary>
    ///  <param name="Sender">TObject [in] Reference to dialogue box in
    ///  question.</param>
    ///  <param name="CanClose">Boolean [in/out] Set to True to permit dialogue
    ///  to close or False to inhibit closure.</param>
    class procedure CanSaveDialogClose(Sender: TObject; var CanClose: Boolean);
    ///  <summary>Creates a list of user defined categories.</summary>
    ///  <param name="IncludeSpecial">Boolean [in] Flag indicating whether list
    ///  should include special, non-deletable, categories.</param>
    ///  <returns>Required category list.</returns>
    ///  <remarks>Caller must free the returned object.</remarks>
    class function CreateUserCatList(
      const IncludeSpecial: Boolean): TCategoryList;
  public
    ///  <summary>Enables user to adds a new user defined snippet to the
    ///  database using the snippets editor.</summary>
    class procedure AddSnippet;
    ///  <summary>Enables user to edit the snippet with the given name using the
    ///  snippets editor.</summary>
    ///  <remarks>The named snippet must be user defined.</remarks>
    class procedure EditSnippet(const SnippetName: string);
    ///  <summary>Duplicates the snippet specified by the given view as a user
    ///  defined snippet with name specified by user.</summary>
    class procedure DuplicateSnippet(ViewItem: IView);
    ///  <summary>Deletes the snippet specified by the given view from the
    ///  database.</summary>
    ///  <remarks>The given snippet must be user defined.</remarks>
    class procedure DeleteSnippet(ViewItem: IView);
    ///  <summary>Checks if given view item can be duplicated.</summary>
    ///  <remarks>To be duplicated view must be a snippet.</summary>
    class function CanDuplicate(ViewItem: IView): Boolean;
    ///  <summary>Checks if the given view item specifies an editable snippet.
    ///  </summary>
    class function CanEdit(ViewItem: IView): Boolean;
    ///  <summary>Adds a new category, specified by the user, to the database.
    ///  </summary>
    class procedure AddCategory;
    ///  <summary>Deletes a user specified category from the database.</summary>
    class procedure DeleteACategory;
    ///  <summary>Renames a user specified category.</summary>
    class procedure RenameACategory;
    ///  <summary>Checks if it is possible for any categories to be renamed.
    ///  </summary>
    class function CanRenameACategory: Boolean;
    ///  <summary>Checks if it is possible for any categories to be deleted.
    ///  </summary>
    class function CanDeleteACategory: Boolean;
    ///  <summary>Saves the current user database to disk.</summary>
    class procedure Save;
    ///  <summary>Checks if the user database can be saved.</summary>
    class function CanSave: Boolean;
    ///  <summary>Creates a backup of the user database in a file specified by
    ///  the user.</summary>
    class procedure BackupDatabase;
    ///  <summary>Restores the user database from a backup file specified by the
    ///  user.</summary>
    class function RestoreDatabase: Boolean;
    ///  <summary>Moves the user database to a new location specified by the
    ///  user.</summary>
    class procedure MoveDatabase;
  end;


implementation


uses
  // Delphi
  SysUtils, Dialogs, Windows {for inlining},
  // Project
  DB.UMain, DB.USnippet,
  FmAddCategoryDlg, FmDeleteCategoryDlg, FmDuplicateSnippetDlg,
  FmRenameCategoryDlg, FmSnippetsEditorDlg,
  {$IFNDEF PORTABLE}
  FmUserDataPathDlg,
  {$ENDIF}
  UConsts, UExceptions, UIStringList, UMessageBox, UOpenDialogEx,
  UOpenDialogHelper, UReservedCategories, USaveDialogEx, USnippetIDs,
  UUserDBBackup;


{ TUserDBMgr }

class procedure TUserDBMgr.AddCategory;
begin
  // all work takes place in dialog box
  TAddCategoryDlg.Execute(nil);
end;

class procedure TUserDBMgr.AddSnippet;
begin
  // Display Add Snippet dialog box which performs update of database.
  TSnippetsEditorDlg.AddNewSnippet(nil);
end;

class procedure TUserDBMgr.BackupDatabase;
var
  FileName: string;             // name of backup file
  SaveDlg: TSaveDialogEx;       // save dialog box used to name backup file
  UserDBBackup: TUserDBBackup;  // object used to perform backup
resourcestring
  // Dialog box caption
  sCaption = 'Save Backup';
begin
  // Get backup file name from user via standard save dialog box
  SaveDlg := TSaveDialogEx.Create(nil);
  try
    SaveDlg.OnCanClose := CanSaveDialogClose;
    SaveDlg.Title := sCaption;
    SaveDlg.Options := [ofShowHelp, ofExtensionDifferent, ofPathMustExist,
      ofNoTestFileCreate, ofEnableSizing];
    SaveDlg.HelpKeyword := 'SaveBackupDlg';
    if SaveDlg.Execute then
    begin
      // Perform backup
      FileName := SaveDlg.FileName;
      UserDBBackup := TUserDBBackup.Create(FileName);
      try
        UserDBBackup.Backup;
      finally
        UserDBBackup.Free;
      end;
    end;
  finally
    SaveDlg.Free;
  end;
end;

class function TUserDBMgr.CanDeleteACategory: Boolean;
var
  CatList: TCategoryList; // list of user deletable categories
begin
  CatList := CreateUserCatList(False);  // builds list of deletable user cats
  try
    Result := CatList.Count > 0;
  finally
    CatList.Free;
  end;
end;

class function TUserDBMgr.CanDuplicate(ViewItem: IView): Boolean;
begin
  Result := Supports(ViewItem, ISnippetView);
end;

class function TUserDBMgr.CanEdit(ViewItem: IView): Boolean;
var
  SnippetView: ISnippetView;  // ViewItem as snippet view if supported
begin
  Assert(Assigned(ViewItem), ClassName + '.CanEdit: ViewItem is nil');
  Result := Assigned(ViewItem)
    and Supports(ViewItem, ISnippetView, SnippetView)
    and SnippetView.Snippet.UserDefined;
end;

class procedure TUserDBMgr.CanOpenDialogClose(Sender: TObject;
  var CanClose: Boolean);
var
  FileName: string;     // name of file entered in dialog box
resourcestring
  // Error message
  sFileDoesNotExist = '"%s" does not exist.';
begin
  CanClose := False;
  FileName := FileOpenEditedFileName(Sender as TOpenDialogEx);
  if not FileExists(FileName) then
  begin
    // Specified file doesn't exist: inhibit closure
    TMessageBox.Error(
      Sender as TOpenDialogEx,
      Format(sFileDoesNotExist, [FileName])
    );
    Exit;
  end;
  // All OK: allow closure
  CanClose := True;
end;

class function TUserDBMgr.CanRenameACategory: Boolean;
var
  CatList: TCategoryList; // list of user renamable categories
begin
  CatList := CreateUserCatList(True); // build list of all user categories
  try
    Result := CatList.Count > 0;
  finally
    CatList.Free;
  end;
end;

class function TUserDBMgr.CanSave: Boolean;
begin
  // We can save database if it's been changed
  Result := (Database as IDatabaseEdit).Updated;
end;

class procedure TUserDBMgr.CanSaveDialogClose(Sender: TObject;
  var CanClose: Boolean);
var
  FileName: string;   // name of file entered in dialog box
resourcestring
  // Error message
  sOverwritePrompt = '"%s" already exists. OK to overwrite?';
begin
  FileName := FileOpenEditedFileName(Sender as TSaveDialogEx);
  if FileExists(FileName) then
    // File exists: allow closure if user permits file to be overwritten
    CanClose := TMessageBox.Confirm(
      Sender as TSaveDialogEx,
      Format(sOverwritePrompt, [FileName])
    );
end;

class function TUserDBMgr.CreateUserCatList(
  const IncludeSpecial: Boolean): TCategoryList;
var
  Cat: TCategory; // references each category in snippets database
begin
  Result := TCategoryList.Create;
  for Cat in Database.Categories do
    if Cat.UserDefined and
      (IncludeSpecial or not TReservedCategories.IsReserved(Cat)) then
      Result.Add(Cat);
end;

class procedure TUserDBMgr.DeleteACategory;
var
  CatList: TCategoryList; // list of deletable categories
begin
  CatList := CreateUserCatList(False);
  try
    // all work takes place in dialog box
    TDeleteCategoryDlg.Execute(nil, CatList)
  finally
    CatList.Free;
  end;
end;

class procedure TUserDBMgr.DeleteSnippet(ViewItem: IView);

  // Builds a list of snippet names from a given snippet ID list.
  function SnippetNames(const IDList: ISnippetIDList): IStringList;
  var
    ID: TSnippetID;     // loops through all IDs in list
    Snippet: TSnippet;  // snippet corresponding to ID
  begin
    Result := TIStringList.Create;
    for ID in IDList do
    begin
      Snippet := Database.Snippets.Find(ID);
      Result.Add(Snippet.DisplayName);
    end;
  end;

var
  Dependents: ISnippetIDList; // list of dependent snippet IDs
  Referrers: ISnippetIDList;  // list referring snippet IDs
  ConfirmMsg: string;         // message displayed to confirm deletion
  Snippet: TSnippet;          // snippet being deleted
resourcestring
  // Prompts & error messages
  sConfirmDelete = 'Please confirm you wish to delete %s';
  sConfirmDeleteEx = 'Please confirm you wish to delete %0:s.' + EOL2
    + 'Deleting this snippet will also remove it from the cross reference list '
    + 'of the following snippets:' + EOL + '    %1:s';
  sHasDependents = 'Sorry, this snippet can''t be deleted. It is required by '
    + 'the following snippets:' + EOL + '    %s';
begin
  Assert(Supports(ViewItem, ISnippetView),
    ClassName + '.Delete: Current view is not a snippet');
  Snippet := (ViewItem as ISnippetView).Snippet;
  Assert(Snippet.UserDefined,
    ClassName + '.Delete: Snippet must be user defined');
  // Check if snippet has dependents: don't allow deletion if so
  Dependents := (Database as IDatabaseEdit).GetDependents(Snippet);
  if Dependents.Count > 0 then
  begin
    TMessageBox.Error(
      nil,
      Format(
        sHasDependents,
        [SnippetNames(Dependents).GetText(',' + EOL + '    ', False)]
      )
    );
    Exit;
  end;
  // Get permission to delete. If snippet has dependents list them in prompt
  Referrers := (Database as IDatabaseEdit).GetReferrers(Snippet);
  if Referrers.Count = 0 then
    ConfirmMsg := Format(sConfirmDelete, [Snippet.DisplayName])
  else
    ConfirmMsg := Format(
      sConfirmDeleteEx,
      [
        Snippet.DisplayName,
        SnippetNames(Referrers).GetText(',' + EOL + '    ', False)
      ]
    );
  if TMessageBox.Confirm(nil, ConfirmMsg) then
    (Database as IDatabaseEdit).DeleteSnippet(Snippet);
end;

class procedure TUserDBMgr.DuplicateSnippet(ViewItem: IView);
begin
  Assert(CanDuplicate(ViewItem),
    ClassName + '.DuplicateSnippet: ViewItem can''t be duplicated');
  TDuplicateSnippetDlg.Execute(nil, (ViewItem as ISnippetView).Snippet);
end;

class procedure TUserDBMgr.EditSnippet(const SnippetName: string);
var
  Snippet: TSnippet;    // reference to snippet to be edited
begin
  Snippet := Database.Snippets.Find(SnippetName, True);
  if not Assigned(Snippet) then
    raise EBug.Create(ClassName + '.EditSnippet: Snippet not in user database');
  TSnippetsEditorDlg.EditSnippet(nil, Snippet);
end;

class procedure TUserDBMgr.MoveDatabase;
begin
  // This dialogue box not available in portable edition
  {$IFNDEF PORTABLE}
  TUserDataPathDlg.Execute(nil);
  {$ENDIF}
end;

class procedure TUserDBMgr.RenameACategory;
var
  CatList: TCategoryList; // list of user defined categories
begin
  CatList := CreateUserCatList(True);
  try
    // all work takes place in dialog box
    TRenameCategoryDlg.Execute(nil, CatList)
  finally
    CatList.Free;
  end;
end;

class function TUserDBMgr.RestoreDatabase: Boolean;
var
  FileName: string;               // name of backup file
  Dlg: TOpenDialogEx;             // open dialog box used to select backup file
  UserDBBackup: TUserDBBackup;    // object used to perform restoration
resourcestring
  sCaption = 'Open Backup File';  // dialog box caption
begin
  // Get name of backup file from user via standard open dialog box
  Dlg := TOpenDialogEx.Create(nil);
  try
    Dlg.OnCanClose := CanOpenDialogClose;
    Dlg.Title := sCaption;
    Dlg.Options := [ofShowHelp, ofPathMustExist, ofHideReadOnly,
      ofEnableSizing];
    Dlg.HelpKeyword := 'RestoreBackupDlg';
    Result := Dlg.Execute;
    if Result then
    begin
      // Perform restoration
      FileName := Dlg.FileName;
      UserDBBackup := TUserDBBackup.Create(FileName);
      try
        UserDBBackup.Restore;
      finally
        UserDBBackup.Free;
      end;
    end;
  finally
    Dlg.Free;
  end;
end;

class procedure TUserDBMgr.Save;
begin
  (Database as IDatabaseEdit).Save;
end;

end.

