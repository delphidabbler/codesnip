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
  // Delphi
  Classes,
  // Project
  CS.Database.Types,
  DB.UCategory,
  UBaseObjects,
  UView;


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
    class procedure EditSnippet(const SnippetID: TSnippetID);
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
    class procedure Save(ParentCtrl: TComponent);
    ///  <summary>Checks if the user database can be saved.</summary>
    class function CanSave: Boolean;
    ///  <summary>Creates a backup of the user database in a file specified by
    ///  the user.</summary>
    class procedure BackupDatabase(ParentCtrl: TComponent);
    ///  <summary>Restores the user database from a backup file specified by the
    ///  user.</summary>
    class function RestoreDatabase(ParentCtrl: TComponent): Boolean;
    ///  <summary>Moves the user database to a new location specified by the
    ///  user.</summary>
    class procedure MoveDatabase;
  end;


implementation


uses
  // Delphi
  SysUtils,
  Dialogs,
  Windows {for inlining},
  // Project
  CS.Init.CommandLineOpts,
  DB.UMain,
  DB.USnippet,
  FmAddCategoryDlg,
  FmDeleteCategoryDlg,
  FmDuplicateSnippetDlg,
  FmRenameCategoryDlg,
  FmSnippetsEditorDlg,
  FmUserDataPathDlg,
  FmWaitDlg,
  UConsts,
  UExceptions,
  UIStringList,
  UMessageBox,
  UOpenDialogEx,
  UOpenDialogHelper,
  UReservedCategories,
  USaveDialogEx,
  UUserDBBackup,
  UWaitForThreadUI;

type
  ///  <summary>Base class for classes that execute a user database management
  ///  function in a thread while displaying a "wait" dialogue box if necessary.
  ///  </summary>
  ///  <remarks>The class is marked abstract because it cannot be used directly.
  ///  </remarks>
  TUserDBWaitUI = class abstract(TNoConstructObject)
  strict private
    const
      ///  <summary>Time to elapse before wait dialogue is displayed.</summary>
      PauseBeforeDisplay = 500;
      ///  <summary>Minimum time to display wait dialogue box.</summary>
      MinDisplayTime = 1000;
  strict protected
    ///  <summary>Runs a thread and displays a wait dialogue box if the
    ///  thread takes more that a given time to execute. Blocks until the
    ///  thread terminates.</summary>
    ///  <param name="Thread">TThread [in] Thread to execute.</param>
    ///  <param name="DlgOwner">TComponent [in] Component that owns the dialogue
    ///  box, over which it is aligned.</param>
    ///  <param name="WaitCaption">string [in] Caption to be displayed in wait
    ///  dialogue box.</param>
    class procedure RunThreadWithWaitDlg(const Thread: TThread;
      const DlgOwner: TComponent; const WaitCaption: string);
  end;

type
  ///  <summary>Class that saves user database to disk in a thread while
  ///  displaying a "wait" dialogue box if necessary.</summary>
  TUserDBSaveUI = class sealed(TUserDBWaitUI)
  strict private
    type
      ///  <summary>Thread that performs user database save operation.</summary>
      TSaveThread = class(TThread)
      strict protected
        ///  <summary>Saves the user database.</summary>
        procedure Execute; override;
      public
        ///  <summary>Constructs a new, suspended, thread instance.</summary>
        constructor Create;
      end;
  public
    ///  <summary>Performs the user database save operation in a background
    ///  thread and displays a wait diaogue box if the operation takes more than
    ///  a given time to execute. Blocks until the thread terminates.</summary>
    ///  <param name="AOwner">TComponent [in] Component that owns the dialogue
    ///  box, over which it is aligned.</param>
    class procedure Execute(AOwner: TComponent);
  end;

type
  ///  <summary>Class that restores a backup of the user database in a thread
  ///  while displaying a "wait" dialogue box if necessary.</summary>
  TUserDBRestoreUI = class sealed(TUserDBWaitUI)
  strict private
    type
      ///  <summary>Thread that performs restore operation.</summary>
      TRestoreThread = class(TThread)
      strict private
        var
          ///  <summary>Name of backup file to be restored.</summary>
          fBakFileName: string;
      strict protected
        ///  <summary>Restores the user database from a backup.</summary>
        procedure Execute; override;
      public
        ///  <summary>Constructs a new, suspended, thread that can restore the
        ///  database from the given backup file.</summary>
        constructor Create(const BakFileName: string);
      end;
  public
    ///  <summary>Performs a user database restoration operation from in a
    ///  background thread and displays a wait diaogue box if the operation
    ///  takes more than a given time to execute. Blocks until the thread
    ///  terminates.</summary>
    ///  <param name="AOwner">TComponent [in] Component that owns the dialogue
    ///  box, over which it is aligned.</param>
    ///  <param name="BakFileName">string [in] Name of backup file to be
    ///  restored.</param>
    class procedure Execute(AOwner: TComponent; const BakFileName: string);
  end;

type
  ///  <summary>Class that creates a backup of the user database in a thread
  ///  while displaying a "wait" dialogue box if necessary.</summary>
  TUserDBBackupUI = class sealed(TUserDBWaitUI)
  strict private
    type
      ///  <summary>Thread that performs backup operation.</summary>
      TBackupThread = class(TThread)
      strict private
        var
          ///  <summary>Name of backup file to be created.</summary>
          fBakFileName: string;
      strict protected
        ///  <summary>Backs up the user database.</summary>
        procedure Execute; override;
      public
        ///  <summary>Constructs a new, suspended, thread that can backup the
        ///  database to the given backup file.</summary>
        constructor Create(const BakFileName: string);
      end;
  public
    ///  <summary>Performs a user database backup operation from in a background
    ///  thread and displays a wait diaogue box if the operation takes more than
    ///  a given time to execute. Blocks until the thread terminates.</summary>
    ///  <param name="AOwner">TComponent [in] Component that owns the dialogue
    ///  box, over which it is aligned.</param>
    ///  <param name="BakFileName">string [in] Name of backup file to be
    ///  created.</param>
    class procedure Execute(AOwner: TComponent; const BakFileName: string);
  end;

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

class procedure TUserDBMgr.BackupDatabase(ParentCtrl: TComponent);
var
  SaveDlg: TSaveDialogEx;       // save dialog box used to name backup file
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
      // Perform backup
      TUserDBBackupUI.Execute(ParentCtrl, SaveDlg.FileName);
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
    and Supports(ViewItem, ISnippetView, SnippetView);
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
      Snippet := Database.Lookup(ID);
      Result.Add(Snippet.Title);
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
    ConfirmMsg := Format(sConfirmDelete, [Snippet.Title])
  else
    ConfirmMsg := Format(
      sConfirmDeleteEx,
      [
        Snippet.Title,
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

class procedure TUserDBMgr.EditSnippet(const SnippetID: TSnippetID);
var
  Snippet: TSnippet;    // reference to snippet to be edited
begin
  if not Database.TryLookup(SnippetID, Snippet) then
    raise EBug.Create(ClassName + '.EditSnippet: Snippet not in database');
  TSnippetsEditorDlg.EditSnippet(nil, Snippet);
end;

class procedure TUserDBMgr.MoveDatabase;
begin
  // This dialogue box not available in portable mode
  if not TCommandLineOpts.IsPortable then
    TUserDataPathDlg.Execute(nil);
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

class function TUserDBMgr.RestoreDatabase(ParentCtrl: TComponent): Boolean;
var
  Dlg: TOpenDialogEx;             // open dialog box used to select backup file
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
      // Perform restoration
      TUserDBRestoreUI.Execute(ParentCtrl, Dlg.FileName);
  finally
    Dlg.Free;
  end;
end;

class procedure TUserDBMgr.Save(ParentCtrl: TComponent);
begin
  TUserDBSaveUI.Execute(ParentCtrl);
end;

{ TUserDBWaitUI }

class procedure TUserDBWaitUI.RunThreadWithWaitDlg(const Thread: TThread;
  const DlgOwner: TComponent; const WaitCaption: string);
begin
  try
    TWaitForThreadUI.Run( // this blocks until thread completes
      Thread,
      TWaitDlg.CreateAutoFree(DlgOwner, WaitCaption),
      PauseBeforeDisplay,
      MinDisplayTime
    );
  except
    raise TExceptionHelper.Clone(ExceptObject as Exception);
  end;
end;

{ TUserDBSaveUI }

class procedure TUserDBSaveUI.Execute(AOwner: TComponent);
resourcestring
  // Caption for wait dialog
  sWaitCaption = 'Saving user database...';
var
  Thread: TSaveThread;   // thread that performs save operation
begin
  Thread := TSaveThread.Create;
  try
    RunThreadWithWaitDlg(Thread, AOwner, sWaitCaption);
  finally
    Thread.Free;
  end;
end;

{ TUserDBSaveUI.TSaveThread }

constructor TUserDBSaveUI.TSaveThread.Create;
begin
  inherited Create(True);
end;

procedure TUserDBSaveUI.TSaveThread.Execute;
begin
  (Database as IDatabaseEdit).Save;
end;

{ TUserDBRestoreUI }

class procedure TUserDBRestoreUI.Execute(AOwner: TComponent;
  const BakFileName: string);
resourcestring
  // Caption for wait dialog
  sWaitCaption = 'Restoring database files...';
var
  Thread: TRestoreThread;   // thread that performs restore operation
begin
  Thread := TRestoreThread.Create(BakFileName);
  try
    RunThreadWithWaitDlg(Thread, AOwner, sWaitCaption);
  finally
    Thread.Free;
  end;
end;

{ TUserDBRestoreUI.TRestoreThread }

constructor TUserDBRestoreUI.TRestoreThread.Create(const BakFileName: string);
begin
  inherited Create(True);
  fBakFileName := BakFileName;
end;

procedure TUserDBRestoreUI.TRestoreThread.Execute;
var
  UserDBBackup: TUserDBBackup;
begin
  UserDBBackup := TUserDBBackup.Create(fBakFileName);
  try
    UserDBBackup.Restore;
  finally
    UserDBBackup.Free;
  end;
end;

{ TUserDBBackupUI }

class procedure TUserDBBackupUI.Execute(AOwner: TComponent;
  const BakFileName: string);
resourcestring
  // Caption for wait dialog
  sWaitCaption = 'Backing up database...';
var
  Thread: TBackupThread;   // thread that performs restore operation
begin
  Thread := TBackupThread.Create(BakFileName);
  try
    RunThreadWithWaitDlg(Thread, AOwner, sWaitCaption);
  finally
    Thread.Free;
  end;
end;

{ TUserDBBackupUI.TBackupThread }

constructor TUserDBBackupUI.TBackupThread.Create(const BakFileName: string);
begin
  inherited Create(True);
  fBakFileName := BakFileName;
end;

procedure TUserDBBackupUI.TBackupThread.Execute;
var
  UserDBBackup: TUserDBBackup;  // object used to perform backup
resourcestring
  // Dialog box caption
  sCaption = 'Save Backup';
begin
  UserDBBackup := TUserDBBackup.Create(fBakFileName);
  try
    UserDBBackup.Backup;
  finally
    UserDBBackup.Free;
  end;
end;

end.

