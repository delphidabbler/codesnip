{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2022, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a static class that manages user's interaction with the database.
}


unit UUserDBMgr;


interface

{TODO -cRefactoring: Rename this unit/classes/methods: the names refer to the
        CodeSnip 4 database structure but the code now works with vaults}

uses
  // Delphi
  Classes,
  // Project
  DB.UCategory,
  UBaseObjects,
  USnippetIDs,
  UView;


type

  ///  <summary>Static class that manages user's interaction with user database
  ///  and performs move and backup operations on it.</summary>
  TUserDBMgr = class(TNoConstructObject)
  public
    ///  <summary>Enables user to adds a new user defined snippet to the
    ///  database using the snippets editor.</summary>
    class procedure AddSnippet;
    ///  <summary>Enables user to edit the snippet with the given ID using the
    ///  snippets editor.</summary>
    class procedure EditSnippet(const ASnippetID: TSnippetID);
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
    ///  <summary>Deletes all the snippets in a collection specified by the
    ///  user.</summary>
    ///  <returns><c>Boolean</c>. <c>True</c> if the collection's data was
    ///  deleted, <c>False</c> otherwise.</returns>
    class function DeleteDatabase: Boolean;
  end;


implementation


uses
  // Delphi
  SysUtils,
  Dialogs,
  Windows {for inlining},
  IOUtils,
  // Project
  DB.UMain,
  DB.USnippet,
  DB.Vaults,
  FmAddCategoryDlg,
  UI.Forms.BackupVaultDlg,
  FmDeleteCategoryDlg,
  UI.Forms.DeleteVaultDlg,
  FmDuplicateSnippetDlg,
  FmRenameCategoryDlg,
  FmSnippetsEditorDlg,
  {$IFNDEF PORTABLE}
  UI.Forms.MoveVaultDlg,
  {$ENDIF}
  FmWaitDlg,
  UAppInfo,
  UConsts,
  UExceptions,
  UIStringList,
  UMessageBox,
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

          fCollection: TVault;
      strict protected
        ///  <summary>Restores the user database from a backup.</summary>
        procedure Execute; override;
      public
        ///  <summary>Constructs a new, suspended, thread that can restore the
        ///  given vault from the given backup file.</summary>
        constructor Create(const BakFileName: string;
          const ACollection: TVault);
      end;
  public
    ///  <summary>Performs the restoration of a vault from a background thread
    ///  and displays a wait diaogue box if the operation takes more than a
    ///  given time to execute. Blocks until the thread terminates.</summary>
    ///  <param name="AOwner">TComponent [in] Component that owns the dialogue
    ///  box, over which it is aligned.</param>
    ///  <param name="BakFileName">string [in] Name of backup file to be
    ///  restored.</param>
    ///  <param name="ACollection"><c>TVault</c> Vault being restored.</param>
    class procedure Execute(AOwner: TComponent; const BakFileName: string;
      const ACollection: TVault);
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

          fCollection: TVault;
      strict protected
        ///  <summary>Backs up the user database.</summary>
        procedure Execute; override;
      public
        ///  <summary>Constructs a new, suspended, thread that can backup the
        ///  given vault to the given backup file.</summary>
        constructor Create(const BakFileName: string;
          const ACollection: TVault);
      end;
  public
    ///  <summary>Performs a vault backup operation in a background thread and
    ///  displays a wait diaogue box if the operation takes more than a given
    ///  time to execute. Blocks until the thread terminates.</summary>
    ///  <param name="AOwner">TComponent [in] Component that owns the dialogue
    ///  box, over which it is aligned.</param>
    ///  <param name="BakFileName">string [in] Name of backup file to be
    ///  created.</param>
    ///  <param name="ACollection"><c>TVault</c> Vault being backed up.</param>
    class procedure Execute(AOwner: TComponent; const BakFileName: string;
      const ACollection: TVault);
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
  FileName: string;
  Collection: TVault;
resourcestring
  sOverwritePrompt = '"%s" already exists. OK to overwrite?';
begin
  if TVaultBackupDlg.Execute(ParentCtrl, FileName, Collection) then
  begin
    if TFile.Exists(FileName)
      and not TMessageBox.Confirm(
        ParentCtrl, Format(sOverwritePrompt, [FileName])
      ) then
      Exit;
    TUserDBBackupUI.Execute(ParentCtrl, FileName, Collection);
  end;
end;

class function TUserDBMgr.CanDeleteACategory: Boolean;
var
  Cat: TCategory;
begin
  Result := False;
  for Cat in Database.Categories do
    if Cat.CanDelete then
      Exit(True);
end;

class function TUserDBMgr.CanDuplicate(ViewItem: IView): Boolean;
begin
  Result := Supports(ViewItem, ISnippetView);
end;

class function TUserDBMgr.CanEdit(ViewItem: IView): Boolean;
begin
  Assert(Assigned(ViewItem), ClassName + '.CanEdit: ViewItem is nil');
  Result := Supports(ViewItem, ISnippetView);
end;

class function TUserDBMgr.CanRenameACategory: Boolean;
begin
  Result := True;
end;

class function TUserDBMgr.CanSave: Boolean;
begin
  // We can save database if it's been changed
  Result := (Database as IDatabaseEdit).Updated;
end;

class procedure TUserDBMgr.DeleteACategory;
var
  CatList: TCategoryList; // list of deletable categories
  Cat: TCategory;
begin
  CatList := TCategoryList.Create;
  try
    for Cat in Database.Categories do
      if Cat.CanDelete then
        CatList.Add(Cat);
    // all work takes place in dialog box
    TDeleteCategoryDlg.Execute(nil, CatList)
  finally
    CatList.Free;
  end;
end;

class function TUserDBMgr.DeleteDatabase: Boolean;
var
  CollectionToDelete: TVault;
begin
  if not TDeleteVaultDlg.Execute(nil, CollectionToDelete) then
    Exit(False);
  if not TDirectory.Exists(CollectionToDelete.Storage.Directory) then
    Exit(False);
  TDirectory.Delete(CollectionToDelete.Storage.Directory, True);
  Result := True;
end;

class procedure TUserDBMgr.DeleteSnippet(ViewItem: IView);

  {TODO -cVault: rename following inner method to SnippetDisplayNames for
          clarity}
  // Builds a list of snippet display names from a given snippet ID list.
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

class procedure TUserDBMgr.EditSnippet(const ASnippetID: TSnippetID);
var
  Snippet: TSnippet;    // reference to snippet to be edited
begin
  Snippet := Database.Snippets.Find(ASnippetID);
  if not Assigned(Snippet) then
    raise EBug.Create(ClassName + '.EditSnippet: Snippet not found');
  TSnippetsEditorDlg.EditSnippet(nil, Snippet);
end;

class procedure TUserDBMgr.MoveDatabase;
begin
  // This dialogue box not available in portable edition
  {$IFNDEF PORTABLE}
  TMoveVaultDlg.Execute(nil);
  {$ENDIF}
end;

class procedure TUserDBMgr.RenameACategory;
var
  Cat: TCategory;
  CatList: TCategoryList; // list of user defined categories
begin
  CatList := TCategoryList.Create;
  try
    for Cat in Database.Categories do
      CatList.Add(Cat);
    // all work takes place in dialog box
    TRenameCategoryDlg.Execute(nil, CatList)
  finally
    CatList.Free;
  end;
end;

class function TUserDBMgr.RestoreDatabase(ParentCtrl: TComponent): Boolean;
var
  FileName: string;
  Collection: TVault;
resourcestring
  sFileDoesNotExist = '"%s" does not exist.';
begin
  Result := TVaultBackupDlg.Execute(ParentCtrl, FileName, Collection);
  if Result then
  begin
    if not TFile.Exists(FileName) then
    begin
      // Specified file doesn't exist: inhibit closure
      TMessageBox.Error(
        ParentCtrl, Format(sFileDoesNotExist, [FileName])
      );
      Exit;
    end;
    TUserDBRestoreUI.Execute(ParentCtrl, FileName, Collection);
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
  const BakFileName: string; const ACollection: TVault);
resourcestring
  // Caption for wait dialog
  sWaitCaption = 'Restoring database files...';
var
  Thread: TRestoreThread;   // thread that performs restore operation
begin
  Thread := TRestoreThread.Create(BakFileName, ACollection);
  try
    RunThreadWithWaitDlg(Thread, AOwner, sWaitCaption);
  finally
    Thread.Free;
  end;
end;

{ TUserDBRestoreUI.TRestoreThread }

constructor TUserDBRestoreUI.TRestoreThread.Create(const BakFileName: string;
  const ACollection: TVault);
begin
  inherited Create(True);
  fBakFileName := BakFileName;
  fCollection := ACollection;
end;

procedure TUserDBRestoreUI.TRestoreThread.Execute;
var
  UserDBBackup: TVaultBackup;
begin
  UserDBBackup := TVaultBackup.Create(fBakFileName, fCollection);
  try
    UserDBBackup.Restore;
  finally
    UserDBBackup.Free;
  end;
end;

{ TUserDBBackupUI }

class procedure TUserDBBackupUI.Execute(AOwner: TComponent;
  const BakFileName: string; const ACollection: TVault);
resourcestring
  // Caption for wait dialog
  sWaitCaption = 'Backing up database...';
var
  Thread: TBackupThread;   // thread that performs restore operation
begin
  Thread := TBackupThread.Create(BakFileName, ACollection);
  try
    RunThreadWithWaitDlg(Thread, AOwner, sWaitCaption);
  finally
    Thread.Free;
  end;
end;

{ TUserDBBackupUI.TBackupThread }

constructor TUserDBBackupUI.TBackupThread.Create(const BakFileName: string;
  const ACollection: TVault);
begin
  inherited Create(True);
  fBakFileName := BakFileName;
  fCollection := ACollection;
end;

procedure TUserDBBackupUI.TBackupThread.Execute;
var
  UserDBBackup: TVaultBackup;
resourcestring
  // Dialog box caption
  sCaption = 'Save Backup';
begin
  UserDBBackup := TVaultBackup.Create(fBakFileName, fCollection);
  try
    UserDBBackup.Backup;
  finally
    UserDBBackup.Free;
  end;
end;

end.

