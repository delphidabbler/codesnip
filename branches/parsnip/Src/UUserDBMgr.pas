{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a static class that manages user's interaction with the snippets
 * database.
}


unit UUserDBMgr;


{ TODO -cRenameUnit: Rename this unit to be based on main class name
        (CS.Controllers.DBModificationMgr ?) }


interface


uses
  // Delphi
  Classes,
  // Project
  CS.Database.Types,
  UBaseObjects,
  UView;


type
  ///  <summary>Static class that manages user's interaction with the snippets
  ///  database.</summary>
  TDBModificationMgr = class(TNoConstructObject)
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
  public
    ///  <summary>Enables user to adds a new snippet to the database using the
    ///  snippets editor.</summary>
    class procedure AddSnippet;
    ///  <summary>Enables user to edit the snippet with the given name using the
    ///  snippets editor.</summary>
    class procedure EditSnippet(const SnippetID: TSnippetID);
    ///  <summary>Duplicates the snippet specified by the given view as a new
    ///  snippet with the title defined by the user.</summary>
    class procedure DuplicateSnippet(ViewItem: IView);
    ///  <summary>Deletes the snippet specified by the given view from the
    ///  database.</summary>
    class procedure DeleteSnippet(ViewItem: IView);
    ///  <summary>Checks if given view item can be duplicated.</summary>
    ///  <remarks>To be duplicated view must be a snippet.</summary>
    class function CanDuplicate(ViewItem: IView): Boolean;
    ///  <summary>Checks if the given view item specifies an editable snippet.
    ///  </summary>
    class function CanEdit(ViewItem: IView): Boolean;
    { TODO: revise TRemoveTagAction to call the following method directly OR
            move the functionality into TRemoveTagAction itself. }
    ///  <summary>Removes given tag from tag list of snippet with given ID.
    ///  </summary>
    class procedure RemoveTagFromSnippet(const SnippetID: TSnippetID;
      const Tag: TTag);
    { TODO: Move following method into action that calls the method ?? }
    ///  <summary>Updates Starred property associated with given snippet ID to
    ///  the given state.</summary>
    class procedure UpdateSnippetStarredState(const SnippetID: TSnippetID;
      const NewState: Boolean);
    ///  <summary>Saves the current database to disk.</summary>
    class procedure Save(ParentCtrl: TComponent);
    ///  <summary>Checks if the database can be saved.</summary>
    class function CanSave: Boolean;
    ///  <summary>Creates a backup of the database in a file specified by the
    ///  user.</summary>
    class procedure BackupDatabase(ParentCtrl: TComponent);
    ///  <summary>Restores the database from a backup file specified by the
    ///  user.</summary>
    class function RestoreDatabase(ParentCtrl: TComponent): Boolean;
    ///  <summary>Moves the database to a new location specified by the user.
    ///  </summary>
    class procedure MoveDatabase;
  end;


implementation


uses
  // Delphi
  SysUtils,
  Dialogs,
  // Project
  CS.Init.CommandLineOpts,
  DB.UMain,
  FmDuplicateSnippetDlg,
  FmSnippetsEditorDlg,
  FmUserDataPathDlg,
  FmWaitDlg,
  UConsts,
  UExceptions,
  UIStringList,
  UMessageBox,
  UOpenDialogEx,
  UOpenDialogHelper,
  USaveDialogEx,
  UUserDBBackup,
  UWaitForThreadUI;

type
  ///  <summary>Base class for classes that execute a database management
  ///  function in a thread while displaying a "wait" dialogue box if necessary.
  ///  </summary>
  ///  <remarks>The class is marked abstract because it cannot be used directly.
  ///  </remarks>
  TDBWaitUI = class abstract(TNoConstructObject)
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
  ///  <summary>Class that saves database to disk in a thread while displaying a
  ///  "wait" dialogue box if necessary.</summary>
  TDBSaveUI = class sealed(TDBWaitUI)
  strict private
    type
      ///  <summary>Thread that performs database save operation.</summary>
      TSaveThread = class(TThread)
      strict protected
        ///  <summary>Saves the database.</summary>
        procedure Execute; override;
      public
        ///  <summary>Constructs a new, suspended, thread instance.</summary>
        constructor Create;
      end;
  public
    ///  <summary>Performs the database save operation in a background thread
    ///  and displays a wait diaogue box if the operation takes more than a
    ///  given time to execute. Blocks until the thread terminates.</summary>
    ///  <param name="AOwner">TComponent [in] Component that owns the dialogue
    ///  box, over which it is aligned.</param>
    class procedure Execute(AOwner: TComponent);
  end;

type
  ///  <summary>Class that restores a backup of the database in a thread while
  ///  displaying a "wait" dialogue box if necessary.</summary>
  TDBRestoreUI = class sealed(TDBWaitUI)
  strict private
    type
      ///  <summary>Thread that performs restore operation.</summary>
      TRestoreThread = class(TThread)
      strict private
        var
          ///  <summary>Name of backup file to be restored.</summary>
          fBakFileName: string;
      strict protected
        ///  <summary>Restores the database from a backup.</summary>
        procedure Execute; override;
      public
        ///  <summary>Constructs a new, suspended, thread that can restore the
        ///  database from the given backup file.</summary>
        constructor Create(const BakFileName: string);
      end;
  public
    ///  <summary>Performs a database restoration operation from in a background
    ///  thread and displays a wait diaogue box if the operation takes more than
    ///  a given time to execute. Blocks until the thread terminates.</summary>
    ///  <param name="AOwner">TComponent [in] Component that owns the dialogue
    ///  box, over which it is aligned.</param>
    ///  <param name="BakFileName">string [in] Name of backup file to be
    ///  restored.</param>
    class procedure Execute(AOwner: TComponent; const BakFileName: string);
  end;

type
  ///  <summary>Class that creates a backup of the database in a thread while
  ///  displaying a "wait" dialogue box if necessary.</summary>
  TDBBackupUI = class sealed(TDBWaitUI)
  strict private
    type
      ///  <summary>Thread that performs backup operation.</summary>
      TBackupThread = class(TThread)
      strict private
        var
          ///  <summary>Name of backup file to be created.</summary>
          fBakFileName: string;
      strict protected
        ///  <summary>Backs up the database.</summary>
        procedure Execute; override;
      public
        ///  <summary>Constructs a new, suspended, thread that can backup the
        ///  database to the given backup file.</summary>
        constructor Create(const BakFileName: string);
      end;
  public
    ///  <summary>Performs a database backup operation from in a background
    ///  thread and displays a wait diaogue box if the operation takes more than
    ///  a given time to execute. Blocks until the thread terminates.</summary>
    ///  <param name="AOwner">TComponent [in] Component that owns the dialogue
    ///  box, over which it is aligned.</param>
    ///  <param name="BakFileName">string [in] Name of backup file to be
    ///  created.</param>
    class procedure Execute(AOwner: TComponent; const BakFileName: string);
  end;

{ TDBModificationMgr }

class procedure TDBModificationMgr.AddSnippet;
begin
  // Display Add Snippet dialogue box which performs update of database.
  TSnippetsEditorDlg.AddNewSnippet(nil);
end;

class procedure TDBModificationMgr.BackupDatabase(ParentCtrl: TComponent);
var
  SaveDlg: TSaveDialogEx; // save dialogue box used to name backup file
resourcestring
  // Dialogue box caption
  sCaption = 'Save Backup';
begin
  // Get backup file name from user via standard save dialogue box
  SaveDlg := TSaveDialogEx.Create(nil);
  try
    SaveDlg.OnCanClose := CanSaveDialogClose;
    SaveDlg.Title := sCaption;
    SaveDlg.Options := [ofShowHelp, ofExtensionDifferent, ofPathMustExist,
      ofNoTestFileCreate, ofEnableSizing];
    SaveDlg.HelpKeyword := 'SaveBackupDlg';
    if SaveDlg.Execute then
      // Perform backup
      TDBBackupUI.Execute(ParentCtrl, SaveDlg.FileName);
  finally
    SaveDlg.Free;
  end;
end;

class function TDBModificationMgr.CanDuplicate(ViewItem: IView): Boolean;
begin
  Assert(Assigned(ViewItem), ClassName + '.CanDuplicate: ViewItem is nil');
  Result := Supports(ViewItem, ISnippetView);
end;

class function TDBModificationMgr.CanEdit(ViewItem: IView): Boolean;
begin
  Assert(Assigned(ViewItem), ClassName + '.CanEdit: ViewItem is nil');
  Result := Supports(ViewItem, ISnippetView);
end;

class procedure TDBModificationMgr.CanOpenDialogClose(Sender: TObject;
  var CanClose: Boolean);
var
  FileName: string;     // name of file entered in dialogue box
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

class function TDBModificationMgr.CanSave: Boolean;
begin
  // We can save database if it's been changed, i.e. it is dirty
  Result := Database.IsDirty
end;

class procedure TDBModificationMgr.CanSaveDialogClose(Sender: TObject;
  var CanClose: Boolean);
var
  FileName: string;   // name of file entered in dialogue box
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

class procedure TDBModificationMgr.DeleteSnippet(ViewItem: IView);

  // Builds a list of snippet titles from a given snippet ID list.
  function SnippetTitles(const IDList: ISnippetIDList): IStringList;
  var
    ID: TSnippetID;     // loops through all IDs in list
    Snippet: ISnippet;  // snippet corresponding to ID
  begin
    Result := TIStringList.Create;
    for ID in IDList do
    begin
      Snippet := Database.LookupSnippet(ID);
      Result.Add(Snippet.Title);
    end;
  end;

var
  Dependents: ISnippetIDList; // list of dependent snippet IDs
  Referrers: ISnippetIDList;  // list referring snippet IDs
  ConfirmMsg: string;         // message displayed to confirm deletion
  Snippet: ISnippet;          // snippet being deleted
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
  Snippet := Database.LookupSnippet((ViewItem as ISnippetView).SnippetID);
  // Check if snippet has dependents: don't allow deletion if so
  Dependents := Database.GetDependentsOf(Snippet.ID);
  if Dependents.Count > 0 then
  begin
    TMessageBox.Error(
      nil,
      Format(
        sHasDependents,
        [SnippetTitles(Dependents).GetText(',' + EOL + '    ', False)]
      )
    );
    Exit;
  end;
  // Get permission to delete. If snippet has dependents list them in prompt
  Referrers := Database.GetReferrersTo(Snippet.ID);
  if Referrers.Count = 0 then
    ConfirmMsg := Format(sConfirmDelete, [Snippet.Title])
  else
    ConfirmMsg := Format(
      sConfirmDeleteEx,
      [
        Snippet.Title,
        SnippetTitles(Referrers).GetText(',' + EOL + '    ', False)
      ]
    );
  if TMessageBox.Confirm(nil, ConfirmMsg) then
    Database.DeleteSnippet(Snippet.ID);
end;

class procedure TDBModificationMgr.DuplicateSnippet(ViewItem: IView);
begin
  Assert(CanDuplicate(ViewItem),
    ClassName + '.DuplicateSnippet: ViewItem can''t be duplicated');
  TDuplicateSnippetDlg.Execute(
    nil,
    Database.LookupSnippet((ViewItem as ISnippetView).SnippetID)
  );
end;

class procedure TDBModificationMgr.EditSnippet(const SnippetID: TSnippetID);
var
  Snippet: ISnippet;    // reference to snippet to be edited
begin
  if not Database.TryLookupSnippet(SnippetID, Snippet) then
    raise EBug.Create(ClassName + '.EditSnippet: Snippet not in database');
  TSnippetsEditorDlg.EditSnippet(nil, Snippet);
end;

class procedure TDBModificationMgr.MoveDatabase;
begin
  // This dialogue box not available in portable mode
  if not TCommandLineOpts.IsPortable then
    TDBMoveDlg.Execute(nil);
end;

class procedure TDBModificationMgr.RemoveTagFromSnippet(
  const SnippetID: TSnippetID; const Tag: TTag);
var
  Snippet: IEditableSnippet;
  Tags: ITagSet;
begin
  Snippet := Database.LookupEditableSnippet(SnippetID);
  Tags := Snippet.Tags;
  Tags.Remove(Tag);
  Snippet.Tags := Tags;
  Database.UpdateSnippet(Snippet);
end;

class function TDBModificationMgr.RestoreDatabase(
  ParentCtrl: TComponent): Boolean;
var
  Dlg: TOpenDialogEx; // open dialogue box used to select backup file
resourcestring
  sCaption = 'Open Backup File';  // dialogue box caption
begin
  // Get name of backup file from user via standard open dialogue box
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
      TDBRestoreUI.Execute(ParentCtrl, Dlg.FileName);
  finally
    Dlg.Free;
  end;
end;

class procedure TDBModificationMgr.Save(ParentCtrl: TComponent);
begin
  TDBSaveUI.Execute(ParentCtrl);
end;

class procedure TDBModificationMgr.UpdateSnippetStarredState(
  const SnippetID: TSnippetID; const NewState: Boolean);
var
  Snippet: IEditableSnippet;
begin
  Snippet := Database.LookupEditableSnippet(SnippetID);
  if NewState = Snippet.Starred then
    Exit;
  Snippet.Starred := NewState;
  Database.UpdateSnippet(Snippet);
end;

{ TDBWaitUI }

class procedure TDBWaitUI.RunThreadWithWaitDlg(const Thread: TThread;
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

{ TDBSaveUI }

class procedure TDBSaveUI.Execute(AOwner: TComponent);
resourcestring
  // Caption for wait dialogue
  sWaitCaption = 'Saving database...';
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

{ TDBSaveUI.TSaveThread }

constructor TDBSaveUI.TSaveThread.Create;
begin
  inherited Create(True);
end;

procedure TDBSaveUI.TSaveThread.Execute;
begin
  Database.Save;
end;

{ TDBRestoreUI }

class procedure TDBRestoreUI.Execute(AOwner: TComponent;
  const BakFileName: string);
resourcestring
  // Caption for wait dialogue
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

{ TDBRestoreUI.TRestoreThread }

constructor TDBRestoreUI.TRestoreThread.Create(const BakFileName: string);
begin
  inherited Create(True);
  fBakFileName := BakFileName;
end;

procedure TDBRestoreUI.TRestoreThread.Execute;
var
  DBBackup: TDatabaseBackup;
begin
  DBBackup := TDatabaseBackup.Create(fBakFileName);
  try
    DBBackup.Restore(True);
  finally
    DBBackup.Free;
  end;
end;

{ TDBBackupUI }

class procedure TDBBackupUI.Execute(AOwner: TComponent;
  const BakFileName: string);
resourcestring
  // Caption for wait dialogue
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

{ TDBBackupUI.TBackupThread }

constructor TDBBackupUI.TBackupThread.Create(const BakFileName: string);
begin
  inherited Create(True);
  fBakFileName := BakFileName;
end;

procedure TDBBackupUI.TBackupThread.Execute;
var
  DBBackup: TDatabaseBackup;  // object used to perform backup
resourcestring
  // Dialogue box caption
  sCaption = 'Save Backup';
begin
  DBBackup := TDatabaseBackup.Create(fBakFileName);
  try
    DBBackup.Backup;
  finally
    DBBackup.Free;
  end;
end;

end.

