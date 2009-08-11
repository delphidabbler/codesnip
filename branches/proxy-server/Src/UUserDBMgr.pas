{
 * UUserDBMgr.pas
 *
 * Implements a static class that manages user's interaction with user database.
 *
 * v1.0 of 14 Sep 2008  - Original version.
 * v1.1 of 19 Sep 2008  - Now prevents deletion of snippets with dependents.
 *                      - Referenced routines now listed in deletion prompt.
 * v1.2 of 04 Oct 2008  - Changed TUserDBMgr to derive from TNoConstructObject
 *                        and hence prevented it from being constructed.
 *                      - Now use ClassName method in all assert and raise EBug
 *                        statements.
 * v1.3 of 15 Dec 2008  - Fixed minor bug where open and save dialogs could
 *                        incorrectly identify existance of a file.
 * v1.4 of 14 Jan 2009  - Replaced control char literals with constants.
 * v1.5 of 06 Jun 2009  - Changed to use TSnippetID and ISnippetIDList instead
 *                        of TRoutineID and IRoutineIDList.
 * v1.6 of 13 Jul 2009  - Removed unused overloaded TUserDBMgr.Edit method and
 *                        modified other Edit method to remove dependency.
 *
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
 * The Original Code is UUserDBMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UUserDBMgr;


interface


uses
  // Project
  UBaseObjects, UView;


type

  {
  TUserDBMgr:
    Static class that manages user's interaction with user database.
  }
  TUserDBMgr = class(TNoConstructObject)
  strict private
    class procedure CanOpenDialogClose(Sender: TObject; var CanClose: Boolean);
      {OnClose event handler for open dialog box. Checks if dialog box can
      close.
        @param Sender [in] Reference to dialog box.
        @param CanClose [in/out] Set to True to permit dialog to close or False
          to inhibit closure.
      }
    class procedure CanSaveDialogClose(Sender: TObject; var CanClose: Boolean);
      {OnClose event handler for save dialog box. Checks if dialog box can
      close.
        @param Sender [in] Reference to dialog box.
        @param CanClose [in/out] Set to True to permit dialog to close or False
          to inhibit closure.
      }
  public
    class function CanEdit(const ViewItem: TViewItem): Boolean;
      {Checks if a view item can be edited.
        @param ViewItem [in] View item to check.
        @return True if view item can be edited, i.e. if ViewItem is a routine
          and is user defined.
      }
    class function CanSave: Boolean;
      {Checks if user database can be saved.
        @return True if database can be saved, False if not.
      }
    class procedure Edit(const RoutineName: string); 
      {Gets user to edit a named snippet using edit snippet dialog box.
        @param RoutineName [in] Name of routine to be edited. Must be user
          defined.
      }
    class procedure Add;
      {Adds a new user defined snippet to database.
      }
    class procedure Save;
      {Saves user database.
      }
    class procedure Delete(const ViewItem: TViewItem);
      {Deletes a snippet from the database if possible.
        @param ViewItem [in] Snippet to be deleted. Must be a user defined
          routine.
      }
    class procedure BackupDatabase;
      {Creates a backup of user database in a file specified by user.
      }
    class function RestoreDatabase: Boolean;
      {Restores user database from a previously created backup file.
        @return True if user OKs, False if not.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Dialogs,
  // Project
  FmUserDBEditDlg, UConsts, UExceptions, UIStringList, UMessageBox,
  UOpenDialogEx, UOpenDialogHelper, USaveDialogEx, USnippetIDs, USnippets,
  UUserDBBackup;


{ TUserDBMgr }

class procedure TUserDBMgr.Add;
  {Adds a new user defined snippet to database.
  }
begin
  // Display Add Snippet dialog box which performs update of database.
  TUserDBEditDlg.AddNewRoutine(nil);
end;

class procedure TUserDBMgr.BackupDatabase;
  {Creates a backup of user database in a file specified by user.
  }
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
        FreeAndNil(UserDBBackup);
      end;
    end;
  finally
    FreeAndNil(SaveDlg);
  end;
end;

class function TUserDBMgr.CanEdit(const ViewItem: TViewItem): Boolean;
  {Checks if a view item can be edited.
    @param ViewItem [in] View item to check.
    @return True if view item can be edited, i.e. if ViewItem is a routine and
      is user defined.
  }
begin
  Assert(Assigned(ViewItem), ClassName + '.CanEdit: VeiwItem is nil');
  Result := Assigned(ViewItem) and
    (ViewItem.Kind = vkRoutine) and
    ViewItem.Routine.UserDefined;
end;

class procedure TUserDBMgr.CanOpenDialogClose(Sender: TObject;
  var CanClose: Boolean);
  {OnClose event handler for open dialog box. Checks if dialog box can close.
    @param Sender [in] Reference to dialog box.
    @param CanClose [in/out] Set to True to permit dialog to close or False to
      inhibit closure.
  }
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

class function TUserDBMgr.CanSave: Boolean;
  {Checks if user database can be saved.
    @return True if database can be saved, False if not.
  }
begin
  // We can save database if it's been changed
  Result := (Snippets as ISnippetsEdit).Updated;
end;

class procedure TUserDBMgr.CanSaveDialogClose(Sender: TObject;
  var CanClose: Boolean);
  {OnClose event handler for save dialog box. Checks if dialog box can close.
    @param Sender [in] Reference to dialog box.
    @param CanClose [in/out] Set to True to permit dialog to close or False to
      inhibit closure.
  }
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

class procedure TUserDBMgr.Delete(const ViewItem: TViewItem);
  {Deletes a snippet from the database if possible.
    @param ViewItem [in] Snippet to be deleted. Must be a user defined routine.
  }

  // ---------------------------------------------------------------------------
  function RoutineNames(const IDList: ISnippetIDList): IStringList;
    {Builds a list of routine names from a routine ID list.
      @param IDList [in] List of routine IDs.
      @return List of routine names.
    }
  var
    ID: TSnippetID; // loops through all IDs in list
  begin
    Result := TIStringList.Create;
    for ID in IDList do
      Result.Add(ID.Name);
  end;
  // ---------------------------------------------------------------------------

var
  Dependents: ISnippetIDList; // list of dependent routine IDs
  Referrers: ISnippetIDList;  // list referring routine IDs
  ConfirmMsg: string;         // message displayed to confirm deletion
resourcestring
  // Prompts & error messages
  sConfirmDelete = 'Please confirm you wish to delete %s';
  sConfirmDeleteEx = 'Please confirm you wish to delete %0:s.' + EOL2
    + 'Deleting this snippet will also remove it from the cross reference list '
    + 'of the following snippets:' + EOL + '    %1:s';
  sHasDependents = 'Sorry, this snippet can''t be deleted. It is required by '
    + 'the following snippets:' + EOL + '    %s';
begin
  Assert(ViewItem.Kind = vkRoutine,
    ClassName + '.Delete: Current view kind is not vkRoutine');
  Assert(ViewItem.Routine.UserDefined,
    ClassName + '.Delete: Routine must be user defined');
  // Check if routine has dependents: don't allow deletion if so
  Dependents := (Snippets as ISnippetsEdit).GetDependents(ViewItem.Routine);
  if Dependents.Count > 0 then
  begin
    TMessageBox.Error(
      nil,
      Format(
        sHasDependents,
        [RoutineNames(Dependents).GetText(',' + EOL + '    ', False)]
      )
    );
    Exit;
  end;
  // Get permission to delete. If routine has dependents list them in prompt
  Referrers := (Snippets as ISnippetsEdit).GetReferrers(ViewItem.Routine);
  if Referrers.Count = 0 then
    ConfirmMsg := Format(sConfirmDelete, [ViewItem.Routine.Name])
  else
    ConfirmMsg := Format(
      sConfirmDeleteEx,
      [
        ViewItem.Routine.Name,
        RoutineNames(Referrers).GetText(',' + EOL + '    ', False)
      ]
    );
  if TMessageBox.Confirm(nil, ConfirmMsg) then
    (Snippets as ISnippetsEdit).DeleteRoutine(ViewItem.Routine);
end;

class procedure TUserDBMgr.Edit(const RoutineName: string);
  {Gets user to edit a named snippet using edit snippet dialog box.
    @param RoutineName [in] Name of routine to be edited. Must be user defined.
  }
var
  Routine: TRoutine;    // reference to routine to be edited
begin
  Routine := Snippets.Routines.Find(RoutineName, True);
  if not Assigned(Routine) then
    raise EBug.Create(ClassName + '.Edit: Routine not in user database');
  TUserDBEditDlg.EditRoutine(nil, Routine);
end;

class function TUserDBMgr.RestoreDatabase: Boolean;
  {Restores user database from a previously created backup file.
    @return True if user OKs, False if not.
  }
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
        FreeAndNil(UserDBBackup);
      end;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

class procedure TUserDBMgr.Save;
  {Saves user database.
  }
begin
  (Snippets as ISnippetsEdit).Save;
end;

end.

