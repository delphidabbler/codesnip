{
 * UCodeImportMgr.pas
 *
 * Implements a static class that handles import of a codesnip export file into
 * the user-defined database.
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
 * The Original Code is UCodeImportMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UCodeImportMgr;


interface


uses
  // Project
  UBaseObjects, UCodeImportExport, UExceptions;


type

  {
  TCodeImportMgr:
    Sealed static class that handles import of a codesnip export file into the
    user-defined database. Gets name of import file, checks for duplicate
    entries and updates the user-defined database.
  }
  TCodeImportMgr = class sealed(TNoConstructObject)
  strict private
    const
      // Ids of buttons used in dialog box shown when an imported snippet exists
      mrReplace = 100;  // replace button
      mrSkip = 101;     // skip button
      mrRename = 102;   // rename button
    class procedure CanOpenDialogClose(Sender: TObject;
      var CanClose: Boolean);
      {Handles open dialog box's OnCanClose event. Prevents dialog from closing
      if selected file does not exist.
        @param Sender [in] Reference to dialog box that triggered event. Must be
          of type TOpenDialogEx.
        @param CanClose [in/out] Flag that determines if dialog can close. Set
          to False to prevent closure or True to permit it.
      }
    class function GetImportFileName: string;
      {Gets name of import file from user.
        @return Name of import file or '' if user cancels.
      }
    class procedure ReadImportFile(const FileName: string;
      out UserInfo: TUserInfo; out SnipList: TSnippetInfoList);
      {Reads and parses import file.
        @param FileName [in] Name of import file.
        @param UserInfo [out] Details of user who created file. May be nul if
          file doesn't contain user information.
        @param SnipList [out] List of snippets specified in import file.
        @except ECodeImportMgr raised if can't read import file or it is
          invalid.
      }
    class function RenameSnippet(var Snippet: TSnippetInfo): Boolean;
      {Gets new name for a snippet from user.
        @param Snippet [in] Info about snippet to be renamed. [out] Name field
          updated to receive new name. Unchanged if False returned.
        @return True if user changed name, false if renamed was cancelled.
      }
    class function FilterSnippets(var SnipList: TSnippetInfoList): Boolean;
      {Checks for duplicate snippets and filters out any that user does not wish
      to import. Duplicates may be renamed.
        @param SnipList [in] List of snippets from import file. [out] List of
          filtered snippets (some may be removed or renamed). Undefined if False
          is returned.
        @return True if import to go ahead, False to abort.
      }
    class function QueryOverwrite(const SnippetName: string): Integer;
      {Displays dialog box that asks user what action to perform if a snippet
      already exists in user database.
        @param SnippetName [in] Name of snippet to be overwritten.
        @return Code representing required action.
      }
    class procedure UpdateUserDatabase(const UserInfo: TUserInfo;
      const SnipList: TSnippetInfoList);
      {Updates user database with filtered list of imported snippets.
        @param UserInfo [in] Record containing user information pertaining to
          imported snippets.
        @param SnipList [in] Details of required snippets.
      }
    class procedure DisplayUserInfo(const UserInfo: TUserInfo);
      {Displays user information from an import file.
        @param UserInfo [in] Record containing user information.
      }
    class procedure ReportImportedSnippets(const SnipList: TSnippetInfoList);
      {Reports details of imported snippets to user.
        @param SnipList [in] Details of all imported snippets.
      }
  public
    class procedure Execute;
      {Performs import from a user-specified file. Does nothing if user cancels.
        @except ECodeImportMgr raised if can't read import file.
      }
  end;

  {
  ECodeImportMgr:
    Class of exception raised when import manager encounteres an expected error.
  }
  ECodeImportMgr = class(ECodeSnip);


implementation


uses
  // Delphi
  SysUtils, Classes, Controls, Dialogs,
  // Project
  DB.USnippet, FmEditTextDlg, UActiveText, UConsts, UIOUtils, UMessageBox,
  UOpenDialogEx, UOpenDialogHelper, USnippetIDs, USnippets, USnippetValidator,
  UStructs;


{ TCodeImportMgr }

class procedure TCodeImportMgr.CanOpenDialogClose(Sender: TObject;
  var CanClose: Boolean);
  {Handles open dialog box's OnCanClose event. Prevents dialog from closing if
  selected file does not exist.
    @param Sender [in] Reference to dialog box that triggered event. Must be of
      type TOpenDialogEx.
    @param CanClose [in/out] Flag that determines if dialog can close. Set to
      False to prevent closure or True to permit it.
  }
var
  Dlg: TOpenDialogEx; // dialog box instance triggering this event
  FileSpec: string;   // full path to entered or selected file name
resourcestring
  // Error messages
  sFileDoesNotExist = '"%s" does not exist.';
begin
  Dlg := Sender as TOpenDialogEx;
  FileSpec := FileOpenEditedFileName(Dlg);
  CanClose := FileExists(FileSpec);
  if not CanClose then
    TMessageBox.Error(Dlg, Format(sFileDoesNotExist, [FileSpec]));
end;

class procedure TCodeImportMgr.DisplayUserInfo(const UserInfo: TUserInfo);
  {Displays user information from an import file.
    @param UserInfo [in] Record containing user information.
  }
resourcestring
  // User name and email message
  sUserInfo = 'Importing data supplied by: %0:s <%1:s>';
  // User comments message
  sUserComments = 'Notes: %s';
var
  DisplayStr: string; // text to be displayed in message box
begin
  DisplayStr := Format(
    sUserInfo, [UserInfo.Details.Name, UserInfo.Details.Email]
  );
  if UserInfo.Comments <> '' then
    DisplayStr := DisplayStr + EOL2
      + Format(sUserComments, [UserInfo.Comments]);
  TMessageBox.Information(nil, DisplayStr);
end;

class procedure TCodeImportMgr.Execute;
  {Performs import from a user-specified file. Does nothing if user cancels.
    @except ECodeImportMgr raised if can't read import file.
  }
var
  UserInfo: TUserInfo;        // any user info from import file
  SnipList: TSnippetInfoList; // list of imported snippets
  FileName: string;           // name of import file
resourcestring
  // Message displayed if no snippets imported
  sNoSnippets = 'No snippets were imported';
begin
  // Import user specified file
  FileName := GetImportFileName;
  if FileName = '' then
    Exit;
  ReadImportFile(FileName, UserInfo, SnipList);
  // Display user info if there is any
  if not UserInfo.IsNul then
    DisplayUserInfo(UserInfo);
  // Perform import: remove any duplicate snippets that are not to be renamed or
  // overwritten
  if FilterSnippets(SnipList) then
  begin
    UpdateUserDatabase(UserInfo, SnipList);
    // report import results to user
    ReportImportedSnippets(SnipList);
  end
  else
    // no snippets imported: inform user
    TMessageBox.Information(nil, sNoSnippets);
end;

class function TCodeImportMgr.FilterSnippets(var SnipList: TSnippetInfoList):
  Boolean;
  {Checks for duplicate snippets and filters out any that user does not wish
  to import. Duplicates may be renamed.
    @param SnipList [in] List of snippets from import file. [out] List of
      filtered snippets (some may be removed or renamed). Undefined if False is
      returned.
    @return True if import to go ahead, False to abort.
  }
var
  Required: TSnippetInfoList;   // list of required snippets
  SnippetIdx: Integer;          // loops through all imported snippets
  RequiredCount: Integer;       // count of required snippets

  // ---------------------------------------------------------------------------
  procedure IncludeSnippet(const Snippet: TSnippetInfo);
    {Includes a snippet in filtered list.
      @param Snippet [in] Info about snippet to be included.
    }
  begin
    Inc(RequiredCount);
    SetLength(Required, RequiredCount);
    Required[RequiredCount - 1] := Snippet;
  end;
  // ---------------------------------------------------------------------------

begin
  Result := False;
  RequiredCount := 0;
  // Scan all snippets checking if they already exist in user database
  for SnippetIdx := Low(SnipList) to High(SnipList) do
  begin
    if Database.Snippets.Find(SnipList[SnippetIdx].Name, True) = nil then
      // snippet doesn't exist: include it in import
      IncludeSnippet(SnipList[SnippetIdx])
    else
    begin
      // snippet exists, get required action from user
      case QueryOverwrite(SnipList[SnippetIdx].Name) of
        mrReplace:
          // overwrite snippet: include in filtered list
          IncludeSnippet(SnipList[SnippetIdx]);
        mrSkip:
          // skip snippet: do nothing (i.e. don't place in filtered list)
          ;
        mrRename:
        begin
          // rename snippet: get user to rename and include in filtered list
          // unless user cancels rename
          if RenameSnippet(SnipList[SnippetIdx]) then
            IncludeSnippet(SnipList[SnippetIdx]);
        end;
        mrCancel:
          // cancel whole import
          Exit;
      end;
    end;
  end;
  if RequiredCount = 0 then
    // No snippets required
    Exit;
  // Copy required snippets into SnipList param
  SetLength(SnipList, Length(Required));
  for SnippetIdx := Low(Required) to High(Required) do
    SnipList[SnippetIdx] := Required[SnippetIdx];
  Result := True;
end;

class function TCodeImportMgr.GetImportFileName: string;
  {Gets name of import file from user.
    @return Name of import file or '' if user cancels.
  }
var
  OpenDlg: TOpenDialogEx; // self-aligning enhanced open dialog box
resourcestring
  sFilter = 'CodeSnip export files (*.csexp)|*.csexp|'  // file filter
    + 'All files (*.*)|*.*';
  sTitle = 'Import Snippets';                           // dialog box title
begin
  // Create and initialise
  OpenDlg := TOpenDialogEx.Create(nil);
  try
    OpenDlg.OnCanClose := CanOpenDialogClose;
    OpenDlg.Filter := sFilter;
    OpenDlg.FilterIndex := 1;
    OpenDlg.InitialDir := '';
    // we don't include ofFileMustExist in Options below since we handle
    // non-existant files ourselves
    OpenDlg.Options := [ofHideReadOnly, ofEnableSizing];
    OpenDlg.OptionsEx := [];
    OpenDlg.Title := sTitle;
    OpenDlg.HelpKeyword := 'ImportFileDlg';
    if OpenDlg.Execute then
      // User OKd: return entered file name
      Result := OpenDlg.FileName
    else
      Result := '';
  finally
    FreeAndNil(OpenDlg);
  end;
end;

class function TCodeImportMgr.QueryOverwrite(const SnippetName: string):
  Integer;
  {Displays dialog box that asks user what action to perform if a snippet
  already exists in user database.
    @param SnippetName [in] Name of snippet to be overwritten.
    @return Code representing required action.
  }
resourcestring
  // Prompt displayed in dialog box
  sPrompt = 'Importing %0:s' + EOL2
    + 'The user database already contains a snippet named %0:s.' + EOL2
    + 'You can replace the existing snippet, skip importing the snippet or '
    + 'rename the imported snippet.' + EOL
    + 'Click cancel to abort the import.';
  // Dialog box button captions
  sBtnReplace = '&Replace';
  sBtnSkip = '&Skip';
  sBtnRename = 'Re&name';
begin
  Result := TMessageBox.Custom(
    nil,
    Format(sPrompt, [SnippetName]),
    [
      TMessageBoxButton.Create(sBtnReplace, mrReplace),
      TMessageBoxButton.Create(sBtnSkip, mrSkip, True),
      TMessageBoxButton.Create(sBtnRename, mrRename),
      TMessageBoxButton.Create(sBtnCancel, mrCancel, False, True)
    ]
  );
end;

class procedure TCodeImportMgr.ReadImportFile(const FileName: string;
  out UserInfo: TUserInfo; out SnipList: TSnippetInfoList);
  {Reads and parses import file.
    @param FileName [in] Name of import file.
    @param UserInfo [out] Details of user who created file. May be nul if file
      doesn't contain user information.
    @param Snips [out] List of snippets specified in import file.
    @except ECodeImportMgr raised if can't read import file or it is invalid.
  }
var
  ImportData: TBytes; // bytes from import file
begin
  Assert(FileName <> '', ClassName + '.ReadImportFile: No file name provided');
  try
    ImportData := TFileIO.ReadAllBytes(FileName);
    TCodeImporter.ImportData(UserInfo, SnipList, ImportData);
  except
    on E: EStreamError do
      raise ECodeImportMgr.Create(E);
    on E: ECodeImporter do
      raise ECodeImportMgr.Create(E);
    else
      raise;
  end;
end;

class function TCodeImportMgr.RenameSnippet(var Snippet: TSnippetInfo): Boolean;
  {Gets new name for a snippet from user.
    @param Snippet [in] Info about snippet to be renamed. [out] Name field
      updated to receive new name. Unchanged if False returned.
    @return True if user changed name, false if renamed was cancelled.
  }
resourcestring
  // Dialog box title and prompt
  sDlgTitle = 'Rename Snippet';
  sDlgPrompt = 'Enter a new snippet &name (cancel skips the snippet):';
  // Information message
  sSkippingSnippet = 'Skipping "%s"';
var
  NewName: string;        // new snippet name
  NameCounter: Integer;   // counter used to append to snippet name
begin
  // Create new suggested name
  NameCounter := 2;
  while Database.Snippets.Find(
    Snippet.Name + IntToStr(NameCounter), True
  ) <> nil do
    Inc(NameCounter);
  NewName := Snippet.Name + IntToStr(NameCounter);
  // Get valid name from user: validation done using anonymous method passed to
  // TEditTextDlg. Dialog box does not return until valid name entered or user
  // cancels
  Result := TEditTextDlg.Execute(
    nil,
    sDlgTitle,
    sDlgPrompt,
    NewName,
    function(const Name: string; out ErrMsg: string): Boolean
    var
      DummySel: TSelection;
    begin
      Result := TSnippetValidator.ValidateName(Name, True, ErrMsg, DummySel);
    end
  );
  if Result then
    // OK: record new name
    Snippet.Name := NewName
  else
    // Cancelled: skip snippet
    TMessageBox.Information(nil, Format(sSkippingSnippet, [NewName]));
end;

class procedure TCodeImportMgr.ReportImportedSnippets(
  const SnipList: TSnippetInfoList);
  {Reports details of imported snippets to user.
    @param SnipList [in] Details of all imported snippets.
  }
resourcestring
  // Messages displayed depending on if one or more than one snippets imported
  sSingle = 'A snippet named "%s" was imported';
  sMultiple = 'The following snippets were imported:' + EOL2 + '%s';
var
  Idx: Integer;         // loops through list of snippets
  SnippetList: string;  // display list of snippet names
begin
  Assert(Length(SnipList) > 0,
    ClassName + '.ReportImportedSnippets: No snippets to report.');
  if Length(SnipList) = 1 then
    // Only one snippet: just report the name
    TMessageBox.Information(nil, Format(sSingle, [SnipList[0].Name]))
  else
  begin
    // More that one snippet: display bullet list of names
    SnippetList := '';
    for Idx := Low(SnipList) to High(SnipList) do
      SnippetList := SnippetList + '  » ' + SnipList[Idx].Name + EOL;
    TMessageBox.Information(nil, Format(sMultiple, [TrimRight(SnippetList)]));
  end;
end;

class procedure TCodeImportMgr.UpdateUserDatabase(const UserInfo: TUserInfo;
  const SnipList: TSnippetInfoList);
  {Updates user database with filtered list of imported snippets.
    @param UserInfo [in] Record containing user information pertaining to
      imported snippets.
    @param SnipList [in] Details of required snippet.
  }

  // ---------------------------------------------------------------------------
  procedure AdjustDependsList(const Depends: ISnippetIDList);
    {Adjusts a snippet's dependency list so that main database is searched for
    a required snippet if it is not in the user database.
      @param Depends [in] Dependency list to be adjusted.
    }
  var
    Idx: Integer;           // loops through dependencies
    SnippetID: TSnippetID;  // each snippet ID in dependency list
  begin
    // NOTE: The data file format does not record which database a required
    // snippet belongs to, so we first look in the user database and if it's
    // not there, we assume the main database
    for Idx := 0 to Pred(Depends.Count) do
    begin
      SnippetID := Depends[Idx];
      SnippetID.UserDefined :=
        Database.Snippets.Find(SnippetID.Name, True) <> nil;
      Depends[Idx] := SnippetID;
    end;
  end;

  function UserInfoActiveText: IActiveText;
    {Builds an active text representation of the contributing user's name and
    email address.
      @return Required active text representation.
    }
  resourcestring
    // user information text template
    sContributorText = 'Contributed by: %0:s <%1:s>';
  begin
    Assert(not UserInfo.IsNul, ClassName +
      '.UpdateUserDatabase:UserInfoActiveText: UserInfo is nul');
    Result := TActiveTextFactory.CreateActiveText;
    Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsOpen));
    Result.AddElem(
      TActiveTextFactory.CreateTextElem(
        Format(
          sContributorText, [UserInfo.Details.Name, UserInfo.Details.Email]
        )
      )
    );
    Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsClose));
  end;
  // ---------------------------------------------------------------------------

var
  Idx: Integer;           // loops through all snippets
  Editor: IDatabaseEdit;  // object used to update user database
  Snippet: TSnippet;      // reference to existing snippets
begin
  Editor := Database as IDatabaseEdit;
  for Idx := Low(SnipList) to High(SnipList) do
  begin
    AdjustDependsList(SnipList[Idx].Data.Refs.Depends);
    if not UserInfo.IsNul then
      SnipList[Idx].Data.Props.Extra.Append(UserInfoActiveText);
    Snippet := Database.Snippets.Find(SnipList[Idx].Name, True);
    if Assigned(Snippet) then
      // snippet already exists: overwrite it
      Editor.UpdateSnippet(Snippet, SnipList[Idx].Data)
    else
      // snippet is new: add to database
      Editor.AddSnippet(SnipList[Idx].Name, SnipList[Idx].Data);
  end;
end;

end.

