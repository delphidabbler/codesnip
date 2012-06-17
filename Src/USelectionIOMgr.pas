{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Provides a static object that manages loading and saving of stored
 * database selections.
}


unit USelectionIOMgr;


interface


uses
  // Project
  UBaseObjects, USearch;


type

  TSelectionIOMgr = class sealed(TNoConstructObject)
  strict private
    class procedure CanOpenDialogClose(Sender: TObject;
      var CanClose: Boolean);
    class procedure CanSaveDialogClose(Sender: TObject;
      var CanClose: Boolean);
    class function GetLoadFileName(out FileName: string): Boolean;
    class function GetSaveFileName(out FileName: string): Boolean;
  public
    class procedure SaveCurrentSelection;
    class function CanSaveCurrentSelection: Boolean;
    class function LoadSelectionSearch(out Search: ISearch): Boolean;
  end;


implementation


uses
  // Delphi
  SysUtils, Dialogs,
  // Project
  DB.USnippet, UConsts, UMessageBox, UOpenDialogEx, UOpenDialogHelper,
  UQuery, USaveDialogEx, USelectionIOHandler, USnippetIDs;


{ TSelectionIOMgr }

class procedure TSelectionIOMgr.CanOpenDialogClose(Sender: TObject;
  var CanClose: Boolean);
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

class function TSelectionIOMgr.CanSaveCurrentSelection: Boolean;
begin
  Result := Query.IsSearchActive and not Query.Selection.IsEmpty;
end;

class procedure TSelectionIOMgr.CanSaveDialogClose(Sender: TObject;
  var CanClose: Boolean);
var
  Dlg: TSaveDialogEx; // dialog box instance triggering this event
  FileSpec: string;   // full path to entered or selected file name

  function QueryOverwrite: Boolean;
  resourcestring
    // Text of query displayed in dialog box
    sQueryMsg = '%s already exists.' + EOL + 'Do you want to replace it?';
  begin
    Result := TMessageBox.Confirm(Dlg, Format(sQueryMsg, [FileSpec]));
  end;

begin
  Dlg := Sender as TSaveDialogEx;
  FileSpec := FileOpenFileNameWithExt(Dlg);
  CanClose := not FileExists(FileSpec) or QueryOverwrite;
end;

class function TSelectionIOMgr.GetLoadFileName(out FileName: string): Boolean;
resourcestring
  sDlgTitle = 'Load Selection From File';               // dialogue box title
  sFilter = 'CodeSnip export files (*.cssel)|*.cssel|'  // file filter
    + 'All files (*.*)|*.*';
var
  OpenDlg: TOpenDialogEx; // load selection dialogue box
begin
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
    OpenDlg.Title := sDlgTitle;
    OpenDlg.HelpKeyword := 'LoadSelectionDlg';
    Result := OpenDlg.Execute;
    if Result then
      FileName := OpenDlg.FileName;
  finally
    OpenDlg.Free;
  end;
end;

class function TSelectionIOMgr.GetSaveFileName(out FileName: string): Boolean;
resourcestring
  sDlgTitle = 'Save Selection From File';               // dialogue box caption
  sFilter = 'CodeSnip export files (*.cssel)|*.cssel|'  // file filter
    + 'All files (*.*)|*.*';
var
  SaveDlg: TSaveDialogEx; // save selection dialogue box
begin
  SaveDlg := TSaveDialogEx.Create(nil);
  try
    SaveDlg.Title := sDlgTitle;
    SaveDlg.Options := [ofShowHelp, ofNoTestFileCreate, ofEnableSizing];
    SaveDlg.Filter := sFilter;
    SaveDlg.FilterIndex := 1;
    SaveDlg.HelpKeyword := 'SaveSelectionDlg';
    SaveDlg.OnCanClose := CanSaveDialogClose;
    Result := SaveDlg.Execute;
    if Result then
      FileName := FileOpenFileNameWithExt(SaveDlg);
  finally
    SaveDlg.Free;
  end;
end;

class function TSelectionIOMgr.LoadSelectionSearch(out Search: ISearch):
  Boolean;
var
  FileName: string;
  Reader: TSelectionFileReader;
  SnippetIDs: ISnippetIDList;
  Criteria: IStoredSelectionSearchCriteria;
begin
  if not GetLoadFileName(FileName) then
    Exit(False);
  Reader := TSelectionFileReader.Create;
  try
    SnippetIDs := Reader.ReadFile(FileName);
  finally
    Reader.Free;
  end;
  Criteria := TSearchCriteriaFactory.CreateStoredSelectionSearchCriteria(
    SnippetIDs
  );
  Search := TSearchFactory.CreateStoredSelectionSearch(Criteria);
  Result := True;
end;

class procedure TSelectionIOMgr.SaveCurrentSelection;
var
  FileName: string;
  Writer: TSelectionFileWriter;
  SnippetIDs: ISnippetIDList;
  Snippet: TSnippet;
begin
  if not GetSaveFileName(FileName) then
    Exit;
  Writer := TSelectionFileWriter.Create;
  try
    SnippetIDs := TSnippetIDList.Create;
    for Snippet in Query.Selection do
      SnippetIDs.Add(Snippet.ID);
    Writer.WriteFile(FileName, SnippetIDs);
  finally
    Writer.Free;
  end;
end;

end.

