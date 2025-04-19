{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2025, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Helper routines for use when working with standard windows open and save file
 * dialog boxes.
}


unit UOpenDialogHelper;


interface


uses
  // Delphi
  Dialogs;


function FileOpenEditedFileName(const Dlg: TOpenDialog): string;
  {Gets full path to the file that is currently entered in a file open dialog
  box.
    @param Dlg [in] Dialog box for which file name is required.
    @return Required file path.
  }

function FileOpenFileNameWithExt(Dlg: TOpenDialog): string;
  {Ensures a file name entered in a dialog box has a default extension if none
  provided.
    @param Dlg [in] Dialog box containing required file name and extension info.
    @return File name, with default extension if (1) file name has no extension,
      (2) current filter is not *.* (3) dialog has a filter string.
  }

function FilterIndexToExt(const Dlg: TOpenDialog): string;
  {Extracts extension associated with a standard file open dialog's selected
  file type from its "|" delimited Filter property.
    @param Dlg [in] Dialog box for which current extension is to be extracted.
    @return Extension specified by dialog's FilterIndex property with a
      prepended '.'.
  }

///  <summary>Calculates the index of a file type description in a &quot;|&quot;
///  delimited string, as used in Windows standard file dialogue boxes.
///  </summary>
///  <param name="FilterStr"><c>string</c> [in] List of file types and
///  extensions. Must have format
///  <c>file desc 1|(*.ext1)|file desc 2|(*.ext2)</c> etc...</param>
///  <param name="Desc"><c>string</c> [in] File type description to be found.
///  </param>
///  <param name="DefIdx"><c>Integer</c> [in] Default 1 based index to use if
///  <c>Desc</c> is not in <c>FilterStr</c>.</param>
///  <returns><c>Integer</c>. 1 based index of the file type description in the
///  filter string, or <c>DefIdx</c> if the description is not found.</returns>
function FilterDescToIndex(const FilterStr, Desc: string;
  const DefIdx: Integer): Integer;

function FileOpenEditedFileNameWithExt(const Dlg: TOpenDialog): string;
  {Gets full path to the file that is currently entered in a file open dialog
  box, with default extension added if necessary.
    @param Dlg [in] Dialog box for which file name is required.
    @return Required file path.
  }


implementation


uses
  // Delphi
  SysUtils, Classes, Windows, Dlgs, CommDlg,
  // Project
  UStrUtils, UUtils;


function FilterIndexToExt(const Dlg: TOpenDialog): string;
  {Extracts extension associated with a standard file open dialog's selected
  file type from its "|" delimited Filter property.
    @param Dlg [in] Dialog box for which current extension is to be extracted.
    @return Extension specified by dialog's FilterIndex property with a
      prepended '.'.
  }
var
  FilterParts: TStringList; // stores filter split into component parts
begin
  FilterParts := TStringList.Create;
  try
    // Split filter string into parts (divided by | chars):
    // even number indexes are descriptions and odd indexes are extensions
    StrExplode(Dlg.Filter, '|', FilterParts);
    Result := ExtractFileExt(FilterParts[2 * (Dlg.FilterIndex - 1) + 1]);
  finally
    FreeAndNil(FilterParts);
  end;
end;

function FilterDescToIndex(const FilterStr, Desc: string;
  const DefIdx: Integer): Integer;
var
  FilterParts: TStringList; // stores filter split into component parts
  Descs: TStringList;       // list of file type descriptions in filter string
  Idx: Integer;             // loops thru Descs in filter string
  DescStr: string;
  DescEnd: Integer;
begin
  Descs := nil;
  FilterParts := TStringList.Create;
  try
    // Split filter string into parts (divided by | chars):
    // even number indexes are descriptions and odd indexes are Descs
    StrExplode(FilterStr, '|', FilterParts);
    // Record only Descs (every 2nd entry starting at index 1)
    Descs := TStringList.Create;
    Idx := 0;
    while Idx < FilterParts.Count do
    begin
      DescStr := FilterParts[Idx];
      DescEnd := StrPos('(', DescStr) - 2;
      DescStr := Copy(DescStr, 1, DescEnd);
      Descs.Add(DescStr);
      Inc(Idx, 2);
    end;
    // Check if required extension in list
    Result := Descs.IndexOf(Desc);
    if Result >= 0 then
      // description in list, increment by 1 since filter indexes are 1 based
      Inc(Result)
    else
      Result := DefIdx;
  finally
    Descs.Free;
    FilterParts.Free;
  end;
end;

function FileOpenFileNameWithExt(Dlg: TOpenDialog): string;
  {Ensures a file name entered in a dialog box has a default extension if none
  provided.
    @param Dlg [in] Dialog box containing required file name and extension info.
    @return File name, with default extension if (1) file name has no extension,
      (2) current filter is not *.* (3) dialog has a filter string.
  }
var
  DefaultExt: string; // default extension for current filter
begin
  Result := Dlg.FileName;
  if (ExtractFileExt(Dlg.FileName) = '') and (Dlg.Filter <> '') then
  begin
    DefaultExt := FilterIndexToExt(Dlg);
    if not StrContainsStr('*', DefaultExt) then
      Result := Result + DefaultExt;
  end;
end;

function FileOpenEditText(const Dlg: TOpenDialog): string;
  {Gets text from file edit control in a file open dialog box.
    @param Dlg [in] Reference to dialog box for which text is required.
    @return Required text.
  }
var
  DlgItemWnd: HWND;                 // handle of edit control window in dlg box
  Text: array[0..MAX_PATH] of Char; // buffer to receive edit control text
begin
  Result := '';
  // File name entered by user or selected in dialog is displayed either in
  // edit control edt1 or in combo box cmb13. We try to get text first from
  // edt1 and if that fails, from cmb13. If we fail to find both we bail out.
  DlgItemWnd := GetDlgItem(GetParent(Dlg.Handle), edt1);
  if DlgItemWnd = 0 then
    DlgItemWnd := GetDlgItem(GetParent(Dlg.Handle), cmb13);
  if DlgItemWnd = 0 then
    Exit;
  // Get text from found control
  GetWindowText(DlgItemWnd, Text, SizeOf(Text));
  Result := Text;
end;

function FileOpenFolderPath(const Dlg: TOpenDialog): string;
  {Gets name of currently selected folder in a file open dialog box.
    @param Dlg [in] Reference to dialog box for which folder is required.
    @return Path to folder.
  }
var
  Folder: array[0..MAX_PATH] of Char; // receives path to folder
begin
  SendMessage(
    GetParent(Dlg.Handle), CDM_GETFOLDERPATH, SizeOf(Folder), Integer(@Folder)
  );
  Result := Folder;
end;

function FileOpenEditedFileName(const Dlg: TOpenDialog): string;
  {Gets full path to the file that is currently entered in a file open dialog
  box.
    @param Dlg [in] Dialog box for which file name is required.
    @return Required file path.
  }
begin
  Result := FileOpenEditText(Dlg);
  if Result = '' then
    Exit;
  if IsBaseFileName(Result) then
    Result := IncludeTrailingPathDelimiter(FileOpenFolderPath(Dlg)) + Result;
end;

function FileOpenEditedFileNameWithExt(const Dlg: TOpenDialog): string;
  {Gets full path to the file that is currently entered in a file open dialog
  box, with default extension added if necessary.
    @param Dlg [in] Dialog box containing required file name and extension info.
    @return File name, with default extension if (1) file name has no extension,
      (2) current filter is not *.* (3) dialog has a filter string.
  }
var
  DefaultExt: string; // default extension for current filter
begin
  Result := FileOpenEditedFileName(Dlg);
  if Result = '' then
    Exit;
  if (ExtractFileExt(Result) = '') and (Dlg.Filter <> '') then
  begin
    DefaultExt := FilterIndexToExt(Dlg);
    if not StrContainsStr('*', DefaultExt) then
      Result := Result + DefaultExt;
  end;
end;

end.
