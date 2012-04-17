{
 * FmCodeExportDlg.pas
 *
 * Implements a dialog box that gets snippets to be exported and creates an
 * export file containing the selected snippets.
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
 * The Original Code is FmCodeExportDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmCodeExportDlg;


interface


uses
  // Delphi
  Classes, StdCtrls, Controls, Forms, ExtCtrls,
  // Project
  FmGenericOKDlg, FrCheckedTV, FrSelectUserSnippets, FrSelectSnippets,
  FrSelectSnippetsBase, UBaseObjects, USnippets;


type

  {
  TCodeExportDlg:
    A dialog box that gets snippets to be exported and creates an export file
    containing the selected snippets.
  }
  TCodeExportDlg = class(TGenericOKDlg, INoPublicConstruct)
    btnBrowse: TButton;
    edFile: TEdit;
    frmSnippets: TSelectUserSnippetsFrame;
    lblFile: TLabel;
    lblRoutines: TLabel;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  strict private
    procedure SelectRoutine(const Snippet: TRoutine);
      {Selects a snippet in the snippets check list.
        @param Snippet [in] Snippet to be selected. If nil, or not user-defined,
          no snippet is selected.
      }
    procedure WriteOutputFile;
      {Writes export file.
      }
  strict protected
    procedure ArrangeForm; override;
      {Aligns controls vertically where necessary to accomodate height of
      controls that depend on UI font.
      }
  public
    class procedure Execute(const AOwner: TComponent; const Snippet: TRoutine);
      {Displays export dialog box and writes export file if user OKs entries.
        @param AOwner [in] Reference to control that owns the dialog box.
        @param Snippet [in] Reference to a snippet to pre-select in snippets
          check list box. If nil or not user-defined then no snippet is pre-
          selected.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Dialogs,
  // Project
  UCodeImportExport, UCtrlArranger, UExceptions, UMessageBox, UOpenDialogHelper,
  USaveDialogEx, UUtils;


{$R *.dfm}

{ TCodeExportDlg }

procedure TCodeExportDlg.ArrangeForm;
  {Aligns controls vertically where necessary to accomodate height of controls
  that depend on UI font.
  }
begin
  frmSnippets.Top := TCtrlArranger.BottomOf(lblRoutines, 4);
  lblFile.Top := TCtrlArranger.BottomOf(frmSnippets, 8);
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(lblFile, 4), [edFile, btnBrowse]
  );
  pnlBody.ClientHeight := TCtrlArranger.BottomOf([edFile, btnBrowse], 8);
  inherited;
end;

procedure TCodeExportDlg.btnBrowseClick(Sender: TObject);
  {Handles clicks on browse (ellipsis) button by displaying file save dialog box
  and copies chosen file name to file name edit control.
    @param Sender [in] Not used.
  }
var
  Dlg: TSaveDialogEx; // save dialog box
resourcestring
  sCaption = 'Export File';                             // dialog box caption
  sFilter = 'CodeSnip export files (*.csexp)|*.csexp|'  // file filter
    + 'All files (*.*)|*.*';
begin
  Dlg := TSaveDialogEx.Create(Self);
  try
    Dlg.Title := sCaption;
    Dlg.Options := [ofShowHelp, ofNoTestFileCreate, ofEnableSizing];
    Dlg.Filter := sFilter;
    Dlg.FilterIndex := 1;
    Dlg.HelpKeyword := 'ExportFileDlg';
    if Dlg.Execute then
      // user selected file name: copy to edit control
      edFile.Text := FileOpenFileNameWithExt(Dlg);
  finally
    FreeAndNil(Dlg);
  end;
end;

procedure TCodeExportDlg.btnOKClick(Sender: TObject);
  {Handles click on OK button. Validates entries in dialog box and writes
  export file.
    @param Sender [in] Not used.
  }
resourcestring
  // Error messages
  sNoRoutines = 'Please select one or more snippets';
  sNoFileName = 'Please specify a file name';
  sBadPath = 'The specified file path does not exist';
  sFileExists = 'Please specify another file name';
  // Confirmation message
  sOverwriteFile = '%s already exists. OK to overwrite?';
var
  FileName: string;   // name of export file
begin
  FileName := Trim(edFile.Text);
  // Assume failure
  ModalResult := mrNone;
  try

    // Validate entries
    // must have at least one snippet
    if frmSnippets.SelectedRoutines.Count = 0 then
      raise EDataEntry.Create(sNoRoutines, frmSnippets);
    // must have a file name
    if FileName = '' then
      raise EDataEntry.Create(sNoFileName, edFile);
    // must have valid path to file
    if not UUtils.IsDirectory(ExtractFileDir(FileName)) then
      raise EDataEntry.Create(sBadPath, edFile);

    // Get permission to overwrite file if necessary
    if FileExists(FileName) and
      not TMessageBox.Confirm(Self, Format(sOverwriteFile, [FileName])) then
    begin
      edFile.SetFocus;
      Exit;
    end;

    // Everything OK: export the file and close
    WriteOutputFile;
    ModalResult := mrOK;

  except
    on E: EDataEntry do
    begin
      // Handle data entry error: display message and focus error control
      TMessageBox.Error(Self, E.Message);
      if Assigned(E.Ctrl) then
        E.Ctrl.SetFocus;
    end;
  end;
end;

class procedure TCodeExportDlg.Execute(const AOwner: TComponent;
  const Snippet: TRoutine);
  {Displays export dialog box and writes export file if user OKs entries.
    @param AOwner [in] Reference to control that owns the dialog box.
    @param Snippet [in] Reference to a routine to pre-select in snippets check
      list box. If nil or not user-defined then no snippet is pre-selected.
  }
begin
  with InternalCreate(AOwner) do
    try
      SelectRoutine(Snippet);
      ShowModal;
    finally
      Free;
    end;
end;

procedure TCodeExportDlg.SelectRoutine(const Snippet: TRoutine);
  {Selects a snippet in the snippets check list.
    @param Snippet [in] Snippet to be selected. If nil, or not user-defined, no
      snippet is selected.
  }
var
  List: TRoutineList; // list containing only the provided routine
begin
  if not Assigned(Snippet) or not Snippet.UserDefined then
    // Snippet is nil or not user-defined: select nothing
    frmSnippets.SelectedRoutines := nil
  else
  begin
    // Snippet is user-defined. We make a snippet list containing only this
    // snippet because frmRoutines requires a list of snippets to select.
    List := TRoutineList.Create;
    try
      List.Add(Snippet);
      frmSnippets.SelectedRoutines := List;
    finally
      FreeAndNil(List);
    end;
  end;
end;

procedure TCodeExportDlg.WriteOutputFile;
  {Writes export file.
  }
var
  OutStm: TStream;  // stream that receives export file content
begin
  OutStm := TFileStream.Create(Trim(edFile.Text), fmCreate);
  try
    TCodeExporter.ExportRoutines(
      TUserInfo.CreateNul, frmSnippets.SelectedRoutines, OutStm
    );
  finally
    FreeAndNil(OutStm);
  end;
end;

end.

