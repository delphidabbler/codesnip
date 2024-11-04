{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that gets snippets to be exported and creates an
 * export file containing the selected snippets.
}


unit FmCodeExportDlg;


interface


uses
  // Delphi
  Classes, StdCtrls, Controls, Forms, ExtCtrls,
  // Project
  DB.USnippet, FmGenericOKDlg, FrCheckedTV, FrSelectUserSnippets,
  FrSelectSnippets, FrSelectSnippetsBase, UBaseObjects;


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
    lblSnippets: TLabel;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  strict private
    procedure SelectSnippet(const Snippet: TSnippet);
      {Selects a snippet in the snippets check list.
        @param Snippet [in] Snippet to be selected. If nil, or not user-defined,
          no snippet is selected.
      }
    procedure WriteOutputFile;
      {Writes export file.
      }
  strict protected
    procedure ConfigForm; override;
    procedure ArrangeForm; override;
      {Aligns controls vertically where necessary to accomodate height of
      controls that depend on UI font.
      }
  public
    class procedure Execute(const AOwner: TComponent; const Snippet: TSnippet);
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
  DB.UCollections,
  UCodeImportExport, UCtrlArranger, UEncodings, UExceptions, UIOUtils,
  UMessageBox, UOpenDialogHelper, USaveDialogEx, UStrUtils, UUtils;


{$R *.dfm}

{ TCodeExportDlg }

procedure TCodeExportDlg.ArrangeForm;
  {Aligns controls vertically where necessary to accomodate height of controls
  that depend on UI font.
  }
begin
  frmSnippets.Top := TCtrlArranger.BottomOf(lblSnippets, 4);
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
  sNoSnippets = 'Please select one or more snippets';
  sNoFileName = 'Please specify a file name';
  sBadPath = 'The specified file path does not exist';
  sFileExists = 'Please specify another file name';
  // Confirmation message
  sOverwriteFile = '%s already exists. OK to overwrite?';
var
  FileName: string;   // name of export file
begin
  FileName := StrTrim(edFile.Text);
  // Assume failure
  ModalResult := mrNone;
  try

    // Validate entries
    // must have at least one snippet
    if frmSnippets.SelectedSnippets.IsEmpty then
      raise EDataEntry.Create(sNoSnippets, frmSnippets);
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

procedure TCodeExportDlg.ConfigForm;
begin
  inherited;
  frmSnippets.CanCollapse := True;
end;

class procedure TCodeExportDlg.Execute(const AOwner: TComponent;
  const Snippet: TSnippet);
  {Displays export dialog box and writes export file if user OKs entries.
    @param AOwner [in] Reference to control that owns the dialog box.
    @param Snippet [in] Reference to a snippet to pre-select in snippets check
      list box. If nil or not user-defined then no snippet is pre-selected.
  }
var
  Dlg: TCodeExportDlg;
begin
  Dlg := InternalCreate(AOwner);
  try
    Dlg.SelectSnippet(Snippet);
    Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

procedure TCodeExportDlg.SelectSnippet(const Snippet: TSnippet);
  {Selects a snippet in the snippets check list.
    @param Snippet [in] Snippet to be selected. If nil, or not user-defined, no
      snippet is selected.
  }
var
  List: TSnippetList; // list containing only the provided snippet
begin
  if not Assigned(Snippet) or (Snippet.CollectionID = TCollectionID.__TMP__MainDBCollectionID) then
    // Snippet is nil or not user-defined: select nothing
    frmSnippets.SelectedSnippets := nil
  else
  begin
    // Snippet is user-defined. We make a snippet list containing only this
    // snippet because frmSnippets requires a list of snippets to select.
    List := TSnippetList.Create;
    try
      List.Add(Snippet);
      frmSnippets.SelectedSnippets := List;
    finally
      FreeAndNil(List);
    end;
  end;
end;

procedure TCodeExportDlg.WriteOutputFile;
  {Writes export file.
  }
var
  OutData: TEncodedData;  // receives export file content
begin
  OutData := TCodeExporter.ExportSnippets(frmSnippets.SelectedSnippets);
  TFileIO.WriteAllBytes(StrTrim(edFile.Text), OutData.Data);
end;

end.

