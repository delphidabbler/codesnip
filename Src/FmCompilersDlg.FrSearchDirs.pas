{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2011-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a frame used to edit lists of search directories used for a
 * compiler being edited in TCompilersDlg.
}


unit FmCompilersDlg.FrSearchDirs;


interface


uses
  // Delphi
  Classes, ActnList, ImgList, Controls, StdCtrls, Buttons,
  // Project
  Compilers.UGlobals, FmCompilersDlg.FrBase;


type
  ///  <summary>Frame used to edit search directories for the compiler being
  ///  edited in TCompilersDlg.</summary>
  TCompilersDlgSearchDirsFrame = class(TCompilersDlgBaseFrame)
    lbPaths: TListBox;
    edPath: TEdit;
    btnBrowse: TButton;
    btnAdd: TButton;
    lblPaths: TLabel;
    lblPath: TLabel;
    btnDelete: TButton;
    btnUp: TSpeedButton;
    btnDown: TSpeedButton;
    btnReplace: TButton;
    ilActions: TImageList;
    alFrame: TActionList;
    actUp: TAction;
    actDown: TAction;
    actBrowse: TAction;
    actAdd: TAction;
    actReplace: TAction;
    actDelete: TAction;
    ///  <summary>Promotes currently selected list item.</summary>
    procedure actUpExecute(Sender: TObject);
    ///  <summary>Enables / disables "up" (promote) action.</summary>
    procedure actUpUpdate(Sender: TObject);
    ///  <summary>Demotes currently selected list item.</summary>
    procedure actDownExecute(Sender: TObject);
    ///  <summary>Enables / disables "down" (demote) action.</summary>
    procedure actDownUpdate(Sender: TObject);
    ///  <summary>Gets a directory from user using "browse for folder" dialogue
    ///  box.</summary>
    procedure actBrowseExecute(Sender: TObject);
    ///  <summary>Adds directory entered in edit control to list box.</summary>
    procedure actAddExecute(Sender: TObject);
    ///  <summary>Enables / disables "add" action.</summary>
    procedure actAddUpdate(Sender: TObject);
    ///  <summary>Replaces currently selected list item with directory entered
    ///  in edit control.</summary>
    procedure actReplaceExecute(Sender: TObject);
    ///  <summary>Enables / disables "replace" action.</summary>
    procedure actReplaceUpdate(Sender: TObject);
    ///  <summary>Deletes currently selected list box item.</summary>
    procedure actDeleteExecute(Sender: TObject);
    ///  <summary>Enables / disables "delete" action.</summary>
    procedure actDeleteUpdate(Sender: TObject);
    ///  <summary>Handles clicks in list box. Copies selected item to edit
    ///  control.</summary>
    procedure lbPathsClick(Sender: TObject);
  strict private
    var
      ///  <summary>List of search directories from current compiler.</summary>
      fSearchDirs: ISearchDirs;
    ///  <summary>Gets directory entered in list box.</summary>
    ///  <remarks>Trims leading and trailing whitespace and removes trailing
    ///  path delimiters.</remarks>
    function PathFromEdit: string;
    ///  <summary>Checks if list box contains an entry that is the same as the
    ///  directory entered in edit control.</summary>
    function ListContainsPathFromEdit: Boolean;
    ///  <summary>Selects the given list item in list box.</summary>
    procedure SelectListItem(const Idx: Integer);
    ///  <summary>Copies directory name of selected list item to edit control.
    ///  </summary>
    procedure CopySelectedItemToEdit;
  strict protected
    ///  <summary>Initialises frame to display details of current compiler.
    ///  </summary>
    procedure Initialise; override;
  public
    constructor Create(AOwner: TComponent); override;
    ///  <summary>Arranges controls in frame.</summary>
    procedure ArrangeControls; override;
    ///  <summary>Updates current compiler object with edited information.
    ///  </summary>
    procedure UpdateCompiler; override;
  end;


implementation


uses
  // Delphi
  SysUtils, Windows, Graphics,
  // Project
  UBrowseForFolderDlg, UClassHelpers, UCtrlArranger, UStrUtils;

{$R *.dfm}


{ TCompilersDlgSearchDirsFrame }

procedure TCompilersDlgSearchDirsFrame.actAddExecute(Sender: TObject);
var
  NewItemIdx: Integer;  // index of new list item
begin
  NewItemIdx := lbPaths.Items.Add(PathFromEdit);
  SelectListItem(NewItemIdx);
end;

procedure TCompilersDlgSearchDirsFrame.actAddUpdate(Sender: TObject);
begin
  actAdd.Enabled := (PathFromEdit <> '') and not ListContainsPathFromEdit;
end;

procedure TCompilersDlgSearchDirsFrame.actBrowseExecute(Sender: TObject);
var
  Dlg: TBrowseForFolderDlg; // browse for folder standard dialogue box
resourcestring
  sDlgHeading = 'Choose search directory';
begin
  Dlg := TBrowseForFolderDlg.Create(nil);
  try
    Dlg.Headline := sDlgHeading;
    if Dlg.Execute then
      edPath.Text := Dlg.FolderName;
  finally
    Dlg.Free;
  end;
end;

procedure TCompilersDlgSearchDirsFrame.actDeleteExecute(Sender: TObject);
var
  Selected: Integer;  // index of selected list item
begin
  Selected := lbPaths.ItemIndex;
  Assert(Selected >= 0);
  lbPaths.Items.Delete(Selected);
  if Selected < lbPaths.Count then
    SelectListItem(Selected)
  else
    SelectListItem(Selected - 1);
end;

procedure TCompilersDlgSearchDirsFrame.actDeleteUpdate(Sender: TObject);
begin
  actDelete.Enabled := lbPaths.ItemIndex >= 0;
end;

procedure TCompilersDlgSearchDirsFrame.actDownExecute(Sender: TObject);
var
  Selected: Integer;  // index of selected list item
begin
  Selected := lbPaths.ItemIndex;
  Assert((Selected >= 0) and (Selected < Pred(lbPaths.Count)));
  lbPaths.Items.Exchange(Selected, Selected + 1);
  SelectListItem(Selected + 1);
end;

procedure TCompilersDlgSearchDirsFrame.actDownUpdate(Sender: TObject);
begin
  actDown.Enabled := (lbPaths.ItemIndex >= 0)
    and (lbPaths.ItemIndex < Pred(lbPaths.Count));
end;

procedure TCompilersDlgSearchDirsFrame.actReplaceExecute(Sender: TObject);
begin
  lbPaths.Items[lbPaths.ItemIndex] := PathFromEdit;
end;

procedure TCompilersDlgSearchDirsFrame.actReplaceUpdate(Sender: TObject);
begin
  actReplace.Enabled := (PathFromEdit <> '') and not ListContainsPathFromEdit
    and (lbPaths.ItemIndex >= 0);
end;

procedure TCompilersDlgSearchDirsFrame.actUpExecute(Sender: TObject);
var
  Selected: Integer;  // index of selected list item
begin
  Selected := lbPaths.ItemIndex;
  Assert(Selected >= 1);
  lbPaths.Items.Exchange(Selected, Selected - 1);
  SelectListItem(Selected - 1);
end;

procedure TCompilersDlgSearchDirsFrame.actUpUpdate(Sender: TObject);
begin
  actUp.Enabled := lbPaths.ItemIndex >= 1;
end;

procedure TCompilersDlgSearchDirsFrame.ArrangeControls;
begin
  TCtrlArranger.SetLabelHeights(Self);
  lbPaths.Top := TCtrlArranger.BottomOf(lblPaths, 4);
  btnUp.Top := lbPaths.Top;
  btnDown.Top := lbPaths.Top + lbPaths.Height - btnDown.Height;
  lblPath.Top := TCtrlArranger.BottomOf(lbPaths, 8);
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(lblPath, 4), [edPath, btnBrowse]
  );
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf([edPath, btnBrowse], 8),
    [btnAdd, btnDelete, btnReplace]
  );
end;

procedure TCompilersDlgSearchDirsFrame.CopySelectedItemToEdit;
begin
  if lbPaths.ItemIndex >= 0 then
    edPath.Text := lbPaths.Items[lbPaths.ItemIndex]
  else
    edPath.Text := '';
end;

constructor TCompilersDlgSearchDirsFrame.Create(AOwner: TComponent);
begin
  inherited;
  ilActions.LoadFromResource(RT_RCDATA, 'ACTIONIMAGES', 16, clFuchsia);
  RefreshActions;
end;

procedure TCompilersDlgSearchDirsFrame.Initialise;
var
  SearchDir: string;  // each directory in search directories list
begin
  fSearchDirs := Compiler.GetSearchDirs;
  lbPaths.Clear;
  for SearchDir in fSearchDirs do
    lbPaths.Items.Add(SearchDir);
end;

procedure TCompilersDlgSearchDirsFrame.lbPathsClick(Sender: TObject);
begin
  CopySelectedItemToEdit;
end;

function TCompilersDlgSearchDirsFrame.ListContainsPathFromEdit: Boolean;
begin
  Result := lbPaths.Items.IndexOf(PathFromEdit) >= 0;
end;

function TCompilersDlgSearchDirsFrame.PathFromEdit: string;
begin
  Result := ExcludeTrailingPathDelimiter(StrTrim(edPath.Text));
end;

procedure TCompilersDlgSearchDirsFrame.SelectListItem(const Idx: Integer);
begin
  lbPaths.ItemIndex := Idx;
  CopySelectedItemToEdit;
end;

procedure TCompilersDlgSearchDirsFrame.UpdateCompiler;
var
  SearchDir: string;  // each directory in search directories list
begin
  fSearchDirs.Clear;
  for SearchDir in lbPaths.Items do
    fSearchDirs.Add(SearchDir);
  Compiler.SetSearchDirs(fSearchDirs);
end;

end.

