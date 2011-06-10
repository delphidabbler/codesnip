{
 * FmCompilersDlg.pas
 *
 * Implements a dialog box where the user can configure which Pascal compilers
 * installed on the local system can be used by CodeSnip.
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
 * The Original Code is FmCompilersDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmCompilersDlg;


interface


uses
  // Delphi
  SysUtils, ComCtrls, Controls, StdCtrls, ExtCtrls, Classes, Forms,
  // Project
  Compilers.UGlobals, FmCompilersDlg.FrBase, FmCompilersDlg.FrCompiler,
  FmCompilersDlg.FrSearchDirs, FmCompilersDlg.FrLog, FmCompilersDlg.FrSwitches,
  FmCompilersDlg.UBannerMgr, FmCompilersDlg.UCompilerListMgr, FmGenericOKDlg,
  UBaseObjects;


type
  {
  TCompilersDlg:
    Implements a dialog box where the user can configure which Pascal compilers
    installed on the local system can be used by CodeSnip.
  }
  TCompilersDlg = class(TGenericOKDlg, INoPublicConstruct)
    btnDetect: TButton;
    lbCompilers: TListBox;
    pbBanner: TPaintBox;
    pcCompiler: TPageControl;
    tsCompiler: TTabSheet;
    tsLog: TTabSheet;
    tsSwitches: TTabSheet;
    frmCompiler: TCompilersDlgCompilerFrame;
    frmSwitches: TCompilersDlgSwitchesFrame;
    frmLog: TCompilersDlgLogFrame;
    tsSearchDirs: TTabSheet;
    frmSearchDirs: TCompilersDlgSearchDirsFrame;
    procedure btnDetectClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  strict private
    var
      fCompListMgr: TCompilerListMgr;
      fBannerMgr: TCompilerBannerMgr;
      fCurCompiler: ICompiler;      // Reference to currently selected compiler
      fLocalCompilers: ICompilers;  // Copy of Compilers that is edited
      fFrames: TArray<TCompilersDlgBaseFrame>;
    procedure CompilerSelectHandler(Sender: TObject);
    procedure CompilerChangeHandler(Sender: TObject);
    procedure IterateFrames(Proc: TProc<TCompilersDlgBaseFrame>);
    procedure SelectCompiler;
      {Stores reference to currently selected local compiler and updates dialog
      box controls with details of the compiler.
      }
    procedure UpdateCurrentCompiler;
      {Updates local copy of currently selected compiler with entries in dialog
      box.
      }
    procedure UpdateEditFrames;
    function CheckCompilerExes: Boolean;
      {Checks that all paths assigned as executable files for compilers are
      valid Windows 32 executables.
        @return True if all compiler exes are valid, False if an error is found.
      }
  strict protected
    procedure InitForm; override;
      {Populates and initialises controls.
      }
    procedure ArrangeForm; override;
      {Dynamically sizes and aligns controls to allow for Vista UI font. Also
      adjusts position of "Auto Detect Compilers" button on bottom button line.
      }
  public
    class function Execute(AOwner: TComponent;
      const ACompilers: ICompilers): Boolean;
      {Displays the dialog box. The dialog updates a compilers object if user
      OKs
        @param AOwner [in] Control that owns this dialog.
        @param ACompilers [in] Compilers object to be updated.
        @return True if user OKs and compiler information is updated or False
          if user cancels and compiler information is unchanged.
      }
  end;


implementation


uses
  // Project
  Compilers.UCompilers, IntfCommon, UCtrlArranger, UExeFileType, UMessageBox;


{$R *.dfm}


{ TCompilersDlg }

procedure TCompilersDlg.ArrangeForm;
  {Dynamically sizes and aligns controls to allow for Vista UI font. Also
  adjusts position of "Auto Detect Compilers" button on bottom button line.
  }
begin
  TCtrlArranger.SetLabelHeights(Self);
  IterateFrames(
    procedure (Frame: TCompilersDlgBaseFrame)
    begin
      Frame.ArrangeControls;
    end
  );
  // size dialog and arrange inherited controls
  inherited;
  // arrange extra button in bottom button line
  btnDetect.Left := pnlBody.Left;
  btnDetect.Top := btnHelp.Top;
end;

procedure TCompilersDlg.btnDetectClick(Sender: TObject);
  {Handles click on Auto Detect Compilers button. Sets executbale program path
  for each compiler present that can detect its own path.
    @param Sender [in] Not used.
  }
var
  Compiler: ICompiler;  // refers to each compiler
resourcestring
  // Text displayed in confirmation box
  sOKToDetect = 'Detected compiler file names will overwrite any existing '
    + 'paths. Do you wish to continue?';
begin
  if not TMessageBox.Confirm(Self, sOKToDetect) then
    Exit;
  // Record any changes to current compiler
  UpdateCurrentCompiler;
  // Loop thru all compilers attempting to detect exe files
  for Compiler in fLocalCompilers do
  begin
    if Supports(Compiler, ICompilerAutoDetect) then
    begin
      if (Compiler as ICompilerAutoDetect).DetectExeFile then
      begin
        // Update currently displayed compiler details
        if Compiler.GetID = fCurCompiler.GetID then
          UpdateEditFrames;
      end;
    end;
  end;
  // Redisplay compiler list and current compiler title to reflect any changes
  fCompListMgr.Refresh;
  fBannerMgr.Refresh;
end;

procedure TCompilersDlg.btnOKClick(Sender: TObject);
  {Handles OK button click. Update globals Compilers object and saves details to
  persistent storage.
    @param Sender [in] Not used.
  }
begin
  inherited;
  // Ensure compiler object is up to date
  UpdateCurrentCompiler;
  // Check assigned exe files
  if not CheckCompilerExes then
  begin
    ModalResult := mrNone;
    Exit;
  end;
  ModalResult := mrOK;
end;

function TCompilersDlg.CheckCompilerExes: Boolean;
  {Checks that all paths assigned as executable files for compilers are valid
  Windows 32 executables.
    @return True if all compiler exes are valid, False if an error is found.
  }
var
  Compiler: ICompiler;  // refers to each compiler
resourcestring
  // Error messages
  sFileDoesNotExist = 'File specified for %s compiler doesn''t exist.';
  sFileNotExe = 'File specified for %s compiler is not a valid executable file';
begin
  Result := False;
  // Scan through compilers to see if paths are valid
  for Compiler in fLocalCompilers do
  begin
    if Compiler.GetExecFile = '' then
      Continue;
    if not FileExists(Compiler.GetExecFile) then
    begin
      TMessageBox.Error(Self, Format(sFileDoesNotExist, [Compiler.GetName]));
      Exit;
    end;
    if ExeFileType(Compiler.GetExecFile) <> fkExe32 then
    begin
      TMessageBox.Error(Self, Format(sFileNotExe, [Compiler.GetName]));
      Exit;
    end;
  end;
  Result := True;
end;

procedure TCompilersDlg.CompilerChangeHandler(Sender: TObject);
begin
  UpdateCurrentCompiler;
end;

procedure TCompilersDlg.CompilerSelectHandler(Sender: TObject);
  {Handles a compiler selection in list box. Updates compiler that is being
  deselected with details in dialog box controls then displays details of newly
  selected compiler.
    @param Sender [in] Not used.
  }
begin
  UpdateCurrentCompiler;
  SelectCompiler;
end;

class function TCompilersDlg.Execute(AOwner: TComponent;
  const ACompilers: ICompilers): Boolean;
  {Displays the dialog box. The dialog updates a compilers object if user OKs
    @param AOwner [in] Control that owns this dialog.
    @param ACompilers [in] Compilers object to be updated.
    @return True if user OKs and compiler information is updated or False if
      user cancels and compiler information is unchanged.
  }
var
  Persister: IPersistCompilers; // object used to save object to storage
begin
  with InternalCreate(AOwner) do
    try
      (fLocalCompilers as IAssignable).Assign(ACompilers);
      Result := ShowModal = mrOK;
      if Result then
      begin
        (ACompilers as IAssignable).Assign(fLocalCompilers);
        Persister := TPersistCompilers.Create;
        Persister.Save(ACompilers);
      end;
    finally
      Free;
    end;
end;

procedure TCompilersDlg.FormCreate(Sender: TObject);
  {Initialises compiler information and creates owned objects.
    @param Sender [in] Not used.
  }
begin
  inherited;
  // Take a copy of global compilers object: stores updates until OK clicked
  fLocalCompilers := TCompilersFactory.CreateCompilers;

  fCompListMgr := TCompilerListMgr.Create(lbCompilers, fLocalCompilers);
  fCompListMgr.OnSelect := CompilerSelectHandler;

  fBannerMgr := TCompilerBannerMgr.Create(pbBanner);

  fFrames := TArray<TCompilersDlgBaseFrame>.Create(
    frmCompiler, frmSwitches, frmSearchDirs, frmLog
  );
  IterateFrames(
    procedure (Frame: TCompilersDlgBaseFrame)
    begin
      Frame.OnChange := CompilerChangeHandler
    end
  );
end;

procedure TCompilersDlg.FormDestroy(Sender: TObject);
  {Frees owned objects.
    @param Sender [in] Not used.
  }
begin
  SetLength(fFrames, 0);
  fBannerMgr.Free;
  fCompListMgr.Free;
  inherited;
end;

procedure TCompilersDlg.InitForm;
  {Populates and initialises controls.
  }
begin
  inherited;
  fCompListMgr.Initialise;
end;

procedure TCompilersDlg.IterateFrames(Proc: TProc<TCompilersDlgBaseFrame>);
var
  Frame: TCompilersDlgBaseFrame;
begin
  for Frame in fFrames do
    Proc(Frame);
end;

procedure TCompilersDlg.SelectCompiler;
  {Stores reference to currently selected local compiler and updates dialog box
  controls with details of the compiler.
  }
begin
  fCurCompiler := fCompListMgr.Selected;
  fBannerMgr.Compiler := fCurCompiler;
  UpdateEditFrames;
end;

procedure TCompilersDlg.UpdateCurrentCompiler;
  {Updates local copy of currently selected compiler with entries in dialog box.
  }
var
  WasAvailable: Boolean;        // whether compiler was available before update
begin
  if Assigned(fCurCompiler) then
  begin
    // Record whether compiler was available before update
    WasAvailable := fCurCompiler.IsAvailable;
    // Update compiler from info entered in frames
    IterateFrames(
      procedure(Frame: TCompilersDlgBaseFrame)
      begin
        Frame.UpdateCompiler;
      end
    );
    // If availability has changed redraw selected list item and compiler title
    // to give visual feedback of changed state
    if WasAvailable <> fCurCompiler.IsAvailable then
    begin
      fCompListMgr.Refresh(fCurCompiler);
      fBannerMgr.Refresh;
    end;
  end;
end;

procedure TCompilersDlg.UpdateEditFrames;
begin
  IterateFrames(
    procedure(Frame: TCompilersDlgBaseFrame)
    begin
      Frame.Compiler := fCurCompiler;
    end
  );
end;

end.

