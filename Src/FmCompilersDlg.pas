{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box where the user can configure the Pascal compilers
 * that are to be used by CodeSnip.
}


unit FmCompilersDlg;


interface


uses
  // Delphi
  SysUtils, ComCtrls, Controls, StdCtrls, ExtCtrls, Classes, Forms,
  // Project
  Compilers.UGlobals, FmCompilersDlg.FrBase, FmCompilersDlg.FrCompiler,
  FmCompilersDlg.FrSearchDirs, FmCompilersDlg.FrLog,
  FmCompilersDlg.FrNamespaces, FmCompilersDlg.FrSwitches,
  FmCompilersDlg.UBannerMgr, FmCompilersDlg.UCompilerListMgr, FmGenericOKDlg,
  UBaseObjects;


type
  ///  <summary>Class that implements a dialogue box where the user can
  ///  configure the Pascal compilers that are to be used by CodeSnip.</summary>
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
    tsNamespaces: TTabSheet;
    frmNamespaces: TCompilersDlgNamespacesFrame;
    ///  <summary>When Auto Detect Compilers button is clicked, sets executable
    ///  program path for each installed compiler that can detect its own path.
    ///  </summary>
    procedure btnDetectClick(Sender: TObject);
    ///  <summary>When OK button clicked, updates compilers object ready to pass
    ///  back to caller.</summary>
    procedure btnOKClick(Sender: TObject);
    ///  <summary>Initialises form's fields and sets default fonts.</summary>
    procedure FormCreate(Sender: TObject);
    ///  <summary>Tidies up form fields.</summary>
    procedure FormDestroy(Sender: TObject);
    ///  <summary>Handles event triggered when user clicks on one of page
    ///  control tabs. Ensures page control has focus.</summary>
    ///  <remarks>Without this fix, page control does not always get focus when
    ///  a tab is clicked.</remarks>
    procedure pcCompilerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  strict private
    var
      ///  <summary>Object that manages owner draw compiler list.</summary>
      fCompListMgr: TCompilerListMgr;
      ///  <summary>Object that manages drawing of banner containing selected
      ///  compiler name.</summary>
      fBannerMgr: TCompilerBannerMgr;
      ///  <summary>Reference to currently selected compiler.</summary>
      fCurCompiler: ICompiler;
      ///  <summary>Local copy of given compilers list used for editing.
      ///  </summary>
      fLocalCompilers: ICompilers;
      ///  <summary>Array of all frames displayed in dialogue box's tabbed
      ///  pages.</summary>
      fFrames: TArray<TCompilersDlgBaseFrame>;
    ///  <summary>Called when a new compiler is selected in list box. Updates
    ///  previously selected compiler with any details from controls then copies
    ///  details of newly selected compiler into controls.</summary>
    procedure CompilerSelectHandler(Sender: TObject);
    ///  <summary>Called when information on a tab page changes. Updates
    ///  selected compiler with information entered in controls.</summary>
    procedure CompilerChangeHandler(Sender: TObject);
    ///  <summary>Iterates each of the form's frames, calling the given
    ///  procedure for each frame.</summary>
    procedure IterateFrames(Proc: TProc<TCompilersDlgBaseFrame>);
    ///  <summary>Stores reference to currently selected local compiler and
    ///  updates dialogue box controls with details of the compiler.</summary>
    procedure SelectCompiler;
    ///  <summary>Updates local copy of currently selected compiler from entries
    ///  in dialogue box controls.</summary>
    procedure UpdateCurrentCompiler;
    ///  <summary>Updates all the dialogue's frames with details of the
    ///  currently selected compiler.</summary>
    procedure UpdateEditFrames;
    ///  <summary>Checks that all entered paths to compiler executable files
    ///  reference valid Windows 32 bit executables.</summary>
    function CheckCompilerExes: Boolean;
  strict protected
    ///  <summary>Initialises form's controls.</summary>
    procedure InitialiseControls; override;
    ///  <summary>Sizes and aligns controls and all of its frames.</summary>
    procedure ArrangeControls; override;
  public
    ///  <summary>Displays the dialogue box and updates given compilers object
    ///  if required.</summary>
    ///  <param name="AOwner">TComponent [in] Control that owns this dialogue
    ///  box. Dialogue box is aligned over the control if possible.</param>
    ///  <param name="ACompilers">ICompilers [in] Compilers object to be edited.
    ///  This object is modified only if the user presses the OK button.</param>
    ///  <returns>Boolean. True if user OKs and False if user cancels.</returns>
    class function Execute(AOwner: TComponent;
      const ACompilers: ICompilers): Boolean;
  end;


implementation


uses
  // Project
  Compilers.UCompilers, IntfCommon, UCtrlArranger, UExeFileType, UFontHelper,
  UMessageBox;


{$R *.dfm}


{ TCompilersDlg }

procedure TCompilersDlg.ArrangeControls;
begin
  TCtrlArranger.SetLabelHeights(Self);
  IterateFrames(
    procedure (Frame: TCompilersDlgBaseFrame)
    begin
      Frame.ArrangeControls;
    end
  );
  // size dialogue and arrange inherited controls
  inherited;
  // arrange extra button in bottom button line
  btnDetect.Left := pnlBody.Left;
  btnDetect.Top := btnHelp.Top;
end;

procedure TCompilersDlg.btnDetectClick(Sender: TObject);
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
begin
  UpdateCurrentCompiler;
  SelectCompiler;
end;

class function TCompilersDlg.Execute(AOwner: TComponent;
  const ACompilers: ICompilers): Boolean;
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
begin
  inherited;
  // Take a copy of global compilers object: stores updates until OK clicked
  fLocalCompilers := TCompilersFactory.CreateCompilers;

  fCompListMgr := TCompilerListMgr.Create(lbCompilers, fLocalCompilers);
  fCompListMgr.OnSelect := CompilerSelectHandler;

  TFontHelper.SetDefaultBaseFont(pbBanner.Font);
  fBannerMgr := TCompilerBannerMgr.Create(pbBanner);

  fFrames := TArray<TCompilersDlgBaseFrame>.Create(
    frmCompiler, frmSwitches, frmNamespaces, frmSearchDirs, frmLog
  );
  IterateFrames(
    procedure (Frame: TCompilersDlgBaseFrame)
    begin
      Frame.OnChange := CompilerChangeHandler
    end
  );
end;

procedure TCompilersDlg.FormDestroy(Sender: TObject);
begin
  SetLength(fFrames, 0);
  fBannerMgr.Free;
  fCompListMgr.Free;
  inherited;
end;

procedure TCompilersDlg.InitialiseControls;
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

procedure TCompilersDlg.pcCompilerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if htOnItem in pcCompiler.GetHitTestInfoAt(X, Y) then
    pcCompiler.SetFocus;
end;

procedure TCompilersDlg.SelectCompiler;
begin
  fCurCompiler := fCompListMgr.Selected;
  fBannerMgr.Compiler := fCurCompiler;
  UpdateEditFrames;
end;

procedure TCompilersDlg.UpdateCurrentCompiler;
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
  tsNamespaces.TabVisible := fCurCompiler.RequiresRTLNamespaces;
  IterateFrames(
    procedure(Frame: TCompilersDlgBaseFrame)
    begin
      Frame.Compiler := fCurCompiler;
    end
  );
end;

end.

