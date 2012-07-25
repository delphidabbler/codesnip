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
 * Portions created by the Initial Developer are Copyright (C) 2005-2012 Peter
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
  Grids, ValEdit, StdCtrls, ComCtrls, Controls, ExtCtrls, Classes, Windows,
  // Project
  Compilers.UGlobals, FmGenericOKDlg, UBaseObjects;


type

  {
  TCompilersDlg:
    Implements a dialog box where the user can configure which Pascal compilers
    installed on the local system can be used by CodeSnip.
  }
  TCompilersDlg = class(TGenericOKDlg, INoPublicConstruct)
    btnAdd: TButton;
    btnBrowse: TButton;
    btnClear: TButton;
    btnDefSwitches: TButton;
    btnDelete: TButton;
    btnDetect: TButton;
    btnReplace: TButton;
    edCompilerPath: TEdit;
    edSwitch: TEdit;
    lbCompilers: TListBox;
    lblExplainSwitches: TLabel;
    lblCompilerPath: TLabel;
    lblLogPrefixes: TLabel;
    lblSwitch: TLabel;
    lblSwitches: TLabel;
    lbSwitches: TListBox;
    pbCompiler: TPaintBox;
    pcCompiler: TPageControl;
    tsExecFile: TTabSheet;
    tsOutputLog: TTabSheet;
    tsSwitches: TTabSheet;
    vleLogPrefixes: TValueListEditor;
    procedure btnAddClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnDefSwitchesClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnDetectClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure edCompilerPathExit(Sender: TObject);
    procedure edSwitchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbCompilersClick(Sender: TObject);
    procedure lbCompilersDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lbSwitchesClick(Sender: TObject);
    procedure pbCompilerPaint(Sender: TObject);
    procedure vleLogPrefixesDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure vleLogPrefixesSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure pcCompilerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  strict private
    fCurCompiler: ICompiler;      // Reference to currently selected compiler
    fLocalCompilers: ICompilers;  // Copy of Compilers that is edited
    procedure SelectCompiler;
      {Stores reference to currently selected local compiler and updates dialog
      box controls with details of the compiler.
      }
    procedure UpdateCurrentCompiler;
      {Updates local copy of currently selected compiler with entries in dialog
      box.
      }
    procedure PopulateSwitchList(const Switches: string);
      {Stores list of command line switches in list box.
        @param Switches [in] Comma separated list of required switches.
      }
    procedure UpdateSwitchButtons;
      {Updates state of buttons that manipulate command line switches depending
      on state of switch edit and list boxes.
      }
    procedure CanOpenDialogClose(Sender: TObject; var CanClose: Boolean);
      {Handles open dialog box's OnCanClose event. Prevents dialog from closing
      if selected file does not exist or if it is not an executable file.
        @param Sender [in] Reference to dialog box that triggered event. Must be
          of type TOpenDialogEx.
        @param CanClose [in/out] Flag that determines if dialog can close. Set
          to False to prevent closure or True to permit it.
      }
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
  // Delphi
  SysUtils, Graphics, GraphUtil, Dialogs,
  // Project
  Compilers.UCompilers, IntfCommon, UCtrlArranger, UExeFileType, UGraphicUtils,
  UMessageBox, UOpenDialogEx, UOpenDialogHelper, UStructs, UThemesEx, UUtils;


{$R *.dfm}


{ TCompilersDlg }

procedure TCompilersDlg.ArrangeForm;
  {Dynamically sizes and aligns controls to allow for Vista UI font. Also
  adjusts position of "Auto Detect Compilers" button on bottom button line.
  }
var
  RowIdx: Integer;    // loops through rows of vleLogPrefixes
  CellSize: TSize;    // size of left hand cell in vleLogPrefixes
begin
  TCtrlArranger.SetLabelHeights(Self);

  // tsExecFile
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(lblCompilerPath, 4), [edCompilerPath, btnBrowse]
  );
  btnClear.Top := TCtrlArranger.BottomOf([edCompilerPath, btnBrowse], 8);

  // tsSwitches
  edSwitch.Top := TCtrlArranger.BottomOf(lblSwitch, 4);
  lblSwitches.Top := TCtrlArranger.BottomOf(edSwitch, 8);
  lblExplainSwitches.Top := TCtrlArranger.BottomOf(btnDelete, 8);
  lbSwitches.Top := lblExplainSwitches.Top;
  lblSwitches.Top := lbSwitches.Top - lblSwitches.Height - 4;

  // tsOutputLog
  vleLogPrefixes.Top := TCtrlArranger.BottomOf(lblLogPrefixes, 4);
  // size rows and columns in value editor
  for RowIdx := 0 to Pred(vleLogPrefixes.RowCount) do
  begin
    CellSize := StringExtent(vleLogPrefixes.Keys[RowIdx], vleLogPrefixes.Font);
    if CellSize.cx > vleLogPrefixes.ColWidths[0] then
      vleLogPrefixes.ColWidths[0] := CellSize.cx;
    if CellSize.cy > vleLogPrefixes.RowHeights[RowIdx] then
      vleLogPrefixes.RowHeights[RowIdx] := CellSize.cy;
  end;
  vleLogPrefixes.ColWidths[0] := vleLogPrefixes.ColWidths[0] + 16;
  vleLogPrefixes.ColWidths[1] :=
    vleLogPrefixes.Width - vleLogPrefixes.ColWidths[0];

  // size dialog and arrange inherited controls
  inherited;

  // arrange Auto-detect button in bottom button line
  btnDetect.Left := pnlBody.Left;
  btnDetect.Top := btnHelp.Top;
end;

procedure TCompilersDlg.btnAddClick(Sender: TObject);
  {Handles click on Add button on Command Line tab. Adds any new switch in edit
  box to list.
    @param Sender [in] Not used.
  }
begin
  lbSwitches.Items.Add(Trim(edSwitch.Text));
  edSwitch.Text := '';
  UpdateSwitchButtons;
end;

procedure TCompilersDlg.btnBrowseClick(Sender: TObject);
  {Handles a click on the Browse button on Compiler tab. Displays open files
  dialog and copies name of any selected file into the compiler path edit box.
    @param Sender [in] Not used.
  }
var
  OpenDlg: TOpenDialogEx; // self-aligning enhanced open dialog box
  DefFileName: string;    // default file name passed to open dialog box
resourcestring
  sFilter = 'Executable files (*.exe)|*.exe|' // file filter
    + 'All files (*.*)|*.*';
  sTitle = 'Select Compiler';                 // dialog box title
begin
  // Create and initialise
  OpenDlg := TOpenDialogEx.Create(Self);
  try
    OpenDlg.OnCanClose := CanOpenDialogClose;
    OpenDlg.Filter := sFilter;
    OpenDlg.FilterIndex := 1;
    OpenDlg.InitialDir := '';
    // we don't include ofFileMustExist in Options below since we handle
    // non-existant files ourselves
    // we don't include ofShowHelp since the dialog box automatically displays
    // help if HelpKeyword property is set.
    OpenDlg.Options := [ofHideReadOnly, ofEnableSizing];
    OpenDlg.OptionsEx := [];
    OpenDlg.Title := sTitle;
    OpenDlg.HelpKeyword := 'SelectCompilerDlg';
    // if we have a compiler path use it as default if it exists
    DefFileName := edCompilerPath.Text;
    if FileExists(DefFileName) then
      OpenDlg.FileName := DefFileName;
    if OpenDlg.Execute then
    begin
      // User OKd: use entered file name
      edCompilerPath.Text := OpenDlg.FileName;
      UpdateCurrentCompiler;
    end;
  finally
    FreeAndNil(OpenDlg);
  end;
end;

procedure TCompilersDlg.btnClearClick(Sender: TObject);
  {Handles click on Clear button on Compiler tab. Clears file name.
    @param Sender [in] Not used.
  }
begin
  edCompilerPath.Text := '';
  UpdateCurrentCompiler;
end;

procedure TCompilersDlg.btnDefSwitchesClick(Sender: TObject);
  {Handles click on Defaults button on Command Line tab. Restores command line
  switches to default for compiler.
    @param Sender [in] Not used.
  }
begin
  PopulateSwitchList(fCurCompiler.GetDefaultSwitches);
  UpdateSwitchButtons;
end;

procedure TCompilersDlg.btnDeleteClick(Sender: TObject);
  {Handles click on Delete button on Command Line tab. Deletes selected switch.
    @param Sender [in] Not used.
  }
begin
  Assert(lbSwitches.ItemIndex >= 0,
    ClassName + '.btnDeleteClick: lbSwitches.ItemIndex < 0');
  lbSwitches.Items.Delete(lbSwitches.ItemIndex);
  UpdateSwitchButtons;
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
  // Loop thru all compilers attempting to detect exe files
  for Compiler in fLocalCompilers do
  begin
    if Supports(Compiler, ICompilerAutoDetect) then
    begin
      if (Compiler as ICompilerAutoDetect).DetectExeFile then
      begin
        // Display file name in edit box if compiler is displayed
        if Compiler.GetID = fCurCompiler.GetID then
          edCompilerPath.Text := Compiler.GetExecFile;
        UpdateCurrentCompiler;
      end;
    end;
  end;
  // Redisplay compiler list and current compiler title to reflect any changes
  lbCompilers.Invalidate;
  pbCompiler.Refresh;
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

procedure TCompilersDlg.btnReplaceClick(Sender: TObject);
  {Handles click on Replace button on Command Line tab. Replaces selected switch
  with new value.
    @param Sender [in] Not used.
  }
begin
  Assert(lbSwitches.ItemIndex >= 0,
    ClassName + '.btnReplaceClick: lbSwitches.ItemIndex < 0');
  lbSwitches.Items[lbSwitches.ItemIndex] := Trim(edSwitch.Text);
  edSwitch.Text := '';
  UpdateSwitchButtons;
end;

procedure TCompilersDlg.CanOpenDialogClose(Sender: TObject;
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
  FileSpec: string;   // name of file entered or selected in dialog box
resourcestring
  // Error messages
  sFileDoesNotExist = '"%s" does not exist.';
  sFileNotExe = '"%s" is not an executable file.';
begin
  Dlg := Sender as TOpenDialogEx;
  // Assume dialog can't close
  CanClose := False;
  // Get file name from dialog, allowing for users editing
  FileSpec := FileOpenEditedFileName(Dlg);
  // Check that file exists
  if not FileExists(FileSpec) then
  begin
    TMessageBox.Error(Dlg, Format(sFileDoesNotExist, [FileSpec]));
    Exit;
  end;
  // Check file is valid exe file
  if ExeFileType(FileSpec) <> fkExe32 then
  begin
    TMessageBox.Error(Dlg, Format(sFileNotExe, [FileSpec]));
    Exit;
  end;
  // Everything's OK: permit closure
  CanClose := True;
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

procedure TCompilersDlg.edCompilerPathExit(Sender: TObject);
  {Updates state of selected compiler when cursor leaves compiler edit control.
    @param Sender [in] Not used.
  }
begin
  UpdateCurrentCompiler;
end;

procedure TCompilersDlg.edSwitchChange(Sender: TObject);
  {Handles changes in switch edit box on Command Line tab. Updates tab's
  controls.
    @param Sender [in] Not used.
  }
begin
  UpdateSwitchButtons;
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
  {Initialises compiler information when form is created.
  }
begin
  inherited;
  // Take a copy of global compilers object: stores updates until OK clicked
  fLocalCompilers := TCompilersFactory.CreateCompilers;
end;

procedure TCompilersDlg.InitForm;
  {Populates and initialises controls.
  }
var
  CompID: TCompilerID;  // loops thru supported compilers
begin
  inherited;
  // Add empty list items: one per supported compiler
  // (note we don't need item text since we handle drawing of dialog ourselves
  // and get display details from compiler objects
  for CompID := Low(TCompilerID) to High(TCompilerID) do
    lbCompilers.Items.Add('');
  // Select first compiler in list and display its details in dialog controls
  lbCompilers.ItemIndex := 0;
  SelectCompiler;
end;

procedure TCompilersDlg.lbCompilersClick(Sender: TObject);
  {Handles a click on compilers list. Updates compiler that is being deselected
  with details in dialog box controls then displays details of newly selected
  compiler.
    @param Sender [in] Not used.
  }
begin
  UpdateCurrentCompiler;
  SelectCompiler;
end;

procedure TCompilersDlg.lbCompilersDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
  {Handles draw item event for compilers list box. We display compiler name and
  any associated glyph. Compilers that a available for use by CodeSnip appear in
  bold. A separator line appears between each list item.
    @param Control [in] Reference to list box.
    @param Index [in] Index of list item being drawn.
    @param Rect [in] Bounding rectangle of list item being drawn.
    @param State [in] State of control: focussed, selected etc.
  }
var
  LB: TListBox;         // list box being drawn
  TxtExtent: TSize;     // extent of text to be displayed
  ItemRect: TRectEx;    // extended rectangle bounding the item being drawn
  ImgRect: TRectEx;     // bounding rectangle of any glyph
  TxtRect: TRectEx;     // bounding rectangle of text to be drawn
  Bmp: TBitmap;         // reference to bitmap to be displayed (or nil if none)
  DrawHeight: Integer;  // total height of drawing (text below bitmap)
  Compiler: ICompiler;  // reference to compiler associated with list item
begin
  // Copy item rectangle as extended rect
  ItemRect := Rect;

  // Get reference to list box control
  LB := Control as TListBox;

  // Get reference to compiler object associated with list item and its bitmap
  Compiler := fLocalCompilers[TCompilerID(Index)];
  Bmp := Compiler.GetGlyph;

  // Set font style: bold if compiler available
  if Compiler.IsAvailable then
    LB.Canvas.Font.Style := [fsBold]
  else
    LB.Canvas.Font.Style := [];

  // Calculate display rectangles for text and any bitmap
  TxtExtent := LB.Canvas.TextExtent(Compiler.GetName);
  if Assigned(Bmp) then
  begin
    // Bitmap included: bitmap drawn above text and bounding box or both centred
    // horizontally and vertically
    DrawHeight := TxtExtent.cy + 2 + Bmp.Height;
    ImgRect := TRectEx.CreateBounds(
      (ItemRect.Left + ItemRect.Right - Bmp.Width) div 2,
      (ItemRect.Top + ItemRect.Bottom - DrawHeight) div 2,
      Bmp.Width,
      Bmp.Height
    );
    TxtRect := TRectEx.CreateBounds(
      (ItemRect.Left + ItemRect.Right - TxtExtent.cx) div 2,
      ImgRect.Bottom + 2,
      TxtExtent
    );
  end
  else
  begin
    // No bitmap: text centred vertically and horizontally
    TxtRect := TRectEx.CreateBounds(
      (ItemRect.Left + ItemRect.Right - TxtExtent.cx) div 2,
      (ItemRect.Top + ItemRect.Bottom - TxtExtent.cy) div 2,
      TxtExtent
    );
  end;

  // Erase background
  LB.Canvas.Pen.Color := LB.Color;
  LB.Canvas.Brush.Color := LB.Color;
  LB.Canvas.FillRect(ItemRect);

  // Draw any highlighting
  if (odFocused in State) or (odSelected in State) then
  begin
    if ThemeServicesEx.ThemesEnabled then
    begin
      // themes are enabled
      if (odFocused in State) then
      begin
        // focussed: draw highlight based on highlight colour and highlight text
        LB.Canvas.Brush.Color := GetHighLightColor(clHighlight);
        LB.Canvas.Pen.Color := GetShadowColor(LB.Canvas.Brush.Color);
        LB.Canvas.Font.Color := clHighlightText;
      end
      else
      begin
        // not focussed: draw highlight in button face and standard text colour
        LB.Canvas.Brush.Color := clBtnFace;
        LB.Canvas.Pen.Color := GetShadowColor(clBtnFace);
        LB.Canvas.Font.Color := LB.Font.Color;
      end;
    end
    else
    begin
      // themes are not available: use a more classic style
      if (odFocused in State) then
      begin
        // focussed: draw highlight in highlight colour
        LB.Canvas.Brush.Color := clHighlight;
        LB.Canvas.Pen.Color := GetShadowColor(LB.Canvas.Brush.Color);
        LB.Canvas.Font.Color := clHighlightText;
      end
      else
      begin
        // not focussed: draw using button colour
        LB.Canvas.Brush.Color := clBtnFace;
        LB.Canvas.Pen.Color := GetShadowColor(clBtnFace);
        LB.Canvas.Font.Color := LB.Font.Color;
      end;
    end;
    // draw the highlight (smaller than item's rectangle)
    LB.Canvas.Rectangle(ItemRect.Inflate(-4, -4));
  end
  else
    // no highlighting: just ensure font colour correct
    LB.Canvas.Font.Color := LB.Font.Color;

  // Draw text and any bitamp
  LB.Canvas.TextOut(TxtRect.Left, TxtRect.Top, Compiler.GetName);
  if Assigned(Bmp) then
    LB.Canvas.BrushCopy(ImgRect, Bmp, Bmp.Canvas.ClipRect, clFuchsia);

  // Draw separator line if item not last one
  if Index < Pred(LB.Count) then
  begin
    LB.Canvas.Pen.Color := clBtnShadow;
    LB.Canvas.MoveTo(ItemRect.Left + 4, ItemRect.Bottom - 1);
    LB.Canvas.LineTo(ItemRect.Right - 4, ItemRect.Bottom - 1);
  end;

  // Remove any focus rectangle
  if odFocused in State then
    LB.Canvas.DrawFocusRect(ItemRect);
end;

procedure TCompilersDlg.lbSwitchesClick(Sender: TObject);
  {Handles click on switches list on Command Line tab. Copies selected switch to
  edit box and updates tab's buttons.
    @param Sender [in] Not used.
  }
begin
  edSwitch.Text := lbSwitches.Items[lbSwitches.ItemIndex];
  UpdateSwitchButtons;
  edSwitch.SetFocus;
end;

procedure TCompilersDlg.pbCompilerPaint(Sender: TObject);
  {Handles paint event of paint box used to show name and glyph of selected
  compiler.
    @param Sender [in] Reference to paint box control.
  }

  // ---------------------------------------------------------------------------
  procedure RenderCompilerTitle(const Canvas: TCanvas; const Rect: TRectEx);
    {Displays compiler title on a gradient background.
      @param Canvas [in] Canvas on which to display title.
      @param Rect [in] Bounding rectangle of title.
    }
  const
    cLeftMargin = 2;  // margin between edge of canvas and logo or text
    cLogoPadding = 6; // padding between logo and text
  var
    GradColor1: TColor;     // primary gradient colour
    GradColor2: TColor;     // secondary gradient colour
    CompilerLogo: TBitmap;  // glyph of compiler logo or nil
    CompilerName: string;   // name of compiler to be displayed
    XOffset: Integer;       // X offset at which next left-aligned drawing done
  begin
    // Record name and glyph of compiler
    CompilerName := fCurCompiler.GetName;
    CompilerLogo := fCurCompiler.GetGlyph;

    // Set up colors and font style for title: depends on compiler availability
    if fCurCompiler.IsAvailable then
    begin
      GradColor1 := clActiveCaption;
      GradColor2 := clGradientActiveCaption;
      Canvas.Font.Style := [fsBold];
      Canvas.Font.Color := clCaptionText;
    end
    else
    begin
      GradColor1 := clInactiveCaption;
      GradColor2 := clGradientInactiveCaption;
      Canvas.Font.Style := [fsBold];
      Canvas.Font.Color := clInactiveCaptionText;
    end;

    // Draw gradient filled background rectangle
    GradientFillCanvas(Canvas, GradColor1, GradColor2, Rect, gdHorizontal);

    // Ensure that all further drawing on background is transparent
    Canvas.Brush.Style := bsClear;

    // Draw compiler logo (if present)
    XOffset := Rect.Left + cLeftMargin;
    if Assigned(CompilerLogo) then
    begin
      // draw the bitmap
      Canvas.BrushCopy(
        TRectEx.CreateBounds(
          XOffset,
          (Rect.Height - CompilerLogo.Height) div 2,
          CompilerLogo.Width,
          CompilerLogo.Height
        ),
        CompilerLogo,
        CompilerLogo.Canvas.ClipRect,
        clFuchsia // all logo glyphs have fuschia background
      );
      // we need to offset text to right of logo
      XOffset := XOffset + CompilerLogo.Width + cLogoPadding;
    end;

    // Draw compiler name text, left aligned and vertically centred
    Canvas.TextOut(
      XOffset,
      (Rect.Height - Canvas.TextHeight(CompilerName)) div 2,
      CompilerName
    );
  end;
  // ---------------------------------------------------------------------------

var
  PB: TPaintBox;          // reference to paint box control
  BufferBmp: TBitmap;     // background bitmap used for double-buffering drawing
begin
  // Get reference to paint box
  PB := Sender as TPaintBox;

  // Render title onto buffer bitmap
  BufferBmp := TBitmap.Create;
  try
    BufferBmp.Width := PB.Width;
    BufferBmp.Height := PB.Height;
    BufferBmp.Canvas.Font := PB.Canvas.Font;
    RenderCompilerTitle(BufferBmp.Canvas, PB.ClientRect);

    // Draw the offscreen bitmap in paintbox
    PB.Canvas.Draw(0, 0, BufferBmp);
  finally
    FreeAndNil(BufferBmp);
  end;
end;

procedure TCompilersDlg.pcCompilerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  {Handles event triggered when user clicks on one of page control tabs. Ensures
  page control has focus. This does always happen automatically.
    @param Sender [in] Not used.
    @param Button [in] Not used.
    @param Shift [in] Not used.
    @param X [in] X co-ordinate of mouse in client co-ordinates.
    @param Y [in] Y co-ordinate of mouse in client co-ordinates.
  }
begin
  if htOnItem in pcCompiler.GetHitTestInfoAt(X, Y) then
    pcCompiler.SetFocus;
end;

procedure TCompilersDlg.PopulateSwitchList(const Switches: string);
  {Stores list of command line switches in list box.
    @param Switches [in] Comma separated list of required switches.
  }
begin
  ExplodeStr(Switches, ',', lbSwitches.Items, False);
end;

procedure TCompilersDlg.SelectCompiler;
  {Stores reference to currently selected local compiler and updates dialog box
  controls with details of the compiler.
  }
var
  Prefixes: TCompLogPrefixes;   // log file prefixes for selected compiler
  PrefixKind: TCompLogPrefixID; // loops thru log file prefixes
begin
  // Store selected compiler
  fCurCompiler := fLocalCompilers[TCompilerID(lbCompilers.ItemIndex)];
  // Redraw compiler logo  and name in paint box
  pbCompiler.Refresh;
  // Update controls
  // edit box
  edCompilerPath.Text := fCurCompiler.GetExecFile;
  // store prefixes in value editor:
  // note: code assumes that editor item index is ordinal value of prefix ID
  Prefixes := fCurCompiler.GetLogFilePrefixes;
  for PrefixKind := Low(TCompLogPrefixID) to High(TCompLogPrefixID) do
    vleLogPrefixes.Cells[1, Ord(PrefixKind)] := Prefixes[PrefixKind];
  // store switches in switch list
  PopulateSwitchList(fCurCompiler.GetSwitches);
  UpdateSwitchButtons;
end;

procedure TCompilersDlg.UpdateCurrentCompiler;
  {Updates local copy of currently selected compiler with entries in dialog box.
  }
var
  Prefixes: TCompLogPrefixes;   // log file prefixes for selected compiler
  PrefixKind: TCompLogPrefixID; // loops thru log file prefixes
  WasAvailable: Boolean;        // whether compiler was available before update
  InvalidRect: TRectEx;         // area of list box to invalidate
begin
  if Assigned(fCurCompiler) then
  begin
    // Record whether compiler was available before update
    WasAvailable := fCurCompiler.IsAvailable;

    // Record executable file as stored in edit control
    fCurCompiler.SetExecFile(edCompilerPath.Text);

    // Record log file prefixes from value editor:
    // note: code assumes ordinal value of prefixes ids map to value edit index
    for PrefixKind := Low(TCompLogPrefixID) to High(TCompLogPrefixID) do
      Prefixes[PrefixKind] := vleLogPrefixes.Cells[1, Ord(PrefixKind)];
    fCurCompiler.SetLogFilePrefixes(Prefixes);

    // Store user defined prefixes
    fCurCompiler.SetSwitches(JoinStr(lbSwitches.Items, ',', False));

    // If availability has changed redraw selected list item and compiler title
    // to give visual feedback of changed state
    if WasAvailable <> fCurCompiler.IsAvailable then
    begin
      InvalidRect := lbCompilers.ItemRect(Ord(fCurCompiler.GetID));
      InvalidateRect(lbCompilers.Handle, @InvalidRect, False);
      pbCompiler.Refresh;
    end;
  end;
end;

procedure TCompilersDlg.UpdateSwitchButtons;
  {Updates state of buttons that manipulate command line switches depending on
  state of switch edit and list boxes.
  }
begin
  btnAdd.Enabled := (Trim(edSwitch.Text) <> '') and
    (lbSwitches.Items.IndexOf(Trim(edSwitch.Text)) = -1);
  btnReplace.Enabled := btnAdd.Enabled and
    (lbSwitches.ItemIndex >= 0);
  btnDelete.Enabled := lbSwitches.ItemIndex >= 0;
end;

procedure TCompilersDlg.vleLogPrefixesDrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
  {Handles event triggered when cell is to be drawn in owner draw value editor
  that displays compiler log prefix on on Log Files tab. This is owner drawn to
  get required borders and to display an arrow that points to selected cell.
    @param Sender [in] Reference to value edit control we are drawing.
    @param ACol [in] Column number of cell being drawn.
    @param ARow [in] Row number of cell being drawn.
    @param Rect [in] Bounding rectangle of cell being drawn.
    @param State [in] State of control: fixed cell, selected etc.
  }
var
  ValEd: TValueListEditor;  // reference to value editor being displayed
begin
  // Get reference to value editor
  ValEd := Sender as TValueListEditor;
  ValEd.Canvas.Font := ValEd.Font;
  with ValEd.Canvas do
  begin
    if gdFixed in State then
    begin
      // Set colours for fixed cells (non-editable)
      Brush.Color := clBtnFace;
      Font.Color := ValEd.Font.Color;
    end
    else
    begin
      // Set colours for editable cell
      Brush.Color := ValEd.Color;
      Font.Color := ValEd.Font.Color;
    end;
    // Colour the current cell
    FillRect(Rect);
    if gdFixed in State then
    begin
      // draw vertical line at right edge of fixed cell to act as border
      Pen.Color := clBtnShadow;
      MoveTo(Rect.Right - 1, Rect.Top);
      LineTo(Rect.Right - 1, Rect.Bottom);
    end;
    // Display required text
    TextOut(
      Rect.Left + 2 ,
      Rect.Top + (ValEd.RowHeights[ARow] - TextHeight('X')) div 2,
      ValEd.Cells[ACol, ARow]
    );
    if (ACol = 0) and (ValEd.Selection.Top = ARow) then
    begin
      // This is a fixed cell which has selected editable cell adjacent to it
      // draw an arrow at the RHS of this cell that points to selected cell
      Pen.Color := clHighlight;
      GraphUtil.DrawArrow(
        ValEd.Canvas,
        sdRight,
        Point(Rect.Right - 8, (Rect.Top + Rect.Bottom) div 2 - 4), 4
      );
    end;
  end;
end;

procedure TCompilersDlg.vleLogPrefixesSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
  {Handles event triggered when cell selected in Log Files tab's value editor
  changes: we need to change location of highlight arrow.
    @param Sender [in] Reference to value editor (not used).
    @param ACol [in] Column of selected cell (not used).
    @param ARow [in] Row of selected cell (not used).
    @param CanSelect [in/out] Left unchanged to enable selection change to go
      ahead.
  }
begin
  // Redraw display to ensure indicator arrows updated
  (Sender as TValueListEditor).Invalidate;
end;

end.

