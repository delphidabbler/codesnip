{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2011-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a frame used to change log file prefixes used for a compiler being
 * edited in TCompilersDlg.
}


unit FmCompilersDlg.FrLog;


interface


uses
  // Delphi
  Controls, Grids, ValEdit, Classes, StdCtrls, Types,
  // Project
  FmCompilersDlg.FrBase;


type
  ///  <summary>
  ///  Frame used to change log file prefixes used for a compiler being edited
  ///  in TCompilersDlg.
  ///  </summary>
  TCompilersDlgLogFrame = class(TCompilersDlgBaseFrame)
    lblLogPrefixes: TLabel;
    vleLogPrefixes: TValueListEditor;
    ///  <summary>Handles event triggered when cell is to be drawn in owner draw
    ///  value editor that displays compiler log prefix. Display an arrow that
    ///  points to selected cell.</summary>
    ///  <param name="Sender">TObject [in] Reference to control being drawn.
    ///  </param>
    ///  <param name="ACol">Integer [in] Column of cell being drawn.</param>
    ///  <param name="ARow">Integer [in] Row of cell being drawn.</param>
    ///  <param name="Rect">TRect [in] Bounding rectangle of cell being drawn.
    ///  </param>
    ///  <param name="State">TGridDrawState [in] State of control.</param>
    procedure vleLogPrefixesDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    ///  <summary>Handles event triggered when a cell is selected in value
    ///  editor. Redraws display to show changed location of highlight arrow.
    ///  </summary>
    ///  <remarks>No parameters other than Sender are used.</remarks>
    procedure vleLogPrefixesSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  strict protected
    ///  <summary>Initialises frame to display details of current compiler.
    ///  </summary>
    procedure Initialise; override;
  public
    ///  <summary>Arranges controls in frame.</summary>
    procedure ArrangeControls; override;
    ///  <summary>Updates current compiler object with edited information.
    ///  </summary>
    procedure UpdateCompiler; override;
  end;


implementation


uses
  // Delphi
  Graphics, GraphUtil,
  // Project
  Compilers.UGlobals, UCtrlArranger, UGraphicUtils;

{$R *.dfm}


{ TCompilersDlgLogFrame }

procedure TCompilersDlgLogFrame.ArrangeControls;
var
  RowIdx: Integer;    // loops through rows of vleLogPrefixes
  CellSize: TSize;    // size of left hand cell in vleLogPrefixes
begin
  TCtrlArranger.SetLabelHeights(Self);
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
end;

procedure TCompilersDlgLogFrame.Initialise;
var
  Prefixes: TCompLogPrefixes;   // log file prefixes for selected compiler
  PrefixKind: TCompLogPrefixID; // loops thru log file prefixes
begin
  Prefixes := Compiler.GetLogFilePrefixes;
  for PrefixKind := Low(TCompLogPrefixID) to High(TCompLogPrefixID) do
    vleLogPrefixes.Cells[1, Ord(PrefixKind)] := Prefixes[PrefixKind];
end;

procedure TCompilersDlgLogFrame.UpdateCompiler;
var
  Prefixes: TCompLogPrefixes;   // log file prefixes for selected compiler
  PrefixKind: TCompLogPrefixID; // loops thru log file prefixes
begin
  // NOTE: code assumes ordinal value of prefixes ids map to value edit index
  for PrefixKind := Low(TCompLogPrefixID) to High(TCompLogPrefixID) do
    Prefixes[PrefixKind] := vleLogPrefixes.Cells[1, Ord(PrefixKind)];
  Compiler.SetLogFilePrefixes(Prefixes);
end;

procedure TCompilersDlgLogFrame.vleLogPrefixesDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
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

procedure TCompilersDlgLogFrame.vleLogPrefixesSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  // Redraw display to ensure indicator arrows updated
  (Sender as TValueListEditor).Invalidate;
end;

end.
