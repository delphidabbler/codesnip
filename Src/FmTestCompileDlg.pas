{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2011-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box which test compiles a snippet and displays the
 * results.
}


unit FmTestCompileDlg;

interface

uses
  Classes, ActnList, StdCtrls, Forms, Controls, ExtCtrls, Messages,
  Generics.Collections,

  Compilers.UGlobals, DB.USnippet, FmGenericViewDlg, UBaseObjects, UCompileMgr,
  ULEDImageList, System.Actions;

type
  ///  <summary>Implements a dialogue box that test compiles a snippet and
  ///  displays the results.</summary>
  TTestCompileDlg = class(TGenericViewDlg, INoPublicConstruct)
    sbCompilers: TScrollBox;
    lblSnippetName: TLabel;
    lblSnippetNameDesc: TLabel;
    alMain: TActionList;
    actScrollPageUp: TAction;
    actScrollPageDown: TAction;
    actScrollLineUp: TAction;
    actScrollLineDown: TAction;
    btnViewErrors: TButton;
    actViewErrors: TAction;
    ///  <summary>Instantiates owned object and notes dialogue initialising.
    ///  </summary>
    procedure FormCreate(Sender: TObject);
    ///  <summary>Tidies up form: frees owned object.</summary>
    procedure FormDestroy(Sender: TObject);
    ///  <summary>Scrolls up one page in scroll box.</summary>
    procedure actScrollPageUpExecute(Sender: TObject);
    ///  <summary>Scrolls down one page in scroll box.</summary>
    procedure actScrollPageDownExecute(Sender: TObject);
    ///  <summary>Scrolls up one "line" in scroll box.</summary>
    procedure actScrollLineUpExecute(Sender: TObject);
    ///  <summary>Scrolls down one "line" in scroll box.</summary>
    procedure actScrollLineDownExecute(Sender: TObject);
    ///  <summary>Updates enabled state of View Errors action.</summary>
    procedure actViewErrorsUpdate(Sender: TObject);
    ///  <summary>Displays any compile errors in a dialogue box.</summary>
    procedure actViewErrorsExecute(Sender: TObject);
  strict private
    type
      ///  <summary>Component that displays information about a compiler and its
      ///  compile result.</summary>
      TCompilerCtrl = class(TGraphicControl)
      strict private
        var
          ///  <summary>Value of Compiler property.</summary>
          fCompiler: ICompiler;
          ///  <summary>Value of CompileResult property.</summary>
          fCompileResult: TCompileResult;
        class var
          ///  <summary>Reference to image list containing images of compile
          ///  result LEDs.</summary>
          fLEDs: TLEDImageList;
      strict private
        ///  <summary>Write accessor for Compiler property.</summary>
        ///  <remarks>Adjusts control height and invalidates it.</remarks>
        procedure SetCompiler(Compiler: ICompiler);
        ///  <summary>Write accessor for CompileResult property.</summary>
        ///  <remarks>Causes control to be repainted.</remarks>
        procedure SetCompileResult(CompileResult: TCompileResult);
        ///  <summary>Resizes control in response to changes in font.</summary>
        procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
        ///  <summary>Calculates and returns required height of control.
        ///  </summary>
        function GetHeight: Integer;
        ///  <summary>Updates height of control.</summary>
        procedure UpdateHeight;
      strict protected
        ///  <summary>Paints control, displaying compiler and compile result
        ///  information.</summary>
        procedure Paint; override;
      public
        ///  <summary>Creates LED image list shared between all instances.
        ///  </summary>
        class constructor Create;
        ///  <summary>Frees shared LED image list instance.</summary>
        class destructor Destroy;
        ///  <summary>Object constructor override. Sets default property values.
        ///  </summary>
        constructor Create(AOwner: TComponent); override;
        ///  <summary>Override of inherited method to set bounds of control.
        ///  Ignores height AHeight and replaces it with calculated height.
        ///  </summary>
        procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
        ///  <summary>Reference to compiler whose info is displayed.</summary>
        property Compiler: ICompiler read fCompiler write SetCompiler;
        ///  <summary>Compiler result to be displayed.</summary>
        property CompileResult: TCompileResult
          read fCompileResult write SetCompileResult;
      end;
    type
      ///  <summary>List of compiler controls.</summary>
      TCompilerCtrls = TList<TCompilerCtrl>;
  strict private
    var
      ///  <summary>Reference to snippet to be test compiled.</summary>
      fSnippet: TSnippet;
      ///  <summary>Reference to compiler manager object used to perform and
      ///  record results of test compilation.</summary>
      fCompileMgr: TCompileMgr;
      ///  <summary>List of compiler controls to be displayed.</summary>
      fCompilerCtrlList: TCompilerCtrls;
      ///  <summary>Flag that informs if form is initialising, i.e. performing
      ///  test compile. Set to True when form is created and to False after
      ///  compilation complete and results displayed.</summary>
      fInitialising: Boolean;
    ///  <summary>Creates all required compiler controls and stores them in
    ///  list.</summary>
    procedure CreateCompilerCtrls;
    ///  <summary>Displays results of test compilation.</summary>
    procedure DisplayCompileResults(const Compilers: ICompilers);
  strict protected
    ///  <summary>Arranges controls on form.</summary>
    ///  <remarks>Called from ancestor class.</remarks>
    procedure ArrangeControls; override;
    ///  <summary>Creates and configures required controls.</summary>
    ///  <remarks>Called from ancestor class.</remarks>
    procedure CustomiseControls; override;
    ///  <summary>Performs test compilation after form is displayed and displays
    ///  results.</summary>
    ///  <remarks>Called from ancestor class.</remarks>
    procedure AfterShowForm; override;
  public
    ///  <summary>Displays dialogue box that performs test compilation and
    ///  displays results.</summary>
    ///  <param name="AOwner">TComponent [in] Reference to any owning control.
    ///  </param>
    ///  <param name="ACompileMgr">TCompileMgr [in] Object used to perform test
    ///  compilation and record results.</param>
    ///  <param name="ASnippet">TSnippet [in] Snippet to be test compiled.
    ///  </param>
    class procedure Execute(const AOwner: TComponent;
      const ACompileMgr: TCompileMgr; const ASnippet: TSnippet);
  end;


implementation


uses
  // Delphi
  Math, Windows, Graphics, Types {for inlining},
  // Project
  UColours, UCtrlArranger, UFontHelper, UPreferences;

{$R *.dfm}


{ TTestCompileDlg }

procedure TTestCompileDlg.actScrollLineDownExecute(Sender: TObject);
begin
  sbCompilers.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

procedure TTestCompileDlg.actScrollLineUpExecute(Sender: TObject);
begin
  sbCompilers.Perform(WM_VSCROLL, SB_LINEUP, 0);
end;

procedure TTestCompileDlg.actScrollPageDownExecute(Sender: TObject);
begin
  sbCompilers.Perform(WM_VSCROLL, SB_PAGEDOWN, 0);
end;

procedure TTestCompileDlg.actScrollPageUpExecute(Sender: TObject);
begin
  sbCompilers.Perform(WM_VSCROLL, SB_PAGEUP, 0);
end;

procedure TTestCompileDlg.actViewErrorsExecute(Sender: TObject);
begin
  fCompileMgr.ShowErrors;
end;

procedure TTestCompileDlg.actViewErrorsUpdate(Sender: TObject);
begin
  actViewErrors.Enabled := not fInitialising and fCompileMgr.HaveErrors;
end;

procedure TTestCompileDlg.AfterShowForm;
begin
  inherited;
  Enabled := False;
  try
    fCompileMgr.Compile(pnlBody, fSnippet, DisplayCompileResults);
  finally
    Enabled := True;
  end;
  fInitialising := False;
end;

procedure TTestCompileDlg.ArrangeControls;
const
  // Various margins, offsets and sizes
  MaxBodyPanelWidth = 760;
  ViewErrorsBtnRightMargin = 16;
  SnippetLblTop = 0;
  SnippetLblBottomMargin = 8;
  CompilerCtrlListTop = 0;
  MaxCompilersBeforeScroll = 8;
var
  CompCtrl: TCompilerCtrl;    // each compiler control
  NextTop: Integer;           // top of next compiler control in scroll box
  MaxCompListHeight: Integer; // max height of compiler list
begin
  TCtrlArranger.SetLabelHeight(lblSnippetNameDesc);

  // Set horizontal alignments
  TCtrlArranger.MoveToRightOf(lblSnippetNameDesc, lblSnippetName);
  btnViewErrors.Left := pnlBody.Left;

  // Set widths
  pnlBody.ClientWidth := Min(
    Max(
      pnlBody.ClientWidth,
      Max(
        // allow room for long snippet names
        TCtrlArranger.RightOf(lblSnippetName),
        // allow room for button at bottom
        btnViewErrors.Width + ViewErrorsBtnRightMargin
          + TCtrlArranger.RightOf(btnHelp)- btnClose.Left
      )
    ),
    MaxBodyPanelWidth // body panel to be no more this any case!
  );
  sbCompilers.Width := pnlBody.ClientWidth;

  // Set vertical alignments
  TCtrlArranger.AlignVCentres(
    SnippetLblTop, [lblSnippetName, lblSnippetNameDesc]
  );
  sbCompilers.Top := TCtrlArranger.BottomOf(
    lblSnippetName, SnippetLblBottomMargin
  );
  if fCompilerCtrlList.Count > 0 then
    MaxCompListHeight := Min(MaxCompilersBeforeScroll, fCompilerCtrlList.Count)
      * fCompilerCtrlList[0].Height
  else
    MaxCompListHeight := 20;
  sbCompilers.Height := MaxCompListHeight + 2 * sbCompilers.BevelWidth;

  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody) + 8;

  // Arrange compiler controls vertically within scroll box
  NextTop := CompilerCtrlListTop;
  for CompCtrl in fCompilerCtrlList do
  begin
    CompCtrl.Top := NextTop;
    Inc(NextTop, CompCtrl.Height);
  end;
  // Set compiler controls' widths: must do this after above loop so that any
  // vertical scroll bar in scroll box has been created
  for CompCtrl in fCompilerCtrlList do
    CompCtrl.Width := sbCompilers.ClientWidth;

  inherited;

  btnViewErrors.Top := btnHelp.Top;
end;

procedure TTestCompileDlg.CreateCompilerCtrls;
var
  Compiler: ICompiler;  // each supported compiler
  Ctrl: TCompilerCtrl;  // compiler control
begin
  for Compiler in fCompileMgr.Compilers do
  begin
    if Compiler.IsAvailable then
    begin
      Ctrl := TCompilerCtrl.Create(Self);
      Ctrl.Parent := sbCompilers;
      Ctrl.Compiler := Compiler;
      Ctrl.Color := Self.Color;
      fCompilerCtrlList.Add(Ctrl);
    end;
  end;
end;

procedure TTestCompileDlg.CustomiseControls;
begin
  inherited;
  CreateCompilerCtrls;
  // Set required label fonts and captions
  TFontHelper.SetDefaultBaseFont(lblSnippetName.Font);
  lblSnippetName.Font.Color :=
    Preferences.DBHeadingColours[fSnippet.UserDefined];
  lblSnippetName.Caption := fSnippet.DisplayName;
end;

procedure TTestCompileDlg.DisplayCompileResults(const Compilers: ICompilers);
var
  Ctrl: TCompilerCtrl;  // each compiler control in list
begin
  for Ctrl in fCompilerCtrlList do
    Ctrl.CompileResult := Compilers[Ctrl.Compiler.GetID].GetLastCompileResult;
end;

class procedure TTestCompileDlg.Execute(const AOwner: TComponent;
  const ACompileMgr: TCompileMgr; const ASnippet: TSnippet);
begin
  Assert(Assigned(ACompileMgr), ClassName + '.Execute: ACompileMgr is nil');
  Assert(Assigned(ASnippet), ClassName + '.Execute: ASnippet is nil');
  with InternalCreate(AOwner) do
    try
      fCompileMgr := ACompileMgr;
      fSnippet := ASnippet;
      ShowModal;
    finally
      Free;
    end;
end;

procedure TTestCompileDlg.FormCreate(Sender: TObject);
begin
  fInitialising := True;
  inherited;
  fCompilerCtrlList := TCompilerCtrls.Create;
end;

procedure TTestCompileDlg.FormDestroy(Sender: TObject);
begin
  fCompilerCtrlList.Free;
  inherited;
end;

{ TTestCompileDlg.TCompilerCtrl }

procedure TTestCompileDlg.TCompilerCtrl.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  UpdateHeight;
end;

constructor TTestCompileDlg.TCompilerCtrl.Create(AOwner: TComponent);
begin
  inherited;
  ParentColor := True;
  ParentFont := True;
  fCompileResult := crQuery;
  Width := 180;
end;

class constructor TTestCompileDlg.TCompilerCtrl.Create;
begin
  fLEDs := TLEDImageList.Create(nil);
end;

class destructor TTestCompileDlg.TCompilerCtrl.Destroy;
begin
  fLEDS.Free;
end;

function TTestCompileDlg.TCompilerCtrl.GetHeight: Integer;
const
  Padding = 4;  // padding applied to calculated height
begin
  if not Assigned(fCompiler) then
    Exit(0);
  Result := Max(Canvas.TextHeight(fCompiler.GetName), fLEDs.Height) + Padding;
end;

procedure TTestCompileDlg.TCompilerCtrl.Paint;
const
  NameLeftOffset = 4;           // left offset of compiler name text
  LEDLeftMargin = 4;            // margin before LED image (after divider)
  LEDRightMargin = 6;           // margin after LED image
  DividerLeftMargin = 2;        // margin before divider bar (after name)
  DividerWidth = 2;             // width of divider bar
var
  MaxNameWidth: Integer;        // max width for displaying compiler name text
  NameRect: TRect;              // rectangle containing compiler name text
  LEDLeftOffset: Integer;       // left offset of LEDs in control
  DividerLeftOffset: Integer;   // left offset of divider bar in control
  DividerRect: TRect;           // rectangle used to display divider bar
  BoundsRect: TRect;            // control's bounding rectangle (control coords)
  CompilerName: string;         // name of compiler
begin
  inherited;
  if not Assigned(fCompiler) then
    Exit;

  CompilerName := fCompiler.GetName;

  // Calculate widths and offsets that depend on control width
  LEDLeftOffset := Width - LEDRightMargin - fLEDs.Width;
  DividerLeftOffset := LEDLeftOffset - LEDLeftMargin - DividerWidth;
  MaxNameWidth := DividerLeftOffset - DividerLeftMargin - NameLeftOffset;

  Canvas.Font := Font;
  Canvas.Brush.Color := Color;

  // Draw compiler name
  NameRect := Bounds(
    NameLeftOffset,
    (Height - Canvas.TextHeight(CompilerName)) div 2,
    MaxNameWidth,
    Canvas.TextHeight(CompilerName)
  );
  Canvas.TextRect(NameRect, CompilerName, [tfLeft, tfNoPrefix]);

  // Draw divider
  DividerRect := Bounds(DividerLeftOffset, 0, DividerWidth, Height - 3);
  Windows.DrawEdge(
    Canvas.Handle,
    DividerRect,
    EDGE_RAISED,
    BF_LEFT or BF_RIGHT
  );

  // Draw compiler result
  fLEDs.Draw(
    Canvas,
    Point(LEDLeftOffset, (Height - fLEDs.Height) div 2),
    fCompileResult
  );

  // Draw bottom ruling
  BoundsRect := Bounds(0, Height - 2, Width, 2);
  Windows.DrawEdge(
    Canvas.Handle,
    BoundsRect,
    EDGE_RAISED,
    BF_BOTTOM or BF_TOP
  );
end;

procedure TTestCompileDlg.TCompilerCtrl.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  AHeight := GetHeight;
  inherited;
end;

procedure TTestCompileDlg.TCompilerCtrl.SetCompiler(Compiler: ICompiler);
begin
  fCompiler := Compiler;
  UpdateHeight;
  Invalidate;
end;

procedure TTestCompileDlg.TCompilerCtrl.SetCompileResult(
  CompileResult: TCompileResult);
begin
  fCompileResult := CompileResult;
  Invalidate;
end;

procedure TTestCompileDlg.TCompilerCtrl.UpdateHeight;
begin
  SetBounds(Left, Top, Width, 0); // Height is set automatically
end;

end.

