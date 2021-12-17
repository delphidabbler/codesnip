{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2011-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that manages display of a compiler name on a gradient
 * filled background in a paint box.
 *
 * This is a helper unit for TCompilersDlg.
}


unit FmCompilersDlg.UBannerMgr;


interface


uses
  // Delphi
  Graphics, ExtCtrls,
  // Project
  Compilers.UGlobals, UStructs;


type
  ///  <summary>Manages display of a compiler name on a gradient filled
  ///  background in a paint box.</summary>
  ///  <remarks>This is is a helper class for TCompilersDlg.</remarks>
  TCompilerBannerMgr = class(TObject)
  strict private
    ///  <summary>Paint box control being managed.</summary>
    fPB: TPaintBox;
    ///  <summary>Value of Compiler property.</summary>
    fCompiler: ICompiler;
    ///  <summary>Write accessor for Compiler property. Sets value and updates
    ///  banner display.</summary>
    procedure SetCompiler(Value: ICompiler);
    ///  <summary>Handles paint box's OnPaint event by displaying details of
    ///  compiler specified by Compiler property.</summary>
    procedure PaintHandler(Sender: TObject);
    ///  <summary>Draws compiler name on gradient filled background.</summary>
    ///  <param name="Canvas">TCanvas [in] Canvas on which to draw.</param>
    ///  <param name="Rect">TRectEx [in] Bounding rectangle of drawing.</param>
    procedure RenderCompilerTitle(const Canvas: TCanvas; const Rect: TRectEx);
  public
    ///  <summary>Constructs object for use with given paint box.</summary>
    constructor Create(const PB: TPaintBox);
    ///  <summary>Causes display in paint box to be redrawn.</summary>
    procedure Refresh;
    ///  <summary>Compiler whose name is to be displayed.</summary>
    property Compiler: ICompiler read fCompiler write SetCompiler;
  end;


implementation


uses
  // Delphi
  GraphUtil;


{ TCompilerBannerMgr }

constructor TCompilerBannerMgr.Create(const PB: TPaintBox);
begin
  inherited Create;
  fPB := PB;
  fPB.OnPaint := PaintHandler;
end;

procedure TCompilerBannerMgr.PaintHandler(Sender: TObject);
var
  PB: TPaintBox;          // reference to paint box control
  BufferBmp: TBitmap;     // background bitmap used for double-buffering drawing
begin
  PB := Sender as TPaintBox;
  // We draw on a bitmap then display that in paint box
  BufferBmp := TBitmap.Create;
  try
    BufferBmp.Width := PB.Width;
    BufferBmp.Height := PB.Height;
    BufferBmp.Canvas.Font := PB.Canvas.Font;
    RenderCompilerTitle(BufferBmp.Canvas, PB.ClientRect);
    PB.Canvas.Draw(0, 0, BufferBmp);
  finally
    BufferBmp.Free;
  end;
end;

procedure TCompilerBannerMgr.Refresh;
begin
  fPB.Refresh;
end;

procedure TCompilerBannerMgr.RenderCompilerTitle(const Canvas: TCanvas;
  const Rect: TRectEx);
  const
    LeftMargin = 4;  // margin between edge of canvas and logo or text
  var
    GradColour1: TColor;     // primary gradient colour
    GradColour2: TColor;     // secondary gradient colour
    CompilerName: string;    // name of compiler to be displayed
  begin
    CompilerName := fCompiler.GetName;

    // set up colours and font style for title: depends on compiler availability
    if fCompiler.IsAvailable then
    begin
      GradColour1 := clActiveCaption;
      GradColour2 := clGradientActiveCaption;
      Canvas.Font.Style := [fsBold];
      Canvas.Font.Color := clCaptionText;
    end
    else
    begin
      GradColour1 := clInactiveCaption;
      GradColour2 := clGradientInactiveCaption;
      Canvas.Font.Style := [fsBold];
      Canvas.Font.Color := clInactiveCaptionText;
    end;

    // draw gradient filled background rectangle
    GradientFillCanvas(Canvas, GradColour1, GradColour2, Rect, gdHorizontal);

    // ensure that all further drawing on background is transparent
    Canvas.Brush.Style := bsClear;

    // draw compiler name text, left aligned and vertically centred
    Canvas.TextOut(
      Rect.Left + LeftMargin,
      (Rect.Height - Canvas.TextHeight(CompilerName)) div 2,
      CompilerName
    );
end;

procedure TCompilerBannerMgr.SetCompiler(Value: ICompiler);
begin
  fCompiler := Value;
  Refresh;
end;

end.

