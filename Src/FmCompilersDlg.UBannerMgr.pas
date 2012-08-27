{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2011-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a class that manages display of a compiler name and glyph in a
 * paint box.
 *
 * This is a helper class for TCompilersDlg.
}


unit FmCompilersDlg.UBannerMgr;


interface


uses
  // Delphi
  Graphics, ExtCtrls,
  // Project
  Compilers.UGlobals, UStructs;


type
  ///  <summary>
  ///  Manages display of compiler name and glyph in a paint box.
  ///  </summary>
  ///  <remarks>
  ///  This is is a helper class for TCompilersDlg.
  ///  </remarks>
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
    ///  <summary>Draws compiler name and optional glyph.</summary>
    ///  <param name="Canvas">TCanvas [in] Canvas on which to draw.</param>
    ///  <param name="Rect">TRectEx [in] Bounding rectangle of drawing.</param>
    procedure RenderCompilerTitle(const Canvas: TCanvas; const Rect: TRectEx);
  public
    ///  <summary>Object constructor. Records and sets up given paint box.
    ///  </summary>
    constructor Create(const PB: TPaintBox);
    ///  <summary>Causes display in paint box to be redrawn.</summary>
    procedure Refresh;
    ///  <summary>Compiler whose details are to be displayed.</summary>
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
    cLeftMargin = 2;  // margin between edge of canvas and logo or text
    cLogoPadding = 6; // padding between logo and text
  var
    GradColor1: TColor;     // primary gradient colour
    GradColor2: TColor;     // secondary gradient colour
    CompilerLogo: TBitmap;  // glyph of compiler logo or nil
    CompilerName: string;   // name of compiler to be displayed
    XOffset: Integer;       // X offset at which next left-aligned drawing done
  begin
    CompilerName := fCompiler.GetName;
    CompilerLogo := fCompiler.GetGlyph;

    // Set up colors and font style for title: depends on compiler availability
    if fCompiler.IsAvailable then
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

procedure TCompilerBannerMgr.SetCompiler(Value: ICompiler);
begin
  fCompiler := Value;
  Refresh;
end;

end.
