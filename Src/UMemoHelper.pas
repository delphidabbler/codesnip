{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Class that provides information about, and assists with manipulation of, memo
 * controls where the functionality is not directly available from the control.
}


unit UMemoHelper;


interface


uses
  // Delphi
  StdCtrls, Windows;


type

  {
  TMemoHelper:
    Provides information about, and assists with manipulation of, memo controls
    where the functionality is not directly available from the control. Some of
    the functions also apply to rich edit controls.
  }
  TMemoHelper = class(TObject)
  strict private
    fMemo: TCustomMemo;   // Memo control that we're manipulating
    function LineExtent(const LineIdx: Integer): TSize;
      {Gets extent (i.e. width and height) of a memo line.
        @param LineIdx [in] Index of line.
        @return Width and height of line in pixels.
      }
    function GetFirstVisibleLine: Integer;
      {Gets index of first visible line in memo control.
        @return Index of line.
      }
    function GetRect: TRect;
      {Get formatting rectangle of memo control.
        @return Required rectangle.
      }
    procedure SetRect(const R: TRect);
      {Set formatting rectangle of memo control.
        @param R [in] New value of formatting rectangle.
      }
    function LineIdxOffset(const LineIdx: Integer): Integer;
      {Gets index of line relative to top of memo.
        @param LineIdx [in] Index of line.
        @return Required index offset.
      }
  public
    constructor Create(const AMemo: TCustomMemo);
      {Class constructor. Creates info object for a specified memo.
        @param AMemo [in] Memo to be manipulated.
      }
    function LineLeft(const LineIdx: Integer): Integer;
      {Gets left margin of a memo line relative to memo's client area.
        @param LineIdx [in] Index of line.
        @return Left margin in pixels.
      }
    function LineTop(const LineIdx: Integer): Integer;
      {Gets location of top of specified line relative to memo's client area.
        @param LineIdx [in] Index of line.
        @return Required offset in pixels.
      }
    function LineWidth(const LineIdx: Integer): Integer;
      {Gets width of a memo line.
        @param LineIdx [in] Index of line.
        @return Required width in pixels.
      }
    function LineHeight(const LineIdx: Integer): Integer;
      {Gets height of a memo line.
        @param LineIdx [in] Index of line.
        @return Required height in pixels.
      }
    procedure SetMargin(const Margin: Byte);
      {Sets memo's margin.
        @param Margin [in] Required width of margin.
      }
    function CaretPos: TPoint;
      {Gets caret position in memo control in character co-ordinates.
        @return Required cursor position.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Messages, Graphics,
  // Project
  UFontHelper, UGraphicUtils, UStructs;


{ TMemoHelper }

function TMemoHelper.CaretPos: TPoint;
  {Gets caret position in memo control in character co-ordinates.
    @return Required cursor position.
  }
begin
  Result.X := fMemo.SelStart - fMemo.Perform(EM_LINEINDEX, WPARAM(-1), 0);
  Result.Y := fMemo.Perform(EM_LINEFROMCHAR, WPARAM(-1), 0);
end;

constructor TMemoHelper.Create(const AMemo: TCustomMemo);
  {Class constructor. Creates info object for a specified memo.
    @param AMemo [in] Memo for which we want info.
  }
begin
  Assert(Assigned(AMemo), ClassName + '.Create: AMemo is nil');
  inherited Create;
  fMemo := AMemo;
end;

function TMemoHelper.GetFirstVisibleLine: Integer;
  {Gets index of first visible line in memo control.
    @return Index of line.
  }
begin
  Result := fMemo.Perform(EM_GETFIRSTVISIBLELINE, 0, 0);
end;

function TMemoHelper.GetRect: TRect;
  {Get formatting rectangle of memo control.
    @return Required rectangle.
  }
begin
  fMemo.Perform(EM_GETRECT, 0, LPARAM(@Result));
end;

function TMemoHelper.LineExtent(const LineIdx: Integer): TSize;
  {Gets extent (i.e. width and height) of a memo line.
    @param LineIdx [in] Index of line.
    @return Width and height of line in pixels.
  }
var
  Font: TFont;  // memo control's font
begin
  // We need a TFont object that represents the memo control's font. But, we
  // can't call Font property since it is not exposed by TCustomMemo. Therefore
  // we create a TFont from a copy of the font whose handle we get by sending a
  // WM_GETFONT message to memo control. We need to copy it since when the
  // TFont is freed it deletes the font handle, which is still required by the
  // memo control and possibly other controls.
  Font := TFont.Create;
  try
    Font.Handle := TFontHelper.CloneFontHandle(
     fMemo.Perform(WM_GETFONT, 0, 0)
    );
    // Get size of memo line in its current font
    Result := StringExtent(fMemo.Lines[LineIdx], Font);
  finally
    FreeAndNil(Font); // releases HCloneFont handle
  end;
end;

function TMemoHelper.LineHeight(const LineIdx: Integer): Integer;
  {Gets height of a memo line.
    @param LineIdx [in] Index of line.
    @return Required height in pixels.
  }
begin
  Result := LineExtent(LineIdx).cy;
end;

function TMemoHelper.LineIdxOffset(const LineIdx: Integer): Integer;
  {Gets index of line relative to top of memo.
    @param LineIdx [in] Index of line.
    @return Required index offset.
  }
begin
  Result := LineIdx - GetFirstVisibleLine;
end;

function TMemoHelper.LineLeft(const LineIdx: Integer): Integer;
  {Gets left margin of a memo line relative to memo's client area.
    @param LineIdx [in] Index of line.
    @return Left margin in pixels.
  }
begin
  Result := GetRect.Left;
end;

function TMemoHelper.LineTop(const LineIdx: Integer): Integer;
  {Gets location of top of specified line relative to memo's client area.
    @param LineIdx [in] Index of line.
    @return Required offset in pixels.
  }
begin
  // assumes all lines same height
  Result := LineIdxOffset(LineIdx) * LineHeight(LineIdx) + GetRect.Top;
end;

function TMemoHelper.LineWidth(const LineIdx: Integer): Integer;
  {Gets width of a memo line.
    @param LineIdx [in] Index of line.
    @return Required width in pixels.
  }
begin
  Result := LineExtent(LineIdx).cx;
end;

procedure TMemoHelper.SetMargin(const Margin: Byte);
  {Sets memo's margin.
    @param Margin [in] Required width of margin.
  }
var
  R: TRectEx; // required bounding rectangle adjusted for margin
begin
  R := fMemo.ClientRect;
  R.InflateBy(-Margin, -Margin);
  SetRect(R);
end;

procedure TMemoHelper.SetRect(const R: TRect);
  {Set formatting rectangle of memo control.
    @param R [in] New value of formatting rectangle.
  }
begin
  fMemo.Perform(EM_SETRECT, 0, LPARAM(@R));
end;

end.

