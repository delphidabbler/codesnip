{
 * UMemoHelper.pas
 *
 * Class that provides information about, and assists with manipulation of, memo
 * controls where the functionality is not directly available from the control.
 *
 * Originally named UMemoInfo.pas. Changed to UMemoHelper.pas at v1.1.
 *
 * v0.1 of 30 Apr 2006  - Original version, named UMemoInfo.pas.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 *                      - Relocated and rationalised $WARN directives.
 * v1.1 of 13 May 2007  - Renamed unit from UMemoInfo to UMemoHelper and renamed
 *                        TMemoInfo as TMemoHelper.
 *                      - Changed TMemoHelper to wrap a TCustomMemo rather than
 *                        TMemo, to allow common functionality to apply to RTF
 *                        controls.
 *                      - Added code to set a memo control margin.
 * v1.2 of 15 Dec 2008  - Modified to use TRectEx record instead of TRect.
 *                      - Made private section strict.
 * v1.3 of 17 Dec 2008  - Fixed bug introduced in v1.2 in SetMargin method. Was
 *                        offsetting rather than shrinking display rectangle.
 *                      - Added new private SetRect method.
 *
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
 * The Original Code is UMenuHelper.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}



unit UMemoHelper;

{$WARN UNSAFE_CODE OFF}

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
    fMemo: TCustomMemo;
      {Memo control that we're manipulating}
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
  end;


implementation


uses
  // Delphi
  SysUtils, Controls, Messages,
  // Project
  UStructs;


{ TMemoHelper }

constructor TMemoHelper.Create(const AMemo: TCustomMemo);
  {Class constructor. Creates info object for a specified memo.
    @param AMemo [in] Memo for which we want info.
  }
begin
  Assert(Assigned(AMemo),   // ** do not localise
    'TMemoHelper.Create: AMemo is nil');
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
  fMemo.Perform(EM_GETRECT, 0, Integer(@Result));
end;

function TMemoHelper.LineExtent(const LineIdx: Integer): TSize;
  {Gets extent (i.e. width and height) of a memo line.
    @param LineIdx [in] Index of line.
    @return Width and height of line in pixels.
  }
var
  Canvas: TControlCanvas;   // canvas of memo control
begin
  Canvas := TControlCanvas.Create;
  try
    Canvas.Control := fMemo;
    Result := Canvas.TextExtent(fMemo.Lines[LineIdx]);
  finally
    FreeAndNil(Canvas);
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

