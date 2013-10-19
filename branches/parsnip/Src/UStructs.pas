{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Various structures. Some are extension of existing structures with added
 * functionality.
}


unit UStructs;


interface


uses
  // Delphi
  Generics.Collections,
  Types;


type

  ///  <summary>Encapsulates a rectangle.</summary>
  ///  <remarks>TRectEx is sssignment compatible with TRect.</remarks>
  TRectEx = packed record
  public
    var
      ///  <summary>Position of left side of rectangle.</summary>
      Left: LongInt;
      ///  <summary>Position of top side of rectangle.</summary>
      Top: LongInt;
      ///  <summary>Position of right side of rectangle.</summary>
      Right: LongInt;
      ///  <summary>Position of bottom side of rectangle.</summary>
      Bottom: Longint;

    ///  <summary>Constructs a new rectangle record from the given left, top,
    ///  right and bottom positions.</summary>
    constructor Create(ALeft, ATop, ARight, ABottom: Longint);

    ///  <summary>Creates a new rectangle with the given bounds.</summary>
    ///  <param name="ALeft">Integer [in] X-coordinate of top left corner of
    ///  rectangle.</param>
    ///  <param name="ATop">Integer [in] Y-coordinate of top left corner of
    ///  rectangle.</param>
    ///  <param name="AWidth">Integer [in] Width of rectangle.</param>
    ///  <param name="AHeight">Integer [in] Height of rectangle.</param>
    constructor CreateBounds(ALeft, ATop, AWidth, AHeight: Longint); overload;

    ///  <summary>Creates a new rectangle with the given bounds.</summary>
    ///  <param name="ALeft">Integer [in] X-coordinate of top left corner of
    ///  rectangle.</param>
    ///  <param name="ATop">Integer [in] Y-coordinate of top left corner of
    ///  rectangle.</param>
    ///  <param name="ASize">TSize [in] Size of rectangle.</param>
    constructor CreateBounds(ALeft, ATop: Longint; ASize: TSize); overload;

    ///  <summary>Casts a TRect record to a TRectEx.</summary>
    class operator Implicit(ARect: TRect): TRectEx;

    ///  <summary>Casts a TRectEx record to a TRect.</summary>
    class operator Implicit(ARect: TRectEx): TRect;

    ///  <summary>Checks if the given rectangles are the same.</summary>
    ///  <remarks>One of the records being compared may be a TRect.</remarks>
    class operator Equal(R1, R2: TRectEx): Boolean;

    ///  <summary>Checks if the given rectangles are not the same.</summary>
    ///  <remarks>One of the records being compared may be a TRect.</remarks>
    class operator NotEqual(R1, R2: TRectEx): Boolean;

    ///  <summary>Returns the width of the rectangle.</summary>
    function Width: Longint;

    ///  <summary>Returns the height of the rectangle.</summary>
    function Height: Longint;

    ///  <summary>Inflates or deflates this rectangle.</summary>
    ///  <param name="DeltaX">LongInt [in] Amount to inflate or deflate
    ///  horizontally. A +ve value inflates and and a -ve value deflates.
    ///  </param>
    ///  <param name="DeltaY">LongInt [in] Amount to inflate or deflate
    ///  vertically. A +ve value inflates and and a -ve value deflates.</param>
    procedure InflateBy(DeltaX, DeltaY: LongInt);

    ///  <summary>Returns an inflated or deflated copy of this rectangle.
    ///  </summary>
    ///  <param name="DeltaX">LongInt [in] Amount to inflate or deflate
    ///  horizontally. A +ve value inflates and and a -ve value deflates.
    ///  </param>
    ///  <param name="DeltaY">LongInt [in] Amount to inflate or deflate
    ///  vertically. A +ve value inflates and and a -ve value deflates.</param>
    function Inflate(DeltaX, DeltaY: Longint): TRectEx;

    ///  <summary>Offsets this rectangle horizontally and vertically.</summary>
    ///  <param name="AX">LongInt [in] Amount to offset horizontally. A +ve
    ///  value moves right and a -ve value moves left.</param>
    ///  <param name="AY">LongInt [in] Amount to offset vertically. A +ve value
    ///  moves down and a -ve value moves up.</param>
    procedure OffsetBy(AX, AY: Longint);

    ///  <summary>Returns the coordinates of the top left corner of this
    ///  rectangle.</summary>
    function GetTopLeft: TPoint;

    ///  <summary>Sets the coordinates of the top left corner of this rectangle
    ///  to the given value.</summary>
    procedure SetTopLeft(const Value: TPoint);

    ///  <summary>Returns the coordinates of the bottom right corner of this
    ///  rectangle.</summary>
    function GetBottomRight: TPoint;

    ///  <summary>Sets the coordinates of the bottom right corner of this
    ///  rectangle to the given value.</summary>
    procedure SetBottomRight(const Value: TPoint);

    ///  <summary>Checks if the given point is contained in this rectangle.
    ///  </summary>
    function ContainsPoint(const Pt: TPoint): Boolean;

    ///  <summary>Checks if this rectangle is empty.</summary>
    ///  <remarks>Empty is defined as Right &lt;= Left or Bottom &lt;= Top.
    ///  </remarks>
    function IsEmpty: Boolean;

    ///  <summary>Makes this rectangle empty.</summary>
    procedure MakeEmpty;

    ///  <summary>Coordinates of the top left corner of this rectangle.
    ///  </summary>
    property TopLeft: TPoint read GetTopLeft write SetTopLeft;

    ///  <summary>Coordinates of the bottom right corner of this rectangle.
    ///  </summary>
    property BottomRight: TPoint read GetBottomRight write SetBottomRight;
  end;

  ///  <summary>Encapsulates an enumerable range of integers.</summary>
  TRange = record
  strict private
    type
      ///  <summary>Enumerator for TRange.</summary>
      TEnumerator = class(TEnumerator<Integer>)
      strict private
        var
          ///  <summary>Minimum value of range being enumerated.</summary>
          fMin: Integer;
          ///  <summary>Maximum value of range being enumerated.</summary>
          fMax: Integer;
          ///  <summary>Current value in enumeration.</summary>
          fCurrent: Integer;
          ///  <summary>Flag indicating if enumeration has begun.</summary>
          fStarted: Boolean;
      strict protected
        ///  <summary>Gets current value in enumeration.</summary>
        function DoGetCurrent: Integer; override;
        ///  <summary>Moves to next value in enumeration, if any.</summary>
        ///  <returns>Boolean. True if a new value exists or False if there are
        ///  no more values in enumeration.</returns>
        function DoMoveNext: Boolean; override;
      public
        ///  <summary>Constructs new instance of enumerator for given range.
        ///  </summary>
        constructor Create(const Range: TRange);
      end;
  public
    var
      ///  <summary>Minimum bound of range.</summary>
      Min: Integer;
      ///  <summary>Maximum bound of range.</summary>
      Max: Integer;
    ///  <summary>Constructs a new range with given bounds.</summary>
    ///  <remarks>AMin should be less than or equal to AMax.</remarks>
    constructor Create(AMin, AMax: Integer);
    ///  <summary>Checks if the given value is contained within the range.
    ///  </summary>
    function Contains(const Value: Integer): Boolean;
    ///  <summary>Returns the nearest integer to the given value that lies
    ///  within the range.</summary>
    ///  <remarks>Value is returned unchanged if it is within the range. If
    ///  Value is less than Min, Min is returned. If Value is greater then Max,
    ///  Max is returned.</remarks>
    function Constrain(const Value: Integer): Integer;
    ///  <summary>Returns an enumerator that enumerates each integer contained
    ///  in the range.</summary>
    ///  <remarks>The caller must free the enumerator when done.</remarks>
    function GetEnumerator: TEnumerator<Integer>;
  end;

  ///  <summary>Encapsulates a selection defined by a starting position and a
  ///  length.</summary>
  TSelection = record
  public
    var
      ///  <summary>Start of selection.</summary>
      StartPos: Cardinal;
      ///  <summary>Length of selection.</summary>
      Length: Cardinal;
    ///  <summary>Constructs a new selection record with the given starting
    ///  position and optional length.</summary>
    ///  <remarks>If no length is provided the selection will be empty.
    ///  </remarks>
    constructor Create(AStartPos: Cardinal; ALength: Cardinal = 0);
  end;

type
  ///  <summary>Encapsulates a size quantity.</summary>
  ///  <remarks>TSizeEx is assignment compatible with TSize.</remarks>
  TSizeEx = record
  public
    var
      ///  <summary>Width.</summary>
      CX: Integer;
      ///  <summary>Height.</summary>
      CY: Integer;
    ///  <summary>Constructs a new size with given dimensions.</summary>
    ///  <param name="ACX">Integer [in] Width.</param>
    ///  <param name="ACY">Integer [in] Height.</param>
    constructor Create(ACX, ACY: Integer);
    ///  <summary>Casts a TSize record to TSizeEx.</summary>
    class operator Implicit(S: Types.TSize): TSizeEx;
    ///  <summary>Casts a TSizeEx record to TSize.</summary>
    class operator Implicit(S: TSizeEx): Types.TSize;
    ///  <summary>Checks if the two given sizes are equal.</summary>
    ///  <remarks>One of the records being compared may be a TSize.</remarks>
    class operator Equal(S1, S2: TSizeEx): Boolean;
    ///  <summary>Checks if the two given sizes are not equal.</summary>
    ///  <remarks>One of the records being compared may be a TSize.</remarks>
    class operator NotEqual(S1, S2: TSizeEx): Boolean;
    ///  <summary>Checks if the current size is zero.</summary>
    ///  <remarks>A size is zero if either the width or the height are zero.
    ///  </remarks>
    function IsZero: Boolean;
  end;


implementation


uses
  // Delphi
  Math;


{ TRectEx }

function TRectEx.ContainsPoint(const Pt: TPoint): Boolean;
begin
  Result := Types.PtInRect(Self, Pt);
end;

constructor TRectEx.Create(ALeft, ATop, ARight, ABottom: Integer);
begin
  Left := ALeft;
  Top := ATop;
  Right := ARight;
  Bottom := ABottom;
end;

constructor TRectEx.CreateBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  Create(ALeft, ATop, ALeft + AWidth, ATop + AHeight);
end;

constructor TRectEx.CreateBounds(ALeft, ATop: Integer; ASize: TSize);
begin
  CreateBounds(ALeft, ATop, ASize.cx, ASize.cy);
end;

class operator TRectEx.Equal(R1, R2: TRectEx): Boolean;
begin
  Result := (R1.Left = R2.Left) and (R1.Top = R2.Top)
    and (R1.Right = R2.Right) and (R1.Bottom = R2.Bottom);
end;

function TRectEx.GetBottomRight: TPoint;
begin
  Result.X := Right;
  Result.Y := Bottom;
end;

function TRectEx.GetTopLeft: TPoint;
begin
  Result.X := Left;
  Result.Y := Top;
end;

function TRectEx.Height: Longint;
begin
  Result := Abs(Bottom - Top);
end;

class operator TRectEx.Implicit(ARect: TRectEx): TRect;
begin
  Result.Left := ARect.Left;
  Result.Top := ARect.Top;
  Result.Right := ARect.Right;
  Result.Bottom := ARect.Bottom;
end;

class operator TRectEx.Implicit(ARect: TRect): TRectEx;
begin
  Result.Left := ARect.Left;
  Result.Top := ARect.Top;
  Result.Right := ARect.Right;
  Result.Bottom := ARect.Bottom;
end;

function TRectEx.Inflate(DeltaX, DeltaY: Integer): TRectEx;
begin
  Result := Self;
  Result.InflateBy(DeltaX, DeltaY);
end;

procedure TRectEx.InflateBy(DeltaX, DeltaY: Integer);
begin
  Dec(Left, DeltaX);
  Inc(Right, DeltaX);
  Dec(Top, DeltaY);
  Inc(Bottom, DeltaY);
end;

function TRectEx.IsEmpty: Boolean;
begin
  Result := Types.IsRectEmpty(Self);
end;

procedure TRectEx.MakeEmpty;
begin
  Left := 0;
  Top := 0;
  Right := 0;
  Bottom := 0;
end;

class operator TRectEx.NotEqual(R1, R2: TRectEx): Boolean;
begin
  Result := not (R1 = R2);
end;

procedure TRectEx.OffsetBy(AX, AY: Integer);
begin
  Inc(Left, AX);
  Inc(Right, AX);
  Inc(Top, AY);
  Inc(Bottom, AY);
end;

procedure TRectEx.SetBottomRight(const Value: TPoint);
begin
  Right := Value.X;
  Bottom := Value.Y;
end;

procedure TRectEx.SetTopLeft(const Value: TPoint);
begin
  Left := Value.X;
  Top := Value.Y;
end;

function TRectEx.Width: Longint;
begin
  Result := Abs(Right - Left);
end;

{ TRange }

function TRange.Constrain(const Value: Integer): Integer;
begin
  if Value < Min then
    Exit(Min);
  if Value > Max then
    Exit(Max);
  Result := Value;
end;

function TRange.Contains(const Value: Integer): Boolean;
begin
  Result := Math.InRange(Value, Min, Max);
end;

constructor TRange.Create(AMin, AMax: Integer);
begin
  Min := AMin;
  Max := AMax;
end;

function TRange.GetEnumerator: TEnumerator<Integer>;
begin
  Result := TEnumerator.Create(Self);
end;

{ TRange.TEnumerator }

constructor TRange.TEnumerator.Create(const Range: TRange);
begin
  inherited Create;
  fMin := Range.Min;
  fMax := Range.Max;
  fCurrent := -MaxInt;
  fStarted := False;
end;

function TRange.TEnumerator.DoGetCurrent: Integer;
begin
  Result := fCurrent;
end;

function TRange.TEnumerator.DoMoveNext: Boolean;
begin
  if fCurrent = fMax then
    Exit(False);
  if not fStarted then
  begin
    fCurrent := fMin;
    fStarted := True;
  end
  else
    Inc(fCurrent);
  Result := True;
end;

{ TSelection }

constructor TSelection.Create(AStartPos, ALength: Cardinal);
begin
  StartPos := AStartPos;
  Length := ALength;
end;

{ TSizeEx }

constructor TSizeEx.Create(ACX, ACY: Integer);
begin
  CX := ACX;
  CY := ACY;
end;

class operator TSizeEx.Equal(S1, S2: TSizeEx): Boolean;
begin
  // zero records are special: can be zero when only one of CX or CY is zero
  if S1.IsZero and S2.IsZero then
  begin
    Result := True;
    Exit;
  end;
  Result := (S1.CX = S1.CX) and (S1.CY = S2.CY);
end;

class operator TSizeEx.Implicit(S: Types.TSize): TSizeEx;
begin
  Result.CX := S.cx;
  Result.CY := S.cy;
end;

class operator TSizeEx.Implicit(S: TSizeEx): Types.TSize;
begin
  Result.cx := S.CX;
  Result.cy := S.CY;
end;

function TSizeEx.IsZero: Boolean;
begin
  Result := (CX = 0) or (CY = 0);
end;

class operator TSizeEx.NotEqual(S1, S2: TSizeEx): Boolean;
begin
  Result := not (S1 = S2);
end;

end.

