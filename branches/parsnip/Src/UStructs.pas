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

  {
  TRectEx:
    Encapsulates a rectangle. Assignment compatible with TRect. Has methods to
    manipulate and cast rectangle.
  }
  TRectEx = packed record
    Left, Top,              // Coordinates of top left corner of rectangle
    Right, Bottom: Longint; // Coordinates of bottom right corner of rectangle
    constructor Create(ALeft, ATop, ARight, ABottom: Longint);
      {Record constructor. Sets initial field values.
        @param ALeft [in] Left position of rectangle.
        @param ATop [in] Top position of rectangle.
        @param ARight [in] Right position of rectangle.
        @param ABottom [in] Bottom position of rectangle.
      }
    constructor CreateBounds(ALeft, ATop, AWidth, AHeight: Longint); overload;
      {Record constructor. Creates rectangle from its bounds.
        @param ALeft [in] Left position of rectangle.
        @param ATop [in] Top position of rectangle.
        @param AWidth [in] Width of rectangle.
        @param AHeight [in] Height of rectangle.
      }
    constructor CreateBounds(ALeft, ATop: Longint; ASize: TSize); overload;
      {Record constructor. Creates rectangle from its bounds.
        @param ALeft [in] Left position of rectangle.
        @param ATop [in] Top position of rectangle.
        @param ASize [in] Size of rectangle.
      }
    class operator Implicit(ARect: TRect): TRectEx;
      {Implicit cast of TRect to a TRectEx.
        @param ARect [in] TRect to be cast.
        @return TRectEx resulting from cast.
      }
    class operator Implicit(ARect: TRectEx): TRect;
      {Implicit cast of TRectEx to a TRect.
        @param ARect [in] TRectEx to be cast.
        @return TRect resulting from cast.
      }
    class operator Equal(R1, R2: TRectEx): Boolean;
      {Equality test for two TRectEx rectangles.
        @param R1 [in] Left hand operand.
        @param R2 [in] Right hand operand.
        @return True if R1 is the same as R2, False if different.
      }
    class operator NotEqual(R1, R2: TRectEx): Boolean;
      {Inequality test for two TRectEx rectangles.
        @param R1 [in] Left hand operand.
        @param R2 [in] Right hand operand.
        @return True if R1 is different to R2, False if same.
      }
    function Width: Longint;
      {Width of rectangle.
        @return Required width.
      }
    function Height: Longint;
      {Height of rectangle.
        @return Required height.
      }
    procedure InflateBy(DeltaX, DeltaY: Longint);
      {Inflates or deflates the rectangle.
        @param DeltaX [in] Amount to inflate horizontally. Deflated if -ve.
        @param DeltaY [in] Amount to inflate vertically. Deflated if -ve.
      }
    function Inflate(DeltaX, DeltaY: Longint): TRectEx;
      {Returns a copy of rectangle, inflated or deflated.
        @param DeltaX [in] Amount to inflate horizontally. Deflated if -ve.
        @param DeltaY [in] Amount to inflate vertically. Deflated if -ve.
        @return Inflated copy of rectangle.
      }
    procedure OffsetBy(AX, AY: Longint);
      {Offsets rectangle in X and Y directions.
        @param AX [in] Horizontal offset.
        @param AY [in] Vertical offset.
      }
    function GetTopLeft: TPoint;
      {Read accessor for TopLeft property.
        @return Co-ordinates of top left of rectangle.
      }
    procedure SetTopLeft(const Value: TPoint);
      {Write accessor for TopLeft property.
        @param Value [in] Coordinates of top left.
      }
    function GetBottomRight: TPoint;
      {Read accessor for BottomRight property.
        @return Co-ordinates of bottom right of rectangle.
      }
    procedure SetBottomRight(const Value: TPoint);
      {Write accessor for BottomRight property.
        @param Value [in] Coordinates of bottom right.
      }
    function ContainsPoint(const Pt: TPoint): Boolean;
      {Checks if a point is contained in the rectangle.
        @param Pt [in] Point to be tested.
        @return True if point is contained in the rectangle, False if not.
      }
    function IsEmpty: Boolean;
      {Checks if a rectangle is empty. Empty is defined as Right <= Left or
      Bottom <= Top.
        return True if empty, False if not.
      }
    procedure MakeEmpty;
      {Makes the rectangle empty.
      }
    property TopLeft: TPoint read GetTopLeft write SetTopLeft;
      {Coordinates of top left corner of rectangle}
    property BottomRight: TPoint read GetBottomRight write SetBottomRight;
      {Coordinates of bottom right corner of rectangle}
  end;

  {
  TRange:
    Encapsulates a range of integers with a method to test wheter a value falls
    within the range.
  }
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
    Min, Max: Integer;  // Minimum and maximum bounds of range
    constructor Create(AMin, AMax: Integer);
      {Record constructor. Initialises range bounds.
        @param AMin [in] Minimum value that falls in range.
        @param AMax [in] Maximum value that falls in range.
      }
    function Contains(const Value: Integer): Boolean;
      {Checks if a value is contained in range.
        @param Value [in] Value to be tested.
        @return True if Value is in range, False otherwise.
      }
    function Constrain(const Value: Integer): Integer;
      {Adjusts the given value to ensure it falls within the range.
        @param Value [in] Value to be constrained.
        @returns Value is returned unchanged if it falls within the range.
          Otherwise Min is returned if Value is less than Min or Max is returned
          if Value is greater than max.
      }
    ///  <summary>Returns an enumerator that enumerates each integer contained
    ///  in the range.</summary>
    ///  <remarks>The caller must free the enumerator when done.</remarks>
    function GetEnumerator: TEnumerator<Integer>;
  end;

  {
  TSelection:
    Encapsulates a selection defined by its starting position and its length.
    An empty selection has length 0.
  }
  TSelection = record
  public
    StartPos: Cardinal; // Start of selection
    Length: Cardinal;   // Length of selection
    constructor Create(AStartPos: Cardinal; ALength: Cardinal = 0);
      {Record constructor. Initialises start position and length.
        @param AStartPos [in] Start of selection.
        @param Length [in] Length of selection. Optional.
      }
  end;

type
  // Encapsulates a size quantity. Assignment compatible with TSize. Has methods
  // to compare and cast size.
  TSizeEx = record
  public
    // Width
    CX: Integer;
    // Height
    CY: Integer;
    // Constructs record with two given CX and CY field values
    constructor Create(ACX, ACY: Integer);
    // Enables TSize to be assigned to and compared with TSizeEx
    class operator Implicit(S: Types.TSize): TSizeEx;
    // Enables TSizeEx to be assigned to and compared with TSize
    class operator Implicit(S: TSizeEx): Types.TSize;
    // Tests for equality of TSizeEx records. Also works if one record is TSize.
    class operator Equal(S1, S2: TSizeEx): Boolean;
    // Tests for inequality of TSizeEx records. Also works if one record is
    // TSize.
    class operator NotEqual(S1, S2: TSizeEx): Boolean;
    // Tests if a TSizeEx instance is zero (i.e. one of fields is zero)
    function IsZero: Boolean;
  end;


implementation


uses
  // Delphi
  Math;


{ TRectEx }

function TRectEx.ContainsPoint(const Pt: TPoint): Boolean;
  {Checks if a point is contained in the rectangle.
    @param Pt [in] Point to be tested.
    @return True if point is contained in the rectangle, False if not.
  }
begin
  Result := Types.PtInRect(Self, Pt);
end;

constructor TRectEx.Create(ALeft, ATop, ARight, ABottom: Integer);
  {Record constructor. Sets initial field values.
    @param ALeft [in] Left position of rectangle.
    @param ATop [in] Top position of rectangle.
    @param ARight [in] Right position of rectangle.
    @param ABottom [in] Bottom position of rectangle.
  }
begin
  Left := ALeft;
  Top := ATop;
  Right := ARight;
  Bottom := ABottom;
end;

constructor TRectEx.CreateBounds(ALeft, ATop, AWidth, AHeight: Integer);
  {Record constructor. Creates rectangle from its bounds.
    @param ALeft [in] Left position of rectangle.
    @param ATop [in] Top position of rectangle.
    @param AWidth [in] Width of rectangle.
    @param AHeight [in] Height of rectangle.
  }
begin
  Create(ALeft, ATop, ALeft + AWidth, ATop + AHeight);
end;

constructor TRectEx.CreateBounds(ALeft, ATop: Integer; ASize: TSize);
  {Record constructor. Creates rectangle from its bounds.
    @param ALeft [in] Left position of rectangle.
    @param ATop [in] Top position of rectangle.
    @param ASize [in] Size of rectangle.
  }
begin
  CreateBounds(ALeft, ATop, ASize.cx, ASize.cy);
end;

class operator TRectEx.Equal(R1, R2: TRectEx): Boolean;
  {Equality test for two TRectEx rectangles.
    @param R1 [in] Left hand operand.
    @param R2 [in] Right hand operand.
    @return True if R1 is the same as R2, False if different.
  }
begin
  Result := (R1.Left = R2.Left) and (R1.Top = R2.Top)
    and (R1.Right = R2.Right) and (R1.Bottom = R2.Bottom);
end;

function TRectEx.GetBottomRight: TPoint;
  {Read accessor for BottomRight property.
    @return Co-ordinates of bottom right of rectangle.
  }
begin
  Result.X := Right;
  Result.Y := Bottom;
end;

function TRectEx.GetTopLeft: TPoint;
  {Read accessor for TopLeft property.
    @return Co-ordinates of top left of rectangle.
  }
begin
  Result.X := Left;
  Result.Y := Top;
end;

function TRectEx.Height: Longint;
  {Height of rectangle.
    @return Required height.
  }
begin
  Result := Abs(Bottom - Top);
end;

class operator TRectEx.Implicit(ARect: TRectEx): TRect;
  {Implicit cast of TRectEx to a TRect.
    @param ARect [in] TRectEx to be cast.
    @return TRect resulting from cast.
  }
begin
  Result.Left := ARect.Left;
  Result.Top := ARect.Top;
  Result.Right := ARect.Right;
  Result.Bottom := ARect.Bottom;
end;

class operator TRectEx.Implicit(ARect: TRect): TRectEx;
  {Implicit cast of TRect to a TRectEx.
    @param ARect [in] TRect to be cast.
    @return TRectEx resulting from cast.
  }
begin
  Result.Left := ARect.Left;
  Result.Top := ARect.Top;
  Result.Right := ARect.Right;
  Result.Bottom := ARect.Bottom;
end;

function TRectEx.Inflate(DeltaX, DeltaY: Integer): TRectEx;
  {Returns a copy of rectangle, inflated or deflated.
    @param DeltaX [in] Amount to inflate horizontally. Deflated if -ve.
    @param DeltaY [in] Amount to inflate vertically. Deflated if -ve.
    @return Inflated copy of rectangle.
  }
begin
  Result := Self;
  Result.InflateBy(DeltaX, DeltaY);
end;

procedure TRectEx.InflateBy(DeltaX, DeltaY: Integer);
  {Inflates or deflates the rectangle.
    @param DeltaX [in] Amount to inflate horizontally. Deflated if -ve.
    @param DeltaY [in] Amount to inflate vertically. Deflated if -ve.
  }
begin
  Dec(Left, DeltaX);
  Inc(Right, DeltaX);
  Dec(Top, DeltaY);
  Inc(Bottom, DeltaY);
end;

function TRectEx.IsEmpty: Boolean;
  {Checks if a rectangle is empty. Empty is defined as Right <= Left or
  Bottom <= Top.
    return True if empty, False if not.
  }
begin
  Result := Types.IsRectEmpty(Self);
end;

procedure TRectEx.MakeEmpty;
  {Makes the rectangle empty.
  }
begin
  Left := 0;
  Top := 0;
  Right := 0;
  Bottom := 0;
end;

class operator TRectEx.NotEqual(R1, R2: TRectEx): Boolean;
  {Inequality test for two TRectEx rectangles.
    @param R1 [in] Left hand operand.
    @param R2 [in] Right hand operand.
    @return True if R1 is different to R2, False if same.
  }
begin
  Result := not (R1 = R2);
end;

procedure TRectEx.OffsetBy(AX, AY: Integer);
  {Offsets rectangle in X and Y directions.
    @param AX [in] Horizontal offset.
    @param AY [in] Vertical offset.
  }
begin
  Inc(Left, AX);
  Inc(Right, AX);
  Inc(Top, AY);
  Inc(Bottom, AY);
end;

procedure TRectEx.SetBottomRight(const Value: TPoint);
  {Write accessor for BottomRight property.
    @param Value [in] Coordinates of bottom right.
  }
begin
  Right := Value.X;
  Bottom := Value.Y;
end;

procedure TRectEx.SetTopLeft(const Value: TPoint);
  {Write accessor for TopLeft property.
    @param Value [in] Coordinates of top left.
  }
begin
  Left := Value.X;
  Top := Value.Y;
end;

function TRectEx.Width: Longint;
  {Width of rectangle.
    @return Required width.
  }
begin
  Result := Abs(Right - Left);
end;

{ TRange }

function TRange.Constrain(const Value: Integer): Integer;
  {Adjusts the given value to ensure it falls within the range.
    @param Value [in] Value to be constrained.
    @returns Value is returned unchanged if it falls within the range. Otherwise
      Min is returned if Value is less than Min or Max is returned if Value is
      greater than max.
  }
begin
  if Value < Min then
    Exit(Min);
  if Value > Max then
    Exit(Max);
  Result := Value;
end;

function TRange.Contains(const Value: Integer): Boolean;
  {Checks if a value is contained in range.
    @param Value [in] Value to be tested.
    @return True if Value is in range, False otherwise.
  }
begin
  Result := Math.InRange(Value, Min, Max);
end;

constructor TRange.Create(AMin, AMax: Integer);
  {Record constructor. Initialises range bounds.
    @param AMin [in] Minimum value that falls in range.
    @param AMax [in] Maximum value that falls in range.
  }
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
  {Record constructor. Initialises start position and length.
    @param AStartPos [in] Start of selection.
    @param Length [in] Length of selection. Optional.
  }
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

