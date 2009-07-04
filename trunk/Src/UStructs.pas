{
 * UStructs.pas
 *
 * Contains various structure with added functionality.
 *
 * v1.0 of 14 Dec 2008  - Original version containing TRectEx.
 * v1.1 of 15 Dec 2008  - Added numerous additional methods, properties and
 *                        casts to TRectEx.
 * v1.2 of 28 Dec 2008  - Added new TRange record with methods.
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
 * The Original Code is UStructs.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UStructs;


interface


uses
  // Delphi
  Types;


type

  {
  TRectEx:
    Encapsulates a rectangle. Assignment compatible with TRect. Has methods to
    manipulate and cast rectangle.
  }
  TRectEx = packed record
    Left, Top,              // coordinates of top left corner of rectangle
    Right, Bottom: Longint; // coordinates of bottom right corner of rectangle
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
      {Implicit cast of TRect to a TRectEx.
        @param ARect [in] TRectEx to be cast.
        @return TRect resulting from cast.
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
    Min, Max: Integer;  // minimum and maximum bounds of range
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
  end;


implementation


uses
  // Delphi
  Math;


{ TRectEx }

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
  {Implicit cast of TRect to a TRectEx.
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

end.

