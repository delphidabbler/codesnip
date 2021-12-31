{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Class that manages a progress bar that is displayed within a memo control and
 * aligned to text in the control.
}


unit UMemoProgBarMgr;


interface


uses
  // Delphi
  StdCtrls, ComCtrls;


type

  {
  TMemoProgBarMgr:
    Manages a progress bar that is displayed within a memo control and aligned
    to text in the control.
  }
  TMemoProgBarMgr = class(TObject)
  strict private
    fMemo: TMemo;
      {Memo control in which progress bar is displayed}
    fProgressBar: TProgressBar;
      {Reference to displayed progress bar. Nil when not displayed}
    fPosition: Integer;
      {Value of progress bar's Position property. Persists value when progress
      bar is not instantiated}
    fMax: Integer;
      {Value of progress bar's Max property. Persists value when progress bar is
      not instantiated}
    procedure SetMax(const Value: Integer);
      {Write accessor for Max property.
        @param Value [in] Required property value.
      }
    procedure SetPosition(const Value: Integer);
      {Write accessor for Position property.
        @param Value [in] Required property value.
      }
    procedure SetBounds(const MemoLine: Integer);
      {Sets bounds of progress bar to fit after a memo line.
        @param MemoLine [in] Index of memo line after which progress bar is
          displayed.
      }
  public
    constructor Create(const Memo: TMemo);
      {Class constructor. Creates object that displays progress bar in a
      specified memo.
        @param Memo [in] Memo to contain progress bar.
      }
    destructor Destroy; override;
      {Class destructor. Ensures progress bar is removed from memo control.
      }
    procedure Show(const MemoLine: Integer);
      {Displays progress bar adjacent to specified line in memo control.
        @param MemoLine [in] Index of memo line after which progress bar is
          displayed.
      }
    procedure Hide;
      {Hides progress bar.
      }
    property Max: Integer read fMax write SetMax;
      {Maximum value displayed by progress bar}
    property Position: Integer read fPosition write SetPosition;
      {Position of progress bar}
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UMemoHelper;


{ TMemoProgBarMgr }

constructor TMemoProgBarMgr.Create(const Memo: TMemo);
  {Class constructor. Creates object that displays progress bar in a specified
  memo.
    @param Memo [in] Memo to contain progress bar.
  }
begin
  Assert(Assigned(Memo), ClassName + '.Create: Memo is nil');
  inherited Create;
  fMemo := Memo;
end;

destructor TMemoProgBarMgr.Destroy;
  {Class destructor. Ensures progress bar is removed from memo control.
  }
begin
  Hide;
  inherited;
end;

procedure TMemoProgBarMgr.Hide;
  {Hides progress bar.
  }
begin
  FreeAndNil(fProgressBar);
end;

procedure TMemoProgBarMgr.SetBounds(const MemoLine: Integer);
  {Sets bounds of progress bar to fit after a memo line.
    @param MemoLine [in] Index of memo line after which progress bar is
      displayed.
  }
var
  MemoHelper: TMemoHelper;  // object providing info about memo control
  Top: Integer;             // top of progress bar
  Left: Integer;            // left of progress bar
  Width: Integer;           // width of progress bar
  Height: Integer;          // height of progress bar
begin
  Assert(Assigned(fProgressBar), ClassName + '.SetBounds: fProgressBar is nil');
  MemoHelper := TMemoHelper.Create(fMemo);
  try
    // Progress bar is placed after end of text on memo line and sized to fit
    // between end of line and right of memo control client area
    Left := MemoHelper.LineLeft(MemoLine) + MemoHelper.LineWidth(MemoLine);
    Top := MemoHelper.LineTop(MemoLine) + 1;
    Width := fMemo.ClientWidth - Left - 2;
    Height := MemoHelper.LineHeight(MemoLine);
    fProgressbar.SetBounds(Left, Top, Width, Height);
  finally
    FreeAndNil(MemoHelper);
  end;
end;

procedure TMemoProgBarMgr.SetMax(const Value: Integer);
  {Write accessor for Max property.
    @param Value [in] Required property value.
  }
begin
  fMax := Value;
  if Assigned(fProgressBar) then
    fProgressBar.Max := fMax;
end;

procedure TMemoProgBarMgr.SetPosition(const Value: Integer);
  {Write accessor for Position property.
    @param Value [in] Required property value.
  }
begin
  fPosition := Value;
  if Assigned(fProgressBar) then
    fProgressBar.Position := fPosition;
end;

procedure TMemoProgBarMgr.Show(const MemoLine: Integer);
  {Displays progress bar adjacent to specified line in memo control.
    @param MemoLine [in] Index of memo line after which progress bar is
      displayed.
  }
begin
  fProgressBar := TProgressBar.Create(fMemo);
  fProgressBar.Parent := fMemo;
  fProgressBar.Min := 0;
  fProgressBar.Max := fMax;
  fProgressBar.Position := fPosition;
  fProgressBar.Smooth := True;
  SetBounds(MemoLine);
end;

end.

