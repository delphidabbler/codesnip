{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2011-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a base class for all frames used for editing compiler information
 * in TCompilersDlg.
}


unit FmCompilersDlg.FrBase;


interface


uses
  // Delphi
  Classes, Forms,
  // Project
  Compilers.UGlobals;


type
  ///  <summary>
  ///  Base class for all frames used for editing compiler information in
  ///  TCompilersDlg.
  ///  </summary>
  TCompilersDlgBaseFrame = class(TFrame)
  strict private
    var
      ///  <summary>Value of Compilers property.</summary>
      fCompiler: ICompiler;
      ///  <summary>OnChange event handler.</summary>
      fOnChange: TNotifyEvent;
    ///  <summary>Write accessor for Compiler property. Records compiler and
    ///  initialises controls using its properties.</summary>
    procedure SetCompiler(Value: ICompiler);
  strict protected
    ///  <summary>Initialises frame to display details of current compiler.
    ///  </summary>
    procedure Initialise; virtual; abstract;
    ///  <summary>Triggers OnChange event.</summary>
    procedure DoChange;
  public
    ///  <summary>Arranges controls in frame.</summary>
    procedure ArrangeControls; virtual; abstract;
    ///  <summary>Updates current compiler object with edited information.
    ///  </summary>
    procedure UpdateCompiler; virtual; abstract;
    ///  <summary>Compiler to be edited.</summary>
    property Compiler: ICompiler read fCompiler write SetCompiler;
    ///  <summary>Event triggered when compiler details are edited.</summary>
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;


implementation


{$R *.dfm}


{ TCompilersDlgBaseFrame }

procedure TCompilersDlgBaseFrame.DoChange;
begin
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

procedure TCompilersDlgBaseFrame.SetCompiler(Value: ICompiler);
begin
  Assert(Assigned(Value), ClassName + '.SetCompiler: Value is nil');
  fCompiler := Value;
  Initialise;
end;

end.
