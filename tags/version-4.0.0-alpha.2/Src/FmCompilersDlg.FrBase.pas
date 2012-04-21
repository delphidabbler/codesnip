{
 * FmCompilersDlg.FrBase.pas
 *
 * Implements a Base class for all frames used for editing compiler information
 * in TCompilersDlg.
 *
 * $Rev$
 * $Date$
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
 * The Original Code is FmCompilersDlg.FrBase.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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
