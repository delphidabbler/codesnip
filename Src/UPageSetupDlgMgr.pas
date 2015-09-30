{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a class that manages the Page Setup dialogue box.
}


unit UPageSetupDlgMgr;


interface


uses
  // Delphi
  Classes,
  // Project
  UBaseObjects;


type

  {
  TPageSetupDlgMgr:
    Class that manages the Page Setup dialogue box. The class sets the
    dialogue's properties, converting between units used by application and
    units used by dialogue box. It also persists information entered by user in
    the dialogue box.
  }
  TPageSetupDlgMgr = class(TNoConstructObject)
  strict private
    class function MMToDlgUnits(const Value: Double): Integer;
      {Converts millimeters to units required by Page Setup dialogue.
        @param Value [in] Millimeters to convert.
        @return Converted value in dialogue box units.
      }
    class function DlgUnitsToMM(const Value: Integer): Double;
      {Converts units used by Page Setup dialogue into millimeters.
        @param Value [in] Dialogue box units to convert.
        @return Converted value in millimeters.
      }
  public
    class function Execute(AOwner: TComponent): Boolean;
      {Configures and displays page setup dialogue box and updates persistent
      page setup properties as required.
        @return True if user OKs dialogue and False if user cancels.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Dialogs,
  // Project
  UExceptions, UMeasurement, UPageSetupDialogEx, UPreferences, UPrintInfo;


{ TPageSetupDlgMgr }

class function TPageSetupDlgMgr.DlgUnitsToMM(const Value: Integer): Double;
  {Converts units used by Page Setup dialogue into millimeters.
    @param Value [in] Dialogue box units to convert.
    @return Converted value in millimeters.
  }
const
  // Bug error message
  cBadValueBug = '.DlgUnitsToMM: Unexpected TMeasurementUnits value';
begin
  // See comments in MMToDlgUnits for details of how dialogue stores values
  case Preferences.MeasurementUnits of
    muInches: Result := InchesToMM(Value / 1000.0);
    muMillimeters: Result := Value / 100.0;
    else raise EBug.Create(ClassName + cBadValueBug);
  end;
end;

class function TPageSetupDlgMgr.Execute(AOwner: TComponent): Boolean;
  {Configures and displays page setup dialogue box and updates persistent page
  setup properties as required.
    @return True if user OKs dialogue and False if user cancels.
  }

var
  Dlg: TPageSetupDialogEx;  // object that encapsulates page setup dialogue

  // ---------------------------------------------------------------------------
  procedure SetDlgMargins;
    {Sets dialogue's margins properties from persistent page margins.
    }
  begin
    Dlg.MarginLeft := MMToDlgUnits(PrintInfo.PageMargins.Left);
    Dlg.MarginTop := MMToDlgUnits(PrintInfo.PageMargins.Top);
    Dlg.MarginRight := MMToDlgUnits(PrintInfo.PageMargins.Right);
    Dlg.MarginBottom := MMToDlgUnits(PrintInfo.PageMargins.Bottom);
  end;

  procedure UpdatePageMargins;
    {Update persistent page margins per values entered in dialogue box.
    }
  begin
    PrintInfo.PageMargins := TPageMargins.Create(
      DlgUnitsToMM(Dlg.MarginLeft),
      DlgUnitsToMM(Dlg.MarginTop),
      DlgUnitsToMM(Dlg.MarginRight),
      DlgUnitsToMM(Dlg.MarginBottom)
    );
  end;
  // ---------------------------------------------------------------------------

begin
  Dlg := TPageSetupDialogEx.Create(AOwner);
  try
    // Set dialogue box's margin properties.
    // We don't pass other values, like page size, to dialogue box since it
    // automatically gets these from printer's document properties.
    SetDlgMargins;
    // Store required units of measurement
    case Preferences.MeasurementUnits of
      muInches: Dlg.Units := pmInches;
      muMillimeters: Dlg.Units := pmMillimeters;
    end;
    // Show Help button to access specified topic and disable Printers button
    Dlg.Options := [psoShowHelp, psoDisablePrinter, psoMargins];
    Dlg.HelpKeyword := 'PageSetupDlg';
    // Display dialogue box
    Result := Dlg.Execute;
    if Result then
      // Update page margins. We convert back to mm since we always use mm when
      // persisting margins.
      // We don't need to record other values since dialogue box automatically
      // updates current printer's document properties to reflect changes.
      UpdatePageMargins;
  finally
    Dlg.Free;
  end;
end;

class function TPageSetupDlgMgr.MMToDlgUnits(const Value: Double): Integer;
  {Converts millimeters to units required by Page Setup dialogue.
    @param Value [in] Millimeters to convert.
    @return Converted value in dialogue box units.
  }
const
  // Bug error message
  cBadValueBug = '.MMToDlgUnits: Unexpected TMeasurementUnits value';
begin
  // Page Setup dialogue stores different values depending on units of
  // measurement being used. If millimeters are being used, dialogue box stores
  // values in 100ths of a millimeter. If inches are being used values are
  // stored in 1000th of an inch.
  case Preferences.MeasurementUnits of
    muInches: Result := Round(1000.0 * MMToInches(Value));
    muMillimeters: Result := Round(100 * Value);
    else raise EBug.Create(ClassName + cBadValueBug);
  end;
end;

end.

