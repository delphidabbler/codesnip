{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Contains routines and enumerations relating to measurement and conversion
 * between measurement systems.
}


unit UMeasurement;


interface


type

  {
  TAxis:
    Enumeration representing X and Y axes.
  }
  TAxis = (
    axX,  // X-axis
    axY   // Y-axis
  );

  {
  TMeasurementUnits:
    Enumeration of measurement units supported by system.
  }
  TMeasurementUnits = (
    muInches,       // measurement in inches
    muMillimeters   // measurement in millimeters
  );

function InchesToMM(const Inches: Double): Double;
  {Converts inches to millimeters.
    @param Inches [in] Inches to be converted.
    @return Value in millimeters.
  }

function MMToInches(const MM: Double): Double;
  {Converts millimeters to inches.
    @param MM [in] Millimeters to be converted.
    @return Value in inches.
  }

function InchesToPixels(const HDC: THandle; const Inches: Double;
  const Axis: TAxis): Integer;
  {Converts from inches to pixels in a particular device context.
    @param HDC [in] Device context for which number of pixels are required.
    @param Inches [in] Number of inches to be converted.
    @param Axis [in] Orientation of pixel measurement.
    @return Number of pixels.
  }

function DefaultMeasurementUnits: TMeasurementUnits;
  {Gets the default measurement units in the user locale.
    @return Required measurement units.
  }

function UnitName(const AUnit: TMeasurementUnits): string;
  {Gets description name of units.
    @param AUnit [in] Unit for which we want name.
    @return Name of unit.
  }


implementation


uses
  // Delphi
  ConvUtils, StdConvs, Windows, SysUtils,
  // Project
  UExceptions, ULocales;


function DefaultMeasurementUnits: TMeasurementUnits;
  {Gets the default measurement units in the user locale.
    @return Required measurement units.
  }
var
  MUStr: string;  // measurement unit code as a string
begin
  // We get default measurement for user's locale by passing LOCALE_IMEASURE to
  // GetLocaleInfo, which returns '0' for metric and '1' for imperial systems.
  MUStr := GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IMEASURE);
  case StrToInt(MUStr) of
    0:
      Result := muMillimeters;
    1:
      Result := muInches;
    else
      raise EBug.CreateFmt(
        'DefaultMeasurementUnits: Unexpected locale value: "%s"', [MUStr]
      );
  end;
end;

function InchesToMM(const Inches: Double): Double;
  {Converts inches to millimeters.
    @param Inches [in] Inches to be converted.
    @return Value in millimeters.
  }
begin
  Result := Convert(Inches, duInches, duMillimeters);
end;

function MMToInches(const MM: Double): Double;
  {Converts millimeters to inches.
    @param MM [in] Millimeters to be converted.
    @return Value in inches.
  }
begin
  Result := Convert(MM, duMillimeters, duInches);
end;

function InchesToPixels(const HDC: THandle; const Inches: Double;
  const Axis: TAxis): Integer;
  {Converts from inches to pixels in a particular device context.
    @param HDC [in] Device context for which number of pixels are required.
    @param Inches [in] Number of inches to be converted.
    @param Axis [in] Orientation of pixel measurement.
    @return Number of pixels.
  }
var
  LogPixels: Integer; // logical pixels per inch
const
  // Map of axis to GetDeviceCaps index used to get logical pixels per inch
  cLogPixelIdx: array[TAxis] of Integer = (LOGPIXELSX, LOGPIXELSY);
begin
  LogPixels := GetDeviceCaps(HDC, cLogPixelIdx[Axis]);
  Result := Round(LogPixels * Inches);
end;

function UnitName(const AUnit: TMeasurementUnits): string;
  {Gets description name of units.
    @param AUnit [in] Unit for which we want name.
    @return Name of unit.
  }
resourcestring
  // Names of measurement units
  sInches = 'Inches';
  sMillimeters = 'Millimeters';
const
  // Maps units to names
  cUnitNames: array[TMeasurementUnits] of string = (sInches, sMillimeters);
begin
  Result :=cUnitNames[AUnit];
end;

end.

