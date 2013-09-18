{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a singleton object that maintains printer and page information for
 * lifetime of program. This object maintains information that is not maintained
 * by the Printer object.
}


unit UPrintInfo;


interface


uses
  // Project
  USingleton;


type

  {
  TPageMargins:
    Record that defines page margins in millimeters.
  }
  TPageMargins = record
    Left, Top, Right, Bottom: Double; // size of each page margin
    constructor Create(ALeft, ATop, ARight, ABottom: Double);
      {Record constructor. Sets initial field values.
        @param ALeft [in] Left margin size.
        @param ATop [in] Top margin size.
        @param ARight [in] Right margin size.
        @param ABottom [in] Bottom margin size.
      }
  end;

  {
  TPrintOption:
    Enumeration of possible print options.
  }
  TPrintOption = (
    poSyntaxHilite, // printed source code will be syntax highlighted
    poUseColour     // colour will be used in printed document
  );

  {
  TPrintOptions:
    Set of print options.
  }
  TPrintOptions = set of TPrintOption;

  {
  TPrintInfo:
    Class that maintains printer and page information for lifetime of
    application. This class stores information that the Printer object doesn't
    maintain.
  }
  TPrintInfo = class(TSingleton)
  strict private
    var
      fPageMargins: TPageMargins;
        {Stores current page margins in millimeters}
      fPrintOptions: TPrintOptions;
        {Stores current print options}
    class function GetInstance: TPrintInfo; static;
      {Returns singletion IPrintInfo object initialised from persistent storage.
        @return Object instance.
      }
  strict protected
    ///  <summary>Initialises singleton object on creation.</summary>
    procedure Initialize; override;
    ///  <summary>Finalises singleton object on destruction.</summary>
    procedure Finalize; override;
  public
    class property Instance: TPrintInfo read GetInstance;
      {Reference to singleton instance of this class}
    property PageMargins: TPageMargins
      read fPageMargins write fPageMargins;
      {Stores current page margins, in millimeters}
    property PrintOptions: TPrintOptions
      read fPrintOptions write fPrintOptions;
      {Stores current print options}
  end;


function PrintInfo: TPrintInfo;
  {Provides access to a singleton implementation of IPrintInfo.
    @return Singletion object instance.
  }


implementation


uses
  // Delphi
  USettings;


function PrintInfo: TPrintInfo;
  {Provides access to a singleton implementation of IPrintInfo.
    @return Singletion object instance.
  }
begin
  Result := TPrintInfo.Instance;
end;


{ TPrintInfo }

procedure TPrintInfo.Finalize;
var
  Storage: ISettingsSection;
begin
  inherited;
  Storage := Settings.EmptySection(ssPrinting);
  Storage.SetBoolean('UseColour', poUseColour in fPrintOptions);
  Storage.SetBoolean('SyntaxHighlight', poSyntaxHilite in fPrintOptions);
  Storage.SetFloat('LeftMargin', fPageMargins.Left);
  Storage.SetFloat('TopMargin', fPageMargins.Top);
  Storage.SetFloat('RightMargin', fPageMargins.Right);
  Storage.SetFloat('BottomMargin', fPageMargins.Bottom);
  Storage.Save;
end;

class function TPrintInfo.GetInstance: TPrintInfo;
  {Returns singletion IPrintInfo object initialised from persistent storage.
    @return Object instance.
  }
begin
  Result := TPrintInfo.Create;
end;

procedure TPrintInfo.Initialize;
const
  // Default margin size in millimeters
  cPageMarginSizeMM = 25.0;
var
  Storage: ISettingsSection;
begin
  inherited;
  // Read printing section
  Storage := Settings.ReadSection(ssPrinting);
  fPrintOptions := [];
  if Storage.GetBoolean('UseColour', True) then
    Include(fPrintOptions, poUseColour);
  if Storage.GetBoolean('SyntaxHighlight', True) then
    Include(fPrintOptions, poSyntaxHilite);
  fPageMargins := TPageMargins.Create(
    Storage.GetFloat('LeftMargin', cPageMarginSizeMM),
    Storage.GetFloat('TopMargin', cPageMarginSizeMM),
    Storage.GetFloat('RightMargin', cPageMarginSizeMM),
    Storage.GetFloat('BottomMargin', cPageMarginSizeMM)
  );
end;

{ TPageMargins }

constructor TPageMargins.Create(ALeft, ATop, ARight, ABottom: Double);
  {Record constructor. Sets initial field values.
    @param ALeft [in] Left margin size.
    @param ATop [in] Top margin size.
    @param ARight [in] Right margin size.
    @param ABottom [in] Bottom margin size.
  }
begin
  Left := ALeft;
  Top := ATop;
  Right := ARight;
  Bottom := ABottom;
end;

end.

