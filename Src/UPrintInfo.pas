{
 * UPrintInfo.pas
 *
 * Implements a singleton object that maintains printer and page information for
 * lifetime of program. This object maintains information that is not maintained
 * by the Printer object.
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
 * The Original Code is UPrintInfo.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UPrintInfo;


interface


type

  {
  TPageMargins:
    Record defines page margins in millimeters.
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
    poSyntaxPrint,  // printed source code will be syntax highlighted
    poUseColor      // colour will be used in printed document
  );

  {
  TPrintOptions:
    Set of print options.
  }
  TPrintOptions = set of TPrintOption;

  {
  IPrintInfo:
    Interface supported by print info singleton object that maintains printer
    and page setup information for lifetime of application that is not
    maintained by the Printer object.
  }
  IPrintInfo = interface(IInterface)
    ['{BEE68732-A74D-4502-9627-63BBDC6C04FB}']
    procedure LoadDefaults;
      {Loads default property values from user preferences.
      }
    function GetPageMargins: TPageMargins;
      {Gets current page margins.
        @return Margins in millimeters.
      }
    procedure SetPageMargins(const Margins: TPageMargins);
      {Sets page margins.
        @param Margins [in] New margins in millimeters.
      }
    property PageMargins: TPageMargins
      read GetPageMargins write SetPageMargins;
      {Stores current page margins, in millimeters}
    function GetPrintOptions: TPrintOptions;
      {Gets current print options.
        @return Current options.
      }
    procedure SetPrintOptions(const Options: TPrintOptions);
      {Sets print options.
        @param Options [in] New options.
      }
    property PrintOptions: TPrintOptions
      read GetPrintOptions write SetPrintOptions;
      {Stores current print options}
  end;


function PrintInfo: IPrintInfo;
  {Provides access to a singleton implementation of IPrintInfo.
    @return Singletion object instance.
  }


implementation


uses
  // Delphi
  UPreferences;


type

  {
  TPrintInfo:
    Class that maintains printer and page information for lifetime of
    application. This class stores information that the Printer object doesn't
    maintain.
  }
  TPrintInfo = class(TInterfacedObject,
    IPrintInfo
  )
  strict private
    fPageMargins: TPageMargins;
      {Stores current page margins in millimeters}
    fPrintOptions: TPrintOptions;
      {Stores current print options}
    class var fInstance: IPrintInfo;
      {Stores reference to singleton instance of this class}
    class function GetInstance: IPrintInfo; static;
      {Returns singletion IPrintInfo object initialised from persistent storage.
        @return Object instance.
      }
  protected // do not make strict
    { IPrintInfo methods }
    procedure LoadDefaults;
      {Loads default property values from user preferences.
      }
    function GetPageMargins: TPageMargins;
      {Gets current page margins.
        @return Margins in millimeters.
      }
    procedure SetPageMargins(const Margins: TPageMargins);
      {Sets page margins.
        @param Margins [in] New margins in millimeters.
      }
    function GetPrintOptions: TPrintOptions;
      {Gets current print options.
        @return Current options.
      }
    procedure SetPrintOptions(const Options: TPrintOptions);
      {Sets print options.
        @param Options [in] New options.
      }
  public
    constructor Create;
      {Class constructor. Sets up and initialises properties to default values.
      }
    class property Instance: IPrintInfo
      read GetInstance;
      {Reference to singleton instance of this class}
  end;


function PrintInfo: IPrintInfo;
  {Provides access to a singleton implementation of IPrintInfo.
    @return Singletion object instance.
  }
begin
  Result := TPrintInfo.Instance;
end;


{ TPrintInfo }

constructor TPrintInfo.Create;
  {Class constructor. Sets up and initialises properties to default values.
  }
begin
  inherited;
  LoadDefaults;
end;

class function TPrintInfo.GetInstance: IPrintInfo;
  {Returns singletion IPrintInfo object initialised from persistent storage.
    @return Object instance.
  }
begin
  if not Assigned(fInstance) then
    fInstance := TPrintInfo.Create;
  Result := fInstance;
end;

function TPrintInfo.GetPageMargins: TPageMargins;
  {Gets current page margins.
    @return Margins in millimeters.
  }
begin
  Result := fPageMargins;
end;

function TPrintInfo.GetPrintOptions: TPrintOptions;
  {Gets current print options.
    @return Current options.
  }
begin
  Result := fPrintOptions;
end;

procedure TPrintInfo.LoadDefaults;
  {Loads default property values from user preferences.
  }
begin
  fPrintOptions := Preferences.PrinterOptions;
  fPageMargins := Preferences.PrinterPageMargins;
end;

procedure TPrintInfo.SetPageMargins(const Margins: TPageMargins);
  {Sets page margins.
    @param Margins [in] New margins in millimeters.
  }
begin
  fPageMargins := Margins;
end;

procedure TPrintInfo.SetPrintOptions(const Options: TPrintOptions);
  {Sets print options.
    @param Options [in] New options.
  }
begin
  fPrintOptions := Options;
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

