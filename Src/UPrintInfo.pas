{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a singleton object that maintains printer and page information for
 * lifetime of program. This object maintains information that is not maintained
 * by the Printer object.
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
  UPreferences, USingleton;


type

  {
  TPrintInfo:
    Class that maintains printer and page information for lifetime of
    application. This class stores information that the Printer object doesn't
    maintain.
  }
  TPrintInfo = class(TSingleton,
    IPrintInfo
  )
  strict private
    var
      fPageMargins: TPageMargins;
        {Stores current page margins in millimeters}
      fPrintOptions: TPrintOptions;
        {Stores current print options}
    class function GetInstance: IPrintInfo; static;
      {Returns singletion IPrintInfo object initialised from persistent storage.
        @return Object instance.
      }
  strict protected
    ///  <summary>Initialises singleton object on creation.</summary>
    procedure Initialize; override;
  public
    class property Instance: IPrintInfo read GetInstance;
      {Reference to singleton instance of this class}
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
  end;


function PrintInfo: IPrintInfo;
  {Provides access to a singleton implementation of IPrintInfo.
    @return Singletion object instance.
  }
begin
  Result := TPrintInfo.Instance;
end;


{ TPrintInfo }

class function TPrintInfo.GetInstance: IPrintInfo;
  {Returns singletion IPrintInfo object initialised from persistent storage.
    @return Object instance.
  }
begin
  Result := TPrintInfo.Create;
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

procedure TPrintInfo.Initialize;
begin
  inherited;
  LoadDefaults;
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

