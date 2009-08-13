{
 * UPrinterDocPropsDlg.pas
 *
 * Implements a wrapper for the Windows DocumentProperties dialog box API.
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
 * The Original Code is UPrinterDocPropsDlg.pas
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


unit UPrinterDocPropsDlg;

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}

interface


uses
  // Delphi
  Windows,
  // Project
  UExceptions;


type

  {
  TPrinterDocPropsDlg:
    Class that wraps the Windows DocumentProperties dialog box API and
    configures, updates and retrieves information from the dialog box.
  }
  TPrinterDocPropsDlg = class(TObject)
  private
    fOwnerHandle: HWND;
      {Handle of window that owns (parents) the document properties dialog box}
    fPrinterHandle: THandle;
      {Handle of printer whose document properties are being edited}
    fDriverBuf: array[0..255] of AnsiChar;
      {Stores name of printer driver}
    fPortBuf: array[0..255] of AnsiChar;
      {Stores name of printer port}
    function GetPrinterHandle(const PrinterName: string): THandle;
      {Gets handle to a named printer.
        @param PrinterName [in] Name of printer.
        @return Handle to named printer.
        @except EPrinterDocPropsDlg raised if we can't get printer handle.
      }
    procedure GetPrinterInfo(out Device: string; out HDevMode: THandle);
      {Retrieves information about the current printer.
        @param Device [out] Printer's device name.
        @param HDevMode [out] Handle to device specific driver information.
      }
    procedure SetPrinterInfo(const Device: string; const HDevMode: THandle);
      {Sets driver properties of current printer.
        @param Device [in] Name of printer device for which properties to be
          set. This must be same value returned from GetPrinterInfo.
        @param HDevMode [in] Handle to device specific driver information.
      }
    function SizeOfDeviceMode(const Device: string): Cardinal;
      {Gets size of buffer required for a device mode structure.
        @param Device [in] Name of device for which structure size is required.
        @return Required structure size
      }
    function AllocDevMode(const Device: string): THandle;
      {Allocates storage for a device mode structure.
        @param Device [in] Name of device for which structure is required.
      }
    procedure ReadDocProperties(const Device: string;
      const DevMode: PDeviceMode);
      {Reads document properties.
        @param Device [in] Devide for which document properties required.
        @param DevMode [in] Pointer to buffer to receive document properties.
        @except EPrinterDocPropsDlg raised if document properties can't be read.
      }
    function DisplayDialog(const Device: string;
      const DevMode: PDeviceMode): Boolean;
      {Displays Document Properties dialog box to allow user to modify document
      properties.
        @param Device [in] Device for which we're are modifying properties.
        @param DevMode [in] Pointer to document properties device mode updated
          if user OKs dialog. DevMode should be set to values to be displayed in
          dialog before calling.
        @return True if user OKs dialog and False if user cancels.
      }
    procedure CopyDeviceMode(const Source: TDeviceMode; out Dest: TDeviceMode);
      {Copies a device mode structures. Note that any extended information
      appended to the device modes.
        @param Source [in] Device mode to be copied.
        @param Dest [out] Receives copy of device mode.
      }
  public
    constructor Create(const OwnerHandle: HWND; const PrinterName: string);
      {Class constructor. Sets up object.
        @param OwnerHandle [in] Handle to window that owns the dialog box.
        @param PrinterName [in] Name of printer whose document properties to be
          edited.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object and tidies up.
      }
    function Execute: Boolean;
      {Displays dialog box.
        @return True if user OKs or False if user Cancels.
      }
  end;

  {
  EPrinterDocPropsDlg:
    Exception raised by TPrinterDocPropsDlg objects.
  }
  EPrinterDocPropsDlg = class(ECodeSnip);


implementation


uses
  // Delphi
  Printers, WinSpool;


resourcestring
  // Error messages
  sReadWriteError = 'Can''t read or update document properties for device "%s"';
  sCantOpenPrinter = 'Can''t open printer "%s"';
  sReadError = 'Can''t read document properties for device "%s"';


{ TPrinterDocPropsDlg }

function TPrinterDocPropsDlg.AllocDevMode(const Device: string): THandle;
  {Allocates storage for a device mode structure.
    @param Device [in] Name of device for which structure is required.
  }
begin
  // NOTE: we can't just allocate a TDeviceMode structure since device driver
  // may append some custom data to end of structure. SizeOfDeviceMode provides
  // required buffer size
  Result := GlobalAlloc(GHND, SizeOfDeviceMode(Device));
end;

procedure TPrinterDocPropsDlg.CopyDeviceMode(const Source: TDeviceMode;
  out Dest: TDeviceMode);
  {Copies a device mode structures. Note that any extended information appended
  to the device modes.
    @param Source [in] Device mode to be copied.
    @param Dest [out] Receives copy of device mode.
  }
begin
  Move(Source, Dest, SizeOf(TDeviceMode));
end;

constructor TPrinterDocPropsDlg.Create(const OwnerHandle: HWND;
  const PrinterName: string);
  {Class constructor. Sets up object.
    @param OwnerHandle [in] Handle to window that owns the dialog box.
    @param PrinterName [in] Name of printer whose document properties to be
      edited.
  }
begin
  inherited Create;
  fOwnerHandle := OwnerHandle;
  fPrinterHandle := GetPrinterHandle(PrinterName);
end;

destructor TPrinterDocPropsDlg.Destroy;
  {Class destructor. Tears down object and tidies up.
  }
begin
  if fPrinterHandle <> 0 then
    ClosePrinter(fPrinterHandle);
  inherited;
end;

function TPrinterDocPropsDlg.DisplayDialog(const Device: string;
  const DevMode: PDeviceMode): Boolean;
  {Displays Document Properties dialog box to allow user to modify document
  properties.
    @param Device [in] Device for which we're are modifying properties.
    @param DevMode [in] Pointer to document properties device mode updated if
      user OKs dialog. DevMode should be set to values to be displayed in
      dialog before calling.
    @return True if user OKs dialog and False if user cancels.
    @except EPrinterDocPropsDlg raised if can't read or write document
      properties.
  }
const
  // Bug error message: ** do not localise
  cUnexpectedError = 'TPrinterDocPropsDlg.DisplayDialog: '
    + 'Unexpected return value from DocumentProperties';
var
  Res: Integer; // result of calling DocumentProperties API
begin
  // Display document properties dialog
  // sets DevMode to changed properties if dialog OKd
  Res := DocumentProperties(
    fOwnerHandle,
    fPrinterHandle,
    PChar(Device),
    DevMode^,
    DevMode^,
    DM_IN_PROMPT or DM_IN_BUFFER or DM_OUT_BUFFER
  );
  // Handle result from API call
  if Res < 0 then
    raise EPrinterDocPropsDlg.CreateFmt(sReadWriteError, [Device]);
  case Res of
    IDOK: Result := True;
    IDCANCEL: Result := False;
    else
      raise EBug.Create(cUnexpectedError);
  end;
end;

function TPrinterDocPropsDlg.Execute: Boolean;
  {Displays dialog box.
    @return True if user OKs or False if user Cancels.
  }
var
  Device: string;
  HPrnDevMode: THandle;       // handle to printer's properties
  PPrnDevMode: PDeviceMode;   // pointer to printer's properties
  HDocDevMode: THandle;       // handle to document properties
  PDocDevMode: PDeviceMode;   // pointer to document properies
begin
  // Intialise handles: keeps compiler quiet!
  HDocDevMode := 0; //
  HPrnDevMode := 0;
  try
    // Get current printer properties
    GetPrinterInfo(Device, HPrnDevMode);
    PPrnDevMode := GlobalLock(HPrnDevMode);
    // Get current document properties
    HDocDevMode := AllocDevMode(Device);
    PDocDevMode := GlobalLock(HDocDevMode);
    ReadDocProperties(Device, PDocDevMode);
    // Update document properties to be same as printer properties
    CopyDeviceMode(PPrnDevMode^, PDocDevMode^);
    // Display dialog box to allow user to update document properties
    Result := DisplayDialog(Device, PDocDevMode);
    if Result then
    begin
      // User OKd
      // Update printer properties to be same as updated document properties
      CopyDeviceMode(PDocDevMode^, PPrnDevMode^);
      SetPrinterInfo(Device, HPrnDevMode);
    end;
  finally
    // Tidy up
    if HPrnDevMode <> 0 then
      GlobalUnlock(HPrnDevMode);
    if HDocDevMode <> 0 then
    begin
      GlobalUnlock(HDocDevMode);
      GlobalFree(HDocDevMode);
    end;
  end;
end;

function TPrinterDocPropsDlg.GetPrinterHandle(
  const PrinterName: string): THandle;
  {Gets handle to a named printer.
    @param PrinterName [in] Name of printer.
    @return Handle to named printer.
    @except EPrinterDocPropsDlg raised if we can't get printer handle.
  }
begin
  if not OpenPrinter(PChar(PrinterName), Result, nil) then
    raise EPrinterDocPropsDlg.CreateFmt(sCantOpenPrinter, [PrinterName]);
end;

procedure TPrinterDocPropsDlg.GetPrinterInfo(out Device: string;
  out HDevMode: THandle);
  {Retrieves information about the current printer.
    @param Device [out] Printer's device name.
    @param HDevMode [out] Handle to device specific driver information.
  }
var
  DeviceBuf: array[0..255] of AnsiChar; // receives name of device
begin
  // Following method gets more info than we need. We store driver and port
  // information in fields since we need to pass them to another method later.
  // These fields are not used by all versions of Windows and are often empty
  // strings.
  Printer.GetPrinter(DeviceBuf, fDriverBuf, fPortBuf, HDevMode);
  Device := DeviceBuf;
end;

procedure TPrinterDocPropsDlg.ReadDocProperties(const Device: string;
  const DevMode: PDeviceMode);
  {Reads document properties.
    @param Device [in] Devide for which document properties required.
    @param DevMode [in] Pointer to buffer to receive document properties.
    @except EPrinterDocPropsDlg raised if document properties can't be read.
  }
begin
  if DocumentProperties(
    0, fPrinterHandle, PChar(Device), DevMode^, DevMode^, DM_OUT_BUFFER
  ) < 0 then
    raise EPrinterDocPropsDlg.CreateFmt(sReadError, [Device]);
end;

procedure TPrinterDocPropsDlg.SetPrinterInfo(const Device: string;
  const HDevMode: THandle);
  {Sets driver properties of current printer.
    @param Device [in] Name of printer device for which properties to be set.
      This must be same value returned from GetPrinterInfo.
    @param HDevMode [in] Handle to device specific driver information.
  }
begin
  Printer.SetPrinter(PChar(Device), fDriverBuf, fPortBuf, HDevMode);
end;

function TPrinterDocPropsDlg.SizeOfDeviceMode(const Device: string): Cardinal;
  {Gets size of buffer required for a device mode structure.
    @param Device [in] Name of device for which structure size is required.
    @return Required structure size
  }
var
  DummyDevMode: TDeviceMode;  // Unused device mode structure
begin
  // We get required size from a special call to DocumentProperties API
  // function.
  Result := DocumentProperties( // gets size of device mode
    0,                  // no handle => don't display dialog
    fPrinterHandle,     // handle to printer
    PChar(Device),      // name of device
    DummyDevMode,       // unused parameter
    DummyDevMode,       // unused parameter
    0                   // informs DocumentProperties we want buffer size
  );
end;

end.

