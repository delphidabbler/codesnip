{
 * UPreferences.pas
 *
 * Implements a singletion object that exposes and persists user preferences.
 *
 * v0.1 of 06 Jan 2006  - Original version.
 * v0.2 of 04 Apr 2006  - Changed to use renamed and revised Settings object and
 *                        associated interfaces.
 * v1.0 of 24 May 2006  - Made minor changes to comments.
 *                      - Prefixed UGlobals reference to global constants.
 * v1.1 of 26 Oct 2006  - Declared new IPreferences interface.
 *                      - Moved TPreferences to implementation section and
 *                        changed to descend from TInterfacedObject and
 *                        implement IPreferences.
 *                      - Changed private implementation variable to be of
 *                        IPreferences type.
 *                      - Changed TPreferences to implement property read/write
 *                        accessor methods of IPreferences rather than using
 *                        direct field access.
 * v1.2 of 29 Oct 2006  - Renamed IPreferences properties:
 *                        * SnippetCommentStyle to SourceCommentStyle,
 *                        * SnippetSyntaxHilited to SourceSyntaxHilited
 *                        * SnippetDefaultFileExt to SourceDefaultFileType.
 *                      - SourceDefaultFileType property changed to return an
 *                        enumeration rather than a file extension. NOTE: this
 *                        breaks compatibility of persistent storage produced by
 *                        CodeSnip v1.0.3 and earlier.
 * v2.0 of 07 Sep 2007  - Major revision:
 *                      - Added ability to get a non-persisting copy of
 *                        singleton to use for temporary preference updates.
 *                        This was done by splitting implementation into two
 *                        classes, one that persists (for singletion) and one
 *                        that does not. Added Clone and Assign methods to
 *                        enable singlton to create a temp copy of itself and to
 *                        assign modified temp data to itself.
 *                      - Added new MeasurementUnits, PrinterOptions and
 *                        PrinterPageMargins properties.
 *                      - Changed to write to various sub sections of
 *                        ssPreferences settings key. NOTE: this breaks
 *                        compatibility with persistent storage produced by
 *                        CodeSnip v1.6.4 and earlier.
 * v2.1 of 17 Oct 2007  - Added HiliteAttrs property and code to support
 *                        persisting syntax highlighter information to
 *                        ssPreferences settings kye. NOTE: this breaks
 *                        compatibility with persistent storage produced by
 *                        CodeSnip v1.7.4 and earlier.
 * v2.2 of 04 Nov 2007  - Changed parameter type of TPreferences.Assign from
 *                        IAssignable to IInterface.
 * v2.3 of 16 Dec 2008  - Modified to user TPageMargins constructor instead of
 *                        deleted PageMargins function to initialise margins.
 *                      - Made TPreferences fields strict protected instead of
 *                        private
 *                      - Moved construction and storage of IPreferences
 *                        singleton into TPreferences. Preferences function now
 *                        calls into TPreferences to get reference to singleton.
 *                      - Move some constants into TPreferencesPersist class.
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
 * The Original Code is UPreferences.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UPreferences;


interface


uses
  // Project
  IntfHiliter, UMeasurement, UPrintInfo, USourceFileInfo, USourceGen;


type

  {
  IPreferences:
    Interface to object that exposes and persists user preferences.
  }
  IPreferences = interface(IInterface)
    ['{381B9A92-B528-47E1-AC04-90E1FFFDADA7}']
    function GetSourceCommentStyle: TCommentStyle;
      {Gets style of commenting used to describe routines in generated code.
        @return Current commenting style.
      }
    procedure SetSourceCommentStyle(const Value: TCommentStyle);
      {Sets style of commenting to be used describe routines in generated code.
        @param Value [in] Required commenting style.
      }
    property SourceCommentStyle: TCommentStyle
      read GetSourceCommentStyle write SetSourceCommentStyle;
      {Commenting style used to describe routines in generated source code}

    function GetSourceDefaultFileType: TSourceFileType;
      {Gets current default file extension / type used when writing code
      snippets to file.
        @return Current default file extension.
      }
    procedure SetSourceDefaultFileType(const Value: TSourceFileType);
      {Sets default file extension / type to be used when writing code snippets
      to file.
        @param Value [in] Required default file extension.
      }
    property SourceDefaultFileType: TSourceFileType
      read GetSourceDefaultFileType write SetSourceDefaultFileType;
      {Default file extension / type used when writing code snippets to file}

    function GetSourceSyntaxHilited: Boolean;
      {Gets current indicator of whether generated source is highlighted by
      default.
        @return Current value indicating whether source is highlighted.
      }
    procedure SetSourceSyntaxHilited(const Value: Boolean);
      {Sets flag indicating whether generated source is highlighted by default.
        @param Value [in] Flag indicating whether source is highlighted.
      }
    property SourceSyntaxHilited: Boolean
      read GetSourceSyntaxHilited write SetSourceSyntaxHilited;
      {Indicates whether generated source is highlighted by default}

    function GetMeasurementUnits: TMeasurementUnits;
      {Gets measurement units used by application.
        @return Current measurement units.
      }
    procedure SetMeasurementUnits(const Value: TMeasurementUnits);
      {Sets measurement units to be used by application.
        @param Value [in] Required measurement units.
      }
    property MeasurementUnits: TMeasurementUnits
      read GetMeasurementUnits write SetMeasurementUnits;
      {Measurement units used by application}

    function GetPrinterOptions: TPrintOptions;
      {Gets print options.
        @return Default print options.
      }
    procedure SetPrinterOptions(const Options: TPrintOptions);
      {Sets default print options.
        @param Options [in] New print options.
      }
    property PrinterOptions: TPrintOptions
      read GetPrinterOptions write SetPrinterOptions;
      {Default print options}

    function GetPrinterPageMargins: TPageMargins;
      {Gets default page margins.
        @return Default page margins.
      }
    procedure SetPrinterPageMargins(const Margins: TPageMargins);
      {Sets new default page margins.
        @param Margins [in] New page margins.
      }
    property PrinterPageMargins: TPageMargins
      read GetPrinterPageMargins write SetPrinterPageMargins;
      {Default page margins used in printing}

    function GetHiliteAttrs: IHiliteAttrs;
      {Gets user defined syntax highlighter.
        @return Current syntax highlighter attributes.
      }
    procedure SetHiliteAttrs(const Attrs: IHiliteAttrs);
      {Sets new user defined syntax highlighter.
        @param Attrs [in] New highlighter attributes.
      }
    property HiliteAttrs: IHiliteAttrs
      read GetHiliteAttrs write SetHiliteAttrs;
      {Attributes of syntax highlighter}
  end;


function Preferences: IPreferences;
  {Provides access to a singleton implementation a IPreferences, ensuring it
  exists.
    @return Singleton object instance.
  }


implementation


uses
  // Delphi
  SysUtils,
  // Project
  IntfCommon, UExceptions, UHiliteAttrs, UHiliterPersist, USettings;


type

  {
  TPreferences:
    Class that implements IPreferences interface and its property access
    methods. Does not persist the data. Intended for use as a temporary local
    copy of the singleton that cannot be saved. Data entered in this object is
    assigned to singleton for saving. Objects should be created using
    singleton's Clone method.
  }
  TPreferences = class(TInterfacedObject,
    IPreferences, IAssignable
  )
  strict protected
    fSourceDefaultFileType: TSourceFileType;
      {Default file extension / type used when writing code snippets to file}
    fSourceCommentStyle: TCommentStyle;
      {Commenting style used to describe routines in generated source code}
    fSourceSyntaxHilited: Boolean;
      {Indicates whether generated source is highlighted by default}
    fMeasurementUnits: TMeasurementUnits;
      {Measurement unit in use by application}
    fPrinterOptions: TPrintOptions;
      {Default print options}
    fPrinterPageMargins: TPageMargins;
      {Default print page margins}
    fHiliteAttrs: IHiliteAttrs;
      {User defined syntax highlighter}
  protected // do not make strict
    { IPreferences methods }
    function GetSourceCommentStyle: TCommentStyle;
      {Gets style of commenting used to describe routines in generated code.
        @return Current commenting style.
      }
    procedure SetSourceCommentStyle(const Value: TCommentStyle);
      {Sets style of commenting to be used describe routines in generated code.
        @param Value [in] Required commenting style.
      }
    function GetSourceDefaultFileType: TSourceFileType;
      {Gets current default file extension / type used when writing code
      snippets to file.
        @return Current default file extension.
      }
    procedure SetSourceDefaultFileType(const Value: TSourceFileType);
      {Sets default file extension / type to be used when writing code snippets
      to file.
        @param Value [in] Required default file extension.
      }
    function GetSourceSyntaxHilited: Boolean;
      {Gets current indicator of whether generated source is highlighted by
      default.
        @return Current value indicating whether source is highlighted.
      }
    procedure SetSourceSyntaxHilited(const Value: Boolean);
      {Sets flag indicating whether generated source is highlighted by default.
        @param Value [in] Flag indicating whether source is highlighted.
      }
    function GetMeasurementUnits: TMeasurementUnits;
      {Gets measurement units used by application.
        @return Current measurement units.
      }
    procedure SetMeasurementUnits(const Value: TMeasurementUnits);
      {Sets measurement units to be used by application.
        @param Value [in] Required measurement units.
      }
    function GetPrinterOptions: TPrintOptions;
      {Gets print options.
        @return Print options.
      }
    procedure SetPrinterOptions(const Options: TPrintOptions);
      {Sets default print options.
        @param Options [in] New print options.
      }
    function GetPrinterPageMargins: TPageMargins;
      {Gets default page margins.
        @return Default page margins.
      }
    procedure SetPrinterPageMargins(const Margins: TPageMargins);
      {Sets new default page margins.
        @param Margins [in] New page margins.
      }
    function GetHiliteAttrs: IHiliteAttrs;
      {Gets user defined syntax highlighter.
        @return Current syntax highlighter attributes.
      }
    procedure SetHiliteAttrs(const Attrs: IHiliteAttrs);
      {Sets new user defined syntax highlighter.
        @param Attrs [in] New highlighter attributes.
      }
    { IAssignable method }
    procedure Assign(const Src: IInterface);
      {Assigns properties of a given object to this object.
        @param Src [in] Object whose properties are to be copied.
        @except Bug raised if Src does not support IPreferences.
      }
  public
    constructor Create;
      {Class constructor. Sets up object.
      }
  end;

  {
  TPreferencesPersist:
    Class that implements IPreferences interface and its property access
    methods. Intended for use as a singleton that reads and writes preference
    from and to persistent storage. Can clone itself as a non-persistent
    temporary object containing all data. Can also copy data from such a cloned
    object via Assign method.
  }
  TPreferencesPersist = class(TPreferences,
    IPreferences, IAssignable, IClonable
  )
  strict private
    const
      // Sub-sections of ssPreferences ini file section ** do not localise
      cGeneral = 'General';
      cSourceCode = 'SourceCode';
      cPrinting = 'Printing';
      cHiliter = 'Hiliter';
    class var fInstance: IPreferences;
      {Stores reference to singleton instance of this class}
    class function GetInstance: IPreferences; static;
      {Returns singleton IPreferences object initialised from persistent
      storage.
        @return Object instance.
      }
  protected // do not make strict
    { IClonable method }
    function Clone: IInterface;
      {Create a new instance of the object that is a non-persisting copy.
        @return New object's IInterface interface.
      }
  public
    constructor Create;
      {Class constructor. Creates object and reads preferences from persistent
      storage.
      }
    destructor Destroy; override;
      {Class destructor. Saves preferences to persistent storage then tears down
      object.
      }
    class property Instance: IPreferences
      read GetInstance;
      {Reference to singleton instance of this class}
  end;


function Preferences: IPreferences;
  {Provides access to a singleton implementation a IPreferences.
    @return Singleton object instance.
  }
begin
  Result := TPreferencesPersist.Instance;
end;


{ TPreferences }

procedure TPreferences.Assign(const Src: IInterface);
  {Assigns properties of a given object to this object.
    @param Src [in] Object whose properties are to be copied.
    @except Bug raised if Src does not support IPreferences.
  }
var
  SrcPref: IPreferences;  // IPreferences interface of Src
begin
  // Get IPreferences interface of given object
  if not Supports(Src, IPreferences, SrcPref) then         // ** do not localise
    raise EBug.Create(ClassName + '.Assign: Src is wrong type');
  // Copy the data
  Self.fSourceDefaultFileType := SrcPref.SourceDefaultFileType;
  Self.fSourceCommentStyle := SrcPref.SourceCommentStyle;
  Self.fSourceSyntaxHilited := SrcPref.SourceSyntaxHilited;
  Self.fMeasurementUnits := SrcPref.MeasurementUnits;
  Self.fPrinterOptions := SrcPref.PrinterOptions;
  Self.fPrinterPageMargins := SrcPref.PrinterPageMargins;
  Self.SetHiliteAttrs(SrcPref.HiliteAttrs);
end;

constructor TPreferences.Create;
  {Class constructor. Sets up object.
  }
begin
  inherited Create;
  // Create object to record syntax highlighter
  fHiliteAttrs := THiliteAttrsFactory.CreateDefaultAttrs;
end;

function TPreferences.GetHiliteAttrs: IHiliteAttrs;
  {Gets user defined syntax highlighter.
    @return Current syntax highlighter attributes.
  }
begin
  Result := fHiliteAttrs;
end;

function TPreferences.GetMeasurementUnits: TMeasurementUnits;
  {Gets measurement units used by application.
    @return Current measurement units.
  }
begin
  Result := fMeasurementUnits;
end;

function TPreferences.GetPrinterOptions: TPrintOptions;
  {Gets print options.
    @return Print options.
  }
begin
  Result := fPrinterOptions;
end;

function TPreferences.GetPrinterPageMargins: TPageMargins;
  {Gets default page margins.
    @return Default page margins.
  }
begin
  Result := fPrinterPageMargins;
end;

function TPreferences.GetSourceCommentStyle: TCommentStyle;
  {Gets style of commenting used to describe routines in generated code.
    @return Current commenting style.
  }
begin
  Result := fSourceCommentStyle;
end;

function TPreferences.GetSourceDefaultFileType: TSourceFileType;
  {Gets current default file extension / type used when writing code snippets to
  file.
    @return Current default file extension.
  }
begin
  Result := fSourceDefaultFileType;
end;

function TPreferences.GetSourceSyntaxHilited: Boolean;
  {Gets current indicator of whether generated source is highlighted by default.
    @return Current value indicating whether source is highlighted.
  }
begin
  Result := fSourceSyntaxHilited;
end;

procedure TPreferences.SetHiliteAttrs(const Attrs: IHiliteAttrs);
  {Sets new user defined syntax highlighter.
    @param Attrs [in] New highlighter attributes.
  }
begin
  (fHiliteAttrs as IAssignable).Assign(Attrs);
end;

procedure TPreferences.SetMeasurementUnits(const Value: TMeasurementUnits);
  {Sets measurement units to be used by application.
    @param Value [in] Required measurement units.
  }
begin
  fMeasurementUnits := Value;
end;

procedure TPreferences.SetPrinterOptions(const Options: TPrintOptions);
  {Sets default print options.
    @param Options [in] New print options.
  }
begin
  fPrinterOptions := Options;
end;

procedure TPreferences.SetPrinterPageMargins(const Margins: TPageMargins);
  {Sets new default page margins.
    @param Margins [in] New page margins.
  }
begin
  fPrinterPageMargins := Margins;
end;

procedure TPreferences.SetSourceCommentStyle(const Value: TCommentStyle);
  {Sets style of commenting to be used describe routines in generated code.
    @param Value [in] Required commenting style.
  }
begin
  fSourceCommentStyle := Value;
end;

procedure TPreferences.SetSourceDefaultFileType(const Value: TSourceFileType);
  {Sets default file extension / type to be used when writing code snippets to
  file.
    @param Value [in] Required default file extension.
  }
begin
  fSourceDefaultFileType := Value;
end;

procedure TPreferences.SetSourceSyntaxHilited(const Value: Boolean);
  {Sets flag indicating whether generated source is highlighted by default.
    @param Value [in] Flag indicating whether source is highlighted.
  }
begin
  fSourceSyntaxHilited := Value;
end;

{ TPreferencesPersist }

function TPreferencesPersist.Clone: IInterface;
  {Create a new instance of the object that is a non-persisting copy.
    @return New object's IInterface interface.
  }
var
  NewPref: IPreferences;  // reference to new object's IPreferences interface
begin
  // Create new object
  Result := TPreferences.Create;
  // Copy properties to it
  NewPref := Result as IPreferences;
  NewPref.SourceDefaultFileType := Self.fSourceDefaultFileType;
  NewPref.SourceCommentStyle := Self.fSourceCommentStyle;
  NewPref.SourceSyntaxHilited := Self.fSourceSyntaxHilited;
  NewPref.MeasurementUnits := Self.fMeasurementUnits;
  NewPref.PrinterOptions := Self.fPrinterOptions;
  NewPref.PrinterPageMargins := Self.fPrinterPageMargins;
  NewPref.HiliteAttrs := Self.GetHiliteAttrs;
end;

constructor TPreferencesPersist.Create;
  {Class constructor. Creates object and reads preferences from persistent
  storage.
  }
var
  Storage: ISettingsSection;  // object used to access persistent storage
const
  // Default margin size in millimeters
  cPrintPageMarginSizeMM = 25.0;
begin
  inherited Create;

  // Read general section
  Storage := Settings.ReadSection(ssPreferences, cGeneral);
  fMeasurementUnits := TMeasurementUnits(
    StrToIntDef(Storage.ItemValues['Units'], Ord(DefaultMeasurementUnits))
  );

  // Read source code section
  Storage := Settings.ReadSection(ssPreferences, cSourceCode);
  fSourceDefaultFileType := TSourceFileType(
    StrToIntDef(Storage.ItemValues['FileType'], Ord(sfPascal))
  );
  fSourceCommentStyle := TCommentStyle(
    StrToIntDef(Storage.ItemValues['CommentStyle'], Ord(csAfter))
  );
  fSourceSyntaxHilited := Boolean(
    StrToIntDef(Storage.ItemValues['UseSyntaxHiliting'], Ord(False))
  );

  // Read printing section
  Storage := Settings.ReadSection(ssPreferences, cPrinting);
  fPrinterOptions := [];
  if Boolean(StrToIntDef(Storage.ItemValues['UseColor'], Ord(True))) then
    Include(fPrinterOptions, poUseColor);
  if Boolean(StrToIntDef(Storage.ItemValues['SyntaxPrint'], Ord(True))) then
    Include(fPrinterOptions, poSyntaxPrint);
  fPrinterPageMargins := TPageMargins.Create(
    StrToFloatDef(Storage.ItemValues['LeftMargin'], cPrintPageMarginSizeMM),
    StrToFloatDef(Storage.ItemValues['TopMargin'], cPrintPageMarginSizeMM),
    StrToFloatDef(Storage.ItemValues['RightMargin'], cPrintPageMarginSizeMM),
    StrToFloatDef(Storage.ItemValues['BottomMargin'], cPrintPageMarginSizeMM)
  );

  // Read syntax highlighter section
  Storage := Settings.ReadSection(ssPreferences, cHiliter);
  THiliterPersist.Load(Storage, fHiliteAttrs);
end;

destructor TPreferencesPersist.Destroy;
  {Class destructor. Saves preferences to persistent storage then tears down
  object.
  }
var
  Storage: ISettingsSection;  // object used to access persistent storage
begin
  // Write general section
  Storage := Settings.EmptySection(ssPreferences, cGeneral);
  Storage.ItemValues['Units'] := IntToStr(Ord(fMeasurementUnits));
  Storage.Save;

  // Write source code section
  Storage := Settings.EmptySection(ssPreferences, cSourceCode);
  Storage.ItemValues['FileType'] := IntToStr(Ord(fSourceDefaultFileType));
  Storage.ItemValues['CommentStyle'] := IntToStr(Ord(fSourceCommentStyle));
  Storage.ItemValues['UseSyntaxHiliting'] := IntToStr(
    Ord(fSourceSyntaxHilited)
  );
  Storage.Save;

  // Write printing section
  Storage := Settings.EmptySection(ssPreferences, cPrinting);
  Storage.ItemValues['UseColor'] := IntToStr(
    Ord(poUseColor in fPrinterOptions)
  );
  Storage.ItemValues['SyntaxPrint'] := IntToStr(
    Ord(poSyntaxPrint in fPrinterOptions)
  );
  Storage.ItemValues['LeftMargin'] := FloatToStr(fPrinterPageMargins.Left);
  Storage.ItemValues['TopMargin'] := FloatToStr(fPrinterPageMargins.Top);
  Storage.ItemValues['RightMargin'] := FloatToStr(fPrinterPageMargins.Right);
  Storage.ItemValues['BottomMargin'] := FloatToStr(fPrinterPageMargins.Bottom);
  Storage.Save;

  // Write syntax highlighter section
  Storage := Settings.EmptySection(ssPreferences, cHiliter);
  THiliterPersist.Save(Storage, fHiliteAttrs);

  inherited;
end;

class function TPreferencesPersist.GetInstance: IPreferences;
  {Returns singleton IPreferences object initialised from persistent storage.
    @return Object instance.
  }
begin
  if not Assigned(fInstance) then
    fInstance := TPreferencesPersist.Create;
  Result := fInstance;
end;

end.

