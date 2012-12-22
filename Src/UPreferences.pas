{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a singletion object that exposes and persists user preferences.
}


unit UPreferences;


interface


uses
  // Delphi
  Graphics,
  // Project
  Hiliter.UGlobals, UIStringList, UMeasurement, UPrintInfo,
  USnippetPageStructure, USourceFileInfo, USourceGen, UWarnings;


type
  // Possible values for startup state of overview treeview.
  TOverviewStartState = (
    ossExpanded,  // start treeview fully expanded
    ossCollapsed  // start treeview fully collapsed
  );

type
  {
  IPreferences:
    Interface to object that exposes and persists user preferences.
  }
  IPreferences = interface(IInterface)
    ['{381B9A92-B528-47E1-AC04-90E1FFFDADA7}']
    function GetSourceCommentStyle: TCommentStyle;
      {Gets style of commenting used to describe snippets in generated code.
        @return Current commenting style.
      }
    procedure SetSourceCommentStyle(const Value: TCommentStyle);
      {Sets style of commenting to be used describe snippets in generated code.
        @param Value [in] Required commenting style.
      }
    property SourceCommentStyle: TCommentStyle
      read GetSourceCommentStyle write SetSourceCommentStyle;
      {Commenting style used to describe snippets in generated source code}

    ///  <summary>Gets flag that determines whether multi paragraph source code
    ///  is truncated to first paragraph in source code comments.</summary>
    function GetTruncateSourceComments: Boolean;
    ///  <summary>Sets flag that determines whether multi paragraph source code
    ///  is truncated to first paragraph in source code comments.</summary>
    procedure SetTruncateSourceComments(const Value: Boolean);
    ///  <summary>Flag determining whether multi paragraph source code is
    ///  truncated to first paragraph in source code comments.</summary>
    property TruncateSourceComments: Boolean
      read GetTruncateSourceComments write SetTruncateSourceComments;

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

    function GetOverviewStartState: TOverviewStartState;
      {Gets startup state of overview tree view.
        @return Current startup state.
      }
    procedure SetOverviewStartState(const Value: TOverviewStartState);
      {Sets startup state of overview tree view.
        @param Value [in] Required startup state.
      }
    property OverviewStartState: TOverviewStartState
      read GetOverviewStartState write SetOverviewStartState;
      {Startup state of overview treeview}

    function GetShowEmptySections: Boolean;
      {Gets flag that indicates whether empty sections are displayed in overview
      pane.
        @returns Flag value.
      }
    procedure SetShowEmptySections(const Value: Boolean);
      {Sets flag that indicates whether empty sections are displayed in overview
      pane.
        @param Value [in] New flag value.
      }
    property ShowEmptySections: Boolean
      read GetShowEmptySections write SetShowEmptySections;
      {Indicates whether empty sections are displayed in overview pane}

    function GetShowNewSnippetsInNewTabs: Boolean;
      {Gets flag that indicates whether new snippets and categories are
      displayed in new tabs in details pane.
        @returns Flag value.
      }
    procedure SetShowNewSnippetsInNewTabs(const Value: Boolean);
      {Sets flag that indicates whether new snippets and categories are
      displayed in new tabs in details pane.
        @param Value [in] New flag value.
      }
    property ShowNewSnippetsInNewTabs: Boolean
      read GetShowNewSnippetsInNewTabs write SetShowNewSnippetsInNewTabs;
      {Indicates whether new snippets and ca-tegories are displayed in new tabs
      in details pane}

    ///  <summary>Gets heading colour for specified database.</summary>
    ///  <param name="UserDefined">Boolean [in] Required database: True for user
    ///  database and False for main database.</param>
    ///  <returns>TColor. Required colour.</returns>
    function GetDBHeadingColour(UserDefined: Boolean): TColor;
    ///  <summary>Sets heading colour for specified database.</summary>
    ///  <param name="UserDefined">Boolean [in] Required database: True for user
    ///  database and False for main database.</param>
    ///  <param name="Value">TColor [in] Required heading colour.</param>
    procedure SetDBHeadingColour(UserDefined: Boolean;
      const Value: TColor);
    ///  <summary>Records colour to be used for headings of items from either
    ///  main database (UserDefined=False) or user database (UserDefined=True).
    ///  </summary>
    property DBHeadingColours[UserDefined: Boolean]: TColor
      read GetDBHeadingColour write SetDBHeadingColour;

    ///  <summary>Gets custom colours for headings for specified database.
    ///  </summary>
    ///  <param name="UserDefined">Boolean [in] Required database: True for user
    ///  database and False for main database.</param>
    ///  <returns>IStringList. String list containing custom colours.</returns>
    function GetDBHeadingCustomColours(UserDefined: Boolean): IStringList;
    ///  <summary>Sets custom colours for headings for specified database.
    ///  </summary>
    ///  <param name="UserDefined">Boolean [in] Required database: True for user
    ///  database and False for main database.</param>
    ///  <param name="Value">IStringList [in] String list containing custom
    ///  colours.</returns>
    procedure SetDBHeadingCustomColours(UserDefined: Boolean;
      Value: IStringList);
    ///  <summary>Records custom colours for headings of items from either main
    ///  database (UserDefined=False) or user database (UserDefined=True).
    ///  </summary>
    property DBHeadingCustomColours[UserDefined: Boolean]: IStringList
      read GetDBHeadingCustomColours write SetDBHeadingCustomColours;

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

    function GetCustomHiliteColours: IStringList;
      {Gets custom syntax highlighter colours.
        @return String list containing custom colours.
      }
    procedure SetCustomHiliteColours(const Colours: IStringList);
      {Sets new custom syntax highlighter colours.
        @param Colours [in] String list defining new colours.
      }
    property CustomHiliteColours: IStringList
      read GetCustomHiliteColours write SetCustomHiliteColours;
      {Custom syntax highlighter colours}

    function GetWarnings: IWarnings;
      {Gets information about warnings to be inhibited by code generator.
        @return Interface to object containing information.
      }
    procedure SetWarnings(Warnings: IWarnings);
      {Updates object that provides information about warnings to be inhibited
      by code generator.
        @param New warnings object.
    }
    property Warnings: IWarnings
      read GetWarnings write SetWarnings;
      {Information about warnings to be inhibited by code generator}

    function GetNewsAge: Integer;
      {Gets maximum age of news items to be displayed.
        @return Required age in days.
      }
    procedure SetNewsAge(const Age: Integer);
      {Sets maximum age of news items to be displayed.
        @param Age [in] Required age in days.
      }
    property NewsAge: Integer
      read GetNewsAge write SetNewsAge;
      {Maximum age of news items to be displayed}

    ///  <summary>Gets information about snippet detail page customisations.
    ///  </summary>
    function GetPageStructures: TSnippetPageStructures;
    ///  <summary>Updates information describing snippet detail page
    ///  customisations.</summary>
    procedure SetPageStructures(PageStructures: TSnippetPageStructures);
    ///  <summary>Information about snippet detail page customisations.
    ///  </summary>
    property PageStructures: TSnippetPageStructures
      read GetPageStructures write SetPageStructures;
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
  Hiliter.UAttrs, Hiliter.UPersist, IntfCommon, UExceptions, UColours,
  USettings;


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
      {Commenting style used to describe snippets in generated source code}
    fTruncateSourceComments: Boolean;
    fSourceSyntaxHilited: Boolean;
      {Indicates whether generated source is highlighted by default}
    fMeasurementUnits: TMeasurementUnits;
      {Measurement unit in use by application}
    fOverviewStartState: TOverviewStartState;
      {Startup state of overview treeview}
    fShowEmptySections: Boolean;
      {Indicates whether empty sections are displayed in overview pane}
    fShowNewSnippetsInNewTabs: Boolean;
      {Indicates whether new snippets and categories are displayed in new tabs
      in details pane}
    fDBHeadingColours: array[Boolean] of TColor;
    fDBHeadingCustomColours: array[Boolean] of IStringList;
    fPrinterOptions: TPrintOptions;
      {Default print options}
    fPrinterPageMargins: TPageMargins;
      {Default print page margins}
    fHiliteAttrs: IHiliteAttrs;
      {User defined syntax highlighter}
    fHiliteCustomColours: IStringList;
      {Custom highlighter colours}
    fWarnings: IWarnings;
      {Information about warnings to be inhibited by code generator}
    fNewsAge: Integer;
      {Maximum age of news items in days}
    fPageStructures: TSnippetPageStructures;
  protected // do not make strict
    { IPreferences methods }
    function GetSourceCommentStyle: TCommentStyle;
      {Gets style of commenting used to describe snippets in generated code.
        @return Current commenting style.
      }
    procedure SetSourceCommentStyle(const Value: TCommentStyle);
      {Sets style of commenting to be used describe snippets in generated code.
        @param Value [in] Required commenting style.
      }

    ///  <summary>Gets flag that determines whether multi paragraph source code
    ///  is truncated to first paragraph in source code comments.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetTruncateSourceComments: Boolean;

    ///  <summary>Sets flag that determines whether multi paragraph source code
    ///  is truncated to first paragraph in source code comments.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetTruncateSourceComments(const Value: Boolean);

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
    function GetOverviewStartState: TOverviewStartState;
      {Gets startup state of overview tree view.
        @return Current startup state.
      }
    procedure SetOverviewStartState(const Value: TOverviewStartState);
      {Sets startup state of overview tree view.
        @param Value [in] Required startup state.
      }
    function GetShowEmptySections: Boolean;
      {Gets flag that indicates whether empty sections are displayed in overview
      pane.
        @returns Flag value.
      }
    procedure SetShowEmptySections(const Value: Boolean);
      {Sets flag that indicates whether empty sections are displayed in overview
      pane.
        @param Value [in] New flag value.
      }
    function GetShowNewSnippetsInNewTabs: Boolean;
      {Gets flag that indicates whether new snippets and categories are
      displayed in new tabs in details pane.
        @returns Flag value.
      }
    procedure SetShowNewSnippetsInNewTabs(const Value: Boolean);
      {Sets flag that indicates whether new snippets and categories are
      displayed in new tabs in details pane.
        @param Value [in] New flag value.
      }

    ///  <summary>Gets heading colour for specified database.</summary>
    ///  <param name="UserDefined">Boolean [in] Required database: True for user
    ///  database and False for main database.</param>
    ///  <returns>TColor. Required colour.</returns>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetDBHeadingColour(UserDefined: Boolean): TColor;

    ///  <summary>Sets heading colour for specified database.</summary>
    ///  <param name="UserDefined">Boolean [in] Required database: True for user
    ///  database and False for main database.</param>
    ///  <param name="Value">TColor [in] Required heading colour.</param>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetDBHeadingColour(UserDefined: Boolean;
      const Value: TColor);

    ///  <summary>Gets custom colours for headings for specified database.
    ///  </summary>
    ///  <param name="UserDefined">Boolean [in] Required database: True for user
    ///  database and False for main database.</param>
    ///  <returns>IStringList. String list containing custom colours.</returns>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetDBHeadingCustomColours(UserDefined: Boolean): IStringList;

    ///  <summary>Sets custom colours for headings for specified database.
    ///  </summary>
    ///  <param name="UserDefined">Boolean [in] Required database: True for user
    ///  database and False for main database.</param>
    ///  <param name="Value">IStringList [in] String list containing custom
    ///  colours.</returns>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetDBHeadingCustomColours(UserDefined: Boolean;
      Value: IStringList);
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
    function GetCustomHiliteColours: IStringList;
      {Gets custom syntax highlighter colours.
        @return String list containing custom colours.
      }
    procedure SetCustomHiliteColours(const Colours: IStringList);
      {Sets new custom syntax highlighter colours.
        @param Colours [in] String list defining new colours.
      }
    function GetWarnings: IWarnings;
      {Gets information about warnings to be inhibited by code generator.
        @return Interface to object containing information.
      }
    procedure SetWarnings(Warnings: IWarnings);
      {Updates object that provides information about warnings to be inhibited
      by code generator.
        @param New warnings object.
    }
    function GetNewsAge: Integer;
      {Gets maximum age of news items to be displayed.
        @return Required age in days.
      }
    procedure SetNewsAge(const Age: Integer);
      {Sets maximum age of news items to be displayed.
        @param Age [in] Required age in days.
      }

    ///  <summary>Gets information about snippet detail page customisations.
    ///  </summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetPageStructures: TSnippetPageStructures;

    ///  <summary>Updates information describing snippet detail page
    ///  customisations.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetPageStructures(PageStructures: TSnippetPageStructures);

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
    destructor Destroy; override;
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
      // Sub-sections of ssPreferences ini file section
      cGeneral = 'General';
      cSourceCode = 'SourceCode';
      cPrinting = 'Printing';
      cHiliter = 'Hiliter';
      cCodeGenerator = 'CodeGen';
      cNews = 'News';
      cDisplay = 'Display';
      cPageStructures = 'SnippetPageStructure';
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
  if not Supports(Src, IPreferences, SrcPref) then
    raise EBug.Create(ClassName + '.Assign: Src is wrong type');
  // Copy the data
  Self.fSourceDefaultFileType := SrcPref.SourceDefaultFileType;
  Self.fSourceCommentStyle := SrcPref.SourceCommentStyle;
  Self.fTruncateSourceComments := SrcPref.TruncateSourceComments;
  Self.fSourceSyntaxHilited := SrcPref.SourceSyntaxHilited;
  Self.fMeasurementUnits := SrcPref.MeasurementUnits;
  Self.fOverviewStartState := SrcPref.OverviewStartState;
  Self.fShowEmptySections := SrcPref.ShowEmptySections;
  Self.fShowNewSnippetsInNewTabs := SrcPref.ShowNewSnippetsInNewTabs;
  Self.fDBHeadingColours[False] := SrcPref.DBHeadingColours[False];
  Self.fDBHeadingCustomColours[False] := SrcPref.DBHeadingCustomColours[False];
  Self.fDBHeadingColours[True] := SrcPref.DBHeadingColours[True];
  Self.fDBHeadingCustomColours[True] := SrcPref.DBHeadingCustomColours[True];
  Self.fPrinterOptions := SrcPref.PrinterOptions;
  Self.fPrinterPageMargins := SrcPref.PrinterPageMargins;
  Self.SetHiliteAttrs(SrcPref.HiliteAttrs);
  Self.SetCustomHiliteColours(SrcPref.CustomHiliteColours);
  Self.SetWarnings(SrcPref.Warnings);
  Self.SetNewsAge(SrcPref.NewsAge);
  Self.SetPageStructures(SrcPref.PageStructures);
end;

constructor TPreferences.Create;
  {Class constructor. Sets up object.
  }
begin
  inherited Create;
  fHiliteAttrs := THiliteAttrsFactory.CreateDefaultAttrs;
  fHiliteCustomColours := TIStringList.Create;
  fWarnings := TWarnings.Create;
  fDBHeadingCustomColours[False] := TIStringList.Create;
  fDBHeadingCustomColours[True] := TIStringList.Create;
  fPageStructures := TSnippetPageStructures.Create;
  TDefaultPageStructures.SetDefaults(fPageStructures);
end;

destructor TPreferences.Destroy;
begin
  fPageStructures.Free;
  inherited;
end;

function TPreferences.GetCustomHiliteColours: IStringList;
  {Gets custom syntax highlighter colours.
    @return String list containing custom colours.
  }
begin
  Result := fHiliteCustomColours;
end;

function TPreferences.GetDBHeadingColour(UserDefined: Boolean): TColor;
begin
  Result := fDBHeadingColours[UserDefined];
end;

function TPreferences.GetDBHeadingCustomColours(
  UserDefined: Boolean): IStringList;
begin
  Result := fDBHeadingCustomColours[UserDefined];
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

function TPreferences.GetNewsAge: Integer;
  {Gets maximum age of news items to be displayed.
    @return Required age in days.
  }
begin
  Result := fNewsAge;
end;

function TPreferences.GetOverviewStartState: TOverviewStartState;
  {Gets startup state of overview tree view.
    @return Current startup state.
  }
begin
  Result := fOverviewStartState;
end;

function TPreferences.GetPageStructures: TSnippetPageStructures;
begin
  Result := fPageStructures;
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

function TPreferences.GetShowEmptySections: Boolean;
begin
  Result := fShowEmptySections;
end;

function TPreferences.GetShowNewSnippetsInNewTabs: Boolean;
begin
  Result := fShowNewSnippetsInNewTabs;
end;

function TPreferences.GetSourceCommentStyle: TCommentStyle;
  {Gets style of commenting used to describe snippets in generated code.
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

function TPreferences.GetTruncateSourceComments: Boolean;
begin
  Result := fTruncateSourceComments;
end;

function TPreferences.GetWarnings: IWarnings;
begin
  Result := fWarnings;
end;

procedure TPreferences.SetCustomHiliteColours(const Colours: IStringList);
  {Sets new custom syntax highlighter colours.
    @param Colours [in] String list defining new colours.
  }
begin
  fHiliteCustomColours := Colours;
end;

procedure TPreferences.SetDBHeadingColour(UserDefined: Boolean;
  const Value: TColor);
begin
  fDBHeadingColours[UserDefined] := Value;
end;

procedure TPreferences.SetDBHeadingCustomColours(UserDefined: Boolean;
  Value: IStringList);
begin
  fDBHeadingCustomColours[UserDefined] := Value;
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

procedure TPreferences.SetNewsAge(const Age: Integer);
  {Sets maximum age of news items to be displayed.
    @param Age [in] Required age in days.
  }
begin
  fNewsAge := Age;
end;

procedure TPreferences.SetOverviewStartState(const Value: TOverviewStartState);
  {Sets startup state of overview tree view.
    @param Value [in] Required startup state.
  }
begin
  fOverviewStartState := Value;
end;

procedure TPreferences.SetPageStructures(
  PageStructures: TSnippetPageStructures);
begin
  fPageStructures.Assign(PageStructures);
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

procedure TPreferences.SetShowEmptySections(const Value: Boolean);
begin
  fShowEmptySections := Value;
end;

procedure TPreferences.SetShowNewSnippetsInNewTabs(const Value: Boolean);
begin
  fShowNewSnippetsInNewTabs := Value;
end;

procedure TPreferences.SetSourceCommentStyle(const Value: TCommentStyle);
  {Sets style of commenting to be used describe snippets in generated code.
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

procedure TPreferences.SetTruncateSourceComments(const Value: Boolean);
begin
  fTruncateSourceComments := Value;
end;

procedure TPreferences.SetWarnings(Warnings: IWarnings);
begin
  (fWarnings as IAssignable).Assign(Warnings);
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
  NewPref.TruncateSourceComments := Self.fTruncateSourceComments;
  NewPref.SourceSyntaxHilited := Self.fSourceSyntaxHilited;
  NewPref.MeasurementUnits := Self.fMeasurementUnits;
  NewPref.OverviewStartState := Self.fOverviewStartState;
  NewPref.ShowEmptySections := Self.fShowEmptySections;
  NewPref.ShowNewSnippetsInNewTabs := Self.fShowNewSnippetsInNewTabs;
  NewPref.DBHeadingColours[False] := Self.fDBHeadingColours[False];
  NewPref.DBHeadingCustomColours[False] := Self.fDBHeadingCustomColours[False];
  NewPref.DBHeadingColours[True] := Self.fDBHeadingColours[True];
  NewPref.DBHeadingCustomColours[True] := Self.fDBHeadingCustomColours[True];
  NewPref.PrinterOptions := Self.fPrinterOptions;
  NewPref.PrinterPageMargins := Self.fPrinterPageMargins;
  NewPref.HiliteAttrs := Self.GetHiliteAttrs;
  NewPref.CustomHiliteColours := Self.GetCustomHiliteColours;
  NewPref.Warnings := Self.GetWarnings;
  NewPref.NewsAge := Self.fNewsAge;
  NewPref.PageStructures := Self.fPageStructures;
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
  // Default maximum age of news items
  cDefNewsAge = 92;
begin
  inherited Create;

  // Read general section
  Storage := Settings.ReadSection(ssPreferences, cGeneral);
  fMeasurementUnits := TMeasurementUnits(
    StrToIntDef(Storage.ItemValues['Units'], Ord(DefaultMeasurementUnits))
  );

  // Read display section
  Storage := Settings.ReadSection(ssPreferences, cDisplay);
  fOverviewStartState := TOverviewStartState(
    StrToIntDef(Storage.ItemValues['OverviewStartState'], Ord(ossExpanded))
  );
  fShowEmptySections := Boolean(
    StrToIntDef(Storage.ItemValues['ShowEmptySections'], Ord(False))
  );
  fShowNewSnippetsInNewTabs := Boolean(
    StrToIntDef(Storage.ItemValues['ShowNewSnippetsInNewTabs'], Ord(False))
  );
  fDBHeadingColours[False] := TColor(
    StrToIntDef(Storage.ItemValues['MainDBHeadingColour'], clMainSnippet)
  );
  fDBHeadingColours[True] := TColor(
    StrToIntDef(Storage.ItemValues['UserDBHeadingColour'], clUserSnippet)
  );
  fDBHeadingCustomColours[False] := Storage.GetStrings(
    'MainDBHeadingCustomColourCount', 'MainDBHeadingCustomColour%d'
  );
  fDBHeadingCustomColours[True] := Storage.GetStrings(
    'UserDBHeadingCustomColourCount', 'UserDBHeadingCustomColour%d'
  );

  // Read source code section
  Storage := Settings.ReadSection(ssPreferences, cSourceCode);
  fSourceDefaultFileType := TSourceFileType(
    StrToIntDef(Storage.ItemValues['FileType'], Ord(sfPascal))
  );
  fSourceCommentStyle := TCommentStyle(
    StrToIntDef(Storage.ItemValues['CommentStyle'], Ord(csAfter))
  );
  fTruncateSourceComments := Boolean(
    StrToIntDef(Storage.ItemValues['TruncateComments'], Ord(False))
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
  // syntax highlighter attributes
  THiliterPersist.Load(Storage, fHiliteAttrs);
  // custom colours
  fHiliteCustomColours := Storage.GetStrings(
    'CustomColourCount', 'CustomColour%d'
  );

  // Read code generator section
  Storage := Settings.ReadSection(ssPreferences, cCodeGenerator);
  TWarningsPersist.Load(Storage, fWarnings);

  // Read news section
  Storage := Settings.ReadSection(ssPreferences, cNews);
  fNewsAge := StrToIntDef(Storage.ItemValues['MaxAge'], cDefNewsAge);

  // Read page structure section
  Storage := Settings.ReadSection(ssPreferences, cPageStructures);
  TSnippetPageStructuresPersist.Load(Storage, fPageStructures);
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

  // Write display section
  Storage := Settings.EmptySection(ssPreferences, cDisplay);
  Storage.ItemValues['OverviewStartState'] := IntToStr(
    Ord(fOverviewStartState)
  );
  Storage.ItemValues['ShowEmptySections'] := IntToStr(
    Ord(fShowEmptySections)
  );
  Storage.ItemValues['ShowNewSnippetsInNewTabs'] := IntToStr(
    Ord(fShowNewSnippetsInNewTabs)
  );
  Storage.ItemValues['MainDBHeadingColour'] := IntToStr(
    fDBHeadingColours[False]
  );
  Storage.ItemValues['UserDBHeadingColour'] := IntToStr(
    fDBHeadingColours[True]
  );
  Storage.SetStrings(
    'MainDBHeadingCustomColourCount',
    'MainDBHeadingCustomColour%d',
    fDBHeadingCustomColours[False]
  );
  Storage.SetStrings(
    'UserDBHeadingCustomColourCount',
    'UserDBHeadingCustomColour%d',
    fDBHeadingCustomColours[True]
  );
  Storage.Save;

  // Write source code section
  Storage := Settings.EmptySection(ssPreferences, cSourceCode);
  Storage.ItemValues['FileType'] := IntToStr(Ord(fSourceDefaultFileType));
  Storage.ItemValues['CommentStyle'] := IntToStr(Ord(fSourceCommentStyle));
  Storage.ItemValues['TruncateComments'] := IntToStr(
    Ord(fTruncateSourceComments)
  );
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
  // syntax highlighter attributes
  THiliterPersist.Save(Storage, fHiliteAttrs);
  // custom colours
  Storage.SetStrings(
    'CustomColourCount', 'CustomColour%d', fHiliteCustomColours
  );
  Storage.Save;

  // Write code generation section
  Storage := Settings.EmptySection(ssPreferences, cCodeGenerator);
  TWarningsPersist.Save(Storage, fWarnings);

  // Write news section
  Storage := Settings.EmptySection(ssPreferences, cNews);
  Storage.ItemValues['MaxAge'] := IntToStr(fNewsAge);
  Storage.Save;

  // Write page structure section
  Storage := Settings.EmptySection(ssPreferences, cPageStructures);
  TSnippetPageStructuresPersist.Save(Storage, fPageStructures);

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

