{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2013, Peter Johnson (www.delphidabbler.com).
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
  ///  <summary>Possible values for startup state of overview treeview.
  ///  </summary>
  TOverviewStartState = (
    ossExpanded,  // start treeview fully expanded
    ossCollapsed  // start treeview fully collapsed
  );

type
  ///  <summary>Interface supported by objects that maintain user preferences.
  ///  </summary>
  IPreferences = interface(IInterface)
    ['{381B9A92-B528-47E1-AC04-90E1FFFDADA7}']

    ///  <summary>Gets style of commenting used to describe snippets in
    ///  generated code.</summary>
    function GetSourceCommentStyle: TCommentStyle;
    ///  <summary>Sets style of commenting to be used describe snippets in
    ///  generated code.</summary>
    procedure SetSourceCommentStyle(const Value: TCommentStyle);
    ///  <summary>Commenting style used to describe snippets in generated source
    ///  code.</summary>
    property SourceCommentStyle: TCommentStyle
      read GetSourceCommentStyle write SetSourceCommentStyle;

    ///  <summary>Gets flag that determines whether multi-paragraph source code
    ///  is truncated to first paragraph in source code comments.</summary>
    function GetTruncateSourceComments: Boolean;
    ///  <summary>Sets flag that determines whether multi-paragraph source code
    ///  is truncated to first paragraph in source code comments.</summary>
    procedure SetTruncateSourceComments(const Value: Boolean);
    ///  <summary>Flag determining whether multi-paragraph source code is
    ///  truncated to first paragraph in source code comments.</summary>
    property TruncateSourceComments: Boolean
      read GetTruncateSourceComments write SetTruncateSourceComments;

    ///  <summary>Gets current default file extension / type used when writing
    ///  code snippets to file.</summary>
    function GetSourceDefaultFileType: TSourceFileType;
    ///  <summary>Sets default file extension / type to be used when writing
    ///  code snippets to file.</summary>
    procedure SetSourceDefaultFileType(const Value: TSourceFileType);
    ///  <summary>Default file extension / type used when writing code snippets
    ///  to file.</summary>
    property SourceDefaultFileType: TSourceFileType
      read GetSourceDefaultFileType write SetSourceDefaultFileType;

    ///  <summary>Gets current indicator of whether generated source is
    ///  highlighted by default.</summary>
    function GetSourceSyntaxHilited: Boolean;
    ///  <summary>Sets flag indicating whether generated source is highlighted
    ///  by default.</summary>
    procedure SetSourceSyntaxHilited(const Value: Boolean);
    ///  <summary>Indicates whether generated source is highlighted by default.
    ///  </summary>
    property SourceSyntaxHilited: Boolean
      read GetSourceSyntaxHilited write SetSourceSyntaxHilited;

    ///  <summary>Gets measurement units used by application.</summary>
    function GetMeasurementUnits: TMeasurementUnits;
    ///  <summary>Sets measurement units to be used by application.</summary>
    procedure SetMeasurementUnits(const Value: TMeasurementUnits);
    ///  <summary>Measurement units used by application.</summary>
    property MeasurementUnits: TMeasurementUnits
      read GetMeasurementUnits write SetMeasurementUnits;

    ///  <summary>Gets startup state of overview tree view.</summary>
    function GetOverviewStartState: TOverviewStartState;
    ///  <summary>Sets startup state of overview tree view.</summary>
    procedure SetOverviewStartState(const Value: TOverviewStartState);
    ///  <summary>Startup state of overview treeview.</summary>
    property OverviewStartState: TOverviewStartState
      read GetOverviewStartState write SetOverviewStartState;

    ///  <summary>Gets flag that indicates whether empty sections are displayed
    ///  in overview pane.</summary>
    function GetShowEmptySections: Boolean;
    ///  <summary>Sets flag that indicates whether empty sections are displayed
    ///  in overview pane.</summary>
    procedure SetShowEmptySections(const Value: Boolean);
    ///  <summary>Indicates whether empty sections are displayed in overview
    ///  pane.</summary>
    property ShowEmptySections: Boolean
      read GetShowEmptySections write SetShowEmptySections;

    ///  <summary>Gets flag that indicates whether new snippets and categories
    ///  are displayed in new tabs in details pane.</summary>
    function GetShowNewSnippetsInNewTabs: Boolean;
    ///  <summary>Sets flag that indicates whether new snippets and categories
    ///  are displayed in new tabs in details pane.</summary>
    procedure SetShowNewSnippetsInNewTabs(const Value: Boolean);
    ///  <summary>Indicates whether new snippets and ca-tegories are displayed
    ///  in new tabs in details pane.</summary>
    property ShowNewSnippetsInNewTabs: Boolean
      read GetShowNewSnippetsInNewTabs write SetShowNewSnippetsInNewTabs;

    ///  <summary>Gets heading colour used for snippets from a specified
    ///  database.</summary>
    ///  <param name="UserDefined">Boolean [in] Required database: True for user
    ///  database and False for inline database.</param>
    ///  <returns>TColor. Required colour.</returns>
    function GetDBHeadingColour(UserDefined: Boolean): TColor;
    ///  <summary>Sets heading colour used for snippets from a specified
    ///  database.</summary>
    ///  <param name="UserDefined">Boolean [in] Required database: True for user
    ///  database and False for online database.</param>
    ///  <param name="Value">TColor [in] Required heading colour.</param>
    procedure SetDBHeadingColour(UserDefined: Boolean;
      const Value: TColor);
    ///  <summary>Records colour to be used for headings of items from either
    ///  online database (UserDefined = False) or user database (UserDefined =
    ///  True).</summary>
    property DBHeadingColours[UserDefined: Boolean]: TColor
      read GetDBHeadingColour write SetDBHeadingColour;

    ///  <summary>Gets custom colours available for headings for specified
    ///  database.</summary>
    ///  <param name="UserDefined">Boolean [in] Required database: True for user
    ///  database and False for online database.</param>
    ///  <returns>IStringList. String list containing custom colours.</returns>
    function GetDBHeadingCustomColours(UserDefined: Boolean): IStringList;
    ///  <summary>Sets custom colours available for headings for specified
    ///  database.</summary>
    ///  <param name="UserDefined">Boolean [in] Required database: True for user
    ///  database and False for online database.</param>
    ///  <param name="Value">IStringList [in] String list containing custom
    ///  colours.</param>
    procedure SetDBHeadingCustomColours(UserDefined: Boolean;
      Value: IStringList);
    ///  <summary>Records custom colours available for headings of items from
    ///  either online database (UserDefined = False) or user database
    ///  (UserDefined = True).</summary>
    property DBHeadingCustomColours[UserDefined: Boolean]: IStringList
      read GetDBHeadingCustomColours write SetDBHeadingCustomColours;

    ///  <summary>Gets default print options.</summary>
    function GetPrinterOptions: TPrintOptions;
    ///  <summary>Sets default print options.</summary>
    procedure SetPrinterOptions(const Options: TPrintOptions);
    ///  <summary>Default print options.</summary>
    property PrinterOptions: TPrintOptions
      read GetPrinterOptions write SetPrinterOptions;

    ///  <summary>Gets default printer page margins.</summary>
    function GetPrinterPageMargins: TPageMargins;
    ///  <summary>Sets new default printer page margins.</summary>
    procedure SetPrinterPageMargins(const Margins: TPageMargins);
    ///  <summary>Default printer page margins.</summary>
    property PrinterPageMargins: TPageMargins
      read GetPrinterPageMargins write SetPrinterPageMargins;

    ///  <summary>Gets current user defined syntax highlighter.</summary>
    function GetHiliteAttrs: IHiliteAttrs;
    ///  <summary>Sets current user defined syntax highlighter.</summary>
    procedure SetHiliteAttrs(const Attrs: IHiliteAttrs);
    ///  <summary>Attributes of current user defined syntax highlighter.
    ///  </summary>
    property HiliteAttrs: IHiliteAttrs
      read GetHiliteAttrs write SetHiliteAttrs;

    ///  <summary>Gets object containing the attributes of all the named user
    ///  defined syntax highlighters.</summary>
    function GetNamedHiliteAttrs: INamedHiliteAttrs;
    ///  <summary>Stores a copy of the given object containing the attributes of
    ///  all the 'named' user defined syntax highlighters.</summary>
    procedure SetNamedHiliteAttrs(NamedHiliteAttrs: INamedHiliteAttrs);
    ///  <summary>Reference tp object containing attributes of all the 'named'
    ///  user defined syntax highlighters.</summary>
    property NamedHiliteAttrs: INamedHiliteAttrs
      read GetNamedHiliteAttrs write SetNamedHiliteAttrs;

    ///  <summary>Gets custom colours available for syntax highlighter.
    ///  </summary>
    function GetCustomHiliteColours: IStringList;
    ///  <summary>Sets custom colours available for syntax highlighter.
    ///  </summary>
    procedure SetCustomHiliteColours(const Colours: IStringList);
    ///  <summary>Custom colours available for syntax highlighters.</summary>
    property CustomHiliteColours: IStringList
      read GetCustomHiliteColours write SetCustomHiliteColours;

    ///  <summary>Gets object containing information about warnings to be
    ///  enabled or disabled by code generator.</summary>
    function GetWarnings: IWarnings;
    ///  <summary>Stores a copy of the object containing information about
    ///  warnings to be enabled or disabled by code generator.</summary>
    procedure SetWarnings(Warnings: IWarnings);
    ///  <summary>Reference to object containing information about warnings to
    ///  be enabled or disabled by code generator.</summary>
    property Warnings: IWarnings
      read GetWarnings write SetWarnings;

    ///  <summary>Gets maximum age of items to be read from news feed.</summary>
    function GetNewsAge: Integer;
    ///  <summary>Sets maximum age of items to be read from news feed.</summary>
    procedure SetNewsAge(const Age: Integer);
    ///  <summary>Maximum age of items to be read from news feed.</summary>
    property NewsAge: Integer
      read GetNewsAge write SetNewsAge;

    ///  <summary>Gets information describing snippet detail page
    ///  customisations.</summary>
    function GetPageStructures: TSnippetPageStructures;
    ///  <summary>Updates information describing snippet detail page
    ///  customisations.</summary>
    procedure SetPageStructures(PageStructures: TSnippetPageStructures);
    ///  <summary>Information describing snippet detail page customisations.
    ///  </summary>
    property PageStructures: TSnippetPageStructures
      read GetPageStructures write SetPageStructures;

    ///  <summary>Gets flag that indicates whether the program should
    ///  automatically check for program updates.</summary>
    function GetAutoCheckProgramUpdates: Boolean;
    ///  <summary>Sets flag that indicates whether the program should
    ///  automatically check for program updates.</summary>
    procedure SetAutoCheckProgramUpdates(const Value: Boolean);
    ///  <summary>Indicates whether the program should automatically check
    ///  for program updates.</summary>
    property AutoCheckProgramUpdates: Boolean
      read GetAutoCheckProgramUpdates write SetAutoCheckProgramUpdates;

    ///  <summary>Gets flag that indicates whether the program should
    ///  automatically check for database updates.</summary>
    function GetAutoCheckDatabaseUpdates: Boolean;
    ///  <summary>Sets flag that indicates whether the program should
    ///  automatically check for database updates.</summary>
    procedure SetAutoCheckDatabaseUpdates(const Value: Boolean);
    ///  <summary>Indicates whether the program should automatically check
    ///  for database updates.</summary>
    property AutoCheckDatabaseUpdates: Boolean
      read GetAutoCheckDatabaseUpdates write SetAutoCheckDatabaseUpdates;
  end;


///  <summary>Provides access to a singleton implementation IPreferences that
///  can also persist the preferences.</summary>
function Preferences: IPreferences;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  Hiliter.UAttrs, Hiliter.UPersist, IntfCommon, UExceptions, UColours,
  USettings;


type
  ///  <summary>Class that maintains a copy of the user preferences.</summary>
  ///  <remarks>
  ///  <para>This class does not persist the preferences. It is intended for
  ///  use as a temporary local copy of the preferences singleton for
  ///  modification.</para>
  ///  <para>To save changes made to an instance of this class, assign the
  ///  instance to the singleton.</para>
  ///  <para>Instances should not be created directly: the singleton's Clone
  ///  method must be used to do this.</para>
  ///  </remarks>
  TPreferences = class(TInterfacedObject,
    IPreferences, IAssignable
  )
  strict protected
    var
      ///  <summary>Default file extension / type used when writing code
      ///  snippets  file.</summary>
      fSourceDefaultFileType: TSourceFileType;
      ///  <summary>Commenting style used to describe snippets in generated
      ///  source code.</summary>
      fSourceCommentStyle: TCommentStyle;
      ///  <summary>Flag determining whether multi-paragraph source code is
      ///  truncated to first paragraph in source code comments.</summary>
      fTruncateSourceComments: Boolean;
      ///  <summary>Indicates whether generated source is highlighted by
      ///  default.</summary>
      fSourceSyntaxHilited: Boolean;
      ///  <summary>Measurement units used by application.</summary>
      fMeasurementUnits: TMeasurementUnits;
      ///  <summary>Startup state of overview treeview.</summary>
      fOverviewStartState: TOverviewStartState;
      ///  <summary>Indicates whether empty sections are displayed in overview
      ///  pane.</summary>
      fShowEmptySections: Boolean;
      ///  <summary>Indicates whether new snippets and ca-tegories are displayed
      ///  in new tabs in details pane.</summary>
      fShowNewSnippetsInNewTabs: Boolean;
      ///  <summary>Records colour to be used for headings of items from either
      ///  online database (UserDefined = False) or user database (UserDefined =
      ///  True).</summary>
      fDBHeadingColours: array[Boolean] of TColor;
      ///  <summary>Records custom colours available for headings of items from
      ///  either online database (UserDefined = False) or user database
      ///  (UserDefined = True).</summary>
      fDBHeadingCustomColours: array[Boolean] of IStringList;
      ///  <summary>Default print options.</summary>
      fPrinterOptions: TPrintOptions;
      ///  <summary>Default printer page margins.</summary>
      fPrinterPageMargins: TPageMargins;
      ///  <summary>Attributes of current user defined syntax highlighter.
      ///  </summary>
      fHiliteAttrs: IHiliteAttrs;
      ///  <summary>Reference tp object containing attributes of all the 'named'
      ///  user defined syntax highlighters.</summary>
      fNamedHiliteAttrs: INamedHiliteAttrs;
      ///  <summary>Custom colours available for syntax highlighters.</summary>
      fHiliteCustomColours: IStringList;
      ///  <summary>Reference to object containing information about warnings to
      ///  be enabled or disabled by code generator.</summary>
      fWarnings: IWarnings;
      ///  <summary>Maximum age of items to be read from news feed.</summary>
      fNewsAge: Integer;
      ///  <summary>Information describing snippet detail page customisations.
      ///  </summary>
      fPageStructures: TSnippetPageStructures;
      ///  <summary>Indicates whether the program should automatically check
      ///  for program updates.</summary>
      fAutoCheckProgramUpdates: Boolean;
      ///  <summary>Indicates whether the program should automatically check
      ///  for database updates.</summary>
      fAutoCheckDatabaseUpdates: Boolean;
  public
    ///  <summary>Constructs a new object instance.</summary>
    constructor Create;

    ///  <summary>Destroys object instance.</summary>
    destructor Destroy; override;

    ///  <summary>Gets style of commenting used to describe snippets in
    ///  generated code.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetSourceCommentStyle: TCommentStyle;

    ///  <summary>Sets style of commenting to be used describe snippets in
    ///  generated code.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetSourceCommentStyle(const Value: TCommentStyle);

    ///  <summary>Gets flag that determines whether multi-paragraph source code
    ///  is truncated to first paragraph in source code comments.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetTruncateSourceComments: Boolean;

    ///  <summary>Sets flag that determines whether multi-paragraph source code
    ///  is truncated to first paragraph in source code comments.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetTruncateSourceComments(const Value: Boolean);

    ///  <summary>Gets current default file extension / type used when writing
    ///  code snippets to file.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetSourceDefaultFileType: TSourceFileType;

    ///  <summary>Sets default file extension / type to be used when writing
    ///  code snippets to file.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetSourceDefaultFileType(const Value: TSourceFileType);

    ///  <summary>Gets current indicator of whether generated source is
    ///  highlighted by default.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetSourceSyntaxHilited: Boolean;

    ///  <summary>Sets flag indicating whether generated source is highlighted
    ///  by default.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetSourceSyntaxHilited(const Value: Boolean);

    ///  <summary>Gets measurement units used by application.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetMeasurementUnits: TMeasurementUnits;

    ///  <summary>Sets measurement units to be used by application.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetMeasurementUnits(const Value: TMeasurementUnits);

    ///  <summary>Gets startup state of overview tree view.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetOverviewStartState: TOverviewStartState;

    ///  <summary>Sets startup state of overview tree view.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetOverviewStartState(const Value: TOverviewStartState);

    ///  <summary>Gets flag that indicates whether empty sections are displayed
    ///  in overview pane.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetShowEmptySections: Boolean;

    ///  <summary>Sets flag that indicates whether empty sections are displayed
    ///  in overview pane.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetShowEmptySections(const Value: Boolean);

    ///  <summary>Gets flag that indicates whether new snippets and categories
    ///  are displayed in new tabs in details pane.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetShowNewSnippetsInNewTabs: Boolean;

    ///  <summary>Sets flag that indicates whether new snippets and categories
    ///  are displayed in new tabs in details pane.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetShowNewSnippetsInNewTabs(const Value: Boolean);

    ///  <summary>Gets heading colour used for snippets from a specified
    ///  database.</summary>
    ///  <param name="UserDefined">Boolean [in] Required database: True for user
    ///  database and False for inline database.</param>
    ///  <returns>TColor. Required colour.</returns>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetDBHeadingColour(UserDefined: Boolean): TColor;

    ///  <summary>Sets heading colour used for snippets from a specified
    ///  database.</summary>
    ///  <param name="UserDefined">Boolean [in] Required database: True for user
    ///  database and False for online database.</param>
    ///  <param name="Value">TColor [in] Required heading colour.</param>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetDBHeadingColour(UserDefined: Boolean;
      const Value: TColor);

    ///  <summary>Gets custom colours available for headings for specified
    ///  database.</summary>
    ///  <param name="UserDefined">Boolean [in] Required database: True for user
    ///  database and False for online database.</param>
    ///  <returns>IStringList. String list containing custom colours.</returns>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetDBHeadingCustomColours(UserDefined: Boolean): IStringList;

    ///  <summary>Sets custom colours available for headings for specified
    ///  database.</summary>
    ///  <param name="UserDefined">Boolean [in] Required database: True for user
    ///  database and False for online database.</param>
    ///  <param name="Value">IStringList [in] String list containing custom
    ///  colours.</param>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetDBHeadingCustomColours(UserDefined: Boolean;
      Value: IStringList);

    ///  <summary>Gets default print options.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetPrinterOptions: TPrintOptions;

    ///  <summary>Sets default print options.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetPrinterOptions(const Options: TPrintOptions);

    ///  <summary>Gets default printer page margins.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetPrinterPageMargins: TPageMargins;

    ///  <summary>Sets new default printer page margins.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetPrinterPageMargins(const Margins: TPageMargins);

    ///  <summary>Gets current user defined syntax highlighter.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetHiliteAttrs: IHiliteAttrs;

    ///  <summary>Sets current user defined syntax highlighter.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetHiliteAttrs(const Attrs: IHiliteAttrs);

    ///  <summary>Gets object containing the attributes of all the named user
    ///  defined syntax highlighters.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetNamedHiliteAttrs: INamedHiliteAttrs;

    ///  <summary>Stores a copy of the given object containing the attributes of
    ///  all the 'named' user defined syntax highlighters.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetNamedHiliteAttrs(NamedHiliteAttrs: INamedHiliteAttrs);

    ///  <summary>Gets custom colours available for syntax highlighter.
    ///  </summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetCustomHiliteColours: IStringList;

    ///  <summary>Sets custom colours available for syntax highlighter.
    ///  </summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetCustomHiliteColours(const Colours: IStringList);

    ///  <summary>Gets object containing information about warnings to be
    ///  enabled or disabled by code generator.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetWarnings: IWarnings;

    ///  <summary>Stores a copy of the object containing information about
    ///  warnings to be enabled or disabled by code generator.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetWarnings(Warnings: IWarnings);

    ///  <summary>Gets maximum age of items to be read from news feed.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetNewsAge: Integer;

    ///  <summary>Sets maximum age of items to be read from news feed.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetNewsAge(const Age: Integer);

    ///  <summary>Gets information describing snippet detail page
    ///  customisations.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetPageStructures: TSnippetPageStructures;

    ///  <summary>Updates information describing snippet detail page
    ///  customisations.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetPageStructures(PageStructures: TSnippetPageStructures);

    ///  <summary>Gets flag that indicates whether the program should
    ///  automatically check for program updates.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetAutoCheckProgramUpdates: Boolean;

    ///  <summary>Sets flag that indicates whether the program should
    ///  automatically check for program updates.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetAutoCheckProgramUpdates(const Value: Boolean);

    ///  <summary>Gets flag that indicates whether the program should
    ///  automatically check for database updates.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetAutoCheckDatabaseUpdates: Boolean;

    ///  <summary>Sets flag that indicates whether the program should
    ///  automatically check for database updates.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetAutoCheckDatabaseUpdates(const Value: Boolean);

    ///  <summary>Assigns the properties of the given object to this object.
    ///  </summary>
    ///  <exceptions>Raises EBug if Src does not support IPreferences.
    ///  </exceptions>
    ///  <remarks>Method of IAssignable</remarks>
    procedure Assign(const Src: IInterface);

  end;

type
  ///  <summary>Descendant class that adds the ability to persist preferences to
  ///  storage.</summary>
  ///  <remarks>Designed for use as a singleton with the ability to create
  ///  non-persistent clones of itself that can be used for editing. Data from
  ///  such clones can be assigned to the singleton.</remarks>
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
      cUpdating = 'Updating';
    class var
      ///  <summary>Stores reference to singleton instance of this class.
      ///  </summary>
      fInstance: IPreferences;
  strict private
    ///  <summary>Returns singleton instance initialised from persistent
    ///  storage.</summary>
    class function GetInstance: IPreferences; static;
  public
    ///  <summary>Creates a new object instance and reads properties from
    ///  persistent storage.</summary>
    constructor Create;
    ///  <summary>Writes properties to persistent storage then destroys the
    ///  instance.</summary>
    destructor Destroy; override;
    ///  <summary>Creates a new non-persistent instance that is a copy of this
    ///  object.</summary>
    ///  <remarks>Method of IClonable.</remarks>
    function Clone: IInterface;
    ///  <summary>Returns a reference to a singleton instance of the class.
    ///  </summary>
    class property Instance: IPreferences
      read GetInstance;
  end;


function Preferences: IPreferences;
begin
  Result := TPreferencesPersist.Instance;
end;

{ TPreferences }

procedure TPreferences.Assign(const Src: IInterface);
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
  Self.SetNamedHiliteAttrs(SrcPref.NamedHiliteAttrs);
  Self.SetCustomHiliteColours(SrcPref.CustomHiliteColours);
  Self.SetWarnings(SrcPref.Warnings);
  Self.SetNewsAge(SrcPref.NewsAge);
  Self.SetPageStructures(SrcPref.PageStructures);
  Self.fAutoCheckProgramUpdates := SrcPref.AutoCheckProgramUpdates;
  Self.fAutoCheckDatabaseUpdates := SrcPref.AutoCheckDatabaseUpdates;

end;

constructor TPreferences.Create;
begin
  inherited Create;
  fHiliteAttrs := THiliteAttrsFactory.CreateDefaultAttrs;
  fNamedHiliteAttrs := THiliteAttrsFactory.CreateNamedAttrs;
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

function TPreferences.GetAutoCheckDatabaseUpdates: Boolean;
begin
  Result := fAutoCheckDatabaseUpdates;
end;

function TPreferences.GetAutoCheckProgramUpdates: Boolean;
begin
  Result := fAutoCheckProgramUpdates;
end;

function TPreferences.GetCustomHiliteColours: IStringList;
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
begin
  Result := fHiliteAttrs;
end;

function TPreferences.GetMeasurementUnits: TMeasurementUnits;
begin
  Result := fMeasurementUnits;
end;

function TPreferences.GetNamedHiliteAttrs: INamedHiliteAttrs;
begin
  Result := fNamedHiliteAttrs;
end;

function TPreferences.GetNewsAge: Integer;
begin
  Result := fNewsAge;
end;

function TPreferences.GetOverviewStartState: TOverviewStartState;
begin
  Result := fOverviewStartState;
end;

function TPreferences.GetPageStructures: TSnippetPageStructures;
begin
  Result := fPageStructures;
end;

function TPreferences.GetPrinterOptions: TPrintOptions;
begin
  Result := fPrinterOptions;
end;

function TPreferences.GetPrinterPageMargins: TPageMargins;
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
begin
  Result := fSourceCommentStyle;
end;

function TPreferences.GetSourceDefaultFileType: TSourceFileType;
begin
  Result := fSourceDefaultFileType;
end;

function TPreferences.GetSourceSyntaxHilited: Boolean;
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

procedure TPreferences.SetAutoCheckDatabaseUpdates(const Value: Boolean);
begin
  fAutoCheckDatabaseUpdates := Value;
end;

procedure TPreferences.SetAutoCheckProgramUpdates(const Value: Boolean);
begin
  fAutoCheckProgramUpdates := Value;
end;

procedure TPreferences.SetCustomHiliteColours(const Colours: IStringList);
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
begin
  (fHiliteAttrs as IAssignable).Assign(Attrs);
end;

procedure TPreferences.SetMeasurementUnits(const Value: TMeasurementUnits);
begin
  fMeasurementUnits := Value;
end;

procedure TPreferences.SetNamedHiliteAttrs(NamedHiliteAttrs: INamedHiliteAttrs);
begin
  (fNamedHiliteAttrs as IAssignable).Assign(NamedHiliteAttrs);
end;

procedure TPreferences.SetNewsAge(const Age: Integer);
begin
  fNewsAge := Age;
end;

procedure TPreferences.SetOverviewStartState(const Value: TOverviewStartState);
begin
  fOverviewStartState := Value;
end;

procedure TPreferences.SetPageStructures(
  PageStructures: TSnippetPageStructures);
begin
  fPageStructures.Assign(PageStructures);
end;

procedure TPreferences.SetPrinterOptions(const Options: TPrintOptions);
begin
  fPrinterOptions := Options;
end;

procedure TPreferences.SetPrinterPageMargins(const Margins: TPageMargins);
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
begin
  fSourceCommentStyle := Value;
end;

procedure TPreferences.SetSourceDefaultFileType(const Value: TSourceFileType);
begin
  fSourceDefaultFileType := Value;
end;

procedure TPreferences.SetSourceSyntaxHilited(const Value: Boolean);
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
  NewPref.NamedHiliteAttrs := Self.GetNamedHiliteAttrs;
  NewPref.CustomHiliteColours := Self.GetCustomHiliteColours;
  NewPref.Warnings := Self.GetWarnings;
  NewPref.NewsAge := Self.fNewsAge;
  NewPref.PageStructures := Self.fPageStructures;
  NewPref.AutoCheckProgramUpdates := Self.fAutoCheckProgramUpdates;
  NewPref.AutoCheckDatabaseUpdates := Self.fAutoCheckDatabaseUpdates;
end;

constructor TPreferencesPersist.Create;
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
    Storage.GetInteger('Units', Ord(DefaultMeasurementUnits))
  );

  // Read display section
  Storage := Settings.ReadSection(ssPreferences, cDisplay);
  fOverviewStartState := TOverviewStartState(
    Storage.GetInteger('OverviewStartState', Ord(ossExpanded))
  );
  fShowEmptySections := Storage.GetBoolean('ShowEmptySections', False);
  fShowNewSnippetsInNewTabs := Storage.GetBoolean(
    'ShowNewSnippetsInNewTabs', False
  );
  fDBHeadingColours[False] := TColor(
    Storage.GetInteger('MainDBHeadingColour', clMainSnippet)
  );
  fDBHeadingColours[True] := TColor(
    Storage.GetInteger('UserDBHeadingColour', clUserSnippet)
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
    Storage.GetInteger('FileType', Ord(sfPascal))
  );
  fSourceCommentStyle := TCommentStyle(
    Storage.GetInteger('CommentStyle', Ord(csAfter))
  );
  fTruncateSourceComments := Storage.GetBoolean('TruncateComments', False);
  fSourceSyntaxHilited := Storage.GetBoolean('UseSyntaxHiliting', False);

  // Read printing section
  Storage := Settings.ReadSection(ssPreferences, cPrinting);
  fPrinterOptions := [];
  if Storage.GetBoolean('UseColor', True) then
    Include(fPrinterOptions, poUseColor);
  if Storage.GetBoolean('SyntaxPrint', True) then
    Include(fPrinterOptions, poSyntaxPrint);
  fPrinterPageMargins := TPageMargins.Create(
    Storage.GetFloat('LeftMargin', cPrintPageMarginSizeMM),
    Storage.GetFloat('TopMargin', cPrintPageMarginSizeMM),
    Storage.GetFloat('RightMargin', cPrintPageMarginSizeMM),
    Storage.GetFloat('BottomMargin', cPrintPageMarginSizeMM)
  );

  // Read syntax highlighter section
  Storage := Settings.ReadSection(ssPreferences, cHiliter);
  // syntax highlighter attributes
  THiliterPersist.Load(Storage, fHiliteAttrs);
  THiliterPersist.LoadNamed(Storage, fNamedHiliteAttrs);
  // custom colours
  fHiliteCustomColours := Storage.GetStrings(
    'CustomColourCount', 'CustomColour%d'
  );

  // Read code generator section
  Storage := Settings.ReadSection(ssPreferences, cCodeGenerator);
  TWarningsPersist.Load(Storage, fWarnings);

  // Read news section
  Storage := Settings.ReadSection(ssPreferences, cNews);
  fNewsAge := Storage.GetInteger('MaxAge', cDefNewsAge);

  // Read page structure section
  Storage := Settings.ReadSection(ssPreferences, cPageStructures);
  TSnippetPageStructuresPersist.Load(Storage, fPageStructures);

  // Read updating section
  Storage := Settings.ReadSection(ssPreferences, cUpdating);
  fAutoCheckProgramUpdates := Storage.GetBoolean('AutoCheckProgram', True);
  fAutoCheckDatabaseUpdates := Storage.GetBoolean('AutoCheckDatabase', True);

end;

destructor TPreferencesPersist.Destroy;
var
  Storage: ISettingsSection;  // object used to access persistent storage
begin
  // Write general section
  Storage := Settings.EmptySection(ssPreferences, cGeneral);
  Storage.SetInteger('Units', Ord(fMeasurementUnits));
  Storage.Save;

  // Write display section
  Storage := Settings.EmptySection(ssPreferences, cDisplay);
  Storage.SetInteger('OverviewStartState', Ord(fOverviewStartState));
  Storage.SetBoolean('ShowEmptySections', fShowEmptySections);
  Storage.SetBoolean('ShowNewSnippetsInNewTabs', fShowNewSnippetsInNewTabs);
  Storage.SetInteger('MainDBHeadingColour', fDBHeadingColours[False]);
  Storage.SetInteger('UserDBHeadingColour', fDBHeadingColours[True]);
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
  Storage.SetInteger('FileType', Ord(fSourceDefaultFileType));
  Storage.SetInteger('CommentStyle', Ord(fSourceCommentStyle));
  Storage.SetBoolean('TruncateComments', fTruncateSourceComments);
  Storage.SetBoolean('UseSyntaxHiliting', fSourceSyntaxHilited);
  Storage.Save;

  // Write printing section
  Storage := Settings.EmptySection(ssPreferences, cPrinting);
  Storage.SetBoolean('UseColor', poUseColor in fPrinterOptions);
  Storage.SetBoolean('SyntaxPrint', poSyntaxPrint in fPrinterOptions);
  Storage.SetFloat('LeftMargin', fPrinterPageMargins.Left);
  Storage.SetFloat('TopMargin', fPrinterPageMargins.Top);
  Storage.SetFloat('RightMargin', fPrinterPageMargins.Right);
  Storage.SetFloat('BottomMargin', fPrinterPageMargins.Bottom);
  Storage.Save;

  // Write syntax highlighter section
  Storage := Settings.EmptySection(ssPreferences, cHiliter);
  // syntax highlighter attributes
  THiliterPersist.Save(Storage, fHiliteAttrs);
  THiliterPersist.SaveNamed(Storage, fNamedHiliteAttrs);
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
  Storage.SetInteger('MaxAge', fNewsAge);
  Storage.Save;

  // Write page structure section
  Storage := Settings.EmptySection(ssPreferences, cPageStructures);
  TSnippetPageStructuresPersist.Save(Storage, fPageStructures);

  // Write updating section
  Storage := Settings.EmptySection(ssPreferences, cUpdating);
  Storage.SetBoolean('AutoCheckProgram', fAutoCheckProgramUpdates);
  Storage.SetBoolean('AutoCheckDatabase', fAutoCheckDatabaseUpdates);
  Storage.Save;

  inherited;
end;

class function TPreferencesPersist.GetInstance: IPreferences;
begin
  if not Assigned(fInstance) then
    fInstance := TPreferencesPersist.Create;
  Result := fInstance;
end;

end.

