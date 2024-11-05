{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2022, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a singletion object that exposes and persists user preferences.
}


unit UPreferences;


interface


uses
  // Delphi
  Graphics,
  // Project
  DB.UCollections,
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

    ///  <summary>Gets last tab displayed by Preferences dialogue box when it
    ///  was last closed, or empty string if the tab is not known.
    ///  </summary>
    ///  <remarks>This is meta data about the dialogue box itself, not about
    ///  user preferences.</remarks>
    function GetLastTab: string;
    ///  <summary>Sets last tab displayed by Preferences dialogue box when it
    ///  was last closed.</summary>
    ///  <remarks>This is meta data about the dialogue box itself, not about
    ///  user preferences.</remarks>
    procedure SetLastTab(const Value: string);
    ///  <summary>Last tab displayed by Preferences dialogue box when it was
    ///  last closed, or empty string if the tab is not known.</summary>
    ///  <remarks>This is meta data about the dialogue box itself, not about
    ///  user preferences.</remarks>
    property LastTab: string read GetLastTab write SetLastTab;

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
    ///  collection.</summary>
    ///  <param name="ACollectionID">TCollectionID [in] ID of required
    ///  collection.</param>
    ///  <returns>TColor. Required colour.</returns>
    function GetDBHeadingColour(const ACollectionID: TCollectionID): TColor;
    ///  <summary>Sets heading colour used for snippets from a specified
    ///  collection.</summary>
    ///  <param name="ACollectionID">TCollectionID [in] ID of required
    ///  collection.</param>
    ///  <returns>TColor. Required colour.</returns>
    procedure SetDBHeadingColour(const ACollectionID: TCollectionID;
      const Value: TColor);

    ///  <summary>Gets custom colours available for headings for specified
    ///  collection.</summary>
    ///  <param name="ACollectionID">TCollectionID [in] ID of required
    ///  collection.</param>
    ///  <returns>IStringList. String list containing custom colours.</returns>
    function GetDBHeadingCustomColours(const ACollectionID: TCollectionID):
      IStringList;
    ///  <summary>Sets custom colours available for headings for specified
    ///  collection.</summary>
    ///  <param name="ACollectionID">TCollectionID [in] ID of required
    ///  collection.</param>
    ///  <param name="Value">IStringList [in] String list containing custom
    ///  colours.</param>
    procedure SetDBHeadingCustomColours(const ACollectionID: TCollectionID;
      Value: IStringList);

    ///  <summary>Gets size of font used in overview pane tree view.</summary>
    function GetOverviewFontSize: Integer;
    ///  <summary>Sets size of font used in overview pane tree view.</summary>
    procedure SetOverviewFontSize(const Value: Integer);
    ///  <summary>Size of font used in overview pane tree view.</summary>
    property OverviewFontSize: Integer
      read GetOverviewFontSize write SetOverviewFontSize;

    ///  <summary>Gets size of base font used in detail pane.</summary>
    function GetDetailFontSize: Integer;
    ///  <summary>Sets size of base font used in detail pane.</summary>
    procedure SetDetailFontSize(const Value: Integer);
    ///  <summary>Size of base font used in detail pane.</summary>
    property DetailFontSize: Integer
      read GetDetailFontSize write SetDetailFontSize;

    ///  <summary>Gets colour used for background of source code in main
    ///  display.</summary>
    function GetSourceCodeBGColour: TColor;
    ///  <summary>Sets colour used for background of source code in main
    ///  display.</summary>
    procedure SetSourceCodeBGColour(const Value: TColor);
    ///  <summary>Colour used for background of source code in main display.
    ///  </summary>
    property SourceCodeBGColour: TColor
      read GetSourceCodeBGColour write SetSourceCodeBGColour;

    ///  <summary>Gets custom colours available for use as background colour of
    ///  source code in main display as a string list.</summary>
    function GetSourceCodeBGCustomColours: IStringList;
    ///  <summary>Sets custom colours available for use as background colour of
    ///  source code in main display from given string list.</summary>
    procedure SetSourceCodeBGCustomColours(Value: IStringList);
    ///  <summary>Records custom colours available for use as background colour
    ///  of source code in main display as a string list.</summary>
    property SourceCodeBGCustomColours: IStringList
      read GetSourceCodeBGCustomColours write SetSourceCodeBGCustomColours;

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
  UFontHelper, USettings;


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
      fLastTab: string;
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
      ///  main (False) or user (True) collections.</summary>
      fDBHeadingColours: array[Boolean] of TColor;
        {TODO -cCollections: WARNING. The fDBHeadingColours field only supports
                the two original main & user collections. This MUST be changed
                once more than two snippet collections are supported}
      ///  <summary>Records custom colours available for headings of items from
      ///  either main (False) or user (True) collections.</summary>
      fDBHeadingCustomColours: array[Boolean] of IStringList;
        {TODO -cCollections: WARNING. The fDBHeadingCustomColours field only
                supports the two original main & user collections. This MUST be
                changed once more than two snippet collections are supported}
      ///  <summary>Records size of font used in overview pane tree view.
      ///  </summary>
      fOverviewFontSize: Integer;
      ///  <summary>Records size of font used in details pane.</summary>
      fDetailFontSize: Integer;
      ///  <summary>Records colour used for background of source code in main
      ///  display.</summary>
      fSourceCodeBGColour: TColor;
      ///  <summary>Records custom colours available for use as background
      ///  colour of source code in main display.</summary>
      fSourceCodeBGCustomColours: IStringList;
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
      ///  <summary>Information describing snippet detail page customisations.
      ///  </summary>
      fPageStructures: TSnippetPageStructures;
    ///  <summary>Returns default font size for overview pane tree view.
    ///  </summary>
    function DefaultOverviewFontSize: Integer;
    ///  <summary>Returns default font size for details pane.</summary>
    function DefaultDetailFontSize: Integer;

    function __TMP__UseUserDBHeadingColour(const ACollectionID: TCollectionID):
      Boolean;
  public
    ///  <summary>Constructs a new object instance.</summary>
    constructor Create;

    ///  <summary>Destroys object instance.</summary>
    destructor Destroy; override;

    ///  <summary>Gets last tab displayed by Preferences dialogue box when it
    ///  was last closed, or empty string if the tab is not known.
    ///  </summary>
    ///  <remarks>
    ///  <para>This is meta data about the dialogue box itself, not about
    ///  user preferences.</para>
    ///  <para>Method of IPreferences.</para>
    ///  </remarks>
    function GetLastTab: string;
    ///  <summary>Sets last tab displayed by Preferences dialogue box when it
    ///  was last closed.</summary>
    ///  <remarks>
    ///  <para>This is meta data about the dialogue box itself, not about user
    ///  preferences.</para>
    ///  <para>Method of IPreferences.</para>
    ///  </remarks>
    procedure SetLastTab(const Value: string);

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
    ///  collection.</summary>
    ///  <param name="ACollectionID">TCollectionID [in] ID of required
    ///  collection.</param>
    ///  <returns>TColor. Required colour.</returns>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetDBHeadingColour(const ACollectionID: TCollectionID): TColor;

    ///  <summary>Sets heading colour used for snippets from a specified
    ///  collection.</summary>
    ///  <param name="ACollectionID">TCollectionID [in] ID of required
    ///  collection.</param>
    ///  <returns>TColor. Required colour.</returns>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetDBHeadingColour(const ACollectionID: TCollectionID;
      const Value: TColor);

    ///  <summary>Gets custom colours available for headings for specified
    ///  collection.</summary>
    ///  <param name="ACollectionID">TCollectionID [in] ID of required
    ///  collection.</param>
    ///  <returns>IStringList. String list containing custom colours.</returns>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetDBHeadingCustomColours(const ACollectionID: TCollectionID):
      IStringList;

    ///  <summary>Sets custom colours available for headings for specified
    ///  collection.</summary>
    ///  <param name="ACollectionID">TCollectionID [in] ID of required
    ///  collection.</param>
    ///  <param name="Value">IStringList [in] String list containing custom
    ///  colours.</param>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetDBHeadingCustomColours(const ACollectionID: TCollectionID;
      Value: IStringList);

    ///  <summary>Gets size of font used in overview pane tree view.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetOverviewFontSize: Integer;

    ///  <summary>Sets size of font used in overview pane tree view.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetOverviewFontSize(const Value: Integer);

    ///  <summary>Gets size of base font used in detail pane.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetDetailFontSize: Integer;

    ///  <summary>Sets size of base font used in detail pane.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetDetailFontSize(const Value: Integer);

    ///  <summary>Gets colour used for background of source code in main
    ///  display.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetSourceCodeBGColour: TColor;

    ///  <summary>Sets colour used for background of source code in main
    ///  display.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetSourceCodeBGColour(const Value: TColor);

    ///  <summary>Gets custom colours available for use as background colour of
    ///  source code in main display as a string list.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetSourceCodeBGCustomColours: IStringList;

    ///  <summary>Sets custom colours available for use as background colour of
    ///  source code in main display from given string list.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetSourceCodeBGCustomColours(Value: IStringList);

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

    ///  <summary>Gets information describing snippet detail page
    ///  customisations.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetPageStructures: TSnippetPageStructures;

    ///  <summary>Updates information describing snippet detail page
    ///  customisations.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetPageStructures(PageStructures: TSnippetPageStructures);

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
      cDisplay = 'Display';
      cPageStructures = 'SnippetPageStructure';
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
  Self.fLastTab := SrcPref.LastTab;
  Self.fSourceDefaultFileType := SrcPref.SourceDefaultFileType;
  Self.fSourceCommentStyle := SrcPref.SourceCommentStyle;
  Self.fTruncateSourceComments := SrcPref.TruncateSourceComments;
  Self.fSourceSyntaxHilited := SrcPref.SourceSyntaxHilited;
  Self.fMeasurementUnits := SrcPref.MeasurementUnits;
  Self.fOverviewStartState := SrcPref.OverviewStartState;
  Self.fShowEmptySections := SrcPref.ShowEmptySections;
  Self.fShowNewSnippetsInNewTabs := SrcPref.ShowNewSnippetsInNewTabs;
  Self.SetDBHeadingColour(TCollectionID.__TMP__MainDBCollectionID, SrcPref.GetDBHeadingColour(TCollectionID.__TMP__MainDBCollectionID));
  Self.SetDBHeadingCustomColours(TCollectionID.__TMP__MainDBCollectionID, SrcPref.GetDBHeadingCustomColours(TCollectionID.__TMP__MainDBCollectionID));
  Self.SetDBHeadingColour(TCollectionID.__TMP__UserDBCollectionID, SrcPref.GetDBHeadingColour(TCollectionID.__TMP__UserDBCollectionID));
  Self.SetDBHeadingCustomColours(TCollectionID.__TMP__UserDBCollectionID, SrcPref.GetDBHeadingCustomColours(TCollectionID.__TMP__UserDBCollectionID));
  Self.fOverviewFontSize := SrcPref.OverviewFontSize;
  Self.fDetailFontSize := SrcPref.DetailFontSize;
  Self.fSourceCodeBGColour := SrcPref.SourceCodeBGColour;
  Self.fSourceCodeBGCustomColours := SrcPref.SourceCodeBGCustomColours;
  Self.fPrinterOptions := SrcPref.PrinterOptions;
  Self.fPrinterPageMargins := SrcPref.PrinterPageMargins;
  Self.SetHiliteAttrs(SrcPref.HiliteAttrs);
  Self.SetNamedHiliteAttrs(SrcPref.NamedHiliteAttrs);
  Self.SetCustomHiliteColours(SrcPref.CustomHiliteColours);
  Self.SetWarnings(SrcPref.Warnings);
  Self.SetPageStructures(SrcPref.PageStructures);
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

function TPreferences.DefaultDetailFontSize: Integer;
begin
  Result := TFontHelper.GetDefaultContentFontSize;
end;

function TPreferences.DefaultOverviewFontSize: Integer;
begin
  Result := TFontHelper.GetDefaultFontSize;
end;

destructor TPreferences.Destroy;
begin
  fPageStructures.Free;
  inherited;
end;

function TPreferences.GetCustomHiliteColours: IStringList;
begin
  Result := fHiliteCustomColours;
end;

function TPreferences.GetDBHeadingColour(
  const ACollectionID: TCollectionID): TColor;
begin
  {TODO -cCollections: WARNING: This implementation of GetDBHeadingColour only
          supports the old main and user collections. It will break when further
          collections are added.
  }
  Result := fDBHeadingColours[__TMP__UseUserDBHeadingColour(ACollectionID)];
end;

function TPreferences.GetDBHeadingCustomColours(
  const ACollectionID: TCollectionID): IStringList;
begin
  {TODO -cCollections: WARNING: This implementation of GetDBHeadingCustomColours
          only supports the old main and user collections. It will break when
          further collections are added.
  }
  Result := fDBHeadingCustomColours[
    __TMP__UseUserDBHeadingColour(ACollectionID)
  ];
end;

function TPreferences.GetDetailFontSize: Integer;
begin
  Result := fDetailFontSize;
end;

function TPreferences.GetHiliteAttrs: IHiliteAttrs;
begin
  Result := fHiliteAttrs;
end;

function TPreferences.GetLastTab: string;
begin
  Result := fLastTab;
end;

function TPreferences.GetMeasurementUnits: TMeasurementUnits;
begin
  Result := fMeasurementUnits;
end;

function TPreferences.GetNamedHiliteAttrs: INamedHiliteAttrs;
begin
  Result := fNamedHiliteAttrs;
end;

function TPreferences.GetOverviewFontSize: Integer;
begin
  Result := fOverviewFontSize
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

function TPreferences.GetSourceCodeBGColour: TColor;
begin
  Result := fSourceCodeBGColour;
end;

function TPreferences.GetSourceCodeBGCustomColours: IStringList;
begin
  Result := fSourceCodeBGCustomColours;
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

procedure TPreferences.SetCustomHiliteColours(const Colours: IStringList);
begin
  fHiliteCustomColours := Colours;
end;

procedure TPreferences.SetDBHeadingColour(const ACollectionID: TCollectionID;
  const Value: TColor);
begin
  {TODO -cCollections: WARNING: This implementation of SetDBHeadingColour only
          supports the old main and user collections. It will break when further
          collections are added.
  }
  fDBHeadingColours[__TMP__UseUserDBHeadingColour(ACollectionID)] := Value;
end;

procedure TPreferences.SetDBHeadingCustomColours(
  const ACollectionID: TCollectionID; Value: IStringList);
begin
  {TODO -cCollections: WARNING: This implementation of SetDBHeadingCustomColours
          only supports the old main and user collections. It will break when
          further collections are added.
  }
  fDBHeadingCustomColours[
    __TMP__UseUserDBHeadingColour(ACollectionID)
  ] := Value;
end;

procedure TPreferences.SetDetailFontSize(const Value: Integer);
begin
  if TFontHelper.IsInCommonFontSizeRange(Value) then
    fDetailFontSize := Value
  else
    fDetailFontSize := DefaultDetailFontSize;
end;

procedure TPreferences.SetHiliteAttrs(const Attrs: IHiliteAttrs);
begin
  (fHiliteAttrs as IAssignable).Assign(Attrs);
end;

procedure TPreferences.SetLastTab(const Value: string);
begin
  fLastTab := Value;
end;

procedure TPreferences.SetMeasurementUnits(const Value: TMeasurementUnits);
begin
  fMeasurementUnits := Value;
end;

procedure TPreferences.SetNamedHiliteAttrs(NamedHiliteAttrs: INamedHiliteAttrs);
begin
  (fNamedHiliteAttrs as IAssignable).Assign(NamedHiliteAttrs);
end;

procedure TPreferences.SetOverviewFontSize(const Value: Integer);
begin
  if TFontHelper.IsInCommonFontSizeRange(Value) then
    fOverviewFontSize := Value
  else
    fOverviewFontSize := DefaultOverviewFontSize;
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

procedure TPreferences.SetSourceCodeBGColour(const Value: TColor);
begin
  fSourceCodeBGColour := Value;
end;

procedure TPreferences.SetSourceCodeBGCustomColours(Value: IStringList);
begin
  fSourceCodeBGCustomColours := Value;
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

function TPreferences.__TMP__UseUserDBHeadingColour(
  const ACollectionID: TCollectionID): Boolean;
begin
  Result := (ACollectionID <> TCollectionID.__TMP__MainDBCollectionID)
    and not ACollectionID.IsNull;
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
  NewPref.LastTab := Self.fLastTab;
  NewPref.SourceDefaultFileType := Self.fSourceDefaultFileType;
  NewPref.SourceCommentStyle := Self.fSourceCommentStyle;
  NewPref.TruncateSourceComments := Self.fTruncateSourceComments;
  NewPref.SourceSyntaxHilited := Self.fSourceSyntaxHilited;
  NewPref.MeasurementUnits := Self.fMeasurementUnits;
  NewPref.OverviewStartState := Self.fOverviewStartState;
  NewPref.ShowEmptySections := Self.fShowEmptySections;
  NewPref.ShowNewSnippetsInNewTabs := Self.fShowNewSnippetsInNewTabs;
  NewPref.SetDBHeadingColour(TCollectionID.__TMP__MainDBCollectionID, Self.GetDBHeadingColour(TCollectionID.__TMP__MainDBCollectionID));
  NewPref.SetDBHeadingCustomColours(TCollectionID.__TMP__MainDBCollectionID, Self.GetDBHeadingCustomColours(TCollectionID.__TMP__MainDBCollectionID));
  NewPref.SetDBHeadingColour(TCollectionID.__TMP__UserDBCollectionID, Self.GetDBHeadingColour(TCollectionID.__TMP__UserDBCollectionID));
  NewPref.SetDBHeadingCustomColours(TCollectionID.__TMP__UserDBCollectionID, Self.GetDBHeadingCustomColours(TCollectionID.__TMP__UserDBCollectionID));
  NewPref.OverviewFontSize := Self.fOverviewFontSize;
  NewPref.DetailFontSize := Self.fDetailFontSize;
  NewPref.SourceCodeBGColour := Self.fSourceCodeBGColour;
  NewPref.SourceCodeBGCustomColours := Self.fSourceCodeBGCustomColours;
  NewPref.PrinterOptions := Self.fPrinterOptions;
  NewPref.PrinterPageMargins := Self.fPrinterPageMargins;
  NewPref.HiliteAttrs := Self.GetHiliteAttrs;
  NewPref.NamedHiliteAttrs := Self.GetNamedHiliteAttrs;
  NewPref.CustomHiliteColours := Self.GetCustomHiliteColours;
  NewPref.Warnings := Self.GetWarnings;
  NewPref.PageStructures := Self.fPageStructures;
end;

constructor TPreferencesPersist.Create;
var
  Storage: ISettingsSection;  // object used to access persistent storage
const
  // Default margin size in millimeters
  cPrintPageMarginSizeMM = 25.0;
begin
  inherited Create;

  // Read meta data section (no sub-section name)
  Storage := Settings.ReadSection(ssPreferences);
  fLastTab := Storage.GetString('LastTab');

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
  SetDBHeadingColour(
    TCollectionID.__TMP__MainDBCollectionID,
    TColor(Storage.GetInteger('MainDBHeadingColour', clMainSnippet))
  );
  SetDBHeadingColour(
    TCollectionID.__TMP__UserDBCollectionID,
    TColor(Storage.GetInteger('UserDBHeadingColour', clUserSnippet))
  );
  fSourceCodeBGColour := TColor(
    Storage.GetInteger('SourceCodeBGColour', clSourceBg)
  );
  fDBHeadingCustomColours[False] := Storage.GetStrings(
    'MainDBHeadingCustomColourCount', 'MainDBHeadingCustomColour%d'
  );
  fDBHeadingCustomColours[True] := Storage.GetStrings(
    'UserDBHeadingCustomColourCount', 'UserDBHeadingCustomColour%d'
  );
  fOverviewFontSize := Storage.GetInteger(
    'OverviewFontSize', DefaultOverviewFontSize
  );
  fDetailFontSize := Storage.GetInteger(
    'DetailFontSize', DefaultDetailFontSize
  );
  fSourceCodeBGCustomColours := Storage.GetStrings(
    'SourceCodeBGCustomColourCount', 'SourceCodeBGCustomColour%d'
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

  // Read page structure section
  Storage := Settings.ReadSection(ssPreferences, cPageStructures);
  TSnippetPageStructuresPersist.Load(Storage, fPageStructures);

end;

destructor TPreferencesPersist.Destroy;
var
  Storage: ISettingsSection;  // object used to access persistent storage
begin
  // Wreite meta section (no sub-section name)
  Storage := Settings.EmptySection(ssPreferences);
  Storage.SetString('LastTab', fLastTab);
  Storage.Save;

  // Write general section
  Storage := Settings.EmptySection(ssPreferences, cGeneral);
  Storage.SetInteger('Units', Ord(fMeasurementUnits));
  Storage.Save;

  // Write display section
  Storage := Settings.EmptySection(ssPreferences, cDisplay);
  Storage.SetInteger('OverviewStartState', Ord(fOverviewStartState));
  Storage.SetBoolean('ShowEmptySections', fShowEmptySections);
  Storage.SetBoolean('ShowNewSnippetsInNewTabs', fShowNewSnippetsInNewTabs);
  Storage.SetInteger(
    'MainDBHeadingColour',
    GetDBHeadingColour(TCollectionID.__TMP__MainDBCollectionID)
  );
  Storage.SetInteger(
    'UserDBHeadingColour',
    GetDBHeadingColour(TCollectionID.__TMP__UserDBCollectionID)
  );
  Storage.SetInteger('OverviewFontSize', fOverviewFontSize);
  Storage.SetInteger('DetailFontSize', fDetailFontSize);
  Storage.SetInteger('SourceCodeBGColour', fSourceCodeBGColour);
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
  Storage.SetStrings(
    'SourceCodeBGCustomColourCount',
    'SourceCodeBGCustomColour%d',
    fSourceCodeBGCustomColours
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

  // Write page structure section
  Storage := Settings.EmptySection(ssPreferences, cPageStructures);
  TSnippetPageStructuresPersist.Save(Storage, fPageStructures);

  inherited;
end;

class function TPreferencesPersist.GetInstance: IPreferences;
begin
  if not Assigned(fInstance) then
    fInstance := TPreferencesPersist.Create;
  Result := fInstance;
end;

end.

