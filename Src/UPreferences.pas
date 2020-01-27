{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2014, Peter Johnson (www.delphidabbler.com).
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
  CS.SourceCode.Hiliter.Themes,
  CS.SourceCode.Pascal.SourceGen,
  UIStringList,
  UMeasurement,
  USnippetPageStructure,
  USourceFileInfo,
  UWarnings;


type
  ///  <summary>Possible values for startup state of overview treeview.
  ///  </summary>
  TOverviewStartState = (
    ossExpanded,  // start treeview fully expanded
    ossCollapsed  // start treeview fully collapsed
  );

type
  ///  <summary>Defines possible kinds of syntax highlighter theme that can be
  ///  recorded as current themes in preferences.</summary>
  TCurrentHiliteThemeKind = (htkUI, htkExport, htkPrint);

type
  ///  <summary>Type of array that maps each kind of current syntax highlighter
  ///  theme to its current theme ID.</summary>
  TCurrentHiliteThemes = array[TCurrentHiliteThemeKind] of TSyntaxHiliteThemeID;

type
  ///  <summary>Interface supported by objects that maintain user preferences.
  ///  </summary>
  IPreferences = interface(IInterface)
    ['{381B9A92-B528-47E1-AC04-90E1FFFDADA7}']

    ///  <summary>Gets style of commenting used to describe snippets in
    ///  generated code.</summary>
    function GetSourceCommentStyle: TPascalCommentStyle;
    ///  <summary>Sets style of commenting to be used describe snippets in
    ///  generated code.</summary>
    procedure SetSourceCommentStyle(const Value: TPascalCommentStyle);
    ///  <summary>Commenting style used to describe snippets in generated source
    ///  code.</summary>
    property SourceCommentStyle: TPascalCommentStyle
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
    function GetSourceDefaultFileType: TSourceOutputFileType;
    ///  <summary>Sets default file extension / type to be used when writing
    ///  code snippets to file.</summary>
    procedure SetSourceDefaultFileType(const Value: TSourceOutputFileType);
    ///  <summary>Default file extension / type used when writing code snippets
    ///  to file.</summary>
    property SourceDefaultFileType: TSourceOutputFileType
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

    ///  <summary>Gets flag that indicates whether new snippets are displayed in
    ///  new tabs in details pane.</summary>
    function GetShowNewSnippetsInNewTabs: Boolean;
    ///  <summary>Sets flag that indicates whether new snippets are displayed in
    ///  new tabs in details pane.</summary>
    procedure SetShowNewSnippetsInNewTabs(const Value: Boolean);
    ///  <summary>Indicates whether new snippets are displayed in new tabs in
    ///  details pane.</summary>
    property ShowNewSnippetsInNewTabs: Boolean
      read GetShowNewSnippetsInNewTabs write SetShowNewSnippetsInNewTabs;

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

    { TODO -cSettings: Merge all custom colours into one entry for colour dialogue box. }
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

    ///  <summary>Gets current highlighter theme of given kind.</summary>
    function GetCurrentHiliteThemeId(Kind: TCurrentHiliteThemeKind):
      TSyntaxHiliteThemeID;
    ///  <summary>Gets current highlighter theme of given kind to given theme
    ///  ID.</summary>
    procedure SetCurrentHiliteThemeId(Kind: TCurrentHiliteThemeKind;
      const ThemeId: TSyntaxHiliteThemeID);
    ///  <summary>IDs of currently selected syntax highlighters of all supported
    ///  kinds.</summary>
    property CurrentHiliteThemeIds[Kind: TCurrentHiliteThemeKind]:
      TSyntaxHiliteThemeID
      read GetCurrentHiliteThemeId write SetCurrentHiliteThemeId;

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
  IntfCommon,
  UExceptions,
  UColours,
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
      fSourceDefaultFileType: TSourceOutputFileType;
      ///  <summary>Commenting style used to describe snippets in generated
      ///  source code.</summary>
      fSourceCommentStyle: TPascalCommentStyle;
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
      ///  <summary>Records colour used for background of source code in main
      ///  display.</summary>
      fSourceCodeBGColour: TColor;
      ///  <summary>Records custom colours available for use as background
      ///  colour of source code in main display.</summary>
      fSourceCodeBGCustomColours: IStringList;
      ///  <summary>Map of current theme kinds to IDs of required highlighters.
      ///  </summary>
      fCurrentHiliteThemeIds: TCurrentHiliteThemes;
      ///  <summary>Custom colours available for syntax highlighters.</summary>
      fHiliteCustomColours: IStringList;
      ///  <summary>Reference to object containing information about warnings to
      ///  be enabled or disabled by code generator.</summary>
      fWarnings: IWarnings;
      ///  <summary>Information describing snippet detail page customisations.
      ///  </summary>
      fPageStructures: TSnippetPageStructures;
  public
    ///  <summary>Constructs a new object instance.</summary>
    constructor Create;

    ///  <summary>Destroys object instance.</summary>
    destructor Destroy; override;

    ///  <summary>Gets style of commenting used to describe snippets in
    ///  generated code.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetSourceCommentStyle: TPascalCommentStyle;

    ///  <summary>Sets style of commenting to be used describe snippets in
    ///  generated code.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetSourceCommentStyle(const Value: TPascalCommentStyle);

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
    function GetSourceDefaultFileType: TSourceOutputFileType;

    ///  <summary>Sets default file extension / type to be used when writing
    ///  code snippets to file.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetSourceDefaultFileType(const Value: TSourceOutputFileType);

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

    ///  <summary>Gets flag that indicates whether new snippets are displayed in
    ///  new tabs in details pane.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    function GetShowNewSnippetsInNewTabs: Boolean;

    ///  <summary>Sets flag that indicates whether new snippets are displayed in
    ///  new tabs in details pane.</summary>
    ///  <remarks>Method of IPreferences.</remarks>
    procedure SetShowNewSnippetsInNewTabs(const Value: Boolean);

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

    ///  <summary>Gets ID of current highlighter theme of given kind.</summary>
    function GetCurrentHiliteThemeId(Kind: TCurrentHiliteThemeKind):
      TSyntaxHiliteThemeID;

    ///  <summary>Sets current highlighter theme ID of given kind to given
    ///  value.</summary>
    procedure SetCurrentHiliteThemeId(Kind: TCurrentHiliteThemeKind;
      const ThemeId: TSyntaxHiliteThemeID);

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
      cHiliter = 'Hiliter';
      cCodeGenerator = 'CodeGen';
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
  HiliteThemeKind: TCurrentHiliteThemeKind;
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
  Self.fSourceCodeBGColour := SrcPref.SourceCodeBGColour;
  Self.fSourceCodeBGCustomColours := SrcPref.SourceCodeBGCustomColours;
  for HiliteThemeKind := Low(TCurrentHiliteThemeKind) to
    High(TCurrentHiliteThemeKind) do
    Self.fCurrentHiliteThemeIds[HiliteThemeKind] :=
      SrcPref.CurrentHiliteThemeIds[HiliteThemeKind];
  Self.SetCustomHiliteColours(SrcPref.CustomHiliteColours);
  Self.SetWarnings(SrcPref.Warnings);
  Self.SetPageStructures(SrcPref.PageStructures);
end;

constructor TPreferences.Create;
begin
  inherited Create;
  fHiliteCustomColours := TIStringList.Create;
  fWarnings := TWarnings.Create;
  fPageStructures := TSnippetPageStructures.Create;
  TDefaultPageStructures.SetDefaults(fPageStructures);
end;

destructor TPreferences.Destroy;
begin
  fPageStructures.Free;
  inherited;
end;

function TPreferences.GetCurrentHiliteThemeId(Kind: TCurrentHiliteThemeKind):
  TSyntaxHiliteThemeID;
begin
  Result := fCurrentHiliteThemeIds[Kind];
  if Result.IsNull then
    Result := TSyntaxHiliteThemeID.CreateDefault;
end;

function TPreferences.GetCustomHiliteColours: IStringList;
begin
  Result := fHiliteCustomColours;
end;

function TPreferences.GetMeasurementUnits: TMeasurementUnits;
begin
  Result := fMeasurementUnits;
end;

function TPreferences.GetOverviewStartState: TOverviewStartState;
begin
  Result := fOverviewStartState;
end;

function TPreferences.GetPageStructures: TSnippetPageStructures;
begin
  Result := fPageStructures;
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

function TPreferences.GetSourceCommentStyle: TPascalCommentStyle;
begin
  Result := fSourceCommentStyle;
end;

function TPreferences.GetSourceDefaultFileType: TSourceOutputFileType;
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

procedure TPreferences.SetCurrentHiliteThemeId(Kind: TCurrentHiliteThemeKind;
  const ThemeId: TSyntaxHiliteThemeID);
begin
  fCurrentHiliteThemeIds[Kind] := ThemeId;
end;

procedure TPreferences.SetCustomHiliteColours(const Colours: IStringList);
begin
  fHiliteCustomColours := Colours;
end;

procedure TPreferences.SetMeasurementUnits(const Value: TMeasurementUnits);
begin
  fMeasurementUnits := Value;
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

procedure TPreferences.SetSourceCommentStyle(const Value: TPascalCommentStyle);
begin
  fSourceCommentStyle := Value;
end;

procedure TPreferences.SetSourceDefaultFileType(
  const Value: TSourceOutputFileType);
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

const
  CurrentHiliterThemeKeyNames: array[TCurrentHiliteThemeKind] of string = (
    'CurrentUITheme', 'CurrentExportTheme', 'CurrentPrintTheme'
  );


function TPreferencesPersist.Clone: IInterface;
var
  NewPref: IPreferences;  // reference to new object's IPreferences interface
  HiliteThemeKind: TCurrentHiliteThemeKind;
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
  NewPref.SourceCodeBGColour := Self.fSourceCodeBGColour;
  NewPref.SourceCodeBGCustomColours := Self.fSourceCodeBGCustomColours;
  for HiliteThemeKind := Low(TCurrentHiliteThemeKind) to
    High(TCurrentHiliteThemeKind) do
    NewPref.CurrentHiliteThemeIds[HiliteThemeKind] :=
      Self.fCurrentHiliteThemeIds[HiliteThemeKind];
  NewPref.CustomHiliteColours := Self.GetCustomHiliteColours;
  NewPref.Warnings := Self.GetWarnings;
  NewPref.PageStructures := Self.fPageStructures;
end;

constructor TPreferencesPersist.Create;
var
  Storage: ISettingsSection;  // object used to access persistent storage
  HiliteThemeKind: TCurrentHiliteThemeKind;
const
  // Default margin size in millimeters
  cPrintPageMarginSizeMM = 25.0;
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
  fSourceCodeBGColour := TColor(
    Storage.GetInteger('SourceCodeBGColour', clSourceBg)
  );
  fSourceCodeBGCustomColours := Storage.GetStrings(
    'SourceCodeBGCustomColourCount', 'SourceCodeBGCustomColour%d'
  );

  // Read source code section
  Storage := Settings.ReadSection(ssPreferences, cSourceCode);
  fSourceDefaultFileType := TSourceOutputFileType(
    Storage.GetInteger('FileType', Ord(sfPascal))
  );
  fSourceCommentStyle := TPascalCommentStyle(
    Storage.GetInteger('CommentStyle', Ord(csAfter))
  );
  fTruncateSourceComments := Storage.GetBoolean('TruncateComments', False);
  fSourceSyntaxHilited := Storage.GetBoolean('UseSyntaxHiliting', False);

  // Read syntax highlighter section
  Storage := Settings.ReadSection(ssPreferences, cHiliter);
  // current theme IDs
  for HiliteThemeKind := Low(TCurrentHiliteThemeKind) to
    High(TCurrentHiliteThemeKind) do
  begin
    fCurrentHiliteThemeIds[HiliteThemeKind] :=
      TSyntaxHiliteThemeID.Create(
        Storage.GetString(
          CurrentHiliterThemeKeyNames[HiliteThemeKind],
          TSyntaxHiliteThemeID.CreateDefault.ToString
        )
      );
  end;
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
  HiliteThemeKind: TCurrentHiliteThemeKind;
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
  Storage.SetInteger('SourceCodeBGColour', fSourceCodeBGColour);
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

  // Write syntax highlighter section
  Storage := Settings.EmptySection(ssPreferences, cHiliter);
  // current theme IDs
  for HiliteThemeKind := Low(TCurrentHiliteThemeKind) to
    High(TCurrentHiliteThemeKind) do
  begin
    Storage.SetString(
      CurrentHiliterThemeKeyNames[HiliteThemeKind],
      GetCurrentHiliteThemeId(HiliteThemeKind).ToString
    );
  end;
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

