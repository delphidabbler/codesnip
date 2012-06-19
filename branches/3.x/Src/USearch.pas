{
 * USearch.pas
 *
 * Interfaces and classes that define and perform searches across snippets in
 * the CodeSnip database. Also declares interfaces and classes to record search
 * criteria.
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
 * The Original Code is USearch.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit USearch;


interface


uses
  // Delphi
  Classes, Windows {for inlining}, Graphics,
  // Project
  Compilers.UGlobals, UBaseObjects, USnippets;


type

  {
  TSearchLogic:
    Types of search. Controls how searches deal with multiple terms either by
    requiring *all* terms to be found or *any* terms to be found. Can be used
    with text search criteria (where the logic applies to the searched for
    words) and compiler search criteria (where the logic applies to compiler
    versions).
  }
  TSearchLogic = (
    slAnd,          // find snippets containing *all* supplied information
    slOr            // find snippets containing *any* supplied information
  );

  {
  ISearchCriteria:
    Base do-nothing interface for all search criteria. Enables all types of
    search criteria to be passed to any search object.
  }
  ISearchCriteria = interface(IInterface)
    ['{C0F8DD70-ED30-4293-98B7-F1DD07AFAD54}']
  end;

  {
  INulSearchCriteria:
    Search criteria for nul searches. This is a do nothing criteria interface
    whose purpose is simply to identify a nul search.
  }
  INulSearchCriteria = interface(ISearchCriteria)
    ['{B683726C-C4EC-43CD-AC76-4DB40390DC8F}']
  end;

  {
  TTextSearchOption:
    Search options used in text search criteria.
  }
  TTextSearchOption = (
    soMatchCase,    // only find words where case matches
    soWholeWord     // only match whole words
  );

  {
  TTextSearchOptions:
    Set of text search options.
  }
  TTextSearchOptions = set of TTextSearchOption;

  {
  ITextSearchCriteria:
    Search criteria for text searches. Stores details of the words to be
    searched for, the search logic and the search options.
  }
  ITextSearchCriteria = interface(ISearchCriteria)
    ['{9A4EB089-F863-48F9-B874-75CA2D75DF05}']
    function GetWords: TStrings;
      {Read accessor for Words property.
        @return List of words to be searched for.
      }
    function GetLogic: TSearchLogic;
      {Read accessor for Logic property.
        @return Search logic to be used: AND or OR.
      }
    function GetOptions: TTextSearchOptions;
      {Read accessor for Options property.
        @return Set of options used to modify how search is performed.
      }
    property Words: TStrings read GetWords;
      {List words to be searched for}
    property Logic: TSearchLogic read GetLogic;
      {Search logic to be used: AND or OR}
    property Options: TTextSearchOptions read GetOptions;
      {Set of options used to modify how search is performed}
  end;

  {
  TCompilerSearchCompilers:
    Set of compiler versions used in compiler search criteria.
  }
  TCompilerSearchCompilers = set of TCompilerID;

  {
  TCompilerSearchOption:
    Search options applied to compiler search criteria.
  }
  TCompilerSearchOption = (
    soCompileOK,      // snippet compiles
    soCompileNoWarn,  // snippet compiles without warnings
    soCompileWarn,    // snippet compiles with warnings
    soCompileFail,    // snippet fails to compile
    soUntested        // snippet compilation not tested
  );

  {
  ICompilerSearchCriteria:
    Search criteria for compiler searches. Stores details of the compilers to be
    searched for, the search logic and options concerning compilation success or
    failure.
  }
  ICompilerSearchCriteria = interface(ISearchCriteria)
    ['{6DFAD486-C142-4B0F-873A-51075E285C0C}']
    function GetCompilers: TCompilerSearchCompilers;
      {Read accessor for Compilers property.
        @return Set of compilers to be included in search.
      }
    function GetLogic: TSearchLogic;
      {Read accessor for Logic property.
        @return Search logic to be used: AND or OR.
      }
    function GetOption: TCompilerSearchOption;
      {Read accessor for Option property.
        @return Option determining the compilation outcome to be searched for.
      }
    property Compilers: TCompilerSearchCompilers read GetCompilers;
      {Set of compilers to be included in search}
    property Logic: TSearchLogic read GetLogic;
      {Search logic to be used: AND or OR}
    property Option: TCompilerSearchOption read GetOption;
      {Option determining the compilation outcome to be searched for}
  end;

  {
  ISelectionSearchCriteria:
    Search criteria for manual snippet selection searches. Stores list of
    snippets to be selected.
  }
  ISelectionSearchCriteria = interface(ISearchCriteria)
    ['{6FA6AC34-439B-4744-ACBC-1836EE140EB6}']
    function GetSelectedItems: TRoutineList;
      {Read accessor for SelectedItems property.
        @return List of snippets to be selected in search.
      }
    property SelectedItems: TRoutineList read GetSelectedItems;
      {List of snippets to be selected in search}
  end;

  {
  TXRefSearchOption:
    Search options used in XRef search criteria.
  }
  TXRefSearchOption = (
    soRequired,         // include required snippets in search results
    soRequiredRecurse,  // recursively find required snippets
    soSeeAlso,          // include X-refs ("see also") in search results
    soSeeAlsoRecurse,   // recursively find X-refs
    soIncludeRoutine    // include original snippet in search results
  );

  {
  TXRefSearchOptions:
    Set of XRef search options.
  }
  TXRefSearchOptions = set of TXRefSearchOption;

  {
  IXRefSearchCriteria:
    Search criteria for snippet cross-reference searches. Stores details of
    snippet whose x-refs are to be searched along with search options.
  }
  IXRefSearchCriteria = interface(ISearchCriteria)
    ['{92277B2B-AB48-4B3B-8C4F-6DCC71716D79}']
    function GetBaseRoutine: TRoutine;
      {Read accessor for BaseRoutine property.
        @return Reference to initiating snippet.
      }
    function GetOptions: TXRefSearchOptions;
      {Read accessor for Options property.
        @return Set of options controlling XRef search.
      }
    property BaseRoutine: TRoutine read GetBaseRoutine;
      {Initiating snippet for search}
    property Options: TXRefSearchOptions read GetOptions;
      {Options controlling XRef search}
  end;

  {
  ISearchUIInfo:
    Defines methods to be supported by search objects to provide information to
    be used to display search information in the UI.
  }
  ISearchUIInfo = interface(IInterface)
    ['{920CAC6F-3944-42BF-A838-EEB7E76D4BBC}']
    function Glyph: TBitmap;
      {Provides a glyph to be used to indicate kind of search.
        @return Reference to a bitmap storing glyph.
      }
  end;

  {
  ISearch:
    Defines methods to be supported by search objects.
  }
  ISearch = interface(IInterface)
    ['{ADD777B6-28B7-4DF9-B537-F2ECE5CB545C}']
    function GetCriteria: ISearchCriteria;
      {Read accessor for Criteria property.
        @return Criteria to be applied to search.
      }
    function Execute(const InList, FoundList: TRoutineList): Boolean;
      {Executes the search, determining which of a list of snippets match the
      search criteria.
        @param InList [in] List of snippets that the search is applied to.
        @param FoundList [in] List of snippets that match the search criteria.
        @return True if some snippets were found or false if search failed.
      }
    function IsNul: Boolean;
      {Checks if search is a nul search, i.e. it finds all snippets.
        @return True if search is nul search and false otherwise.
      }
    property Criteria: ISearchCriteria read GetCriteria;
      {Criteria to be applied to search}
  end;

  {
  TSearchFactory:
    Static class that creates and clones the different types of search objects.
  }
  TSearchFactory = class(TNoConstructObject)
  public
    class function CreateCompilerSearch(
      const Criteria: ICompilerSearchCriteria): ISearch;
      {Creates a compiler search object.
        @param Criteria [in] Criteria to apply to search.
        @return ISearch interface to compiler search object instance.
      }
    class function CreateTextSearch(
      const Criteria: ITextSearchCriteria): ISearch;
      {Creates a text search object.
        @param Criteria [in] Criteria to apply to sarch.
        @return ISearch interface to text search object instance.
      }
    class function CreateSelectionSearch(
      const Criteria: ISelectionSearchCriteria): ISearch;
      {Creates a selection search object.
        @param Criteria [in] Criteria to apply to search.
        @return ISearch interface to selection search object instance.
      }
    class function CreateXRefSearch(
      const Criteria: IXRefSearchCriteria): ISearch;
      {Creates a cross-reference search object.
        @param Criteria [in] Criteria to apply to search.
        @return ISearch interface to cross-reference search object instance.
      }
    class function CreateNulSearch: ISearch;
      {Creates a nul search object.
        @return ISearch interface to nul search object instance.
      }
    class function CloneSearch(const ASearch: ISearch): ISearch;
      {Creates a search object that is a clone of another search.
        @param ASearch [in] Search object to clone. If nil a Nul search criteria
          object is created.
        @return ISearch interface of cloned object.
      }
  end;

  {
  TSearchCriteriaFactory:
    Static class that creates different types of search criteria objects.
  }
  TSearchCriteriaFactory = class(TNoConstructObject)
  public
    class function CreateCompilerSearchCriteria(
      const Compilers: TCompilerSearchCompilers; const Logic: TSearchLogic;
      const Option: TCompilerSearchOption): ICompilerSearchCriteria;
      {Creates a compiler search criteria object with specified property values.
        @param Compilers [in] Set of compilers to be included in search.
        @param Logic [in] Search logic to be used: AND or OR.
        @param Option [in] Compilation outcome to be searched for.
        @return ICompilerSearchCriteria interface to created object.
      }
    class function CreateTextSearchCriteria(
      const Words: string; const Logic: TSearchLogic;
      const Options: TTextSearchOptions): ITextSearchCriteria;
      {Creates a text search criteria object with specified property values.
        @param Words [in] List words to be searched for.
        @param Logic [in] Search logic to be used: AND or OR.
        @param Options [in] Set of options used to modify how search is
          performed.
        @return ITextSearchCriteria interface to created object.
      }
    class function CreateSelectionSearchCriteria(
      const SelectedItems: TRoutineList): ISelectionSearchCriteria;
      {Creates a selection search criteria object with specified property
      values.
        @param SelectedItems [in] List snippets to be included in search.
        @return ISelectionSearchCriteria interface to created object.
      }
    class function CreateXRefSearchCriteria(const BaseRoutine: TRoutine;
      const Options: TXRefSearchOptions): IXRefSearchCriteria;
      {Creates a cross-reference search criteria object with specified property
      values.
        @param BaseRoutine [in] Snippet whose cross references are to be
          searched.
        @param Options [in] Options controlling XRef search.
        @return IXRefSearchCriteria interface to created object.
      }
 end;


implementation


uses
  // Delphi
  SysUtils, Character,
  // Project
  UActiveText, UConsts, UUtils;


type

  {
  TSearch:
    Abstract base class for non-nul search objects. Each search object must
    indicate whether a given snippet matches a search by overriding the Match
    method.
  }
  TSearch = class(TInterfacedObject)
  strict protected
    function Match(const Routine: TRoutine): Boolean; virtual; abstract;
      {Checks whether a snippet matches the search criteria.
        @param Routine [in] Snippet to be tested.
        @return True if snippet matches criteria, false if not.
      }
  protected
    { ISearch methods }
    function Execute(const InList, FoundList: TRoutineList): Boolean;
      {Executes the search, determining which of a list of snippets match the
      search criteria.
        @param InList [in] List of snippets that the search is applied to.
        @param FoundList [in] List of snippets that match the search criteria.
        @return True if some snippets were found or false if search failed.
      }
    function IsNul: Boolean;
      {Checks if search is a nul search, i.e. it finds all snippets.
        @return False. Assumes only non-nul search classes will be descended
          from this class.
      }
  end;

  {
  TCompilerSearch:
    Class that finds snippets depending on how they compile with specified
    compilers.
  }
  TCompilerSearch = class(TSearch, ISearch)
  strict private
    fCriteria: ICompilerSearchCriteria;
      {Search criteria}
  strict protected
    function Match(const Routine: TRoutine): Boolean; override;
      {Checks whether a snippet matches the search criteria.
        @param Routine [in] Snippet to be tested.
        @return True if snippet matches criteria, false if not.
      }
  protected
    { ISearch methods not defined in base class }
    function GetCriteria: ISearchCriteria;
      {Read accessor for Criteria property.
        @return Criteria to be applied to search.
      }
  public
    constructor Create(const Criteria: ICompilerSearchCriteria);
      {Class constructor. Sets up compiler search.
        @param Critera [in] Criteria for this search.
      }
  end;

  {
  TTextSearch:
    Class that finds snippets that match specified text.
  }
  TTextSearch = class(TSearch, ISearch)
  strict private
    fCriteria: ITextSearchCriteria;
      {Search criteria}
  strict protected
    function Match(const Routine: TRoutine): Boolean; override;
      {Checks whether a snippet matches the search criteria.
        @param Routine [in] Snippet to be tested.
        @return True if snippet matches criteria, false if not.
      }
  protected
    { ISearch methods not defined in base class }
    function GetCriteria: ISearchCriteria;
      {Read accessor for Criteria property.
        @return Criteria to be applied to search.
      }
  public
    constructor Create(const Criteria: ITextSearchCriteria);
      {Class constructor. Sets up text search.
        @param Critera [in] Criteria for this search.
      }
  end;

  {
  TSelectionSearch:
    Class that selects snippets specified in a list.
  }
  TSelectionSearch = class(TSearch, ISearch)
  strict private
    fCriteria: ISelectionSearchCriteria;
      {Search criteria}
  strict protected
    function Match(const Routine: TRoutine): Boolean; override;
      {Checks whether a snippet matches the search criteria.
        @param Routine [in] Snippet to be tested.
        @return True if snippet matches criteria, false if not.
      }
  protected
    { ISearch methods not defined in base class }
    function GetCriteria: ISearchCriteria;
      {Read accessor for Criteria property.
        @return Criteria to be applied to search.
      }
  public
    constructor Create(const Criteria: ISelectionSearchCriteria);
      {Class constructor. Sets up selection search.
        @param Critera [in] Criteria for this search.
      }
  end;

  {
  TXRefSearch:
    Class that searches for snippets cross-referenced by a specified snippet.
  }
  TXRefSearch = class(TSearch, ISearch)
  strict private
    fCriteria: IXRefSearchCriteria;
      {Search criteria}
    fXRefs: TRoutineList;
      {List of all cross-referenced snippets per current criteria}
    function AddToXRefs(const Routine: TRoutine): Boolean;
      {Adds snippet to list of cross-references if not already in list.
        @param Routine [in] Snippet to add to list.
        @return True if snippet added or false if snippet was already in list.
      }
    procedure ReferenceRequired(const Routine: TRoutine);
      {Adds all a snippet's required snippets to cross-reference list. These
      references are only added if appropriate search option is set.
        @param Routine [in] Snippet whose required snippets are to be added to
          x-ref list.
      }
    procedure ReferenceSeeAlso(const Routine: TRoutine);
      {Adds all a snippet's "see also" snippets to cross-reference list. These
      references are only added if appropriate search option is set.
        @param Routine [in] Snippet whose "see also" snippets are to be added to
          x-ref list.
      }
    procedure ReferenceRoutine(const Routine: TRoutine);
      {Adds a snippet to cross-reference list if it is not already present. Also
      recursively adds the snippet's all its cross-referenced snippets if
      appropriate search options are set.
        @param Routine [in] Snippet to add to x-ref list.
      }
  strict protected
    function Match(const Routine: TRoutine): Boolean; override;
      {Checks whether a snippet matches the search criteria.
        @param Routine [in] Snippet to be tested.
        @return True if snippet matches criteria, false if not.
      }
  protected
    { ISearch methods not defined in base class }
    function GetCriteria: ISearchCriteria;
      {Read accessor for Criteria property.
        @return Criteria to be applied to search.
      }
  public
    constructor Create(const Criteria: IXRefSearchCriteria);
      {Class constructor. Sets up cross-reference search.
        @param Critera [in] Criteria for this search.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
  end;

  {
  TNulSearch:
    Class that finds all snippets - a nul search.
  }
  TNulSearch = class(TInterfacedObject, ISearch)
  strict private
    fCriteria: INulSearchCriteria;
      {Search criteria}
  protected
    { ISearch methods }
    function GetCriteria: ISearchCriteria;
      {Read accessor for Criteria property.
        @return Criteria to be applied to search.
      }
    function Execute(const InList, FoundList: TRoutineList): Boolean;
      {Executes the search, determining which of a list of snippets match the
      search criteria.
        @param InList [in] List of snippets that the search is applied to.
        @param FoundList [in] List of snippets that match the search criteria.
        @return True if some snippets were found or false if search failed.
      }
    function IsNul: Boolean;
      {Checks if search is a nul search, i.e. it finds all snippets.
        @return True.
      }
  public
    constructor Create;
      {Class constructor. Sets up nul search.
      }
  end;

  {
  TBaseSearchCriteria:
    Abstract base class for all search criteria objects that implements
    ISearchUIInfo.
  }
  TBaseSearchCriteria = class(TInterfacedObject)
  strict private
    fBitmap: TBitmap;
      {Stores bitmap of glyph associated with search type}
  strict protected
    function GlyphResourceName: string; virtual; abstract;
      {Provides name of required glyph bitmap in resources.
        @return Name of bitmap resource.
      }
  public
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    function Glyph: TBitmap;
      {Provides a glyph to be used to indicate kind of search.
        @return Reference to a bitmap storing glyph.
      }
  end;

  {
  TCompilerSearchCriteria:
    Search criteria for compiler searches. Stores details of the compilers to be
    searched for, the search logic and the options concerning compilation
    success or failure.
  }
  TCompilerSearchCriteria = class(TBaseSearchCriteria,
    ISearchCriteria,
    ICompilerSearchCriteria,
    ISearchUIInfo
    )
  private
    fCompilers: TCompilerSearchCompilers;
      {Compilers to include in search}
    fLogic: TSearchLogic;
      {Search logic}
    fOption: TCompilerSearchOption;
      {Compile result option}
  strict protected
    function GlyphResourceName: string; override;
      {Provides name of required glyph bitmap in resources.
        @return Name of bitmap resource.
      }
  protected
    { ICompilerSearchCriteria methods }
    function GetCompilers: TCompilerSearchCompilers;
      {Read accessor for Compilers property.
        @return Set of compilers to be included in search.
      }
    function GetLogic: TSearchLogic;
      {Read accessor for Logic property.
        @return Search logic to be used: AND or OR.
      }
    function GetOption: TCompilerSearchOption;
      {Read accessor for Option property.
        @return Option determining the compilation outcome to be searched for.
      }
  public
    constructor Create(const Compilers: TCompilerSearchCompilers;
      const Logic: TSearchLogic; const Option: TCompilerSearchOption);
      {Class consructor. Sets up object with specified property values.
        @param Compilers [in] Set of compilers to be included in search.
        @param Logic [in] Search logic to be used: AND or OR.
        @param Option [in] Determines compilation outcome to be searched for.
      }
  end;

  {
  TTextSearchCriteria:
    Search criteria for text searches. Stores details of the words to be
    searched for, the search logic and the options concerning how the text
    search is implemented.
  }
  TTextSearchCriteria = class(TBaseSearchCriteria,
    ISearchCriteria,
    ITextSearchCriteria,
    ISearchUIInfo
    )
  strict private
    fWords: TStrings;
      {List of search words}
    fLogic: TSearchLogic;
      {Search logic}
    fOptions: TTextSearchOptions;
      {Text search options}
  strict protected
    function GlyphResourceName: string; override;
      {Provides name of required glyph bitmap in resources.
        @return Name of bitmap resource.
      }
  protected
    { ITextSearchCriteria methods }
    function GetWords: TStrings;
      {Read accessor for Words property.
        @return List of words to be searched for.
      }
    function GetLogic: TSearchLogic;
      {Read accessor for Logic property.
        @return Search logic to be used: AND or OR.
      }
    function GetOptions: TTextSearchOptions;
      {Read accessor for Options property.
        @return Set of options used to modify how search is performed.
      }
  public
    constructor Create(const Words: string; const Logic: TSearchLogic;
      const Options: TTextSearchOptions);
      {Class constructor. Sets up object with specified property values.
        @param Words [in] Words to be searched for, separated by spaces.
        @param Logic [in] Search logic to be used: AND or OR.
        @param Options [in] Set of options used to modify how search is
          performed.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
  end;

  {
  TSelectionSearchCriteria:
    Search criteria for selection searches. Stores names of items to be
    selected.
  }
  TSelectionSearchCriteria = class(TBaseSearchCriteria,
    ISearchCriteria,
    ISelectionSearchCriteria,
    ISearchUIInfo
  )
  strict private
    fSelectedItems: TRoutineList;
      {List snippets to be selected in search}
  strict protected
    function GlyphResourceName: string; override;
      {Provides name of required glyph bitmap in resources.
        @return Name of bitmap resource.
      }
  protected
    { ISelectionSearchCriteria methods }
    function GetSelectedItems: TRoutineList;
      {Read accessor for SelectedItems property.
        @return List of snippets to be selected in search.
      }
  public
    constructor Create(const SelectedItems: TRoutineList);
      {Class constructor. Sets up object with specified property values.
        @param SelectedItems [in] List of snippets to be selected in search.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
  end;

  {
  TXRefSearchCriteria:
    Search criteria for cross-reference searches. Stores snippet whose x-refs
    are to be searched along with search options.
  }
  TXRefSearchCriteria = class(TBaseSearchCriteria,
    ISearchCriteria,
    IXRefSearchCriteria,
    ISearchUIInfo
  )
  strict private
    fBaseRoutine: TRoutine;
      {Snippet to which search of cross-references applies}
    fOptions: TXRefSearchOptions;
      {Set of poptions controlling XRef search}
  strict protected
    function GlyphResourceName: string; override;
      {Provides name of required glyph bitmap in resources.
        @return Name of bitmap resource.
      }
  protected
    { IXRefSearchCriteria methods }
    function GetBaseRoutine: TRoutine;
      {Read accessor for BaseRoutine property.
        @return Reference to initiating snippet.
      }
    function GetOptions: TXRefSearchOptions;
      {Read accessor for Options property.
        @return Set of options controlling XRef search.
      }
  public
    constructor Create(const BaseRoutine: TRoutine;
      const Options: TXRefSearchOptions);
      {Class constructor. Sets up object with specified property values.
        @param BaseRoutine [in] Snippet whose cross references are to be
          searched.
        @param Options [in] Set of options conrtolling search.
      }
  end;

  {
  TNulSearchCriteria:
    Do nothing object that implements the nul search criteria interface UI info
    interfaces.
  }
  TNulSearchCriteria = class(TBaseSearchCriteria,
    ISearchCriteria,
    INulSearchCriteria,
    ISearchUIInfo
    )
  strict protected
    function GlyphResourceName: string; override;
      {Provides name of required glyph bitmap in resources.
        @return Name of bitmap resource.
      }
  end;


{ TSearch }

function TSearch.Execute(const InList, FoundList: TRoutineList): Boolean;
  {Executes the search, determining which of a list of snippets match the search
  criteria.
    @param InList [in] List of snippets that the search is applied to.
    @param FoundList [in] List of snippets that match the search criteria.
    @return True if some snippets were found or false if search failed.
  }
var
  Idx: Integer;         // loops thru snippets in InList
  Routine: TRoutine;    // reference to a snippet in InList
begin
  Assert(Assigned(InList), ClassName + '.Execute: InList is nil');
  Assert(Assigned(FoundList), ClassName + '.Execute: FoundList is nil');
  Assert(InList <> FoundList, ClassName + '.Execute: InList = FoundList');
  // Ensure found list is empty
  FoundList.Clear;
  // We add all snippets from InList to FoundList if they match search criteria
  for Idx := 0 to Pred(InList.Count) do
  begin
    Routine := InList[Idx];
    if Match(Routine) then
      FoundList.Add(Routine);
  end;
  Result := FoundList.Count > 0;
end;

function TSearch.IsNul: Boolean;
  {Checks if search is a nul search, i.e. it finds all snippets.
    @return False. Assumes only non-nul search classes will be descended from
      this class.
  }
begin
  Result := False;
end;

{ TCompilerSearch }

constructor TCompilerSearch.Create(const Criteria: ICompilerSearchCriteria);
  {Class constructor. Sets up compiler search.
    @param Critera [in] Criteria for this search.
  }
begin
  Assert(Assigned(Criteria), ClassName + '.Create: Criteria is nil');
  inherited Create;
  // Record search criteria
  fCriteria := Criteria;
end;

function TCompilerSearch.GetCriteria: ISearchCriteria;
  {Read accessor for Criteria property.
    @return Criteria to be applied to search.
  }
begin
  Result := fCriteria;
end;

function TCompilerSearch.Match(const Routine: TRoutine): Boolean;
  {Checks whether a snippet matches the search criteria.
    @param Routine [in] Snippet to be tested.
    @return True if snippet matches criteria, false if not.
  }
const
  // Maps compiler search option onto set of compiler results it describes
  cCompatMap: array[TCompilerSearchOption] of set of TCompileResult = (
    [crSuccess, crWarning],   // soCompileOK,
    [crSuccess],              // soCompileNoWarn,
    [crWarning],              // soCompileWarn,
    [crError],                // soCompileFail,
    [crQuery]                 // soUnkown
  );
var
  CompID: TCompilerID;  // loops thru supported compilers
begin
  if fCriteria.Logic = slOr then
  begin
    // Find any compiler: we set result true as soon as any compiler matches and
    // stop searching
    Result := False;
    for CompID := Low(TCompilerID) to High(TCompilerID) do
    begin
      if CompID in fCriteria.Compilers then
      begin
        // this is one of selected compilers: check compile result
        if (Routine.Compatibility[CompID] in cCompatMap[fCriteria.Option]) then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  end
  else {fLogic = slAnd}
  begin
    // Find all compilers: we set result false as soon as any compiler doesn't
    // match and stop searching
    Result := True;
    for CompID := Low(TCompilerID) to High(TCompilerID) do
    begin
      if CompID in fCriteria.Compilers then
      begin
        // this is one of selected compilers: check compile result
        if not
          (Routine.Compatibility[CompID] in cCompatMap[fCriteria.Option]) then
        begin
          Result := False;
          Break;
        end;
      end;
    end;
  end;
end;

{ TTextSearch }

constructor TTextSearch.Create(const Criteria: ITextSearchCriteria);
  {Class constructor. Sets up text search.
    @param Critera [in] Criteria for this search.
  }
begin
  Assert(Assigned(Criteria), ClassName + '.Create: Criteria is nil');
  inherited Create;
  // Record search criteria
  fCriteria := Criteria;
end;

function TTextSearch.GetCriteria: ISearchCriteria;
  {Read accessor for Criteria property.
    @return Criteria to be applied to search.
  }
begin
  Result := fCriteria;
end;

function TTextSearch.Match(const Routine: TRoutine): Boolean;
  {Checks whether a snippet matches the search criteria.
    @param Routine [in] Snippet to be tested.
    @return True if snippet matches criteria, false if not.
  }

  // ---------------------------------------------------------------------------
  function NormaliseSearchText(const RawText: string): string;
    {Converts the text to be searched into a standard format.
      @param RawText [in] Text to be normalised.
      @return Normalised text: quote characters are deleted, all words ending in
        punctuation are included in list in punctuated and non-punctuated state
        and all words are separated by a single space.
    }
  var
    SpacedText: string;         // raw text with all white space as space chars
    SrcIdx: Integer;            // loops through raw text string
    Words: TStringList;         // list of words from raw text
    Word: string;               // a word from Words list
    WordIdx: Integer;           // index into Words list
    ExtraWords: TStringList;    // extra search words derived from Words list
  const
    // Characters that can end words
    cWordEnders = [
      '!', '"', '%', '^', '&', '*', '(', ')', '-', '+', '=',
      '{', '}', '[', ']', ':', ';', '~', '<', '>', ',', '.',
      '?', '/', '|', '\', ''''
    ];
  begin
    // Create word lists
    Words := TStringList.Create;
    ExtraWords := nil;
    try
      ExtraWords := TStringList.Create;
      // Pre-size spaced text string: same size as raw text input
      SetLength(SpacedText, Length(RawText));
      // Convert all white space characters to spaces
      for SrcIdx := 1 to Length(RawText) do
      begin
        if TCharacter.IsWhiteSpace(RawText[SrcIdx]) then
          SpacedText[SrcIdx] := ' '
        else
          SpacedText[SrcIdx] := RawText[SrcIdx]
      end;
      // Convert spaced text to word list
      ExplodeStr(SpacedText, ' ', Words, False);
      // Scan word list adding any additional derived words to extra list
      for WordIdx := 0 to Pred(Words.Count) do
      begin
        Word := Words[WordIdx];
        // strip quotes from word
        if (Word[1] = '''') or (Word[1] = '"') then
          Delete(Word, 1, 1);
        if (Word <> '') and
          ((Word[Length(Word)] = '''') or (Word[Length(Word)] = '"')) then
          Delete(Word, Length(Word), 1);
        Words[WordIdx] := Word;
        // add any word ending in punctuation in non-punctuated state
        while (Word <> '') and CharInSet(Word[Length(Word)], cWordEnders) do
        begin
          // we add any variations to Extra words list
          Delete(Word, Length(Word), 1);
          ExtraWords.Add(Word);
        end;
      end;
      // Build result string, topping and tailing with spaces
      Result := ' ' + JoinStr(Words, ' ', False) + ' '
        + JoinStr(ExtraWords, ' ', False) + ' ';
    finally
      ExtraWords.Free;
      Words.Free;
    end;
  end;

  function NormaliseSearchWord(const Word: string): string;
    {Converts a word being searched for into correct format for searching
    depending on search options.
      @param Word [in] Word to be normalised.
      @return Normalised word: if case being ignored word is lower case and if
        whole words being matched word is topped and tailed by a space.
    }
  begin
    Result := Word;
    if not (soMatchCase in fCriteria.Options) then
      Result := AnsiLowerCase(Result);
    if soWholeWord in fCriteria.Options then
      Result := ' ' + Result + ' ';
  end;

  function ExtraText(const Extra: IActiveText): string;
    {Gets plain text from a snippet's Extra property's active text.
      @param Extra [in] Active text to process.
      @return Plain text extracted from active text.
    }
  var
    Elem: IActiveTextElem;              // each active text elements
    TextElem: IActiveTextTextElem;      // each text active text element
    ActionElem: IActiveTextActionElem;  // each active text action element
  begin
    Result := '';
    for Elem in Extra do
    begin
      if Supports(Elem, IActiveTextTextElem, TextElem) then
        Result := Result + TextElem.Text;
      if Supports(Elem, IActiveTextActionElem, ActionElem)
        and (ActionElem.DisplayStyle = dsBlock)
        and (ActionElem.State = fsClose) then
        Result := Result + EOL;
    end;
  end;
  // ---------------------------------------------------------------------------

var
  SearchText: string; // text we're searching in
  SearchWord: string; // a word we're searching for
  Idx: Integer;       // loops thru words to be found
begin
  // Build search text: text begins and ends with a single space, has no
  // punctuation, and each word is separated by a single space
  SearchText := NormaliseSearchText(
    ' ' + MakeSentence(Routine.Description) +
    ' ' + Routine.SourceCode +
    ' ' + MakeSentence(ExtraText(Routine.Extra)) +
    ' '
  );
  // Set search text to lower case if we're ignoring case: we also convert words
  // to be found to lower case when case ignored
  if not (soMatchCase in fCriteria.Options) then
    SearchText := AnsiLowerCase(SearchText);
  if fCriteria.Logic = slOr then
  begin
    // Find any of words in search text
    // we set result true as soon as any word matches and stop searching
    Result := False;
    for Idx := 0 to Pred(fCriteria.Words.Count) do
    begin
      SearchWord := NormaliseSearchWord(fCriteria.Words[Idx]);
      if AnsiPos(SearchWord, SearchText) > 0 then
      begin
        Result := True;
        Break;
      end;
    end;
  end
  else {fLogic = slAnd}
  begin
    // Find all words in search text
    // we set result false as soon as any word doesn't match and stop searching
    Result := True;
    for Idx := 0 to Pred(fCriteria.Words.Count) do
    begin
      SearchWord := NormaliseSearchWord(fCriteria.Words[Idx]);
      if AnsiPos(SearchWord, SearchText) = 0 then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

{ TSelectionSearch }

constructor TSelectionSearch.Create(const Criteria: ISelectionSearchCriteria);
  {Class constructor. Sets up selection search.
    @param Critera [in] Criteria for this search.
  }
begin
  Assert(Assigned(Criteria), ClassName + '.Create: Criteria is nil');
  inherited Create;
  // Record search criteria
  fCriteria := Criteria;
end;

function TSelectionSearch.GetCriteria: ISearchCriteria;
  {Read accessor for Criteria property.
    @return Criteria to be applied to search.
  }
begin
  Result := fCriteria;
end;

function TSelectionSearch.Match(const Routine: TRoutine): Boolean;
  {Checks whether a snippet matches the search criteria.
    @param Routine [in] Snippet to be tested.
    @return True if snippet matches criteria, false if not.
  }
begin
  Result := fCriteria.SelectedItems.Contains(Routine);
end;

{ TXRefSearch }

function TXRefSearch.AddToXRefs(const Routine: TRoutine): Boolean;
  {Adds snippet to list of cross-references if not already in list.
    @param Routine [in] Snippet to add to list.
    @return True if snippet added or false if snippet was already in list.
  }
begin
  Result := not fXRefs.Contains(Routine);
  if Result then
    fXRefs.Add(Routine);
end;

constructor TXRefSearch.Create(const Criteria: IXRefSearchCriteria);
  {Class constructor. Sets up cross-reference search.
    @param Critera [in] Criteria for this search.
  }
begin
  Assert(Assigned(Criteria), ClassName + '.Create: Criteria is nil');
  inherited Create;
  // Record search criteria
  fCriteria := Criteria;
  // Create and populate list of cross-referenced snippets
  fXRefs := TRoutineList.Create;
  // reference required and "see also" snippets
  // these methods do nothing if appropriate search options not set
  ReferenceRequired(fCriteria.BaseRoutine);
  ReferenceSeeAlso(fCriteria.BaseRoutine);
  // add base snippet if appropriate search option set
  if soIncludeRoutine in fCriteria.Options then
    AddToXRefs(fCriteria.BaseRoutine);
end;

destructor TXRefSearch.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fXRefs);
  inherited;
end;

function TXRefSearch.GetCriteria: ISearchCriteria;
  {Read accessor for Criteria property.
    @return Criteria to be applied to search.
  }
begin
  Result := fCriteria;
end;

function TXRefSearch.Match(const Routine: TRoutine): Boolean;
  {Checks whether a snippet matches the search criteria.
    @param Routine [in] Snippet to be tested.
    @return True if snippet matches criteria, false if not.
  }
begin
  // We have already set up list of x-ref snippets: simply look up snippet in it
  Result := fXRefs.Contains(Routine);
end;

procedure TXRefSearch.ReferenceRequired(const Routine: TRoutine);
  {Adds all a snippet's required snippets to cross-reference list. These
  references are only added if appropriate search option is set.
    @param Routine [in] Snippet whose required snippets are to be added to x-ref
      list.
  }
var
  Idx: Integer; // loops thru all required snippets
begin
  if soRequired in fCriteria.Options then
    for Idx := 0 to Pred(Routine.Depends.Count) do
      ReferenceRoutine(Routine.Depends[Idx]);
end;

procedure TXRefSearch.ReferenceRoutine(const Routine: TRoutine);
  {Adds a snippet to cross-reference list if it is not already present. Also
  recursively adds the snippet's all its cross-referenced snippets if
  appropriate search options are set.
    @param Routine [in] Snippet to add to x-ref list.
  }
begin
  // Add snippet to list if not present. Quit if snippet already referenced.
  if not AddToXRefs(Routine) then
    Exit;
  // Recurse required snippets if specified in options
  if soRequiredRecurse in fCriteria.Options then
    ReferenceRequired(Routine);
  // Recurse "see also" snippets if specified in options
  if soSeeAlsoRecurse in fCriteria.Options then
    ReferenceSeeAlso(Routine);
end;

procedure TXRefSearch.ReferenceSeeAlso(const Routine: TRoutine);
  {Adds all a snippet's "see also" snippets to cross-reference list. These
  references are only added if appropriate search option is set.
    @param Routine [in] Snippet whose "see also" snippets are to be added to
      x-ref list.
  }
var
  Idx: Integer; // loops thru all "see also" snippets
begin
  if soSeeAlso in fCriteria.Options then
    for Idx := 0 to Pred(Routine.XRef.Count) do
      ReferenceRoutine(Routine.XRef[Idx]);
end;

{ TBaseSearchCriteria }

destructor TBaseSearchCriteria.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fBitmap);
  inherited;
end;

function TBaseSearchCriteria.Glyph: TBitmap;
  {Provides a glyph to be used to indicate kind of search.
    @return Reference to a bitmap storing glyph.
  }
begin
  if not Assigned(fBitmap) then
  begin
    // Bitmap not yet created: create it and load from resources
    fBitmap := TBitmap.Create;
    fBitmap.LoadFromResourceName(HInstance, GlyphResourceName);
  end;
  Result := fBitmap;
end;

{ TNulSearch }

constructor TNulSearch.Create;
  {Class constructor. Sets up nul search.
  }
begin
  inherited Create;
  // Create nul search criteria object
  fCriteria := TNulSearchCriteria.Create;
end;

function TNulSearch.Execute(const InList, FoundList: TRoutineList): Boolean;
  {Executes the search, determining which of a list of snippets match the
  search criteria.
    @param InList [in] List of snippets that the search is applied to.
    @param FoundList [in] List of snippets that match the search criteria.
    @return True if some snippets were found or false if search failed.
  }
begin
  Assert(Assigned(InList), ClassName + '.Execute: InList is nil');
  Assert(Assigned(FoundList), ClassName + '.Execute: FoundList is nil');
  Assert(InList <> FoundList, ClassName + '.Execute: InList = FoundList');
  // Nul search finds all items: simply copy source list to found list
  FoundList.Assign(InList);
  // Return true unless there are no snippets in original
  Result := InList.Count > 0;
end;

function TNulSearch.GetCriteria: ISearchCriteria;
  {Read accessor for Criteria property.
    @return Criteria to be applied to search.
  }
begin
  Result := fCriteria;
end;

function TNulSearch.IsNul: Boolean;
  {Checks if search is a nul search, i.e. it finds all snippets.
    @return True.
  }
begin
  Result := True;
end;

{ TCompilerSearchCriteria }

constructor TCompilerSearchCriteria.Create(
  const Compilers: TCompilerSearchCompilers; const Logic: TSearchLogic;
  const Option: TCompilerSearchOption);
  {Class consructor. Sets up object with specified property values.
    @param Compilers [in] Set of compilers to be included in search.
    @param Logic [in] Search logic to be used: AND or OR.
    @param Option [in] Determines compilation outcome to be searched for.
  }
begin
  inherited Create;
  // Store properties
  fCompilers := Compilers;
  fLogic := Logic;
  fOption := Option;
end;

function TCompilerSearchCriteria.GetCompilers: TCompilerSearchCompilers;
  {Read accessor for Compilers property.
    @return Set of compilers to be included in search.
  }
begin
  Result := fCompilers;
end;

function TCompilerSearchCriteria.GetLogic: TSearchLogic;
  {Read accessor for Logic property.
    @return Search logic to be used: AND or OR.
  }
begin
  Result := fLogic;
end;

function TCompilerSearchCriteria.GetOption: TCompilerSearchOption;
  {Read accessor for Option property.
    @return Option determining the compilation outcome to be searched for.
  }
begin
  Result := fOption;
end;

function TCompilerSearchCriteria.GlyphResourceName: string;
  {Provides name of required glyph bitmap in resources.
    @return Name of bitmap resource.
  }
begin
  Result := 'COMPILERSEARCH';
end;

{ TTextSearchCriteria }

constructor TTextSearchCriteria.Create(const Words: string;
  const Logic: TSearchLogic; const Options: TTextSearchOptions);
  {Class constructor. Sets up object with specified property values.
    @param Words [in] Words to be searched for, separated by spaces.
    @param Logic [in] Search logic to be used: AND or OR.
    @param Options [in] Set of options used to modify how search is performed.
  }
begin
  Assert(Words <> '', ClassName + '.Create: Words is empty string');
  inherited Create;
  // Store properties
  fLogic := Logic;
  fOptions := Options;
  // store each search word as entry in string list
  fWords := TStringList.Create;
  ExplodeStr(CompressWhiteSpace(Words), ' ', fWords);
end;

destructor TTextSearchCriteria.Destroy;
  {Class destructor. Tears down object.
  }
begin
  fWords.Free;
  inherited;
end;

function TTextSearchCriteria.GetLogic: TSearchLogic;
  {Read accessor for Logic property.
    @return Search logic to be used: AND or OR.
  }
begin
  Result := fLogic;
end;

function TTextSearchCriteria.GetOptions: TTextSearchOptions;
  {Read accessor for Options property.
    @return Set of options used to modify how search is performed.
  }
begin
  Result := fOptions;
end;

function TTextSearchCriteria.GetWords: TStrings;
  {Read accessor for Words property.
    @return List of words to be searched for.
  }
begin
  Result := fWords;
end;

function TTextSearchCriteria.GlyphResourceName: string;
  {Provides name of required glyph bitmap in resources.
    @return Name of bitmap resource.
  }
begin
  Result := 'TEXTSEARCH';
end;

{ TSelectionSearchCriteria }

constructor TSelectionSearchCriteria.Create(
  const SelectedItems: TRoutineList);
  {Class constructor. Sets up object with specified property values.
    @param SelectedItems [in] List of snippets to be selected in search.
  }
begin
  inherited Create;
  fSelectedItems := TRoutineList.Create;
  fSelectedItems.Assign(SelectedItems);
end;

destructor TSelectionSearchCriteria.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fSelectedItems);
  inherited;
end;

function TSelectionSearchCriteria.GetSelectedItems: TRoutineList;
  {Read accessor for SelectedItems property.
    @return List of snippets to be selected in search.
  }
begin
  Result := fSelectedItems;
end;

function TSelectionSearchCriteria.GlyphResourceName: string;
  {Provides name of required glyph bitmap in resources.
    @return Name of bitmap resource.
  }
begin
  Result := 'SELECTIONSEARCH';
end;

{ TXRefSearchCriteria }

constructor TXRefSearchCriteria.Create(const BaseRoutine: TRoutine;
  const Options: TXRefSearchOptions);
  {Class constructor. Sets up object with specified property values.
    @param BaseRoutine [in] Snippet whose cross references are to be searched.
    @param Options [in] Set of options conrtolling search.
  }
begin
  Assert(Assigned(BaseRoutine), ClassName + '.Create: BaseRoutine is nil');
  inherited Create;
  fBaseRoutine := BaseRoutine;
  fOptions := Options;
end;

function TXRefSearchCriteria.GetBaseRoutine: TRoutine;
  {Read accessor for BaseRoutine property.
    @return Reference to initiating snippet.
  }
begin
  Result := fBaseRoutine;
end;

function TXRefSearchCriteria.GetOptions: TXRefSearchOptions;
  {Read accessor for Options property.
    @return Set of options controlling XRef search.
  }
begin
  Result := fOptions;
end;

function TXRefSearchCriteria.GlyphResourceName: string;
  {Provides name of required glyph bitmap in resources.
    @return Name of bitmap resource.
  }
begin
  Result := 'XREFSEARCH';
end;

{ TNulSearchCriteria }

function TNulSearchCriteria.GlyphResourceName: string;
  {Provides name of required glyph bitmap in resources.
    @return Name of bitmap resource.
  }
begin
  Result := 'NULSEARCH';
end;

{ TSearchFactory }

class function TSearchFactory.CloneSearch(const ASearch: ISearch): ISearch;
  {Creates a search object that is a clone of another search.
    @param ASearch [in] Search object to clone. If nil a Nul search criteria
      object is created.
    @return ISearch interface of cloned object.
  }
var
  TextCriteria: ITextSearchCriteria;            // text search criteria
  CompilerCriteria: ICompilerSearchCriteria;    // compiler search criteria
  SelectionCriteria: ISelectionSearchCriteria;  // selection search criteria
  XRefCriteria: IXRefSearchCriteria;            // cross-ref search criteria
begin
  // Assume error result
  Result := nil;

  if not Assigned(ASearch)
    or Supports(ASearch.Criteria, INulSearchCriteria) then
    // source is nil or Nul search object: create a nul search object
    Result := TSearchFactory.CreateNulSearch

  else if Supports(ASearch.Criteria, ITextSearchCriteria, TextCriteria) then
    // source is text search object
    Result := TSearchFactory.CreateTextSearch(
      TSearchCriteriaFactory.CreateTextSearchCriteria(
        JoinStr(TextCriteria.Words, ' '),
        TextCriteria.Logic,
        TextCriteria.Options
      )
    )

  else if Supports(
    ASearch.Criteria, ICompilerSearchCriteria, CompilerCriteria
  ) then
    // source is compiler search object
    Result := TSearchFactory.CreateCompilerSearch(
      TSearchCriteriaFactory.CreateCompilerSearchCriteria(
        CompilerCriteria.Compilers,
        CompilerCriteria.Logic,
        CompilerCriteria.Option
      )
    )

  else if Supports(
    ASearch.Criteria, ISelectionSearchCriteria, SelectionCriteria
  ) then
    // source is selection search object
    Result := TSearchFactory.CreateSelectionSearch(
      TSearchCriteriaFactory.CreateSelectionSearchCriteria(
        SelectionCriteria.SelectedItems
      )
    )

  else if Supports(
    ASearch.Criteria, IXRefSearchCriteria, XRefCriteria
  ) then
    // source is cross-reference search object
    Result := TSearchFactory.CreateXRefSearch(
      TSearchCriteriaFactory.CreateXRefSearchCriteria(
        XRefCriteria.BaseRoutine,
        XRefCriteria.Options
      )
    );

  Assert(Assigned(Result), ClassName + '.CloneSearch: Unknown ASearch type');
end;

class function TSearchFactory.CreateCompilerSearch(
  const Criteria: ICompilerSearchCriteria): ISearch;
  {Creates a compiler search object.
    @param Criteria [in] Criteria to apply to search.
    @return ISearch interface to compiler search object instance.
  }
begin
  Result := TCompilerSearch.Create(Criteria);
end;

class function TSearchFactory.CreateNulSearch: ISearch;
  {Creates a nul search object.
    @return ISearch interface to nul search object instance.
  }
begin
  Result := TNulSearch.Create;
end;

class function TSearchFactory.CreateSelectionSearch(
  const Criteria: ISelectionSearchCriteria): ISearch;
  {Creates a selection search object.
    @param Criteria [in] Criteria to apply to search.
    @return ISearch interface to selection search object instance.
  }
begin
  Result := TSelectionSearch.Create(Criteria);
end;

class function TSearchFactory.CreateTextSearch(
  const Criteria: ITextSearchCriteria): ISearch;
  {Creates a text search object.
    @param Criteria [in] Criteria to apply to sarch.
    @return ISearch interface to text search object instance.
  }
begin
  Result := TTextSearch.Create(Criteria);
end;

class function TSearchFactory.CreateXRefSearch(
  const Criteria: IXRefSearchCriteria): ISearch;
  {Creates a cross-reference search object.
    @param Criteria [in] Criteria to apply to search.
    @return ISearch interface to cross-reference search object instance.
  }
begin
  Result := TXRefSearch.Create(Criteria);
end;

{ TSearchCriteriaFactory }

class function TSearchCriteriaFactory.CreateCompilerSearchCriteria(
  const Compilers: TCompilerSearchCompilers; const Logic: TSearchLogic;
  const Option: TCompilerSearchOption): ICompilerSearchCriteria;
  {Creates a compiler search criteria object with specified property values.
    @param Compilers [in] Set of compilers to be included in search.
    @param Logic [in] Search logic to be used: AND or OR.
    @param Option [in] Compilation outcome to be searched for.
    @return ICompilerSearchCriteria interface to created object.
  }
begin
  Result := TCompilerSearchCriteria.Create(Compilers, Logic, Option);
end;

class function TSearchCriteriaFactory.CreateSelectionSearchCriteria(
  const SelectedItems: TRoutineList): ISelectionSearchCriteria;
  {Creates a selection search criteria object with specified property values.
    @param SelectedItems [in] List of snippets to be included in search.
    @return ISelectionSearchCriteria interface to created object.
  }
begin
  Result := TSelectionSearchCriteria.Create(SelectedItems);
end;

class function TSearchCriteriaFactory.CreateTextSearchCriteria(
  const Words: string; const Logic: TSearchLogic;
  const Options: TTextSearchOptions): ITextSearchCriteria;
  {Creates a text search criteria object with specified property values.
    @param Words [in] List words to be searched for.
    @param Logic [in] Search logic to be used: AND or OR.
    @param Options [in] Set of options used to modify how search is performed.
    @return ITextSearchCriteria interface to created object.
  }
begin
  Result := TTextSearchCriteria.Create(Words, Logic, Options);
end;

class function TSearchCriteriaFactory.CreateXRefSearchCriteria(
  const BaseRoutine: TRoutine;
  const Options: TXRefSearchOptions): IXRefSearchCriteria;
  {Creates a cross-reference search criteria object with specified property
  values.
    @param BaseRoutine [in] Snippet whose cross references are to be searched.
    @param Options [in] Options controlling XRef search.
    @return IXRefSearchCriteria interface to created object.
  }
begin
  Result := TXRefSearchCriteria.Create(BaseRoutine, Options);
end;

end.

