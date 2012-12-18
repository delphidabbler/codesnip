{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Interfaces and classes that define and perform searches across snippets in
 * the CodeSnip database. Also declares interfaces and classes to record search
 * criteria.
}


unit USearch;

// TODO: Rewrite this unit - it's all become rather messy!!

interface


uses
  // Delphi
  Classes, Windows {for inlining}, Graphics,
  // Project
  Compilers.UGlobals, DB.USnippet, UBaseObjects, USnippetIDs;


type
  ///  <summary>Identifies search logic to be applied when multiple search terms
  ///  terms are specified.</summary>
  TSearchLogic = (
    slAnd,          // find snippets containing *all* search terms
    slOr            // find snippets containing *any* search terms
  );


type
  ///  <summary>Interface defining basic search filter operations.</summary>
  ///  <remarks>Must be supported by all search filters.</remarks>
  ISearchCriteria = interface(IInterface)
    ['{C0F8DD70-ED30-4293-98B7-F1DD07AFAD54}']
    ///  <summary>Checks whether the given snippet matches the filter's search
    ///  criteria, returning True if so or False if not.</summary>
    function Match(const Snippet: TSnippet): Boolean;
    ///  <summary>Indicates whether the object is a null filter or not.
    ///  </summary>
    ///  <remarks>A null filter is a no-op - every snippet is passed through.
    ///  </remarks>
    function IsNull: Boolean;
  end;

type
  ///  <summary>Options that can be used to customise a text search.</summary>
  TTextSearchOption = (
    soMatchCase,    // only match words where case matches
    soWholeWord     // only match whole words
  );

type
  ///  <summary>Set of text search options.</summary>
  TTextSearchOptions = set of TTextSearchOption;

type
  ///  <summary>Interface defining search operations and search criteria for a
  ///  text search filter.</summary>
  ///  <remarks>Must be supported by all text search filters.</remarks>
  ITextSearchCriteria = interface(ISearchCriteria)
    ['{9A4EB089-F863-48F9-B874-75CA2D75DF05}']
    ///  <summary>Read accessor for Words property.</summary>
    ///  <returns>TStringList. List of one or more words to be included in
    ///  search.</returns>
    function GetWords: TStrings;
    ///  <summary>Read accessor for Logic property.</summary>
    ///  <returns>TSearchLogic. Search logic to be used: "and" which requires
    ///  that all words in Words property are found in a snippet or "or" which
    ///  permits snippets to contain any one or more such words.</returns>
    function GetLogic: TSearchLogic;
    ///  <summary>Read accessor for Options property.</summary>
    ///  <returns>TTextSearchOptions. Set of search options to be used to
    ///  customise the filter. Set may be empty.</returns>
    function GetOptions: TTextSearchOptions;
    ///  <summary>List of one or more words to be searched for.</summary>
    property Words: TStrings read GetWords;
    ///  <summary>Search logic to be used: "and" which requires that all words
    ///  in Words property are found in a snippet or "or" which permits snippets
    ///  to contain any one or more such words.</summary>
    property Logic: TSearchLogic read GetLogic;
    ///  <summary>Set of options used to modify the operation of the filter.
    ///  </summary>
    property Options: TTextSearchOptions read GetOptions;
  end;

type
  ///  <summary>Set of ids of compilers to be included in a compiler search.
  ///  </summary>
  TCompilerSearchCompilers = set of TCompilerID;

type
  ///  <summary>Options that can be used to customise a compiler search by
  ///  specifying the expected compilation results.</summary>
  TCompilerSearchOption = (
    soCompileOK,      // snippet compiles
    soCompileNoWarn,  // snippet compiles without warnings
    soCompileWarn,    // snippet compiles with warnings
    soCompileFail,    // snippet fails to compile
    soUntested        // snippet compilation not tested
  );

type
  ///  <summary>Interface defining search operations and search criteria for a
  ///  compiler search filter.</summary>
  ///  <remarks>Must be supported by all compiler search filters.</remarks>
  ICompilerSearchCriteria = interface(ISearchCriteria)
    ['{6DFAD486-C142-4B0F-873A-51075E285C0C}']
    ///  <summary>Read accessor for Compilers property.</summary>
    ///  <returns>TCompilerSearchCompilers. Set of compilers to be included in
    ///  search.</returns>
    function GetCompilers: TCompilerSearchCompilers;
    ///  <summary>Read accessor for Logic property.</summary>
    ///  <returns>TSearchLogic. Search logic to be used: "and" which requires
    ///  that all compilers in Compilers property are found or "or" which
    ///  permits snippets to contain any one or more such compilers.</returns>
    function GetLogic: TSearchLogic;
    ///  <summary>Read accessor for Option property.</summary>
    ///  <returns>TCompilerSearchOption. Option determining the compilation
    ///  outcome to be searched for.</returns>
    function GetOption: TCompilerSearchOption;
    ///  <summary>Set of compilers to be included in the search.</summary>
    property Compilers: TCompilerSearchCompilers read GetCompilers;
    ///  <summary>Search logic to be used: "and" which requires all a matched
    ///  snippet to have the compile result that matches the Option property for
    ///  all compilers specified in the Compilers property, or "or" which
    ///  permits the snippet to have the required result for any of the given
    ///  compilers.</summary>
    property Logic: TSearchLogic read GetLogic;
    ///  <summary>Determines the compilation outcome being searched for.
    ///  </summary>
    property Option: TCompilerSearchOption read GetOption;
  end;

type
  ///  <summary>Interface defining search operations and search criteria for
  ///  filters that select a given list of snippets.</summary>
  ISelectionSearchCriteria = interface(ISearchCriteria)
    ['{6FA6AC34-439B-4744-ACBC-1836EE140EB6}']
    ///  <summary>Read accessor for SelectedItems property.</summary>
    ///  <returns>ISnippetIDList. List of ids of snippets to be selected by
    ///  filter.</returns>
    function GetSelectedItems: ISnippetIDList;
    ///  <summary>List of ids of snippets to be selected by filter.</summary>
    property SelectedItems: ISnippetIDList read GetSelectedItems;
  end;

type
  ///  <summary>Permitted options that can be used to customise an XRef search.
  ///  </summary>
  TXRefSearchOption = (
    soRequired,         // include required snippets in search results
    soRequiredRecurse,  // recursively find required snippets
    soSeeAlso,          // include X-refs ("see also") in search results
    soSeeAlsoRecurse,   // recursively find X-refs
    soIncludeSnippet    // include original snippet in search results
  );

type
  ///  <summary>Set of options that can be used to customise an XRef search.
  ///  </summary>
  TXRefSearchOptions = set of TXRefSearchOption;

type
  ///  <summary>Interface defining search operations and search criteria for a
  ///  cross-reference filter.</summary>
  IXRefSearchCriteria = interface(ISearchCriteria)
    ['{92277B2B-AB48-4B3B-8C4F-6DCC71716D79}']
    ///  <summary>Read accessor for BaseSnippet property.</summary>
    ///  <returns>TSnippet. Snippet for which cross-references are to be found.
    ///  </returns>
    function GetBaseSnippet: TSnippet;
    ///  <summary>Read accessor for Options property.</summary>
    ///  <returns>TXRefSearchOptions. Set of search options used to customise
    ///  the filter.</returns>
    function GetOptions: TXRefSearchOptions;
    ///  <summary>Snippet for which cross-references are to be found.</summary>
    property BaseSnippet: TSnippet read GetBaseSnippet;
    ///  <summary>Set of search options used to customise the filter.</summary>
    property Options: TXRefSearchOptions read GetOptions;
  end;

type
  ///  <summary>Interface defining method to be supported by search filters that
  ///  provide visual feedback in the UI as to the filter type.</summary>
  ISearchUIInfo = interface(IInterface)
    ['{920CAC6F-3944-42BF-A838-EEB7E76D4BBC}']
    ///  <summary>Returns a glyph to be used to indicate the filter type.
    ///  </summary>
    function Glyph: TBitmap;
  end;

type
  ///  <summary>Interface defining methods and property exposed by an object
  ///  that runs a search using a given filter.</summary>
  ISearch = interface(IInterface)
    ['{ADD777B6-28B7-4DF9-B537-F2ECE5CB545C}']
    ///  <summary>Read accessor for Criteria property.</summary>
    ///  <returns>ISearchCriteria. Search filter object.</returns>
    function GetCriteria: ISearchCriteria;
    ///  <summary>Executes the search selecting the snippets that pass the
    ///  filter referenced by the Criteria property.</summary>
    ///  <param name="InList">TSnippetList [in] List of snippets to be searched.
    ///  </param>
    ///  <param name="FoundList">TSnippetList [in] Receives list of snippets
    ///  that pass the search filter.</param>
    ///  <returns>Boolean. True if some snippets were found or False if not.
    ///  </returns>
    function Execute(const InList, FoundList: TSnippetList): Boolean;
    ///  <summary>Reference to the search filter.</summary>
    property Criteria: ISearchCriteria read GetCriteria;
  end;

type
  ///  <summary>Static factory class that creates search objects with given
  ///  search filters.</summary>
  TSearchFactory = class(TNoConstructObject)
  public
    ///  <summary>Creates and returns a search object using given search filter.
    ///  </summary>
    class function CreateSearch(Criteria: ISearchCriteria): ISearch;
    ///  <summary>Creates and returns a null search object that selects all the
    ///  snippets passed to it.</summary>
    class function CreateNulSearch: ISearch;
  end;

type
  ///  <summary>Static factory class that creates various search filter objects.
  ///  </summary>
  TSearchCriteriaFactory = class(TNoConstructObject)
  public
    ///  <summary>Creates and returns a compiler search filter object.</summary>
    ///  <param name="Compilers">TCompilerSearchCompilers [in] Set of compilers
    ///  to be included in search.</param>
    ///  <param name="Logic">TSearchLogic [in] Search logic to be used: AND or
    ///  OR.</param>
    ///  <param name="Option">TCompilerSearchOption [in] Required compilation
    ///  outcome.</param>.
    ///  <returns>ICompilerSearchCriteria. Interface to filter object.</returns>
    class function CreateCompilerSearchCriteria(
      const Compilers: TCompilerSearchCompilers; const Logic: TSearchLogic;
      const Option: TCompilerSearchOption): ICompilerSearchCriteria;
    ///  <summary>Creates and returns a text search filter object.</summary>
    ///  <param name="Words">string [in] List of words to be searched for.
    ///  </param>
    ///  <param name="Logic">TSearchLogic [in] Search logic to be used: AND or
    ///  OR.</param>
    ///  <param name="Options">TTextSearchOptions [in] Set of search
    ///  customisation options.</param>
    ///  <returns>ITextSearchCriteria. Interface to filter object.</returns>
    class function CreateTextSearchCriteria(const Words: string;
      const Logic: TSearchLogic; const Options: TTextSearchOptions):
      ITextSearchCriteria;
    ///  <summary>Creates and returns a search filter that selects from a given
    ///  list of snippets provided by a user.</summary>
    ///  <param name="SelectedItems">TSnippetList [in] List of snippets to be
    ///  included in search.</param>
    ///  <returns>ISelectionSearchCriteria. Interface to filter object.
    ///  </returns>
    class function CreateManualSelectionSearchCriteria(
      const SelectedSnippets: TSnippetList): ISelectionSearchCriteria;
    ///  <summary>Creates and returns a search filter that selects from a given
    ///  list of snippets provided from file.</summary>
    ///  <param name="SelectedItems">TSnippetList [in] List of snippets to be
    ///  included in search.</param>
    ///  <returns>ISelectionSearchCriteria. Interface to filter object.
    ///  </returns>
    class function CreateStoredSelectionSearchCriteria(
      const SelectedSnippets: ISnippetIDList): ISelectionSearchCriteria;
    ///  <summary>Creates and returns a cross-reference search filter object.
    ///  </summary>
    ///  <param name="BaseSnippet">TSnippet [in] Snippet whose cross references
    ///  are to be searched for.</param>
    ///  <param name="Options">TXRefSearchOptions [in] Set of search
    ///  customisation options.</param>
    ///  <returns>IXRefSearchCriteria. Interface to filter object.</returns>
    class function CreateXRefSearchCriteria(const BaseSnippet: TSnippet;
      const Options: TXRefSearchOptions): IXRefSearchCriteria;
 end;


implementation


uses
  // Delphi
  SysUtils, Character,
  // Project
  ActiveText.UMain, IntfCommon, UStrUtils;


type
  ///  <summary>Class that performs searches.</summary>
  TSearch = class sealed(TInterfacedObject, ISearch)
  strict private
    var
      ///  <summary>Filter to apply to search.</summary>
      fCriteria: ISearchCriteria;
  public
    ///  <summary>Constructs search object to use given search filter.</summary>
    constructor Create(const Criteria: ISearchCriteria);
    ///  <summary>Executes the search.</summary>
    ///  <param name="InList">TSnippetList [in] List of snippets to be searched.
    ///  </param>
    ///  <param name="FoundList">TSnippetList [in] Receives list of snippets
    ///  that pass the search filter.</param>
    ///  <returns>Boolean. True if some snippets were found or False if not.
    ///  </returns>
    ///  <remarks>Method of ISearch.</remarks>
    function Execute(const InList, FoundList: TSnippetList): Boolean;
    ///  <summary>Returns search filter that is used for search.</summary>
    ///  <remarks>Method of ISearch</remarks>
    function GetCriteria: ISearchCriteria;
  end;

type
  ///  <summary>Abstract base class for all search filters.</summary>
  TBaseSearchCriteria = class abstract(TInterfacedObject)
  strict private
    var
      ///  <summary>Stores bitmap of glyph associated with search filter type.
      ///  </summary>
      fBitmap: TBitmap;
  strict protected
    ///  <summary>Returns resource id of glyph bitmap.</summary>
    function GlyphResourceName: string; virtual; abstract;
  public
    ///  <summary>Destroys filter object.</summary>
    destructor Destroy; override;
    ///  <summary>Returns a glyph to be used to indicate the filter type.
    ///  </summary>
    ///  <remarks>Method of ISearchUIInfo.</remarks>
    function Glyph: TBitmap;
  end;

type
  ///  <summary>Class that implements a compiler search filter.</summary>
  TCompilerSearchCriteria = class(TBaseSearchCriteria,
    ISearchCriteria,
    ICompilerSearchCriteria,
    ISearchUIInfo
    )
  strict private
    var
      ///  <summary>Compilers to include in search.</summary>
      fCompilers: TCompilerSearchCompilers;
      ///  <summary>Search logic.</summary>
      fLogic: TSearchLogic;
      ///  <summary>Required compilation result.</summary>
      fOption: TCompilerSearchOption;
  strict protected
    ///  <summary>Returns resource id of glyph bitmap.</summary>
    function GlyphResourceName: string; override;
  public
    ///  <summary>Constructs filter object with given criteria.</summary>
    ///  <param name="Compilers">TCompilerSearchCompilers [in] Set of compilers
    ///  to be included in search.</param>
    ///  <param name="Logic">TSearchLogic [in] Search logic to be used: AND or
    ///  OR.</param>
    ///  <param name="Option">TCompilerSearchOption [in] Compiler result to be
    ///  searched for.</param>
    constructor Create(const Compilers: TCompilerSearchCompilers;
      const Logic: TSearchLogic; const Option: TCompilerSearchOption);
    ///  <summary>Checks whether the given snippet matches the search criteria,
    ///  returning True if so or False if not.</summary>
    ///  <remarks>Method of ISearchCriteria and ICompilerSearchCriteria.
    ///  </remarks>
    function Match(const Snippet: TSnippet): Boolean;
    ///  <summary>Indicates whether the object is a null filter. Returns False.
    ///  </summary>
    ///  <remarks>Method of ISearchCriteria and ICompilerSearchCriteria.
    ///  </remarks>
    function IsNull: Boolean;
    ///  <summary>Returns sets of compilers to be included in search.</summary>
    ///  <remarks>Method of ICompilerSearchCriteria.</remarks>
    function GetCompilers: TCompilerSearchCompilers;
    ///  <summary>Returns search logic to be used.</summary>
    ///  <remarks>Method of ICompilerSearchCriteria.</remarks>
    function GetLogic: TSearchLogic;
    ///  <summary>Returns compilation result to be searched for.</summary>
    ///  <remarks>Method of ICompilerSearchCriteria.</remarks>
    function GetOption: TCompilerSearchOption;
  end;

type
  ///  <summary>Class that implements a text search filter.</summary>
  TTextSearchCriteria = class(TBaseSearchCriteria,
    ISearchCriteria,
    ITextSearchCriteria,
    ISearchUIInfo
    )
  strict private
    var
      ///  <summary>List of search words.</summary>
      fWords: TStrings;
      ///  <summary>Search logic.</summary>
      fLogic: TSearchLogic;
      ///  <summary>Set of options used to modify the operation of the filter.
      ///  </summary>
      fOptions: TTextSearchOptions;
  strict protected
    ///  <summary>Returns resource id of glyph bitmap.</summary>
    function GlyphResourceName: string; override;
  public
    ///  <summary>Constructs filter obejct with given criteria.</summary>
    ///  <param name="Words">string [in] List of search words.</param>
    ///  <param name="Logic">TSearchLogic [in] Search logic to be used: AND or
    ///  OR.</param>
    ///  <param name="Options">TTextSearchOptions [in] Set of options used to
    ///  modify the operation of the filter.</param>
    constructor Create(const Words: string; const Logic: TSearchLogic;
      const Options: TTextSearchOptions);
    ///  <summary>Destroys filter object.</summary>
    destructor Destroy; override;
    ///  <summary>Checks whether the given snippet matches the search criteria,
    ///  returning True if so or False if not.</summary>
    ///  <remarks>Method of ISearchCriteria and ITextSearchCriteria.
    ///  </remarks>
    function Match(const Snippet: TSnippet): Boolean;
    ///  <summary>Indicates whether the object is a null filter. Returns False.
    ///  </summary>
    ///  <remarks>Method of ISearchCriteria and ITextSearchCriteria.
    ///  </remarks>
    function IsNull: Boolean;
    ///  <summary>Returns list of words to be searched for.</summary>
    ///  <remarks>Method of ITextSearchCriteria.</remarks>
    function GetWords: TStrings;
    ///  <summary>Returns search logic to be used.</summary>
    ///  <remarks>Method of ITextSearchCriteria.</remarks>
    function GetLogic: TSearchLogic;
    ///  <summary>Returns set of options used to modify operation of the filter.
    ///  </summary>
    ///  <remarks>Method of ITextSearchCriteria.</remarks>
    function GetOptions: TTextSearchOptions;
  end;

type
  ///  <summary>Base class for search filters that select snippets from a given
  ///  list of snippets.</summary>
  TBaseSelectionSearchCriteria = class abstract(TBaseSearchCriteria)
  strict private
    var
      ///  <summary>List of ids of snippets to be selected in search.</summary>
      fSelectedItems: ISnippetIDList;
  strict protected
    ///  <summary>Returns resource id of glyph bitmap.</summary>
    function GlyphResourceName: string; override; abstract;
  public
    ///  <summary>Constructs filter with given list of snippets.</summary>
    ///  <param name="SelectedItems">ISnippetIDList [in] List of snippets to be
    ///  selected in search.</param>
    constructor Create(const SelectedItems: ISnippetIDList);
    ///  <summary>Checks whether the given snippet matches the search criteria,
    ///  returning True if so or False if not.</summary>
    ///  <remarks>Method of ISearchCriteria and ISelectionSearchCriteria.
    ///  </remarks>
    function Match(const Snippet: TSnippet): Boolean;
    ///  <summary>Indicates whether the object is a null filter. Returns False.
    ///  </summary>
    ///  <remarks>Method of ISearchCriteria and ISelectionSearchCriteria.
    ///  </remarks>
    function IsNull: Boolean;
    ///  <summary>Returns list of snippets to be selected in search.</summary>
    ///  <remarks>Method of ISelectionSearchCriteria.</remarks>
    function GetSelectedItems: ISnippetIDList;
  end;

type
  ///  <summary>Class that implements a selection search filter.</summary>
  ///  <remarks>This class is for use with searches where the user specifies the
  ///  snippets to be selected. It simply provides the correct glyph for this
  ///  search type.</remarks>
  TSelectionSearchCriteria = class(TBaseSelectionSearchCriteria,
    ISearchCriteria,
    ISelectionSearchCriteria,
    ISearchUIInfo
  )
  strict protected
    ///  <summary>Returns resource id of glyph bitmap.</summary>
    function GlyphResourceName: string; override;
  end;

type
  ///  <summary>Class that implements a selection search filter.</summary>
  ///  <remarks>This class is for use with searches where the snippets to be
  ///  selected are read from a file. It simply provides the correct glyph for
  ///  this search type.</remarks>
  TStoredSelectionSearchCriteria = class(TBaseSelectionSearchCriteria,
    ISearchCriteria,
    ISelectionSearchCriteria,
    ISearchUIInfo
  )
  strict protected
    ///  <summary>Returns resource id of glyph bitmap.</summary>
    function GlyphResourceName: string; override;
  end;

type
  ///  <summary>Class that implements a cross-reference search filter.</summary>
  TXRefSearchCriteria = class(TBaseSearchCriteria,
    ISearchCriteria,
    IXRefSearchCriteria,
    ISearchUIInfo
  )
  strict private
    var
      ///  <summary>Snippet to which the cross-reference filter applies.
      ///  </summary>
      fBaseSnippet: TSnippet;
      ///  <summary>Set of options used to modify the operation of the filter.
      ///  </summary>
      fOptions: TXRefSearchOptions;
      ///  <summary>Stores list of cross-referenced snippets.</summary>
      fXRefs: TSnippetList;
    ///  <summary>Adds given snippet to list of x-refs if snippet is not already
    ///  in the list. Returns True if snippet added, False if not.</summary>
    function AddToXRefs(const Snippet: TSnippet): Boolean;
    ///  <summary>Adds all a snippet's required snippets to the x-ref list.
    ///  </summary>
    ///  <remarks>References are only added if appropriate search option is set.
    ///  </remarks>
    procedure ReferenceRequired(const Snippet: TSnippet);
    ///  <summary>Adds all a snippet's required snippets to x-ref list.
    ///  </summary>
    ///  <remarks>These references are only added if appropriate search option
    ///  is set.</remarks>
    procedure ReferenceSeeAlso(const Snippet: TSnippet);
    ///  <summary>Adds a snippet to x-ref list if it is not already present.
    ///  Also recursively adds the snippet's all its cross-referenced snippets
    ///  if appropriate search options are set.</summary>
    procedure ReferenceSnippet(const Snippet: TSnippet);
    ///  <summary>Initialises x-ref list with all required snippets.</summary>
    ///  <remarks>Must only be called once when x-ref list is empty.</remarks>
    procedure Initialise;
  strict protected
    ///  <summary>Returns resource id of glyph bitmap.</summary>
    function GlyphResourceName: string; override;
  public
    ///  <summary>Constructs filter object with given criteria.</summary>
    ///  <param name="BaseSnippet">TSnippet [in] Snippet whose cross references
    ///  are to be searched.</param>
    ///  <param name="Options">TXRefSearchOptions [in] Set of options used to
    ///  modify the operation of the filter.</param>
    constructor Create(const BaseSnippet: TSnippet;
      const Options: TXRefSearchOptions);
    ///  <summary>Destroys filter object.</summary>
    destructor Destroy; override;
    ///  <summary>Checks whether the given snippet matches the search criteria,
    ///  returning True if so or False if not.</summary>
    ///  <remarks>Method of ISearchCriteria and IXRefSearchCriteria.
    ///  </remarks>
    function Match(const Snippet: TSnippet): Boolean;
    ///  <summary>Indicates whether the object is a null filter. Returns False.
    ///  </summary>
    ///  <remarks>Method of ISearchCriteria and IXRefSearchCriteria.
    ///  </remarks>
    function IsNull: Boolean;
    ///  <summary>Returns snippet whose cross references are to be searched.
    ///  </summary>
    ///  <remarks>Method of IXRefSearchCriteria.</remarks>
    function GetBaseSnippet: TSnippet;
    ///  <summary>Returns set of options used to modify operation of the filter.
    ///  </summary>
    ///  <remarks>Method of IXRefSearchCriteria.</remarks>
    function GetOptions: TXRefSearchOptions;
  end;

type
  ///  <summary>Class that implements a null, do-nothing, search filter.
  ///  </summary>
  TNulSearchCriteria = class(TBaseSearchCriteria,
    ISearchCriteria,
    ISearchUIInfo
    )
  strict protected
    ///  <summary>Returns resource id of glyph bitmap.</summary>
    function GlyphResourceName: string; override;
  public
    ///  <summary>Checks whether the given snippet matches the search criteria.
    ///  Always returns True.</summary>
    ///  <remarks>Method of ISearchCriteria.</remarks>
    function Match(const Snippet: TSnippet): Boolean;
    ///  <summary>Indicates whether the object is a null filter. Returns True.
    ///  </summary>
    ///  <remarks>Method of ISearchCriteria.</remarks>
    function IsNull: Boolean;
  end;

{ TSearch }

constructor TSearch.Create(const Criteria: ISearchCriteria);
begin
  Assert(Assigned(Criteria), ClassName + '.Create: Criteria is nil');
  inherited Create;
  fCriteria := Criteria;
end;

function TSearch.Execute(const InList, FoundList: TSnippetList): Boolean;
var
  Snippet: TSnippet;    // each snippet in InList
begin
  Assert(Assigned(InList), ClassName + '.Execute: InList is nil');
  Assert(Assigned(FoundList), ClassName + '.Execute: FoundList is nil');
  Assert(InList <> FoundList, ClassName + '.Execute: InList = FoundList');

  FoundList.Clear;
  for Snippet in InList do
    if GetCriteria.Match(Snippet) then
      FoundList.Add(Snippet);
  Result := FoundList.Count > 0;
end;

function TSearch.GetCriteria: ISearchCriteria;
begin
  Result := fCriteria;
end;

{ TBaseSearchCriteria }

destructor TBaseSearchCriteria.Destroy;
begin
  FreeAndNil(fBitmap);
  inherited;
end;

function TBaseSearchCriteria.Glyph: TBitmap;
begin
  if not Assigned(fBitmap) then
  begin
    // Bitmap not yet created: create it and load from resources
    fBitmap := TBitmap.Create;
    fBitmap.LoadFromResourceName(HInstance, GlyphResourceName);
  end;
  Result := fBitmap;
end;

{ TCompilerSearchCriteria }

constructor TCompilerSearchCriteria.Create(
  const Compilers: TCompilerSearchCompilers; const Logic: TSearchLogic;
  const Option: TCompilerSearchOption);
begin
  inherited Create;
  // Store properties
  fCompilers := Compilers;
  fLogic := Logic;
  fOption := Option;
end;

function TCompilerSearchCriteria.GetCompilers: TCompilerSearchCompilers;
begin
  Result := fCompilers;
end;

function TCompilerSearchCriteria.GetLogic: TSearchLogic;
begin
  Result := fLogic;
end;

function TCompilerSearchCriteria.GetOption: TCompilerSearchOption;
begin
  Result := fOption;
end;

function TCompilerSearchCriteria.GlyphResourceName: string;
begin
  Result := 'COMPILERSEARCH';
end;

function TCompilerSearchCriteria.IsNull: Boolean;
begin
  Result := False;
end;

function TCompilerSearchCriteria.Match(const Snippet: TSnippet): Boolean;
const
  // Maps compiler search option onto set of compiler results it describes
  cCompatMap: array[TCompilerSearchOption] of set of TCompileResult = (
    [crSuccess, crWarning],   // soCompileOK,
    [crSuccess],              // soCompileNoWarn,
    [crWarning],              // soCompileWarn,
    [crError],                // soCompileFail,
    [crQuery]                 // soUnkown
  );

  // Checks if a snippet's compiler result for given compiler ID matches
  // expected results.
  function CompatibilityMatches(const CompID: TCompilerID): Boolean;
  begin
    Result := Snippet.Compatibility[CompID] in cCompatMap[fOption];
  end;

var
  CompID: TCompilerID;  // loops thru supported compilers
begin
  if fLogic = slOr then
  begin
    // Find any compiler: we return true as soon as any compiler compatibility
    // matches
    Result := False;
    for CompID in fCompilers do
    begin
      if CompatibilityMatches(CompID) then
        Exit(True);
    end;
  end
  else {fLogic = slAnd}
  begin
    // Find all compilers: we return false as soon as any compiler compatibility
    // doesn't match
    Result := True;
    for CompID in fCompilers do
    begin
      if not CompatibilityMatches(CompID) then
        Exit(False);
    end;
  end;
end;

{ TTextSearchCriteria }

constructor TTextSearchCriteria.Create(const Words: string;
  const Logic: TSearchLogic; const Options: TTextSearchOptions);
begin
  Assert(Words <> '', ClassName + '.Create: Words is empty string');
  inherited Create;
  // Store properties
  fLogic := Logic;
  fOptions := Options;
  // store each search word as entry in string list
  fWords := TStringList.Create;
  StrExplode(StrCompressWhiteSpace(Words), ' ', fWords);
end;

destructor TTextSearchCriteria.Destroy;
begin
  fWords.Free;
  inherited;
end;

function TTextSearchCriteria.GetLogic: TSearchLogic;
begin
  Result := fLogic;
end;

function TTextSearchCriteria.GetOptions: TTextSearchOptions;
begin
  Result := fOptions;
end;

function TTextSearchCriteria.GetWords: TStrings;
begin
  Result := fWords;
end;

function TTextSearchCriteria.GlyphResourceName: string;
begin
  Result := 'TEXTSEARCH';
end;

function TTextSearchCriteria.IsNull: Boolean;
begin
  Result := False;
end;

function TTextSearchCriteria.Match(const Snippet: TSnippet): Boolean;

  // Converts the text to be searched into a standard format.
  function NormaliseSearchText(const RawText: string): string;

    // Replace each white space char in string S with single space character.
    function WhiteSpaceToSpaces(const S: string): string;
    var
      Idx: Integer; // loops through chars of S
    begin
      // Pre-size spaced text string: same size as raw text input
      SetLength(Result, Length(S));
      // Convert all white space characters to spaces
      for Idx := 1 to Length(S) do
      begin
        if TCharacter.IsWhiteSpace(S[Idx]) then
          Result[Idx] := ' '
        else
          Result[Idx] := S[Idx]
      end;
    end;

    // Strip single and double quotes from a string S.
    procedure StripQuotes(var S: string);
    begin
      if S = '' then
        Exit;
      if (S[1] = '''') or (S[1] = '"') then
        Delete(S, 1, 1);
      if (S <> '') and
        ((S[Length(S)] = '''') or (S[Length(S)] = '"')) then
        Delete(S, Length(S), 1);
    end;

  var
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
    ExtraWords := nil;
    Words := TStringList.Create;
    try
      ExtraWords := TStringList.Create;
      // Convert text to word list and process each word
      StrExplode(WhiteSpaceToSpaces(RawText), ' ', Words, False);
      for WordIdx := 0 to Pred(Words.Count) do
      begin
        Word := Words[WordIdx];
        StripQuotes(Word);
        Words[WordIdx] := Word;
        // Add any word ending in punctuation in non-punctuated state
        // (note that any word ending in x punctuation character will be added
        // several times, once with each of x, x-1, x-2 ... 0 punctuation
        // characters.
        // We need to do this in case user searches for a word followed by one
        // or more punctuation characters.
        while (Word <> '') and CharInSet(Word[Length(Word)], cWordEnders) do
        begin
          // we add any variations to Extra words list
          Delete(Word, Length(Word), 1);
          ExtraWords.Add(Word);
        end;
      end;
      Result := ' ' + StrJoin(Words, ' ', False) + ' '
        + StrJoin(ExtraWords, ' ', False) + ' ';
      if not (soMatchCase in fOptions) then
        Result := StrToLower(Result);
    finally
      ExtraWords.Free;
      Words.Free;
    end;
  end;

  // Converts a word being searched for into correct format for searching
  // depending on search options.
  function NormaliseSearchWord(const Word: string): string;
  begin
    Result := Word;
    if not (soMatchCase in fOptions) then
      Result := StrToLower(Result);
    if soWholeWord in fOptions then
      Result := ' ' + Result + ' ';
  end;

var
  SearchText: string; // text we're searching in
  SearchWord: string; // a word we're searching for
begin
  // Build search text
  SearchText := NormaliseSearchText(
    ' ' + StrMakeSentence(Snippet.Description.ToString) +
    ' ' + Snippet.SourceCode +
    ' ' + StrMakeSentence(Snippet.Extra.ToString) +
    ' '
  );
  if fLogic = slOr then
  begin
    // Find any of words in search text: return True as soon as any word matches
    Result := False;
    for SearchWord in fWords do
      if StrContainsStr(NormaliseSearchWord(SearchWord), SearchText) then
        Exit(True);
  end
  else {fLogic = slAnd}
  begin
    // Find all words in search text: return False as soon as any word doesn't
    // match
    Result := True;
    for SearchWord in fWords do
      if not StrContainsStr(NormaliseSearchWord(SearchWord), SearchText) then
        Exit(False);
  end;
end;

{ TBaseSelectionSearchCriteria }

constructor TBaseSelectionSearchCriteria.Create(
  const SelectedItems: ISnippetIDList);
begin
  inherited Create;
  fSelectedItems := TSnippetIDList.Create;
  (fSelectedItems as IAssignable).Assign(SelectedItems);
end;

function TBaseSelectionSearchCriteria.GetSelectedItems: ISnippetIDList;
begin
  Result := fSelectedItems;
end;

function TBaseSelectionSearchCriteria.IsNull: Boolean;
begin
  Result := False;
end;

function TBaseSelectionSearchCriteria.Match(const Snippet: TSnippet): Boolean;
begin
  Result := fSelectedItems.Contains(Snippet.ID);
end;

{ TSelectionSearchCriteria }

function TSelectionSearchCriteria.GlyphResourceName: string;
begin
  Result := 'SELECTIONSEARCH';
end;

{ TStoredSelectionSearchCriteria }

function TStoredSelectionSearchCriteria.GlyphResourceName: string;
begin
  Result := 'STOREDSELECTIONSEARCH';
end;

{ TXRefSearchCriteria }

function TXRefSearchCriteria.AddToXRefs(const Snippet: TSnippet): Boolean;
begin
  Result := not fXRefs.Contains(Snippet);
  if Result then
    fXRefs.Add(Snippet);
end;

constructor TXRefSearchCriteria.Create(const BaseSnippet: TSnippet;
  const Options: TXRefSearchOptions);
begin
  Assert(Assigned(BaseSnippet), ClassName + '.Create: BaseSnippet is nil');
  inherited Create;
  fBaseSnippet := BaseSnippet;
  fOptions := Options;
end;

destructor TXRefSearchCriteria.Destroy;
begin
  fXRefs.Free;
  inherited;
end;

function TXRefSearchCriteria.GetBaseSnippet: TSnippet;
begin
  Result := fBaseSnippet;
end;

function TXRefSearchCriteria.GetOptions: TXRefSearchOptions;
begin
  Result := fOptions;
end;

function TXRefSearchCriteria.GlyphResourceName: string;
begin
  Result := 'XREFSEARCH';
end;

procedure TXRefSearchCriteria.Initialise;
begin
  Assert(Assigned(fXRefs), ClassName + '.Initialise: fXRefs is nil');
  Assert(fXRefs.Count = 0, ClassName + '.Initialise: fXRefs not empty');
  ReferenceRequired(fBaseSnippet);
  ReferenceSeeAlso(fBaseSnippet);
  if soIncludeSnippet in fOptions then
    AddToXRefs(fBaseSnippet);
end;

function TXRefSearchCriteria.IsNull: Boolean;
begin
  Result := False;
end;

function TXRefSearchCriteria.Match(const Snippet: TSnippet): Boolean;
begin
  // Check if cross references are still to be calcaluted and do it if so
  // We do this here to avoid the overhead if just using object to store / read
  // persistent settings.
  if not Assigned(fXRefs) then
  begin
    fXRefs := TSnippetList.Create;
    Initialise;
  end;
  Result := fXRefs.Contains(Snippet);
end;

procedure TXRefSearchCriteria.ReferenceRequired(const Snippet: TSnippet);
var
  Idx: Integer; // loops thru all required snippets
begin
  if soRequired in fOptions then
    for Idx := 0 to Pred(Snippet.Depends.Count) do
      ReferenceSnippet(Snippet.Depends[Idx]);
end;

procedure TXRefSearchCriteria.ReferenceSeeAlso(const Snippet: TSnippet);
var
  Idx: Integer; // loops thru all "see also" snippets
begin
  if soSeeAlso in fOptions then
    for Idx := 0 to Pred(Snippet.XRef.Count) do
      ReferenceSnippet(Snippet.XRef[Idx]);
end;

procedure TXRefSearchCriteria.ReferenceSnippet(const Snippet: TSnippet);
begin
  // Add snippet to list if not present. Quit if snippet already referenced.
  if not AddToXRefs(Snippet) then
    Exit;
  // Recurse required snippets if specified in options
  if soRequiredRecurse in fOptions then
    ReferenceRequired(Snippet);
  // Recurse "see also" snippets if specified in options
  if soSeeAlsoRecurse in fOptions then
    ReferenceSeeAlso(Snippet);
end;

{ TNulSearchCriteria }

function TNulSearchCriteria.GlyphResourceName: string;
begin
  Result := 'NULSEARCH';
end;

function TNulSearchCriteria.IsNull: Boolean;
begin
  Result := True;
end;

function TNulSearchCriteria.Match(const Snippet: TSnippet): Boolean;
begin
  Result := True;
end;

{ TSearchFactory }

class function TSearchFactory.CreateNulSearch: ISearch;
begin
  Result := CreateSearch(TNulSearchCriteria.Create);
end;

class function TSearchFactory.CreateSearch(Criteria: ISearchCriteria): ISearch;
begin
  Result := TSearch.Create(Criteria);
end;

{ TSearchCriteriaFactory }

class function TSearchCriteriaFactory.CreateCompilerSearchCriteria(
  const Compilers: TCompilerSearchCompilers; const Logic: TSearchLogic;
  const Option: TCompilerSearchOption): ICompilerSearchCriteria;
begin
  Result := TCompilerSearchCriteria.Create(Compilers, Logic, Option);
end;

class function TSearchCriteriaFactory.CreateManualSelectionSearchCriteria(
  const SelectedSnippets: TSnippetList): ISelectionSearchCriteria;
var
  SnippetIDs: ISnippetIDList; // snippet id list
  Snippet: TSnippet;          // each snippet in SelectedSnippets
begin
  SnippetIDs := TSnippetIDList.Create;
  for Snippet in SelectedSnippets do
    SnippetIDs.Add(Snippet.ID);
  Result := TSelectionSearchCriteria.Create(SnippetIDs);
end;

class function TSearchCriteriaFactory.CreateStoredSelectionSearchCriteria(
  const SelectedSnippets: ISnippetIDList): ISelectionSearchCriteria;
begin
  Result := TStoredSelectionSearchCriteria.Create(SelectedSnippets);
end;

class function TSearchCriteriaFactory.CreateTextSearchCriteria(
  const Words: string; const Logic: TSearchLogic;
  const Options: TTextSearchOptions): ITextSearchCriteria;
begin
  Result := TTextSearchCriteria.Create(Words, Logic, Options);
end;

class function TSearchCriteriaFactory.CreateXRefSearchCriteria(
  const BaseSnippet: TSnippet;
  const Options: TXRefSearchOptions): IXRefSearchCriteria;
begin
  Result := TXRefSearchCriteria.Create(BaseSnippet, Options);
end;

end.

