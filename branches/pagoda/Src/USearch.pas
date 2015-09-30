{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Interfaces and classes that define and perform searches across snippets in
 * the CodeSnip database.
}


unit USearch;


interface


uses
  // Delphi
  Classes,
  Graphics,
  // Project
  CS.Database.Types,
  Compilers.UGlobals,
  UBaseObjects;


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
  ISearchFilter = interface(IInterface)
    ['{C0F8DD70-ED30-4293-98B7-F1DD07AFAD54}']
    ///  <summary>Performs any required initialisation of the filter before a
    ///  search is commenced.</summary>
    procedure Initialise;
    ///  <summary>Performs any required tidying up of filter after a search
    ///  finishes.</summary>
    procedure Finalise;
    ///  <summary>Checks whether the snippet with the given ID matches the
    ///  filter's search criteria, returning True if so or False if not.
    ///  </summary>
    function Match(const SnippetID: TSnippetID): Boolean;
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
  ITextSearchFilter = interface(ISearchFilter)
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
  ///  <summary>Interface defining search operations and search criteria for a
  ///  tag search filter.</summary>
  ///  <remarks>Must be supported by all tag search filters.</remarks>
  ITagSearchFilter = interface(ISearchFilter)
    ['{20698658-F545-4875-9703-9408EDDEE049}']
    ///  <summary>Read accessor for Logic property.</summary>
    ///  <returns>TSearchLogic. Search logic to be used: "and" which requires
    ///  that all tags in Tags property are found in a snippet or "or" which
    ///  permits snippets to contain any one or more such tags.</returns>
    function GetLogic: TSearchLogic;
    ///  <summary>Read accessor for Tags property.</summary>
    ///  <returns>ITagSet. Set of tags to be included in search.</returns>
    function GetTags: ITagSet;
    ///  <summary>Search logic to be used: "and" which requires that all tags
    ///  in Tags property are found in a snippet or "or" which permits snippets
    ///  to contain any one or more such tags.</summary>
    property Logic: TSearchLogic read GetLogic;
    ///  <summary>Set of tags to be included in search.</summary>
    property Tags: ITagSet read GetTags;
  end;

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
  ICompilerSearchFilter = interface(ISearchFilter)
    ['{6DFAD486-C142-4B0F-873A-51075E285C0C}']
    ///  <summary>Read accessor for Compilers property.</summary>
    ///  <returns>TCompilerIDs. Set of compilers to be included in
    ///  search.</returns>
    function GetCompilers: TCompilerIDs;
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
    property Compilers: TCompilerIDs read GetCompilers;
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
  ISelectionSearchFilter = interface(ISearchFilter)
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
    soRequiredReverse,  // included snippets requiring original snippet
    soSeeAlso,          // include X-refs ("see also") in search results
    soSeeAlsoRecurse,   // recursively find X-refs
    soSeeAlsoReverse,   // include snippets x-referencing original snippet
    soIncludeSnippet    // include original snippet in search results
  );

type
  ///  <summary>Set of options that can be used to customise an XRef search.
  ///  </summary>
  TXRefSearchOptions = set of TXRefSearchOption;

type
  ///  <summary>Interface defining search operations and search criteria for a
  ///  cross-reference filter.</summary>
  IXRefSearchFilter = interface(ISearchFilter)
    ['{92277B2B-AB48-4B3B-8C4F-6DCC71716D79}']
    ///  <summary>Read accessor for BaseSnippetID property.</summary>
    ///  <returns>TSnippetID. ID of snippet for which cross-references are to be
    ///  found.</returns>
    function GetBaseSnippetID: TSnippetID;
    ///  <summary>Read accessor for Options property.</summary>
    ///  <returns>TXRefSearchOptions. Set of search options used to customise
    ///  the filter.</returns>
    function GetOptions: TXRefSearchOptions;
    ///  <summary>ID of snippet for which cross-references are to be found.
    ///  </summary>
    property BaseSnippetID: TSnippetID read GetBaseSnippetID;
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
    ///  <summary>Read accessor for Filter property.</summary>
    ///  <returns>ISearchFilter. Search filter object.</returns>
    function GetFilter: ISearchFilter;
    ///  <summary>Executes the search selecting the snippets that pass the
    ///  filter referenced by the Filter property.</summary>
    ///  <param name="InList">ISnippetIDList [in] IDs of snippets to be
    ///  searched.</param>
    ///  <param name="FoundList">ISnippetIDList [out] Receives IDs of snippets
    ///  that pass the search filter.</param>
    ///  <returns>Boolean. True if some snippets were found or False if not.
    ///  </returns>
    function Execute(InList: ISnippetIDList; out FoundList: ISnippetIDList):
      Boolean;
    ///  <summary>Reference to the search filter.</summary>
    property Filter: ISearchFilter read GetFilter;
  end;

type
  ///  <summary>Static factory class that creates search objects with given
  ///  search filters.</summary>
  TSearchFactory = class(TNoConstructObject)
  public
    ///  <summary>Creates and returns a search object using given search filter.
    ///  </summary>
    class function CreateSearch(Filter: ISearchFilter): ISearch;
    ///  <summary>Creates and returns a null search object that selects all the
    ///  snippets passed to it.</summary>
    class function CreateNullSearch: ISearch;
  end;

type
  ///  <summary>Static factory class that creates various search filter objects.
  ///  </summary>
  TSearchFilterFactory = class(TNoConstructObject)
  public
    ///  <summary>Creates and returns a compiler search filter object.</summary>
    ///  <param name="Compilers">TCompilerIDs [in] Set of compilers
    ///  to be included in search.</param>
    ///  <param name="Logic">TSearchLogic [in] Search logic to be used: AND or
    ///  OR.</param>
    ///  <param name="Option">TCompilerSearchOption [in] Required compilation
    ///  outcome.</param>.
    ///  <returns>ICompilerSearchFilter. Interface to filter object.</returns>
    class function CreateCompilerSearchFilter(
      const Compilers: TCompilerIDs; const Logic: TSearchLogic;
      const Option: TCompilerSearchOption): ICompilerSearchFilter;
    ///  <summary>Creates and returns a text search filter object.</summary>
    ///  <param name="Words">string [in] List of words to be searched for.
    ///  </param>
    ///  <param name="Logic">TSearchLogic [in] Search logic to be used: AND or
    ///  OR.</param>
    ///  <param name="Options">TTextSearchOptions [in] Set of search
    ///  customisation options.</param>
    ///  <returns>ITextSearchFilter. Interface to filter object.</returns>
    class function CreateTextSearchFilter(const Words: string;
      const Logic: TSearchLogic; const Options: TTextSearchOptions):
      ITextSearchFilter;
    ///  <summary>Creates and returns a tag search filter object.</summary>
    ///  <param name="Tags">ITagSet [in] Set of search tags.</param>
    ///  <param name="Logic">TSearchLogic [in] Search logic to be used: AND or
    ///  OR.</param>
    ///  <returns>ITagSearchFilter. Interface to filter object.</returns>
    class function CreateTagSearchFilter(Tags: ITagSet;
      const Logic: TSearchLogic): ITagSearchFilter;
    ///  <summary>Creates and returns a search filter that selects from a given
    ///  list of snippets provided by a user.</summary>
    ///  <param name="SelectedSnippets">ISnippetIDList [in] IDs of snippets to
    ///  be included in search.</param>
    ///  <returns>ISelectionSearchFilter. Interface to filter object.
    ///  </returns>
    class function CreateManualSelectionSearchFilter(
      SelectedSnippets: ISnippetIDList): ISelectionSearchFilter;
    ///  <summary>Creates and returns a search filter that selects from a given
    ///  list of snippets provided from file.</summary>
    ///  <param name="SelectedSnippets">ISnippetIDList [in] IDs of snippets to
    ///  be included in search.</param>
    ///  <returns>ISelectionSearchFilter. Interface to filter object.
    ///  </returns>
    class function CreateStoredSelectionSearchFilter(
      SelectedSnippets: ISnippetIDList): ISelectionSearchFilter;
    ///  <summary>Creates and returns a cross-reference search filter object.
    ///  </summary>
    ///  <param name="BaseSnippetID">TSnippetID [in] ID of snippet whose cross
    ///  references are to be searched for.</param>
    ///  <param name="Options">TXRefSearchOptions [in] Set of search
    ///  customisation options.</param>
    ///  <returns>IXRefSearchFilter. Interface to filter object.</returns>
    class function CreateXRefSearchFilter(const BaseSnippetID: TSnippetID;
      const Options: TXRefSearchOptions): IXRefSearchFilter;
 end;


implementation


uses
  // Delphi
  SysUtils,
  Character,
  // Project
  CS.ActiveText,
  CS.ActiveText.Renderers.PlainText,
  CS.Database.Snippets,
  DB.UMain,
  IntfCommon,
  UConsts,
  UExceptions,
  UStrUtils;


type
  ///  <summary>Class that performs searches.</summary>
  TSearch = class sealed(TInterfacedObject, ISearch)
  strict private
    var
      ///  <summary>Filter to apply to search.</summary>
      fFilter: ISearchFilter;
  public
    ///  <summary>Constructs search object to use given search filter.</summary>
    constructor Create(const Filter: ISearchFilter);
    ///  <summary>Executes the search selecting the snippets that pass the
    ///  filter referenced by the Filter property.</summary>
    ///  <param name="InList">ISnippetIDList [in] IDs of snippets to be
    ///  searched.</param>
    ///  <param name="FoundList">ISnippetIDList [out] Receives IDs of snippets
    ///  that pass the search filter.</param>
    ///  <returns>Boolean. True if some snippets were found or False if not.
    ///  </returns>
    ///  <remarks>Method of ISearch.</remarks>
    function Execute(InList: ISnippetIDList; out FoundList: ISnippetIDList):
      Boolean;
    ///  <summary>Returns search filter that is used for search.</summary>
    ///  <remarks>Method of ISearch</remarks>
    function GetFilter: ISearchFilter;
  end;

type
  ///  <summary>Abstract base class for all search filters.</summary>
  TBaseSearchFilter = class abstract(TInterfacedObject)
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
    ///  <summary>Called to initialise filter before a search is commenced.
    ///  </summary>
    ///  <remarks>
    ///  <para>Does nothing in this base implementation. Descendants needing
    ///  the functionality must override.</para>
    ///  <para>Method of ISearchFilter.</para>
    ///  </remarks>
    procedure Initialise; virtual;
    ///  <summary>Called to tidy up of filter after a search finishes.</summary>
    ///  <remarks>
    ///  <para>Does nothing in this base implementation. Descendants needing
    ///  the functionality must override.</para>
    ///  <para>Method of ISearchFilter.</para>
    ///  </remarks>
    procedure Finalise; virtual;
  end;

type
  ///  <summary>Class that implements a compiler search filter.</summary>
  TCompilerSearchFilter = class(TBaseSearchFilter,
    ISearchFilter,
    ICompilerSearchFilter,
    ISearchUIInfo
    )
  strict private
    var
      ///  <summary>Compilers to include in search.</summary>
      fCompilers: TCompilerIDs;
      ///  <summary>Search logic.</summary>
      fLogic: TSearchLogic;
      ///  <summary>Required compilation result.</summary>
      fOption: TCompilerSearchOption;
  strict protected
    ///  <summary>Returns resource id of glyph bitmap.</summary>
    function GlyphResourceName: string; override;
  public
    ///  <summary>Constructs filter object with given criteria.</summary>
    ///  <param name="Compilers">TCompilerIDs [in] Set of compilers
    ///  to be included in search.</param>
    ///  <param name="Logic">TSearchLogic [in] Search logic to be used: AND or
    ///  OR.</param>
    ///  <param name="Option">TCompilerSearchOption [in] Compiler result to be
    ///  searched for.</param>
    constructor Create(const Compilers: TCompilerIDs;
      const Logic: TSearchLogic; const Option: TCompilerSearchOption);
    ///  <summary>Checks whether the snippet with the given ID matches the
    ///  filter's search criteria, returning True if so or False if not.
    ///  </summary>
    ///  <remarks>Method of ISearchFilter.</remarks>
    function Match(const SnippetID: TSnippetID): Boolean;
    ///  <summary>Indicates whether the object is a null filter. Returns False.
    ///  </summary>
    ///  <remarks>Method of ISearchFilter.</remarks>
    function IsNull: Boolean;
    ///  <summary>Returns sets of compilers to be included in search.</summary>
    ///  <remarks>Method of ICompilerSearchFilter.</remarks>
    function GetCompilers: TCompilerIDs;
    ///  <summary>Returns search logic to be used.</summary>
    ///  <remarks>Method of ICompilerSearchFilter.</remarks>
    function GetLogic: TSearchLogic;
    ///  <summary>Returns compilation result to be searched for.</summary>
    ///  <remarks>Method of ICompilerSearchFilter.</remarks>
    function GetOption: TCompilerSearchOption;
  end;

type
  ///  <summary>Class that implements a text search filter.</summary>
  TTextSearchFilter = class(TBaseSearchFilter,
    ISearchFilter,
    ITextSearchFilter,
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
    ///  <summary>Checks whether the snippet with the given ID matches the
    ///  filter's search criteria, returning True if so or False if not.
    ///  </summary>
    ///  <remarks>Method of ISearchFilter.</remarks>
    function Match(const SnippetID: TSnippetID): Boolean;
    ///  <summary>Indicates whether the object is a null filter. Returns False.
    ///  </summary>
    ///  <remarks>Method of ISearchFilter.</remarks>
    function IsNull: Boolean;
    ///  <summary>Returns list of words to be searched for.</summary>
    ///  <remarks>Method of ITextSearchFilter.</remarks>
    function GetWords: TStrings;
    ///  <summary>Returns search logic to be used.</summary>
    ///  <remarks>Method of ITextSearchFilter.</remarks>
    function GetLogic: TSearchLogic;
    ///  <summary>Returns set of options used to modify operation of the filter.
    ///  </summary>
    ///  <remarks>Method of ITextSearchFilter.</remarks>
    function GetOptions: TTextSearchOptions;
  end;

type
  ///  <summary>Class that implements a tag search filter.</summary>
  TTagSearchFilter = class(TBaseSearchFilter,
    ISearchFilter,
    ITagSearchFilter,
    ISearchUIInfo
  )
  strict private
    var
      ///  <summary>Set of search tags.</summary>
      fTags: ITagSet;
      ///  <summary>Search logic.</summary>
      fLogic: TSearchLogic;
  strict protected
    ///  <summary>Returns resource id of glyph bitmap.</summary>
    function GlyphResourceName: string; override;
  public
    ///  <summary>Constructs filter object with given criteria.</summary>
    ///  <param name="Tags">ITagSet [in] Set of search tags.</param>
    ///  <param name="Logic">TSearchLogic [in] Search logic to be used: AND or
    ///  OR.</param>
    constructor Create(Tags: ITagSet; const Logic: TSearchLogic);
    ///  <summary>Checks whether the snippet with the given ID matches the
    ///  filter's search criteria, returning True if so or False if not.
    ///  </summary>
    ///  <remarks>Method of ISearchFilter.</remarks>
    function Match(const SnippetID: TSnippetID): Boolean;
    ///  <summary>Indicates whether the object is a null filter. Returns False.
    ///  </summary>
    ///  <remarks>Method of ISearchFilter.</remarks>
    function IsNull: Boolean;
    ///  <summary>Read accessor for Logic property.</summary>
    ///  <returns>TSearchLogic. Search logic to be used: "and" which requires
    ///  that all tags in Tags property are found in a snippet or "or" which
    ///  permits snippets to contain any one or more such tags.</returns>
    ///  <remarks>Method of ITagSearchFilter.</remarks>
    function GetLogic: TSearchLogic;
    ///  <summary>Read accessor for Tags property.</summary>
    ///  <returns>ITagSet. Set of tags to be included in search.</returns>
    ///  <remarks>Method of ITagSearchFilter.</remarks>
    function GetTags: ITagSet;
  end;

type
  ///  <summary>Base class for search filters that select snippets from a given
  ///  list of snippets.</summary>
  TBaseSelectionSearchFilter = class abstract(TBaseSearchFilter)
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
    ///  <summary>Checks whether the snippet with the given ID matches the
    ///  filter's search criteria, returning True if so or False if not.
    ///  </summary>
    ///  <remarks>Method of ISearchFilter.</remarks>
    function Match(const SnippetID: TSnippetID): Boolean;
    ///  <summary>Indicates whether the object is a null filter. Returns False.
    ///  </summary>
    ///  <remarks>Method of ISearchFilter.</remarks>
    function IsNull: Boolean;
    ///  <summary>Returns list of snippets to be selected in search.</summary>
    ///  <remarks>Method of ISelectionSearchFilter.</remarks>
    function GetSelectedItems: ISnippetIDList;
  end;

type
  ///  <summary>Class that implements a selection search filter.</summary>
  ///  <remarks>This class is for use with searches where the user specifies the
  ///  snippets to be selected. It simply provides the correct glyph for this
  ///  search type.</remarks>
  TManualSelectionSearchFilter = class(TBaseSelectionSearchFilter,
    ISearchFilter,
    ISelectionSearchFilter,
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
  TStoredSelectionSearchFilter = class(TBaseSelectionSearchFilter,
    ISearchFilter,
    ISelectionSearchFilter,
    ISearchUIInfo
  )
  strict protected
    ///  <summary>Returns resource id of glyph bitmap.</summary>
    function GlyphResourceName: string; override;
  end;

type
  ///  <summary>Class that implements a cross-reference search filter.</summary>
  TXRefSearchFilter = class(TBaseSearchFilter,
    ISearchFilter,
    IXRefSearchFilter,
    ISearchUIInfo
  )
  strict private
    var
      ///  <summary>ID of snippet to which the cross-reference filter applies.
      ///  </summary>
      fBaseSnippetID: TSnippetID;
      ///  <summary>Set of options used to modify the operation of the filter.
      ///  </summary>
      fOptions: TXRefSearchOptions;
      ///  <summary>Stores list of cross-referenced snippets.</summary>
      fXRefs: ISnippetIDList;
    ///  <summary>Adds given snippet ID to list of x-refs if ID is not already
    ///  in the list. Returns True if ID added, False if not.</summary>
    function AddToXRefs(const ASnippetID: TSnippetID): Boolean; overload;
    ///  <summary>Adds all snippets with all the given IDs to the x-ref list
    ///  unless the snippet is already in the list.</summary>
    procedure AddToXRefs(ASnippetIDs: ISnippetIDList); overload;
    ///  <summary>Adds all the given snippet's required snippets to the x-ref
    ///  list.</summary>
    ///  <remarks>References are only added if appropriate search option is set.
    ///  </remarks>
    procedure ReferenceRequired(const ASnippetID: TSnippetID);
    ///  <summary>Adds all snippets that require (depend upon) the given snippet
    ///  is selected.</summary>
    ///  <remarks>References are only added if appropriate search option is set.
    ///  </remarks>
    procedure ReferenceReverseRequired(const ASnippetID: TSnippetID);
    ///  <summary>Adds all the given snippet's required snippets to x-ref list.
    ///  </summary>
    ///  <remarks>References are only added if appropriate search option is set.
    ///  </remarks>
    procedure ReferenceSeeAlso(const ASnippetID: TSnippetID);
    ///  <summary>Adds all snippets that cross-reference the given snippet is
    ///  selected.</summary>
    ///  <remarks>References are only added if appropriate search option is set.
    ///  </remarks>
    procedure ReferenceReverseSeeAlso(const ASnippetID: TSnippetID);
    ///  <summary>Adds a snippet to x-ref list if it is not already present.
    ///  Also recursively adds the snippet's all its cross-referenced snippets
    ///  if appropriate search options are set.</summary>
    procedure ReferenceSnippet(const ASnippetID: TSnippetID);
  strict protected
    ///  <summary>Returns resource id of glyph bitmap.</summary>
    function GlyphResourceName: string; override;
  public
    ///  <summary>Constructs filter object with given criteria.</summary>
    ///  <param name="BaseSnippetID">TSnippetID [in] ID of snippet whose cross
    ///  references are to be searched.</param>
    ///  <param name="Options">TXRefSearchOptions [in] Set of options used to
    ///  modify the operation of the filter.</param>
    constructor Create(const BaseSnippetID: TSnippetID;
      const Options: TXRefSearchOptions);
    ///  <summary>Called to initialise filter before a search is commenced.
    ///  Prepares list of XRefs.</summary>
    ///  <remarks>Method of ISearchFilter.</remarks>
    procedure Initialise; override;
    ///  <summary>Called to tidy up of filter after a search finishes. Clears
    ///  list of XRefs.</summary>
    ///  <remarks>Method of ISearchFilter.</remarks>
    procedure Finalise; override;
    ///  <summary>Checks whether the snippet with the given ID matches the
    ///  filter's search criteria, returning True if so or False if not.
    ///  </summary>
    ///  <remarks>Method of ISearchFilter.</remarks>
    function Match(const SnippetID: TSnippetID): Boolean;
    ///  <summary>Indicates whether the object is a null filter. Returns False.
    ///  </summary>
    ///  <remarks>Method of ISearchFilter.</remarks>
    function IsNull: Boolean;
    ///  <summary>Returns ID snippet whose cross references are to be searched.
    ///  </summary>
    ///  <remarks>Method of IXRefSearchFilter.</remarks>
    function GetBaseSnippetID: TSnippetID;
    ///  <summary>Returns set of options used to modify operation of the filter.
    ///  </summary>
    ///  <remarks>Method of IXRefSearchFilter.</remarks>
    function GetOptions: TXRefSearchOptions;
  end;

type
  ///  <summary>Class that implements a null, do-nothing, search filter.
  ///  </summary>
  TNullSearchFilter = class(TBaseSearchFilter,
    ISearchFilter,
    ISearchUIInfo
    )
  strict protected
    ///  <summary>Returns resource id of glyph bitmap.</summary>
    function GlyphResourceName: string; override;
  public
    ///  <summary>Checks whether the snippet with the given ID matches the
    ///  filter's search criteria. Always returns True.</summary>
    ///  <remarks>Method of ISearchFilter.</remarks>
    function Match(const SnippetID: TSnippetID): Boolean;
    ///  <summary>Indicates whether the object is a null filter. Returns True.
    ///  </summary>
    ///  <remarks>Method of ISearchFilter.</remarks>
    function IsNull: Boolean;
  end;

{ TSearch }

constructor TSearch.Create(const Filter: ISearchFilter);
begin
  Assert(Assigned(Filter), ClassName + '.Create: Filter is nil');
  inherited Create;
  fFilter := Filter;
end;

function TSearch.Execute(InList: ISnippetIDList; out FoundList: ISnippetIDList):
  Boolean;
var
  SnippetID: TSnippetID;    // each snippet ID in InList
begin
  Assert(Assigned(InList), ClassName + '.Execute: InList is nil');
  FoundList := TSnippetIDList.Create;
  if InList.IsEmpty then
    Exit(False);
  GetFilter.Initialise;
  try
    for SnippetID in InList do
      if GetFilter.Match(SnippetID) then
        FoundList.Add(SnippetID);
  finally
    GetFilter.Finalise;
  end;
  Result := not FoundList.IsEmpty;
end;

function TSearch.GetFilter: ISearchFilter;
begin
  Result := fFilter;
end;

{ TBaseSearchFilter }

destructor TBaseSearchFilter.Destroy;
begin
  fBitmap.Free;
  inherited;
end;

procedure TBaseSearchFilter.Finalise;
begin
  // Do nothing
end;

function TBaseSearchFilter.Glyph: TBitmap;
begin
  if not Assigned(fBitmap) then
  begin
    // Bitmap not yet created: create it and load from resources
    fBitmap := TBitmap.Create;
    fBitmap.LoadFromResourceName(HInstance, GlyphResourceName);
  end;
  Result := fBitmap;
end;

procedure TBaseSearchFilter.Initialise;
begin
  // Do nothing
end;

{ TCompilerSearchFilter }

constructor TCompilerSearchFilter.Create(
  const Compilers: TCompilerIDs; const Logic: TSearchLogic;
  const Option: TCompilerSearchOption);
begin
  inherited Create;
  // Store properties
  fCompilers := Compilers;
  fLogic := Logic;
  fOption := Option;
end;

function TCompilerSearchFilter.GetCompilers: TCompilerIDs;
begin
  Result := fCompilers;
end;

function TCompilerSearchFilter.GetLogic: TSearchLogic;
begin
  Result := fLogic;
end;

function TCompilerSearchFilter.GetOption: TCompilerSearchOption;
begin
  Result := fOption;
end;

function TCompilerSearchFilter.GlyphResourceName: string;
begin
  Result := 'COMPILERSEARCH';
end;

function TCompilerSearchFilter.IsNull: Boolean;
begin
  Result := False;
end;

function TCompilerSearchFilter.Match(const SnippetID: TSnippetID): Boolean;
const
  // Maps compiler search option onto set of compiler results it describes
  cCompatMap: array[TCompilerSearchOption] of set of TCompileResult = (
    [crSuccess, crWarning],   // soCompileOK,
    [crSuccess],              // soCompileNoWarn,
    [crWarning],              // soCompileWarn,
    [crError],                // soCompileFail,
    [crQuery]                 // soUnkown
  );

  // Checks if a snippet's compiler results for given compiler ID matches
  // expected results.
  function CompileResultsMatch(Snippet: ISnippet;
    const CompID: TCompilerID): Boolean;
  begin
    Result := Snippet.CompileResults[CompID] in cCompatMap[fOption];
  end;

var
  CompID: TCompilerID;  // loops thru supported compilers
  Snippet: ISnippet;    //
begin
  Snippet := Database.LookupSnippet(SnippetID);
  if fLogic = slOr then
  begin
    // Find any compiler: we return true as soon as any compiler compatibility
    // matches
    Result := False;
    for CompID in fCompilers do
    begin
      if CompileResultsMatch(Snippet, CompID) then
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
      if not CompileResultsMatch(Snippet, CompID) then
        Exit(False);
    end;
  end;
end;

{ TTextSearchFilter }

constructor TTextSearchFilter.Create(const Words: string;
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

destructor TTextSearchFilter.Destroy;
begin
  fWords.Free;
  inherited;
end;

function TTextSearchFilter.GetLogic: TSearchLogic;
begin
  Result := fLogic;
end;

function TTextSearchFilter.GetOptions: TTextSearchOptions;
begin
  Result := fOptions;
end;

function TTextSearchFilter.GetWords: TStrings;
begin
  Result := fWords;
end;

function TTextSearchFilter.GlyphResourceName: string;
begin
  Result := 'TEXTSEARCH';
end;

function TTextSearchFilter.IsNull: Boolean;
begin
  Result := False;
end;

function TTextSearchFilter.Match(const SnippetID: TSnippetID): Boolean;

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
      if (S[1] = SINGLEQUOTE) or (S[1] = DOUBLEQUOTE) then
        Delete(S, 1, 1);
      if (S <> '') and
        ((S[Length(S)] = SINGLEQUOTE) or (S[Length(S)] = DOUBLEQUOTE)) then
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
      '!', DOUBLEQUOTE, '%', '^', '&', '*', '(', ')', '-', '+', '=',
      '{', '}', '[', ']', ':', ';', '~', '<', '>', ',', '.',
      '?', '/', '|', '\', SINGLEQUOTE
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
  SearchText: string;                      // text we're searching in
  SearchWord: string;                      // a word we're searching for
  ActiveTextRenderer: IActiveTextRenderer; // converts active text to plain text
  StrBuilder: TStringBuilder;              // constructs search text
  Snippet: ISnippet;                       // snippet we're matching
begin
  Snippet := Database.LookupSnippet(SnippetID);
  // Build search text
  StrBuilder := TStringBuilder.Create;
  try
    ActiveTextRenderer := TActiveTextPlainTextRenderer.Create(
      StrBuilder, ' ', [ptrIgnoreEmptyBlocks]
    );
    StrBuilder.Append(' ');
    Snippet.Description.Render(ActiveTextRenderer);
    StrBuilder.Append(' ');
    Snippet.Notes.Render(ActiveTextRenderer);
    StrBuilder.Append(' ');
    StrBuilder.Append(Snippet.SourceCode);
    StrBuilder.Append(' ');
    SearchText := NormaliseSearchText(StrBuilder.ToString);
  finally
    StrBuilder.Free;
  end;
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

{ TTagSearchFilter }

constructor TTagSearchFilter.Create(Tags: ITagSet; const Logic: TSearchLogic);
begin
  Assert(Assigned(Tags), ClassName + '.Create: Tags is nil');
  inherited Create;
  fTags := Tags;
  fLogic := Logic;
end;

function TTagSearchFilter.GetLogic: TSearchLogic;
begin
  Result := fLogic;
end;

function TTagSearchFilter.GetTags: ITagSet;
begin
  Result := fTags;
end;

function TTagSearchFilter.GlyphResourceName: string;
begin
  Result := 'TAGSSEARCH';
end;

function TTagSearchFilter.IsNull: Boolean;
begin
  Result := False;
end;

function TTagSearchFilter.Match(const SnippetID: TSnippetID): Boolean;
var
  Tag: TTag;
  Snippet: ISnippet;
begin
  Snippet := Database.LookupSnippet(SnippetID);
  case fLogic of
    slAnd:
    begin
      for Tag in Snippet.Tags do
        if not fTags.Contains(Tag) then
          Exit(False);
      Result := True;
    end;
    slOr:
    begin
      for Tag in Snippet.Tags do
        if fTags.Contains(Tag) then
          Exit(True);
      Result := False;
    end;
    else
      raise EBug.Create(ClassName + '.Match: Invalid TSearchLogic value');
  end;
end;

{ TBaseSelectionSearchFilter }

constructor TBaseSelectionSearchFilter.Create(
  const SelectedItems: ISnippetIDList);
begin
  inherited Create;
  fSelectedItems := TSnippetIDList.Create;
  (fSelectedItems as IAssignable).Assign(SelectedItems);
end;

function TBaseSelectionSearchFilter.GetSelectedItems: ISnippetIDList;
begin
  Result := fSelectedItems;
end;

function TBaseSelectionSearchFilter.IsNull: Boolean;
begin
  Result := False;
end;

function TBaseSelectionSearchFilter.Match(const SnippetID: TSnippetID): Boolean;
begin
  Result := fSelectedItems.Contains(SnippetID);
end;

{ TManualSelectionSearchFilter }

function TManualSelectionSearchFilter.GlyphResourceName: string;
begin
  Result := 'SELECTIONSEARCH';
end;

{ TStoredSelectionSearchFilter }

function TStoredSelectionSearchFilter.GlyphResourceName: string;
begin
  Result := 'STOREDSELECTIONSEARCH';
end;

{ TXRefSearchFilter }

function TXRefSearchFilter.AddToXRefs(const ASnippetID: TSnippetID): Boolean;
begin
  Result := not fXRefs.Contains(ASnippetID);
  if Result then
    fXRefs.Add(ASnippetID);
end;

procedure TXRefSearchFilter.AddToXRefs(ASnippetIDs: ISnippetIDList);
var
  SnippetID: TSnippetID;
begin
  for SnippetID in ASnippetIDs do
    AddToXRefs(SnippetID);
end;

constructor TXRefSearchFilter.Create(const BaseSnippetID: TSnippetID;
  const Options: TXRefSearchOptions);
begin
  Assert(not BaseSnippetID.IsNull,
    ClassName + '.Create: BaseSnippetID is null');
  inherited Create;
  fBaseSnippetID := BaseSnippetID;
  fOptions := Options;
  fXRefs := TSnippetIDList.Create;
end;

procedure TXRefSearchFilter.Finalise;
begin
  fXRefs.Clear;
end;

function TXRefSearchFilter.GetBaseSnippetID: TSnippetID;
begin
  Result := fBaseSnippetID;
end;

function TXRefSearchFilter.GetOptions: TXRefSearchOptions;
begin
  Result := fOptions;
end;

function TXRefSearchFilter.GlyphResourceName: string;
begin
  Result := 'XREFSEARCH';
end;

procedure TXRefSearchFilter.Initialise;
begin
  Assert(Assigned(fXRefs), ClassName + '.Initialise: fXRefs is nil');
  fXRefs.Clear;
  ReferenceRequired(fBaseSnippetID);
  ReferenceReverseRequired(fBaseSnippetID);
  ReferenceSeeAlso(fBaseSnippetID);
  ReferenceReverseSeeAlso(fBaseSnippetID);
  if soIncludeSnippet in fOptions then
    AddToXRefs(fBaseSnippetID);
end;

function TXRefSearchFilter.IsNull: Boolean;
begin
  Result := False;
end;

function TXRefSearchFilter.Match(const SnippetID: TSnippetID): Boolean;
begin
  // Check if cross references are still to be calcaluted and do it if so
  // We do this here to avoid the overhead if just using object to store / read
  // persistent settings.
  if not Assigned(fXRefs) then
  begin
    fXRefs := TSnippetIDList.Create;
    Initialise;
  end;
  Result := fXRefs.Contains(SnippetID);
end;

procedure TXRefSearchFilter.ReferenceRequired(const ASnippetID: TSnippetID);
var
  ASnippet: ISnippet;
  SnippetID: TSnippetID;
begin
  if not (soRequired in fOptions) then
    Exit;
  ASnippet := Database.LookupSnippet(ASnippetID);
  for SnippetID in ASnippet.RequiredSnippets do
    ReferenceSnippet(SnippetID);
end;

procedure TXRefSearchFilter.ReferenceReverseRequired(
  const ASnippetID: TSnippetID);
begin
  if not (soRequiredReverse in fOptions) then
    Exit;
  AddToXRefs(Database.GetDependentsOf(ASnippetID));
end;

procedure TXRefSearchFilter.ReferenceReverseSeeAlso(
  const ASnippetID: TSnippetID);
begin
  if not (soSeeAlsoReverse in fOptions) then
    Exit;
  AddToXRefs(Database.GetReferrersTo(ASnippetID));
end;

procedure TXRefSearchFilter.ReferenceSeeAlso(const ASnippetID: TSnippetID);
var
  ASnippet: ISnippet;
  SnippetID: TSnippetID;
begin
  if not (soSeeAlso in fOptions) then
    Exit;
  ASnippet := Database.LookupSnippet(ASnippetID);
  for SnippetID in ASnippet.XRefs do
    ReferenceSnippet(SnippetID);
end;

procedure TXRefSearchFilter.ReferenceSnippet(const ASnippetID: TSnippetID);
begin
  // Add snippet to list if not present. Quit if snippet already referenced.
  if not AddToXRefs(ASnippetID) then
    Exit;
  // Recurse required snippets if specified in options
  if soRequiredRecurse in fOptions then
    ReferenceRequired(ASnippetID);
  // Recurse "see also" snippets if specified in options
  if soSeeAlsoRecurse in fOptions then
    ReferenceSeeAlso(ASnippetID);
end;

{ TNullSearchFilter }

function TNullSearchFilter.GlyphResourceName: string;
begin
  Result := 'NULSEARCH';
end;

function TNullSearchFilter.IsNull: Boolean;
begin
  Result := True;
end;

function TNullSearchFilter.Match(const SnippetID: TSnippetID): Boolean;
begin
  Result := True;
end;

{ TSearchFactory }

class function TSearchFactory.CreateNullSearch: ISearch;
begin
  Result := CreateSearch(TNullSearchFilter.Create);
end;

class function TSearchFactory.CreateSearch(Filter: ISearchFilter): ISearch;
begin
  Result := TSearch.Create(Filter);
end;

{ TSearchFilterFactory }

class function TSearchFilterFactory.CreateCompilerSearchFilter(
  const Compilers: TCompilerIDs; const Logic: TSearchLogic;
  const Option: TCompilerSearchOption): ICompilerSearchFilter;
begin
  Result := TCompilerSearchFilter.Create(Compilers, Logic, Option);
end;

class function TSearchFilterFactory.CreateManualSelectionSearchFilter(
  SelectedSnippets: ISnippetIDList): ISelectionSearchFilter;
begin
  Result := TManualSelectionSearchFilter.Create(SelectedSnippets);
end;

class function TSearchFilterFactory.CreateStoredSelectionSearchFilter(
  SelectedSnippets: ISnippetIDList): ISelectionSearchFilter;
begin
  Result := TStoredSelectionSearchFilter.Create(SelectedSnippets);
end;

class function TSearchFilterFactory.CreateTagSearchFilter(Tags: ITagSet;
  const Logic: TSearchLogic): ITagSearchFilter;
begin
  Result := TTagSearchFilter.Create(Tags, Logic);
end;

class function TSearchFilterFactory.CreateTextSearchFilter(
  const Words: string; const Logic: TSearchLogic;
  const Options: TTextSearchOptions): ITextSearchFilter;
begin
  Result := TTextSearchFilter.Create(Words, Logic, Options);
end;

class function TSearchFilterFactory.CreateXRefSearchFilter(
  const BaseSnippetID: TSnippetID;
  const Options: TXRefSearchOptions): IXRefSearchFilter;
begin
  Result := TXRefSearchFilter.Create(BaseSnippetID, Options);
end;

end.

