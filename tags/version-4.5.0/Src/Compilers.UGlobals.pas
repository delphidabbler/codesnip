{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Declares various types that describe the compiler and compilation results and
 * defines interfaces to compiler objects.
}


unit Compilers.UGlobals;


interface


uses
  // Delphi
  Generics.Collections, Classes, Graphics;


type
  ///  <summary>Enumeration that identifies all compilers supported by the
  ///  program.</summary>
  TCompilerID = (
    ciD2, ciD3, ciD4, ciD5, ciD6, ciD7, // Delphi 2-7
    ciD2005w32, ciD2006w32,             // Delphi 2005/6 Win32 personality
    ciD2007,                            // Delphi 2007 for Win32
    ciD2009w32,                         // Delphi 2009 Win32 personality
    ciD2010,                            // Delphi 2010
    ciDXE,                              // Delphi XE
    ciDXE2,                             // Delphi XE2
    ciDXE3,                             // Delphi XE3
    ciDXE4,                             // Delphi XE4
    ciFPC                               // Free Pascal
  );

const
  ///  <summary>Set of classic Borland / Inprise Delphi compilers.</summary>
  cClassicDelphiCompilers = [ciD2, ciD3, ciD4, ciD5, ciD6, ciD7];

const
  ///  <summary>Set of Borland / CodeGear / Embarcadero BDS based Delphi
  ///  compilers.</summary>
  cBDSCompilers = [
    ciD2005w32, ciD2006w32, ciD2007, ciD2009w32, ciD2010, ciDXE, ciDXE2,
    ciDXE3, ciDXE4
  ];

const
  ///  <summary>Set of Free Pascal compilers.</summary>
  ///  <remarks>At present there is only the one compiler: no distinction is
  ///  made between different versions.</remarks>
  cFreePascalCompilers = [ciFPC];

type
  ///  <summary>Enumeration of possible results of a compilation.</summary>
  TCompileResult = (
    crSuccess,    // successful compilation without warnings
    crWarning,    // successful compilation with warnings
    crError,      // compilation failed
    crQuery       // compilation result not known
  );

type
  ///  <summary>Defines an array of TCompileResult values with an element for
  ///  each supported compiler.</summary>
  TCompileResults = array[TCompilerID] of TCompileResult;

type
  ///  <summary>Enumeration of different warning and error prefixes that can be
  ///  recognised in a compiler log file.</summary>
  ///  <remarks>These values are used to identify the prefix text used by a
  ///  compiler.</remarks>
  TCompLogPrefixID = (
    cpFatal,      // identifies fatal error messages in log file
    cpError,      // identifies error messages in log file
    cpWarning     // identifies warnings in log file
  );


type
  ///  <summary>Defines an array used to store prefix text of warning and error
  ///  messages in compiler log files.</summary>
  ///  <remarks>Prefixes may be different for each compiler and may change with
  ///  the compiler's output language.</remarks>
  TCompLogPrefixes = array[TCompLogPrefixID] of string;

type
  ///  <summary>Enumeration of various filter types that can be applied to
  ///  compiler output logs.</summary>
  TCompLogFilter = (
    cfAll,        // no filtering: use all of log
    cfWarnings,   // filter out anything that is not a warning message
    cfErrors      // filter out anything that is not an error message
  );

type
  ///  <summary>Interface to list of directories to be searched by a compiler
  ///  when looking for files.</summary>
  ISearchDirs = interface(IInterface)
    ['{77CAFAC1-9B0F-4244-9FFF-A9FB4EBDEE8B}']

    ///  <summary>Creates and returns enumerator for directories list.</summary>
    function GetEnumerator: TEnumerator<string>;

    ///  <summary>Adds a new search directory to list.</summary>
    procedure Add(const DirName: string);

    ///  <summary>Clears list.</summary>
    procedure Clear;

    ///  <summary>Checks if list is empty.</summary>
    function IsEmpty: Boolean;

    ///  <summary>Returns an array containing the names of all directories in
    ///  the list.</summary>
    function ToStrings: TArray<string>;
  end;

type
  ///  <summary>Interface that must be supported by any object that represents a
  ///  compiler. Exposes methods used to get information about the compiler, to
  ///  execute the compiler and to process the compiler's log file.</summary>
  ICompiler = interface(IInterface)
    ['{8D473D8A-3341-401C-B054-17E9334DF6A6}']

    ///  <summary>Returns the human readable name of the compiler.</summary>
    function GetName: string;

    ///  <summary>Returns the compiler's unique ID.</summary>
    function GetID: TCompilerID;

    ///  <summary>Returns a non-localisable string that uniquely identifies the
    ///  compiler.</summary>
    function GetIDString: string;

    ///  <summary>Checks whether the compiler is both installed on this computer
    ///  and made available to CodeSnip.</summary>
    function IsAvailable: Boolean;

    ///  <summary>Returns the full path of the compiler's executable file.
    ///  </summary>
    ///  <remarks>Returns the empty string if the compiler is not known to
    ///  CodeSnip.</remarks>
    function GetExecFile: string;

    ///  <summary>Records the the full path of the compiler's executable file.
    ///  </summary>
    ///  <remarks>Passing the empty string to this method disassociates the
    ///  compiler from CodeSnip.</remarks>
    procedure SetExecFile(const Value: string);

    ///  <summary>Returns a comma separated list of the default command line
    ///  switches for use with the compiler.</summary>
    ///  <remarks>The default switches are used if the user has not provided any
    ///  switches.</remarks>
    function GetDefaultSwitches: string;

    ///  <summary>Returns a comma separated list of any user defined switches
    ///  for use with the compiler.</summary>
    function GetSwitches: string;

    ///  <summary>Records the given comma delimited list of user defined
    ///  switches to be used with the compiler.</summary>
    procedure SetSwitches(const Switches: string);

    ///  <summary>Checks if the compiler has RTL unit names that are prefixed by
    ///  its namespace.</summary>
    function RequiresRTLNamespaces: Boolean;

    ///  <summary>Returns a space separated list of the compiler's default RTL
    ///  unit namespaces.</summary>
    function GetDefaultRTLNamespaces: string;

    ///  <summary>Returns a space separated list of user-defined RTL unit
    ///  namespaces to be searched by the compiler.</summary>
    function GetRTLNamespaces: string;

    ///  <summary>Records a list of user defined RTL unit namespaces to be
    ///  searched by the compiler.</summary>
    ///  <remarks>Namespaces is expected to be a space separated list of valid
    ///  Pascal identfiers.</remarks>
    procedure SetRTLNamespaces(const Namespaces: string);

    ///  <summary>Returns a copy of the list of search directories used by the
    ///  compiler.</summary>
    function GetSearchDirs: ISearchDirs;

    ///  <summary>Records a copy of the given list of search directories to be
    ///  used by the compiler.</summary>
    procedure SetSearchDirs(Dirs: ISearchDirs);

    ///  <summary>Returns the prefixes used in interpreting error, fatal error
    ///  and warning conditions in compiler log files.</summary>
    function GetLogFilePrefixes: TCompLogPrefixes;

    ///  <summary>Records the given prefixes to be used in interpreting error,
    ///  fatal error and warning conditions in compiler log files.</summary>
    procedure SetLogFilePrefixes(const Prefixes: TCompLogPrefixes);

    ///  <summary>Returns a flag indicating if the compiler is displayable.
    ///  </summary>
    ///  <remarks>A 'displayable' compiler has its compile results displayed in
    ///  the UI etc.</remarks>
    function GetDisplayable: Boolean;

    ///  <summary>Sets the flag that determines if the compiler is displayable
    ///  to the given value.</summary>
    ///  <remarks>A 'displayable' compiler has its compile results displayed in
    ///  the UI etc.</remarks>
    procedure SetDisplayable(const Flag: Boolean);

    ///  <summary>Compiles a project and returns the result of compilation.
    ///  </summary>
    ///  <param name="Path">string [in] The full path of the directory
    ///  containing the project file.</param>
    ///  <param name="Project">string [in] Name of project source file.</param>
    ///  <returns>TCompileResult. Result of compilation (success, warning or
    ///  error).</returns>
    ///  <exception>An exception is raised if the compiler can't be executed.
    ///  </exception>
    ///  <remarks>The result of the compilation and the compiler output log are
    ///  stored: see the Log and GetLastCompileResult methods.</remarks>
    function Compile(const Path, Project: string): TCompileResult;

    ///  <summary>Deletes intermediate files created during a compilation of a.
    ///  project.</summary>
    ///  <param name="Path">string [in] The full path of the directory
    ///  containing the project file.</param>
    ///  <param name="Project">string [in] Name of project source file.</param>
    ///  <remarks>Does nothing if no project has been compiled.</remarks>
    procedure DeleteObjFiles(const Path, Project: string);

    ///  <summary>Filters compiler output log and copies the result into a
    ///  string list.</summary>
    ///  <param name="Filter">TCompLogFilter [in] Indicates how the compiler log
    ///  is to be filtered.</param>
    ///  <param name="Lines">TStrings [in] String list that receives the lines
    ///  of the filtered log. May be empty.</param>
    procedure Log(const Filter: TCompLogFilter; const Lines: TStrings);
      overload;

    ///  <summary>Filters compiler output log using a filter of given type and
    ///  returns the result as a string with lines delimited by CRLF.</summary>
    function Log(const Filter: TCompLogFilter): string;
      overload;

    ///  <summary>Checks if last compilation resulted in an error or a warning.
    ///  </summary>
    ///  <remarks>Returns False if Compile method has not been called.</remarks>
    function HasErrorsOrWarnings: Boolean;

    ///  <summary>Returns result of last compilation by this compiler.</summary>
    ///  <remarks>crQuery is returned if compiler is not available or if Compile
    ///  method has not been called.</remarks>
    function GetLastCompileResult: TCompileResult;
  end;

type
  ///  <summary>Interface implemented by objects that maintain a list of all
  ///  compilers supported by the program.</summary>
  ICompilers = interface(IInterface)
    ['{39083768-2479-4F39-8473-0BDA381E1E81}']

    ///  <summary>Getter for Compilers property.</summary>
    function GetCompiler(CompID: TCompilerID): ICompiler;

    ///  <summary>Getter for Count property.</summary>
    function GetCount: Integer;

    ///  <summary>Getter for AvailableCount property.</summary>
    function GetAvailableCount: Integer;

    ///  <summary>Creates and returns an enumerator for all compilers in this
    ///  object.</summary>
    function GetEnumerator: TEnumerator<ICompiler>;

    ///  <summary>List of all compilers supported by the program.</summary>
    ///  <remarks>Ths list is indexed by compiler ID.</remarks>
    property Compilers[Ver: TCompilerID]: ICompiler
      read GetCompiler; default;
      {List of all compilers supported by the program, indexed by compiler id}

    ///  <summary>Number of compilers in list.</summary>
    property Count: Integer read GetCount;

    ///  <summary>Number of compilers installed on this computer and made
    ///  available CodeSnip.</summary>
    property AvailableCount: Integer read GetAvailableCount;

    ///  <summary>Checks if any compilers in the list are displayable.</summary>
    function HaveDisplayable: Boolean;
  end;

type
  ///  <summary>Iterface supported by objects that can save and load an
  ///  ICompilers object to and from persistent storage.</summary>
  IPersistCompilers = interface(IInterface)
    ['{76A78863-5B95-4ECA-9295-75A98994EFA5}']
    ///  <summary>Saves the given list of compilers to persistent storage.
    ///  </summary>
    procedure Save(const Compilers: ICompilers);
    ///  <summary>Loads given list of compiler from storage, overwriting any
    ///  existing previous content.</summary>
    procedure Load(const Compilers: ICompilers);
  end;

type
  ///  <summary>Interface supported by compiler objects that can detect their
  ///  own executable file path.</summary>
  ICompilerAutoDetect = interface(IInterface)
    ['{62FE97CE-4616-406B-A9C2-D4B3BC751936}']
    ///  <summary>Detects and records the full path of the compiler's
    ///  executable.</summary>
    function DetectExeFile: Boolean;
  end;


implementation

end.

