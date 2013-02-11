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

  {
  TCompilerID:
    Enumeration that identifies all compilers supported by the program.
  }
  TCompilerID = (
    ciD2, ciD3, ciD4, ciD5, ciD6, ciD7, // Delphi 2-7
    ciD2005w32, ciD2006w32,             // Delphi 2005/6 Win32 personality
    ciD2007,                            // Delphi 2007 for Win32
    ciD2009w32,                         // Delphi 2009 Win32 personality
    ciD2010,                            // Delphi 2010
    ciDXE,                              // Delphi XE
    ciDXE2,                             // Delphi XE2
    ciDXE3,                             // Delphi XE3
    ciFPC                               // Free Pascal
  );

const
  // Sets grouping compiler ids into compiler types
  cClassicDelphiCompilers =   // Classic Borland / Inprise Delphi
    [ciD2, ciD3, ciD4, ciD5, ciD6, ciD7];
  cBDSCompilers =             // BDS based compilers
    [
      ciD2005w32, ciD2006w32, ciD2007, ciD2009w32, ciD2010,
      ciDXE, ciDXE2, ciDXE3
    ];
  cFreePascalCompilers =      // Free Pascal
    [ciFPC];

type

  {
  TCompileResult:
    Enumeration of possible results of a compilation.
  }
  TCompileResult = (
    crSuccess,    // successful compilation without warnings
    crWarning,    // successful compilation with warnings
    crError,      // compilation failed
    crQuery       // compilation result not known
  );

  {
  TCompileResults:
    Defines array of TCompileResult values with an entry for each supported
    compiler.
  }
  TCompileResults = array[TCompilerID] of TCompileResult;

  {
  TCompLogPrefixID:
    Enumeration of different prefixes compiler logs can recognise in a log file.
    The values are used to map onto the prefix text used by for a specific
    compiler.
  }
  TCompLogPrefixID = (
    cpFatal,      // identifies fatal error messages in log file
    cpError,      // identifies error messages in log file
    cpWarning     // identifies warnings in log file
  );

  {
  TCompLogPrefixes
    Defines array used to record prefix text used to detect error and warning
    lines in compiler log files.
  }
  TCompLogPrefixes = array[TCompLogPrefixID] of string;

  {
  TCompLogFilter:
    Enumeration of various filter types that can be applied to compiler output
    logs.
  }
  TCompLogFilter = (
    cfAll,        // no filtering: use all of log
    cfWarnings,   // filter out anything that is not a warning message
    cfErrors      // filter out anything that is not an error message
  );

type
  ///  <summary>
  ///  Interface to list of directories to be searched by a compiler when
  ///  looking for files.
  ///  </summary>
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

  {
  ICompiler:
    Interface that must be supported by any object that represents a compiler.
    Exposes methods used to get information about the compiler, to execute the
    compiler and to process the compiler's log file.
  }
  ICompiler = interface(IInterface)
    ['{8D473D8A-3341-401C-B054-17E9334DF6A6}']
    function GetName: string;
      {Provides the human readable name of the compiler.
        @return Compiler name.
      }
    function GetID: TCompilerID;
      {Provides the unique id of the compiler.
        @return Compiler Id code.
      }
    function GetIDString: string;
      {Provides a non-localisable string that identifies the compiler.
        @return Compiler Id string.
      }
    function IsAvailable: Boolean;
      {Tells whether the compiler is installed on this computer and made
      available to CodeSnip.
        @return True if compiler is available to CodeSnip.
      }
    function GetExecFile: string;
      {Gets full path to compiler's executable file.
        @return Required path.
      }
    procedure SetExecFile(const Value: string);
      {Stores full path to compiler's executable file.
        @param Value [in] Path to compiler.
      }
    function GetDefaultSwitches: string;
      {Returns default command line switches for compiler.
        @return Switches separated by commas.
      }
    function GetSwitches: string;
      {Returns user-defined swtches to be used by compiler.
        @return Required switches separated by commas.
      }
    procedure SetSwitches(const Switches: string);
      {Sets user defined switches.
        @param Switches [in] Required switches separated by commas.
      }
    ///  <summary>Checks if compiler has RTL unit names that are prefixed by
    ///  its namespace.</summary>
    function RequiresRTLNamespaces: Boolean;
    ///  <summary>Returns a space separated list of compiler's default RTL unit
    ///  namespaces.</summary>
    function GetDefaultRTLNamespaces: string;
    ///  <summary>Returns a space separated list of user-defined RTL unit
    ///  namespaces.</summary>
    function GetRTLNamespaces: string;
    ///  <summary>Sets user defined RTL unit namespaces.</summary>
    ///  <remarks>Namespaces is expected to be a space separated list of valid
    ///  Pascal identfiers.</remarks>
    procedure SetRTLNamespaces(const Namespaces: string);
    function GetSearchDirs: ISearchDirs;
      {Returns copy of list of search directories used by compiler.
        @return Required list of directories.
      }
    procedure SetSearchDirs(Dirs: ISearchDirs);
      {Stores a copy of given list of search directories.
        @param Dirs [in] List of search directories.
      }
    function GetLogFilePrefixes: TCompLogPrefixes;
      {Returns prefixes used in interpreting error, fatal error and warning
      conditions in log files.
        @return Array of prefix strings.
      }
    procedure SetLogFilePrefixes(const Prefixes: TCompLogPrefixes);
      {Records prefixes used in interpreting error, fatal error and warning
      conditions in log files.
        @param Prefixes [in] Array of required prefix strings.
      }
    function GetDisplayable: Boolean;
      {Returns flag indicating if compiler is displayable, i.e. compile results
      for it are to be displayed in UI etc.
        @return Boolean flag.
      }
    procedure SetDisplayable(const Flag: Boolean);
      {Sets a flag indicating if compiler is displayable, i.e. compile results
      for it are to be displayed in UI etc.
        @param Flag [in] Requried value.
      }
    function Compile(const Path, Project: string): TCompileResult;
      {Compiles a project and returns result of compilation. Records result of
      compilation and compiler's output log.
        @param Path [in] Path where the project is found.
        @param Project [in] Name of project (source) file.
        @return Result of compilation i.e. success, warnings or error.
        @except Exception raised if we can't execute the compiler.
      }
    procedure DeleteObjFiles(const Path, Project: string);
      {Delete binary intermdiates files created during a compilation. Does
      nothing if there has been no compilation.
        @param Path [in] Path where project file is found.
        @param Project [in] Name of project (source file)
      }
    procedure Log(const Filter: TCompLogFilter; const Lines: TStrings);
      overload;
      {Copies filtered compiler log to a string list.
        @param Filter [in] Indicates how log to be filtered i.e. return all log,
          return warnings only or return errors only.
        @param Lines [in] List of string in filtered log (cleared if no
          entries).
      }
    function Log(const Filter: TCompLogFilter): string;
      overload;
      {Returns compiler log as a CRLF delimited string using a filter.
        @param Filter [in] Indicates how log to be filtered i.e. return all log,
          return warnings only or return errors only.
        @return Text of log, with lines separated by CRLF.
      }
    function HasErrorsOrWarnings: Boolean;
      {Checks if last compile result was an error or a warning.
        @return True if there are errors or warning, False otherwise.
      }
    function GetLastCompileResult: TCompileResult;
      {Informs of result of last compilation by this compiler.
        @return Result of last compilation or crQuery of compiler not available.
      }
  end;

  {
  ICompilers:
    Interface implemented by an object that maintans a list of all compilers
    supported by the program.
  }
  ICompilers = interface(IInterface)
    ['{39083768-2479-4F39-8473-0BDA381E1E81}']
    function GetCompiler(CompID: TCompilerID): ICompiler;
      {Read accessor for Compilers property. Returns requested compiler object.
        @param CompID [in] Id of required compiler.
        @return Requested compiler object.
      }
    function GetCount: Integer;
      {Read access method for Count property.
        @return Number of compilers in list.
      }
    function GetAvailableCount: Integer;
      {Read access method for AvailableCount property
        @return Number of installed compilers available to program.
      }
    function GetEnumerator: TEnumerator<ICompiler>;
      {Gets an enumerator that enumerates all compilers in this object.
        @return Required enumerator.
      }
    property Compilers[Ver: TCompilerID]: ICompiler
      read GetCompiler; default;
      {List of all compilers supported by the program, indexed by compiler id}
    property Count: Integer
      read GetCount;
      {Number of compilers in list}
    property AvailableCount: Integer
      read GetAvailableCount;
      {Number of compilers installed on this computer and made available to
      program}
    function HaveDisplayable: Boolean;
      {Checks if any compilers are displayable.
        @return True if at least one compiler is displayable, False otherwise.
      }
  end;

  {
  IPersistCompilers:
    Iterface supported by objects that can save and load an ICompilers object to
    and from persistent storage, e.g. the program's ini file.
  }
  IPersistCompilers = interface(IInterface)
    ['{76A78863-5B95-4ECA-9295-75A98994EFA5}']
    procedure Save(const Compilers: ICompilers);
      {Save a list of compilers to storage.
        @param Compilers [in] List of compilers to save.
      }
    procedure Load(const Compilers: ICompilers);
      {Load a list of compilers from persistent storage.
        @param Compilers [in] List of compilers to load.
      }
  end;

  {
  ICompilerAutoDetect:
    Interface supported by compiler objects that can detect their executable
    file path as registered on the host computer.
  }
  ICompilerAutoDetect = interface(IInterface)
    ['{62FE97CE-4616-406B-A9C2-D4B3BC751936}']
    function DetectExeFile: Boolean;
      {Detects and records path to command line compiler if present.
        @return True if compiler path found, false otherwise.
      }
  end;


implementation

end.

