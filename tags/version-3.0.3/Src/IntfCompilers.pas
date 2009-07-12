{
 * IntfCompilers.pas
 *
 * Declares various types that describe the compiler and compilation results and
 * defines interfaces to compiler objects.
 *
 * v0.1 of 30 Jan 2005  - Original version.
 * v0.2 of 22 Feb 2005  - Localised compiler names.
 * v0.3 of 24 Feb 2005  - Removed compiler names from unit: now available via
 *                        global Compilers objects.
 * v0.4 of 05 Mar 2005  - Added interface declarations for new ICompiler,
 *                        ICompilers, IPersistCompilers and ICompilerAutoDetect
 *                        objects.
 *                      - Renamed TDelphhiVersion as TCompilerID and added new
 *                        item to enumeration to represent the Free Pascal
 *                        compiler.
 *                      - Renamed TCompilerLogFilter as TCompLogFilter.
 *                      - Added new TCompLogPrefixID that enumerates the
 *                        recognised compiler log prefixes that identify error
 *                        and warning a error lines and TCompLogPrefixes array
 *                        to hold the prefixes.
 * v0.5 of 20 Apr 2005  - Renamed from UCompilerTypes.pas to IntfCompilers.pas.
 * v0.6 of 25 Apr 2005  - Added new AvailableCount method to ICompilers along
 *                        with associated GetAvailableCount method.
 * v0.7 of 08 Jan 2006  - Removed redundant parameterss from ICompiler's Compile
 *                        and DeleteObjFiles methods.
 *                      - Added ids for Delphi 2005/6 to TCompilerID.
 *                      - Changed ICompilerAutoDetect's only method to enable
 *                        implementation as part of compiler objects rather than
 *                        in removed TCompilerAutoDetect class.
 *                      - Added support to ICompiler for user-defined command
 *                        line switches.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 *                      - Removed warnings about un-fixed interfaces: now fixed.
 *                      - Removed unused unit reference.
 * v1.1 of 08 May 2007  - Added support for Delphi 2007 by adding item to
 *                        TCompilerID enumeration.
 * v1.2 of 25 Aug 2008  - Added ICompilersEnum interface and added GetEnumerator
 *                        method to ICompilers.
 * v1.3 of 11 Oct 2008  - Added support for Delphi 2009 by adding item to
 *                        TCompilerID enumeration.
 * v1.4 of 10 Jan 2009  - Added HasErrorsOrWarnings method to ICompiler.
 * v1.5 of 25 Jan 2009  - Corrected name of ICompilers.GetGylph method to
 *                        GetGlyph.
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
 * The Original Code is IntfCompilers.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit IntfCompilers;


interface


uses
  // Delphi
  Classes, Graphics;


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
    ciFPC                               // Free Pascal
  );

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
    function GetGlyph: TBitmap;
      {Returns reference to any 18x18 bitmap associated with the compiler.
        @return Reference to bitmap or nil if there is no associated bitmap.
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
  ICompilersEnum:
    Interface to object that enumerates compilers.
  }
  ICompilersEnum = interface(IInterface)
    ['{C0B3DA8E-38BB-49E9-9F25-0AA5BC494B6E}']
    function GetCurrent: ICompiler;
      {Gets reference to current compiler.
        @return Reference to compiler.
      }
    function MoveNext: Boolean;
      {Moves to next item in enumeration.
        @return True if there is a next item, False if beyond last item.
      }
    property Current: ICompiler read GetCurrent;
      {Reference to current compiler}
  end;

  {
  ICompilers:
    Interface implemented by an object tha maintans a list of all compilers
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
    function GetEnumerator: ICompilersEnum;
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

