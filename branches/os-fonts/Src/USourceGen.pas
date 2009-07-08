{
 * USourceGen.pas
 *
 * Implements a class that is used to generate Pascal source code containing
 * specified database snippets.
 *
 * Originally named UUnitGen.pas. Renamed as USourceGen.pas as at v0.4.
 *
 * v0.1 of 30 Jan 2005  - Original version named UUnitGen.pas.
 * v0.2 of 19 Feb 2005  - Refactoring:
 *                        - Added new ExtractPrototype function that splits out
 *                          routine prototype from routine source code.
 *                        - Converted code that generates unit to use a
 *                          TStrStreamWriter object to help with writing.
 * v0.3 of 17 Mar 2005  - Added code that can generate an include file for
 *                        specified routines.
 *                      - Added support for writing routine descriptions as
 *                        comments in different styles.
 *                      - Method used to record user-specified and required
 *                        routines has been generalised.
 *                      - Removed StringToFile helper routine - now use routine
 *                        in UUtils.
 * v0.4 of 20 Apr 2005  - Renamed unit from UUnitGen.pas to USourceGen.pas.
 *                      - Renamed TUnitGen class as TSourceGen.
 *                      - Renamed TUnitGen's GenerateFile and GenerateAsString
 *                        methods as UnitFile and UnitAsString in TSourceGen
 *                        respectively.
 * v0.5 of 06 Jan 2006  - Changed comments written to include-style files to
 *                        make sense both with saved and copied snippets.
 * v1.0 of 25 May 2006  - Improved and corrected comments.
 *                      - Localised source code comment text.
 * v1.1 of 29 Oct 2006  - Added new static UnitNameFromFileName and
 *                        IsFileNameValidUnitName methods.
 *                      - Fixed bug in UnitFile method that was ignoring
 *                        CommentStyle and always using "no comment" style.
 *                      - Added facility to specify header comments for
 *                        generated units. Removed fixed header comments.
 *                      - Pulled TextWrap routine from containing routine and
 *                        made accessible by other code in unit.
 *                      - Made uses unit list word wrap.
 *                      - Made minor changes to comments used in .inc files.
 * v1.2 of 09 Feb 2007  - Added new TSourceGen.IncludeRoutines method to enable
 *                        lists of routines to be added to the source code.
 *                      - Extracted code that writes header comments into a
 *                        separate method.
 *                      - Modified to use user provided header comments instead
 *                        of hard wired title with generating include files.
 *                      - Replaced calls to objects' Free method with calls to
 *                        FreeAndNil.
 * v1.3 of 13 Sep 2008  - Modified to work with user defined routines: csAfter
 *                        comment style now replaced with csBefore for routines
 *                        that are not in standard format.
 * v1.4 of 03 Jan 2009  - Moved TextWrap routine to UUtils unit.
 * v1.5 of 13 Jan 2009  - Replaced control char literals with constants.
 *                      - Made private class section strict.
 *                      - Deleted unused method.
 *                      - Now use constants for line width and indent size.
 * v2.0 of 06 Jun 2009  - Added support for constants, types and freeform code
 *                        as well as routines to TSourceGen. TRoutine.Kind
 *                        property now used to get kind of snippet being output.
 *                      - Pulled out code from TSourceGen that analyses the
 *                        code to be output into new TSourceAnalyser class.
 *                      - Pulled out most formatting code from TSourceGen into
 *                        new classes that format consts and types and one that
 *                        formats routines. Old formatting routines removed.
 *                      - Added new TConstAndTypeList class to manage order that
 *                        consts and types are written to source code.
 *                      - Refactored some TSourceGen code and removed redundant
 *                        methods.
 *                      - Renamed TSourceGen's IncludeRoutine & IncludeRoutines
 *                        methods as IncludeSnippets & IncludeSnippets.
 *                      - Added support for calling conventions and overload
 *                        directives in routine prototypes.
 * v2.1 of 08 Jul 2009  - Fixed bug where units required by constants and type
 *                        definitions were not being added to generated units.
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
 * The Original Code is USourceGen.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit USourceGen;


interface


uses
  // Delphi
  Classes, Contnrs,
  // Project
  USnippets, UStrStreamWriter;


type

  {
  TCommentStyle:
    Different styles of commenting used when documenting snippets from database.
  }
  TCommentStyle = (
    csNone,     // no documentation of snippets
    csAfter,    // description of snippet between prototype and body
    csBefore    // description of snippet immediatly preceeds code
  );

  {
  TConstAndTypeList:
    Maintains a list of constant and type snippets and maintains order to
    account for dependencies.
  }
  TConstAndTypeList = class(TObject)
  strict private
    type
      {
      TUnitRecorder:
        Method that is called to record the units required by a constant or
        type.
          @param Units [in] String list containing list of units.
      }
      TUnitRecorder = procedure(const Units: TStringList) of object;
      {
      TEnumerator:
        Enumerator for snippets in TConstAndTypeList.
      }
      TEnumerator = class(TObject)
      strict private
        fList: TConstAndTypeList; // Reference to object being enumerated
        fIndex: Integer;          // Index of current item in enumeration
        function GetCurrent: TRoutine;
          {Gets current snippet in enumeration. Error if at end of enumeration.
            @return Current snippet.
          }
      public
        constructor Create(const CTL: TConstAndTypeList);
          {Class constructor. Sets up and initialises enumeration.
            @param CTL [in] Reference to object to be enumerated.
          }
        function MoveNext: Boolean;
          {Moves to next item in enumeration.
            @return True if there is a next item, False if enumeration
              completed.
          }
        property Current: TRoutine read GetCurrent;
          {Current item in enumeration. Error if at end of enumeration}
      end;
    var
      fItems: TObjectList;    // records objects in list
    function GetCount: Integer;
      {Read accessor for Count property.
        @return Number of items in list.
      }
    function GetItem(Idx: Integer): TRoutine;
      {Read access for Items[] property.
        @param Idx [in] Index of required snippet in list.
        @return Indexed snippet.
      }
    function Contains(const ConstOrType: TRoutine): Boolean;
      {Checks if the list contains a constant or type snippet.
        @param ConstOrType [in] Constant or type snippet being checked for.
        @return True if snippet in list, False if not.
      }
  public
    constructor Create;
      {Class constructor. Sets up list.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    procedure Add(const ConstOrType: TRoutine; const UnitRecorder: TUnitRecorder);
      {Adds a constant or type snippet to the list, ignoring duplicates.
        @param ConstOrType [in] Constant or type snippet to be added.
        @except Exception raised if dependency list is not valid.
      }
    function GetEnumerator: TEnumerator;
      {Gets an intialised const and type list enumerator.
        @return Required enumerator.
      }
    property Items[Idx: Integer]: TRoutine read GetItem; default;
      {Array of items in list, in dependency oder}
    property Count: Integer read GetCount;
      {Number of items in list}
  end;

  {
  TSourceAnalyser:
    Class that receives snippets for which source is to be generated and
    analyses relationships, pulling in any required snippets. Creates data
    structures that can be used to emit source code with all dependencies
    resolved.
  }
  TSourceAnalyser = class(TObject)
  strict private
    var
      fTypesAndConsts: TConstAndTypeList; // Value of TypesAndConsts property
      fIntfRoutines: TRoutineList;        // Value of IntfRoutines property
      fAllRoutines: TRoutineList;         // Value of AllRoutines property
      fForwardRoutines: TRoutineList;     // Value of ForwardRoutines property
      fRequiredRoutines: TRoutineList;    // Value of RequiredRoutines property
      fUnits: TStringList;                // Value of Units property
    procedure AddIntfRoutine(const Routine: TRoutine);
      {Adds a user-specified routine to list of routines specified by user.
      Duplicates ignored.
        @param Routine [in] Routine to be added.
      }
    procedure AddTypeOrConst(const TypeOrConst: TRoutine);
      {Adds a user specified or required type or constant to the analysis.
        @param TypeOrConst [in] Type of constant snippet to be added.
      }
    procedure RequireSnippets(const Snips: TRoutineList);
      {Process all snippets in a dependency list.
        @param Snips [in] List of snippets to process.
      }
    procedure RequireSnippet(const Snippet: TRoutine);
      {Process a snippet from a dependency list.
        @param Snippet [in] Snippet to be processed.
        @except Exception raised if attempt is made to require freeform snippet
      }
    procedure RequireUnits(const Units: TStringList);
      {Adds a list of units to required units list. Duplicates ignored.
        @param Units [in] List of units.
      }
    procedure RequireUnit(const UnitName: string);
      {Add a unit to list of required units. Duplicates ignored.
        @param UnitName [in] Name of required unit.
      }
    procedure RequireRoutine(const Routine: TRoutine);
      {Adds a routine from a dependency list to the analysis. Duplicates
      ignored.
        @param Routine [in] Routine to be added.
      }
  public
    constructor Create;
      {Class constructor. Sets up object.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    procedure AddSnippet(const Snippet: TRoutine);
      {Adds a user-specified snippet to the analysis. Freeform snippets are
      ignored.
        @param Snippet [in] Snippet to be added.
      }
    procedure Generate;
      {Generates the analysis.
      }
    property TypesAndConsts: TConstAndTypeList read fTypesAndConsts;
      {List of both added and required Types and constants, in required order}
    property IntfRoutines: TRoutineList read fIntfRoutines;
      {List of routines added by user. These routines would appear in a unit's
      interface section}
    property RequiredRoutines: TRoutineList read fRequiredRoutines;
      {List of routines required by other snippets, i.e. that appear in
      dependency lists}
    property AllRoutines: TRoutineList read fAllRoutines;
      {List of all routines, both added and required. Not valid until Generate
      method called. Invalidated if further snippets added}
    property ForwardRoutines: TRoutineList read fForwardRoutines;
      {List of required routines that are not also specified by user. These
      routines would appear in forward section of a unit. Not valid until
      Generate method called. Invalidated if further snippets added}
    property Units: TStringList read fUnits;
      {List of units required by any snippet}
  end;

  {
  TSourceGen:
    Generates Pascal source containing specified snippets and any other snippets
    that are required.
  }
  TSourceGen = class(TObject)
  strict private
    var
      fSourceAnalyser: TSourceAnalyser;
        {Object used to analyse the snippets and take care of dependencies}
    procedure WriteHeaderComments(const Writer: TStrStreamWriter;
      const HeaderComments: TStrings);
      {Writes a multi-line comment containing header text.
        @param Writer [in] Object used to output comments.
        @param HeaderComments [in] Header comments to be written.
      }
  public
    constructor Create;
      {Class constructor. Sets up the object.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    procedure IncludeSnippet(const Snippet: TRoutine);
      {Includes a snippet in the source code.
        @param Routine [in] Snippet to be included.
      }
    procedure IncludeSnippets(const Snips: TRoutineList);
      {Includes a one or more snippes in the source code.
        @param Routines [in] List of snippets to be included.
      }
    function UnitAsString(const UnitName: string;
      const CommentStyle: TCommentStyle = csNone;
      const HeaderComments: TStrings = nil): string;
      {Generates source code of a unit containing all specified snippets and
      any additional snippets depended upon by the included snippets.
        @param UnitName [in] Name of unit.
        @param CommentStyle [in] Style of commenting used in documenting
          snippets.
        @param HeaderComments [in] List of comments to be included at top of
          unit.
        @return Unit source code.
      }
    function IncFileAsString(const CommentStyle: TCommentStyle = csNone;
      const HeaderComments: TStrings = nil): string;
      {Generates source code of an include file containing all specified
      routines and notes in comments which units, types, consts and other
      routines are also required.
        @param CommentStyle [in] Style of commenting used in documenting
          snippets.
        @param HeaderComments [in] List of comments to be included at top of
          snippet.
        @return Source code of include file.
      }
    class function UnitNameFromFileName(const FileName: string): string;
      {Creates a unit name from a file name. The unit name is the base file name
      with any extension removed.
        @param FileName [in] Name of file.
        @return Unit name.
      }
    class function IsFileNameValidUnitName(const FileName: string): Boolean;
      {Checks if a file name is valid as basis for a unit name.
        @param FileName [in] Name of file to be checked.
        @return True if file name is valid for unit name, false if not.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, StrUtils,
  // Project
  UConsts, UExceptions, UIStringList, USnippetValidator, UUtils;


const
  cLineWidth = 80;              // max characters on line
  cIndent = 2;                  // indent size
  cDoubleIndent = 2 * cIndent;  // double indent size


type

  {
  TRoutineFormatter:
    Class that can format a routine to include descriptive comments at required
    position.
  }
  TRoutineFormatter = class(TObject)
  strict private
    class procedure Split(const Routine: TRoutine; out Head, Body: string);
      {Splits source code of a routine into the head (routine prototype) and
      body code.
        @param Routine [in] Routine whose source code to be split.
        @param Head [out] Routine prototype.
        @param Body [out] Remainder of routine without prototype.
      }
    class function RenderDescComment(CommentStyle: TCommentStyle;
      const Routine: TRoutine): string;
      {Creates comment in required style that contains routine's description.
        @param CommentStyle [in] Required commenting style.
        @param Routine [in] Routine for which comments required.
        @return Formatted comments.
      }
  public
    class function ExtractPrototype(const Routine: TRoutine): string;
      {Extracts a routine's prototype from source code.
        @param Routine [in] Routine whose source code to be processed.
        @return Routine prototype.
      }
    class function FormatRoutinePrototype(const Routine: TRoutine;
      CommentStyle: TCommentStyle = csNone): string;
      {Formats a routine's prototype, documented by the routine's description in
      a comment.
        @param Routine [in] Routine whose prototype is to be formatted.
        @param CommentStyle [in] Style of commenting used in documenting
          routine.
        @return Formatted prototype.
      }
    class function FormatRoutine(CommentStyle: TCommentStyle;
      const Routine: TRoutine): string;
      {Formats a routine's whole source code, documented by the routine's
      description in a comment.
        @param Routine [in] Routine whose source code is to be formatted.
        @param CommentStyle [in] Style of commenting used in documenting
          routine.
        @return Formatted prototype.
      }
  end;

  {
  TConstAndTypeFormatter:
    Class that can format a constant or type definition to include descriptive
    comments at required position.
  }
  TConstAndTypeFormatter = class(TObject)
  strict private
    class procedure Split(const ConstOrType: TRoutine; out Keyword,
      Body: string);
      {Splits source code of a type or constant into the keyword ("const" or
      "type") and definition itself (body code).
        @param ConstOrType [in] Constant or type whose source code to be split.
        @param Keyword [out] "const" or "type" keyword.
        @param Body [out] Remainder of constant or type without keyword.
      }
    class function RenderDescComment(CommentStyle: TCommentStyle;
      const ConstOrType: TRoutine): string;
      {Creates comment in required style that contains constant or type's
      description.
        @param CommentStyle [in] Required commenting style.
        @param ConstOrType [in] Constant or type for which comments required.
        @return Formatted comments.
      }
  public
    class function FormatConstOrType(CommentStyle: TCommentStyle;
      const ConstOrType: TRoutine): string;
      {Formats a constant or type's source code, documented by the snippet's
      description in a comment.
        @param ConstOrType [in] Constant or type whose source code is to be
          formatted.
        @param CommentStyle [in] Style of commenting used in documenting
          constant or type.
        @return Formatted prototype.
      }
  end;

{ TSourceGen }

constructor TSourceGen.Create;
  {Class constructor. Sets up the object.
  }
begin
  inherited;
  fSourceAnalyser := TSourceAnalyser.Create;
end;

destructor TSourceGen.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fSourceAnalyser);
  inherited;
end;

function TSourceGen.IncFileAsString(const CommentStyle: TCommentStyle;
  const HeaderComments: TStrings): string;
  {Generates source code of an include file containing all specified routines
  and notes in comments which units, types, consts and other routines are also
  required.
    @param CommentStyle [in] Style of commenting used in documenting
      snippets.
    @param HeaderComments [in] List of comments to be included at top of
      snippet.
    @return Source code of include file.
  }
resourcestring
  // Body comment text
  sReqUnits           = 'Required units(s):';
  sReqRoutines        = 'Additional required routine(s) from the database:';
  sReqConstsAndTypes  = 'Required consts / types';
  sXRefRoutines       = 'Cross referenced routine(s):';
var
  Idx: Integer;             // loops thru snippets list
  SS: TStringStream;        // string stream used to build unit output
  Writer: TStrStreamWriter; // helper object used to write text to stream
  ForwardWritten: Boolean;  // flag true if forward decls have been written
  FirstForward: Boolean;    // flag true when first forward decl to be written
  Snippet: TRoutine;        // accesses various snippet objects
  UnitName: string;         // accesses unit names from a list
begin
  // Generate the unit data
  fSourceAnalyser.Generate;

  // Create writer onto string stream to receive source code
  Writer := nil;
  SS := TStringStream.Create('');
  try
    Writer := TStrStreamWriter.Create(SS);

    // Write header comment
    WriteHeaderComments(Writer, HeaderComments);

    // Write required units, additional routines, types and consts
    if (fSourceAnalyser.Units.Count > 0) or
      (fSourceAnalyser.ForwardRoutines.Count > 0) or
      (fSourceAnalyser.TypesAndConsts.Count > 0) then
    begin
      Writer.WriteStrLn('{');
      if fSourceAnalyser.Units.Count > 0 then
      begin
        // list of required units
        Writer.WriteStrLn('  ' + sReqUnits);
        for UnitName in fSourceAnalyser.Units do
          Writer.WriteStrLn('    ' + UnitName);
      end;
      if fSourceAnalyser.TypesAndConsts.Count > 0 then
      begin
        // list of types and consts
        if (fSourceAnalyser.Units.Count > 0) then
          Writer.WriteStrLn;
        Writer.WriteStrLn('  ' + sReqConstsAndTypes);
        for Snippet in fSourceAnalyser.TypesAndConsts do
          Writer.WriteStrLn('    ' + Snippet.Name);
      end;
      if fSourceAnalyser.ForwardRoutines.Count > 0 then
      begin
        // list of other routines required to compile
        if (fSourceAnalyser.Units.Count > 0) or
          (fSourceAnalyser.TypesAndConsts.Count > 0) then
          Writer.WriteStrLn;
        Writer.WriteStrLn('  ' + sReqRoutines);
        for Snippet in fSourceAnalyser.ForwardRoutines do
          Writer.WriteStrLn('    ' + Snippet.Name);
      end;
      Writer.WriteStrLn('}');
      Writer.WriteStrLn;
    end;

    // Write out forward declarations for included routines required by others
    FirstForward := True;
    ForwardWritten := False;
    for Snippet in fSourceAnalyser.IntfRoutines do
    begin
      if fSourceAnalyser.RequiredRoutines.Contains(Snippet) then
      begin
        if FirstForward then
        begin
          Writer.WriteStrLn('// ' + sXRefRoutines);
          FirstForward := False;
        end;
        Writer.WriteStrLn(TRoutineFormatter.FormatRoutinePrototype(Snippet));
        Writer.WriteStrLn('  forward;');
        ForwardWritten := True;
      end;
    end;
    if ForwardWritten then
      Writer.WriteStrLn;

    // Write routines
    for Idx := 0 to Pred(fSourceAnalyser.IntfRoutines.Count) do
    begin
      Snippet := fSourceAnalyser.IntfRoutines[Idx];
      Writer.WriteStrLn(
        TRoutineFormatter.FormatRoutine(CommentStyle, Snippet)
      );
      if Idx < Pred(fSourceAnalyser.IntfRoutines.Count) then
        Writer.WriteStrLn;
    end;

    // Return string containing source code
    Result := SS.DataString;
  finally
    FreeAndNil(Writer);
    FreeAndNil(SS);
  end;
end;

procedure TSourceGen.IncludeSnippet(const Snippet: TRoutine);
  {Includes a snippet in the source code.
    @param Snippet [in] Snippet to be included.
  }
begin
  fSourceAnalyser.AddSnippet(Snippet);
end;

procedure TSourceGen.IncludeSnippets(const Snips: TRoutineList);
  {Includes a one or more snippes in the source code.
    @param Routines [in] List of snippets to be included.
  }
var
  Snippet: TRoutine;  // iterates through snippets to be added
begin
  for Snippet in Snips do
    IncludeSnippet(Snippet);
end;

class function TSourceGen.IsFileNameValidUnitName(
  const FileName: string): Boolean;
  {Checks if a file name is valid as basis for a unit name.
    @param FileName [in] Name of file to be checked.
    @return True if file name is valid for unit name, false if not.
  }
begin
  Result := IsValidIdent(UnitNameFromFileName(FileName));
end;

function TSourceGen.UnitAsString(const UnitName: string;
  const CommentStyle: TCommentStyle = csNone;
  const HeaderComments: TStrings = nil): string;
  {Generates source code of a unit containing all specified routines and
  routines depended upon by the included routines.
    @param UnitName [in] Name of unit.
    @param CommentStyle [in] Style of commenting used in documenting routines.
    @param HeaderComments [in] List of comments to be included at top of unit.
    @return Unit source code.
  }
var
  SS: TStringStream;        // string stream used to build unit output
  Writer: TStrStreamWriter; // helper object used to write text to stream
  Snippet: TRoutine;        // reference to a snippet object
begin
  // Generate the unit data
  fSourceAnalyser.Generate;
  // Create writer object onto string stream that receives output
  Writer := nil;
  SS := TStringStream.Create('');
  try
    Writer := TStrStreamWriter.Create(SS);

    // Write unit
    // heading comment
    WriteHeaderComments(Writer, HeaderComments);

    // unit name
    Writer.WriteStrLn('unit %s;', [UnitName]);
    Writer.WriteStrLn;

    // open interface section
    Writer.WriteStrLn('interface');
    Writer.WriteStrLn;

    // uses statement
    if fSourceAnalyser.Units.Count > 0 then
    begin
      Writer.WriteStrLn('uses');
      Writer.WriteStrLn(
        TextWrap(
          JoinStr(fSourceAnalyser.Units, ', ') + ';',
          cLineWidth - cIndent,
          cIndent
        )
      );
      Writer.WriteStrLn;
    end;

    // consts and types
    for Snippet in fSourceAnalyser.TypesAndConsts do
    begin
      Writer.WriteStrLn(
        TConstAndTypeFormatter.FormatConstOrType(CommentStyle, Snippet)
      );
      Writer.WriteStrLn;
    end;

    // routine prototypes
    for Snippet in fSourceAnalyser.IntfRoutines do
    begin
      Writer.WriteStrLn(
        TRoutineFormatter.FormatRoutinePrototype(Snippet, CommentStyle)
      );
      Writer.WriteStrLn;
    end;

    // open implementation section
    Writer.WriteStrLn('implementation');
    Writer.WriteStrLn;

    // forward declarations
    if fSourceAnalyser.ForwardRoutines.Count > 0 then
    begin
      for Snippet in fSourceAnalyser.ForwardRoutines do
      begin
        Writer.WriteStrLn(TRoutineFormatter.ExtractPrototype(Snippet));
        Writer.WriteStrLn('  forward;');
      end;
      Writer.WriteStrLn;
    end;

    // routine source code
    for Snippet in fSourceAnalyser.AllRoutines do
    begin
      Writer.WriteStrLn(TRoutineFormatter.FormatRoutine(CommentStyle, Snippet));
      Writer.WriteStrLn;
    end;

    // close unit
    Writer.WriteStrLn('end.');

    // Return string built in string stream
    Result := SS.DataString;
  finally
    FreeAndNil(Writer);
    FreeAndNil(SS);
  end;
end;

class function TSourceGen.UnitNameFromFileName(const FileName: string): string;
  {Creates a unit name from a file name. The unit name is the base file name
  with any extension removed.
    @param FileName [in] Name of file.
    @return Unit name.
  }
var
  BaseFileName: string; // base file name (i.e. file name without path)
  Ext: string;          // file's extension
begin
  BaseFileName := ExtractFileName(FileName);
  Ext := ExtractFileExt(FileName);
  Result := AnsiLeftStr(BaseFileName, Length(BaseFileName) - Length(Ext));
end;

procedure TSourceGen.WriteHeaderComments(const Writer: TStrStreamWriter;
  const HeaderComments: TStrings);
  {Writes a multi-line comment containing header text.
    @param Writer [in] Object used to output comments.
    @param HeaderComments [in] Header comments to be written.
  }
var
  Line: string; // loops through each line of comments
begin
  // Only write comment if some comment text is provided
  if Assigned(HeaderComments) and (HeaderComments.Count > 0) then
  begin
    Writer.WriteStrLn('{');
    for Line in HeaderComments do
      Writer.WriteStrLn(
        TextWrap(Line, cLineWidth - cIndent, cIndent)
      );
    Writer.WriteStrLn('}');
    Writer.WriteStrLn;
  end;
end;

{ TSourceAnalyser }

procedure TSourceAnalyser.AddIntfRoutine(const Routine: TRoutine);
  {Adds a user-specified routine to list of routines specified by user.
  Duplicates ignored.
    @param Routine [in] Routine to be added.
  }
begin
  Assert(Routine.Kind = skRoutine,
    ClassName + '.AddIntfRoutine: Routine must have kind skRoutine');
  if not fIntfRoutines.Contains(Routine) then
  begin
    fIntfRoutines.Add(Routine);         // add to user-specified list
    RequireUnits(Routine.Units);        // add all routine's required units
    RequireSnippets(Routine.Depends);   // add all routine's required snippets
  end;
end;

procedure TSourceAnalyser.AddSnippet(const Snippet: TRoutine);
  {Adds a user-specified snippet to the analysis. Freeform snippets are ignored.
    @param Snippet [in] Snippet to be added.
  }
var
  ErrorMsg: string; // any error message
begin
  // NOTE: this method must not be called from any other method
  // Validate the snippet
  if not TSnippetValidator.Validate(Snippet, ErrorMsg) then
    raise ECodeSnip.Create(ErrorMsg);
  // Process the snippet
  case Snippet.Kind of
    skRoutine:
      AddIntfRoutine(Snippet);
    skTypeDef, skConstant:
      AddTypeOrConst(Snippet);
    skFreeform:
      {Ignore};
  end;
end;

procedure TSourceAnalyser.AddTypeOrConst(const TypeOrConst: TRoutine);
  {Adds a user specified or required type or constant to the analysis.
    @param TypeOrConst [in] Type of constant snippet to be added.
  }
begin
  fTypesAndConsts.Add(TypeOrConst, RequireUnits);
end;

constructor TSourceAnalyser.Create;
  {Class constructor. Sets up object.
  }
begin
  inherited;
  fTypesAndConsts := TConstAndTypeList.Create;
  fIntfRoutines := TRoutineList.Create;
  fAllRoutines := TRoutineList.Create;
  fForwardRoutines := TRoutineList.Create;
  fRequiredRoutines := TRoutineList.Create;
  fUnits := TStringList.Create;
end;

destructor TSourceAnalyser.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fTypesAndConsts);
  FreeAndNil(fIntfRoutines);
  FreeAndNil(fAllRoutines);
  FreeAndNil(fForwardRoutines);
  FreeAndNil(fRequiredRoutines);
  FreeAndNil(fUnits);
  inherited;
end;

procedure TSourceAnalyser.Generate;
  {Generates the analysis.
  }
var
  Routine: TRoutine;  // iterates through various routine lists
begin
  fForwardRoutines.Clear;
  fAllRoutines.Clear;
  // Build forward routines list
  for Routine in fRequiredRoutines do
    if not fIntfRoutines.Contains(Routine)
      and not fForwardRoutines.Contains(Routine) then
      fForwardRoutines.Add(Routine);
  // Build all routines list
  for Routine in fIntfRoutines do
    fAllRoutines.Add(Routine);
  for Routine in fForwardRoutines do
    fAllRoutines.Add(Routine);
end;

procedure TSourceAnalyser.RequireRoutine(const Routine: TRoutine);
  {Adds a routine from a dependency list to the analysis. Duplicates ignored.
    @param Routine [in] Routine to be added.
  }
begin
  if not fRequiredRoutines.Contains(Routine) then
  begin
    fRequiredRoutines.Add(Routine);   // add routine to required list
    RequireUnits(Routine.Units);      // add all routine's required unit
    RequireSnippets(Routine.Depends); // require all snippets in depends list
  end;
end;

procedure TSourceAnalyser.RequireSnippet(const Snippet: TRoutine);
  {Process a snippet from a dependency list.
    @param Snippet [in] Snippet to be processed.
    @except Exception raised if attempt is made to require freeform snippet
  }
resourcestring
  // Error message
  sCantDependOnFreeform = 'Can''t depend on "%s" - it is freeform code';
begin
  case Snippet.Kind of
    skRoutine:                    // require routine
      RequireRoutine(Snippet);
    skConstant, skTypeDef:        // add type or const allowing for dependencies
      AddTypeOrConst(Snippet);
    skFreeform:                   // can't require a freeform snippet
      raise ECodeSnip.CreateFmt(sCantDependOnFreeform, [Snippet.Name]);
  end;
end;

procedure TSourceAnalyser.RequireSnippets(const Snips: TRoutineList);
  {Process all snippets in a dependency list.
    @param Snips [in] List of snippets to process.
  }
var
  Snippet: TRoutine;  // iterates through snippets list
begin
  for Snippet in Snips do
    RequireSnippet(Snippet);
end;

procedure TSourceAnalyser.RequireUnit(const UnitName: string);
  {Add a unit to list of required units. Duplicates ignored.
    @param UnitName [in] Name of required unit.
  }
begin
  if fUnits.IndexOf(UnitName) = -1 then
    fUnits.Add(UnitName);
end;

procedure TSourceAnalyser.RequireUnits(const Units: TStringList);
  {Adds a list of units to required units list. Duplicates ignored.
    @param Units [in] List of units.
  }
var
  UnitName: string; // iterates through list of units.
begin
  for UnitName in Units do
    RequireUnit(UnitName);
end;

{ TConstAndTypeList }

procedure TConstAndTypeList.Add(const ConstOrType: TRoutine; const UnitRecorder: TUnitRecorder);
  {Adds a constant or type snippet to the list, ignoring duplicates.
    @param ConstOrType [in] Constant or type snippet to be added.
    @except Exception raised if dependency list is not valid.
  }
var
  RequiredSnip: TRoutine; // reference snippets in depends list
  ErrorMsg: string;       // any error message
begin
  Assert(Assigned(ConstOrType), ClassName + '.Add: ConstOrType in nil');
  Assert(ConstOrType.Kind in [skTypeDef, skConstant],
    ClassName + '.Add: ConstOrType must have kind skTypeDef or skConstant');
  // Ignore if already in list
  if Contains(ConstOrType) then
    Exit;
  // Validate dependency list
  if not TSnippetValidator.HasValidDependsList(ConstOrType, ErrorMsg) then
    raise ECodeSnip.Create(ErrorMsg);
  // Add all required snippets to list before adding this one: this ensures
  // required snippets preceed those that depend on them
  for RequiredSnip in ConstOrType.Depends do
    Add(RequiredSnip, UnitRecorder);
  UnitRecorder(ConstOrType.Units);
  fItems.Add(ConstOrType)
end;

function TConstAndTypeList.Contains(const ConstOrType: TRoutine): Boolean;
  {Checks if the list contains a constant or type snippet.
    @param ConstOrType [in] Constant or type snippet being checked for.
    @return True if snippet in list, False if not.
  }
begin
  Result := fItems.IndexOf(ConstOrType) >= 0;
end;

constructor TConstAndTypeList.Create;
  {Class constructor. Sets up list.
  }
begin
  inherited;
  fItems := TObjectList.Create(False);
end;

destructor TConstAndTypeList.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fItems);
  inherited;
end;

function TConstAndTypeList.GetCount: Integer;
  {Read accessor for Count property.
    @return Number of items in list.
  }
begin
  Result := fItems.Count;
end;

function TConstAndTypeList.GetEnumerator: TEnumerator;
  {Gets an intialised const and type list enumerator.
    @return Required enumerator.
  }
begin
  Result := TEnumerator.Create(Self);
end;

function TConstAndTypeList.GetItem(Idx: Integer): TRoutine;
  {Read access for Items[] property.
    @param Idx [in] Index of required snippet in list.
    @return Indexed snippet.
  }
begin
  Result := fItems[Idx] as TRoutine;
end;

{ TConstAndTypeList.TEnumerator }

constructor TConstAndTypeList.TEnumerator.Create(const CTL: TConstAndTypeList);
  {Class constructor. Sets up and initialises enumeration.
    @param CTL [in] Reference to object to be enumerated.
  }
begin
  inherited Create;
  fList := CTL;
  fIndex := -1;
end;

function TConstAndTypeList.TEnumerator.GetCurrent: TRoutine;
  {Gets current snippet in enumeration. Error if at end of enumeration.
    @return Current snippet.
  }
begin
  Result := fList[fIndex];
end;

function TConstAndTypeList.TEnumerator.MoveNext: Boolean;
  {Moves to next item in enumeration.
    @return True if there is a next item, False if enumeration
      completed.
  }
begin
  Result := fIndex < Pred(fList.Count);
  if Result then
    Inc(fIndex);
end;

{ TRoutineFormatter }

class function TRoutineFormatter.ExtractPrototype(const
  Routine: TRoutine): string;
  {Extracts a routine's prototype from source code.
    @param Routine [in] Routine whose source code to be processed.
    @return Routine prototype.
  }
var
  DummyBody: string;  // stores unused routine body retrieved from Split
begin
  Split(Routine, Result, DummyBody);
end;

class function TRoutineFormatter.FormatRoutine(
  CommentStyle: TCommentStyle; const Routine: TRoutine): string;
  {Formats a routine's whole source code, documented by the routine's
  description in a comment.
    @param Routine [in] Routine whose source code is to be formatted.
    @param CommentStyle [in] Style of commenting used in documenting routine.
    @return Formatted prototype.
  }
var
  Prototype, Body: string;  // prototype and body of routine
begin
  Assert(Routine.Kind = skRoutine,
    ClassName + '.FormatRoutine: Routine must have kind skRoutine');
  case CommentStyle of
    csAfter:
    begin
      // Format is: routine prototype - comment - routine body
      Split(Routine, Prototype, Body);
      Result := Trim(Prototype) + EOL +
        RenderDescComment(CommentStyle, Routine) + EOL +
        Trim(Body);
    end;
    csBefore:
      // Format is: comment - routine
      Result := RenderDescComment(CommentStyle, Routine) + EOL +
        Trim(Routine.SourceCode);
    else
      // No commenting: just return source code
      Result := Trim(Routine.SourceCode);
  end;
end;

class function TRoutineFormatter.FormatRoutinePrototype(const Routine: TRoutine;
  CommentStyle: TCommentStyle): string;
  {Formats a routine's prototype, documented by the routine's description in a
  comment.
    @param Routine [in] Routine whose prototype is to be formatted.
    @param CommentStyle [in] Style of commenting used in documenting routine.
    @return Formatted prototype.
  }
var
  Prototype: string;  // prototype of given routine
begin
  Assert(Routine.Kind = skRoutine,
    ClassName + '.FormatRoutinePrototype: Routine must have kind skRoutine');
  // Get prototype
  Prototype := ExtractPrototype(Routine);
  // Write comment depending on style
  case CommentStyle of
    csAfter:
      // comments follow prototype
      Result := Trim(Prototype) + EOL +
        RenderDescComment(CommentStyle, Routine);
    csBefore:
      // comments preceed prototype
      Result := RenderDescComment(CommentStyle, Routine) + EOL +
        Trim(Prototype);
    else
      // no comments: just return prototype
      Result := Trim(Prototype);
  end;
end;

class function TRoutineFormatter.RenderDescComment(
  CommentStyle: TCommentStyle; const Routine: TRoutine): string;
  {Creates comment in required style that contains routine's description.
    @param CommentStyle [in] Required commenting style.
    @param Routine [in] Routine for which comments required.
    @return Formatted comments.
  }
begin
  Assert(Routine.Kind = skRoutine,
    ClassName + '.RenderDescComment: Routine must have kind skRoutine');
  // Format the output
  case CommentStyle of
    csAfter:
      // Comment after prototype
      Result := TextWrap(
        '{' + Trim(Routine.Description) + '}',
        cLineWidth - cIndent,
        cIndent
      );
    csBefore:
      // Comment before routine
      Result := '{' + EOL
        + '  ' + Routine.Name + EOL
        + TextWrap(
            Trim(Routine.Description),
            cLineWidth - cDoubleIndent,
            cDoubleIndent
          ) + EOL
        + '}';
    else
      // No description comment required
      Result := '';
  end;
end;

class procedure TRoutineFormatter.Split(const Routine: TRoutine; out Head,
  Body: string);
  {Splits source code of a routine into the head (routine prototype) and body
  code.
    @param Routine [in] Routine whose source code to be split.
    @param Head [out] Routine prototype.
    @param Body [out] Remainder of routine without prototype.
  }

  // ---------------------------------------------------------------------------
  function IsDirective(const Symbol: string): Boolean;
    {Checks if a symbol is a calling convention directive.
      @param Symbol [in] Symbol to be checked.
      @return True if symbol is a calling convention, False otherwise.
    }
  const
    // list of calling convention directives
    cCallConventions: array[0..4] of string = (
      'register', 'pascal', 'cdecl', 'stdcall', 'safecall'
    );
  var
    ConventionList: IStringList;  // list of calling conventions
  begin
    ConventionList := TIStringList.Create(cCallConventions);
    ConventionList.CaseSensitive := False;
    Result := ConventionList.Contains(Symbol);
  end;
  // ---------------------------------------------------------------------------

var
  SourceCode: string;         // routine's source code
  StartParam: Integer;        // possible position of start of any parameters
  AfterParams: Integer;       // possible position of end of any parameters
  EndDeclaration: Integer;    // position of end of routine declaration
  StartCodeBody: Integer;     // position of start of body of routine
  SemiColonPos: Integer;      // position of a semi colon
  Fragment: string;           // a fragment of code
const
  cOverload = 'overload';     // overload directive
begin
  // Record code without any surrounding white space
  SourceCode := Trim(Routine.SourceCode);
  // Find relative positions of first key characters
  StartParam := PosEx('(', SourceCode);
  AfterParams := PosEx(')', SourceCode) + 1;
  SemiColonPos := PosEx(';', SourceCode);
  // Determine end of head section
  if SemiColonPos > StartParam then
  begin
    // semi colon after param => we have params: skip them before looking for
    // ending ';'
    EndDeclaration := PosEx(
      ';',
      Copy(SourceCode, AfterParams, Length(SourceCode) - AfterParams + 1)
    ) + AfterParams - 1;
  end
  else
  begin
    // semi colon before "params" => no params and ';' ends header
    EndDeclaration := SemiColonPos;
  end;
  // Look for directives that are part of prototype
  // first look for calling conventions
  SemiColonPos := PosEx(';', SourceCode, EndDeclaration + 1);
  if SemiColonPos > 0 then
  begin
    Fragment := Trim(
      Copy(SourceCode, EndDeclaration + 1, SemiColonPos - EndDeclaration - 1)
    );
    if IsDirective(Fragment) then
      EndDeclaration := SemiColonPos + 1;
  end;
  // now look for 'overload' directive
  SemiColonPos := PosEx(';', SourceCode, EndDeclaration + 1);
  if SemiColonPos > 0 then
  begin
    Fragment := Trim(
      Copy(SourceCode, EndDeclaration + 1, SemiColonPos - EndDeclaration - 1)
    );
    if AnsiLowerCase(Fragment) = cOverload then
      EndDeclaration := SemiColonPos + 1;
  end;
  // Record declaration (i.e. prototype)
  Head := Copy(SourceCode, 1, EndDeclaration);
  // Get code body
  StartCodeBody := EndDeclaration + 1;
  Body := Trim(Copy(SourceCode, StartCodeBody, MaxInt));
end;

{ TConstAndTypeFormatter }

class function TConstAndTypeFormatter.FormatConstOrType(
  CommentStyle: TCommentStyle; const ConstOrType: TRoutine): string;
  {Formats a constant or type's source code, documented by the snippet's
  description in a comment.
    @param ConstOrType [in] Constant or type whose source code is to be
      formatted.
    @param CommentStyle [in] Style of commenting used in documenting constant or
      type.
    @return Formatted prototype.
  }
var
  Keyword: string;  // keyword that preceeds source code body
  Body: string;     // source code that follows keyword
begin
  Assert(ConstOrType.Kind in [skConstant, skTypeDef],
    ClassName + '.FormatConstOrType: ConstOrType must have kind skTypeDef or '
    + 'skConstant');
  Result := Trim(ConstOrType.Name);
  case CommentStyle of
    csNone:
      Result := Trim(ConstOrType.SourceCode);
    csBefore:
      Result := RenderDescComment(CommentStyle, ConstOrType)
        + EOL
        + Trim(ConstOrtype.SourceCode);
    csAfter:
    begin
      Split(ConstOrType, Keyword, Body);
      if Keyword <> '' then
        Result := Keyword
          + EOL
          + RenderDescComment(CommentStyle, ConstOrType)
          + EOL
          + Body
      else
        Result := ConstOrType.SourceCode;
    end;
  end;
end;

class function TConstAndTypeFormatter.RenderDescComment(
  CommentStyle: TCommentStyle; const ConstOrType: TRoutine): string;
  {Creates comment in required style that contains constant or type's
  description.
    @param CommentStyle [in] Required commenting style.
    @param ConstOrType [in] Constant or type for which comments required.
    @return Formatted comments.
  }
begin
  Assert(ConstOrType.Kind in [skConstant, skTypeDef],
    ClassName + '.RenderDescComment: ConstOrType must have kind skTypeDef or '
      + 'skConstant');
  case CommentStyle of
    csNone:
      Result := '';
    csBefore:
      Result := '{'
        + EOL
        + TextWrap(ConstOrType.Description, cLineWidth - cIndent, cIndent)
        + EOL
        + '}';
    csAfter:
      Result := TextWrap(
        '{' + ConstOrType.Description + '}', cLineWidth - cIndent, cIndent
      );
  end;
end;

class procedure TConstAndTypeFormatter.Split(const ConstOrType: TRoutine;
  out Keyword, Body: string);
  {Splits source code of a type or constant into the keyword ("const" or
  "type") and definition itself (body code).
    @param ConstOrType [in] Constant or type whose source code to be split.
    @param Keyword [out] "const" or "type" keyword.
    @param Body [out] Remainder of constant or type without keyword.
  }

  // ---------------------------------------------------------------------------
  procedure SplitAtKeyword(const SourceCode, KW: string;
    out Keyword, Body: string);
    {Splits an introductory keyword from following source code.
      @param SourceCode [in] Source code to be split.
      @param KW [in] Introductory keyword.
      @param Keyword [out] Set to KW if KW is present, otherwise ''.
      @param Body [out] Source code that follows keyword if KW is present,
        otherwise set to SourceCode.
    }
  var
    EOLPos: Integer;  // position of end of line chars, if present after KW
  begin
    if AnsiStartsStr(KW, SourceCode) then
    begin
      // KW starts SourceCode - perform split
      Keyword := KW;
      EOLPos := PosEx(EOL, SourceCode, Length(KW));
      if EOLPos > 0 then
        Body := Copy(SourceCode, EOLPos + Length(EOL), MaxInt)
      else
        Body := '  ' + Trim(Copy(SourceCode, Length(KW) + 1, MaxInt));
    end
    else
    begin
      // KW not present - can't split
      Keyword := '';
      Body := SourceCode;
    end;
  end;
  // ---------------------------------------------------------------------------

begin
  if ConstOrType.Kind = skConstant then
    SplitAtKeyword(ConstOrType.SourceCode, 'const', Keyword, Body)
  else // if ConstOrType.Kind = skTypeDef
    SplitAtKeyword(ConstOrType.SourceCode, 'type', Keyword, Body)
end;

end.

