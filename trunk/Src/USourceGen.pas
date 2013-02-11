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
 * Implements a class that is used to generate Pascal source code containing
 * specified database snippets.
}


unit USourceGen;


interface


uses
  // Delphi
  Classes, Generics.Collections,
  // Project
  ActiveText.UMain, DB.USnippet, UBaseObjects, UIStringList;


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
  TSourceComments:
    Static class that provides information about comment styles and formats
    comments in appropriate style.
  }
  TSourceComments = class(TNoConstructObject)
  strict private
    class function FormatCommentLines(const Text: string;
      const Indent: Cardinal): string;
  public
    class function CommentStyleDesc(const Style: TCommentStyle): string;
      {Gets description of a comment style for use in UI elements.
        @param Style [in] Comment style for which description wanted.
        @return Required description.
      }
    class function FormatSnippetComment(const Style: TCommentStyle;
      const TruncateComments: Boolean; const Text: IActiveText): string;
      {Formats a snippet's comment text as Pascal comment according to
      commenting style.
        @param Style [in] Desired commenting style.
        @param TruncateComments [in] Whether comments are to be truncated to
          just first line of multi line snippet descriptions.
        @param Text [in] Active text of comment. Ignored if Style = csNone.
        @return Formatted comment. Empty string if Style = csNone.
      }
    class function FormatHeaderComments(const Comments: IStringList): string;
      {Formats header comment text as Pascal comments.
        @param Comments [in] List of comments to format.
        @return Formatted comments.
      }
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
      fTypesAndConsts: TObjectList<TSnippet>; // Value of TypesAndConsts prop
      fIntfRoutines: TSnippetList;        // Value of IntfRoutines property
      fAllRoutines: TSnippetList;         // Value of AllRoutines property
      fForwardRoutines: TSnippetList;     // Value of ForwardRoutines property
      fRequiredRoutines: TSnippetList;    // Value of RequiredRoutines property
      fUnits: TStringList;                // Value of Units property
    procedure AddIntfRoutine(const Routine: TSnippet);
      {Adds a user-specified routine to list of routines specified by user.
      Duplicates ignored.
        @param Routine [in] Routine to be added.
      }
    procedure AddTypeOrConst(const TypeOrConst: TSnippet);
      {Adds a user specified or required type or constant to the analysis.
        @param TypeOrConst [in] Type of constant snippet to be added.
      }
    procedure RequireSnippets(const Snips: TSnippetList);
      {Process all snippets in a dependency list.
        @param Snips [in] List of snippets to process.
      }
    procedure RequireSnippet(const Snippet: TSnippet);
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
    procedure RequireRoutine(const Routine: TSnippet);
      {Adds a routine from a dependency list to the analysis. Duplicates
      ignored.
        @param Routine [in] Routine to be added.
      }
  public
    constructor Create;
      {Constructor. Sets up object.
      }
    destructor Destroy; override;
      {Destructor. Tears down object.
      }
    procedure AddSnippet(const Snippet: TSnippet);
      {Adds a user-specified snippet to the analysis. Freeform snippets are
      ignored.
        @param Snippet [in] Snippet to be added.
      }
    procedure Generate;
      {Generates the analysis.
      }
    property TypesAndConsts: TObjectList<TSnippet> read fTypesAndConsts;
      {List of both added and required Types and constants, in required order}
    property IntfRoutines: TSnippetList read fIntfRoutines;
      {List of routines added by user. These routines would appear in a unit's
      interface section}
    property RequiredRoutines: TSnippetList read fRequiredRoutines;
      {List of routines required by other snippets, i.e. that appear in
      dependency lists}
    property AllRoutines: TSnippetList read fAllRoutines;
      {List of all routines, both added and required. Not valid until Generate
      method called. Invalidated if further snippets added}
    property ForwardRoutines: TSnippetList read fForwardRoutines;
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
      fSourceAnalyser: TSourceAnalyser; // Analyses snippets and dependencies
  public
    constructor Create;
      {Constructor. Sets up the object.
      }
    destructor Destroy; override;
      {Destructor. Tears down object.
      }
    procedure IncludeSnippet(const Snippet: TSnippet);
      {Includes a snippet in the source code.
        @param Routine [in] Snippet to be included.
      }
    procedure IncludeSnippets(const Snips: TSnippetList);
      {Includes a one or more snippets in the source code.
        @param Routines [in] List of snippets to be included.
      }
    function UnitAsString(const UnitName: string;
      const CommentStyle: TCommentStyle = csNone;
      const TruncateComments: Boolean = False;
      const HeaderComments: IStringList = nil): string;
      {Generates source code of a unit containing all specified snippets and
      any additional snippets depended upon by the included snippets.
        @param UnitName [in] Name of unit.
        @param CommentStyle [in] Style of commenting used in documenting
          snippets.
        @param TruncateComments [in] Whether comments are to be truncated to
          just first line of multi line snippet descriptions.
        @param HeaderComments [in] List of comments to be included at top of
          unit.
        @return Unit source code.
      }
    function IncFileAsString(const CommentStyle: TCommentStyle = csNone;
      const TruncateComments: Boolean = False;
      const HeaderComments: IStringList = nil): string;
      {Generates source code of an include file containing all specified
      routines and notes in comments which units, types, consts and other
      routines are also required.
        @param CommentStyle [in] Style of commenting used in documenting
          snippets.
        @param TruncateComments [in] Whether comments are to be truncated to
          just first line of multi line snippet descriptions.
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
  SysUtils,
  // Project
  ActiveText.UTextRenderer, DB.USnippetKind, UConsts, UExceptions, UPreferences,
  USnippetValidator, UStrUtils, UWarnings, Hiliter.UPasLexer;


const
  cLineWidth = 80;  // max characters on line
  cIndent = 2;      // indent size


type

  {
  TRoutineFormatter:
    Static class that can format a routine to include descriptive comments at
    required position.
  }
  TRoutineFormatter = class(TNoConstructObject)
  strict private
    class procedure Split(const Routine: TSnippet; out Head, Body: string);
      {Splits source code of a routine into the head (routine prototype) and
      body code.
        @param Routine [in] Routine whose source code to be split.
        @param Head [out] Routine prototype.
        @param Body [out] Remainder of routine without prototype.
      }
    class function RenderDescComment(CommentStyle: TCommentStyle;
      const TruncateComments: Boolean; const Routine: TSnippet): string;
      {Creates comment in required style that contains routine's description.
        @param CommentStyle [in] Required commenting style.
        @param TruncateComments [in] Whether comments are to be truncated to
          just first line of multi line snippet descriptions.
        @param Routine [in] Routine for which comments required.
        @return Formatted comments.
      }
  public
    class function ExtractPrototype(const Routine: TSnippet): string;
      {Extracts a routine's prototype from source code.
        @param Routine [in] Routine whose source code to be processed.
        @return Routine prototype.
      }
    class function FormatRoutinePrototype(const Routine: TSnippet;
      CommentStyle: TCommentStyle; const TruncateComments: Boolean): string;
      {Formats a routine's prototype, documented by the routine's description in
      a comment.
        @param Routine [in] Routine whose prototype is to be formatted.
        @param CommentStyle [in] Style of commenting used in documenting
          routine.
        @param TruncateComments [in] Whether comments are to be truncated to
          just first line of multi line snippet descriptions.
        @return Formatted prototype.
      }
    class function FormatRoutine(CommentStyle: TCommentStyle;
      const TruncateComments: Boolean; const Routine: TSnippet): string;
      {Formats a routine's whole source code, documented by the routine's
      description in a comment.
        @param CommentStyle [in] Style of commenting used in documenting
          routine.
        @param TruncateComments [in] Whether comments are to be truncated to
          just first line of multi line snippet descriptions.
        @param Routine [in] Routine whose source code is to be formatted.
        @return Formatted prototype.
      }
  end;

  {
  TConstAndTypeFormatter:
    Static class that can format a constant or type definition to include
    descriptive comments at required position.
  }
  TConstAndTypeFormatter = class(TNoConstructObject)
  strict private
    class procedure Split(const ConstOrType: TSnippet; out Prefix,
      Body: string);
      {Splits source code of a type or constant into the prefix (text up to
      "const" or "type") and definition itself (body code).
        @param ConstOrType [in] Constant or type whose source code to be split.
        @param Prefix [out] Text up to "const" or "type" keyword.
        @param Body [out] Remainder of constant or type without keyword.
      }
    class function RenderDescComment(CommentStyle: TCommentStyle;
      const TruncateComments: Boolean; const ConstOrType: TSnippet): string;
      {Creates comment in required style that contains constant or type's
      description.
        @param CommentStyle [in] Required commenting style.
        @param TruncateComments [in] Whether comments are to be truncated to
          just first line of multi line snippet descriptions.
        @param ConstOrType [in] Constant or type for which comments required.
        @return Formatted comments.
      }
  public
    class function FormatConstOrType(CommentStyle: TCommentStyle;
      const TruncateComments: Boolean; const ConstOrType: TSnippet): string;
      {Formats a constant or type's source code, documented by the snippet's
      description in a comment.
        @param CommentStyle [in] Style of commenting used in documenting
          constant or type.
        @param TruncateComments [in] Whether comments are to be truncated to
          just first line of multi line snippet descriptions.
        @param ConstOrType [in] Constant or type whose source code is to be
          formatted.
        @return Formatted prototype.
      }
  end;

  TClassFormatter = class(TNoConstructObject)
  strict private
    class function RenderDescComment(CommentStyle: TCommentStyle;
      const TruncateComments: Boolean; const Snippet: TSnippet): string;
      {Creates comment in required style that contains class' description.
        @param CommentStyle [in] Required commenting style.
        @param TruncateComments [in] Whether comments are to be truncated to
          just first line of multi line snippet descriptions.
        @param Snippet [in] Class for which comments required.
        @return Formatted comments.
      }
    class function RemoveKeywordFromDecl(const Decl: string;
      out DeclBody: string): Boolean;
      {Removes any introductory "type" keyword from a type declaration, if
      possible.
        @param Decl [in] Type declaration to be processed.
        @param DeclBody [out] Source code that follows "type" keyword if
          keyword is found, otherwise set to Decl.
        @returns True if successful, False if not.
      }
    class procedure SplitDeclFromDefn(const Source: string; out Decl,
      Defn: string);
  public
    class function FormatClassDeclaration(CommentStyle: TCommentStyle;
      const TruncateComments: Boolean; const Snippet: TSnippet): string;
    class function FormatClassDefinition(const Snippet: TSnippet): string;
  end;

{ TSourceGen }

constructor TSourceGen.Create;
  {Constructor. Sets up the object.
  }
begin
  inherited;
  fSourceAnalyser := TSourceAnalyser.Create;
end;

destructor TSourceGen.Destroy;
  {Destructor. Tears down object.
  }
begin
  fSourceAnalyser.Free;
  inherited;
end;

function TSourceGen.IncFileAsString(const CommentStyle: TCommentStyle;
  const TruncateComments: Boolean; const HeaderComments: IStringList): string;
  {Generates source code of an include file containing all specified routines
  and notes in comments which units, types, consts and other routines are also
  required.
    @param CommentStyle [in] Style of commenting used in documenting
      snippets.
    @param TruncateComments [in] Whether comments are to be truncated to just
      first line of multi line snippet descriptions.
    @param HeaderComments [in] List of comments to be included at top of
      snippet.
    @return Source code of include file.
  }
resourcestring
  // Comment text
  sReqUnits           = 'Required unit(s):';
  sReqRoutines        = 'Additional required routine(s):';
  sReqConstsAndTypes  = 'Required constant(s) and / or type(s):';
  sXRefRoutines       = 'Cross referenced routine(s):';
var
  Idx: Integer;             // loops thru snippets list
  Writer: TStringBuilder;   // used to build source code string
  ForwardWritten: Boolean;  // flag true if forward decls have been written
  FirstForward: Boolean;    // flag true when first forward decl to be written
  Snippet: TSnippet;        // accesses various snippet objects
  UnitName: string;         // accesses unit names from a list
begin
  // Generate the unit data
  fSourceAnalyser.Generate;

  // Create writer used to build source code
  Writer := TStringBuilder.Create;
  try

    // Write header comment
    Writer.Append(TSourceComments.FormatHeaderComments(HeaderComments));

    // Write required units, additional routines, types and consts
    if (fSourceAnalyser.Units.Count > 0) or
      (fSourceAnalyser.ForwardRoutines.Count > 0) or
      (fSourceAnalyser.TypesAndConsts.Count > 0) then
    begin
      Writer.AppendLine('{');
      if fSourceAnalyser.Units.Count > 0 then
      begin
        // list of required units
        Writer.AppendLine('  ' + sReqUnits);
        for UnitName in fSourceAnalyser.Units do
          Writer.AppendLine('    ' + UnitName);
      end;
      if fSourceAnalyser.TypesAndConsts.Count > 0 then
      begin
        // list of types and consts
        if (fSourceAnalyser.Units.Count > 0) then
          Writer.AppendLine;
        Writer.AppendLine('  ' + sReqConstsAndTypes);
        for Snippet in fSourceAnalyser.TypesAndConsts do
          Writer.AppendLine('    ' + Snippet.DisplayName);
      end;
      if fSourceAnalyser.ForwardRoutines.Count > 0 then
      begin
        // list of other routines required to compile
        if (fSourceAnalyser.Units.Count > 0) or
          (fSourceAnalyser.TypesAndConsts.Count > 0) then
          Writer.AppendLine;
        Writer.AppendLine('  ' + sReqRoutines);
        for Snippet in fSourceAnalyser.ForwardRoutines do
          Writer.AppendLine('    ' + Snippet.DisplayName);
      end;
      Writer.AppendLine('}');
      Writer.AppendLine;
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
          Writer.AppendLine('// ' + sXRefRoutines);
          FirstForward := False;
        end;
        Writer.AppendLine(
          TRoutineFormatter.FormatRoutinePrototype(Snippet, csNone, False)
        );
        Writer.AppendLine('  forward;');
        ForwardWritten := True;
      end;
    end;
    if ForwardWritten then
      Writer.AppendLine;

    // Write routines
    for Idx := 0 to Pred(fSourceAnalyser.IntfRoutines.Count) do
    begin
      Snippet := fSourceAnalyser.IntfRoutines[Idx];
      Writer.AppendLine(
        TRoutineFormatter.FormatRoutine(CommentStyle, TruncateComments, Snippet)
      );
      if Idx < Pred(fSourceAnalyser.IntfRoutines.Count) then
        Writer.AppendLine;
    end;

    // Return string containing source code
    Result := Writer.ToString;
  finally
    Writer.Free;
  end;
end;

procedure TSourceGen.IncludeSnippet(const Snippet: TSnippet);
  {Includes a snippet in the source code.
    @param Snippet [in] Snippet to be included.
  }
begin
  fSourceAnalyser.AddSnippet(Snippet);
end;

procedure TSourceGen.IncludeSnippets(const Snips: TSnippetList);
  {Includes a one or more snippets in the source code.
    @param Routines [in] List of snippets to be included.
  }
var
  Snippet: TSnippet;  // iterates through snippets to be added
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
  const TruncateComments: Boolean = False;
  const HeaderComments: IStringList = nil): string;
  {Generates source code of a unit containing all specified routines and
  routines depended upon by the included routines.
    @param UnitName [in] Name of unit.
    @param CommentStyle [in] Style of commenting used in documenting routines.
    @param TruncateComments [in] Whether comments are to be truncated to just
      first line of multi line snippet descriptions.
    @param HeaderComments [in] List of comments to be included at top of unit.
    @return Unit source code.
  }
var
  Writer: TStringBuilder;   // used to build source code string
  Snippet: TSnippet;        // reference to a snippet object
  Warnings: IWarnings;      // object giving info about any inhibited warnings
begin
  // Generate the unit data
  fSourceAnalyser.Generate;
  // Create writer object onto string stream that receives output
  Writer := TStringBuilder.Create;
  try
    // Write unit

    // heading comment
    Writer.Append(TSourceComments.FormatHeaderComments(HeaderComments));

    // unit name
    Writer.AppendFormat('unit %s;', [UnitName]).AppendLine;
    Writer.AppendLine;

    // any conditional compilation symbols
    Warnings := Preferences.Warnings;
    if Warnings.Enabled and not Warnings.IsEmpty then
    begin
      Writer.Append(Warnings.Render);
      Writer.AppendLine;
    end;

    // open interface section
    Writer.AppendLine('interface');
    Writer.AppendLine;

    // uses statement
    if fSourceAnalyser.Units.Count > 0 then
    begin
      Writer.AppendLine('uses');
      Writer.AppendLine(
        StrWrap(
          StrJoin(fSourceAnalyser.Units, ', ') + ';',
          cLineWidth - cIndent,
          cIndent
        )
      );
      Writer.AppendLine;
    end;

    // consts and types
    for Snippet in fSourceAnalyser.TypesAndConsts do
    begin
      case Snippet.Kind of
        skTypeDef, skConstant:
          Writer.AppendLine(
            TConstAndTypeFormatter.FormatConstOrType(
              CommentStyle, TruncateComments, Snippet
            )
          );
        skClass:
          Writer.AppendLine(
            TClassFormatter.FormatClassDeclaration(
              CommentStyle, TruncateComments, Snippet
            )
          );
      end;
      Writer.AppendLine;
    end;

    // routine prototypes
    for Snippet in fSourceAnalyser.IntfRoutines do
    begin
      Writer.AppendLine(
        TRoutineFormatter.FormatRoutinePrototype(
          Snippet, CommentStyle, TruncateComments
        )
      );
      Writer.AppendLine;
    end;

    // open implementation section
    Writer.AppendLine('implementation');
    Writer.AppendLine;

    // forward declarations
    if fSourceAnalyser.ForwardRoutines.Count > 0 then
    begin
      for Snippet in fSourceAnalyser.ForwardRoutines do
      begin
        Writer.AppendLine(TRoutineFormatter.ExtractPrototype(Snippet));
        Writer.AppendLine('  forward;');
      end;
      Writer.AppendLine;
    end;

    // routine source code
    for Snippet in fSourceAnalyser.AllRoutines do
    begin
      Writer.AppendLine(
        TRoutineFormatter.FormatRoutine(CommentStyle, TruncateComments, Snippet)
      );
      Writer.AppendLine;
    end;

    for Snippet in fSourceAnalyser.TypesAndConsts do
    begin
      if Snippet.Kind = skClass then
      begin
        Writer.AppendLine(TClassFormatter.FormatClassDefinition(Snippet));
        Writer.AppendLine;
      end;
    end;

    // close unit
    Writer.AppendLine('end.');

    // Return string built in string stream
    Result := Writer.ToString;
  finally
    Writer.Free;
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
  Result := StrSliceLeft(BaseFileName, Length(BaseFileName) - Length(Ext));
end;

{ TSourceAnalyser }

procedure TSourceAnalyser.AddIntfRoutine(const Routine: TSnippet);
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

procedure TSourceAnalyser.AddSnippet(const Snippet: TSnippet);
  {Adds a user-specified snippet to the analysis. Freeform snippets are ignored.
    @param Snippet [in] Snippet to be added.
  }
var
  ErrorMsg: string;       // any error message
begin
  // NOTE: this method must not be called from any other method of this class
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
    skUnit:
      {Ignore};
    skClass:
      AddTypeOrConst(Snippet);
  end;
end;

procedure TSourceAnalyser.AddTypeOrConst(const TypeOrConst: TSnippet);
  {Adds a user specified or required type or constant to the analysis.
    @param TypeOrConst [in] Type of constant snippet to be added.
  }
var
  ErrorMsg: string;       // any error message
begin
  Assert(Assigned(TypeOrConst), ClassName + '.Add: ConstOrType in nil');
  Assert(TypeOrConst.Kind in [skTypeDef, skConstant, skClass],
    ClassName + '.Add: ConstOrType.Kind is not valid');
  // Ignore if already in list
  if fTypesAndConsts.Contains(TypeOrConst) then
    Exit;
  // Validate dependency list
  if not TSnippetValidator.ValidateDependsList(TypeOrConst, ErrorMsg) then
    raise ECodeSnip.Create(ErrorMsg);
  // Add all required snippets to list before adding this one: this ensures
  // required snippets preceed those that depend on them
  RequireSnippets(TypeOrConst.Depends);
  RequireUnits(TypeOrConst.Units);
  fTypesAndConsts.Add(TypeOrConst)
end;

constructor TSourceAnalyser.Create;
  {Constructor. Sets up object.
  }
begin
  inherited;
  fTypesAndConsts := TObjectList<TSnippet>.Create(False);
  fIntfRoutines := TSnippetList.Create;
  fAllRoutines := TSnippetList.Create;
  fForwardRoutines := TSnippetList.Create;
  fRequiredRoutines := TSnippetList.Create;
  fUnits := TStringList.Create;
end;

destructor TSourceAnalyser.Destroy;
  {Destructor. Tears down object.
  }
begin
  fTypesAndConsts.Free;
  fIntfRoutines.Free;
  fAllRoutines.Free;
  fForwardRoutines.Free;
  fRequiredRoutines.Free;
  fUnits.Free;
  inherited;
end;

procedure TSourceAnalyser.Generate;
  {Generates the analysis.
  }
var
  Routine: TSnippet;  // iterates through various routine lists
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

procedure TSourceAnalyser.RequireRoutine(const Routine: TSnippet);
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

procedure TSourceAnalyser.RequireSnippet(const Snippet: TSnippet);
  {Process a snippet from a dependency list.
    @param Snippet [in] Snippet to be processed.
    @except Exception raised if attempt is made to require freeform snippet
  }
resourcestring
  // Error message
  sCantDependOnFreeform = 'Can''t depend on "%s" - it is freeform code';
begin
  case Snippet.Kind of
    skRoutine:                      // require routine
      RequireRoutine(Snippet);
    skConstant, skTypeDef, skClass: // add type/const allowing for dependencies
      AddTypeOrConst(Snippet);
    skFreeform:                     // can't require a freeform snippet
      raise ECodeSnip.CreateFmt(sCantDependOnFreeform, [Snippet.DisplayName]);
  end;
end;

procedure TSourceAnalyser.RequireSnippets(const Snips: TSnippetList);
  {Process all snippets in a dependency list.
    @param Snips [in] List of snippets to process.
  }
var
  Snippet: TSnippet;  // iterates through snippets list
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

{ TRoutineFormatter }

class function TRoutineFormatter.ExtractPrototype(const
  Routine: TSnippet): string;
  {Extracts a routine's prototype from source code.
    @param Routine [in] Routine whose source code to be processed.
    @return Routine prototype.
  }
var
  DummyBody: string;  // stores unused routine body retrieved from Split
begin
  Split(Routine, Result, DummyBody);
  Result := StrTrim(Result);
end;

class function TRoutineFormatter.FormatRoutine(CommentStyle: TCommentStyle;
  const TruncateComments: Boolean; const Routine: TSnippet): string;
  {Formats a routine's whole source code, documented by the routine's
  description in a comment.
    @param CommentStyle [in] Style of commenting used in documenting routine.
    @param TruncateComments [in] Whether comments are to be truncated to just
      first line of multi line snippet descriptions.
    @param Routine [in] Routine whose source code is to be formatted.
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
      Result := StrTrim(Prototype)
        + EOL
        + RenderDescComment(CommentStyle, TruncateComments, Routine)
        + EOL
        + StrTrim(Body);
    end;
    csBefore:
      // Format is: comment - routine
      Result := RenderDescComment(CommentStyle, TruncateComments, Routine)
        + EOL
        + StrTrim(Routine.SourceCode);
    else
      // No commenting: just return source code
      Result := StrTrim(Routine.SourceCode);
  end;
end;

class function TRoutineFormatter.FormatRoutinePrototype(const Routine: TSnippet;
  CommentStyle: TCommentStyle; const TruncateComments: Boolean): string;
  {Formats a routine's prototype, documented by the routine's description in a
  comment.
    @param Routine [in] Routine whose prototype is to be formatted.
    @param CommentStyle [in] Style of commenting used in documenting routine.
    @param TruncateComments [in] Whether comments are to be truncated to just
      first line of multi line snippet descriptions.
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
      Result := Prototype
        + EOL
        + RenderDescComment(CommentStyle, TruncateComments, Routine);
    csBefore:
      // comments preceed prototype
      Result := RenderDescComment(CommentStyle, TruncateComments, Routine)
        + EOL
        + Prototype;
    else
      // no comments: just return prototype
      Result := Prototype;
  end;
end;

class function TRoutineFormatter.RenderDescComment(
  CommentStyle: TCommentStyle; const TruncateComments: Boolean;
  const Routine: TSnippet): string;
  {Creates comment in required style that contains routine's description.
    @param CommentStyle [in] Required commenting style.
    @param TruncateComments [in] Whether comments are to be truncated to just
      first line of multi line snippet descriptions.
    @param Routine [in] Routine for which comments required.
    @return Formatted comments.
  }
begin
  Assert(Routine.Kind = skRoutine,
    ClassName + '.RenderDescComment: Routine must have kind skRoutine');
  // Format the output
  Result := TSourceComments.FormatSnippetComment(
    CommentStyle, TruncateComments, Routine.Description
  );
end;

class procedure TRoutineFormatter.Split(const Routine: TSnippet; out Head,
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
  SourceCode := StrTrim(Routine.SourceCode);
  // Find relative positions of first key characters
  StartParam := StrPos('(', SourceCode);
  AfterParams := StrPos(')', SourceCode) + 1;
  SemiColonPos := StrPos(';', SourceCode);
  // Determine end of head section
  if SemiColonPos > StartParam then
  begin
    // semi colon after param => we have params: skip them before looking for
    // ending ';'
    EndDeclaration := StrPos(
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
  SemiColonPos := StrPos(';', SourceCode, EndDeclaration + 1);
  if SemiColonPos > 0 then
  begin
    Fragment := StrTrim(
      Copy(SourceCode, EndDeclaration + 1, SemiColonPos - EndDeclaration - 1)
    );
    if IsDirective(Fragment) then
      EndDeclaration := SemiColonPos + 1;
  end;
  // now look for 'overload' directive
  SemiColonPos := StrPos(';', SourceCode, EndDeclaration + 1);
  if SemiColonPos > 0 then
  begin
    Fragment := StrTrim(
      Copy(SourceCode, EndDeclaration + 1, SemiColonPos - EndDeclaration - 1)
    );
    if StrToLower(Fragment) = cOverload then
      EndDeclaration := SemiColonPos + 1;
  end;
  // Record declaration (i.e. prototype)
  Head := Copy(SourceCode, 1, EndDeclaration);
  // Get code body
  StartCodeBody := EndDeclaration + 1;
  Body := StrTrim(Copy(SourceCode, StartCodeBody, MaxInt));
end;

{ TConstAndTypeFormatter }

class function TConstAndTypeFormatter.FormatConstOrType(
  CommentStyle: TCommentStyle; const TruncateComments: Boolean;
  const ConstOrType: TSnippet): string;
  {Formats a constant or type's source code, documented by the snippet's
  description in a comment.
    @param CommentStyle [in] Style of commenting used in documenting constant or
      type.
    @param TruncateComments [in] Whether comments are to be truncated to just
      first line of multi line snippet descriptions.
    @param ConstOrType [in] Constant or type whose source code is to be
      formatted.
    @return Formatted prototype.
  }
var
  Keyword: string;  // keyword that preceeds source code body
  Body: string;     // source code that follows keyword
begin
  Assert(ConstOrType.Kind in [skConstant, skTypeDef],
    ClassName + '.FormatConstOrType: ConstOrType must have kind skTypeDef or '
    + 'skConstant');
  Result := '';
  case CommentStyle of
    csNone:
      Result := StrTrim(ConstOrType.SourceCode);
    csBefore:
      Result := RenderDescComment(CommentStyle, TruncateComments, ConstOrType)
        + EOL
        + StrTrim(ConstOrType.SourceCode);
    csAfter:
    begin
      Split(ConstOrType, Keyword, Body);
      if Keyword <> '' then
        Result := Keyword
          + EOL
          + RenderDescComment(CommentStyle, TruncateComments, ConstOrType)
          + EOL
          + Body
      else
        Result := ConstOrType.SourceCode;
    end;
  end;
end;

class function TConstAndTypeFormatter.RenderDescComment(
  CommentStyle: TCommentStyle; const TruncateComments: Boolean;
  const ConstOrType: TSnippet): string;
  {Creates comment in required style that contains constant or type's
  description.
    @param CommentStyle [in] Required commenting style.
    @param TruncateComments [in] Whether comments are to be truncated to just
      first line of multi line snippet descriptions.
    @param ConstOrType [in] Constant or type for which comments required.
    @return Formatted comments.
  }
begin
  Assert(ConstOrType.Kind in [skConstant, skTypeDef],
    ClassName + '.RenderDescComment: ConstOrType must have kind skTypeDef or '
      + 'skConstant');
  Result := TSourceComments.FormatSnippetComment(
    CommentStyle, TruncateComments, ConstOrType.Description
  );
end;

class procedure TConstAndTypeFormatter.Split(const ConstOrType: TSnippet;
  out Prefix, Body: string);
  {Splits source code of a type or constant into the prefix (text up to "const"
  or "type") and definition itself (body code).
    @param ConstOrType [in] Constant or type whose source code to be split.
    @param Prefix [out] Text up to "const" or "type" keyword.
    @param Body [out] Remainder of constant or type without keyword.
  }

  // ---------------------------------------------------------------------------
  procedure SplitAtKeyword(const SourceCode, KW: string;
    out Prefix, Body: string);
    {Splits an introductory prefix (up to KW) from following source code.
      @param SourceCode [in] Source code to be split.
      @param KW [in] Introductory keyord.
      @param Prefix [out] Set to KW if KW is present, otherwise ''.
      @param Body [out] Source code that follows prefix if KW is present,
        otherwise set to SourceCode.
    }
  var
    Lexer: THilitePasLexer;       // parses Pascal code
    PrefixCode: TStringBuilder;   // records prefix code
  const
    SkipTokens = [tkComment, tkCompilerDir, tkWhitespace, tkEOL];
    WhiteSpaceTokens = [tkWhitespace, tkEOL];
  resourcestring
    sTypeKwdError = '"%s" must be first keyword in source code';
  begin
    Lexer := THilitePasLexer.Create(SourceCode);
    try
      PrefixCode := TStringBuilder.Create;
      try
        while Lexer.NextToken in SkipTokens do
          PrefixCode.Append(Lexer.TokenStr);
        if (Lexer.Token = tkKeyword) and StrSameText(Lexer.TokenStr, KW) then
        begin
          PrefixCode.Append(Lexer.TokenStr);
          Prefix := StrTrimRight(PrefixCode.ToString);
          while Lexer.NextToken in WhiteSpaceTokens do
            PrefixCode.Append(Lexer.TokenStr);
          Body := '  ' +
            StrTrim(
              StrSliceRight(
                SourceCode, Length(SourceCode) - Length(PrefixCode.ToString)
              )
            );
        end
        else
        begin
          Prefix := '';
          Body := SourceCode;
        end;
      finally
        PrefixCode.Free;
      end;
    finally
      Lexer.Free;
    end;
  end;
  // ---------------------------------------------------------------------------

begin
  if ConstOrType.Kind = skConstant then
    SplitAtKeyword(ConstOrType.SourceCode, 'const', Prefix, Body)
  else // if ConstOrType.Kind = skTypeDef
    SplitAtKeyword(ConstOrType.SourceCode, 'type', Prefix, Body)
end;

{ TSourceComments }

class function TSourceComments.CommentStyleDesc(
  const Style: TCommentStyle): string;
  {Gets description of a comment style for use in UI elements.
    @param Style [in] Comment style for which description wanted.
    @return Required description.
  }
resourcestring
  // Comment style descriptions
  sCSNone = 'No descriptive comments';
  sCSAfter = 'Comments after snippet header';
  sCSBefore = 'Comments before snippet';
const
  // Map of comment styles to descriptions
  sDescriptions: array[TCommentStyle] of string = (
    sCSNone, sCSAfter, sCSBefore
  );
begin
  Result := sDescriptions[Style];
end;

class function TSourceComments.FormatCommentLines(const Text: string;
  const Indent: Cardinal): string;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := Text;
    Result := StrTrimRight(StrWrap(Lines, cLineWidth - Indent, Indent, False));
  finally
    Lines.Free;
  end;
end;

class function TSourceComments.FormatHeaderComments(
  const Comments: IStringList): string;
  {Formats header comment text as Pascal comments.
    @param Comments [in] List of comments to format.
    @return Formatted comments.
  }
var
  Line: string;         // loops thru each line of comments & exploded comments
  Lines: IStringList;   // comments after exploding multiple wrapped lines
const
  cLinePrefix = ' * ';  // prefixes each comment line
begin
  // Only create comment if some comment text is provided
  if Assigned(Comments) and (Comments.Count > 0) then
  begin
    // text wrap each line of comments and exploded into separate lines
    Lines := TIStringList.Create;
    for Line in Comments do
      if Length(Line) > 0 then
        Lines.Add(
          StrWrap(Line, cLineWidth - Length(cLinePrefix), 0), EOL, True
        )
      else
        Lines.Add('');
    Result := '{';
    // write out each comment line
    for Line in Lines do
      Result := Result + EOL + cLinePrefix + Line;
    Result := Result + EOL + '}' + EOL2;
  end
  else
    Result := '';
end;

class function TSourceComments.FormatSnippetComment(const Style: TCommentStyle;
  const TruncateComments: Boolean; const Text: IActiveText): string;
  {Formats a snippet's comment text as Pascal comment according to commenting
  style.
    @param Style [in] Desired commenting style.
    @param TruncateComments [in] Whether comments are to be truncated to just
      first line of multi line snippet descriptions.
    @param Text [in] Active text of comment. Ignored if Style = csNone.
    @return Formatted comment. Empty string if Style = csNone.
  }
var
  Renderer: TActiveTextTextRenderer;
  PlainText: string;
  Lines: IStringList;
begin
  Renderer := TActiveTextTextRenderer.Create;
  try
    Renderer.DisplayURLs := False;
    PlainText := Renderer.Render(Text);
    if TruncateComments then
    begin
      // use first non-empty paragraph of Text as comment
      Lines := TIStringList.Create(PlainText, string(sLineBreak), False);
      if Lines.Count > 0 then
        PlainText := Lines[0];
    end;
    case Style of
      csNone:
        Result := '';
      csBefore:
        Result := '{'
          + EOL
          + FormatCommentLines(PlainText, cIndent)
          + EOL
          + '}';
      csAfter:
        Result := FormatCommentLines(
          '{' + PlainText + '}', cIndent
        );
    end;
  finally
    Renderer.Free;
  end;
end;

{ TClassFormatter }

class function TClassFormatter.FormatClassDeclaration(
  CommentStyle: TCommentStyle; const TruncateComments: Boolean;
  const Snippet: TSnippet): string;
var
  Dummy: string;
  Decl: string;
  DeclBody: string;
begin
  SplitDeclFromDefn(Snippet.SourceCode, Decl, Dummy);
  Decl := StrTrim(Decl);
  case CommentStyle of
    csNone:
      Result := StrTrim(Decl);
    csBefore:
      Result := RenderDescComment(CommentStyle, TruncateComments, Snippet)
        + EOL
        + StrTrim(Decl);
    csAfter:
    begin
      if RemoveKeywordFromDecl(Decl, DeclBody) then
        Result := 'type'
          + EOL
          + RenderDescComment(CommentStyle, TruncateComments, Snippet)
          + EOL
          + DeclBody
      else
        Result := Decl;
    end;
  end;
end;

class function TClassFormatter.FormatClassDefinition(const Snippet: TSnippet):
  string;
var
  Dummy: string;
begin
  SplitDeclFromDefn(Snippet.SourceCode, Dummy, Result);
end;

class function TClassFormatter.RemoveKeywordFromDecl(const Decl: string;
  out DeclBody: string): Boolean;
const
  Keyword = 'type';
begin
  Result := StrStartsStr(Keyword, Decl);
  if Result then
    DeclBody := '  ' + StrTrim(Copy(Decl, Length(Keyword) + 1, MaxInt))
  else
    // "type" not found - can't remove
    DeclBody := Decl;
end;

class function TClassFormatter.RenderDescComment(CommentStyle: TCommentStyle;
  const TruncateComments: Boolean; const Snippet: TSnippet): string;
begin
  Result := TSourceComments.FormatSnippetComment(
    CommentStyle, TruncateComments, Snippet.Description
  );
end;

class procedure TClassFormatter.SplitDeclFromDefn(const Source: string;
  out Decl, Defn: string);
var
  Lexer: THilitePasLexer;
  SB: TStringBuilder;
  ClassTypeName: string;
  Temp: string;
const
  WhiteSpaceTokens = [tkComment, tkCompilerDir, tkWhitespace, tkEOL];

  function IsMethodKwd(const Tok: string): Boolean;
  begin
    Result := StrSameText(Lexer.TokenStr, 'function')
      or StrSameText(Lexer.TokenStr, 'procedure')
      or StrSameText(Lexer.TokenStr, 'constructor')
      or StrSameText(Lexer.TokenStr, 'destructor')
      or StrSameText(Lexer.TokenStr, 'operator');
  end;

resourcestring
  sTypeKwdError = '"type" must be first keyword in source code';
  sClassTypeNameError = 'Class type name expected in source code';
  sBadTypeError = 'Invalid class or advanced record type';
  sImplementationKwdError = '"implementation" keyword not permitted in class or '
    + 'advanced record snippets.';
begin
  Lexer := THilitePasLexer.Create(Source);
  try
    SB := TStringBuilder.Create;
    try
      // skip any leading white space and comments to first Pascal token
      // this must be "type" keyword
      while Lexer.NextToken in WhiteSpaceTokens do
        SB.Append(Lexer.TokenStr);
      if (Lexer.Token <> tkKeyword)
        and not StrSameText(Lexer.TokenStr, 'type') then
        raise ECodeSnip.Create(sTypeKwdError);
      SB.Append(Lexer.TokenStr);

      // get name of class from following indentifier
      while Lexer.NextToken in WhiteSpaceTokens do
        SB.Append(Lexer.TokenStr);
      if Lexer.Token <> tkIdentifier then
        raise ECodeSnip.Create(sClassTypeNameError);
      ClassTypeName := Lexer.TokenStr;
      SB.Append(Lexer.TokenStr);

      while True do
      begin
        // look for methods
        while not (Lexer.NextToken in [tkKeyword, tkEOF])
          and not (
            IsMethodKwd(Lexer.TokenStr)
            or StrSameText(Lexer.TokenStr, 'class')
            or StrSameText(Lexer.TokenStr, 'implementation')
          ) do
          SB.Append(Lexer.TokenStr);
        if Lexer.Token = tkEOF then
          raise ECodeSnip.Create(sBadTypeError);
        if (Lexer.Token = tkKeyword)
          and StrSameText(Lexer.TokenStr, 'implementation') then
          raise ECodeSnip.Create(sImplementationKwdError);
        // check if function is followed by ClassTypeName and a dot => start
        // of declaration
        Temp := '';
        if not IsMethodKwd(Lexer.TokenStr) then
        begin
          // must have tkKeyword of "class": record it and look for
          Temp := Lexer.TokenStr;
          // skip whitespace following "class"
          while Lexer.NextToken in WhiteSpaceTokens do
            Temp := Temp + Lexer.TokenStr;
          // now look for one of method keywords "class" keyword
          if (Lexer.Token <> tkKeyword) and not IsMethodKwd(Lexer.TokenStr) then
          begin
            // didn't find method: record text read and go round again
            SB.Append(Temp);            // "class" and whitespace
            SB.Append(Lexer.TokenStr);  // token after whitespace
            Continue;
          end;
        end;
        // record method name
        Temp := Temp + Lexer.TokenStr;
        // record following white space
        while Lexer.NextToken in WhiteSpaceTokens do
          Temp := Temp + Lexer.TokenStr;
        // record pascal item after white space
        Temp := Temp + Lexer.TokenStr;
        if (Lexer.Token <> tkIdentifier)
          or not StrSameText(Lexer.TokenStr, ClassTypeName) then
        begin
          // not the required identifier: record text and go round again
          SB.Append(Temp);
          Continue;
        end;
        // check for following '.'
        while Lexer.NextToken in WhiteSpaceTokens do
          Temp := Temp + Lexer.TokenStr;
        Temp := Temp + Lexer.TokenStr;
        if (Lexer.Token <> tkSymbol) or (Lexer.TokenStr <> '.') then
        begin
          SB.Append(Temp);
          Continue;
        end;
        // check for following identifier
        while Lexer.NextToken in WhiteSpaceTokens do
          Temp := Temp + Lexer.TokenStr;
        Temp := Temp + Lexer.TokenStr;
        if (Lexer.Token <> tkIdentifier) then
        begin
          SB.Append(Temp);
          Continue;
        end;
        Break;
      end;
      // Lexer replaces CRLF with LF, but we need CRLF to keep string length
      // same as original so that string slice below works
      Decl := StrReplace(SB.ToString, LF, CRLF);
    finally
      SB.Free;
    end;
    Defn := StrSliceRight(Source, Length(Source) - Length(Decl));
  finally
    Lexer.Free;
  end;
end;

end.

