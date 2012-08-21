{
 * USaveUnitMgr.pas
 *
 * Defines a class that manages generation, previewing and saving of a pascal
 * unit.
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
 * The Original Code is USaveUnitMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit USaveUnitMgr;


interface


uses
  // Project
  UBaseObjects, UIStringList, USourceFileOutputMgr, USourceGen, USnippets;


type

  {
  TSaveUnitMgr:
    Manages generation, previewing and saving of a Pascal unit to disk.
    Generated file can be a valid Pascal unit, a plain text file, an HTML file
    or a RTF file. The last two files types can optionally be syntax
    highlighted.
  }
  TSaveUnitMgr = class(TNoPublicConstructObject)
  strict private
    fSourceGen: TSourceGen;
      {Generates source code unit}
    fOutputMgr: TSourceFileOutputMgr;
      {Gets file information from user and controls saving of snippers to disk}
    fUnitName: string;
      {Name of generated unit. Name is based on file when saving unit and has a
      default fixed value when previewing}
    fContainsMainDBSnippets: Boolean;
      {Flag true if unit contains at least one snippet from main database, False
      only if unit is completely user defined}
    procedure SourceGenHandler(Sender: TObject;
      const CommentStyle: TCommentStyle; out RawSourceCode, DocTitle: string);
      {Handles output manager's OnGenerateOutput event by generating source code
      of unit in required comment style.
        @param Sender [in] Not used.
        @param CommentStyle [in] Style of commenting to be used in source code.
        @param SourceCode [out] Receives generated source code.
        @param DocTitle [out] Receives document title.
      }
    procedure CheckFileNameHandler(Sender: TObject; const FileName: string;
      var NameOK: Boolean; var ErrorMessage: string);
      {Handler of output manager's OnCheckFileName event. Checks if file name is
      suitable for use as basis of a unit name. If so unit name is recorded.
        @param Sender [in] Not used.
        @param FileName [in] File name to be checked.
        @param NameOK [in/out] Defaults to true. Set to false if file name fails
          check, i.e. is not valid as a unit name.
        @param ErrorMessage [in/out] Default to ''. Set to error message if
          NameOK is set false.
      }
    function UnitName: string;
      {Gets name of unit to be used in generated code.
        @return Name of unit.
      }
    function CreateHeaderComments: IStringList;
      {Creates and stores header comments to be written to head of unit.
        @return String list containing comments.
      }
  strict protected
    constructor InternalCreate(const Snips: TRoutineList);
      {Class constructor. Sets up object to save a unit containing all snippets
      in a list.
        @param Snips [in] List of snippets to include in unit.
      }
    procedure DoExecute;
      {Gets information from user about name and format of required file and
      saves unit to disk.
      }
  public
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    class procedure Execute(const Snips: TRoutineList);
      {Gets information from user about name and format of required file and
      saves unit containing specified snippets to disk.
        @param Snips [in] List of snippets to include in unit.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UAppInfo, USourceFileInfo, UUtils, Web.UInfo;


resourcestring
  // Dialog box strings
  // title
  sSaveDlgTitle = 'Save Unit';
  // default file / unit name
  sDefUnitName = 'Snippets';
  // file filter strings
  sHTMLDesc = 'HTML file';
  sRTFDesc = 'Rich text file';
  sPascalDesc = 'Pascal unit';
  sTextDesc = 'Plain text file';

  // Error message
  sErrorMsg = 'Filename is not valid for a Pascal unit';

  // Unit header comments
  sLicense = 'This unit may be freely distributed and used on the condition '
    + 'that this comment is not removed from the unit.';
  sMainDescription = 'The unit was generated automatically from a '
    + 'selection of source code taken from the Code Snippets Database at %0:s.';
  sDisclaimer = 'The source code contained in this unit is made available on '
    + 'an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or '
    + 'implied. The code is used entirely at your own risk.';
  sGenerated = 'Generated on : %0:s.';
  sGenerator = 'Generated by : %0:s %1:s.';
  sAdvert = 'The latest version of %0:s is available from the %1:s website '
    + 'at %2:s.';
  sUserDescription = 'This unit was generated automatically.';

  // Output document title
  sDocTitle = 'Unit "%0:s" generated by %1:s';


{ TSaveUnitMgr }

procedure TSaveUnitMgr.CheckFileNameHandler(Sender: TObject;
  const FileName: string; var NameOK: Boolean; var ErrorMessage: string);
  {Handler of output manager's OnCheckFileName event. Checks if file name is
  suitable for use as basis of a unit name. If so unit name is recorded.
    @param Sender [in] Not used.
    @param FileName [in] File name to be checked.
    @param NameOK [in/out] Defaults to true. Set to false if file name fails
      check, i.e. is not valid as a unit name.
    @param ErrorMessage [in/out] Default to ''. Set to error message if NameOK
      is set false.
  }
begin
  NameOK := TSourceGen.IsFileNameValidUnitName(FileName);
  if NameOK then
    fUnitName := TSourceGen.UnitNameFromFileName(FileName)
  else
  begin
    fUnitName := '';
    ErrorMessage := sErrorMsg
  end;
end;

function TSaveUnitMgr.CreateHeaderComments: IStringList;
  {Creates and stores header comments to be written to head of unit.
    @return String list containing comments.
  }
begin
  Result := TIStringList.Create;
  if fContainsMainDBSnippets then
  begin
    // Result used for units that contain at snippet(s) from main database
    Result.Add(sLicense);
    Result.Add('');
    Result.Add(Format(sMainDescription, [TWebInfo.DatabaseURL]));
    Result.Add('');
    Result.Add(sDisclaimer);
    Result.Add('');
    Result.Add(Format(sGenerated, [DateStamp]));
    Result.Add(
      Format(
        sGenerator, [TAppInfo.FullProgramName, TAppInfo.ProgramReleaseInfo]
      )
    );
    Result.Add('');
    Result.Add(
      Format(
        sAdvert,
        [TAppInfo.ProgramName, TAppInfo.CompanyName, TWebInfo.ProgramHomeURL]
      )
    );
  end
  else
  begin
    // Result used for units that contain only user defined snippets
    Result.Add(sUserDescription);
    Result.Add('');
    Result.Add(Format(sGenerated, [DateStamp]));
    Result.Add(
      Format(
        sGenerator, [TAppInfo.FullProgramName, TAppInfo.ProgramReleaseInfo]
      )
    );
  end;
end;

destructor TSaveUnitMgr.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fOutputMgr);
  FreeAndNil(fSourceGen);
  inherited;
end;

procedure TSaveUnitMgr.DoExecute;
  {Gets information from user about name and format of required file and saves
  unit to disk.
  }
begin
  // Hand off processing to output manager
  fOutputMgr.Execute;
end;

class procedure TSaveUnitMgr.Execute(const Snips: TRoutineList);
  {Gets information from user about name and format of required file and saves
  unit containing specified snippets to disk.
    @param Snips [in] List of snippets to include in unit.
  }
begin
  with InternalCreate(Snips) do
    try
      DoExecute;
    finally
      Free;
    end;
end;

constructor TSaveUnitMgr.InternalCreate(const Snips: TRoutineList);
  {Class constructor. Sets up object to save a unit containing all snippets in a
  list.
    @param Snips [in] List of snippets to include in unit.
  }
var
  Snippet: TRoutine;  // references each snippet in list
begin
  Assert(Assigned(Snips), ClassName + '.InternalCreate: Snips is nil');
  inherited InternalCreate;

  // Create source generator and initialize it with required snippets
  fSourceGen := TSourceGen.Create;
  fSourceGen.IncludeSnippets(Snips);

  // Determine if snippet list contains at least one snippet from main database
  fContainsMainDBSnippets := False;
  for Snippet in Snips do
  begin
    if not Snippet.UserDefined then
    begin
      fContainsMainDBSnippets := True;
      Break;
    end;
  end;

  // Create and initialise output manager object
  fOutputMgr := TSourceFileOutputMgr.Create;
  fOutputMgr.DlgTitle := sSaveDlgTitle;
  fOutputMgr.DlgHelpKeyword := 'SaveUnitDlg';
  fOutputMgr.OnGenerateOutput := SourceGenHandler;
  fOutputMgr.OnCheckFileName := CheckFileNameHandler;
  with fOutputMgr.SourceFileInfo do
  begin
    Descriptions[sfText] := sTextDesc;
    FileExtensions[sfText] := '.txt';
    Descriptions[sfPascal] := sPascalDesc;
    FileExtensions[sfPascal] := '.pas';
    Descriptions[sfHTML] := sHTMLDesc;
    FileExtensions[sfHTML] := '.html';
    Descriptions[sfRTF] := sRTFDesc;
    FileExtensions[sfRTF] := '.rtf';
    FileName := sDefUnitName;
  end;
end;

procedure TSaveUnitMgr.SourceGenHandler(Sender: TObject;
  const CommentStyle: TCommentStyle; out RawSourceCode, DocTitle: string);
  {Handles output manager's OnGenerateOutput event by generating source code of
  unit in required comment style.
    @param Sender [in] Not used.
    @param CommentStyle [in] Style of commenting to be used in source code.
    @param SourceCode [out] Receives generated source code.
    @param DocTitle [out] Receives document title.
  }
begin
  RawSourceCode := fSourceGen.UnitAsString(
    UnitName, CommentStyle, CreateHeaderComments
  );
  DocTitle := Format(sDocTitle, [UnitName, TAppInfo.ProgramName]);
end;

function TSaveUnitMgr.UnitName: string;
  {Gets name of unit to be used in generated code.
    @return Name of unit.
  }
begin
  // If we have valid unit name based on file, use it, otherwise use default
  if fUnitName <> '' then
    Result := fUnitName
  else
    Result := sDefUnitName;
end;

end.

