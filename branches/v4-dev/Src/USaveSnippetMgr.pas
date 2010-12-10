{
 * USaveSnippetMgr.pas
 *
 * Defines a class that manages generation, previewing and saving of a code
 * snippet.
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
 * The Original Code is USaveSnippetMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit USaveSnippetMgr;


interface


uses
  // Project
  UBaseObjects, USourceFileInfo, USourceFileOutputMgr, USourceGen, UView;


type

  {
  TSaveSnippetMgr:
    Manages generation, previewing and saving of a suitable code snippet or
    category to disk. Generated file can be a Pascal include file, a plain text
    file, an HTML file or a RTF file. The last two files types can optionally be
    syntax highlighted.
  }
  TSaveSnippetMgr = class(TNoPublicConstructObject)
  strict private
    fView: IView;                     // View to be output
    fDocTitle: string;                // Title of saved documents
    fOutputMgr: TSourceFileOutputMgr; // Gets save info and manages output
    fSourceFileInfo: TSourceFileInfo; // Info about supported source file types
    procedure SourceGenHandler(Sender: TObject;
      const CommentStyle: TCommentStyle; out RawSourceCode, DocTitle: string);
      {Handles output manager's OnGenerateOutput event by generating source code
      for snippet in required comment style.
        @param Sender [in] Not used.
        @param CommentStyle [in] Style of commenting to be used in source code.
        @param SourceCode [out] Receives generated source code.
        @param DocTitle [out] Receives document title.
      }
  strict protected
    constructor InternalCreate(View: IView);
      {Class constructor. Sets up object for view.
        @param View [in] View to be output.
      }
    procedure DoExecute;
      {Gets information from user about name and format of required file and
      saves generated source code to disk.
      }
  public
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    class procedure Execute(View: IView);
      {Creates and outputs a compilable include file generated from a view item.
        @param View [in] View from which source code is generated. CanHandleView
          must return True for this view.
      }
    class function CanHandleView(View: IView): Boolean;
      {Checks whether a snippet include file can be created from a view.
        @param View [in] View to be checked.
        @return True if view contains code that can be output as a compilable
          snippet include file, False otherwise.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  USnippetSourceGen;


resourcestring
  // Dialog box strings
  // title
  sSaveDlgTitle = 'Save %0:s Snippet';
  // file filter strings
  sHtmExtDesc = 'HTML file';
  sRtfExtDesc = 'Rich text file';
  sIncExtDesc = 'Pascal include file';
  sTxtExtDesc = 'Plain text file';

  // Output document title for routines and categories
  sDocTitle = '"%0:s" %1:s';
  sCategory = 'category';
  sRoutine = 'routine';


{ TSaveSnippetMgr }

class function TSaveSnippetMgr.CanHandleView(View: IView): Boolean;
  {Checks whether a snippet include file can be created from a view.
    @param View [in] View to be checked.
    @return True if view contains code that can be output as a compilable
      snippet include file, False otherwise.
  }
begin
  Result := TSnippetSourceGen.CanGenerate(View);
end;

destructor TSaveSnippetMgr.Destroy;
  {Class destructor. Tears down object.
  }
begin
  fOutputMgr.Free;
  fSourceFileInfo.Free;
  inherited;
end;

procedure TSaveSnippetMgr.DoExecute;
  {Gets information from user about name and format of required file and saves
  generated source code to disk.
  }
begin
  // Hand off processing to output manager
  fOutputMgr.Execute;
end;

class procedure TSaveSnippetMgr.Execute(View: IView);
  {Creates and outputs a compilable include file generated from a view item.
    @param View [in] View from which source code is generated. CanHandleView
      must return True for this view.
  }
begin
  with InternalCreate(View) do
    try
      DoExecute;
    finally
      Free;
    end;
end;

constructor TSaveSnippetMgr.InternalCreate(View: IView);
  {Class constructor. Sets up object for view.
    @param View [in] View to be output.
  }
begin
  inherited InternalCreate;
  // Record reference to view object
  fView := View;

  // Record info about types of source code file supported
  fSourceFileInfo := TSourceFileInfo.Create;
  with fSourceFileInfo do
  begin
    Descriptions[sfText] := sTxtExtDesc;
    FileExtensions[sfText] := '.txt';
    Descriptions[sfPascal] := sIncExtDesc;
    FileExtensions[sfPascal] := '.inc';
    Descriptions[sfHTML] := sHtmExtDesc;
    FileExtensions[sfHTML] := '.html';
    Descriptions[sfRTF] := sRtfExtDesc;
    FileExtensions[sfRTF] := '.rtf';
    FileName := View.Description;
  end;

  // Create and initialise output manager object
  fOutputMgr := TSourceFileOutputMgr.Create(fSourceFileInfo);
  fOutputMgr.DlgTitle := Format(sSaveDlgTitle, [View.Description]);
  fOutputMgr.DlgHelpKeyword := 'SaveSnippetDlg';
  fOutputMgr.OnGenerateOutput := SourceGenHandler;

  // Record document title
  if Supports(View, ICategoryView) then
    fDocTitle := Format(sDocTitle, [View.Description, sCategory])
  else if Supports(View, ISnippetView) then
    fDocTitle := Format(sDocTitle, [View.Description, sRoutine]);
end;

procedure TSaveSnippetMgr.SourceGenHandler(Sender: TObject;
  const CommentStyle: TCommentStyle; out RawSourceCode, DocTitle: string);
  {Handles output manager's OnGenerateOutput event by generating source code for
  snippet in required comment style.
    @param Sender [in] Not used.
    @param CommentStyle [in] Style of commenting to be used in source code.
    @param SourceCode [out] Receives generated source code.
    @param DocTitle [out] Receives document title.
  }
begin
  RawSourceCode := TSnippetSourceGen.Generate(fView, CommentStyle);
  DocTitle := fDocTitle;
end;

end.

