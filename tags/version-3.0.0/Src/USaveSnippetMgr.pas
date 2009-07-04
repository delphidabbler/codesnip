{
 * USaveSnippetMgr.pas
 *
 * Defines a class that manages generation, previewing and saving of a code
 * snippet.
 *
 * v0.1 of 17 Mar 2005  - Original version.
 * v0.2 of 21 Apr 2005  - Changed to use renamed USourceGen unit and renamed
 *                        TSourceGen class.
 *                      - Added new IntfHiliter unit that contains required type
 *                        definitions formerly in other units.
 * v0.3 of 25 Apr 2005  - Added code to persist managed save last snippet dialog
 *                        user's settings to application's storage.
 * v0.4 of 08 Jan 2006  - Extracted highlighting code from TSaveSnippetMgr into
 *                        separate class.
 *                      - Extracted source code generation from TSaveSnippetMgr
 *                        into separate class.
 *                      - Extracted super class made up of code shared between
 *                        TSaveSnippetMgr and new TCopySnippetMgr.
 *                      - Heavily revised TSaveSnippetMgr as a result of class
 *                        extractions.
 *                      - Added new CanHandleView class method (in new super
 *                        class) to enable class to test for a valid view from
 *                        which to generate snippet.
 *                      - Extracted dialog box's title to resource string.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 *                      - Removed unused unit reference.
 * v2.0 of 29 Oct 2006  - Major update. Passed off much of processing to helper
 *                        objects:
 *                        - Now uses TSourceFileInfo object to handle
 *                          extensions, dialog box file type filter, and mapping
 *                          to source file type.
 *                        - Now uses TSourceFileOutputMgr to handle display of
 *                          dialog box and formatting of source code.
 * v2.1 of 04 Feb 2007  - Replaced redundant TDetailView class references with
 *                        TViewItem.
 * v2.2 of 02 Jul 2007  - Added code to build a document title and to include in
 *                        generated snippets.
 *                      - A default file name, based on snippet name and made
 *                        into valid Pascal identifier, is now provided for use
 *                        in dialog box.
 * v2.3 of 12 Sep 2008  - Changed document title to remove reference to
 *                        database.
 * v2.4 of 03 Oct 2008  - Changed to use new inherited static Execute method and
 *                        to override new DoExecute method of base class.
 *                      - Made protected and private sections strict.
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
 * The Original Code is USaveSnippetMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit USaveSnippetMgr;


interface


uses
  // Project
  USourceFileOutputMgr, USourceGen, USnippetMgr, UView;


type

  {
  TSaveSnippetMgr:
    Manages generation, previewing and saving of a code snippet routine or
    category to disk. Generated file can be a Pascal include file, a plain text
    file, an HTML file or a RTF file. The last two files types can optionally be
    syntax highlighted.
  }
  TSaveSnippetMgr = class(TSnippetMgr)
  strict private
    fDocTitle: string;
      {Title to used for saved documents}
    fOutputMgr: TSourceFileOutputMgr;
      {Object used to get file information from user and to control saving of
      snippers to disk}
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
    constructor InternalCreate(const View: TViewItem); override;
      {Class constructor. Sets up object to save specified routine or category.
        @param View [in] View that stores category or routine to be saved. Must
          be a saveable view.
      }
    procedure DoExecute; override;
      {Gets information from user about name and format of required file and
      saves required snippet to disk.
      }
  public
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  USourceFileInfo;


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

destructor TSaveSnippetMgr.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fOutputMgr);
  inherited;
end;

procedure TSaveSnippetMgr.DoExecute;
  {Gets information from user about name and format of required file and saves
  required snippet to disk.
  }
begin
  // Hand off processing to output manager
  fOutputMgr.Execute;
end;

constructor TSaveSnippetMgr.InternalCreate(const View: TViewItem);
  {Class constructor. Sets up object to save specified routine or category.
    @param View [in] View that stores category or routine to be saved. Must be a
      saveable view.
  }
begin
  // ** Do not localise any literal strings in this method
  // Pass processing of view off to super class
  inherited InternalCreate(View);
  // Create and initialise output manager object
  fOutputMgr := TSourceFileOutputMgr.Create;
  fOutputMgr.DlgTitle := Format(sSaveDlgTitle, [View.Description]);
  fOutputMgr.DlgHelpKeyword := 'SaveSnippetDlg';
  fOutputMgr.OnGenerateOutput := SourceGenHandler;
  with fOutputMgr.SourceFileInfo do
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
  // Record document title
  if View.Kind = vkCategory then
    fDocTitle := Format(sDocTitle, [View.Description, sCategory])
  else if View.Kind = vkRoutine then
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
  RawSourceCode := Self.SourceCode(CommentStyle);
  DocTitle := fDocTitle;
end;

end.

