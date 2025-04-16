{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2025, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Saves information about a snippet to disk in rich text format. Only routine
 * snippet kinds are supported.
}


unit USaveInfoMgr;

interface

uses
  // Project
  UEncodings,
  UView;


type
  ///  <summary>Method-only record that saves information about a snippet to
  ///  file in rich text format. The snippet is obtained from a view. Only
  ///  snippet views are supported.</summary>
  TSaveInfoMgr = record
  strict private
    ///  <summary>Attempts to name of the file to be written from the user.
    ///  </summary>
    ///  <param name="AFileName"><c>string</c> [out] Set to the name of the file
    ///  entered by the user. Undefined if the user cancelled.</param>
    ///  <returns><c>Boolean</c>. <c>True</c> if the user entered and accepted a
    ///  file name of <c>False</c> if the user cancelled.</returns>
    class function TryGetFileNameFromUser(out AFileName: string): Boolean;
      static;
    ///  <summary>Returns encoded data containing a RTF representation of
    ///  information about the snippet represented by the given view.</summary>
    class function GenerateRichText(View: IView): TEncodedData; static;
  public
    ///  <summary>Saves information about the snippet referenced by the a given
    ///  view to file.</summary>
    ///  <remarks>The view must be a snippet view.</remarks>
    class procedure Execute(View: IView); static;
    ///  <summary>Checks if a given view can be saved to the clipboard. Returns
    ///  True only if the view represents a snippet.</summary>
    class function CanHandleView(View: IView): Boolean; static;

  end;

implementation

uses
  // Delphi
  SysUtils,
  Dialogs,
  // Project
  Hiliter.UAttrs,
  Hiliter.UGlobals,
  UIOUtils,
  UOpenDialogHelper,
  URTFSnippetDoc,
  URTFUtils,
  USaveDialogEx;

{ TSaveInfoMgr }

class function TSaveInfoMgr.CanHandleView(View: IView): Boolean;
begin
  Result := Supports(View, ISnippetView);
end;

class procedure TSaveInfoMgr.Execute(View: IView);
var
  FileName: string;
  RTF: TRTF;
begin
  Assert(Assigned(View), 'TSaveInfoMgr.Execute: View is nil');
  Assert(CanHandleView(View), 'TSaveInfoMgr.Execute: View not supported');
  if not TryGetFileNameFromUser(FileName) then
    Exit;
  RTF := TRTF.Create(GenerateRichText(View));
  TFileIO.WriteAllBytes(FileName, RTF.ToBytes);
end;

class function TSaveInfoMgr.GenerateRichText(View: IView): TEncodedData;
var
  Doc: TRTFSnippetDoc;        // object that generates RTF document
  HiliteAttrs: IHiliteAttrs;  // syntax highlighter formatting attributes
begin
  Assert(Supports(View, ISnippetView),
    'TSaveInfoMgr.GenerateRichText: View is not a snippet view');
  if (View as ISnippetView).Snippet.HiliteSource then
    HiliteAttrs := THiliteAttrsFactory.CreateUserAttrs
  else
    HiliteAttrs := THiliteAttrsFactory.CreateNulAttrs;
  Doc := TRTFSnippetDoc.Create(HiliteAttrs);
  try
    // TRTFSnippetDoc generates stream of ASCII bytes
    Result := Doc.Generate((View as ISnippetView).Snippet);
    Assert(Result.EncodingType = etASCII,
      'TSaveInfoMgr.GenerateRichText: ASCII encoded data expected');
  finally
    Doc.Free;
  end;
end;

class function TSaveInfoMgr.TryGetFileNameFromUser(
  out AFileName: string): Boolean;
var
  Dlg: TSaveDialogEx;
resourcestring
  sCaption = 'Save Snippet Information';     // dialogue box caption
  sFilter = 'Rich Text File (*.rtf)|*.rtf|'  // file filter
    + 'All files (*.*)|*.*';
begin
  Dlg := TSaveDialogEx.Create(nil);
  try
    Dlg.Title := sCaption;
    Dlg.Options := [ofShowHelp, ofNoTestFileCreate, ofEnableSizing];
    Dlg.Filter := sFilter;
    Dlg.FilterIndex := 1;
    Dlg.HelpKeyword := 'SnippetInfoFileDlg';
    Result := Dlg.Execute;
    if Result then
      AFileName := FileOpenFileNameWithExt(Dlg)
  finally
    Dlg.Free;
  end;
end;

end.
