{
 * FmPreviewDlg.pas
 *
 * Implements a dialog box that is used to preview or display text, HTML and
 * Rich text documents.
 *
 * v0.1 of 17 Mar 2005  - Original version.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 *                      - Removed unused unit reference.
 * v1.1 of 11 Nov 2006  - Replaced code that detects HTML and RTF formats with
 *                        calls to helper routines from RTF and HTML utility
 *                        units.
 *                      - Changed to set page control tabs invisible at design
 *                        time rather than run time.
 * v1.2 of 08 Feb 2007  - Moved code that displays document from FormShow event
 *                        handler to new overridden InitForm method deleted
 *                        FormShow.
 * v1.3 of 13 May 2007  - Added popup menu for selection and copying of
 *                        previewed text along with associated actions and image
 *                        list.
 *                      - Added code to call into viewer objects to perform
 *                        selection and copying.
 * v1.4 of 17 Jun 2009  - Added support for variable dialog box captions:
 *                        - An optional parameter was to the dialog's Execute
 *                          method to allow caller to specify a title.
 *                        - Added support for getting caption from any title
 *                          embedded in displayed document.
 *                      - Made private and protected sections strict.
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
 * The Original Code is FmPreviewDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FmPreviewDlg;


interface


uses
  // Delphi
  ImgList, Controls, Classes, ActnList, Menus, Forms, ComCtrls, StdCtrls,
  ExtCtrls,
  // Project
  FmGenericViewDlg, FrBrowserBase, FrHTMLPreview, FrMemoPreview, FrRTFPreview,
  FrTextPreview, IntfPreview;


type

  {
  TPreviewDlg:
    Dialog box used to preview text, HTML and Rich text documents.
  }
  TPreviewDlg = class(TGenericViewDlg)
    actCopy: TAction;
    actSelectAll: TAction;
    alPreview: TActionList;
    frRTF: TRTFPreviewFrame;
    frHTML: THTMLPreviewFrame;
    frText: TTextPreviewFrame;
    ilPreview: TImageList;
    miCopy: TMenuItem;
    miSelectAll: TMenuItem;
    mnuPreview: TPopupMenu;
    pcViews: TPageControl;
    tsHTML: TTabSheet;
    tsRTF: TTabSheet;
    tsText: TTabSheet;
    procedure actCopyExecute(Sender: TObject);
    procedure actCopyUpdate(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actSelectAllUpdate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  strict private
    fViewer: IInterface;  // Interfaces with viewer frame
    fDocContent: string;  // Stores content of document we are displaying
    fDlgTitle: string;    // Dialog box title
    procedure GetViewerInfo(out Viewer: IInterface; out TabSheet: TTabSheet);
      {Gets information about required document viewer and tab sheet that
      contains it.
        @param Viewer [out] Interface to viewer frame.
        @param TabSheet [out] Tab sheet containing viewer frame.
      }
    function CanCopy: Boolean;
      {Checks if current view supports copying to clipboard.
        @return True if copying supported.
      }
    function CanSelectAll: Boolean;
      {Checks if current view supports text selection.
        @return True if selection supported.
      }
    procedure CopyToClipboard;
      {Copies selected text to clipboard from current view if view supports
      copying.
      }
    procedure SelectAll;
      {Selects all text in current view if view supports selection.
      }
  strict protected
    procedure InitForm; override;
      {Loads and displays the document being previewed.
      }
  public
    class procedure Execute(AOwner: TComponent; const ADocContent: string;
      const ADlgTitle: string = '');
      {Displays a document in the preview dialog.
        @param AOwner [in] Owning component.
        @param ADocContent [in] Content of document to be displayed (HTML, RTF
          or plain text).
        @param ADlgTitle [in] Title of dialog box. Default is used if ''.
      }
    property DocContent: string read fDocContent write fDocContent;
      {Content of document being displayed}
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  IntfFrameMgrs, URTFUtils, UHTMLUtils;


{$R *.dfm}


{ TPreviewDlg }

procedure TPreviewDlg.actCopyExecute(Sender: TObject);
  {Copies preview text to clipboard.
    @param Sender [in] Not used.
  }
begin
  CopyToClipboard;
end;

procedure TPreviewDlg.actCopyUpdate(Sender: TObject);
  {Enables / disables Copy action depending or whether copying is supported in
  current view.
    @param Sender [in] Not used.
  }
begin
  actCopy.Enabled := CanCopy;
end;

procedure TPreviewDlg.actSelectAllExecute(Sender: TObject);
  {Selects all preview text.
    @param Sender [in] Not used.
  }
begin
  SelectAll;
end;

procedure TPreviewDlg.actSelectAllUpdate(Sender: TObject);
  {Enables / disables Select All action depending on whether selection is
  supported in current view.
    @param Sender [in] Not used.
  }
begin
  actSelectAll.Enabled := CanSelectAll;
end;

function TPreviewDlg.CanCopy: Boolean;
  {Checks if current view supports copying to clipboard.
    @return True if copying supported.
  }
begin
  Result := False;
  if Supports(fViewer, IClipboardMgr) then
    Result := (fViewer as IClipboardMgr).CanCopy;
end;

function TPreviewDlg.CanSelectAll: Boolean;
  {Checks if current view supports text selection.
    @return True if selection supported.
  }
begin
  Result := False;
  if Supports(fViewer, ISelectionMgr) then
    Result := (fViewer as ISelectionMgr).CanSelectAll;
end;

procedure TPreviewDlg.CopyToClipboard;
  {Copies selected text to clipboard from current view if view supports copying.
  }
begin
  if Supports(fViewer, IClipboardMgr) then
    (fViewer as IClipboardMgr).CopyToClipboard;
end;

class procedure TPreviewDlg.Execute(AOwner: TComponent;
  const ADocContent: string; const ADlgTitle: string = '');
  {Displays a document in the preview dialog.
    @param AOwner [in] Owning component.
    @param ADocContent [in] Content of document to be displayed (HTML, RTF or
      plain text).
    @param ADlgTitle [in] Title of dialog box. Default is used if ''.
  }
begin
  with TPreviewDlg.Create(AOwner) do
    try
      fDlgTitle := ADlgTitle;
      DocContent := ADocContent;
      ShowModal;
    finally
      Free;
    end;
end;

procedure TPreviewDlg.FormClose(Sender: TObject; var Action: TCloseAction);
  {Frees viewer object whether form is closed.
    @param Sender [in] Not used.
    @param Action [in] Not used.
  }
begin
  inherited;
  fViewer := nil; // required to prevent access violation
end;

procedure TPreviewDlg.GetViewerInfo(out Viewer: IInterface;
  out TabSheet: TTabSheet);
  {Gets information about required document viewer and tab sheet that contains
  it.
    @param Viewer [out] Interface to viewer frame.
    @param TabSheet [out] Tab sheet containing viewer frame.
  }
begin
  if URTFUtils.IsValidRTFCode(DocContent) then
  begin
    // RTF document
    TabSheet := tsRTF;
    Viewer := frRTF;
  end
  else if UHTMLUtils.IsValidHTMLCode(DocContent) then
  begin
    // HTML document
    TabSheet := tsHTML;
    Viewer := frHTML;
  end
  else
  begin
    // Plain text document
    TabSheet := tsText;
    Viewer := frText;
  end;
end;

procedure TPreviewDlg.InitForm;
  {Loads and displays the document being previewed.
  }
var
  TabSheet: TTabSheet;  // tab sheet containing preview frame
  Title: string;        // document title
begin
  inherited;
  // Display document (select tab, load doc and set submenu on into frame)
  GetViewerInfo(fViewer, TabSheet);
  // select tab containing required frame
  pcViews.ActivePage := TabSheet;
  // update required frame's popup menu and display document in it
  (fViewer as IPreview).SetPopupMenu(mnuPreview);
  // load content into preview and set dialog caption
  (fViewer as IPreview).Display(DocContent, Title);
  if fDlgTitle <> '' then
    Caption := fDlgTitle                // caller specified title - use it
  else if Title <> '' then
    Caption := Caption + ': ' + Title;  // use title extracted from document
end;

procedure TPreviewDlg.SelectAll;
  {Selects all text in current view if view supports selection.
  }
begin
  if Supports(fViewer, ISelectionMgr) then
    (fViewer as ISelectionMgr).SelectAll;
end;

end.

