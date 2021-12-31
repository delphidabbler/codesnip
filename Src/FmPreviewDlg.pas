{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that is used to preview or display plain text, HTML
 * and Rich text documents.
}


unit FmPreviewDlg;


interface


uses
  // Delphi
  ImgList, Controls, Classes, ActnList, Menus, Forms, ComCtrls, StdCtrls,
  ExtCtrls,
  // Project
  FmGenericViewDlg, FrBrowserBase, FrHTMLPreview, FrMemoPreview, FrRTFPreview,
  FrTextPreview, UBaseObjects, UEncodings;


type
  ///  <summary>
  ///  Enumeration of types of document that can be displayed by preview dialog
  ///  box.
  ///  </summary>
  TPreviewDocType = (
    dtPlainText,  // plain text document
    dtHTML,       // HTML document
    dtRTF         // rich text format document
  );

type
  ///  <summary>
  ///  Dialog box used to preview text, HTML and Rich text documents.
  ///  </summary>
  TPreviewDlg = class(TGenericViewDlg, INoPublicConstruct)
    actCopy: TAction;
    actSelectAll: TAction;
    alPreview: TActionList;
    frRTF: TRTFPreviewFrame;
    frHTML: THTMLPreviewFrame;
    frText: TTextPreviewFrame;
    miCopy: TMenuItem;
    miSelectAll: TMenuItem;
    mnuPreview: TPopupMenu;
    pcViews: TPageControl;
    tsHTML: TTabSheet;
    tsRTF: TTabSheet;
    tsText: TTabSheet;
    ///  <summary>Copies preview text to clipboard.</summary>
    procedure actCopyExecute(Sender: TObject);
    ///  <summary>Enables / disables Copy action depending or whether copying is
    ///  supported in current view.</summary>
    procedure actCopyUpdate(Sender: TObject);
    ///  <summary>Selects all preview text.</summary>
    procedure actSelectAllExecute(Sender: TObject);
    ///  <summary>Enables / disables Select All action depending on whether
    /// selection is supported in current view.</summary>
    procedure actSelectAllUpdate(Sender: TObject);
    ///  <summary>Frees viewer object when form is closed.</summary>
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  strict private
    var
      ///  <summary>Reference to viewer frame's IInterface.</summary>
      fViewer: IInterface;
      ///  <summary>Content of document being displayed.</summary>
      fDocContent: TEncodedData;
      ///  <summary>Type (format) of document to be displayed.</summary>
      fDocType: TPreviewDocType;
      ///  <summary>Dialog box title.</summary>
      fDlgTitle: string;
    ///  <summary>Gets information about required document viewer and tab sheet
    ///  that contains it.</summary>
    ///  <param name="Viewer">IInterface [out] Interface to viewer frame.
    ///  </param>
    ///  <param name="TabSheet">TTabSheet [out] Tab sheet containing viewer
    ///  frame.</param>
    procedure GetViewerInfo(out Viewer: IInterface; out TabSheet: TTabSheet);
    ///  <summary>Checks if current view supports copying to clipboard.
    ///  </summary>
    function CanCopy: Boolean;
    ///  <summary>Checks if current view supports text selection.</summary>
    function CanSelectAll: Boolean;
    ///  <summary>Copies selected text to clipboard from current view if viewer
    ///  supports copying.</summary>
    procedure CopyToClipboard;
    ///  <summary>Selects all text in current view if viewer supports selection.
    ///  </summary>
    procedure SelectAll;
    ///  <summary>Finds tab sheet that is parent of a given frame.</summary>
    class function FindParentTabSheet(const Frame: TFrame): TTabSheet;
  strict protected
    ///  <summary>Loads and displays the document being previewed.</summary>
    procedure InitForm; override;
  public
    ///  <summary>Displays a document in preview dialog box.</summary>
    ///  <param name="AOwner">TComponent [in] Component that owns dialog box.
    ///  </param>
    ///  <param name="ADocContent">TEncodedData [in] Content of document to be
    ///  displayed.</param>
    ///  <param name="ADocType">TPreviewDocType [in] Type or format of document
    ///  to be displayed: HTML, RTF or plain text.</param>
    ///  <param name="ADlgTitle">string [in] Optional dialog box title. If not
    ///  supplied default title is used.</param>
    class procedure Execute(AOwner: TComponent; const ADocContent: TEncodedData;
      const ADocType: TPreviewDocType; const ADlgTitle: string = '');
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  IntfFrameMgrs, IntfPreview;


{$R *.dfm}


{ TPreviewDlg }

procedure TPreviewDlg.actCopyExecute(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TPreviewDlg.actCopyUpdate(Sender: TObject);
begin
  actCopy.Enabled := CanCopy;
end;

procedure TPreviewDlg.actSelectAllExecute(Sender: TObject);
begin
  SelectAll;
end;

procedure TPreviewDlg.actSelectAllUpdate(Sender: TObject);
begin
  actSelectAll.Enabled := CanSelectAll;
end;

function TPreviewDlg.CanCopy: Boolean;
begin
  Result := False;
  if Supports(fViewer, IClipboardMgr) then
    Result := (fViewer as IClipboardMgr).CanCopy;
end;

function TPreviewDlg.CanSelectAll: Boolean;
begin
  Result := False;
  if Supports(fViewer, ISelectionMgr) then
    Result := (fViewer as ISelectionMgr).CanSelectAll;
end;

procedure TPreviewDlg.CopyToClipboard;
begin
  if Supports(fViewer, IClipboardMgr) then
    (fViewer as IClipboardMgr).CopyToClipboard;
end;

class procedure TPreviewDlg.Execute(AOwner: TComponent;
  const ADocContent: TEncodedData; const ADocType: TPreviewDocType;
  const ADlgTitle: string);
begin
  with InternalCreate(AOwner) do
    try
      fDlgTitle := ADlgTitle;
      fDocContent := TEncodedData.Create(ADocContent);
      fDocType := ADocType;
      ShowModal;
    finally
      Free;
    end;
end;

class function TPreviewDlg.FindParentTabSheet(const Frame: TFrame): TTabSheet;
var
  ParentCtrl: TWinControl;  // moves up tree of parent controls
begin
  ParentCtrl := Frame.Parent;
  while Assigned(ParentCtrl) and not (ParentCtrl is TTabSheet) do
    ParentCtrl := ParentCtrl.Parent;
  Assert(Assigned(ParentCtrl),
    ClassName + '.FindParentTabSheet: Tab sheet not found.');
  Result := ParentCtrl as TTabSheet;
end;

procedure TPreviewDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  fViewer := nil; // required to prevent access violation
end;

procedure TPreviewDlg.GetViewerInfo(out Viewer: IInterface;
  out TabSheet: TTabSheet);
var
  Frame: TFrame;
begin
  case fDocType of
    dtPlainText: Frame := frText;
    dtHTML: Frame := frHTML;
    dtRTF: Frame := frRTF;
    else Frame := nil;
  end;
  Assert(Assigned(Frame), ClassName + '.GetViewerInfo: No frame assigned');
  Viewer := Frame as IInterface;
  TabSheet := FindParentTabSheet(Frame);
end;

procedure TPreviewDlg.InitForm;
var
  TabSheet: TTabSheet;  // tab sheet containing preview frame
begin
  inherited;
  // Display document (select tab, load doc and set submenu on into frame)
  GetViewerInfo(fViewer, TabSheet);
  // select tab containing required frame
  pcViews.ActivePage := TabSheet;
  // update required frame's popup menu and display document in it
  (fViewer as IPreview).SetPopupMenu(mnuPreview);
  // load content into preview and set dialog caption
  (fViewer as IPreview).Display(fDocContent);
  if fDlgTitle <> '' then
    Caption := fDlgTitle; // caller specified title - use it
end;

procedure TPreviewDlg.SelectAll;
begin
  if Supports(fViewer, ISelectionMgr) then
    (fViewer as ISelectionMgr).SelectAll;
end;

end.

