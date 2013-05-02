{
 * FrRTFPreview.pas
 *
 * Frame used to display previews of RTF documents.
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
 * The Original Code is FrRTFPreview.pas
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


unit FrRTFPreview;


interface


uses
  // Delphi
  Classes, StdCtrls, Controls, ExtCtrls, ComCtrls, Menus,
  // Project
  FrMemoPreview, IntfFrameMgrs, IntfPreview;



type

  {
  TRTFPreviewFrame:
    Frame used to display previews of RTF documents.
  }
  TRTFPreviewFrame = class(TMemoPreviewFrame,
    IPreview, IClipboardMgr, ISelectionMgr
  )
    reView: TRichEdit;
  strict protected
    { Implementation of base class' abstract methods }
    function GetMemoCtrl: TCustomMemo; override;
      {Gets reference to rich edit control used to display RTF code.
        @return Required TRichEdit reference.
      }
    function GetTitle(const DocContent: string): string; override;
      {Extracts document title from RTF document if possible.
        @param DocContent [in] Document content as RTF.
        @return Required tile or '' if no title present in RTF.
      }
    procedure LoadContent(const DocContent: string); override;
      {Loads document into rich edit control.
        @param DocContent [in] Valid RTF document to be displayed.
      }
  protected // do not made strict
    { IPreview: Partially implemented in base class }
    procedure SetPopupMenu(const Menu: TPopupMenu);
      {Sets pop up menu to be displayed when rich edit control right clicked.
        @param Menu [in] Required popup menu.
      }
    { IClipboardMgr: Implemented in base class }
    { ISelectionMgr: Implemented in base class }
  end;


implementation


uses
  // Delphi
  SysUtils, StrUtils,
  // Project
  UEncodings, URTFUtils;


{$R *.dfm}


{ TRTFPreviewFrame }

function TRTFPreviewFrame.GetMemoCtrl: TCustomMemo;
  {Gets reference to rich edit control used to display RTF code.
    @return Required TRichEdit reference.
  }
begin
  Result := reView;
end;

function TRTFPreviewFrame.GetTitle(const DocContent: string): string;
  {Extracts document title from RTF document if possible.
    @param DocContent [in] Document content as RTF.
    @return Required tile or '' if no title present in RTF.
  }
var
  TitleStart: Integer;        // start of doc title in rtf
  TitleEnd: Integer;          // end of doc title in rtf
const
  cTitleControl = '{\title';  // rtf title control
begin
  Result := '';
  TitleStart := PosEx(cTitleControl, DocContent);
  if TitleStart > 0 then
  begin
    Inc(TitleStart, Length(cTitleControl));
    TitleEnd := PosEx('}', DocContent, TitleStart + 1);
    Result := Trim(Copy(DocContent, TitleStart, TitleEnd - TitleStart));
  end;
end;

procedure TRTFPreviewFrame.LoadContent(const DocContent: string);
  {Loads document into rich edit control.
    @param DocContent [in] Valid RTF document to be displayed.
  }
begin
  RTFLoadFromString(reView, StringToASCIIString(DocContent));
end;

procedure TRTFPreviewFrame.SetPopupMenu(const Menu: TPopupMenu);
  {Sets pop up menu to be displayed when rich edit control right clicked.
    @param Menu [in] Required popup menu.
  }
begin
  reView.PopupMenu := Menu;
end;

end.

