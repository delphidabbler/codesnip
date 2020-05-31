{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a frame used to display previews of RTF documents.
}


unit FrRTFPreview;


interface


uses
  // Delphi
  Classes, StdCtrls, Controls, ExtCtrls, ComCtrls, Menus,
  // Project
  FrMemoPreview, IntfFrameMgrs, IntfPreview, UEncodings;



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
    procedure LoadContent(const DocContent: TEncodedData); override;
      {Loads document into rich edit control.
        @param DocContent [in] Valid RTF document to be displayed.
      }
  public
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
  // Project
  URTFUtils;


{$R *.dfm}


{ TRTFPreviewFrame }

function TRTFPreviewFrame.GetMemoCtrl: TCustomMemo;
  {Gets reference to rich edit control used to display RTF code.
    @return Required TRichEdit reference.
  }
begin
  Result := reView;
end;

procedure TRTFPreviewFrame.LoadContent(const DocContent: TEncodedData);
  {Loads document into rich edit control.
    @param DocContent [in] Valid RTF document to be displayed.
  }
begin
  TRichEditHelper.Load(reView, TRTF.Create(DocContent));
end;

procedure TRTFPreviewFrame.SetPopupMenu(const Menu: TPopupMenu);
  {Sets pop up menu to be displayed when rich edit control right clicked.
    @param Menu [in] Required popup menu.
  }
begin
  reView.PopupMenu := Menu;
end;

end.

