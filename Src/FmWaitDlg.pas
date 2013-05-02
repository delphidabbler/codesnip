{
 * FmWaitDlg.pas
 *
 * Implements a borderless dialog box that displays a message. Designed for
 * display when application is waiting.
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
 * The Original Code is FmWaitDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmWaitDlg;


interface


uses
  // Delphi
  Controls, StdCtrls, Classes, ExtCtrls, Forms, Messages,
  // Project
  FmBase, IntfAligner, UMarquee;


type

  {
  TWaitDlg:
    Implements dialog box for display when application is waiting. Dialog box
    is borderless, displays via its Caption property and shows an animation.
  }
  TWaitDlg = class(TBaseForm)
    lblCaption: TLabel;
    pnlMain: TPanel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  strict private
    fMarquee: TMarquee; // Custom marquee component instance
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
      {Triggered when form's caption is set. Displays form caption in
      lblCaption.
        @var Msg [in/out] Not used.
      }
  strict protected
    function GetAligner: IFormAligner; override;
      {Creates and returns reference to an object that is used to align the form
      to the owner.
        @return Required aligner object instance.
      }
    procedure CustomiseForm; override;
      {Sets required UI font for label and creates and locates custom marquee
      component.
      }
    procedure InitForm; override;
      {Sets hourglass cursor and starts marquee when form is shown.
      }
  end;


implementation


uses
  // Project
  UDlgHelper, UFontHelper, UFormAligner, UGraphicUtils;


{$R *.dfm}


{ TWaitDlg }

procedure TWaitDlg.CMTextChanged(var Msg: TMessage);
  {Triggered when form's caption is set. Displays form caption in lblCaption.
    @var Msg [in/out] Not used.
  }
begin
  inherited;
  lblCaption.Caption := Text;
end;

procedure TWaitDlg.CustomiseForm;
  {Sets required UI font for label and creates and locates custom marquee
  component.
  }
begin
  inherited;
  // Update label font to use UI default font: it is set to have bold style,
  // which is preserved. We also have to ensure label is correct size
  TFontHelper.SetDefaultBaseFont(lblCaption.Font, False);
  lblCaption.Height := StringExtent(lblCaption.Caption, lblCaption.Font).cy;
  // Create and locate marquee
  fMarquee := TMarquee.Create(Self);
  fMarquee.Parent := pnlMain;
  fMarquee.Left := 8;
  fMarquee.Width := pnlMain.ClientWidth - 16;
  fMarquee.Top := lblCaption.Top + lblCaption.Height + 8;
  fMarquee.Height := 13;
end;

procedure TWaitDlg.FormClose(Sender: TObject; var Action: TCloseAction);
  {Resets default cursor and halts marquee when form closes.
    @param Sender [in] Not used.
    @param Action [in/out] Not used or set.
  }
begin
  inherited;
  fMarquee.Stop;
  Screen.Cursor := crDefault;
end;

procedure TWaitDlg.FormCreate(Sender: TObject);
  {Handles form's OnCreate event. Sets handle of Owner control as parent of
  form's window. If Owner doesn't have handle then either active form or main
  form used as parent.
    @param Sender [in] Not used.
  }
begin
  inherited;
  TDlgHelper.SetDlgParentToOwner(Self);
end;

function TWaitDlg.GetAligner: IFormAligner;
  {Creates and returns reference to an object that is used to align the form to
  the owner.
    @return Required aligner object instance.
  }
begin
  Result := TFormAligner.Create;
end;

procedure TWaitDlg.InitForm;
  {Sets hourglass cursor and starts marquee when form is shown.
  }
begin
  inherited;
  Screen.Cursor := crHourGlass;
  fMarquee.Start;
end;

end.

