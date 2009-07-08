{
 * FmWaitDlg.pas
 *
 * Implements a borderless dialog box that displays a message. Designed for
 * display when application is waiting.
 *
 * v1.0 of 01 Dec 2006  - Original version.
 * v2.0 of 08 Feb 2007  - Modified to work with revised TBaseForm base class and
 *                        new form customisation and alignment framework:
 *                        - Removed form alignment code. This is now provided by
 *                          an TFormAligner object.
 *                        - Moved code from FormShow to InitForm method.
 *                        - Removed FormShow and FormCreate methods.
 * v2.1 of 25 Sep 2007  - Changed to use renamed IFormAligner interface.
 * v2.2 of 02 Jun 2008  - Removed progress bar and timer components and replaced
 *                        with dynamically created custom marquee control. This
 *                        new control avoids a display problem with old control
 *                        on Vista.
 * v2.3 of 11 Jun 2008  - Changed to use window handle of owner control as
 *                        form's parent if suitable, or to use active form or
 *                        main form as parent. This change needed for app to
 *                        work correctly with Vista task bar.
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
 * The Original Code is FmWaitDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s): None
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
  private
    fMarquee: TMarquee;
      {Custom marquee component instance}
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
      {Triggered when form's caption is set. Displays form caption in
      lblCaption.
        @var Msg [in/out] Not used.
      }
  protected
    function GetAligner: IFormAligner; override;
      {Creates and returns reference to an object that is used to align the form
      to the owner.
        @return Required aligner object instance.
      }
    procedure CustomiseForm; override;
      {Creates and locates owned custom marquee component.
      }
    procedure InitForm; override;
      {Sets hourglass cursor and starts marquee when form is shown.
      }
  end;


implementation


uses
  // Project
  UDlgHelper, UFormAligner;


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
  {Creates and locates owned custom marquee component.
  }
begin
  inherited;
  fMarquee := TMarquee.Create(Self);
  fMarquee.Parent := pnlMain;
  fMarquee.Left := 8;
  fMarquee.Width := pnlMain.ClientWidth - 16;
  fMarquee.Top := lblCaption.Top + lblCaption.Height + 12;
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

