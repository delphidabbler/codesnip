{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Launcher program that runs CodeSnip in portable mode.
}


program CodeSnipPortable;

{$RESOURCE LauncherResources.res}
{$RESOURCE LauncherVerInfo.res}

uses
  Windows,
  ShellApi,
  SysUtils;

var
  SI: TShellExecuteInfo;

const
  ParaBreak = #13#10#13#10;

resourcestring
  sCantRunErrorPara1 = 'Can''t run CodeSnip.';
  sCantRunErrorPara2 = 'CodeSnip.exe must be in the same directory as this '
    + 'program.';
  sUnexpectedError = 'UNEXPECTED ERROR:';

procedure ErrorMessage(const Msg: string);
const
  DlgCaption = 'CodeSnip Portable';
begin
  MessageBox(0, PChar(Msg), DlgCaption, MB_OK);
end;

begin
  try
    FillChar(SI, SizeOf(SI), 0);
    SI.cbSize := SizeOf(SI);
    SI.fMask := SEE_MASK_DEFAULT or SEE_MASK_FLAG_NO_UI;
    SI.Wnd := 0;
    SI.lpVerb := 'open';
    SI.lpFile := PChar(ExtractFilePath(ParamStr(0)) + 'CodeSnip.exe');
    SI.lpParameters := '-portable';
    SI.lpDirectory := nil;
    SI.nShow := SW_SHOW;
    if not ShellExecuteEx(@SI) then
      ErrorMessage(sCantRunErrorPara1 + ParaBreak + sCantRunErrorPara2);
  except
    on E: Exception do
      ErrorMessage(sUnexpectedError + ParaBreak + E.Message);
  end;
end.
