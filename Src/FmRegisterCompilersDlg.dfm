inherited RegisterCompilersDlg: TRegisterCompilersDlg
  Caption = 'RegisterCompilersDlg'
  ClientHeight = 453
  ExplicitWidth = 474
  ExplicitHeight = 482
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvlBottom: TBevel
    Top = 403
    ExplicitTop = 403
  end
  inherited pnlBody: TPanel
    Width = 433
    Height = 389
    ExplicitWidth = 433
    ExplicitHeight = 389
    object lblDesc: TLabel
      Left = 0
      Top = 0
      Width = 33
      Height = 13
      Caption = 'lblDesc'
      FocusControl = clbCompilers
    end
    object clbCompilers: TCheckListBox
      Left = 0
      Top = 32
      Width = 361
      Height = 56
      IntegralHeight = True
      ItemHeight = 13
      TabOrder = 0
    end
    inline frmNotes: TFixedHTMLDlgFrame
      Left = 0
      Top = 159
      Width = 361
      Height = 199
      TabOrder = 1
      TabStop = True
      ExplicitTop = 159
      ExplicitWidth = 361
      ExplicitHeight = 199
      inherited pnlBrowser: TPanel
        Width = 361
        Height = 199
        ExplicitWidth = 361
        ExplicitHeight = 199
        inherited wbBrowser: TWebBrowser
          Width = 361
          Height = 199
          ExplicitWidth = 353
          ExplicitHeight = 199
          ControlData = {
            4C0000004F250000911400000000000000000000000000000000000000000000
            000000004C000000000000000000000001000000E0D057007335CF11AE690800
            2B2E126208000000000000004C0000000114020000000000C000000000000046
            8000000000000000000000000000000000000000000000000000000000000000
            00000000000000000100000000000000000000000000000000000000}
        end
      end
    end
    object chkDontShowAgain: TCheckBox
      Left = 0
      Top = 372
      Width = 145
      Height = 17
      Caption = '&Don'#39't show this again'
      TabOrder = 2
    end
  end
  inherited btnHelp: TButton
    Left = 362
    Top = 420
    ExplicitLeft = 362
    ExplicitTop = 420
  end
  inherited btnCancel: TButton
    Left = 282
    Top = 420
    ExplicitLeft = 282
    ExplicitTop = 420
  end
  inherited btnOK: TButton
    Left = 201
    Top = 420
    Default = False
    ExplicitLeft = 201
    ExplicitTop = 420
  end
end
