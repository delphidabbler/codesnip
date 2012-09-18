inherited ProgramUpdatesDlg: TProgramUpdatesDlg
  Caption = 'Check For Program Updates'
  ExplicitWidth = 474
  ExplicitHeight = 354
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    object lblProgram: TLabel
      Left = 0
      Top = 0
      Width = 54
      Height = 13
      Caption = 'Please wait'
    end
    object lblPreReleaseMsg: TLabel
      Left = 0
      Top = 50
      Width = 345
      Height = 33
      AutoSize = False
      Caption = 
        'Preview and beta releases are not reported here. They are announ' +
        'ced in the CodeSnip news feed.'
      Visible = False
      WordWrap = True
    end
    object btnProgUpdate: TButton
      Left = 0
      Top = 19
      Width = 129
      Height = 25
      Caption = 'Go To &Download Page'
      TabOrder = 0
      Visible = False
      OnClick = btnProgUpdateClick
    end
  end
end
