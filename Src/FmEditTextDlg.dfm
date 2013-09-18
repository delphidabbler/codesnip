inherited EditTextDlg: TEditTextDlg
  Caption = 'EditTextDlg'
  ExplicitWidth = 474
  ExplicitHeight = 354
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 321
    Height = 60
    ExplicitWidth = 321
    ExplicitHeight = 60
    object lblPrompt: TLabel
      Left = 0
      Top = 0
      Width = 43
      Height = 13
      Caption = 'lblPrompt'
      FocusControl = edText
    end
    object edText: TEdit
      Left = 0
      Top = 19
      Width = 321
      Height = 21
      TabOrder = 0
      Text = 'edText'
    end
  end
  inherited btnHelp: TButton
    Visible = False
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
