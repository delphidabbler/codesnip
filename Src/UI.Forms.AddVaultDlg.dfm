inherited AddVaultDlg: TAddVaultDlg
  Caption = 'Add Vault'
  ExplicitWidth = 565
  ExplicitHeight = 436
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    inherited pcWizard: TPageControl
      ActivePage = tsIntro
      object tsIntro: TTabSheet
        Caption = 'tsIntro'
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblIntroDesc1: TLabel
          Left = 0
          Top = 3
          Width = 360
          Height = 26
          AutoSize = False
          Caption = 
            'You can use this wizard to add an existing vault directory to th' +
            'e database.'
          WordWrap = True
        end
        object lblIntroDesc2: TLabel
          Left = 0
          Top = 35
          Width = 363
          Height = 30
          AutoSize = False
          Caption = 'You will need to provide the following information:'
          WordWrap = True
        end
        object lblIntroDesc3: TLabel
          Left = 0
          Top = 59
          Width = 363
          Height = 74
          AutoSize = False
          Caption = '- a name for the vault'#13#10'- the directory containing the vault'
          WordWrap = True
        end
        object lblIntroDesc4: TLabel
          Left = 0
          Top = 123
          Width = 366
          Height = 23
          AutoSize = False
          Caption = 'When ready, click the Next button below to begin.'
          WordWrap = True
        end
      end
      object tsName: TTabSheet
        Caption = 'tsName'
        ImageIndex = 1
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblName: TLabel
          Left = 3
          Top = 3
          Width = 334
          Height = 38
          AutoSize = False
          Caption = 'Enter a &name for the vault:'
          WordWrap = True
        end
        object edName: TEdit
          Left = 0
          Top = 35
          Width = 328
          Height = 21
          TabOrder = 0
        end
      end
      object tsSourcePath: TTabSheet
        Caption = 'tsSourcePath'
        ImageIndex = 2
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblSourcePath: TLabel
          Left = 3
          Top = 3
          Width = 334
          Height = 38
          AutoSize = False
          Caption = 'Enter the full path of the &directory containing the vault:'
          FocusControl = edSourcePath
          WordWrap = True
        end
        object lblSourcePathDesc: TLabel
          Left = 3
          Top = 83
          Width = 334
          Height = 38
          AutoSize = False
          Caption = 
            'The chosen directory must contain valid vault data in any of the' +
            ' supported formats.'
          WordWrap = True
        end
        object edSourcePath: TEdit
          Left = 0
          Top = 40
          Width = 332
          Height = 21
          TabOrder = 0
        end
        object btnSourcePath: TButton
          Left = 338
          Top = 40
          Width = 27
          Height = 21
          Action = actBrowseSourcePath
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
        end
      end
      object tsConfirm: TTabSheet
        Caption = 'tsConfirm'
        ImageIndex = 4
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblConfirmDesc1: TLabel
          Left = 0
          Top = 0
          Width = 334
          Height = 50
          AutoSize = False
          Caption = 
            'Ready to add the vault. Please review the information below. If ' +
            'anything is wrong go back to correct it.'
          WordWrap = True
        end
        object lblConfirmDesc2: TLabel
          Left = 0
          Top = 217
          Width = 327
          Height = 13
          AutoSize = False
          Caption = 
            'When content press the Install button. This step cannot be undon' +
            'e.'
          WordWrap = True
        end
        object lvConfirm: TListView
          Left = 3
          Top = 32
          Width = 365
          Height = 179
          Columns = <
            item
              Width = 120
            end
            item
              Width = 200
            end>
          ReadOnly = True
          ShowColumnHeaders = False
          TabOrder = 0
          ViewStyle = vsReport
        end
      end
      object tsFinish: TTabSheet
        Caption = 'tsFinish'
        ImageIndex = 4
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblFinishMsg1: TLabel
          Left = 0
          Top = 3
          Width = 360
          Height = 26
          AutoSize = False
          Caption = 'The chosen vault has been added.'
          WordWrap = True
        end
        object lblFinishMsg2: TLabel
          Left = 0
          Top = 35
          Width = 363
          Height = 30
          AutoSize = False
          Caption = 
            'If you wish to move the new vault to a different location you sh' +
            'ould choose the Vault | Move Vault menu option after closing thi' +
            's wizard.'
          WordWrap = True
        end
        object lblFinishMsg3: TLabel
          Left = 3
          Top = 95
          Width = 360
          Height = 26
          AutoSize = False
          Caption = 'Please click the Finish button to close the wizard.'
          WordWrap = True
        end
      end
    end
  end
  object alMain: TActionList
    Left = 24
    Top = 344
    object actBrowseSourcePath: TAction
      Caption = '...'
      OnExecute = actBrowseSourcePathExecute
    end
  end
end
