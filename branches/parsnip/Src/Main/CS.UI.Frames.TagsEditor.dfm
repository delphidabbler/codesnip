object TagsEditorFrame: TTagsEditorFrame
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  OnResize = FrameResize
  object sbTagEditors: TScrollBox
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    HorzScrollBar.Visible = False
    Align = alClient
    BevelEdges = []
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    ParentBackground = True
    TabOrder = 0
    object pnlEditors: TPanel
      Left = 0
      Top = 0
      Width = 320
      Height = 41
      Align = alTop
      BevelEdges = []
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 0
    end
  end
  object alEditor: TActionList
    Left = 144
    Top = 104
    object actMore: TAction
      Caption = '&More...'
      ShortCut = 24641
      OnExecute = actMoreExecute
    end
  end
end
