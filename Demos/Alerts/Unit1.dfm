object Form62: TForm62
  Left = 0
  Top = 0
  Width = 770
  Height = 316
  Color = clWhite
  PixelsPerInch = 96
  TextHeight = 13
  object EWButton1: TEWButton
    Left = 48
    Top = 105
    Width = 185
    Height = 41
    OnClick = EWButton1Click
    ButtonType = btInfo
    Text = 'Confirmation Box'
  end
  object EWLabel1: TEWLabel
    Left = 256
    Top = 105
    Width = 249
    Height = 41
    Font.Family = 'Arial, Helvetica, sans-serif'
    Font.Size = 26
    Font.Style = []
    Font.Color = clBlack
    Text = ''
  end
  object EWButton2: TEWButton
    Left = 48
    Top = 41
    Width = 185
    Height = 41
    OnClick = EWButton2Click
    ButtonType = btSuccess
    Text = 'Show Message'
  end
  object EWButton3: TEWButton
    Left = 48
    Top = 169
    Width = 185
    Height = 41
    OnClick = EWButton3Click
    ButtonType = btDanger
    Text = 'Input Prompt'
  end
  object EWLabel2: TEWLabel
    Left = 256
    Top = 169
    Width = 249
    Height = 41
    Font.Family = 'Arial, Helvetica, sans-serif'
    Font.Size = 26
    Font.Style = []
    Font.Color = clBlack
    Text = ''
  end
  object EWDialog1: TEWDialog
    OnConfirmResult = EWDialog1ConfirmResult
    OnPrompt = EWDialog1Prompt
    Left = 264
    Top = 40
  end
end
