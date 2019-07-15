{***************************************************************************}
{                                                                           }
{           EasyWeb - Bootstrap Framework for Delphi                        }
{                                                                           }
{           Copyright (c) 2019 Graham Murt                                  }
{                                                                           }
{           https://bitbucket.org/gmurt/easyweb/                            }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit EWLabel;

interface

uses Classes, EWIntf, EWBase, VCL.Graphics, EWTypes;

type
  TEWLabel = class(TEWBaseObject, IEWLabel)
  private
    FBackgroundColor: TColor;
    FAlignment: TAlignment;
    FText: string;
    FVertAlignment: TVerticalAlignment;
    FFont: TEWFont;
    FToolTip: string;

    function GetText: string;
    procedure SetText(const Value: string);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetVertAlignment(const Value: TVerticalAlignment);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetFont(const Value: TEWFont);
    procedure FontChanged(Sender: TObject);
    procedure SetToolTip(const Value: string);
  protected
    procedure BuildCss(AProperties: TStrings); override;
    function DesignTimeCaption: string; override;
    function GenerateHtml: string; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clNone;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Font: TEWFont read FFont write SetFont;
    property Text: string read GetText write SetText;
    property ToolTip: string read FToolTip write SetToolTip;
    property VertAlignment: TVerticalAlignment read FVertAlignment write SetVertAlignment default taVerticalCenter;
  end;

implementation

uses Types, SysUtils, EWConst;

{ TEWLabel }

procedure TEWLabel.BuildCss(AProperties: TStrings);
begin
 inherited;
 case FAlignment of
    taLeftJustify: AProperties.Values[C_TEXT_ALIGN] := C_LEFT;
    taCenter: AProperties.Values[C_TEXT_ALIGN] := C_CENTER;
    taRightJustify: AProperties.Values[C_TEXT_ALIGN] := C_RIGHT;
  end;
  if FBackgroundColor <> clNone then
    AProperties.Values[C_BACKGROUND_COLOR] := ColorToHex(FBackgroundColor);
  AProperties.Values[C_FONT] := StringReplace(FFont.AsCssProperty, '"', '''', [rfReplaceAll]);
  AProperties.Values[C_COLOR] := ColorToHex(FFont.Color);
end;

constructor TEWLabel.Create(AOwner: TComponent);
begin
  inherited;
  FAlignment := taLeftJustify;
  FVertAlignment := taVerticalCenter;
  FBackgroundColor := clNone;
  FFont := TEWFont.Create(FontChanged);
end;

function TEWLabel.DesignTimeCaption: string;
begin
  inherited;
  Result := FText;
  if FText = '' then
    Result := Name;
end;

destructor TEWLabel.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TEWLabel.FontChanged(Sender: TObject);
begin
  Changed;
end;

function TEWLabel.GenerateHtml: string;
begin
  inherited;
  Result := ReplaceTokens(C_HTML_LABEL);
  Result := StringReplace(Result, '%text%', FText, []);
  Result := StringReplace(Result, '%tooltip%', FToolTip, []);
end;

function TEWLabel.GetText: string;
begin
  Result := FText;
end;

procedure TEWLabel.Paint;
var
  r: TRect;
  AStr: string;
  AFormat: TTextFormat;
begin
  inherited;
  r := ClientRect;
  AFormat := [tfSingleLine];
  case FAlignment of
    taLeftJustify: AFormat := AFormat + [tfLeft];
    taCenter: AFormat := AFormat + [tfCenter];
    taRightJustify: AFormat := AFormat + [tfRight];
  end;
  case FVertAlignment of
    taAlignTop: AFormat := AFormat + [tfTop];
    taVerticalCenter: AFormat := AFormat + [tfVerticalCenter];
    taAlignBottom: AFormat := AFormat + [tfBottom];
  end;
  AStr := DesignTimeCaption;
  Canvas.TextRect(r, AStr, AFormat);
end;

procedure TEWLabel.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed;
  end;
end;

procedure TEWLabel.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    Changed;
  end;
end;

procedure TEWLabel.SetFont(const Value: TEWFont);
begin
  FFont.Assign(Value);
  Changed;
end;

procedure TEWLabel.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TEWLabel.SetToolTip(const Value: string);
begin
  if FToolTip <> Value then
  begin
    FToolTip := Value;
    Changed;
  end;
end;

procedure TEWLabel.SetVertAlignment(const Value: TVerticalAlignment);
begin
  if FVertAlignment <> Value then
  begin
    FVertAlignment := Value;
    Changed;
  end;
end;

end.
