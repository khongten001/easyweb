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

unit EWTypes;

interface

uses Vcl.Forms, Classes, Vcl.Graphics;

type
  TEWBootstrapVersion = (UseNewest, Bootstrap_v4_0, Bootstrap_v4_1, Bootstrap_v4_2, Bootstrap_v4_3);

  TEWBaseForm = class(TCustomForm);

  TEWClickItemEvent = procedure(Sender: TObject; AItem: string; AIndex: integer) of object;
  TEWNavItemClickEvent = procedure(Sender: TObject; AItem: TCollectionItem; ADropDownIndex: integer) of object;
  TEWDropDownClickItemEvent = procedure(Sender: TObject; AItem: TCollectionItem; ADropDownIndex: integer) of object;

  TEWConfirmResultEvent = procedure(Sender: TObject; AConfirmed: Boolean) of object;
  TEWPromptResultEvent = procedure(Sender: TObject; AValue: string) of object;

  TEWKeyEvent = procedure(Sender: TObject; Key: Word) of object;



  TEWButtonType = (btDefault, btBasic, btPrimary, btSecondary, btSuccess,
                   btDanger, btWarning, btInfo, btLight, btDark, btLink);
  TEWButtonGroupLayout = (bgHorizontal, bgVertical);

  TEWImageShape = (isDefault, isRoundedCorners, isCircle, isThumbnail);

  TEWFontVariant = (fvNormal, fvSmallCaps);

  TEWInputType = (itDate, itDateTime, itEmail, itHidden, itMonthYear, itNumber, itPassword, itText, itTime, itUrl);

  TEWPadding = class(TPersistent)
  private
    FLeft: integer;
    FTop: integer;
    FRight: integer;
    FBottom: integer;
    FOnChange: TNotifyEvent;
    function GetAsCssProperty: string;
    procedure Changed;
    procedure SetBottom(const Value: integer);
    procedure SetLeft(const Value: integer);
    procedure SetRight(const Value: integer);
    procedure SetTop(const Value: integer);
  public
    constructor Create(AOnChange: TNotifyEvent);
    procedure Assign(Source: TPersistent); override;
    property AsCssProperty: string read GetAsCssProperty;
  published
    property Left: integer read FLeft write SetLeft default 0;
    property Top: integer read FTop write SetTop default 0;
    property Right: integer read FRight write SetRight default 0;
    property Bottom: integer read FBottom write SetBottom default 0;
  end;

  TEWFont = class(TPersistent)
  private
    FFamily: string;
    FVariant: TEWFontVariant;
    FSize: integer;
    FStyle: TFontStyles;
    FColor: TColor;
    FOnChange: TNotifyEvent;
    procedure SetFamily(const Value: string);
    procedure SetColor(const Value: TColor);
    procedure SetSize(const Value: integer);
    procedure SetStyle(const Value: TFontStyles);
    procedure Changed;
    procedure SetVariant(const Value: TEWFontVariant);
    function GetAsCssProperty: string;
  protected
  public
    constructor Create(AOnChange: TNotifyEvent);
    procedure Assign(Source: TPersistent); override;
    property AsCssProperty: string read GetAsCssProperty;
  published
    property Family: string read FFamily write SetFamily;
    property Variant: TEWFontVariant read FVariant write SetVariant;
    property Size: integer read FSize write SetSize;// default 12;
    property Style: TFontStyles read FStyle write SetStyle;
    property Color: TColor read FColor write SetColor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


  function ColorToHex( Color : TColor ): string;

implementation

uses Windows, SysUtils;

function ColorToHex( Color : TColor ): string;
begin
  Result := '#'+
            IntToHex( GetRValue( Color ), 2 ) +
            IntToHex( GetGValue( Color ), 2 ) +
            IntToHex( GetBValue( Color ), 2 );
end;

{ TEWFont }

procedure TEWFont.Assign(Source: TPersistent);
begin
  FFamily := (Source as TEWFont).Family;
  FVariant := (Source as TEWFont).Variant;
  FSize := (Source as TEWFont).Size;
  FStyle := (Source as TEWFont).Style;
  FColor := (Source as TEWFont).Color;
end;

procedure TEWFont.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TEWFont.Create(AOnChange: TNotifyEvent);
begin
  inherited Create;;
  FOnChange := AOnChange;
end;

function TEWFont.GetAsCssProperty: string;

begin
  Result := ' ';
  if (fsBold in FStyle) then Result := Result + 'bold ';
  if (fsItalic in FStyle) then Result := Result + 'italic ';
  if (fsUnderline in FStyle) then Result := Result + 'underline ';
  //Result := Result + FVariant+' ';

  Result := Result + FSize.ToString+'px ';
  Result := Result + FFamily+' ';
  Result := Trim(StringReplace(Result, '  ', ' ', [rfReplaceAll]));
  if FVariant = fvSmallCaps then Result := Result + ';font-variant:small-caps;';
end;

procedure TEWFont.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TEWFont.SetFamily(const Value: string);
begin
  if FFamily <> Value then
  begin
    FFamily := Value;
    Changed;
  end;
end;

procedure TEWFont.SetSize(const Value: integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    Changed;
  end;
end;

procedure TEWFont.SetStyle(const Value: TFontStyles);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed;
  end;
end;


procedure TEWFont.SetVariant(const Value: TEWFontVariant);
begin
  if FVariant <> Value then
  begin
    FVariant := Value;
    Changed;
  end;
end;

{ TEWPadding }

procedure TEWPadding.Assign(Source: TPersistent);
begin
  Left := (Source as TEWPadding).Left;
  Top := (Source as TEWPadding).Top;
  Right := (Source as TEWPadding).Right;
  Bottom := (Source as TEWPadding).Bottom;
end;

procedure TEWPadding.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TEWPadding.Create(AOnChange: TNotifyEvent);
begin
  inherited Create;
  FLeft := 0;
  FTop := 0;
  FRight := 0;
  FBottom := 0;
  FOnChange := AOnChange;
end;

function TEWPadding.GetAsCssProperty: string;
begin
  Result := '';
  if (FLeft = 0) and (FTop = 0) and (FRight = 0) and (FBottom = 0) then
    Exit;
  Result := Format('padding: %dpx %dpx %dpx %dpx', [FTop, FRight, FBottom, FLeft]);
end;

procedure TEWPadding.SetBottom(const Value: integer);
begin
  if FBottom <> Value then
  begin
    FBottom := Value;
    Changed;
  end;
end;

procedure TEWPadding.SetLeft(const Value: integer);
begin
  if FLeft <> Value then
  begin
    FLeft := Value;
    Changed;
  end;
end;

procedure TEWPadding.SetRight(const Value: integer);
begin
  if FRight <> Value then
  begin
    FRight := Value;
    Changed;
  end;
end;

procedure TEWPadding.SetTop(const Value: integer);
begin
  if FTop <> Value then
  begin
    FTop := Value;
    Changed;
  end;
end;

end.
