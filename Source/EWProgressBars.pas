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

unit EWProgressBars;

interface

uses Classes, EWIntf, EWBase, EWTypes;

type
  TEWProgressBar = class(TEWBaseObject, IEWProgressBar)
  private
    FMin: integer;
    FMax: integer;
    FPosition: integer;
    FStriped: Boolean;
    FStyle: TewButtonType;
    FAnimated: Boolean;
    function GetMax: integer;
    function GetMin: integer;
    procedure SetMax(const Value: integer);
    procedure SetMin(const Value: integer);
    function GetPosition: integer;
    procedure SetPosition(const Value: integer);
    procedure SetStriped(const Value: Boolean);
    procedure SetStyle(const Value: TewButtonType);
    function GetStyleString: string;
    procedure SetAnimated(const Value: Boolean);
  protected
    function GenerateHtml: string; override;
    procedure Paint; override;
    function DesignTimeCaption: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Animated: Boolean read FAnimated write SetAnimated default False;
    property Max: integer read GetMax write SetMax;
    property Min: integer read GetMin write SetMin;
    property Position: integer read GetPosition write SetPosition;
    property Striped: Boolean read FStriped write SetStriped;
    property Style: TewButtonType read FStyle write SetStyle;
  end;

implementation

uses SysUtils, Types, VCL.Graphics;

{ TEWProgressBar }

constructor TEWProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  Width := 200;
  Height := 20;
  FMin := 0;
  FMax := 100;
  FPosition := 50;
  FStriped := False;
end;

function TEWProgressBar.DesignTimeCaption: string;
begin
  Result := FPosition.ToString+'%';
end;

function TEWProgressBar.GenerateHtml: string;
var
  AStriped: string;
begin
  inherited;
  AStriped := '';
  if FStriped then AStriped := ' progress-bar-striped ';
  if FAnimated then AStriped := AStriped + ' progress-bar-animated';

  Result := '<div id="'+Name+'" '+GetCss+' class="progress">'+
  '<div class="progress-bar '+GetStyleString+' '+AStriped+'" role="progressbar" style="width: '+fPosition.ToString+'%" aria-valuenow="'+FPosition.ToString+
    '" aria-valuemin="'+FMin.ToString+'" aria-valuemax="'+FMax.ToString+'"></div>'+
  '</div>';
end;

function TEWProgressBar.GetMax: integer;
begin
  Result := FMax;
end;

function TEWProgressBar.GetMin: integer;
begin
  Result := FMin;
end;

function TEWProgressBar.GetPosition: integer;
begin
  Result := FPosition;
end;

function TEWProgressBar.GetStyleString: string;
begin
  case FStyle of
    btPrimary: Result := 'bg-primary';
    btSecondary: Result := 'bg-secondary';
    btSuccess: Result := 'bg-success';
    btInfo: Result := 'bg-info';
    btWarning: Result := 'bg-warning';
    btDanger: Result := 'bg-danger';
  end;
end;

procedure TEWProgressBar.Paint;
var
  ARect: TRect;
  AText: string;
begin
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := clSilver;
  Canvas.Brush.Color := clWhite;
  Canvas.Brush.Style := bsSolid;
  Canvas.RoundRect(ClientRect, 8, 8);
  Canvas.Brush.Color := clWebDodgerBlue;
  Canvas.RoundRect(0, 0, Round((ClientWidth/100) * FPosition), Height, 8, 8);
  Canvas.Font.Size := 10;
  ARect := ClientRect;
  InflateRect(ARect, -0, -0);
  Canvas.Brush.Style := bsClear;
  AText := DesignTimeCaption;
  Canvas.TextRect(ARect, AText, [tfVerticalCenter, tfCenter, tfSingleLine]);
end;

procedure TEWProgressBar.SetAnimated(const Value: Boolean);
begin
  if FAnimated <> Value then
  begin
    FAnimated := Value;
    FStriped := True;
    Changed;
  end;
end;

procedure TEWProgressBar.SetMax(const Value: integer);
begin
  if FMax <> Value then
  begin
    FMax := Value;
    Changed;
  end;
end;

procedure TEWProgressBar.SetMin(const Value: integer);
begin
  if FMin <> Value then
  begin
    FMin := Value;
    Changed;
  end;
end;

procedure TEWProgressBar.SetPosition(const Value: integer);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    Changed;
  end;
end;

procedure TEWProgressBar.SetStriped(const Value: Boolean);
begin
  if FStriped <> Value then
  begin
    FStriped := Value;
    Changed;
  end;
end;

procedure TEWProgressBar.SetStyle(const Value: TewButtonType);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed;
  end;
end;

end.

