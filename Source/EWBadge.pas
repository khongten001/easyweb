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

unit EWBadge;

interface

uses Classes, EWBase, EWTypes, EWIntf;

type
  TEWBadge = class(TPersistent)
  private
    FOwner: TComponent;
    FText: string;
    FStyle: TEWButtonType;
    FVisible: Boolean;
    procedure SetText(const Value: string);
    procedure SetStyle(const Value: TEWButtonType);
    procedure SetVisible(const Value: Boolean);
    function GetBadgeTypeStr: string;
  protected
    function ReplaceTokens(AHtml: string): string;
    function GetHtml: string;
    procedure Changed;
  public
    constructor Create(AOwner: TComponent); virtual;
    property Html: string read GetHtml;
  published
    property Style: TEWButtonType read FStyle write SetStyle default btLight;
    property Text: string read FText write SetText;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;

implementation

uses SysUtils, EWConst;

{ TEWBadge }

procedure TEWBadge.Changed;
var
  I: IEWBaseComponent;
begin
  inherited;
  if Supports(FOwner, IEWBaseComponent, I) then
    I.Changed;
end;

constructor TEWBadge.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FStyle := btLight;
  FVisible := False;
  FText := '';
end;

function TEWBadge.GetBadgeTypeStr: string;
begin
  case FStyle of
    btBasic:      Result := '';
    btDefault:    Result := 'default';
    btPrimary:    Result := 'primary';
    btSecondary:  Result := 'secondary';
    btSuccess:    Result := 'success';
    btDanger:     Result := 'danger';
    btWarning:    Result := 'warning';
    btInfo:       Result := 'info';
    btLight:      Result := 'light';
    btDark:       Result := 'dark';
    btLink:       Result := 'link';
  end;
end;

function TEWBadge.GetHtml: string;
begin
  if FVisible = False then
    Result := ''
  else
    Result := ReplaceTokens(C_HTML_BADGE);
  //  Result := '<span class="badge badge-'+GetBadgeTypeStr+'">'+FText+'</span>';
end;

function TEWBadge.ReplaceTokens(AHtml: string): string;
begin
  Result := AHtml;
  Result := StringReplace(Result, '%badgetype%', GetBadgeTypeStr, []);
  Result := StringReplace(Result, '%text%', FText, []);
end;

procedure TEWBadge.SetStyle(const Value: TEWButtonType);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed;
  end;
end;

procedure TEWBadge.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TEWBadge.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

end.
