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

unit EWTimer;

interface

uses Classes, EWBase, EWTypes, EWIntf;

type
  TEWTimer = class(TEWBaseComponent, IEWTimer)
  private
    FInterval: integer;
    FActive: Boolean;
    FOnTimer: TNotifyEvent;
    procedure SetActive(const Value: Boolean);
    procedure SetInterval(const Value: integer);
  protected
    function GenerateHtml: string; override;
    function GetScript: string; override;
    procedure GetGlobalVars(AStrings: TStrings); override;
    procedure DoEvent(APArams: TStrings); override;
    procedure DoTimer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Active: Boolean read FActive write SetActive default False;
    property Interval: integer read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

implementation

uses SysUtils, Json;

{ TEWTimer }

constructor TEWTimer.Create(AOwner: TComponent);
begin
  inherited;
  FInterval := 1000;
end;

procedure TEWTimer.DoEvent(AParams: TStrings);
begin
  inherited;
  DoTimer;
end;

procedure TEWTimer.DoTimer;
begin
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

procedure TEWTimer.GetGlobalVars(AStrings: TStrings);
begin
  inherited;
  AStrings.Add('var timer'+Name+';');
end;

function TEWTimer.GenerateHtml: string;
var
  AScript: string;
begin
  inherited;
  AScript := GetScript;
  Result := '<script id="'+Name+'">'+AScript+'</script>';
end;

function TEWTimer.GetScript: string;
var
  AJson: TJsonObject;
begin
  AJson := TJSONObject.Create;
  try
    AJson.AddPair('name', Name);
    if (FActive) and (FInterval > 0) then
      Result := 'timer'+Name+' = setInterval(function(){ eventCall(''timer'', '''', '''+AJson.ToString+'''); }, '+FInterval.ToString+');'
    else
      Result := 'clearTimeout(timer'+Name+');'
  finally
    AJson.Free;
  end;
end;

procedure TEWTimer.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    Changed;
  end;
end;

procedure TEWTimer.SetInterval(const Value: integer);
begin
  if FInterval <> Value then
  begin
    FInterval := Value;
    Changed;
  end;
end;

end.
