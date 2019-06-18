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

unit EWCheckbox;

interface

uses Classes, EWIntf, EWBase, EWTypes;

type
  TEWCheckBox = class(TEWBaseObject, IEWCheckBox)
  private
    FText: string;
    FChecked: Boolean;
    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
    function GetText: string;
    procedure SetText(const Value: string);
  protected
    function ReplaceTokens(AHtml: string): string; override;
    function DesignTimeCaption: string; override;
    function GenerateHtml: string; override;
    procedure DoClick(AParams: TStrings); override;
    procedure AddClickEvent(AEvents: TStrings); override;
  published
    property Align;
    property Checked: Boolean read GetChecked write SetChecked;
    property Text: string read GetText write SetText;
  end;

implementation

uses EWConst, SysUtils;

{ TEWCheckBox }

procedure TEWCheckBox.AddClickEvent(AEvents: TStrings);
begin
  AddObjectEvent(Name, C_CLICK, [], AEvents, 'document.getElementById("'+Name+'Cbx").checked');
end;

function TEWCheckBox.DesignTimeCaption: string;
begin
  Result := Text;
  if Result = '' then
    Result := Name;
end;

procedure TEWCheckBox.DoClick(AParams: TStrings);
begin
  Checked := AParams.Values[C_VALUE] = C_TRUE;
  inherited;
end;

function TEWCheckBox.GetChecked: Boolean;
begin
  Result := FChecked;
end;

function TEWCheckBox.GenerateHtml: string;
begin
  Result := ReplaceTokens(C_HTML_CHECKBOX);
end;

function TEWCheckBox.GetText: string;
begin
  Result := FText;
end;

function TEWCheckBox.ReplaceTokens(AHtml: string): string;
var
  AChecked: string;
begin
  Result := inherited ReplaceTokens(AHtml);
  AChecked := '';
  if FChecked then AChecked := C_CHECKED;
  Result := StringReplace(Result, '%checked%', AChecked, []);
  Result := StringReplace(Result, '%text%', FText, []);
end;

procedure TEWCheckBox.SetChecked(Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    Changed;
  end;
end;

procedure TEWCheckBox.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;


end.
