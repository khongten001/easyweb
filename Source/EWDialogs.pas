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

unit EWDialogs;

interface

uses Classes, EWBase, EWTypes, EWIntf;

type
  TEWConfirmResultEvent = procedure(Sender: TObject; AConfirmed: Boolean) of object;
  TEWPromptResultEvent = procedure(Sender: TObject; AValue: string) of object;

  TEWDialog = class(TEWBaseComponent)
  private
    FPending: string;
    FOnConfirm: TEWConfirmResultEvent;
    FOnPrompt: TEWPromptResultEvent;
    procedure DoConfirm(AResult: Boolean);
    procedure DoPrompt(AValue: string);
  protected
    function GenerateHtml: string; override;
    function GetScript: string; override;
    procedure DoEvent(AParams: TStrings); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowMessage(AText: string);
    procedure ShowConfirmation(AText: string);
    procedure ShowPrompt(APrompt, ADefaultText: string);
  published
    property OnConfirmResult: TEWConfirmResultEvent read FOnConfirm write FOnConfirm;
    property OnPrompt: TEWPromptResultEvent read FOnPrompt write FOnPrompt;
  end;

implementation

uses SysUtils, Json, EWConst;

{ TEWDialog }

constructor TEWDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TEWDialog.Destroy;
begin
  inherited;
end;

procedure TEWDialog.DoConfirm(AResult: Boolean);
begin
  if Assigned(FOnConfirm) then
    FOnConfirm(Self, AResult);
end;

procedure TEWDialog.DoPrompt(AValue: string);
begin
  if Assigned(FOnPrompt) then
    FOnPrompt(Self, AValue);
end;

procedure TEWDialog.DoEvent(AParams: TStrings);
var
  AType: string;
begin
  inherited;
  AType := AParams.Values[C_TYPE];
  if AType = C_CONFIRM then DoConfirm(AParams.Values[C_VALUE] = C_TRUE);
  if AType = C_PROMPT then DoPrompt(AParams.Values[C_VALUE]);
end;

function TEWDialog.GenerateHtml: string;
begin
  Result := C_HTML_DIALOG;
  Result := StringReplace(Result, '%id%', Name, []);
end;

function TEWDialog.GetScript: string;
begin
  Result := FPending;
  FPending := '';
end;


procedure TEWDialog.ShowConfirmation(AText: string);
var
  AJson: TJSONObject;
begin
  AJson := TJSONObject.Create;
  try
    AJson.AddPair(C_NAME, Name);
    AJson.AddPair(C_TYPE, C_CONFIRM);

    FPending := 'if (confirm("'+AText+'")) '+
                '{eventCall(''action'', true, '''+AJson.ToJSON+''')} else '+
                '{eventCall(''action'', false, '''+AJson.ToJSON+''')}';
    Changed;
  finally
    AJson.Free;
  end;

end;

procedure TEWDialog.ShowPrompt(APrompt, ADefaultText: string);
var
  AJson: TJSONObject;
begin
  AJson := TJSONObject.Create;
  try
    AJson.AddPair(C_NAME, Name);
    AJson.AddPair(C_TYPE, C_PROMPT);

    FPending := 'var value = prompt('''+APrompt+''', '''+ADefaultText+''');'+
                'if (!(value == null || value == '''')) {'+
                ' eventCall(''action'', value, '''+AJson.ToJSON+''') '+
                '} ';


    Changed;
  finally
    AJson.Free;
  end;

end;

procedure TEWDialog.ShowMessage(AText: string);
begin
  FPending := StringReplace(C_ALERT, '%text%', AText, []);
  FPending := StringReplace(FPending, #10, '\n', [rfReplaceAll]);
  FPending := StringReplace(FPending, #13, '\r', [rfReplaceAll]);
  Changed;
end;

end.
