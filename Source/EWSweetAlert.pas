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

unit EWSweetAlert;

interface

uses Classes, EWBase, EWTypes, EWIntf;

type
  TEWSweetAlertIcon = (saNone, saWarning, saError, saSuccess, saInfo);
  TEWSweetAlertButton = (sabOk, sabYes, sabNo, sabCancel);
  TEWSweetAlertButtons = set of TEWSweetAlertButton;

  TEWSweetAlertButtonClickEvent = procedure(Sender: TObject; AButton: TEWSweetAlertButton);
  TEWSweetAlertConfirmResultEvent = procedure(Sender: TObject; AButton: TEWSweetAlertButton) of object;

  TEWSweetAlert = class(TEWBaseComponent)
  private
    FPending: string;
    FOnConfirm: TEWSweetAlertConfirmResultEvent;
    FOnButtonClicked: Boolean;
    procedure DoConfirm(AButton: TEWSweetAlertButton);
  protected
    procedure GetRequiredThirdPartySrc(AStrings: TStrings); override;
    function GenerateHtml: string; override;
    function GetScript: string; override;
    procedure DoEvent(AParams: TStrings); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowMessage(ATitle, AText: string;
                          const AIcon: TEWSweetAlertIcon = saNone;
                          const AButtonText: string = 'OK');
    procedure ShowConfirmation(ATitle, AText: string;
                               AButtons: TEWSweetAlertButtons;
                               const AIcon: TEWSweetAlertIcon = saNone);

  published
    property OnConfirmResult: TEWSweetAlertConfirmResultEvent read FOnConfirm write FOnConfirm;
    property OnButtonClicked: Boolean read FOnButtonClicked write FOnButtonClicked;
  end;

implementation

uses SysUtils, Json, EWConst;

{ TEWSweetAlert }

constructor TEWSweetAlert.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TEWSweetAlert.Destroy;
begin
  inherited;
end;

procedure TEWSweetAlert.DoConfirm(AButton: TEWSweetAlertButton);
begin
  if Assigned(FOnConfirm) then
    FOnConfirm(Self, AButton);
end;

procedure TEWSweetAlert.DoEvent(AParams: TStrings);
var
  AButton: string;
begin
  inherited;
  AButton := AParams.Values['button'].ToLower;
  if AButton = 'ok' then DoConfirm(sabOk);
  if AButton = 'yes' then DoConfirm(sabYes);
  if AButton = 'no' then DoConfirm(sabNo);
  if AButton = 'cancel' then DoConfirm(sabCancel);
end;

function TEWSweetAlert.GenerateHtml: string;
begin
  Result := C_HTML_DIALOG;
  Result := StringReplace(Result, '%id%', Name, []);
end;

procedure TEWSweetAlert.GetRequiredThirdPartySrc(AStrings: TStrings);
begin
  inherited;
  AStrings.Add(C_SWEET_ALERT_URL);
end;

function TEWSweetAlert.GetScript: string;
begin
  Result := FPending;
  FPending := '';
end;


procedure TEWSweetAlert.ShowConfirmation(ATitle, AText: string;
                                         AButtons: TEWSweetAlertButtons;
                                         const AIcon: TEWSweetAlertIcon = saNone);
var
  AIconStr: string;
  AButtonJson: TJSONObject;
  ABtn: TEWSweetAlertButton;
begin
  case AIcon of
    saNone: AIconStr := '';
    saWarning: AIconStr := 'warning';
    saError: AIconStr := 'error';
    saSuccess: AIconStr := 'success';
    saInfo: AIconStr := 'info';
  end;

  AButtonJson := TJSONObject.Create;
  try
    for ABtn in AButtons do
    begin
      if ABtn = sabOk then AButtonJson.AddPair('Ok', 'Ok');
      if ABtn = sabYes then AButtonJson.AddPair('Yes', 'Yes');
      if ABtn = sabNo then AButtonJson.AddPair('No', 'No');
      if ABtn = sabCancel then AButtonJson.AddPair('Cancel', 'Cancel');
    end;

    FPending :='swal("%title%", "%text%", "%icon%", {'+
    '  buttons: '+AButtonJson.ToString+','+
    '})'+
    '.then((value) => {'+
    //'  switch (value) {'+
    '              '+
    //'    case "Yes":'+
    '      eventCall(''action'', false, ''{"name": "'+Name+'","button":"''+value+''"}'');'+
   (* '      break;'+
    '             '+
    '    case "No":'+
    '      eventCall(''action'', true, ''{"name": "'+Name+'","button":"yes"}'');'+
    '      break;'+
    '        '+
    '    default:'+
    '      eventCall(''action'', true, ''{"name": "'+Name+'","button":"yes"}'');'+  *)
    //'  }'+
    '});';
  finally
    AButtonJson.Free;
  end;


  FPending := StringReplace(FPending, '%text%', AText, []);
  FPending := StringReplace(FPending, '%title%', ATitle, []);
  FPending := StringReplace(FPending, '%icon%', AIconStr, []);
  FPending := StringReplace(FPending, #10, '\n', [rfReplaceAll]);
  FPending := StringReplace(FPending, #13, '\r', [rfReplaceAll]);
  Changed;

  (*AJson := TJSONObject.Create;
  try
    AJson.AddPair(C_NAME, Name);
    AJson.AddPair(C_TYPE, C_CONFIRM);

    FPending := 'if (confirm("'+AText+'")) '+
                '{eventCall(''action'', true, '''+AJson.ToJSON+''')} else '+
                '{eventCall(''action'', false, '''+AJson.ToJSON+''')}';
    Changed;
  finally
    AJson.Free;
  end;    *)

end;


procedure TEWSweetAlert.ShowMessage(ATitle, AText: string;
                                    const AIcon: TEWSweetAlertIcon = saNone;
                                    const AButtonText: string = 'OK');
var
  AIconStr: string;
begin
  case AIcon of
    saNone: AIconStr := '';
    saWarning: AIconStr := 'warning';
    saError: AIconStr := 'error';
    saSuccess: AIconStr := 'success';
    saInfo: AIconStr := 'info';
  end;
  FPending := C_ALERT_SWEEET_ALERT;
  FPending := StringReplace(FPending, '%text%', AText, []);
  FPending := StringReplace(FPending, '%title%', ATitle, []);
  FPending := StringReplace(FPending, '%icon%', AIconStr, []);
  FPending := StringReplace(FPending, '%button%', AButtonText, []);
  FPending := StringReplace(FPending, #10, '\n', [rfReplaceAll]);
  FPending := StringReplace(FPending, #13, '\r', [rfReplaceAll]);
  Changed;
end;

end.
