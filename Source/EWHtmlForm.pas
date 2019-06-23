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

unit EWHtmlForm;

interface

uses Classes, EWIntf, EWBase, VCL.Graphics, EWTypes;

type
  TEWHTMLForm = class(TEWBaseObject, IEWHTMLForm)
  private
    FFormAction: string;
    FPendingScript: string;
    procedure SetFormAction(const Value: string);
  protected
    function GetScript: string; override;
    function GenerateHtml: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Submit;
  published
    property FormAction: string read FFormAction write SetFormAction;
  end;

implementation

uses Types, SysUtils, Vcl.Controls;

{ TEWHTMLForm }

constructor TEWHTMLForm.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

function TEWHTMLForm.GenerateHtml: string;
var
  ICount: integer;
  AAction: string;
begin
  inherited;
  AAction := '';
  if FFormAction <> '' then
    AAction := 'action="'+FFormAction+'"';
  Result := '<form '+AAction+' method="POST" id="'+Name+'" name="'+Name+'" style="position:relative; height:'+Height.ToString+'px;" id="'+Name+'">';
  for ICount := 0 to ControlCount-1 do
    Result := Result + TewBaseObject(Controls[ICount]).Html;
  Result := Result + '</form>';
end;


function TEWHTMLForm.GetScript: string;
begin
  Result := FPendingScript;
end;

procedure TEWHTMLForm.SetFormAction(const Value: string);
begin
  if FFormAction <> Value then
  begin
    FFormAction := Value;
    Changed;
  end;
end;

procedure TEWHTMLForm.Submit;
begin
  FPendingScript := 'document.getElementById("'+Name+'").submit();';
  Changed;
end;

end.
