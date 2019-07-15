
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

unit EWCustomHTML;

interface

uses Classes, EWBase, VCL.Graphics, EWTypes, EWIntf;

type
  TEWCustomHTML = class(TEWBaseObject, IEWCustomHTML)
  private
    FHTML: TStrings;
    procedure SetHTML(const Value: TStrings);
  protected
    function GenerateHtml: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property HTML: TStrings read FHTML write SetHTML;
  end;

implementation

uses Types, SysUtils, EWConst;

{ TEWCustomHTML }

constructor TEWCustomHTML.Create(AOwner: TComponent);
begin
  inherited;
  FHTML := TStringList.Create;
end;


destructor TEWCustomHTML.Destroy;
begin
  FHTML.Free;
  inherited;
end;


function TEWCustomHTML.GenerateHtml: string;
begin
  inherited;
  Result := ReplaceTokens(C_HTML_CUSTOM_HTML);
  Result := StringReplace(Result, '%html%', FHTML.Text, []);
end;

procedure TEWCustomHTML.SetHTML(const Value: TStrings);
begin
  if Trim(FHTML.Text) <> Trim(Value.Text) then
  begin
    FHTML.Assign(Value);
    Changed;
  end;
end;

end.
