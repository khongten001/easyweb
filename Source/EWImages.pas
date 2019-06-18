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

unit EWImages;

interface

uses Windows, Classes, EWIntf, EWBase, EWTypes, VCL.Graphics;

type
  TEWImage = class(TEWBaseObject, IEWImage)
  private
    FUrl: string;
    FImageShape: TEWImageShape;
    FPicture: TPicture;
    FTimeStamp: Cardinal;
    procedure SetUrl(const Value: string);
    procedure SetImageShape(const Value: TEWImageShape);
    procedure SetPicture(const Value: TPicture);
  protected
    procedure Changed; override;
    function GetClass: string;
    function ReplaceTokens(AHtml: string): string; override;
    function GenerateHtml: string; override;
    procedure Paint; override;
    procedure BuildCss(AProperties: TStrings); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property ImageShape: TEWImageShape read FImageShape write SetImageShape default isDefault;
    property Picture: TPicture read FPicture write SetPicture;
    property URL: string read FUrl write SetUrl;
  end;

implementation

uses Types, SysUtils, EWConst;

{ TEWImage }


procedure TEWImage.BuildCss(AProperties: TStrings);
begin
  inherited;
end;

procedure TEWImage.Changed;
begin
  inherited;
  FTimeStamp := GetTickCount;
end;

constructor TEWImage.Create(AOwner: TComponent);
begin
  inherited;
  FPicture := TPicture.Create;
  FImageShape := isDefault;
end;

destructor TEWImage.Destroy;
begin
  FPicture.Free;
  inherited;
end;

function TEWImage.GetClass: string;
begin
  case FImageShape of
    isDefault: Result := '';
    isRoundedCorners: Result := C_ROUNDED;
    isCircle: Result := C_ROUNDED_CIRCLE;
    isThumbnail: Result := C_IMG_THUMBNAIL;
  end;
end;

function TEWImage.GenerateHtml: string;
begin
  inherited;
  Result := ReplaceTokens(C_HTML_IMAGE);
end;

procedure TEWImage.Paint;
var
  r: TRect;
  AStr: string;
begin
  inherited;
  r := ClientRect;
  AStr := DesignTimeCaption;
  Canvas.TextRect(r, AStr, [tfCenter, tfVerticalCenter, tfSingleLine]);
end;


function TEWImage.ReplaceTokens(AHtml: string): string;
var
  ASrc: string;
begin
  Result := inherited ReplaceTokens(AHtml);

  ASrc := FUrl;
  if Assigned(FPicture.Graphic) then
  begin
    if FPicture.Graphic.Empty = False then
      ASrc := '/images?s='+SessionID+'&img='+Name+'&ts='+FTimeStamp.ToString;
  end;
  Result := StringReplace(Result, '%src%', ASrc, []);
  Result := StringReplace(Result, '%class%', GetClass, []);
end;

procedure TEWImage.SetImageShape(const Value: TEWImageShape);
begin
  if FImageShape <> Value then
  begin
    FImageShape := Value;
    Changed;
  end;
end;

procedure TEWImage.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
  if Assigned(FPicture.Graphic) then
    if FPicture.Graphic.Empty = False then
      FUrl := '';
  Changed;
end;

procedure TEWImage.SetUrl(const Value: string);
begin
  if FUrl <> Value then
  begin
    FUrl := Value;
    if FUrl <> '' then
      FPicture.Assign(nil);
    Changed;
  end;
end;

end.
