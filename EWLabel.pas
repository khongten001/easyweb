unit EWLabel;

interface

uses Classes, EWIntf, EWBase;

type
  TEWLabel = class(TEWBaseObject, IEWLabel)
  private
    FText: string;
    function GetText: string;
    procedure SetText(const Value: string);
  protected
    function DesignTimeCaption: string; override;
    function GetHtml: string; override;
    procedure Paint; override;
  published
    property Text: string read GetText write SetText;
  end;

implementation

uses Types, Graphics;

{ TBsLabel }

function TEWLabel.DesignTimeCaption: string;
begin
  inherited;
  Result := FText;
  if FText = '' then
    Result := Name;
end;

function TEWLabel.GetHtml: string;
begin
  inherited;
  Result := '';
  Result := '<div id="'+Name+'" name="'+Name+'" '+GetCss+'>'+FText+'</div>';
end;

function TEWLabel.GetText: string;
begin
  Result := FText;
end;

procedure TEWLabel.Paint;
var
  r: TRect;
  AStr: string;
begin
  inherited;
  r := ClientRect;
  AStr := DesignTimeCaption;
  Canvas.TextRect(r, AStr, [tfCenter, tfVerticalCenter, tfSingleLine]);
end;

procedure TEWLabel.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

end.
