unit EWLabel;

interface

uses Classes, EWIntf, EWBase, VCL.Graphics, EWTypes;

type
  TEWLabel = class(TEWBaseObject, IEWLabel)
  private
    FBackgroundColor: TColor;
    FAlignment: TAlignment;
    FText: string;
    FVertAlignment: TVerticalAlignment;
    FFont: TEWFont;
    function GetText: string;
    procedure SetText(const Value: string);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetVertAlignment(const Value: TVerticalAlignment);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetFont(const Value: TEWFont);
  protected
    procedure BuildCss(AProperties: TStrings); override;
    function DesignTimeCaption: string; override;
    function GetHtml: string; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clNone;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Font: TEWFont read FFont write SetFont;
    property Text: string read GetText write SetText;
    property VertAlignment: TVerticalAlignment read FVertAlignment write SetVertAlignment default taVerticalCenter;
  end;

implementation

uses Types, SysUtils;

{ TEWLabel }

procedure TEWLabel.BuildCss(AProperties: TStrings);
begin
 inherited;
 case FAlignment of
    taLeftJustify: AProperties.Values['text-align'] := 'left';
    taCenter: AProperties.Values['text-align'] := 'center';
    taRightJustify: AProperties.Values['text-align'] := 'right';
  end;
  if FBackgroundColor <> clNone then
    AProperties.Values['background-color'] := ColorToHex(FBackgroundColor);
  AProperties.Values['font'] := StringReplace(FFont.AsCssProperty, '"', '''', [rfReplaceAll]);
  AProperties.Values['color'] := ColorToHex(FFont.Color);
end;

constructor TEWLabel.Create(AOwner: TComponent);
begin
  inherited;
  FAlignment := taLeftJustify;
  FVertAlignment := taVerticalCenter;
  FBackgroundColor := clNone;
  FFont := TEWFont.Create;
end;

function TEWLabel.DesignTimeCaption: string;
begin
  inherited;
  Result := FText;
  if FText = '' then
    Result := Name;
end;

destructor TEWLabel.Destroy;
begin
  FFont.Free;
  inherited;
end;

function TEWLabel.GetHtml: string;
var
  ATranslate: string;
begin
  inherited;
  Result := '';
  case FVertAlignment of
    taAlignTop: ATranslate := '0%';
    taVerticalCenter: ATranslate := '50%';
    taAlignBottom: ATranslate := '100%';
  end;
  Result := '<div id="'+Name+'" '+GetCss+{' style="position: relative; top: '+IntToStr(Height div 2)+'px; transform: translateY(-'+ATranslate+')"}'>'+FText+'</div>';
end;

function TEWLabel.GetText: string;
begin
  Result := FText;
end;

procedure TEWLabel.Paint;
var
  r: TRect;
  AStr: string;
  AFormat: TTextFormat;
begin
  inherited;
  r := ClientRect;
  AFormat := [tfSingleLine];
  case FAlignment of
    taLeftJustify: AFormat := AFormat + [tfLeft];
    taCenter: AFormat := AFormat + [tfCenter];
    taRightJustify: AFormat := AFormat + [tfRight];
  end;
  case FVertAlignment of
    taAlignTop: AFormat := AFormat + [tfTop];
    taVerticalCenter: AFormat := AFormat + [tfVerticalCenter];
    taAlignBottom: AFormat := AFormat + [tfBottom];
  end;
  AStr := DesignTimeCaption;
  Canvas.TextRect(r, AStr, AFormat);
end;

procedure TEWLabel.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed;
  end;
end;

procedure TEWLabel.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    Changed;
  end;
end;

procedure TEWLabel.SetFont(const Value: TEWFont);
begin
  FFont.Assign(Value);
  Changed;
end;

procedure TEWLabel.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TEWLabel.SetVertAlignment(const Value: TVerticalAlignment);
begin
  if FVertAlignment <> Value then
  begin
    FVertAlignment := Value;
    Changed;
  end;
end;

end.
