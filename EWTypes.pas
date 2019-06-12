unit EWTypes;

interface

uses Vcl.Forms, Classes, Vcl.Graphics;

type

  TEWBaseForm = class(TCustomForm);

  TEWClickItemEvent = procedure(Sender: TObject; AItem: string; AIndex: integer) of object;
  TEWNavItemClickEvent = procedure(Sender: TObject; AItem: TCollectionItem; ADropDownIndex: integer) of object;

  TEWKeyEvent = procedure(Sender: TObject; Key: Word) of object;

  TEWButtonType = (btDefault, btBasic, btPrimary, btSecondary, btSuccess,
                   btDanger, btWarning, btInfo, btLight, btDark, btLink);

  TEWImageShape = (isDefault, isRoundedCorners, isCircle, isThumbnail);

  TEWFontVariant = (fvSmallCaps);

  TEWInputType = (itDate, itDateTime, itEmail, itHidden, itMonthYear, itNumber, itPassword, itText, itTime, itUrl);

  //TEWFontFamily = type string;

  TEWFont = class(TPersistent)
  private
    FFamily: string;
    FVariant: string;
    FSize: integer;
    FStyle: TFontStyles;
    FColor: TColor;
    FOnChange: TNotifyEvent;
    procedure SetFamily(const Value: string);
    procedure SetColor(const Value: TColor);
    procedure SetSize(const Value: integer);
    procedure SetStyle(const Value: TFontStyles);
    procedure Changed;
    procedure SetVariant(const Value: string);
    function GetAsCssProperty: string;
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Family: string read FFamily write SetFamily;
    property Variant: string read FVariant write SetVariant;
    property Size: integer read FSize write SetSize default 12;
    property Style: TFontStyles read FStyle write SetStyle;
    property Color: TColor read FColor write SetColor;
    property AsCssProperty: string read GetAsCssProperty;
  end;


  function ColorToHex( Color : TColor ): string;

implementation

uses Windows, SysUtils;

function ColorToHex( Color : TColor ): string;
begin
  Result := '#'+
            IntToHex( GetRValue( Color ), 2 ) +
            IntToHex( GetGValue( Color ), 2 ) +
            IntToHex( GetBValue( Color ), 2 );
end;

{ TEWFont }

procedure TEWFont.Assign(Source: TPersistent);
begin
  FFamily := (Source as TEWFont).Family;
  FVariant := (Source as TEWFont).Variant;
  FSize := (Source as TEWFont).Size;
  FStyle := (Source as TEWFont).Style;
  FColor := (Source as TEWFont).Color;
end;

procedure TEWFont.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TEWFont.Create;
begin
  inherited;
  FSize := 12;
end;

function TEWFont.GetAsCssProperty: string;

begin
  Result := '';
  if (fsBold in FStyle) then Result := Result + 'bold ';
  if (fsItalic in FStyle) then Result := Result + 'italic ';
  if (fsUnderline in FStyle) then Result := Result + 'underline ';
  Result := Result + FVariant+' ';
  Result := Result + FSize.ToString+'px ';
  Result := Result + FFamily+' ';
  Result := Trim(StringReplace(Result, '  ', ' ', [rfReplaceAll]));
end;

procedure TEWFont.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TEWFont.SetFamily(const Value: string);
begin
  if FFamily <> Value then
  begin
    FFamily := Value;
    Changed;
  end;
end;

procedure TEWFont.SetSize(const Value: integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    Changed;
  end;
end;

procedure TEWFont.SetStyle(const Value: TFontStyles);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed;
  end;
end;


procedure TEWFont.SetVariant(const Value: string);
begin
  if FVariant <> Value then
  begin
    FVariant := Value;
    Changed;
  end;
end;

end.
