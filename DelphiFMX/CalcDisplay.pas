unit CalcDisplay;

{$ifdef fpc}{$mode delphi}{$endif}

interface

uses
  Classes, SysUtils;

type

  { TCalcDisplay }

  TCalcDisplay = class
  private
    Digits: array of Integer;
    Index: Integer;
    PointIndex: Integer;
    HasDigits: Boolean;
    HasPoint: Boolean;
    IsNegative: Boolean;
    procedure FixupSign;
    procedure Normalize;
  private
    FMaxCount: Integer;
    function GetValue: Double;
    procedure SetValue(const AValue: Double);
  public
    constructor Create(const AMaxCount: Integer);
    procedure Clear;
    procedure TryAddDigit(const N: Integer);
    procedure TryAddPoint;
    procedure TryDeleteSymbol;
    procedure TryToggleSign;
    function ToString: string; override;
    property Value: Double read GetValue write SetValue;
    property MaxCount: Integer read FMaxCount;
  end;

implementation

{ TCalcDisplay }

function TCalcDisplay.GetValue: Double;
var
  F: TFormatSettings;
begin
  F := FormatSettings;
  F.DecimalSeparator := '.';
  Result := StrToFloat(ToString, F);
end;

procedure TCalcDisplay.SetValue(const AValue: Double);
var
  S: string;
  C: Char;
  F: TFormatSettings;
begin
  F := FormatSettings;
  F.DecimalSeparator := '.';
  S := FloatToStrF(AValue, ffFixed, MaxCount, MaxCount - 1, F);

  Clear;
  for C in S do
    case C of
      '0'..'9': TryAddDigit(Ord(C) - Ord('0'));
      '.': TryAddPoint;
    end;

  if Pos('-', S) <> 0 then
    TryToggleSign;

  Normalize;
end;

procedure TCalcDisplay.FixupSign;
begin
  if not HasDigits then
    IsNegative := False;
end;

procedure TCalcDisplay.Normalize;
begin
  if HasPoint then
  begin
    while (Digits[Index] = 0) and HasPoint do
    begin
      TryDeleteSymbol;
    end;
    if HasPoint and (Index = PointIndex) then
      TryDeleteSymbol;
  end;
  FixupSign;
end;

constructor TCalcDisplay.Create(const AMaxCount: Integer);
begin
  FMaxCount := AMaxCount;
  SetLength(Digits, AMaxCount);
  Clear;
end;

procedure TCalcDisplay.Clear;
begin
  HasDigits := False;
  HasPoint := False;
  IsNegative := False;
end;

procedure TCalcDisplay.TryAddDigit(const N: Integer);
begin
  if (N = 0) and (not HasDigits) then
    Exit;

  if not HasDigits then
  begin
    HasDigits := True;
    Index := -1;
  end;

  if Index + 1 < MaxCount then
  begin
    Inc(Index);
    Digits[Index] := N;
  end;
end;

procedure TCalcDisplay.TryAddPoint;
begin
  if not HasDigits then
  begin
    HasDigits := True;
    Index := 0;
    Digits[Index] := 0;
  end;

  if (not HasPoint) and (Index + 1 < MaxCount) then
  begin
    HasPoint := True;
    PointIndex := Index;
  end;
end;

procedure TCalcDisplay.TryDeleteSymbol;
begin
  if HasPoint and (Index = PointIndex) then
  begin
    HasPoint := False;
    if (Index = 0) and (Digits[0] = 0) then
    begin
      HasDigits := False;
      IsNegative := False;
    end;
    Exit;
  end;

  if Index > 0 then
    Dec(Index)
  else
    HasDigits := False;

  FixupSign;
end;

procedure TCalcDisplay.TryToggleSign;
begin
  IsNegative := not IsNegative;
  FixupSign;
end;

function TCalcDisplay.ToString: string;
var
  I: Integer;
begin
  if not HasDigits then
    Exit('0')
  else
  begin
    if IsNegative then
      Result := '-'
    else
      Result := '';
    for I := 0 to Index do
    begin
      Result := Result + IntToStr(Digits[I]);
      if HasPoint and (PointIndex = I) then
        Result := Result + '.';
    end;

  end;
end;

end.

