{
Copyright (C) 2005  Remco Mulder

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

For source notes please refer to Notes.txt
For license terms please refer to GPL.txt.

These files should be stored in the root of the compression you 
received this source in.
}
// This unit controls all processing and recording of data

unit
  Process;

interface

uses
  Core,
  Observer,
  SysUtils,
  DataBase,
  Classes;

type
  TSectorPosition = (spNormal, spPorts, spPlanets, spShips, spMines, spTraders);
  TDisplay = (dNone, dSector, dDensity, dWarpLane, dCIM, dPortCIM, dPort, dWarpCIM, dVScreen);

  TModExtractor = class(TTWXModule, IModExtractor)
  private
    FCurrentSectorIndex : Integer;
    FSectorPosition     : TSectorPosition;
    FCurrentDisplay     : TDisplay;
    FLastWarp           : Integer;
    FSectorSaved        : Boolean;
    FCurrentTrader      : TTrader;
    FCurrentShip        : TShip;
    FCurrentMessage     : string;
    FTraderList,
    FShipList,
    FPlanetList         : TList;

    FCurrentLine,
    FCurrentANSILine    : string;
    FCurrentSector      : TSector;
    FInAnsi             : Boolean;
    FMenuKey            : Char;

    procedure SectorCompleted;
    procedure ResetSectorLists;
    procedure ProcessPrompt(Line : string);
    procedure AddWarp(SectNum, Warp : Integer);
    procedure ProcessWarpLine(Line : String);
    procedure ProcessCIMLine(Line : String);
    procedure ProcessSectorLine(Line : String);
    procedure ProcessVScreen(Line : String);
    procedure ProcessLine(Line : String);
    procedure ProcessPortLine(Line : String);
    procedure StripANSI(var S : string);

  protected
    function GetMenuKey: Char;
    procedure SetMenuKey(Value: Char);

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Reset;
    procedure ProcessInBound(var InData : string);
    function ProcessOutBound(OutData : string; ClientIndex : Byte) : Boolean;

    property CurrentLine: string read FCurrentLine write FCurrentLine;
    property CurrentANSILine: string read FCurrentANSILine write FCurrentANSILine;

  published
    property MenuKey: Char read GetMenuKey write SetMenuKey;
  end;

implementation

uses
  Global,
  Utility,
  Ansi;


procedure TModExtractor.AfterConstruction;
begin
  inherited;

  // Create lists to store ships, traders and planets
  FShipList := TList.Create;
  FTraderList := TList.Create;
  FPlanetList := TList.Create;

  MenuKey := '$';
end;

procedure TModExtractor.BeforeDestruction;
begin
  ResetSectorLists;

  FShipList.Free;
  FTraderList.Free;
  FPlanetList.Free;

  inherited;
end;

procedure TModExtractor.Reset;
begin
  // Reset state values

  CurrentLine := '';
  CurrentANSILine := '';
  FInAnsi := FALSE;
  ResetSectorLists;
end;

function TModExtractor.GetMenuKey: Char;
begin
  Result := FMenuKey;
end;

procedure TModExtractor.SetMenuKey(Value: Char);
begin
  FMenuKey := Value;
end;


// ********************************************************************
// Process inbound data



procedure TModExtractor.ResetSectorLists;
begin
  // Reset all ship, planet and trader lists

  while (FShipList.Count > 0) do
  begin
    FreeMem(FShipList[0], SizeOf(TShip));
    FShipList.Delete(0);
  end;

  while (FPlanetList.Count > 0) do
  begin
    FreeMem(FPlanetList[0], SizeOf(TPlanet));
    FPlanetList.Delete(0);
  end;

  while (FTraderList.Count > 0) do
  begin
    FreeMem(FTraderList[0], SizeOf(TTrader));
    FTraderList.Delete(0);
  end;
end;

procedure TModExtractor.SectorCompleted;
var
  I,
  WarpIndex : Integer;
begin
  if (FCurrentSectorIndex = 0) then
    Exit;

  FCurrentSector.UpDate := Now;
  FCurrentSector.Explored := etHolo;
  FSectorSaved := TRUE;
  WarpIndex := 0;

  for I := 1 to 6 do
    if (FCurrentSector.Warp[I] = 0) then
    begin
      WarpIndex := I;
      Break;
    end;

  if (WarpIndex = 0) then
    FCurrentSector.Warps := 0
  else if (FCurrentSector.Warp[WarpIndex] = 0) then
    FCurrentSector.Warps := WarpIndex - 1
  else
    FCurrentSector.Warps := 6;

  TWXDatabase.SaveSector(FCurrentSector, FCurrentSectorIndex, FShipList, FTraderList, FPlanetList);
  FCurrentSectorIndex := 0;
  ResetSectorLists;
end;

procedure TModExtractor.ProcessPrompt(Line : string);
begin
  // This procedure checks command prompts.  It is called from both
  // processline and processinbound, as it can come in as part of
  // a large packet or still be waiting for the user.

  if (Copy(Line, 1, 12) = 'Command [TL=') then
  begin
    // Save current sector if not done already
    if not (FSectorSaved) then
      SectorCompleted;

    // No displays anymore, all done
    FCurrentDisplay := dNone;
    FLastWarp := 0;
  end
  else if (Copy(Line, 1, 23) = 'Probe entering sector :') or (Copy(Line, 1, 20) = 'Probe Self Destructs') then
  begin
    // mid probe - save the sector
    if not (FSectorSaved) then
      SectorCompleted;

    // No displays anymore, all done
    FCurrentDisplay := dNone;
  end
  else if (Copy(Line, 1, 21) = 'Computer command [TL=') then
  begin
    // in computer prompt, kill all displays and clear warp data
    FCurrentDisplay := dNone;
    FLastWarp := 0;
  end
  else if (Copy(Line, 1, 25) = 'Citadel treasury contains') then
  begin
    // In Citadel - Save current sector if not done already
    if not (FSectorSaved) then
      SectorCompleted;

    // No displays anymore, all done
    FCurrentDisplay := dNone;
  end
  else if (Copy(Line, 1, 19) = 'Stop in this sector') or (Copy(Line, 1, 21) = 'Engage the Autopilot?') then
  begin
    // Save current sector if not done already
    if not (FSectorSaved) then
      SectorCompleted;

    // No displays anymore, all done
    FCurrentDisplay := dNone;
  end
  else if (Copy(Line, 1, 2) = ': ') then
  begin
    // at the CIM prompt
    if (FCurrentDisplay <> dCIM) then
      FCurrentDisplay := dNone;

    FLastWarp := 0;
  end;

  TWXInterpreter.TextEvent(CurrentLine, FALSE);
end;

procedure TModExtractor.AddWarp(SectNum, Warp : Integer);
var
  S     : TSector;
  I,
  X,
  Pos   : Integer;
begin
  // Used by ProcessWarpLine to add a warp to a sector

  S := TWXDatabase.LoadSector(SectNum);

  // see if the warp is already in there
  for I := 1 to 6 do
    if (S.Warp[I] = Warp) then
      Exit;

  // find where it should fit
  Pos := 7;
  for I := 1 to 6 do
    if (S.Warp[I] > Warp) or (S.Warp[I] = 0) then
    begin
      Pos := I;
      Break;
    end;

  if (Pos = 1) then
    X := 2
  else
    X := Pos;

  // move them all up one
  if (Pos < 6) then
    for I := 6 downto X do
      S.Warp[I] := S.Warp[I - 1];

  if (Pos < 7) then
    S.Warp[Pos] := Warp;

  if (S.Explored = etNo) then
  begin
    S.Constellation := '???' + ANSI_9 + ' (warp calc only)';
    S.Explored := etCalc;
    S.Update := Now;
  end;

  TWXDatabase.SaveSector(S, SectNum, nil, nil, nil);
end;

procedure TModExtractor.ProcessWarpLine(Line : String);
var
  I,
  CurSect,
  LastSect : Integer;
  S        : String;
begin
  // A WarpLine is a line of warps plotted using the ship's computer.  Add new warps to
  // any sectors listed in the warp lane (used extensively for ZTM).
  // e.g:  3 > 300 > 5362 > 13526 > 149 > 434

  LastSect := FLastWarp;
  StripChar(Line, ')');
  StripChar(Line, '(');

  I := 1;
  S := GetParameter(Line, I);

  while (S <> '') do
  begin
    if (S <> '>') then
    begin
      CurSect := StrToIntSafe(S);

      if (CurSect < 1) or (CurSect > TWXDatabase.DBHeader.Sectors) then
        // doesn't look like this line is what we thought it was.
        // Best to leave it alone
        exit;

      if (LastSect > 0) then
        AddWarp(LastSect, CurSect);

      LastSect := CurSect;
      FLastWarp := CurSect;
    end;

    Inc(I);
    S := GetParameter(Line, I);
  end;
end;

procedure TModExtractor.ProcessCIMLine(Line : String);
  function GetCIMValue(M : String; Num : Integer) : Integer;
  var
    S : String;
  begin
    S := GetParameter(M, Num);

    if (S = '') then
      Result := 0
    else
    try
      Result := StrToInt(S);
    except
      Result := -1;
    end;
  end;
var
  Sect   : Integer;
  S      : TSector;
  X,
  I,
  Len,
  Ore,
  Org,
  Equip,
  POre,
  POrg,
  PEquip : Integer;
  M      : String;
begin
  if (FCurrentDisplay = dWarpCIM) then
  begin
    // save warp CIM data
    Sect := GetCIMValue(Line, 1);

    if (Sect <= 0) or (Sect > TWXDatabase.DBHeader.Sectors) then
    begin
      FCurrentDisplay := dNone;
      Exit;
    end;

    S := TWXDatabase.LoadSector(Sect);

    for I := 1 to 6 do
    begin
      X := GetCIMValue(Line, I + 1);

      if (X < 0) or (X > TWXDatabase.DBHeader.Sectors) then
      begin
        FCurrentDisplay := dNone;
        Exit;
      end
      else
        S.Warp[I] := X;
    end;

    if (S.Explored = etNo) then
    begin
      S.Constellation := '???' + ANSI_9 + ' (warp calc only)';
      S.Explored := etCalc;
      S.Update := Now;
    end;

    TWXDatabase.SaveSector(S, Sect, nil, nil, nil);
  end
  else
  begin
    // save port CIM data
    Sect := GetCIMValue(Line, 1);
    Len := Length(IntToStr(TWXDatabase.DBHeader.Sectors));

    if (Sect <= 0) or (Sect > TWXDatabase.DBHeader.Sectors) or (Length(Line) < Len + 36) then
    begin
      FCurrentDisplay := dNone;
      Exit;
    end;

    M := StringReplace(Line, '-', '', [rfReplaceAll]);
    M := StringReplace(M, '%', '', [rfReplaceAll]);
    S := TWXDatabase.LoadSector(Sect);

    Ore := GetCIMValue(M, 2);
    Org := GetCIMValue(M, 4);
    Equip := GetCIMValue(M, 6);
    POre := GetCIMValue(M, 3);
    POrg := GetCIMValue(M, 5);
    PEquip := GetCIMValue(M, 7);

    if (Ore < 0) or (Org < 0) or (Equip < 0)
     or (POre < 0) or (POre > 100)
     or (POrg < 0) or (POrg > 100)
     or (PEquip < 0) or (PEquip > 100) then
    begin
      FCurrentDisplay := dNone;
      Exit;
    end;

    S.SPort.ProductAmount[ptFuelOre] := Ore;
    S.SPort.ProductAmount[ptOrganics] := Org;
    S.SPort.ProductAmount[ptEquipment] := Equip;
    S.SPort.ProductPercent[ptFuelOre] := POre;
    S.SPort.ProductPercent[ptOrganics] := POrg;
    S.SPort.ProductPercent[ptEquipment] := PEquip;
    S.SPort.UpDate := Now;

    if (S.SPort.Name = '') then
    begin
      // port not saved/seen before - get its details

      if (Line[Len + 2] = '-') then
        S.SPort.BuyProduct[ptFuelOre] := TRUE
      else
        S.SPort.BuyProduct[ptFuelOre] := FALSE;

      if (Line[Len + 14] = '-') then
        S.SPort.BuyProduct[ptOrganics] := TRUE
      else
        S.SPort.BuyProduct[ptOrganics] := FALSE;

      if (Line[Len + 26] = '-') then
        S.SPort.BuyProduct[ptEquipment] := TRUE
      else
        S.SPort.BuyProduct[ptEquipment] := FALSE;

      if (S.SPort.BuyProduct[ptFuelOre]) and (S.SPort.BuyProduct[ptOrganics]) and (S.SPort.BuyProduct[ptEquipment]) then
        S.SPort.ClassIndex := 8
      else if (S.SPort.BuyProduct[ptFuelOre]) and (S.SPort.BuyProduct[ptOrganics]) and not (S.SPort.BuyProduct[ptEquipment]) then
        S.SPort.ClassIndex := 1
      else if (S.SPort.BuyProduct[ptFuelOre]) and not (S.SPort.BuyProduct[ptOrganics]) and (S.SPort.BuyProduct[ptEquipment]) then
        S.SPort.ClassIndex := 2
      else if not (S.SPort.BuyProduct[ptFuelOre]) and (S.SPort.BuyProduct[ptOrganics]) and (S.SPort.BuyProduct[ptEquipment]) then
        S.SPort.ClassIndex := 3
      else if not (S.SPort.BuyProduct[ptFuelOre]) and not (S.SPort.BuyProduct[ptOrganics]) and (S.SPort.BuyProduct[ptEquipment]) then
        S.SPort.ClassIndex := 4
      else if not (S.SPort.BuyProduct[ptFuelOre]) and (S.SPort.BuyProduct[ptOrganics]) and not (S.SPort.BuyProduct[ptEquipment]) then
        S.SPort.ClassIndex := 5
      else if (S.SPort.BuyProduct[ptFuelOre]) and not (S.SPort.BuyProduct[ptOrganics]) and not (S.SPort.BuyProduct[ptEquipment]) then
        S.SPort.ClassIndex := 6
      else if not (S.SPort.BuyProduct[ptFuelOre]) and not (S.SPort.BuyProduct[ptOrganics]) and not (S.SPort.BuyProduct[ptEquipment]) then
        S.SPort.ClassIndex := 7;

      S.SPort.Name := '???';
    end;

    if (S.Explored = etNo) then
    begin
      S.Constellation := '???' + ANSI_9 + ' (port data/calc only)';
      S.Explored := etCalc;
      S.Update := Now;
    end;

    TWXDatabase.SaveSector(S, Sect, nil, nil, nil);
  end;
end;

procedure TModExtractor.ProcessSectorLine(Line : String);
var
  S         : String;
  I         : Integer;
  NewPlanet : PPlanet;
  NewShip   : PShip;
  NewTrader : PTrader;
begin
  if (Copy(Line, 1, 10) = 'Beacon  : ') then
  begin
    // Get beacon text
    FCurrentSector.Beacon := Copy(Line, 11, length(Line) - 10);
  end
  else if (Copy(Line, 1, 10) = 'Ports   : ') then
  begin
    // Save port data

    if (Pos('<=-DANGER-=>', Line) > 0) then
      // Port is destroyed
      FCurrentSector.SPort.Dead := TRUE
    else
    begin
      FCurrentSector.SPort.Dead := FALSE;
      FCurrentSector.SPort.BuildTime := 0;
      FCurrentSector.SPort.Name := Copy(Line, 11, Pos(', Class', Line) - 11);
      FCurrentSector.SPort.ClassIndex := StrToIntSafe(Copy(Line, Pos(', Class', Line) + 8, 1));

      if (Line[length(Line) - 3] = 'B') then
        FCurrentSector.SPort.BuyProduct[ptFuelOre] := TRUE
      else
        FCurrentSector.SPort.BuyProduct[ptFuelOre] := FALSE;

      if (Line[length(Line) - 2] = 'B') then
        FCurrentSector.SPort.BuyProduct[ptOrganics] := TRUE
      else
        FCurrentSector.SPort.BuyProduct[ptOrganics] := FALSE;

      if (Line[length(Line) - 1] = 'B') then
        FCurrentSector.SPort.BuyProduct[ptEquipment] := TRUE
      else
        FCurrentSector.SPort.BuyProduct[ptEquipment] := FALSE;
    end;

    FSectorPosition := spPorts;
  end
  else if (Copy(Line, 1, 10) = 'Planets : ') then
  begin
    // Get planet data
    NewPlanet := AllocMem(SizeOf(TPlanet));
    TWXDatabase.NULLPlanet(NewPlanet^);
    NewPlanet^.Name := Copy(Line, 11, length(Line) - 10);
    FPlanetList.Add(NewPlanet);

    FSectorPosition := spPlanets;
  end
  else if (Copy(Line, 1, 10) = 'Traders : ') then
  begin
    // Save traders
    I := Pos(', w/', Line);
    FCurrentTrader.Name := Copy(Line, 11, I - 11);
    S := Copy(Line, I + 5, Pos(' ftrs', Line) - I - 5);
    StripChar(S, ',');
    FCurrentTrader.Figs := StrToIntSafe(S);
    FSectorPosition := spTraders;
  end
  else if (Copy(Line, 1, 10) = 'Ships   : ') then
  begin
    // Save ships
    I := Pos('[Owned by]', Line);
    FCurrentShip.Name := Copy(Line, 11, I - 12);
    FCurrentShip.Owner := Copy(Line, I + 11, Pos(', w/', Line) - I - 11);
    I := Pos(', w/', Line);
    S := Copy(Line, I + 5, Pos(' ftrs,', Line) - I - 5);
    StripChar(S, ',');
    FCurrentShip.Figs := StrToIntSafe(S);
    FSectorPosition := spShips;
  end
  else if (Copy(Line, 1, 10) = 'Fighters: ') then
  begin
    // Get fig details
    S := GetParameter(Line, 2);
    StripChar(S, ',');
    FCurrentSector.Figs.Quantity := StrToIntSafe(S);
    I := GetParameterPos(Line, 3) + 1;
    FCurrentSector.Figs.Owner := Copy(Line, I, Pos(')', Line) - I);

    if (Copy(Line, length(Line) - 5, 6) = '[Toll]') then
      FCurrentSector.Figs.FigType := ftToll
    else if (Copy(Line, length(Line) - 10, 11) = '[Defensive]') then
      FCurrentSector.Figs.FigType := ftDefensive
    else
      FCurrentSector.Figs.FigType := ftOffensive;
  end
  else if (Copy(Line, 1, 10) = 'NavHaz  : ') then
  begin
    S := GetParameter(Line, 3);
    S := Copy(S, 1, length(S) - 1);
    FCurrentSector.NavHaz := StrToIntSafe(S);
  end
  else if (Copy(Line, 1, 10) = 'Mines   : ') then
  begin
    // Save mines
    FSectorPosition := spMines;
    I := GetParameterPos(Line, 7) + 1;
    S := Copy(Line, I, length(Line) - I);

    if (GetParameter(Line, 6) = 'Armid)') then
    begin
      FCurrentSector.Mines_Armid.Quantity := StrToIntSafe(GetParameter(Line, 3));
      FCurrentSector.Mines_Armid.Owner := S;
    end
    else
    begin
      FCurrentSector.Mines_Limpet.Quantity := StrToIntSafe(GetParameter(Line, 3));
      FCurrentSector.Mines_Limpet.Owner := S;
    end;
  end
  else if (Copy(Line, 1, 8) = '        ') then
  begin
    // Continue from last occurance

    if (FSectorPosition = spMines) then
    begin
      I := GetParameterPos(Line, 6) + 1;
      FCurrentSector.Mines_Limpet.Quantity := StrToIntSafe(GetParameter(Line, 2));
      FCurrentSector.Mines_Limpet.Owner := Copy(Line, I, length(Line) - I);
    end
    else if (FSectorPosition = spPorts) then
      FCurrentSector.SPort.BuildTime := StrToIntSafe(GetParameter(Line, 4))
    else if (FSectorPosition = spPlanets) then
    begin
      // Get planet data
      NewPlanet := AllocMem(SizeOf(TPlanet));
      TWXDatabase.NULLPlanet(NewPlanet^);
      NewPlanet^.Name := Copy(Line, 11, length(Line) - 10);
      FPlanetList.Add(NewPlanet);
    end
    else if (FSectorPosition = spTraders) then
    begin
      if (GetParameter(Line, 1) = 'in') then
      begin
        // Still working on one trader
        NewTrader := AllocMem(SizeOf(TTrader));
        I := GetParameterPos(Line, 2);
        NewTrader^.ShipName := Copy(Line, I, Pos('(', Line) - I - 1);
        I := Pos('(', Line);
        NewTrader^.ShipType := Copy(Line, I + 1, Pos(')', Line) - I - 1);
        NewTrader^.Name := FCurrentTrader.Name;
        NewTrader^.Figs := FCurrentTrader.Figs;
        FTraderList.Add(NewTrader);
      end
      else
      begin
        // New trader
        I := Pos(', w/', Line);
        FCurrentTrader.Name := Copy(Line, 11, I - 11);
        S := Copy(Line, I + 5, Pos(' ftrs', Line) - I - 5);
        StripChar(S, ',');
        FCurrentTrader.Figs := StrToIntSafe(S);
      end;
    end
    else if (FSectorPosition = spShips) then
    begin
      if (Copy(Line, 12, 1) = '(') then
      begin
        // Get the rest of the ship info
        NewShip := AllocMem(SizeOf(TShip));
        NewShip^.Name := FCurrentShip.Name;
        NewShip^.Owner := FCurrentShip.Owner;
        NewShip^.Figs := FCurrentShip.Figs;
        NewShip^.ShipType := Copy(Line, 13, Pos(')', Line) - 13);
        FShipList.Add(NewShip);
      end
      else
      begin
        // New ship
        I := Pos('[Owned by]', Line);
        FCurrentShip.Name := Copy(Line, 11, I - 12);
        FCurrentShip.Owner := Copy(Line, I + 11, Pos(', w/', Line) - I - 11);
        I := Pos(', w/', Line);
        S := Copy(Line, I + 5, Pos(' ftrs,', Line) - I - 5);
        StripChar(S, ',');
        FCurrentShip.Figs := StrToIntSafe(S);
        FSectorPosition := spShips;
      end;
    end;
  end
  else if (Copy(Line, 9, 1) = ':') then
    FSectorPosition := spNormal  
  else if (Copy(Line, 1, 20) = 'Warps to Sector(s) :') then
  begin
    StripChar(Line, '(');
    StripChar(Line, ')');

    // Get sector warps
    FCurrentSector.Warp[1] := StrToIntSafe(GetParameter(Line, 5));
    FCurrentSector.Warp[2] := StrToIntSafe(GetParameter(Line, 7));
    FCurrentSector.Warp[3] := StrToIntSafe(GetParameter(Line, 9));
    FCurrentSector.Warp[4] := StrToIntSafe(GetParameter(Line, 11));
    FCurrentSector.Warp[5] := StrToIntSafe(GetParameter(Line, 13));
    FCurrentSector.Warp[6] := StrToIntSafe(GetParameter(Line, 15));

    // sector done
    if not (FSectorSaved) then
      SectorCompleted;

    // No displays anymore, all done
    FCurrentDisplay := dNone;
    FSectorPosition := spNormal;
  end;
end;

procedure TModExtractor.ProcessPortLine(Line : String);
begin
  // Process a line from the CR port display
end;

procedure TModExtractor.ProcessVScreen(Line : String);
var
  tempStr : string;
begin
  if (Pos('StarDock is', Line) > 0) then
  begin
    tempStr := Copy(Line, 44, Pos('.', Line));
    tempStr := Copy(tempStr, 0, Length(tempStr) - 1);
    TWXDatabase.LastStardock := StrToInt(tempStr);
  end;
end;

procedure TModExtractor.ProcessLine(Line : String);
var
  S,
  X       : String;
  I       : Integer;
  Sect    : TSector;
begin
  // Every line is passed to this procedure to be processed and recorded

  if (FCurrentMessage <> '') then
  begin
    if (Line <> '') then
    begin
      if (FCurrentMessage = 'Figs') then
        TWXGUI.AddToHistory(htFighter, TimeToStr(Time) + '  ' + StripChars(Line))
      else if (FCurrentMessage = 'Comp') then
        TWXGUI.AddToHistory(htComputer, TimeToStr(Time) + '  ' + StripChars(Line))
      else
        TWXGUI.AddToHistory(htMsg, TimeToStr(Time) + '  ' + StripChars(Line));

      FCurrentMessage := '';
    end;
  end
  else if (Copy(Line, 1, 2) = 'R ') or (Copy(Line, 1, 2) = 'F ') then
    TWXGUI.AddToHistory(htMsg, TimeToStr(Time) + '  ' + StripChars(Line))
  else if (Copy(Line, 1, 2) = 'P ') then
  begin
    if (GetParameter(Line, 2) <> 'indicates') then
      TWXGUI.AddToHistory(htMsg, TimeToStr(Time) + '  ' + StripChars(Line))
  end
  else if (Copy(Line, 1, 26) = 'Incoming transmission from') or (Copy(Line, 1, 28) = 'Continuing transmission from') then
  begin
    // Transmission with ansi off
    I := GetParameterPos(Line, 4);

    if (Copy(Line, Length(Line) - 9, 10) = 'comm-link:') then
    begin
      // Fedlink
      FCurrentMessage := 'F ' + Copy(Line, I, Pos(' on Federation', Line) - I) + ' ';
    end
    else if (GetParameter(Line, 5) = 'Fighters:') then
    begin
      // Fighters
      FCurrentMessage := 'Figs';
    end
    else if (GetParameter(Line, 5) = 'Computers:') then
    begin
      // Computer
      FCurrentMessage := 'Comp';
    end
    else if (Pos(' on channel ', Line) <> 0) then
    begin
      // Radio
      FCurrentMessage := 'R ' + Copy(Line, I, Pos(' on channel ', Line) - I) + ' ';
    end
    else
    begin
      // hail
      FCurrentMessage := 'P ' + Copy(Line, I, Length(Line) - I) + ' ';
    end
  end
  else if (Copy(Line, 1, 31) = 'Deployed Fighters Report Sector') then
    TWXGUI.AddToHistory(htFighter, TimeToStr(Time) + '  ' + Copy(Line, 19, Length(Line)))
  else if (Copy(Line, 1, 20) = 'Shipboard Computers ') then
    TWXGUI.AddToHistory(htComputer, TimeToStr(Time) + '  ' + Copy(Line, 21, Length(Line)))
  else if (Copy(Line, 1, 19) = 'The shortest path (') or (Copy(Line, 1, 7) = '  TO > ') then
  begin
    FCurrentDisplay := dWarpLane;
    FLastWarp := 0;
  end
  else if (FCurrentDisplay = dWarpLane) then
    ProcessWarpLine(Line)
  else if (FCurrentDisplay = dWarpCIM) or (FCurrentDisplay = dPortCIM) then
    ProcessCIMLine(Line)
  else if (FCurrentDisplay = dCIM) then
  begin
    // find out what kind of CIM this is
    if (Length(Line) > 2) then
    begin
      if (Line[Length(Line) - 1] = '%') then
      begin
        TWXDatabase.LastPortCIM := Now;
        FCurrentDisplay := dPortCIM;
      end
      else
        FCurrentDisplay := dWarpCIM;

      ProcessCIMLine(Line);
    end;
  end
  else if (Copy(Line, 1, 10) = 'Sector  : ') then
  begin
    // Check if this is a probe or holoscan (no warp pickup)
    if not (FSectorSaved) then
      SectorCompleted;

    // Begin recording of sector data
    FCurrentDisplay := dSector;
    FSectorSaved := FALSE;

    // Clear sector variables
    TWXDatabase.NULLSector(FCurrentSector);

    FCurrentSectorIndex := StrToIntSafe(GetParameter(Line, 3));
    I := GetParameterPos(Line, 5);
    FCurrentSector.Constellation := Copy(Line, I, length(Line) - I + 1);
  end
  else if (FCurrentDisplay = dSector) then
    ProcessSectorLine(Line)
  else if (FCurrentDisplay = dPort) then
    ProcessPortLine(Line)
  else if (Copy(Line, 27, 16) = 'Relative Density') then
  begin
    // A density scanner is being used - lets grab some data
    FCurrentDisplay := dDensity;
  end
  else if (FCurrentDisplay = dDensity) and (Copy(Line, 1, 6) = 'Sector') then
  begin
    // Save all density data into sector database
    X := Line;
    StripChar(X, '(');
    StripChar(X, ')');
    I := StrToIntSafe(GetParameter(X, 2));
    Sect := TWXDatabase.LoadSector(I);
    S := GetParameter(X, 4);
    StripChar(S, ',');
    Sect.Density := StrToIntSafe(S);

    if (GetParameter(X, 13) = 'Yes') then
      // Sector has anomoly
      Sect.Anomoly := TRUE
    else
      Sect.Anomoly := FALSE;

    S := GetParameter(X, 10);
    S := Copy(S, 1, length(S) - 1);
    Sect.NavHaz := StrToIntSafe(S);

    Sect.Warps := StrToIntSafe(GetParameter(X, 7));

    if (Sect.Explored in [etNo, etCalc]) then
    begin
      // Sector hasn't been scanned or seen before
      Sect.Constellation := '???' + ANSI_9 + ' (Density only)';
      Sect.Explored := etDensity;
      Sect.Update := Now;
    end;

    TWXDatabase.SaveSector(Sect, I, nil, nil, nil);
  end
  else if (Copy(Line, 1, 28) = 'What sector is the port in? ') then
  begin
    // begin port data download

  end
  else if (Copy(Line, 1, 2) = ': ') then
  begin
    // begin CIM download
    FCurrentDisplay := dCIM;
  end
  else if (Copy(Line, 26, 54) = 'Game Configuration and Status') then
  begin
    FCurrentDisplay := dVScreen;
  end
  else if (FCurrentDisplay = dVScreen) then
  begin
    if (Copy(Line, 14, 24) = 'StarDock is') then
    begin
    end;
    ProcessVScreen(Line);
  end;

  TWXInterpreter.TextLineEvent(Line, FALSE);
  ProcessPrompt(Line);

  // Reactivate script triggers
  TWXInterpreter.ActivateTriggers;
end;

procedure TModExtractor.StripANSI(var S : String);
var
  I    : Integer;
  X    : String;
begin
  // Remove ANSI codes from text
  X := '';

  for I := 1 to length(S) do
  begin
    if (S[I] = #27) then
      FInAnsi := TRUE;

    if (FInAnsi = FALSE) then
      X := X + S[I];

    if ((Byte(S[I]) >= 65) and (Byte(S[I]) <= 90)) or ((Byte(S[I]) >= 97) and (Byte(S[I]) <= 122)) then
      FInAnsi := FALSE;
  end;

  S := X;
end;

procedure TModExtractor.ProcessInBound(var InData : String);
var
  X        : Integer;
  I        : Integer;
  S,
  ANSIS,
  ANSILine,
  Line     : string;
begin
  S := InData;

  // Remove null chars
  StripChar(S, #0);

  // strip the ANSI
  AnsiS := S;
  StripANSI(S);

  TWXLog.DoLogData(S, InData);

  // Remove linefeed
  StripChar(S, #10);
  StripChar(AnsiS, #10);

  // Form and process lines out of data
  I := 1;
  Line := CurrentLine + S;
  AnsiLine := CurrentANSILine + AnsiS;

  while (I <= Length(Line)) do
  begin
    if (Line[I] = #13) then
    begin
      // find the matching carriage return in the ansi line
      X := 1;
      
      if (Length(ANSILine) > 0) then
        while (ANSILine[X] <> #13) and (X < Length(ANSILine)) do
          Inc(X);

      CurrentLine := Copy(Line, 1, I - 1);
      CurrentANSILine := Copy(ANSILine, 1, X - 1);
      ProcessLine(CurrentLine);

      if (I < Length(Line)) then
      begin
        Line := Copy(Line, I + 1, Length(Line) - I);
        ANSILine := Copy(ANSILine, X + 1, Length(ANSILine) - X);
      end
      else
      begin
        Line := '';
        ANSILine := '';
        Break;
      end;

      I := 0;
    end;

    Inc(I);
  end;

  // Process what we have left
  CurrentLine := Line;
  CurrentANSILine := ANSILine;
  ProcessPrompt(CurrentLine);

end;



// ********************************************************************
// Process outbound data



function TModExtractor.ProcessOutBound(OutData : string; ClientIndex : Byte) : Boolean;
begin
  Result := TRUE;

  if (OutData[1] = MenuKey) and (TWXMenu.CurrentMenu = nil) then
  begin
    // Activate menu

    if not (TWXClient.Connected) then
    begin
      // User trying to access database while not connected

      if not (TWXDatabase.DataBaseOpen) then
        TWXServer.ClientMessage(endl + ANSI_12 + 'Warning: This database is corrupt or does not exist.  No data is available.' + ANSI_7 + endl);
    end;

    TWXMenu.OpenMenu('TWX_MAIN', ClientIndex);

    // run the rest of the text through the menus
    if (Length(OutData) > 1) then
      ProcessOutBound(Copy(OutData, 2, Length(OutData)), ClientIndex);

    Result := FALSE;
  end
  else if (TWXMenu.CurrentMenu <> nil) then
  begin
    if (OutData[1] = MenuKey) then
      // De-activate menu
      TWXMenu.CloseMenu(TRUE)
    else
      // Send commands to menu
      TWXMenu.MenuText(OutData, ClientIndex);

    Result := FALSE;
  end;

  // don't return a value if trigger had this key
  if (Result) and (OutData <> '') then
    Result := not TWXInterpreter.TextOutEvent(OutData, nil);
end;

end.



