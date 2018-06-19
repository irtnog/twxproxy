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
unit ScriptCmp;

interface

uses
  Core,
  SysUtils,
  Classes,
  ScriptRef,
  Contnrs;

const
  COMPILED_SCRIPT_VERSION = 10;

  PARAM_CMD = 0;
  PARAM_VAR = 1; // User variable prefix
  PARAM_CONST = 2; // Compiler string constant prefix
  PARAM_SYSCONST = 3; // Read only system value
  PARAM_PROGVAR = 4; // Program variable
  PARAM_CHAR = 5; // Character code

  OP_GREATEREQUAL = #230;
  OP_LESSEREQUAL = #231;
  OP_AND = #232;
  OP_OR = #233;
  OP_XOR = #234;
  OP_NOT = #235;
  OP_NOTEQUAL = #236;
  OP_NONE = #0;

type
  EScriptError = class(Exception);

  TScriptFileHeader = record // header at top of compiled script file
    ProgramName : string[14];
    Version     : Word;
    DescSize,
    CodeSize    : Integer;
  end;

  TConditionStruct = record
    ConLabel,
    EndLabel  : string;
    IsWhile,
    AtElse    : Boolean;
  end;

  // TVarParam: A variable within the script.  Typically referenced by its ID.  Can contain
  // a list of indexed values in the event of it being used as an array within the script.
  TVarParam = class(TCmdParam) // variable or variable array declared within script
  protected
    FName      : string;
    Vars       : TList; // list of variables indexed within this variable
    FArraySize : Integer; // static array size

  public
    constructor Create; override;
    destructor Destroy; override;

    function AddVar(NewVar : TVarParam) : Integer;
    procedure Dump(Tab : string);
    function GetIndexVar(Indexes : TStringArray) : TVarParam;
    procedure SetArray(Dimensions : array of Integer);

    property Name : string read FName write FName;
    property ArraySize : Integer read FArraySize write FArraySize;
  end;

  // TScriptLabel: A jump label within a script.
  TScriptLabel = class(TObject)
  protected
    FLocation : Integer;
    FName     : string;

  public
    constructor Create; reintroduce;

    property Location : Integer read FLocation write FLocation;
    property Name : string read FName write FName;
  end;

  TScriptCmp = class(TComponent)
  private
    IFStack           : TStack;
    FParamList,
    FLabelList        : TList;
    IncludeScriptList,
    FDescription      : TStringList;
    FScriptFile       : string;
    FCode             : Pointer;
    IFLabelCount,
    SysVarCount,
    WaitOnCount,
    FLineCount,
    FCmdCount,
    FCodeSize         : Integer;
    FScriptRef        : TScriptRef;

  protected
    function GetParamCount : Integer;
    function GetLabelCount : Integer;
    function GetParam(Index : Integer) : TCmdParam;
    function GetLabel(Index : Integer) : TScriptLabel;
    function GetIncludeScript(Index : Integer) : string;
    function ApplyEncryption(const Value : string; Key : Byte) : string;

    procedure AppendCode(NewCode : Pointer; NewCodeSize : Byte);
    procedure BuildLabel(const Name : string; Location : Integer);
    function IdentifyParam(ParamName : string) : Byte;
    procedure WriteCode(var CmdCode : string; Code : Pointer; CodeLength : Integer);
    procedure CompileValue(Value : string; var CmdCode : string; ParamKind : TParamKind; Line : Integer; ScriptID : Byte);
    procedure RecurseCmd(const CmdLine : array of string; Line : Integer; ScriptID : Byte);
    procedure CompileParam(Param : string; var CmdCode : string; ParamKind : TParamKind; Line : Integer; ScriptID : Byte);
    procedure CompileParamLine(const ParamLine : TStringList; const Line : Integer; const ScriptID : Byte);
    function ConvertOps(const Line : string) : string;
    function ConvertConditions(const Line : string) : string;
    function IsOperator(C : Char) : Boolean;
    procedure CompileFromStrings(ScriptText : TStringList; ScriptName : string);
    procedure IncludeFile(Filename : string);

  public
    constructor Create(ScriptRef : TScriptRef); reintroduce;
    destructor Destroy; override;

    procedure CompileFromFile(const Filename, DescFile : string);
    procedure AddParam(Param : TCmdParam);
    procedure LoadFromFile(const Filename : string);
    procedure WriteToFile(const Filename : string);
    procedure ExtendName(var Name : string; ScriptID : Integer);
    procedure ExtendLabelName(var Name : string; ScriptID : Integer);

    property Params[Index : Integer] : TCmdParam read GetParam;
    property Labels[Index : Integer] : TScriptLabel read GetLabel;
    property IncludeScripts[Index : Integer] : string read GetIncludeScript;

    property ParamCount : Integer read GetParamCount;
    property LabelCount : Integer read GetLabelCount;
    property Code : Pointer read FCode;
    property CodeSize : Integer read FCodeSize;
    property LineCount : Integer read FLineCount;
    property CmdCount : Integer read FCmdCount;
    property ScriptRef : TScriptRef read FScriptRef;
    property ScriptFile : string read FScriptFile;
  end;

implementation

uses
  Utility,
  Global,
  Ansi,
  Encryptor,
  Windows;


// ***************************************************************
// TVarParam implementation



constructor TVarParam.Create;
begin
  inherited;

  Vars := TList.Create;
  Name := '';
  FArraySize := 0;
end;

destructor TVarParam.Destroy;
begin
  // free all index element variables
  while (Vars.Count > 0) do
  begin
    TVarParam(Vars.Items[0]).Free;
    Vars.Delete(0);
  end;

  Vars.Free;
  Name := '';

  inherited;
end;

function TVarParam.AddVar(NewVar : TVarParam) : Integer;
begin
  // link up an index element variable

  Result := Vars.Add(NewVar);
end;

function TVarParam.GetIndexVar(Indexes : TStringArray) : TVarParam;
var
  J,
  I         : Integer;
  NextIndex : TStringArray;
  NewVar    : TVarParam;
begin
  // move through the array of index dimensions and return a reference to the
  // variable with the specified name/index.

  if (Length(Indexes) = 0) then
  begin
    Result := Self; // no index to search on
    Exit;
  end;

  SetLength(NextIndex, Length(Indexes) - 1);

  if (Length(Indexes) > 1) then
    for J := 1 to Length(Indexes) - 1 do
      NextIndex[J - 1] := Indexes[J];

  // search the index for a variable with a matching name
  if (FArraySize > 0) then
  begin
    // static array - we can look up the variable directly
    I := 0;

    try
      I := StrToInt(Indexes[0]);
    except
    end;

    if (I < 1) or (I > FArraySize) then
      raise EScriptError.Create('Static array index ''' + Indexes[0] + ''' is out of range (must be 1-'
        + IntToStr(FArraySize) + ')')
    else
      Result := TVarParam(Vars[I - 1]).GetIndexVar(NextIndex);
  end
  else
  begin
    Result := nil;

    if (Vars.Count > 0) then
      for I := 0 to Vars.Count - 1 do
        if (TVarParam(Vars[I]).Name = Indexes[0]) then
        begin
          Result := TVarParam(Vars[I]).GetIndexVar(NextIndex);
          Break;
        end;

    if (Result = nil) then
    begin
      // variable not found in index - make a new one
      NewVar := TVarParam.Create;
      NewVar.Name := Indexes[0];
      AddVar(NewVar);
      Result := NewVar.GetIndexVar(NextIndex);
    end;
  end;
end;

procedure TVarParam.SetArray(Dimensions : array of Integer);
var
  I,
  J         : Integer;
  NewVar    : TVarParam;
  NextDimen : array of Integer;
begin
  // make this variable a static array

  // delete all existing indexed variables
  while (Vars.Count > 0) do
  begin
    TVarParam(Vars[0]).Free;
    Vars.Delete(0);
  end;

  FArraySize := Dimensions[0];

  // build variables up until size limit
  try
    for I := 1 to Dimensions[0] do
    begin
      NewVar := TVarParam.Create;
      NewVar.Name := IntToStr(I);
      AddVar(NewVar);

      if (Length(Dimensions) > 1) then
      begin
        SetLength(NextDimen, Length(Dimensions) - 1);

        for J := 1 to Length(Dimensions) - 1 do
          NextDimen[J - 1] := Dimensions[J];

        NewVar.SetArray(NextDimen);
      end;
    end;
  except
    raise EScriptError.Create('Not enough memory to set static array');
  end;
end;

procedure TVarParam.Dump(Tab : string);
var
  I : Integer;
begin
  // broadcast variable details to active telnet connections

  if (Length(Name) >= 2) then
    if (Copy(Name, 1, 2) = '$$') then
      Exit; // don't dump system variables

  TWXServer.AddBuffer(Tab + ANSI_15 + '"' + ANSI_7 + Name + ANSI_15 + '" = "' + ANSI_7 + Value + ANSI_15 + '"' + endl);

  if (Vars.Count > 0) then
  begin
    // dump array contents
    if (FArraySize > 0) then
      TWXServer.AddBuffer(Tab + ANSI_15 + 'Static array of "' + Name + '" (size ' + IntToStr(Vars.Count) + ')' + endl)
    else
      TWXServer.AddBuffer(Tab + ANSI_15 + 'Dynamic array of "' + Name + '" (size ' + IntToStr(Vars.Count) + ')' + endl);

    for I := 0 to Vars.Count - 1 do
      TVarParam(Vars.Items[I]).Dump(Tab + '  ');
  end;
end;


// ***************************************************************
// TScriptLabel implementation


constructor TScriptLabel.Create;
begin
  inherited Create;

  // set defaults
  Location := 0;
  Name := '';
end;


// ***************************************************************
// TScriptCmp implementation


constructor TScriptCmp.Create(ScriptRef : TScriptRef);
begin
  FParamList := TList.Create;
  FLabelList := TList.Create;
  FDescription := TStringList.Create;
  FScriptRef := ScriptRef;
  IncludeScriptList := TStringList.Create;
  IFStack := TStack.Create;

  FLineCount := 0;
  FCmdCount := 0;
end;

destructor TScriptCmp.Destroy;
var
  ConStruct : ^TConditionStruct;
begin
  while (FParamList.Count > 0) do
  begin
    Params[0].Free;
    FParamList.Delete(0);
  end;

  FParamList.Free;

  while (FLabelList.Count > 0) do
  begin
    Labels[0].Free;
    FLabelList.Delete(0);
  end;

  FLabelList.Free;
  FDescription.Free;

  // free up IF stack
  while (IFStack.Count > 0) do
  begin
    ConStruct := IFStack.Pop;
    ConStruct^.ConLabel := '';
    ConStruct^.EndLabel := '';
    FreeMem(ConStruct);
  end;

  IFStack.Free;

  if (FCode <> nil) then
    FreeMem(FCode, FCodeSize);

  IncludeScriptList.Free;
end;

function TScriptCmp.GetParamCount : Integer;
begin
  Result := FParamList.Count;
end;

function TScriptCmp.GetLabelCount : Integer;
begin
  Result := FLabelList.Count;
end;

function TScriptCmp.GetParam(Index : Integer) : TCmdParam;
begin
  Result := FParamList[Index];
end;

function TScriptCmp.GetLabel(Index : Integer) : TScriptLabel;
begin
  Result := FLabelList[Index];
end;

function TScriptCmp.GetIncludeScript(Index : Integer) : string;
begin
  Result := IncludeScriptList[Index];
end;



// ***************************************************************
// Script compilation


procedure TScriptCmp.AppendCode(NewCode : Pointer; NewCodeSize : Byte);
var
  B  : Byte;
  P1,
  P2 : Pointer;
begin
  // write this data to the end of the byte-code

  ReallocMem(FCode, FCodeSize + NewCodeSize);
  P1 := Pointer(Integer(FCode) + FCodeSize);
  P2 := NewCode;

  for B := 1 to NewCodeSize do
  begin
    Byte(P1^) := Byte(P2^);
    P1 := Pointer(Integer(P1) + 1);
    P2 := Pointer(Integer(P2) + 1);
  end;

  Inc(FCodeSize, NewCodeSize);
end;

procedure TScriptCmp.BuildLabel(const Name : string; Location : Integer);
var
  NewLabel : TScriptLabel;
begin
  // create a new label - label's constructor will add it to label list automatically

  NewLabel := TScriptLabel.Create;
  NewLabel.Name := Name;
  NewLabel.Location := Location;
  FLabelList.Add(NewLabel);
end;

procedure TScriptCmp.ExtendName(var Name : string; ScriptID : Integer);
begin
  if (Pos('~', Name) = 0) then
  begin
    if (ScriptID > 0) then
      Name := IncludeScriptList[ScriptID] + '~' + Name;
  end
  else
  begin
    if (Name <> '') then
      if (Name[1] = '~') then
      begin
        if (Length(Name) = 1) then
          raise EScriptError.Create('Bad name');

        Name := Copy(Name, 2, Length(Name));
      end;
  end;
end;

procedure TScriptCmp.ExtendLabelName(var Name : string; ScriptID : Integer);
begin
  if (Pos('~', Name) = 0) and (ScriptID > 0) then
    Name := ':' + IncludeScriptList[ScriptID] + '~' + Copy(Name, 2, Length(Name));
end;

function TScriptCmp.IdentifyParam(ParamName : string) : Byte;
var
  IndexLevel,
  I          : Integer;
  ConstName  : string;
begin
  // identify the type of this parameter

  if (ParamName[1] = '$') then
    Result := PARAM_VAR
  else if (ParamName[1] = '%') then
    Result := PARAM_PROGVAR
  else if (ParamName[1] = '#') then
    Result := PARAM_CHAR
  else
  begin
    Result := PARAM_CONST;

    // remove indexes from constant name (if its a constant)
    IndexLevel := 0;
    ConstName := '';

    for I := 1 to Length(ParamName) do
    begin
      if (ParamName[I] = '[') then
        Inc(IndexLevel)
      else if (ParamName[I] = ']') then
        Dec(IndexLevel)
      else if (IndexLevel = 0) then
        ConstName := ConstName + ParamName[I];
    end;

    // check for system constant
    if (ScriptRef.FindSysConst(ConstName) > -1) then
      Result := PARAM_SYSCONST;
  end;
end;

function TScriptCmp.ApplyEncryption(const Value : string; Key : Byte) : string;
var
  I : Integer;
begin
  Result := '';

  if (Length(Value) > 0) then
    for I := 1 to Length(Value) do
      Result := Result + Char(Byte(Value[I]) xor Key);
end;

procedure TScriptCmp.WriteCode(var CmdCode : string; Code : Pointer; CodeLength : Integer);
var
  S : string;
begin
  SetString(S, PChar(Code), CodeLength);
  CmdCode := CmdCode + S;
end;

procedure TScriptCmp.CompileValue(Value : string; var CmdCode : string; ParamKind : TParamKind; Line : Integer; ScriptID : Byte);
// Value can be a variable name, a constant, a system constant, a program variable or a character

  procedure QuotationError;
  begin
    raise EScriptError.Create('Quotation syntax error');
  end;

  procedure ParamTypeError;
  begin
    raise EScriptError.Create('Invalid command parameter type ''' + Value + '''');
  end;

  procedure BuildIndexList(IndexList : TStringList; var Name : string);
  var
    RetnName,
    Index      : string;
    I,
    IndexDepth : Integer;
  begin
    RetnName := '';
    IndexDepth := 0;
    Index := '';

    for I := 1 to Length(Name) do
    begin
      if (Name[I] = '[') then
      begin
        if (IndexDepth > 0) then
          Index := Index + Name[I];

        Inc(IndexDepth)
      end
      else if (Name[I] = ']') then
      begin
        Dec(IndexDepth);

        if (IndexDepth > 0) then
          Index := Index + Name[I]
        else if (IndexDepth < 0) then
          raise EScriptError.Create('Array syntax error');

        if (IndexDepth = 0) then
        begin
          if (Index = '') then
            raise EScriptError.Create('Expected array index specifier');

          IndexList.Add(Index);
          Index := '';
        end;
      end
      else if (IndexDepth = 0) then
        RetnName := RetnName + Name[I]
      else
        Index := Index + Value[I];
    end;

    if (IndexDepth > 0) then
      raise EScriptError.Create('Array syntax error');

    Name := RetnName;
  end;

  procedure WriteIndexList(IndexList : TStringList);
  var
    CodeByte : Byte;
    I        : Integer;
  begin
    if (IndexList.Count > 255) then
      raise EScriptError.Create('Too many array dimensions');

    CodeByte := IndexList.Count;
    WriteCode(CmdCode, @CodeByte, 1); // write index count

    // loop through index list and process each index value as a separate parameter
    for I := 0 to IndexList.Count - 1 do
      CompileParam(IndexList.Strings[I], CmdCode, pkValue, Line, ScriptID);
  end;

var
  CodeByte,
  PType      : Byte;
  NewConst   : TCmdParam;
  NewVar     : TVarParam;
  IndexList  : TStringList;
  ID         : Integer;
  Found      : Boolean;
  CodeWord   : Word;
begin
  PType := 0;

  if (Pos('"', Value) > 0) then
  begin
    // constant - remove the quotes

    if (Value[1] <> '"') or (Value[Length(Value)] <> '"') then
      QuotationError;

    Value := StringReplace(Copy(Value, 2, Length(Value) - 2), '*', #13, [rfReplaceAll]);

    if (Pos('"', Value) > 0) then
      QuotationError;

    PType := PARAM_CONST;
  end
  else
    PType := IdentifyParam(Value);

  // write value type to byte code
  WriteCode(CmdCode, @PType, 1);

  if (PType = PARAM_CONST) then
  begin
    // write 32-bit constant reference

    if (ParamKind <> pkValue) then
      ParamTypeError;

    NewConst := TCmdParam.Create;
    NewConst.Value := Value;
    ID := FParamList.Add(NewConst);
    WriteCode(CmdCode, @ID, 4);
  end
  else if (PType = PARAM_VAR) then
  begin
    // write 32-bit variable reference

    if (Length(Value) < 2) then
      raise EScriptError.Create('Variable name expected');

    if (ScriptID > 0) and (Pos('~', Value) = 0) then
      Value := '$' + IncludeScriptList[ScriptID] + '~' + Copy(Value, 2, Length(Value));

    if (ParamKind <> pkValue) and (ParamKind <> pkVar) then
      ParamTypeError;

    // generate index list from variable, stripping out index specifiers
    IndexList := TStringList.Create;

    try
      BuildIndexList(IndexList, Value);

      // see if the variable exists
      Found := FALSE;

      if (FParamList.Count > 0) then
        for ID := 0 to FParamList.Count - 1 do
          if (TObject(FParamList.Items[ID]) is TVarParam) then
            if (TVarParam(FParamList.Items[ID]).Name = Value) then
            begin
              Found := TRUE;
              Break;
            end;

      if not (Found) then
      begin
        // build new variable
        NewVar := TVarParam.Create;
        NewVar.Name := Value;
        ID := FParamList.Add(NewVar);
      end;

      WriteCode(CmdCode, @ID, 4); // write variable reference
      WriteIndexList(IndexList); // write variable indexes
    finally
      IndexList.Free;
    end;
  end
  else if (PType = PARAM_SYSCONST) then
  begin
    // write 16-bit system constant reference

    if (ParamKind <> pkValue) then
      ParamTypeError;

    // generate index list for this constant
    IndexList := TStringList.Create;

    try
      BuildIndexList(IndexList, Value);

      // get the ID of this system const
      CodeWord := ScriptRef.FindSysConst(Value);
      
      WriteCode(CmdCode, @CodeWord, 2);
      WriteIndexList(IndexList);
    finally
      IndexList.Free;
    end;
  end
  else if (PType = PARAM_PROGVAR) then
  begin
    // write 32-bit program variable reference

    if (ParamKind <> pkValue) then
      ParamTypeError;

    // find the program variable matching this one
  end
  else if (PType = PARAM_CHAR) then
  begin
    // write 8-bit character code

    if (ParamKind <> pkValue) then
      ParamTypeError;

    if (Length(Value) < 2) then
      raise EScriptError.Create('No character code supplied');

    try
      ID := StrToInt(Copy(Value, 2, Length(Value)));
    except
      raise EScriptError.Create('Bad character code');
    end;

    if (ID < 0) or (ID > 255) then
      raise EScriptError.Create('Character #' + IntToStr(ID) + ' does not exist');

    CodeByte := ID;
    WriteCode(CmdCode, @CodeByte, 1);
  end;
end;

procedure TScriptCmp.RecurseCmd(const CmdLine : array of string; Line : Integer; ScriptID : Byte);
var
  ParamLine : TStringList;
  I         : Integer;
begin
  // convert the CmdLine into a string list and throw it all back through compiler
  ParamLine := TStringList.Create;

  try
    for I := 0 to Length(CmdLine) - 1 do
      ParamLine.Append(CmdLine[I]);

    CompileParamLine(ParamLine, Line, ScriptID);
  finally
    ParamLine.Free;
  end;
end;

procedure TScriptCmp.CompileParam(Param : string; var CmdCode : string; ParamKind : TParamKind; Line : Integer; ScriptID : Byte);
// Param is a full equation of values joined by operators, it must be broken down and
// the values passed to CompileValue
type
  PBranch = ^TBranch;
  TBranch = record
    Value1,
    Value2 : Pointer;
    Op     : Char;
  end;

  function SplitOperator(const Equation : string; const Ops : string; var Value1, Value2 : string; var Op : Char) : Boolean;
  var
    InQuote      : Boolean;
    BracketLevel,
    I,
    J            : Integer;
  begin
    // seek out the specified operator within this equation, and split the values around it

    BracketLevel := 0;
    Result := FALSE;
    InQuote := FALSE;

    for I := 1 to Length(Equation) do
    begin
      if (Equation[I] = '"') then
        InQuote := not InQuote
      else if not (InQuote) then
      begin
        if (Equation[I] = '(') then
          Inc(BracketLevel)
        else if (Equation[I] = ')') then
        begin
          Dec(BracketLevel);

          if (BracketLevel < 0) then
            raise EScriptError.Create('Bracket mismatch');
        end
        else if (BracketLevel = 0) then
          for J := 1 to Length(Ops) do
            if (Ops[J] = Equation[I]) then
            begin
              if (I = 1) or (I = Length(Equation)) then
                raise EScriptError.Create('Operation syntax error');

              // split into values
              Value1 := Copy(Equation, 1, I - 1);
              Value2 := Copy(Equation, I + 1, Length(Equation));

              Op := Ops[J]; // return the operator that was found

              Result := TRUE;
              Exit;
            end;
      end;
    end;
  end;

  function BreakDown(Equation : string) : PBranch;
  var
    Op           : Char;
    V1,
    V2           : string;
    Encased,
    Split,
    InQuote,
    TestBrackets : Boolean;
    BracketLevel,
    I            : Integer;
  begin
    Result := AllocMem(SizeOf(TBranch));

    try
      repeat
        TestBrackets := FALSE;

        if (Length(Equation) > 1) then
        begin
          if (Equation[1] = '(') and (Equation[Length(Equation)] = ')') then
          begin
            if (Equation = '()') then
              raise EScriptError.Create('Empty brackets');

            BracketLevel := 0;
            Encased := TRUE;
            InQuote := FALSE;

            for I := 1 to Length(Equation) do
            begin
              if (Equation[I] = '"') then
                InQuote := not InQuote
              else if not (InQuote) then
              begin
                if (Equation[I] = '(') then
                  Inc(BracketLevel)
                else if (Equation[I] = ')') then
                begin
                  Dec(BracketLevel);

                  if (BracketLevel = 0) and (I <> Length(Equation)) then
                  begin
                    Encased := FALSE;
                    Break;
                  end;
                end;
              end;
            end;

            if (Encased) then
            begin
              // entire equation is encased in brackets, strip them out
              Equation := Copy(Equation, 2, Length(Equation) - 2);
              TestBrackets := TRUE;
            end;
          end;
        end;
      until not (TestBrackets);

      Split := TRUE;

     if not (SplitOperator(Equation, '=<>&'
       + OP_GREATEREQUAL
       + OP_LESSEREQUAL
       + OP_AND
       + OP_OR
       + OP_XOR
       + OP_NOTEQUAL, V1, V2, Op)) then
       if not (SplitOperator(Equation, '+-', V1, V2, Op)) then
         if not (SplitOperator(Equation, '*/', V1, V2, Op)) then
           Split := FALSE;

      if (Split) then
      begin
        Result^.Value1 := BreakDown(V1);
        Result^.Value2 := BreakDown(V2);
        Result^.Op := Op;
      end
      else
      begin
        // no operators left, just a value
        Result^.Op := OP_NONE;
        Result^.Value1 := StrNew(PChar(Equation));
      end;

    except
      // trap exceptions to free up CalcUnit
      on E : Exception do
      begin
        FreeMem(Result);
        raise E;
      end;
    end;
  end;

  function CompileTree(Branch : PBranch) : string;
  var
    Value1,
    Value2 : string;
  begin
    // return the name of the variable containing the result of this operation
    Inc(SysVarCount);
    Result := '$$' + IntToStr(SysVarCount);

    if (Branch^.Op = OP_NONE) then
    begin
      // its a value
      SetString(Result, PChar(Branch^.Value1), StrLen(Branch^.Value1));
      StrDispose(PChar(Branch^.Value1));
    end
    else
    begin
      // its an operation
      Value1 := CompileTree(Branch^.Value1);
      Value2 := CompileTree(Branch^.Value2);

      if (Branch^.Op = '+') then
      begin
        // addition (+): add the values together
        RecurseCmd(['SETVAR', Result, Value1], Line, ScriptID);
        RecurseCmd(['ADD', Result, Value2], Line, ScriptID);
      end
      else if (Branch^.Op = '-') then
      begin
        // subraction (-): subtract Value2 from Value1
        RecurseCmd(['SETVAR', Result, Value1], Line, ScriptID);
        RecurseCmd(['SUBTRACT', Result, Value2], Line, ScriptID);
      end
      else if (Branch^.Op = '*') then
      begin
        // multiplication (*): multiply values together
        RecurseCmd(['SETVAR', Result, Value1], Line, ScriptID);
        RecurseCmd(['MULTIPLY', Result, Value2], Line, ScriptID);
      end
      else if (Branch^.Op = '/') then
      begin
        // division (/): divide Value1 by Value2
        RecurseCmd(['SETVAR', Result, Value1], Line, ScriptID);
        RecurseCmd(['DIVIDE', Result, Value2], Line, ScriptID);
      end
      else if (Branch^.Op = '&') then
        // concatenation (&): concatenate both values
        RecurseCmd(['MERGETEXT', Value1, Value2, Result], Line, ScriptID)
      else if (Branch^.Op = '=') then
        // equal test (=): set result to 1 if both values are equal, otherwise 0
        RecurseCmd(['ISEQUAL', Result, Value1, Value2], Line, ScriptID)
      else if (Branch^.Op = '>') then
        // greater than test (>): set result to 1 if Value1 > Value2, otherwise 0
        RecurseCmd(['ISGREATER', Result, Value1, Value2], Line, ScriptID)
      else if (Branch^.Op = '<') then
        // lesser than test (<): set result to 1 if Value1 < Value2, otherwise 0
        RecurseCmd(['ISLESSER', Result, Value1, Value2], Line, ScriptID)
      else if (Branch^.Op = OP_GREATEREQUAL) then
        // greater than or equal to test (>=): set result to 1 if Value1 >= Value2, otherwise 0
        RecurseCmd(['ISGREATEREQUAL', Result, Value1, Value2], Line, ScriptID)
      else if (Branch^.Op = OP_LESSEREQUAL) then
        // lesser than or equal to test (<=): set result to 1 if Value1 <= Value2, otherwise 0
        RecurseCmd(['ISLESSEREQUAL', Result, Value1, Value2], Line, ScriptID)
      else if (Branch^.Op = OP_NOTEQUAL) then
        // not equal to test (<>): set result to 1 if values are not equal, otherwise 0
        RecurseCmd(['ISNOTEQUAL', Result, Value1, Value2], Line, ScriptID)
      else if (Branch^.Op = OP_AND) then
      begin
        // AND boolean operator (AND)
        RecurseCmd(['SETVAR', Result, Value1], Line, ScriptID);
        RecurseCmd(['AND', Result, Value2], Line, ScriptID);
      end
      else if (Branch^.Op = OP_OR) then
      begin
        // OR boolean operator (OR)
        RecurseCmd(['SETVAR', Result, Value1], Line, ScriptID);
        RecurseCmd(['OR', Result, Value2], Line, ScriptID);
      end
      else if (Branch^.Op = OP_XOR) then
      begin
        // Exclusive OR boolean operator (XOR)
        RecurseCmd(['SETVAR', Result, Value1], Line, ScriptID);
        RecurseCmd(['XOR', Result, Value2], Line, ScriptID);
      end;
    end;

    // free up the branch
    FreeMem(Branch);
  end;
var
  Root  : PBranch;
  Value : string;
begin
  // Param is a set of Command parameters linked by operators, i.e. ($VAR+50)&"text"
  // We need to form a binary tree of all operations to be performed

  Root := BreakDown(Param);

  if (Root^.Op = OP_NONE) then
  begin
    // tree is only one value, compile it
    SetString(Value, PChar(Root^.Value1), StrLen(Root^.Value1));
    StrDispose(PChar(Root^.Value1));
    FreeMem(Root);
    CompileValue(Value, CmdCode, ParamKind, Line, ScriptID);
  end
  else
  begin
    if (ParamKind = pkVar) then
      raise EScriptError.Create('Command parameter must be a variable'); 

    // branch up the tree and process each operation
    CompileValue(CompileTree(Root), CmdCode, ParamKind, Line, ScriptID);
  end;
end;

procedure TScriptCmp.CompileParamLine(const ParamLine : TStringList; const Line : Integer; const ScriptID : Byte);
var
  LineWord,
  ID        : Word;
  I         : Integer;
  LabelName,
  CmdCode   : string;
  ConStruct : ^TConditionStruct;
begin
  // ParamLine is a broken down list of all params in the line, including the command.

  // TEMP:
//  for I := 0 to ParamLine.Count - 1 do
//    TWXServer.Broadcast(ParamLine[I] + ' ');

//  TWXServer.Broadcast(endl);

  if (ParamLine[0][1] = ':') then
  begin
    // label - record its position

    if (ParamLine.Count > 1) then
      raise EScriptError.Create('Unnecessary parameters after label declaration');

    if (Length(ParamLine[0]) < 2) then
      raise EScriptError.Create('Bad label name');

    LabelName := Copy(ParamLine[0], 2, Length(ParamLine[0]));

    if (Pos('~', LabelName) = 0) and (ScriptID > 0) then
      BuildLabel(IncludeScriptList[ScriptID] + '~' + LabelName, FCodeSize)
    else
      BuildLabel(LabelName, FCodeSize);
  end
  else
  begin
    // check for macro commands
    if (ParamLine[0] = 'WHILE') then
    begin
      if (ParamLine.Count > 2) then
        raise EScriptError.Create('Unnecessary parameters after WHILE macro')
      else if (ParamLine.Count < 2) then
        raise EScriptError.Create('No parameters to compare with WHILE macro');

      // write WHILE macro

      ConStruct := AllocMem(SizeOf(TConditionStruct));
      ConStruct^.IsWhile := TRUE;
      Inc(IFLabelCount);
      ConStruct^.ConLabel := '::' + IntToStr(IFLabelCount);
      Inc(IFLabelCount);
      ConStruct^.EndLabel := '::' + IntToStr(IFLabelCount);
      IFStack.Push(ConStruct);
      RecurseCmd([ConStruct^.ConLabel], Line, ScriptID);
      RecurseCmd(['BRANCH', ParamLine[1], ConStruct^.EndLabel], Line, ScriptID);
    end
    else if (ParamLine[0] = 'IF') then
    begin
      if (ParamLine.Count > 2) then
        raise EScriptError.Create('Unnecessary parameters after IF macro')
      else if (ParamLine.Count < 2) then
        raise EScriptError.Create('No parameters to compare with IF macro');

      // write IF macro
      ConStruct := AllocMem(SizeOf(TConditionStruct));
      ConStruct^.AtElse := FALSE;
      ConStruct^.IsWhile := FALSE;
      Inc(IFLabelCount);
      ConStruct^.ConLabel := '::' + IntToStr(IFLabelCount);
      Inc(IFLabelCount);
      ConStruct^.EndLabel := '::' + IntToStr(IFLabelCount);
      IFStack.Push(ConStruct);
      RecurseCmd(['BRANCH', ParamLine[1], ConStruct^.ConLabel], Line, ScriptID);
    end
    else if (ParamLine[0] = 'ELSE') then
    begin
      if (ParamLine.Count > 1) then
        raise EScriptError.Create('Unnecessary parameters after ELSE macro');

      if (IFStack.Count = 0) then
        raise EScriptError.Create('ELSE without IF');

      // write ELSE macro
      ConStruct := IFStack.Peek;

      if (ConStruct.IsWhile) then
        raise EScriptError.Create('Cannot use ELSE with WHILE');

      if (ConStruct.AtElse) then
        raise EScriptError.Create('IF macro syntax error');

      ConStruct^.AtElse := TRUE;
      RecurseCmd(['GOTO', ConStruct^.EndLabel], Line, ScriptID);
      RecurseCmd([ConStruct^.ConLabel], Line, ScriptID);
    end
    else if (ParamLine[0] = 'ELSEIF') then
    begin
      if (ParamLine.Count > 2) then
        raise EScriptError.Create('Unnecessary parameters after ELSEIF macro')
      else if (ParamLine.Count < 2) then
        raise EScriptError.Create('No parameters to compare with ELSEIF macro');

      if (IFStack.Count = 0) then
        raise EScriptError.Create('ELSEIF without IF');

      // write ELSEIF macro
      ConStruct := IFStack.Peek;

      if (ConStruct.IsWhile) then
        raise EScriptError.Create('Cannot use ELSEIF with WHILE');

      if (ConStruct.AtElse) then
        raise EScriptError.Create('IF macro syntax error');

      RecurseCmd(['GOTO', ConStruct^.EndLabel], Line, ScriptID);
      RecurseCmd([ConStruct^.ConLabel], Line, ScriptID);
      Inc(IFLabelCount);
      ConStruct^.ConLabel := '::' + IntToStr(IFLabelCount);
      RecurseCmd(['BRANCH', ParamLine[1], ConStruct^.ConLabel], Line, ScriptID);
    end
    else if (ParamLine[0] = 'END') then
    begin
      if (ParamLine.Count > 1) then
        raise EScriptError.Create('Unnecessary parameters after END macro');

      if (IFStack.Count = 0) then
        raise EScriptError.Create('END without IF');

      // write END macro
      ConStruct := IFStack.Pop;

      if (ConStruct^.IsWhile) then
        RecurseCmd(['GOTO', ConStruct^.ConLabel], Line, ScriptID)
      else
        RecurseCmd([ConStruct^.ConLabel], Line, ScriptID);
        
      RecurseCmd([ConStruct^.EndLabel], Line, ScriptID);
      ConStruct^.ConLabel := '';
      ConStruct^.EndLabel := '';
      FreeMem(ConStruct);
    end
    else if (ParamLine[0] = 'INCLUDE') then
    begin
      if (ParamLine.Count > 2) then
        raise EScriptError.Create('Unnecessary parameters after INCLUDE macro')
      else if (ParamLine.Count < 2) then
        raise EScriptError.Create('No file name supplied for INCLUDE macro');

      if (ParamLine[1][1] = '"') then
        ParamLine[1] := Copy(ParamLine[1], 2, Length(ParamLine[1]) - 2);

      // include script
      IncludeFile(ParamLine[1]);
    end
    else if (ParamLine[0] = 'WAITON') then
    begin
      // WaitOn macro - create text trigger and label
      if (ParamLine.Count > 2) then
        raise EScriptError.Create('Unnecessary parameters after WAITON macro')
      else if (ParamLine.Count < 2) then
        raise EScriptError.Create('No wait text supplied for WAITON macro');

      Inc(WaitOnCount);

      RecurseCmd(['SETTEXTTRIGGER', 'WAITON' + IntToStr(WaitOnCount), ':WAITON' + IntToStr(WaitOnCount), ParamLine[1]], Line, ScriptID);
      RecurseCmd(['PAUSE'], Line, ScriptID);
      RecurseCmd([':WAITON' + IntToStr(WaitOnCount)], Line, ScriptID); 
    end
    else
    begin
      // identify script command
      Inc(FCmdCount);
      I := ScriptRef.FindCmd(ParamLine[0]);

      if (I < 0) then
        // command not found, give error message
        raise EScriptError.Create('Unknown command ''' + ParamLine[0] + '''');

      ID := I;

      CmdCode := '';
      LineWord := Line;
      WriteCode(CmdCode, @ScriptID, 1);
      WriteCode(CmdCode, @LineWord, 2);
      WriteCode(CmdCode, @ID, 2);

      // check if we have the right number of parameters
      if (ParamLine.Count - 1 > ScriptRef.Cmds[ID].MaxParams) and (ScriptRef.Cmds[ID].MaxParams > -1) then
        raise EScriptError.Create('Too many parameters for command ''' + ParamLine[0] + '''')
      else if (ParamLine.Count - 1 < ScriptRef.Cmds[ID].MinParams) then
        raise EScriptError.Create('Too few parameters for command ''' + ParamLine[0] + '''');

      // compile the parameters independantly
      for I := 1 to ParamLine.Count - 1 do
        CompileParam(ParamLine[I], CmdCode, ScriptRef.Cmds[ID].ParamKinds[I - 1], Line, ScriptID);

      // write the command to byte code (with null termination)
      AppendCode(PChar(CmdCode), Length(CmdCode) + 1);
    end;
  end;
end;

function TScriptCmp.ConvertOps(const Line : string) : string;
var
  I       : Integer;
  UpParam,
  Param   : string;
  InQuote : Boolean;
begin
  Param := '';
  Result := '';
  InQuote := FALSE;

  for I := 1 to Length(Line) do
  begin
    if (Line[I] = '"') then
      InQuote := not InQuote;

    if (Line[I] = ' ') then
    begin
      if not (InQuote) then
      begin
        UpParam := UpperCase(Param);

        if (UpParam = 'AND') then
          Param := OP_AND
        else if (UpParam = 'OR') then
          Param := OP_OR
        else if (UpParam = 'XOR') then
          Param := OP_XOR;
      end;

      Result := Result + Param + ' ';
      Param := '';
    end
    else
      Param := Param + Line[I];
  end;
end;

function TScriptCmp.ConvertConditions(const Line : string) : string;
var
  I       : Integer;
  C,
  Last    : Char;
  InQuote : Boolean;
begin
  // convert multi-character conditions (>=, <> and <=) into single character ones
  Last := #0;
  Result := '';
  InQuote := FALSE;

  for I := 1 to Length(Line) do
  begin
    C := Line[I];

    if (C = '"') then
    begin
      InQuote := not InQuote;
      Result := Result + C;
    end
    else if (C = '=') and not (InQuote) then
    begin
      if (Last = '>') then
        Result := Result + OP_GREATEREQUAL
      else if (Last = '<') then
        Result := Result + OP_LESSEREQUAL
      else
        Result := Result + '=';
    end
    else if ((C <> '>') and (C <> '<')) or (InQuote) then
    begin
      if ((Last = '>') or (Last = '<')) and not (InQuote) then
        Result := Result + Last + C
      else
        Result := Result + C;
    end
    else if (Last = '<') and (C = '>') then
    begin
      Result := Result + OP_NOTEQUAL;
      C := #0;
    end;

    Last := C;
  end;
end;

function TScriptCmp.IsOperator(C : Char) : Boolean;
begin
  if (C = '=') or
     (C = '>') or
     (C = '<') or
     (C = '+') or
     (C = '-') or
     (C = '*') or
     (C = '/') or
     (C = '&') or
     (C = OP_GREATEREQUAL) or
     (C = OP_LESSEREQUAL) or
     (C = OP_AND) or
     (C = OP_OR) or
     (C = OP_XOR) or
     (C = OP_NOTEQUAL) then
    Result := TRUE
  else
    Result := FALSE;
end;

procedure TScriptCmp.CompileFromStrings(ScriptText : TStringList; ScriptName : string);
var
  ScriptID    : Byte;
  Line,
  I           : Integer;
  ParamStr,
  LineText    : string;
  Last        : Char;
  Linked,
  InQuote     : Boolean;
  ParamLine   : TStringList;
begin
  FLineCount := FLineCount + ScriptText.Count;
  ScriptID := IncludeScriptList.Add(UpperCase(ScriptName));
  ParamLine := TStringList.Create;

  Line := 1;

  try
    try
      while (Line <= ScriptText.Count) do
      begin
        Last := ' ';
        ParamStr := '';
        LineText := ConvertConditions(ConvertOps(ScriptText.Strings[Line - 1] + ' '));
        InQuote := FALSE;
        Linked := FALSE;
        ParamLine.Clear;

        if (Length(LineText) > 0) then
        begin
          for I := 1 to Length(LineText) do
          begin
            if (LineText[I] = '#') and (ParamStr = '') and (ParamLine.Count = 0) then
              Break; // its a comment

            if not (InQuote) and (IsOperator(LineText[I])) then
            begin
              if (Linked) then
                raise EScriptError.Create('Operation syntax error');

              Linked := TRUE;
              ParamStr := ParamStr + LineText[I];
            end
            else if ((LineText[I] <> ' ') and (LineText[I] <> #9)) or (InQuote) then
            begin
              if ((Last = ' ') or (Last = #9)) and not (Linked) and not (InQuote) and (ParamStr <> '') then
              begin
                // parameter completed
                ParamLine.Append(ParamStr);
                ParamStr := '';
              end;

              if (InQuote) then
                ParamStr := ParamStr + LineText[I]
              else
                ParamStr := ParamStr + UpCase(LineText[I]);

              if (LineText[I] = '"') then
                InQuote := not InQuote;

              Linked := FALSE;
            end;

            Last := LineText[I];
          end;

          // reset our system variable count (for operators)
          SysVarCount := 0;

          // complete last param
          ParamLine.Append(ParamStr);

          // compile the line
          if not ((ParamLine.Count = 0) or ((ParamStr = '') and (ParamLine.Count = 1))) then
            CompileParamLine(ParamLine, Line, ScriptID);
        end;

        Inc(Line);
      end;

      // make sure our IF/ELSE/END blocks match up properly
      if (IFStack.Count <> 0) then
        raise EScriptError.Create('IF/WHILE .. END structure mismatch');
    except
      // add a line number to the exception message
      on E : EScriptError do
        raise EScriptError.Create(E.Message + ', line ' + IntToStr(Line) + ' (' + IncludeScriptList[ScriptID] + ')');
    else
      raise EScriptError.Create('Unknown error, line ' + IntToStr(Line) + ' (' + IncludeScriptList[ScriptID] + ')');
    end;
  finally
    ParamLine.Free;
  end;
end;

procedure TScriptCmp.IncludeFile(Filename : string);
var
  ScriptText : TStringList;
  Len,
  I          : Integer;
  S,
  Name       : string;
  Encryptor  : TEncryptor;
  F          : File;
  Buf        : Pointer;
begin
  // include more code in this script

  Name := UpperCase(FetchScript(Filename, TRUE));

  // see if its already included
  if (IncludeScriptList.Count > 0) then
    for I := 0 to IncludeScriptList.Count - 1 do
      if (IncludeScriptList[I] = Name) then
        Exit; // already included

  ScriptText := TStringList.Create;

  try
    if (Copy(Name, Length(Name) - 3, 4) = '.INC') then
    try
      Encryptor := TEncryptor.Create(Self);

      try
        with (Encryptor) do
        begin
          ChunkKey := 210;
          ChunkSize := 25;
          Key := '195,23,85,11,77';
          Shift := 14;
          ShiftKey := 78;
        end;

        AssignFile(F, Name);
        Reset(F, 1);
        Len := FileSize(F);
        Buf := AllocMem(Len);

        try
          BlockRead(F, Buf^, Len);
          SetString(S, PChar(Buf), Len);
        finally
          FreeMem(Buf);
        end;

        CloseFile(F);
        Encryptor.Decrypt(S);
        ScriptText.Text := S;
      finally
        Encryptor.Free;
      end;
    except
      raise EScriptError.Create('Unable to include pack2 subroutine ''' + Filename + '''');
    end
    else
    try
      ScriptText.LoadFromFile(Name);
    except
      raise EScriptError.Create('Unable to process include file ''' + Filename + '''');
    end;

    CompileFromStrings(ScriptText, StripFileExtension(ShortFilename(Filename)));
  finally
    ScriptText.Free;
  end;
end;

procedure TScriptCmp.CompileFromFile(const Filename, DescFile : string);
var
  ScriptText : TStringList;
begin
  // compile this file into byte code

  FScriptFile := Filename;
  ScriptText := TStringList.Create;

  try
    if (DescFile <> '') then
      FDescription.LoadFromFile(DescFile);
    ScriptText.LoadFromFile(Filename);
    IFLabelCount := 0;
    WaitOnCount := 0;
    CompileFromStrings(ScriptText, ShortFilename(Filename));
  finally
    ScriptText.Free;
  end;
end;

procedure TScriptCmp.WriteToFile(const Filename : string);
var
  F         : File;
  Hdr       : TScriptFileHeader;
  Param     : TCmdParam;
  Pos,
  Len,
  I         : Integer;
  ParamType : Byte;
  Val       : PChar;
begin
  // write script to a file to be loaded from later

  AssignFile(F, Filename);
  ReWrite(F, 1);

  Hdr.ProgramName := 'TWX PRO SCRIPT';
  Hdr.Version := COMPILED_SCRIPT_VERSION;
  Hdr.CodeSize := FCodeSize;
  Hdr.DescSize := Length(FDescription.Text);

  BlockWrite(F, Hdr, SizeOf(Hdr));

  if (Hdr.DescSize > 0) then
  begin
    // write description to file
    Val := PChar(FDescription.Text);
    BlockWrite(F, Val^, Hdr.DescSize);
  end;

  // write code
  BlockWrite(F, FCode^, FCodeSize);

  // write cmdParams
  if (FParamList.Count > 0) then
    for I := 0 to FParamList.Count - 1 do
    begin
      Param := TCmdParam(FParamList[I]);
      Val := PChar(ApplyEncryption(Param.Value, 113));
      Len := Length(Param.Value);

      if (Param is TVarParam) then
      begin
        ParamType := 2; // TVarParam is parameter type 2
        BlockWrite(F, ParamType, 1);
        BlockWrite(F, Len, 4);
        BlockWrite(F, Val^, Len);
        Val := PChar(ApplyEncryption(TVarParam(Param).Name, 113));
        Len := Length(TVarParam(Param).Name);
        BlockWrite(F, Len, 4);
        BlockWrite(F, Val^, Len);
      end
      else // TCmdParam
      begin
        ParamType := 1; // TCmdParam is parameter type 1
        BlockWrite(F, ParamType, 1);
        BlockWrite(F, Len, 4);
        BlockWrite(F, Val^, Len);
      end;
    end;

  // terminate cmdParams with null value
  ParamType := 0;
  BlockWrite(F, ParamType, 1);

  // write include script names
  if (IncludeScriptList.Count > 0) then
    for I := 0 to IncludeScriptList.Count - 1 do
    begin
      Val := PChar(IncludeScriptList[I]);
      Len := Length(IncludeScriptList[I]);
      BlockWrite(F, Len, 4);
      BlockWrite(F, Val^, Len);
    end;

  // terminal include script names with null length
  Len := 0;
  BlockWrite(F, Len, 4);

  // write labels
  if (FLabelList.Count > 0) then
    for I := 0 to FLabelList.Count - 1 do
    begin
      Pos := TScriptLabel(FLabelList[I]).Location;
      BlockWrite(F, Pos, 4);
      Val := PChar(TScriptLabel(FLabelList[I]).Name);
      Len := Length(TScriptLabel(FLabelList[I]).Name);
      BlockWrite(F, Len, 4);
      BlockWrite(F, Val^, Len);
    end;

  CloseFile(F);
end;

procedure TScriptCmp.AddParam(Param : TCmdParam);
begin
  FParamList.Add(Param);
end;

procedure TScriptCmp.LoadFromFile(const Filename : string);
var
  F         : File;
  Hdr       : TScriptFileHeader;
  Location,
  Len       : Integer;
  Param     : TCmdParam;
  ParamType : Byte;
  Val       : PChar;
  ValStr    : string;
  Lab       : TScriptLabel;
begin
  // load script data from file
  FScriptFile := Filename;

  if (FCodeSize > 0) then
    raise EScriptError.Create('Code already exists - cannot load from file');

  AssignFile(F, Filename);
  Reset(F, 1);

  // read header
  BlockRead(F, Hdr, SizeOf(Hdr));

  // check header
  if (Hdr.ProgramName <> 'TWX PRO SCRIPT') then
    raise EScriptError.Create('File is not a compiled TWX script')
  else if (Hdr.Version <> COMPILED_SCRIPT_VERSION) then
    raise EScriptError.Create('Script file is an incorrect version');

  // skip past description
  if (Hdr.DescSize > 0) then
    Seek(F, FilePos(F) + Hdr.DescSize);

  // read code
  FCode := AllocMem(Hdr.CodeSize);
  BlockRead(F, FCode^, Hdr.CodeSize);
  FCodeSize := Hdr.CodeSize;

  // read cmdParams
  BlockRead(F, ParamType, 1);

  while (ParamType > 0) do
  begin
    if (ParamType = 1) then
    begin
      // TCmdParam
      BlockRead(F, Len, 4);
      Val := AllocMem(Len + 1);
      BlockRead(F, Val^, Len);
      SetString(ValStr, Val, Len);
      FreeMem(Val);

      Param := TCmdParam.Create;
      Param.Value := ApplyEncryption(ValStr, 113);
      FParamList.Add(Param);
    end
    else
    begin
      // TVarParam (2)
      BlockRead(F, Len, 4);
      Val := AllocMem(Len + 1);
      BlockRead(F, Val^, Len);
      SetString(ValStr, Val, Len);
      FreeMem(Val);

      Param := TVarParam.Create;
      Param.Value := ApplyEncryption(ValStr, 113);

      BlockRead(F, Len, 4);
      Val := AllocMem(Len + 1);
      BlockRead(F, Val^, Len);
      SetString(ValStr, Val, Len);
      FreeMem(Val);

      TVarParam(Param).Name := ApplyEncryption(ValStr, 113);
      FParamList.Add(Param);
    end;

    BlockRead(F, ParamType, 1);
  end;

  // read include script names
  BlockRead(F, Len, 4);

  while (Len > 0) do
  begin
    Val := AllocMem(Len + 1);
    BlockRead(F, Val^, Len);
    SetString(ValStr, Val, Len);
    FreeMem(Val);
    IncludeScriptList.Add(ValStr);
    BlockRead(F, Len, 4);
  end;

  // read labels
  while not (Eof(F)) do
  begin
    BlockRead(F, Location, 4);
    BlockRead(F, Len, 4);
    Val := AllocMem(Len + 1);
    BlockRead(F, Val^, Len);
    SetString(ValStr, Val, Len);
    FreeMem(Val);

    Lab := TScriptLabel.Create;
    Lab.Name := ValStr;
    Lab.Location := Location;
    FLabelList.Add(Lab);
  end;

  CloseFile(F);
end;

end.
