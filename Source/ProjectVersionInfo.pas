unit ProjectVersionInfo;

interface

uses
  ShellAPI, ExtCtrls, StdCtrls, Classes, SysUtils, Windows, Messages, Forms;

type
  TVS_VERSION_INFO = record
    wLength: Word;
    wValueLength: Word;
    wType: Word;
    szKey: array [1..16] of WideChar;
    Padding1: Word;
    Value: TVSFixedFileInfo;
    Padding2: Word;
    Children: Word;
  end;

  PVS_VERSION_INFO = ^TVS_VERSION_INFO;

  function GetVersionField(Info : String): String;
  procedure initVersionInfo;
  procedure clearVersionInfo;
  function GetField( Index: Integer ): string;
  function GetVerField( Index: Integer ): string;

implementation

  //uses Main;

  var
    VersionList: TStringList;
    CompanyName, FileDescription, FileVersion, InternalName, LegalCopyright: string;
    LegalTradeMarks, OriginalFilename, ProductName, ProductVersion, Comments: string;
    FileBuild, ProductBuild, FileVersionBuild, ProductVersionBuild,ProgramVersion: string;
    Major, Minor, Release, Build: word;

  const
    num_Info = 15;
    InfoStr : array [1..num_Info] of String = ('CompanyName', 'FileDescription',
      'FileVersion', 'FileBuild', 'InternalName', 'LegalCopyright', 'LegalTradeMarks',
      'OriginalFilename', 'ProductName', 'ProductVersion', 'ProductBuild', 'Comments',
      'FileVersionBuild', 'ProductVersionBuild', 'ProgramVersion');

  (*
    This is the prefered method to retreave the Version Information.  Use
    this method unless you get a access violation runtime error.
    
    To use call the method while passing the name of the Version Information Field
    to the method.
    example:  GetVersionField('FileVersion');
  *)
  function GetVersionField(Info : String): String;
  var
    forLoop : Integer;
  begin
    for forLoop := 1 to num_info do
    begin
      if InfoStr[forLoop] = Info then
      begin
        //frmMain.memoTest.Lines.Add(Info + ': ' + GetVerField(forLoop));
        Result := GetVerField(forLoop);
      end;
    end;
  end;

  (*
    Call this method to initalize the Version Information fields.
    This must be called prior to calling any of the previous methods to
    retrieve a Version Information field.
  *)
  procedure initVersionInfo;

  const
    SNotAvailable = 'Value is Not Available';

  var
    LanguageID: string;
    CodePage: string;
    TranslationLength: Cardinal;
    TranslationTable: Pointer;
    Info  : String;
    BuffSize,
    Len   : DWord;
    Buff  : PChar;
    Value : PChar;
    Value2 : ^VS_FIXEDFILEINFO;
    TranslateString: string;

  begin
    VersionList := TStringList.Create;

    Info := Application.ExeName;
    BuffSize := GetFileVersionInfoSize(PChar(Info), BuffSize);
    if BuffSize > 0 then
    begin
      Buff := AllocMem(BuffSize);

      try
        GetFileVersionInfo(PChar(Info), 0, BuffSize, Buff);

        if VerQueryValue(Buff, '\VarFileInfo\Translation', TranslationTable, TranslationLength) then
        begin
          CodePage := Format('%.4x', [HiWord(PLongInt(TranslationTable)^)]);
          LanguageID := Format('%.4x', [LoWord(PLongInt(TranslationTable)^)]);
        end;

        TranslateString := 'StringFileInfo\' + LanguageID + CodePage + '\';

        (*
          Get the actuall file version.  And split it into it's main parts.
          Then group the 3 main parts together for the fileversion.  Leaving
          the last part for the option build number.
        *)
        if VerQueryValue(Buff, '\', pointer(Value2), len) then
        begin
          Major   := HiWord(Value2.dwFileVersionMS);
          Minor   := LoWord(Value2.dwFileVersionMS);
          Release := HiWord(Value2.dwFileVersionLS);
          Build   := LoWord(Value2.dwFileVersionLS);
          FileVersion := IntToStr(Major) + '.' +
                        IntToStr(Minor) + '.' +
                        IntToStr(Release);
          FileBuild := IntToStr(Build);
          ProgramVersion := IntToStr(Major) + '.' +
                        IntToStr(Minor) + IntToStr(Release);
          Major   := HiWord(Value2.dwProductVersionMS);
          Minor   := LoWord(Value2.dwProductVersionMS);
          Release := HiWord(Value2.dwProductVersionLS);
          Build   := LoWord(Value2.dwProductVersionLS);
          ProductVersion := IntToStr(Major) + '.' +
                        IntToStr(Minor) + '.' +
                        IntToStr(Release);
          ProductBuild := IntToStr(Build);
        end;

        (*
          Now get the remaining FileVersion information.
        *)
        if VerQueryValue(Buff,PChar(TranslateString + 'CompanyName'), Pointer(Value), Len) then
          CompanyName := Value;
        if VerQueryValue(Buff,PChar(TranslateString + 'FileDescription'), Pointer(Value), Len) then
          FileDescription := Value;
        if VerQueryValue(Buff,PChar(TranslateString + 'FileVersion'), Pointer(Value), Len) then
          FileVersionBuild := Value;
        if VerQueryValue(Buff,PChar(TranslateString + 'InternalName'), Pointer(Value), Len) then
          InternalName := Value;
        if VerQueryValue(Buff,PChar(TranslateString + 'LegalCopyright'), Pointer(Value), Len) then
          LegalCopyright := Value;
        if VerQueryValue(Buff,PChar(TranslateString + 'LegalTradeMarks'), Pointer(Value), Len) then
          LegalTradeMarks := Value;
        if VerQueryValue(Buff,PChar(TranslateString + 'OriginalFilename'), Pointer(Value), Len) then
          OriginalFilename := Value;
        if VerQueryValue(Buff,PChar(TranslateString + 'ProductName'), Pointer(Value), Len) then
          ProductName := Value;
        if VerQueryValue(Buff,PChar(TranslateString + 'ProductVersion'), Pointer(Value), Len) then
          ProductVersionBuild := Value;
        if VerQueryValue(Buff,PChar(TranslateString + 'Comments'), Pointer(Value), Len) then
          Comments := Value;
      finally
        Freemem(Buff, BuffSize);
      end;
    end
    else
    begin
      CompanyName := SNotAvailable;
      FileDescription := SNotAvailable;
      FileVersion := SNotAvailable;
      FileBuild := SNotAvailable;
      ProgramVersion := SNotAvailable;
      InternalName := SNotAvailable;
      LegalCopyright := SNotAvailable;
      LegalTrademarks := SNotAvailable;
      OriginalFilename := SNotAvailable;
      ProductName := SNotAvailable;
      ProductVersion := SNotAvailable;
      Comments := SNotAvailable;
    end;

    VersionList.Clear;
    VersionList.Add('');
    VersionList.Add(CompanyName);
    VersionList.Add(FileDescription);
    VersionList.Add(FileVersion);
    VersionList.Add(FileBuild);
    VersionList.Add(InternalName);
    VersionList.Add(LegalCopyright);
    VersionList.Add(LegalTrademarks);
    VersionList.Add(OriginalFilename);
    VersionList.Add(ProductName);
    VersionList.Add(ProductVersion);
    VersionList.Add(ProductBuild);
    VersionList.Add(Comments);
    VersionList.Add(FileVersionBuild);
    VersionList.Add(ProductVersionBuild);
    VersionList.Add(ProgramVersion);
  end;

  (*
    Call this method to clear/destroy the Version Information fieldlist.
    This must be called during application shutdown.  Else a possible memory leak
    could happen.
  *)
  procedure clearVersionInfo;
  begin
    VersionList.Free;
  end;

  (*
    The following methods are sub-functions for the GetVersionField method.
  *)
  function GetField(Index: Integer): string;
  begin
    Result := VersionList[Index]
  end;

  function GetVerField(Index: Integer): string;
  begin
    Result := GetField(Ord(Index));
  end;

end.
