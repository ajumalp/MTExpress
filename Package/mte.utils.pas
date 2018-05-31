Unit mte.utils;

{$mode objfpc}{$H+}

Interface

Uses
  base64,
  Classes,
  DB,
  Dialogs,
  lclintf,
  sqldb,
  SysUtils,
  CheckLst,
  variants;

Type

  { TMTEUtils }

  TMTEUtils = Class
  Public
    Class Function StreamToBase64(aInputStream: TStream; aReset: Boolean = True): String;
    Class Function Base64ToStream(Const aBase64: String; aOutStream: TStream; Const aStrict: Boolean = False): Boolean; Overload;
    Class Function Base64ToStream(Const aBase64: TStrings; aOutStream: TStream; Const aStrict: Boolean = False): Boolean; Overload;
    Class Function Base64ToFile(Const aBase64, aFile: String): Boolean; Overload;
    Class Function Base64ToFile(Const aBase64: TStrings; Const aFile: String): Boolean; Overload;
    Class Function FileToBase64(Const aFile: String): String;

    Class Function StringToSet(Const aSrc: String): TSysCharSet;
    Class Function URIParamsEncode(Const aSrc: String): String;
    Class Function CharRange(Const aMin, aMax: Char): String;
    Class Function CharRangeAsSet(Const aMin, aMax: Char): TSysCharSet;
  End;

Function StrStartsWith(aContent, aStart: String; aCaseSensitive: Boolean = True): Boolean;
Function StrEndsWith(AContent, AEnd: String): Boolean;
Function StrLastWord(AContent: String; ASeperator: Char = ' '): String;
Function StrWordByIndex(AContent: String; AIndex: Integer; ASeperator: Char = ' '): String;
Function StrSubString(AContent: String; AStart, AEnd: Integer): String; Overload;
Function StrSubString(AContent: String; AStart: Integer): String; Overload;
Function StrSubString(AReverseCnt: Integer; AContent: String): String; Overload;
Function RemoveWhiteSpace(Const AValue: String): String;

Procedure ShowInfoMsg(Const aMessage: String);
Procedure ShowWarnMsg(Const aMessage: String);
Procedure ShowErrMsg(Const aMessage: String);

Function StringPad(Const aString: String; aLen: Integer; Const aChar: Char; aLeft: Boolean = True): String;
Function StrLPad(Const aString: String; aLen: Integer; Const aChar: Char = ' '): String;
Function StrRPad(Const aString: String; aLen: Integer; Const aChar: Char = ' '): String;

Function IfThen(Const aCondition: Boolean; Const aTrueValue, aFalseValue: Variant): Variant;

Procedure AssignParamFromVariant(Const aParam: TParam; Const aVarinat: Variant);
Procedure AssignParamFromField(Const aParam: TParam; Const aField: TField);

Function FuncEncDecrypt(Const aPassword: String; Const aDecrypt: Boolean = False): String;
Function FuncEncrypt(aPassword: String): String;
Function FuncEncryptPassword(aPassword: String): String;
Function FileToBase64(Const aFileName: String): String;

Procedure EFreeAndNil(Var aObj);
Procedure SendMail(Const aMailAddress, aSubject, aBody: String);
Function GetCheckedItems(Const aCheckListBox: TCheckListBox): String;
Procedure SetCheckedItems(Const aCheckListBox: TCheckListBox; aData: String);

Implementation

Function StrStartsWith(aContent, aStart: String; aCaseSensitive: Boolean): Boolean;
Var
  sStartStr: String;
Begin
  sStartStr := Copy(aContent, 0, Length(aStart));
  // returns true if sContent starts with sStart
  If aCaseSensitive Then
    Result := sStartStr = aStart
  Else
    Result := SameText(sStartStr, aStart);
End;

Function StrEndsWith(AContent, AEnd: String): Boolean;
Var
  iLen: Integer;
Begin
  // returns true if sContent ends with sEnd
  iLen := Length(AContent);
  Result := (Copy(AContent, iLen - (Length(AEnd) - 1), iLen) = AEnd);
End;

Function StrLastWord(AContent: String; ASeperator: Char = ' '): String;
Var
  varList: TStrings;
  iCnt: Integer;
Begin
  // return the last word in the string
  varList := TStringList.Create;
  Try
    iCnt := ExtractStrings([ASeperator], [' '], PChar(AContent), varList);
    Result := varList.Strings[Pred(iCnt)];
  Finally
    varList.Free;
  End;
End;

Function StrWordByIndex(AContent: String; AIndex: Integer; ASeperator: Char): String;
Var
  varList: TStringList;
Begin
  // return the nth word in the string
  varList := TStringList.Create;
  Try
    ExtractStrings([aSeperator], [' '], PChar(aContent), varList);
    If aIndex = -1 Then // If aIndex = -1 // will return all words as TStringList Text
      Result := varList.Text
    Else
      Result := varList.Strings[Pred(aIndex)];
  Finally
    varList.Free;
  End;
End;

Function StrSubString(AContent: String; AStart, AEnd: Integer): String;
Begin
  // returns the substring from iStart to iEnd
  Result := Copy(aContent, aStart, aEnd - (aStart - 1));
End;

Function StrSubString(AContent: String; AStart: Integer): String;
Begin
  // returns the substring starting from iStart char index
  Result := StrSubString(aContent, aStart, Length(aContent));
End;

Function StrSubString(AReverseCnt: Integer; AContent: String): String;
Begin
  // returns the substring till the StrLen - iRevCnt
  Result := StrSubString(aContent, 1, Length(aContent) - aReverseCnt);
End;

Function RemoveWhiteSpace(Const AValue: String): String;
Begin
  Result := StringReplace(AValue, ' ', '', [rfReplaceAll]);
End;

Function StringPad(Const aString: String; aLen: Integer; Const aChar: Char; aLeft: Boolean = True): String;
Var
  varPadString: String;
  iPadLen: Integer;
Begin
  Result := aString;

  iPadLen := aLen - Length(aString);

  If iPadLen > 0 Then
  Begin
    varPadString := StringOfChar(aChar, iPadLen);
    If aLeft Then
      Result := varPadString + aString
    Else
      Result := aString + varPadString;
  End;
End;

Function StrLPad(Const aString: String; aLen: Integer; Const aChar: Char): String;
Begin
  Result := StringPad(aString, aLen, aChar, True);
End;

Function StrRPad(Const aString: String; aLen: Integer; Const aChar: Char): String;
Begin
  Result := StringPad(aString, aLen, aChar, False);
End;

Procedure ShowInfoMsg(Const aMessage: String);
Begin
  MessageDlg(aMessage, mtInformation, [mbOK], 0);
End;

Procedure ShowWarnMsg(Const aMessage: String);
Begin
  MessageDlg(aMessage, mtWarning, [mbOK], 0);
End;

Procedure ShowErrMsg(Const aMessage: String);
Begin
  MessageDlg(aMessage, mtError, [mbOK], 0);
End;

Function IfThen(Const aCondition: Boolean; Const aTrueValue, aFalseValue: Variant): Variant;
Begin
  If ACondition Then
    Result := ATrueValue
  Else
    Result := AFalseValue;
End;

Procedure AssignParamFromField(Const aParam: TParam; Const aField: TField);
Var
  varStrm: TStream;
Begin
  If aField.IsNull Then
  Begin
    aParam.Clear;
    Exit;
  End;

  Case aField.DataType Of
    ftString: aParam.AsString := aField.AsString;
    ftSmallint, ftInteger, ftWord: aParam.AsSmallInt := aField.AsInteger;
    ftBoolean: aParam.AsBoolean := aField.AsBoolean;
    ftFloat: aParam.AsFloat := aField.AsFloat;
    ftCurrency: aParam.AsCurrency := aField.AsCurrency;
    ftBCD: aParam.AsBCD := aField.AsCurrency;
    ftDate: aParam.AsDate := aField.AsDateTime;
    ftTime: aParam.AsTime := aField.AsDateTime;
    ftDateTime: aParam.AsDateTime := aField.AsDateTime;
    ftBytes: aParam.AsBytes := aField.AsBytes;
    ftMemo, ftWideString, ftWideMemo: aParam.AsMemo := aField.AsString;
    ftLargeint: aParam.AsLargeInt := aField.AsLargeInt;
    ftBlob:
    Begin
      varStrm := TMemoryStream.Create;
      Try
        TBlobField(aField).SaveToStream(varStrm);
        aParam.LoadFromStream(varStrm, aField.DataType);
      Finally
        varStrm.Free;
      End;
    End
    Else aParam.Value := aField.AsVariant;
  End;
End;

Procedure AssignParamFromVariant(Const aParam: TParam; Const aVarinat: Variant);
Var
  varVariantType: TVarType;
Begin
  varVariantType := VarType(aVarinat);
  Case varVariantType Of
    varSmallint: aParam.AsSmallInt := VarAsType(aVarinat, varVariantType);
    varInteger, varSingle, varShortInt: aParam.AsInteger := VarAsType(aVarinat, varVariantType);
    varDouble: aParam.AsFloat := VarAsType(aVarinat, varVariantType);
    varCurrency: aParam.AsCurrency := VarAsType(aVarinat, varVariantType);
    varDate: aParam.AsDateTime := VarToDateTime(aVarinat);
    varBoolean: aParam.AsBoolean := VarAsType(aVarinat, varVariantType);
    varWord: aParam.AsWord := VarAsType(aVarinat, varVariantType);
    varInt64: aParam.AsLargeInt := VarAsType(aVarinat, varVariantType);
    varString: aParam.AsString := VarToStr(aVarinat);
    varUString: aParam.AsMemo := VarToStr(aVarinat);
    Else aParam.Value := aVarinat;
  End;
End;

Function FuncEncDecrypt(Const aPassword: String; Const aDecrypt: Boolean): String;
Begin
  If aDecrypt Then
  Begin
    Result := DecodeStringBase64(aPassword);
    Result := StrSubString(Result, 6);
    Result := StrSubString(3, Result);
    Result := DecodeStringBase64(Result);
  End
  Else
  Begin
    Result := EncodeStringBase64(aPassword);
    Result := 'tfose' + Result + 'yrt';
    Result := EncodeStringBase64(Result);
  End;
End;

Function FuncEncrypt(aPassword: String): String;
Begin
  aPassword := EncodeStringBase64(aPassword);
  aPassword := 'tfose' + aPassword + 'yrt';
  aPassword := EncodeStringBase64(aPassword);
  aPassword := StrSubString(aPassword, 3, aPassword.Length - 2);
  Result := aPassword.ToUpper;
End;

Function FuncEncryptPassword(aPassword: String): String;
Begin
  aPassword := EncodeStringBase64(aPassword);
  While StrEndsWith(aPassword, '=') Do
    aPassword := StrSubString(aPassword, 1, Length(aPassword) - 1);
  aPassword := EncodeStringBase64(aPassword);
  While StrEndsWith(aPassword, '=') Do
    aPassword := StrSubString(aPassword, 1, Length(aPassword) - 1);
  Result := UpperCase(aPassword);
End;

Function FileToBase64(Const aFileName: String): String;
Begin
  // base64.
End;

Procedure EFreeAndNil(Var aObj);
Begin
  If Assigned(TObject(aObj)) Then
    FreeAndNil(aObj);
End;

Procedure SendMail(Const aMailAddress, aSubject, aBody: String);
Const
  cMailToFormat = 'mailto:%s?subject=%s&body=%s';
Var
  sURL, sSubject, sBody: String;
Begin
  sSubject := TMTEUtils.URIParamsEncode(aSubject);
  sBody := TMTEUtils.URIParamsEncode(aBody);
  sURL := Format(cMailToFormat, [aMailAddress, sSubject, sBody]);
  OpenURL(sURL);
End;

Function GetCheckedItems(Const aCheckListBox: TCheckListBox): String;
Var
  iCntr: Integer;
  varList: TStringList;
Begin
  varList := TStringList.Create;
  Try
    For iCntr := 0 To Pred(aCheckListBox.Items.Count) Do
    Begin
      If aCheckListBox.Checked[iCntr] Then
        varList.Add(aCheckListBox.Items[iCntr]);
    End;
    Result := varList.Text;
  Finally
    varList.Free;
  End;
End;

Procedure SetCheckedItems(Const aCheckListBox: TCheckListBox; aData: String);
Var
  iCntr: Integer;
  iIndex: Integer;
  varList: TStringList;
Begin
  varList := TStringList.Create;
  Try
    varList.Text := aData;
    For iCntr := 0 To Pred(aCheckListBox.Items.Count) Do
      aCheckListBox.Checked[iCntr] := varList.IndexOf(aCheckListBox.Items[iCntr]) >= 0;
  Finally
    varList.Free;
  End;
End;

{ TMTEUtils }

Class Function TMTEUtils.StringToSet(Const aSrc: String): TSysCharSet;
Var
  iCntr: Integer;
Begin
  Result := [];

  For iCntr := 1 To Length(aSrc) Do
    Result := Result + [aSrc[iCntr]];
End;

Class Function TMTEUtils.URIParamsEncode(Const aSrc: String): String;
Var
  i: Integer;
Const
  cUnsafeChars = '*#%<>[]';  {do not localize}
Begin
  Result := '';    {Do not Localize}
  For i := 1 To Length(aSrc) Do
  Begin
    // S.G. 27/11/2002: Changed the parameter encoding: Even in parameters, a space
    // S.G. 27/11/2002: is much more likely to be meaning "space" than "this is
    // S.G. 27/11/2002: a new parameter"
    // S.G. 27/11/2002: ref: Message-ID: <3de30169@newsgroups.borland.com> borland.public.delphi.internet.winsock
    // S.G. 27/11/2002: Most low-ascii is actually Ok in parameters encoding.
    If CharInSet(aSrc[i], StringToSet(cUnsafeChars)) Or (Not CharInSet(aSrc[i], CharRangeAsSet(#33, #128))) Then
      Result := Result + '%' + IntToHex(Ord(aSrc[i]), 2)
    Else
      Result := Result + aSrc[i];
  End;
End;

Class Function TMTEUtils.CharRange(Const aMin, aMax: Char): String;
Var
  iCntr: Char;
Begin
  Result := '';

  For iCntr := aMin To aMax Do
    Result := Result + iCntr;
End;

Class Function TMTEUtils.CharRangeAsSet(Const aMin, aMax: Char): TSysCharSet;
Var
  iCntr: Char;
Begin
  Result := [];

  For iCntr := aMin To aMax Do
    Result := Result + [iCntr];
End;

Class Function TMTEUtils.StreamToBase64(aInputStream: TStream; aReset: Boolean): String;
Var
  varOutputStrm: TStringStream;
  varEncoder: TBase64EncodingStream;
Begin
  Result := EmptyStr;

  varOutputStrm := TStringStream.Create(EmptyStr);
  varEncoder := TBase64EncodingStream.Create(varOutputStrm);
  Try
    If aReset Then
      aInputStream.Seek(0, 0);
    varEncoder.CopyFrom(aInputStream, aInputStream.Size);
    varEncoder.Flush;

    Result := varOutputStrm.DataString;
  Finally
    varEncoder.Free;
    varOutputStrm.Free;
  End;
End;

Class Function TMTEUtils.Base64ToStream(Const aBase64: String; aOutStream: TStream; Const aStrict: Boolean): Boolean;
Var
  varInStrm: TStringStream;
  varDecoder: TBase64DecodingStream;
Begin
  Result := False;
  varInStrm := TStringStream.Create(aBase64);
  Try
    If aStrict Then
      varDecoder := TBase64DecodingStream.Create(varInStrm, bdmStrict)
    Else
      varDecoder := TBase64DecodingStream.Create(varInStrm, bdmMIME);
    Try
      aOutStream.CopyFrom(varDecoder, varDecoder.Size);
      Result := True;
    Finally
      varDecoder.Free;
    End;
  Finally
    varInStrm.Free;
  End;
End;

Class Function TMTEUtils.Base64ToStream(Const aBase64: TStrings; aOutStream: TStream; Const aStrict: Boolean): Boolean;
Begin
  Result := TMTEUtils.Base64ToStream(aBase64.Text, aOutStream, aStrict);
End;

Class Function TMTEUtils.Base64ToFile(Const aBase64, aFile: String): Boolean;
Var
  varOutStrm: TFileStream;
Begin
  Result := False;

  varOutStrm := TFileStream.Create(aFile, fmCreate Or fmShareExclusive);
  Try
    Base64ToStream(aBase64, varOutStrm);
    Result := True;
  Finally
    varOutstrm.Free;
  End;
End;

Class Function TMTEUtils.Base64ToFile(Const aBase64: TStrings; Const aFile: String): Boolean;
Begin
  Result := TMTEUtils.Base64ToFile(aBase64.Text, aFile);
End;

Class Function TMTEUtils.FileToBase64(Const aFile: String): String;
Var
  varInputStrm: TFileStream;
Begin
  If Not FileExists(aFile) Then
    Exit(EmptyStr);

  varInputStrm := TFileStream.Create(aFile, fmOpenRead Or fmShareDenyWrite);
  Try
    Result := StreamToBase64(varInputStrm);
  Finally
    varInputStrm.Free;
  End;
End;

End.
