
Unit mte.core.baseclasses;

{----------------------------------------------------------}
{ Developed by Muhammad Ajmal p }
{ ajumalp@gmail.com  }
{ pajmal@hotmail.com }
{ ajmal@erratums.com }
{----------------------------------------------------------}

{$mode objfpc}{$H+}

Interface

Uses
  Classes,
  SysUtils,
  fpjson,
  jsonparser,
  variants,
  TypInfo,
  sqldb;

Type
  TMTEBase = Class;

  eTJSONReader = (ejrLoadDefault, ejrLoadFromJSONString);
  TEBaseGetter = Function(Const aCode: eTJSONReader): TMTEBase Of Object;

  { TMTEBase }

  TMTEBase = Class(TObject)
  Strict Private
  Const
    cDATA_TYPE = 'DATA_TYPE';
    cPROPERTY_NAME = 'Name';
    cPROPERTY_VALUE = 'Value';
    cPROPERTY_TYPE = 'Type';
    cPROPERTY_INDEX = 'Index';

  Var
    FJSONString: String;

    Procedure WriteToJSON(Var aJSONObject: TJSONObject; Const aName, aValue: String); Overload;
  Protected
    Procedure ReadFromJSON(Const aJSONObject: TJSONObject);
    Procedure WriteToJSON(Var aJSONObject: TJSONObject; Const aPropInfo: PPropInfo); Overload;
    Procedure WriteToJSON(Var aJSONObject: TJSONObject; Const aPropInfo: PPropInfo; aValue: Variant); Overload;
  Public
    Function QueryInterface(constref aID: TGuid; out aObj): Longint; Stdcall;
    Function _AddRef: Longint; Stdcall;
    Function _Release: Longint; Stdcall;

    Function ToJSON: TJSONStringType; Virtual;
    Procedure LoadFromJSON(Const aJSONData: TJSONStringType); Virtual;
  End;

Implementation

{ TMTEBase }

Procedure TMTEBase.WriteToJSON(Var aJSONObject: TJSONObject; Const aName, aValue: String);
Begin
  aJSONObject.Add(aName, aValue);
End;

Procedure TMTEBase.ReadFromJSON(Const aJSONObject: TJSONObject);
Var
  varPropType: TTypeKind;
  varPropName, varPropValue: String;
  varValue: Variant;
  varMethod: TMethod;
Begin
  varPropType := TTypeKind(aJSONObject.Integers[cPROPERTY_TYPE]);
  varPropName := aJSONObject.Strings[cPROPERTY_NAME];
  varPropValue := aJSONObject.Strings[cPROPERTY_VALUE];

  Case varPropType Of
    tkInt64:
    Begin
      VarCast(varValue, varPropValue, varInt64);
      SetPropValue(Self, varPropName, varValue);
    End;
    tkInteger:
    Begin
      VarCast(varValue, varPropValue, varInteger);
      SetPropValue(Self, varPropName, varValue);
    End;
    tkEnumeration: SetEnumProp(Self, varPropName, varPropValue);
    tkFloat:
    Begin
      VarCast(varValue, varPropValue, varDouble);
      SetPropValue(Self, varPropName, varValue);
    End;
    tkBool:
    Begin
      VarCast(varValue, varPropValue, varboolean);
      SetPropValue(Self, varPropName, varValue);
    End;
    tkSet: ;
    tkClass:
    Begin
      FJSONString := varPropValue;
      Try
        varMethod.Code := GetPropInfo(Self, varPropName)^.GetProc;
        varMethod.Data := Pointer(Self);
        TEBaseGetter(varMethod)(ejrLoadFromJSONString);
      Finally
        FJSONString := EmptyStr;
      End;
    End;
    tkMethod: ;
    tkWChar, tkChar, tkUString, tkString, tkLString, tkWString, tkAString: SetPropValue(Self, varPropName, varPropValue);
    tkVariant:
    Begin
      Try
        VarCast(varValue, varPropValue, varVariant);
      Except
        On E: EVariantTypeCastError Do
        Begin
          If varPropValue <> '' Then
          Begin
            Try
              VarCast(varValue, varPropValue, varstring);
            Except
              Raise;
            End;
          End;
        End;
      End;
      SetPropValue(Self, varPropName, varValue);
    End;
  End;
End;

Procedure TMTEBase.WriteToJSON(Var aJSONObject: TJSONObject; Const aPropInfo: PPropInfo);
Var
  varPropValue: Variant;
  varObject: TObject;
Begin
  Case aPropInfo^.PropType^.Kind Of
    tkEnumeration, tkInteger, tkInt64, tkFloat, tkChar, tkString, tkAString,
    tkWChar, tkLString, tkWString, tkVariant, tkUString, tkBool: varPropValue := GetPropValue(Self, aPropInfo^.Name);
    tkClass:
    Begin
      varObject := GetObjectProp(Self, aPropInfo);
      Assert(varObject.InheritsFrom(TMTEBase), 'Unknown Class. Currently implimented for TEBaseClass only.');
      varPropValue := (varObject As TMTEBase).ToJSON;
    End;
    Else Assert(False, 'Unknown Class');
  End;
  WriteToJSON(aJSONObject, aPropInfo, varPropValue);
End;

Procedure TMTEBase.WriteToJSON(Var aJSONObject: TJSONObject; Const aPropInfo: PPropInfo; aValue: Variant);
Begin
  WriteToJSON(aJSONObject, cPROPERTY_NAME, aPropInfo^.Name);
  WriteToJSON(aJSONObject, cPROPERTY_VALUE, VarToStr(aValue));
  WriteToJSON(aJSONObject, cPROPERTY_TYPE, IntToStr(Ord(aPropInfo^.PropType^.Kind)));
  // WriteToJSON(aJSONObject, cPROPERTY_INDEX, IntToStr(aPropInfo.Index));
End;

Function TMTEBase.QueryInterface(constref aID: TGuid; Out aObj): Longint; Stdcall;
Begin
  If GetInterface(aID, aObj) Then
    Result := 0
  Else
    Result := E_NOINTERFACE;
End;

Function TMTEBase._AddRef: Longint; Stdcall;
Begin
  Result := 0;
End;

Function TMTEBase._Release: Longint; Stdcall;
Begin
  Free;
  Result := 0;
End;

Function TMTEBase.ToJSON: TJSONStringType;
Var
  varPropList: PPropList;
  iCnt, iPropCount: Integer;
  varJSONObject: TJSONObject;
  varJSONArray: TJSONArray;
Begin
  iPropCount := GetTypeData(ClassInfo)^.PropCount;
  If iPropCount = 0 Then
    Exit('');

  varPropList := Nil;
  GetMem(varPropList, iPropCount * SizeOf(PPropInfo));
  iPropCount := GetPropList(ClassInfo, tkProperties, varPropList);
  varJSONArray := TJSONArray.Create;
  Try
    For iCnt := 0 To Pred(iPropCount) Do
    Begin
      // We export only published properties { Ajmal }
      If Not IsPublishedProp(Self, varPropList^[iCnt]^.Name) Then
        Continue;

      varJSONObject := TJSONObject.Create;
      WriteToJSON(varJSONObject, varPropList^[iCnt]);
      varJSONArray.Add(varJSONObject);
    End;
    Result := varJSONArray.AsJSON;
  Finally
    varJSONArray.Free;
    FreeMemory(varPropList);
  End;
End;

Procedure TMTEBase.LoadFromJSON(Const aJSONData: TJSONStringType);
Var
  varJSONArray: TJSONArray;
  varJSONObject: TJSONObject;
  iCnt: Integer;
Begin
  varJSONArray := GetJSON(aJSONData) As TJSONArray;
  Try
    For iCnt := 0 To Pred(varJSONArray.Count) Do
    Begin
      varJSONObject := varJSONArray.Items[iCnt] As TJSONObject;
      ReadFromJSON(varJSONObject);
    End;
  Finally
    varJSONArray.Free;
  End;
End;

End.
