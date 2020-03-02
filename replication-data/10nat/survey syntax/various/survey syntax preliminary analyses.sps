
* IMPORTANT!!!! MANUALLY SET APPROPRIATE MISSING VALUES BEFORE PROCEDING!!!!***

FREQUENCIES VARIABLES=Pol1 Pol2 Pol3 Ethn1 Ethn2 Ethn3 Rel1 Rel2 Rel3 Peg1 Peg2 Eff1 Eff2 Eff3 Eff4 
    Eff5 Eff6 Trust1 Trust2 Trust3 Followpol Int1 Int2 Int3 Exp1 Exp2 Exp3 Exp4 Exp5 Exp6 Exp7 Exp8 
    Talk Homeinternet Str1
  /HISTOGRAM NORMAL
  /FORMAT=LIMIT(101)
  /ORDER=ANALYSIS.

SORT CASES  BY country.
SPLIT FILE LAYERED BY country.



FREQUENCIES VARIABLES=Pol1 Pol2 Pol3 Ethn1 Ethn2 Ethn3 Rel1 Rel2 Rel3 Peg1 Peg2 Eff1 Eff2 Eff3 Eff4 
    Eff5 Eff6 Trust1 Trust2 Trust3 Followpol Int1 Int2 Int3 Exp1 Exp2 Exp3 Exp4 Exp5 Exp6 Exp7 Exp8 
    Talk Homeinternet Str1
  /STATISTICS=STDDEV MINIMUM MAXIMUM MEAN
  /FORMAT=LIMIT(101)
  /ORDER=ANALYSIS.

split file off.


RECODE Eff3 Eff6 (1=4) (2=3) (3=2) (4=1) INTO Eff3r Eff6r.
EXECUTE.


RELIABILITY
  /VARIABLES=Eff3r Eff6r Eff1 Eff2 Eff4 Eff5
  /SCALE('Efficacy') ALL
  /MODEL=ALPHA
  /SUMMARY=TOTAL.

RELIABILITY
  /VARIABLES=Exp1 Exp6 Exp7 Exp8 Talk
  /SCALE('Exposure') ALL
  /MODEL=ALPHA
  /SUMMARY=TOTAL.

RELIABILITY
  /VARIABLES=Trust1 Trust2 Trust3
  /SCALE('Trust') ALL
  /MODEL=ALPHA
  /SUMMARY=TOTAL.

RELIABILITY
  /VARIABLES=Int1 Int2 Int3
  /SCALE('Interest') ALL
  /MODEL=ALPHA
  /SUMMARY=TOTAL.


compute Efficacy = mean ( Eff3r, Eff6r, Eff1, Eff2, Eff4, Eff5). 
EXECUTE .
compute Trust = mean (Trust1 to Trust3).
EXECUTE .
compute Interest = mean (Int1 to Int3).
EXECUTE .


FREQUENCIES VARIABLES=H1_I1 H2_I2 H3_I3 S1_I6 H5_I5 H6_I7 H7_D1 I3_1 I3_2 CC
  /FORMAT=NOTABLE
  /STATISTICS=STDDEV SEMEAN MEAN SKEWNESS SESKEW KURTOSIS SEKURT
  /HISTOGRAM NORMAL
  /ORDER=ANALYSIS.

RECODE H1_I1 H2_I2 H3_I3 S1_I6 H5_I5 H6_I7 H7_D1 I3_1 I3_2 CC (MISSING=SYSMIS) (1=1) (ELSE=0) INTO 
    Copenhagen Merkel Putin Tiger Taliban Expo Unemployment UNsecgen Redshirts CO2.
EXECUTE.
 
if (country = 'AS') weight = weight_AS.
if (country = 'CA') weight = weight_CA.
if (country = 'IN') weight =weight_IN.
if (country = 'IT') weight = weight_IT.
if (country = 'JP') weight = weight_JP.
if (country = 'NO') weight = weight_NO.
if (country = 'UK') weight = weight_UK.
if (country = 'US') weight = weight_US.

WEIGHT BY weight.


UNIANOVA Copenhagen BY country
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(country) COMPARE ADJ(BONFERRONI)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=country.


UNIANOVA Merkel BY country
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(country) COMPARE ADJ(BONFERRONI)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=country.

UNIANOVA Putin BY country
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(country) COMPARE ADJ(BONFERRONI)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=country.

UNIANOVA Tiger BY country
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(country) COMPARE ADJ(BONFERRONI)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=country.

UNIANOVA Taliban BY country
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(country) COMPARE ADJ(BONFERRONI)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=country.

UNIANOVA Expo BY country
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(country) COMPARE ADJ(BONFERRONI)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=country.

UNIANOVA Unemployment BY country
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(country) COMPARE ADJ(BONFERRONI)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=country.


UNIANOVA UNsecgen BY country
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(country) COMPARE ADJ(BONFERRONI)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=country.


UNIANOVA Redshirts BY country
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(country) COMPARE ADJ(BONFERRONI)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=country.


UNIANOVA CO2 BY country
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(country) COMPARE ADJ(BONFERRONI)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=country.


RECODE H3_I3_2 H3_I3_3 HI_I1 (MISSING=SYSMIS) (1=1) (ELSE=0) INTO CARedshirts CAUNSec  ASCopenh.

IF (country = 'CA') Redshirts = CARedshirts.
IF (country = 'CA') UNsecgen = CAUNSec.
IF (country = 'AS') Copenhagen = ASCopenh.

UNIANOVA Copenhagen BY country
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(country) COMPARE ADJ(BONFERRONI)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=country.

UNIANOVA UNsecgen BY country
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(country) COMPARE ADJ(BONFERRONI)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=country.


UNIANOVA Redshirts BY country
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(country) COMPARE ADJ(BONFERRONI)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=country.


COMPUTE commonhardprov=mean(Copenhagen,Merkel,Putin,Taliban,Expo,Redshirts).
VARIABLE LABELS  commonhardprov 'provisionalcommonhard'.
EXECUTE.

UNIANOVA commonhardprov BY country
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(country) COMPARE ADJ(BONFERRONI)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=country.



FREQUENCIES VARIABLES=Efficacy Trust Interest
  /FORMAT=NOTABLE
  /HISTOGRAM NORMAL
  /ORDER=ANALYSIS.



UNIANOVA Efficacy BY country
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(country) COMPARE ADJ(BONFERRONI)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=country.

UNIANOVA Trust BY country
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(country) COMPARE ADJ(BONFERRONI)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=country.

UNIANOVA Interest BY country
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(country) COMPARE ADJ(BONFERRONI)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=country.


MISSING VALUES H7_D1 know9_US know10_US H8_D2_UK H9_I8_UK H9_D3_Can Can1_Can Q10_H8_D2_Japan H9_I8_Aus
AHD1_1_Aus Q11_H9_I8_Japan H8_D2_Italy H9_D3_Italy Q9_Ind Q10_Ind Q11_Ind H8_D2_Nor H9_D2_Nor (5 THRU HIGHEST).

RECODE  H7_D1 know9_US know10_US H8_D2_UK H9_I8_UK H9_D3_Can Can1_Can Q10_H8_D2_Japan H9_I8_Aus
AHD1_1_Aus Q11_H9_I8_Japan H8_D2_Italy H9_D3_Italy Q9_Ind Q10_Ind Q11_Ind H8_D2_Nor H9_D2_Nor (MISSING=SYSMIS) (1=1) (ELSE=0) INTO 
RH7_D1 Rknow9_US Rknow10_US RH8_D2_UK RH9_I8_UK RH9_D3_Can RCan1_Can RQ10_H8_D2_Japan RH9_I8_Aus
RAHD1_1_Aus RQ11_H9_I8_Japan RH8_D2_Italy RH9_D3_Italy RQ9_Ind RQ10_Ind RQ11_Ind RH8_D2_Nor RH9_D2_Nor.


IF  (country = 'US') NationSpecific_Hard=mean (RH7_D1, Rknow9_US,Rknow10_US).
IF (country = 'AS') NationSpecific_Hard=mean (RH7_D1, RH9_I8_Aus, RAHD1_1_Aus).
IF  (country = 'UK') NationSpecific_Hard=mean (RH7_D1, RH8_D2_UK,RH9_I8_UK).
IF  (country = 'CA') NationSpecific_Hard=mean (RH7_D1, RH9_D3_Can,RCan1_Can).
IF  (country = 'JP') NationSpecific_Hard=mean (RQ10_H8_D2_Japan,RQ11_H9_I8_Japan).
IF  (country = 'IT') NationSpecific_Hard=mean (RH7_D1, RH8_D2_Italy,RH9_D3_Italy).
IF  (country = 'IN') NationSpecific_Hard=mean (RQ9_Ind, RQ10_Ind,RQ11_Ind).
IF  (country = 'NO') NationSpecific_Hard=mean (RH7_D1,RH8_D2_Nor, RH9_D2_Nor).
EXECUTE.


MISSING VALUES  S1_I6  know11_US  know12_US  know14_US  S2_D3_UK  S3_D4_UK  S4_D8_UK  S2_D2_Aus S2_D4_Can  S3_D6_Can  Q12_S2_D3_Japan  Q13_S3_D4_Japan 
 Q17_S4_D6_JP S2_D3_Italy  S3_D4_Italy  S4_D8_IT  Q12_Ind  Q13_Ind  Q17_IN S2_D3_Nor  S3_D4_Nor  S4_D8_Nor (5 THRU HIGHEST).

RECODE  S1_I6  know11_US  know12_US  know14_US  S2_D3_UK  S3_D4_UK  S4_D8_UK  S2_D2_Aus S2_D4_Can  S3_D6_Can  Q12_S2_D3_Japan  Q13_S3_D4_Japan 
 Q17_S4_D6_JP S2_D3_Italy  S3_D4_Italy  S4_D8_IT  Q12_Ind  Q13_Ind  Q17_IN S2_D3_Nor  S3_D4_Nor  S4_D8_Nor (MISSING=SYSMIS) (1=1) (ELSE=0) INTO 
RS1_I6  Rknow11_US  Rknow12_US  Rknow14_US  RS2_D3_UK  RS3_D4_UK  RS4_D8_UK  RS2_D2_Aus RS2_D4_Can  RS3_D6_Can  RQ12_S2_D3_Japan  RQ13_S3_D4_Japan 
 RQ17_S4_D6_JP RS2_D3_Italy  RS3_D4_Italy  RS4_D8_IT  RQ12_Ind  RQ13_Ind  RQ17_IN RS2_D3_Nor  RS3_D4_Nor  RS4_D8_Nor.

 *note: only one soft item for australia and three for canada*

IF  (country = 'US') Soft=mean (RS1_I6, Rknow11_US, Rknow12_US, Rknow14_US).
IF  (country = 'UK') Soft=mean (RS1_I6, RS2_D3_UK, RS3_D4_UK, RS4_D8_UK).
IF (country = 'AS') NationSpecific_Hard=mean (RS1_I6, RS2_D2_Aus).    
IF  (country = 'CA') Soft=mean (RS1_I6, RS2_D4_Can, RS3_D6_Can).
IF  (country = 'JP') Soft=mean (RS1_I6, RQ12_S2_D3_Japan, RQ13_S3_D4_Japan, RQ17_S4_D6_JP).
IF  (country = 'IT') Soft=mean (RS1_I6, RS2_D3_Italy, RS3_D4_Italy, RS4_D8_IT).
IF  (country = 'IN') Soft=mean (RS1_I6, RQ12_Ind, RQ13_Ind, RQ17_IN).
IF  (country = 'NO') Soft=mean (RS1_I6, RS2_D3_Nor, RS3_D4_Nor, RS4_D8_Nor).
EXECUTE.

SORT CASES  BY country.
SPLIT FILE LAYERED BY country.



FREQUENCIES VARIABLES=NationSpecific_Hard Soft commonhardprov
  /HISTOGRAM NORMAL
  /ORDER=ANALYSIS.
