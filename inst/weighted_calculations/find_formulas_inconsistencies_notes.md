# Finding discrepancies for calculations that differ when using RRUFF vs IMA formulas

### Some form of discrepancy that means EXCLUDE!!
+ Pitticite
  + rruff: Fe^3+^_x_(As^5+^O_4_)_y_(S^6+^O_4_)_z_·nH_2_O 
  + ima[Fe,AsO_4_,SO_4_,H_2_O] (?)
  + mindat is IMA, and yeah this is a mess.
+ Protochabourneite
  + rruff: Tl^1+^_5-x_Pb^2+^_2x_(Sb^3+^,As^3+^)_21-x_S^2-^_34_ (x~1.2-1.5) 
  + ima: Tl_2_Pb(Sb,As)_10_S_17_
  + mindat:  Tl2Pb(Sb,As)10S17 , IMA formula is Tl2Pb(Sb,As)10S17. Also given as Tl5-xPb2x(Sb,As)21-xS34 (x ~ 1.2-1.5).
+ Rosieresite
  + rruff Pb_x_Cu_y_Al_z_(PO_4_)_m_·nH_2_O 
  + ima [Pb,Cu,Al,PO_4_,H_2_O] (?)
  + mindat: (Pb, Cu, Al, P, O, H) 
+ Woodruffite
  + rruff Zn^2+^_2_Mn^4+^_5_O_12_·4H_2_O
  + ima Zn_2_(Mn^4+^,Mn^3+^)_5_O_10_·4H_2_O ( how many oxygens?)
  + mindat Zn2+x/2(Mn4+1-xMn3+x)O2 · yH2O; x ~ 0.4 and y ~ 0.7


## EXCLUDE since RRUFF != IMA != MINDAT

+ Bystrite, how many sulfurs?
  + ima:  (Na,K)_7_Ca(Si_6_Al_6_)O_24_(S^2-^)_1.5_·H_2_O
  + rruff: (Na,K)_7_Ca(Si_6_Al_6_)O_24_(S_3_)_1.5_·H_2_O
  + mindat (Na,K)7Ca(Al6Si6O24)(S5)Cl
+ Camanchacaite, how many AsO4? is this a parantheses issue?
  + rruff: Na[box]CaMg_2_[As^5+^O_4_]_2_[As^5+^O_2_(OH)_2_] 
  + ima:  Na[box]CaMg_2_[AsO_4_]_2_[AsO_3_(OH)_2_]
  + mindat: NaCaMg2[AsO4][AsO3(OH)]2
+ Cyprine, how many al/mg?
  + rruff: Ca_19_Cu^2+^(Al_10_Mg_2_)Si_18_O_69_(OH)_9_ 
  + ima: Ca_19_Cu^2+^(Al,Mg)_12_Si_18_O_69_(OH)_9_
  + mindat: Ca19Cu2+Al4(Al6Mg2)(☐4)☐[Si2O7]4[(SiO4)10](OH)(OH)9
+ Ganomalite, how many ca? what even
  + rruff:  Pb^2+^_3_Ca_2_(SiO_4_)(Si_2_O_7_)
  + ima: Pb_9_Ca_6_(Si_2_O_7_)_4_(SiO_4_)O
  + mindat: Pb9Ca5Mn(Si2O7)4(SiO4)O
+ Gerasimovskite how many Ti and Nb? should i consider a Ca?
  + ima: Mn^2+^(Ti,Nb)_5_O_12_·9H_2_O (?)
  + rruff:  Mn^2+^Ti^4+^_3_Nb^5+^_2_O_12_·9H_2_O
  + mindat: (Mn,Ca)(Nb,Ti)5O12 · 9H2O
+ Meta-alunogen also has a *2 thing, rruff has 27, ima has 14, mindat has 12 but otherwise agrees with ima sooo????????????
  + rruff: Al_4_(SO_4_)_6_·27H_2_O 
  + ima: Al_2_(SO_4_)_3_·14H_2_O
  + mindat: Al2(SO4)3 · 12H2O
+ Metauranocircite-I 
  + rruff 8 water ima 6, mindat 7 ??????
+ Natroglaucocerinite has x's but ima/mindat agree on specific values
  + rruff: Zn^2+^_8-x_Al_x_(OH)_16_(S^6+^O_4_)_x/2+y/2_Na_y_(H_2_O)_6_ 
  + ima: Zn_6_Al_3_(OH)_18_[Na(H_2_O)_6_](SO_4_)_2_·6H_2_O
  + mindat: Zn6Al3(OH)18[Na(H2O)6](SO4)2•6H2O
+ Nordgauite OH? Waters?
  + rruff: Mn^2+^Al_2_(P^5+^O_4_)_2_F_2_·5H_2_O 
  + ima: MnAl_2_(PO_4_)_2_(F,OH)_2_·5.5H_2_O
  + mindat: MnAl2(PO4)2(F,OH)2 · 5H2O
+ Umbozerite OH? Waters?
  + rruff: Na_3_Sr_4_Th^4+^Si_8_O_23_(OH) 
  + ima: Na_3_Sr_4_ThSi_8_(O,OH)_24_
  + mindat: Na3Sr4Th[Si(O,OH)3-4]8
+ Yedlinite OH? O? Waters?
  + rruff: Pb^2+^_6_Cr^3+^Cl^1-^_6_O(OH)_7_ 
  + ima: Pb_6_Cr(Cl,OH)_6_(OH,O)_8_
  + mindat: Pb6Cr3+Cl6(O,OH,H2O)8
+ Yukonite waters?
  + rruff: Ca_7_Fe^3+^_15_(As^5+^O_4_)_9_O_16_·25H_2_O 
  + ima: Ca_2_Fe^3+^_3_(AsO_4_)_3_(OH)_4_·4H_2_O
  + mindat: Ca3Fe3+(AsO4)2(OH)3 · 5H2O


### Minerals where rruff/ima formulas are the same but increased by a factor of 2, or something, so calculations unaffected

+ Baghdadite
+ Burpalite
+ Clinoenstatite
+ Clinoferrosilite
+ Cuspidine
+ Droninoite
+ Eltyubyuite
+ Enstatite
+ Glaucodot
+ Hannebachite
+ Hielscherite
+ Hodrusite
+ Jagoite
+ Killalaite
+ Kuannersuite-(Ce)
+ Lavenite
+ Magnussonite
+ Marumoite
+ Nagyagite
+ Normandite
+ Okenite
+ Pararealgar
+ Paraschachnerite
+ Paratimroseite
+ Penkvilksite
+ Peprossiite-(Ce)
+ Pigeonite
+ Plumbojarosite
+ Roxbyite (1.8 != 1.78 but that's cool for this application)
+ Wallkilldellite
+ Weibullite (rounding, one is 0.3 and one is 0.33, OK)



### Miscellaneous things found: 

+ Ammineite ima formula needs more parentheses
  + CuCl_2_·2NH_3_ should be CuCl_2_·2(NH_3_)
+ Ferricopiapite ima uses 0.67 and rruff uses 2/3, this is ok.


### RRUFF matches Mindat, NOT IMA: **USE RRUFF**
+ Chukhrovite-(Ce) waters: rruff 10, ima 12, mindat 10. keep RRUFF 
+ Chukhrovite-(Y) waters: rruff 10, ima 12, mindat 10. keep RRUFF
+ Cooketie 
  + ruff: LiAl_4_(Si_3_Al)O_10_(OH)_8_
  + ima: (Al,Li)_3_Al_2_(Si,Al)_4_O_10_(OH)_8_
  + mindat: (Al2Li)Al2(AlSi3O10)(OH)8
+ Furongite 
  + rruff: Al_13_(U^6+^O_2_)_7_(PO_4_)_13_(OH)_14_·58H_2_O
  + ima: Al_4_(UO_2_)_4_(PO_4_)_6_(OH)_2_(H_2_O)_19.5_
  + mindat: Al13(UO2)7(PO4)13(OH)14 · 58H2O
+ Jorgkellerite
  + rruff: (Na,[box])_3_Mn^3+^_3_(P^5+^O_4_)_2_(C^4+^O_3_)(O,OH)_2_·5H_2_O
  + ima: Na_3_Mn^3+^_3_(PO_4_)_2_(CO_3_)O_2_·5H_2_O
  + mindat: (Na,☐)3Mn3+3(PO4)2(CO3)(O,OH)2·5H2O
+ Krasnoite 
  + rruff: Ca_3_Al_7.7_Si_3_P_4_O_23.5_(OH)_12.1_F_2_·8H_2_O
  + ima:  Ca_3_Al_7.7_Si_3_P_4_O_22.9_(OH)_13.3_F_2_·8H_2_O
  + mindat: Ca3Al7.7Si3P4O23.5(OH)12.1F2 · 8H2O
+ Melkovite 
  + rruff: [Ca_2_(H_2_O)_15_Ca(H_2_O)_6_][Mo^6+^_8_P_2_Fe^3+^_3_O_36_(OH)]
  + ima:  CaFe^3+^_2_Mo_5_O_10_(PO_4_)_2_(OH)_12_·8H_2_O
  + mindat: [Ca2(H2O)15Ca(H2O)6][Mo8P2Fe3+3O36(OH)]
+ Microsommite 
  + rruff: Na_4_K_2_Ca_2_(SO_4_)(Si_6_Al_6_O_24_)Cl_2_
  + ima: [(Na,K)_6_(SO_4_)][Ca_2_Cl_2_][(Si_6_Al_6_O_24_)]
  + mindat Na4K2Ca2(Al6Si6O24)(SO4)Cl2
+ Nielsbohrite 
  + rruff: K(UO2)3(AsO4)(OH)4 · H2O
  + ima: (K,U,[box])(UO_2_)_3_(AsO_4_)(OH)_4_·H_2_O
  + mindat: K(UO2)3(AsO4)(OH)4 · H2O
+ Taimyrite-I 
  + rruff: (Pd,Cu,Pt)_3_Sn 
  + ima: (Pd,Pt)_9_Cu_3_Sn_4_
  + mindat: (Pd,Cu,Pt)_3_Sn
+ Tatyanaite
  + rruff: (Pt,Pd,Cu)_9_Cu_3_Sn_4_ 
  + ima: (Pt,Pd)_9_Cu_3_Sn_4_
  + mindat: (Pt,Pd,Cu)9Cu3Sn4
+ Telluroperite
  + rruff: Pb^2+^_3_Te^4+^O_4_Cl^1-^_2_ 
  + ima Pb(Te_0.5_Pb_0.5_)O_2_Cl
  + mindat: Pb3TeO4Cl2
+ Theoparacelsite 
  + rruff: Cu^2+^_3_(OH)_2_As^5+^_2_O_7_(OH)_2_ 
  + ima: Cu_3_(OH)_2_As_2_O_7_
  + mindat: Cu3(As2O7)(OH)2
+ Uranospathite
  + rruff: (Al,[box])(U^6+^O_2_)_2_F(PO_4_)_2_·20(H_2_O,F)
  + ima: (Al,[box])(UO_2_)_2_F(PO_4_)_2_·20H_2_O 
  + mindat: (Al,◻)(UO2)2(PO4)2F · 20(H2O,F)



### Inconsistent rruff/ima: keep IMA

+ Alumohydrocalcite waters: rruff 3, ima 4, mindat 4. keep ima
+ Byzantievite (rruff missing trace REE)
  + rruff  Ba_5_(Ca,Y^3+^)_22_(Ti^4+^,Nb^5+^)_18_(SiO_4_)_4_(P^5+^O_4_,SiO_4_)_4_(BO_3_)_9_O_22_((OH),F)_43_(H_2_O)_1.5_
  + ima  Ba_5_(Ca,REE,Y)_22_(Ti,Nb)_18_(SiO_4_)_4_[(PO_4_),(SiO_4_)]_4_(BO_3_)_9_O_22_[(OH),F]_43_(H_2_O)_1.5_
  + mindat Ba5(Ca,REE,Y)22(Ti,Nb)18(SiO4)4[(PO4, SiO4)]4(BO3)9O22[(OH),F]43(H2O)1.5
+ Calciocatapleiite waters: rruff 1, ima 2, mindat 2. keep ima
+ Christelit
  + rruff: Zn^2+^_3_Cu^2+^_2_(S^6+^_4_)_2_(OH)_6_·4H_2_O 
  + ima: Zn_3_Cu_2_(SO_4_)_2_(OH)_6_·4H_2_O
  + mindat: Cu2Zn3(SO4)2(OH)6 · 4H2O
+ Donpeacorite Mn^2+^MgSi_2_O_6_  ( due to a trace) 
+ Galloplumbogummite   Pb(Ga,Al,Ge)3(PO4)2(OH)6 (due to a trace)-> USE IMA!!!!!
+ Gjerdingenite-Na!
  + ima: K_2_Na(Nb,Ti)_4_(Si_4_O_12_)_2_(OH,O)_4_·5H_2_O 
  + rruff: (K,Na)_2_Na(Nb^5+^,Ti^4+^)_4_(Si_4_O_12_)_2_(OH,O)_4_·5H_2_O
  + mindat: K2Na(Nb,Ti)4(Si4O12)2(OH,O)4 · 5H2O
+ Launayite
  + ima: CuPb_10_(Sb,As)_13_S_20_
  + rruff: CuPb_10_(Sb,As)_13_S_30_
  + mindat: CuPb10(Sb,As)12S20
+ Metanovacekite waters: rruff 4-8 (6), ima 8, mindat 8. keep ima
+ Milarite waters: rruff `x`, mindat 1, ima 1. USE IMA
+ Novacekite-II waters: rruff 9, ima 10, mindat 10. USE IMA
+ Oxy-vanadium-dravite 
  + ima: NaV_3_(V_4_Mg_2_)(Si_6_O_18_)(BO_3_)_3_(OH)_3_O
  + rruff:  NaMg_3_V^3+^_6_(Si_6_O_18_)(BO_3_)_3_(OH)_3_OH
  + mindat: Na(V)3(V4Mg2)Si6O18(BO3)3(OH)3O
+ Qandilite (rruff missing a trace) 
  + rruff: Mg_2_(Ti^4+^,Fe^3+^,Al)O_4_
  + ima: (Mg,Fe^3+^)_2_(Ti,Fe^3+^,Al)O_4_
  + mindat: (Mg,Fe3+)2(Ti,Fe3+,Al)O4
+ Ramikite-(Y) (rruff missing a trace) 
  + rruff: Li_2_Na_6_(Y,Ca,REE)_3_Zr^4+^_3_(P^5+^O_4_)_6_(C^4+^O_3_)_2_O_2_(OH,F)_2_ 
  + ima: Li_4_(Na,Ca)_12_(Y,Ca,REE)_6_Zr_6_(PO_4_)_12_(CO_3_)_4_O_4_[(OH),F]_4_
  + mindat: Li4(Na,Ca)12Y6Zr6(PO4)12(CO3)4O4[(OH),F]4
+ Rankachite
  + rruff CaFe^2+^(V^5+^,V^3+^)_4_O_4_(WO_4_)_8_·12H_2_O 
  + ima Ca_0.5_(V^4+^,V^5+^)(W^6+^,Fe^3+^)_2_O_8_(OH)·2H_2_O
  + mindat  Ca0.5(V4+,V5+)(W6+,Fe3+)2O8(OH) · 2H2O
+ Renierite
  + rruff (Cu^1+^,Cu^2+^Zn^2+^)_11_(Fe^2+^,Fe^3+^)_4_(Ge^4+^,As^5+^)_2_S^2-^_16_ 
  + ima (Cu^1+^,Zn)_11_Fe_4_(Ge^4+^,As^5+^)_2_S_16_
  + mindat (Cu1+,Zn)11Fe4(Ge4+,As5+)2S16
+ Rhodesite
  + rruff K_2_Ca_2_Si_8_O_19_·5H_2_O 
  + ima KHCa_2_Si_8_O_19_·5H_2_O
  + mindat KHCa2Si8O19 · 5H2O
+ Rogermitchellite
  + rruff (has extra trace?) Na_6_(Sr,Na)_12_Ba_2_Zr^4+^_13_Si_39_(B,Si)_6_O_123_(OH)_12_·9H_2_O 
  + ima Na_6_Sr_12_Ba_2_Zr_13_Si_39_B_4_O_123_(OH)_6_·20H_2_O
  + mindat  Na6Sr12Ba2Zr13Si39B4O123(OH)6 ·20H2O
+ Meurigite-K
  + rruff: [K(H_2_O)_2.5_][Fe^3+^_8_(PO_4_)_6_(OH)_7_]·4H_2_O
  + ima:  KFe^3+^_8_(PO_4_)_6_(OH)_7_·6.5H_2_O
  + mindat: KFe3+8(PO4)6(OH)7 · 6.5H2O
+ Paceite 
  + rruff: CaCu(CH_3_COO)_2_·6H_2_O 
  + ima: CaCu(CH_3_COO)_4_·6H_2_O
  + mindat: CaCu(CH3COO)4 · 6H2O
+ Parasibirskite
  + rruff: CaHBO_3_ 
  + ima: Ca_2_B_2_O_5_·H_2_O
  + mindat: Ca2(B2O5) · H2O
+ Pennantite 
  + rruff: (Mn^2+^,Al)_6_(Si,Al)_4_O_10_(OH)_8_ 
  + ima: Mn^2+^_5_Al(Si_3_Al)O_10_(OH)_8_
  + mindat: Mn2+5Al(AlSi3O10)(OH)8
+ Phosphofibrite 
  + rruff (K_0.5_(H_2_O)_3_)Fe^3+^_8_(PO_4_)_6_(OH)_6.5_·6.5H_2_O 
  + ima (H_2_O,K)_3.5_Fe^3+^_8_(PO_4_)_6_(OH)_7_·5H_2_O
  + mindat (H2O,K)3.5Fe8(PO4)6(OH)7 · 5H2O)
+ Pierrotite
  + rruff: TlSb_3_As_2_S_8_ 
  + ima: Tl_2_(Sb,As)_10_S_16_ (effectively the same but slight diff)
  + mindat: Tl2(Sb,As)10S16
+ Saneroite 
  + rruff: Na_2_(Mn^2+^,Mn^3+^)_10_V^5+^Si_11_O_34_(OH)_4_ 
  + ima: NaMn^2+^_5_[Si_5_O_14_(OH)](VO_3_)(OH)
  + mindat: NaMn2+5[Si5O14(OH)](VO3)(OH)
+ Spionkopite 
  + rruff: Cu_1.32_S 
  + ima: Cu_39_S_28_   ( 39/28 = 1.39 so not comfortable with 1.32)
  + mindat Cu39S28
+ Stannoidite
  + rruff: Cu^1+^_8_Fe^3+^_2_(Fe^2+^,Zn^2+^)Sn^4+^_2_S^2-^_12_ 
  + ima:Cu_8_(Fe,Zn)_3_Sn_2_S_12
  + mindat: Cu+6Cu2+2(Fe2+,Zn)3Sn2S12
+ Vladimirivanovite use ima, the rruff parentheses are bonkers
+ Wilcoxite waters: rruff 18, ima 17, mindat 17. USE IMA
+ Wiklundite  use ima, the rruff parentheses are bonkers
+ Yttrotantalite-(Y) 
  + rruff: (Y^3+^,U^6+^,Fe^2+^)(Ta^5+^,Nb^5+^)_2_(O,OH)_4_
  + ima: (Y,U,Fe^2+^)(Ta,Nb)(O,OH)_4_
  + mindat (Y,U4+,Fe2+)(Ta,Nb)(O,OH)4



### Duplicate elements in parantheses that IMA doesn't do (either fe2+ or fe3+ means only 1 fe, for example)

+ Christofschaferite-(Ce)
+ Eveslogite
+ Kobeite-(Y)
+ Magnesionigerite-6N6S
+ Oxyphlogopite
+ Prismatine
+ Sakuraiite
+ Thorosteenstrupine
+ Yttrocrasite-(Y)





