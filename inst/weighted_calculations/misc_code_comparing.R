


# Check:
final_counts_possible %>% 
  dplyr::filter(mineral_name %in% true_counts$mineral_name) %>% 
  dplyr::arrange(mineral_name) %>%
  dplyr::mutate(count = round(count, TOL)) -> test_counts
stopifnot(   all_equal(true_counts, test_counts)    )





# print("RRUFF")
# 
# med_data_raw %>%
#   dplyr::mutate(chem = rruff) %>%
#   apply_manual_formula_changes() -> med_data_rruff_chem
# rruff_counts <- count_elements(med_data_rruff_chem, exclude, water_standin, hydroxy_standin, ree_standin)
# 
# print("IMA UNLESS EASIER RRUFF")
# 
# med_data_raw %>%
#   dplyr::mutate(chem = ifelse(mineral_name %in% easier_parsing_with_rruff, rruff, ima)) %>%
#   apply_manual_formula_changes() -> med_data_ima_chem
# ima_counts <- count_elements(med_data_ima_chem, exclude, water_standin, hydroxy_standin, ree_standin)
#   
# print("IMA UNLESS RRUFF, ALL OF THEM")
# 
# med_data_raw %>%
#   dplyr::mutate(chem = ifelse(mineral_name %in% use_rruff, rruff, ima)) %>%
#   apply_manual_formula_changes() -> med_data_ima_except_known_rruff_chem
# ima_except_counts <- count_elements(med_data_ima_except_known_rruff_chem, exclude, water_standin, hydroxy_standin, ree_standin)
# stop()

#anti_join(ima_counts, ima_except_counts) %>% select(mineral_name) %>% distinct() %>% pull() --> 
## Only differences are those in the known to be rruff list. 

# anti_join(rruff_counts, ima_except_counts) %>% select(mineral_name) %>% distinct()
## These disagree between rruff and ima. do one final check to confirm so we can automate later.
#write_csv(rruff_counts, "rruff_counts.csv")
#write_csv(ima_counts, "ima_counts_except_3easier_rruff.csv")
#write_csv(ima_except_counts, "ima_counts_except_all_known_rruff.csv")


rruff_counts <- read_csv("rruff_counts.csv") %>% rename(rruff_count = count)
ima_except_counts<- read_csv("ima_counts_except_all_known_rruff.csv")%>% rename(ima_count = count)


dplyr::full_join(ima_except_counts, rruff_counts) %>%
  dplyr::filter(ima_count != rruff_count) %>%
  dplyr::inner_join(med_data_raw) -> manually_check_these
write_csv(manually_check_these, "manually_check_these.csv")
# mindat formulas shown in comment
confirmed_ima_right <- c("Aeschynite-(Y)",     # (Y,Ln,Ca,Th)(Ti,Nb)2(O,OH)6
                         "Afghanite",          # (Na,K)22Ca10(Si24Al24O96)(SO4)6Cl6
                         "Albrechtschraufite", # Ca4Mg(UO2)2(CO3)6F2 · 17-18H2O
                         "Almarudite",         # K(◻,Na)2(Mn,Fe,Mg)2[(Be,Al)3Si12]O30
                         "Aluminoceladonite",  # K(Mg,Fe2+)Al(Si4O10)(OH)2
                         "Aluminocerite-(Ce)", # (Ce,La,Ca)9(Al,Fe3+)(SiO4)3(HSiO4)4(OH)3
                         "Alumohydrocalcite",  # CaAl2(CO3)2(OH)4 · 4H2O
                         "Ankerite",           # Ca(Fe2+,Mg)(CO3)2
                         "Aurichalcite",       # (Zn,Cu)5(CO3)2(OH)6
                         "Babkinite",          # Pb2Bi2(S,Se)3
                         "Baileychlore",       # (Zn,Fe2+,Al,Mg)6(Si,Al)4O10(OH)8
                         "Bambollaite",        #Cu(Se,Te)2
                         "Baricite",           # (Mg,Fe)3(PO4)2 · 8H2O
                         "Barringerite",      # (Fe,Ni)2P
                         "Bussyite-(Ce)",     # (Ce,REE)3(Na,H2O)6MnSi9Be5(O,OH)30F4
                         "Calciocatapleiite", # CaZr(Si3O9) · 2H2O
                         "Calciosamarskite",  # (Ca,Fe3+,Y)2(Nb,Ta,Ti)2O8
                         "Chamosite", # (Fe2+,Mg,Al,Fe3+)6(Si,Al)4O10(OH,O)8
                         "Christelite", #Cu2Zn3(SO4)2(OH)6 · 4H2O (rruff has an obvious typo missing an oxygen)
                         "Combeite", #Na4.5Ca3.5Si6O17.5(OH)0.5
                         "Donpeacorite", # (Mn2+,Mg)Mg[SiO3]2
                         "Ferrochiavennite", # Ca1-2Fe[(Si,Al,Be)5Be2O13(OH)2] · 2H2O
                         "Galloplumbogummite", #Pb(Ga,Al,Ge)3(PO4)2(OH)6 
                         "Georgbarsanovite", #Na12(Mn,Sr,REE)3Ca6Fe2+3Zr3NbSi25O76Cl2 · H2O
                         "Gjerdingenite-Na", #K2Na(Nb,Ti)4(Si4O12)2(OH,O)4 · 5H2O
                         "Grayite", # (Th,Pb,Ca)(PO4) · H2O
                         "Hematophanite", #Pb4Fe3O8(OH,Cl),
                         "Hydroxyapophyllite-(K)", # KCa4(Si8O20)(OH,F) · 8H2O
                         "Launayite", #CuPb10(Sb,As)12S20
                         "Lukrahnite", #CaCuFe3+(AsO4)2(OH,H2O)2
                         "Magnesioneptunite") #KNa2Li(Mg,Fe)2Ti2Si8O24


new_discrepencies <- c("Andersonite",    # Na2Ca(UO2)(CO3)3 · 6H2O. ima: 5-66 waters. rruff: 5-6 waters.
                       "Batisivite",  # BaV3+8Ti6(Si2O7)O22 . ima: has a (V,Cr). rruff has 27 waters
                       "Calcjarlite",   # Na(Ca,Sr)3Al3(OH)2F14. ima: Na_2_(Ca,[box])_14_(Mg,[box])_2_Al_12_F_64_(OH)_4_, rruff:  NaCa_3_Al_3_F_16_           
                       "Chirvinskyite", # (Na,Ca)13(Fe,Mn,□)2(Ti,Zr)5(Si2O7)4(OH,O)12·2H2O. ima: (Na,Ca)_13_(Fe,Mn,[box])_2_Ti_2_(Zr,Ti)_3_(Si_2_O_7_)_4_(OH,O,F)_12_ rruff: (Na_8_Ca_5_)Fe^2+^_2_Ti^4+^_2_Zr^4+^_3_(Si_2_O_7_)_4_O_6_(OH)_6_
                       "Cookeite",#(Al2Li)Al2(AlSi3O10)(OH)8, ima: (Al,Li)_3_Al_2_(Si,Al)_4_O_10_(OH)_8_ rruff:  LiAl_4_(Si_3_Al)O_10_(OH)_8_
                       "Garronite-Ca", # Na2Ca5Al12Si20O64 · 27H2O, ima: Ca_3_(Al_6_Si_10_O_32_)·14H_2_O   rruff: NaCa_2.5_(Si_10_Al_6_)O_32_·14H_2_O           
                       "Laitakarite") # Bi4Se2S ima: Bi_4_(Se,S)_3_  rruff:Bi_4_Se_3_      


new_use_rruff <- c("Grossmanite", #CaTi3+ AlSiO6 matches rruff, but ima is: Ca(Ti^3+^,Mg,Ti^4+^)AlSiO_6_  
                   "Guettardite", # PbAsSbS4 ima:Pb_8_(Sb_0.56_As_0.44_)_16_S_32_ rruff: PbSbAsS_4_  .
                   "Juanite",    # "Ca10Mg4Al2Si11O39 · 4H2O or near". ima: Ca_10_(Mg,Fe^2+^)_4_(Si,Al)_13_(O,OH)_39_·4H_2_O rruff:Ca_10_Mg_4_Al_2_Si_11_O_39_·4H_2_O  
                   "Krieselite", # (Al,Ga)2(GeO4)(OH)2 ima: Al_2_(GeO_4_)F_2_ rruff: Al_2_GeO_4_(OH)_2_        .
                   "Kuzmenkoite-Mn", # K2Mn2+(Ti,Nb)4(Si4O12)2(OH,O)4 · 5-6H2O. ima: K_2_MnTi_4_(Si_4_O_12_)_2_(OH)_4_·5-6H_2_O ruff: K_4_Mn^2+^_2_Ti^4+^_8_(Si_4_O_12_)_4_(OH,O)_8_·10-12H_2_O 
                   "Labuntsovite-Fe", # Na4K4(Ba,K)2Fe2+(Ti,Nb)8(Si4O12)4(O,OH)8 · 10H2O   ima: Na_4_K_4_Fe^2+^_2_Ti_8_O_4_(Si_4_O_12_)_4_(OH)_4_·10-12H_2_O  rruff: Na_4_K_4_Fe^2+^_2_Ti^4+^_8_(Si_4_O_12_)_4_(O,OH)_8_·10-12H_2_O
                   "Luinaite-(OH)", # Na(Fe2+)3Al6(Si6O18)(BO3)3(OH)3(OH) ima has a trace Mg comma w/ Fe that mindat doesn't ??????? NETWORK BUG ALERT
                   "Manganbelyankinite", # (Mn,Ca)(Ti,Nb)5O12 · 9H2O . rruff has the trace Ca and ima just has Mn
                   "Mapiquiroite") # (Sr,Pb)(U,Y)Fe_2_(Ti,Fe^3+^)_18_O_38_   rruff has the Cr and ima just has Ti,Fe


new_exclusions <- c( "Augite")   # (CaxMgyFez)(Mgy1Fez1)Si2O6  EXCLUDE

known_mult_rounding_ok <- c("Baghdadite", 
                            "Burpalite", 
                            "Burpalite", 
                            "Clinoenstatite", 
                            "Clinoferrosilite", 
                            "Cuspidine", 
                            "Droninoite", 
                            "Eltyubyuite", 
                            "Enstatite", 
                            "Ferricopiapite",
                            "Glaucodot", "Hannebachite", "Hielscherite", "Hodrusite", "Jagoite", "Killalaite",
                            "Kuannersuite-(Ce)", "Lavenite","Magnussonite", "Marumoite")

manually_check_these %>% filter(!(mineral_name %in% confirmed_ima_right), 
                                !(mineral_name %in% new_discrepencies), 
                                !(mineral_name %in% new_exclusions),
                                !(mineral_name %in% known_mult_rounding_ok),
                                !(mineral_name %in% new_use_rruff))
#STILL EXPLORING
stop()
######################################################################
