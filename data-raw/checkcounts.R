

# Random list of minerals to check:
tibble::tribble(~mineral_name,       ~element,    ~count,
                # ZnFe^3+^_2_(PO_4_)_2_(OH)_2_·6.5H_2_O
                "Zincostrunzite", "Zn",1,
                "Zincostrunzite", "Fe",2,
                "Zincostrunzite", "P",2,
                "Zincostrunzite", "O",16.5,
                "Zincostrunzite", "H",15,
                # Na_2_Ca_4_YTi(Si_2_O_7_)_2_OF_3_
                "Rinkite-(Y)", "Na",2,
                "Rinkite-(Y)", "Ca",4,
                "Rinkite-(Y)", "Y",1,
                "Rinkite-(Y)", "Ti",1,
                "Rinkite-(Y)", "Si",4,
                "Rinkite-(Y)", "O",15,
                "Rinkite-(Y)", "F",3,
                # MgMn^2+^Al(PO_4_)_2_(OH)·4H_2_O
                "Lunokite","Mg",1,
                "Lunokite","Mn",1,
                "Lunokite","Al",1,
                "Lunokite","P",2,
                "Lunokite","O",13,
                "Lunokite","H",9,
                # [(UO_2_)_2_(C_2_O_4_)(OH)_2_(H_2_O)_2_]·H_2_O
                "Uroxite","U",2,
                "Uroxite","O",13,
                "Uroxite","C",2,
                "Uroxite","H",8,
                # Mn_2_Fe^3+^(SiFe^3+^)O_5_(OH)_4_
                "Guidottiite","Mn",2,
                "Guidottiite","Fe",2,
                "Guidottiite","Si",1,
                "Guidottiite","O",9,
                "Guidottiite","H",4,
                # (Mn,Th,Na,Ca,REE)_2_(Nb,Ti)_2_O_6_(OH)
                "Hydroxymanganopyrochlore", "Mn", 0.4, 
                "Hydroxymanganopyrochlore", "Th", 0.4, 
                "Hydroxymanganopyrochlore", "Na", 0.4, 
                "Hydroxymanganopyrochlore", "Ca", 0.4, 
                "Hydroxymanganopyrochlore", "REE", 0.4, 
                "Hydroxymanganopyrochlore", "Nb", 1, 
                "Hydroxymanganopyrochlore", "Ti", 1, 
                "Hydroxymanganopyrochlore", "O", 7, 
                "Hydroxymanganopyrochlore", "H", 1, 
                # Al_13_(U^6+^O_2_)_7_(PO_4_)_13_(OH)_14_·58H_2_O
                "Furongite","Al",13,
                "Furongite","U",7,
                "Furongite","P",13,
                "Furongite","O",138,
                "Furongite","H",130,
                # Pb(U^4+^,U^6+^)Fe^2+^_2_(Ti,Fe^2+^,Fe^3+^)_18_(O,OH)_38_
                "Cleusonite", "Pb", 1,
                "Cleusonite", "U", 1,
                "Cleusonite", "Fe", 11,
                "Cleusonite", "Ti", 9,
                "Cleusonite", "O", 28.5,
                "Cleusonite", "H", 9.5,
                #"NaCa_2_Fe_2_(Fe,Mn,Fe)_4_(PO_4_)_6_·2H_2_O"
                "Wicksite", "Na", 1,
                "Wicksite", "Ca", 2,
                "Wicksite", "Fe", 4,
                "Wicksite", "Mn", 2,
                "Wicksite", "P", 6,
                "Wicksite", "O", 26,
                "Wicksite", "H", 4,
                # (Al,[box])(U^6+^O_2_)_2_F(PO_4_)_2_(H_2_O,F)_20_
                "Uranospathite", "Al", 1,
                "Uranospathite", "U", 2,
                "Uranospathite", "O", 4 + 8 + (20 * 1/2*1/3), 
                "Uranospathite", "F", 11,
                "Uranospathite", "P", 2,
                "Uranospathite", "H", 20*(2/3 * 1/2), 
                # (Ba,Na,K)_2_(Na,Ti,Mn)_4_(Ti,Nb)_2_O_2_Si_4_O_14_(H_2_O,F,OH)_2_·3.5H_2_O 
                "Bykovaite", "Ba", 2/3,
                "Bykovaite", "Na", 2,
                "Bykovaite", "K", 2/3,
                "Bykovaite", "Ti", 7/3,
                "Bykovaite", "Mn", 4/3,
                "Bykovaite", "Nb", 1,
                "Bykovaite", "O", 2+ 14 + 3.5 + 2/9 + 1/3,
                "Bykovaite", "Si",4,
                "Bykovaite", "F", 2/3,
                "Bykovaite", "H", 7+4/9 + 1/3,
                # Na_4_Ti_4_(Si_2_O_6_)_2_[(Si,Al)_4_O_10_]O_4_(H_2_O,Na,K)_3_ 
                "Vinogradovite", "Na", 5,
                "Vinogradovite", "Ti", 4,
                "Vinogradovite", "Si", 6,
                "Vinogradovite", "Al", 2,
                "Vinogradovite", "K", 1,
                "Vinogradovite", "O", 12 + 10 + 4 + 1/3,
                "Vinogradovite", "H", 2/3) %>%
  dplyr::arrange(mineral_name) %>%
  dplyr::mutate(count = round(count,TOL)) -> true_counts


