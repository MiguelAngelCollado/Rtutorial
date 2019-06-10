#Phylogenies in R
#install CRAN Task View for phylogenetics
#Con esto instalas y actualizas prácticamente todas las herramientas de filogenia de R
install.packages('ctv')
library('ctv') 
install.views('Phylogenetics')
update.views('Phylogenetics')

#Librería para leer y modificar árboles filogenéticos
library(ape)
#Creamos uno al azar como ejemplo
tree<-rtree(n=20)
tree
#Un árbol filogenético es una list() de clase "phylo" 
plot(tree)
#Con cuatro partes
str(tree)

#Creamos manualmente un arbol, con simple texto, para enternderlos mejor
tree2 <- read.tree(text = "(((A,B),(C,D)),E);")
plot(tree2, type = "cladogram")
str(tree2)

#esta matriz contiene el principio y el final de cada nodo, para todos los nodos
#y hojas del arbol
tree2$edge

#Tip label contiene los nombres de las hojas
tree2$tip.label

#Nnode te da el número de nodos, puede incluir la raíz del arbol
tree2$Nnode

#Con esto identificamos el número de cada nodo y cada hoja
plot(tree2, label.offset = 0.1, type = "cladogram")
nodelabels()
tiplabels()




#################




##BEE_PHYLO STRUCTURE
library(MCMCglmm)
library(brms)
library(data.tree)
library('ctv') 
install.views('Phylogenetics')
update.views('Phylogenetics')

#Librería para leer y modificar árboles filogenéticos
library(ape)

##LOAD IN YOUR TREE
############
getwd()
apoidea<-read.tree(file = "data/phylogeny_genus_level.txt")

str(apoidea)
apoidea[[1]]
tree1<-apoidea[[1]]
str(tree1)
plot(tree1)
tree1$tip.label

tree1

A <- drop.tip(tree1, tip = c("Hylaeus","Bombus","Panurgus","Xenochilicola"))
# We drop tips except "Andrena", "Anthophora", "Apis", "Bombus", "Lasioglossum","Megachile", "Eucera", "Osmia", "Panurgus", "Rhodanthidium", "Xylocopa"
# We don't have Flavipanurgus, Psithyrus
B <-drop.tip(tree1, tip = c("Xenochilicola", "Geodiscelis", "Xeromelissa", "Chilimelissa",    
                              "Hylaeus", "Amphylaeus", "Meroglossa", "Palaeorhiza",     
                              "Hyleoides", "Scrapter", "Euhesma", "Euryglossina",    
                              "Callohesma", "Euryglossa", "Xanthesma", "Stenotritus",     
                              "Ctenocolletes", "Alocandrena", "Megandrena",      
                              "Euherbstia", "Orphana", "Protoxaea", "Nolanomelissa",   
                              "Neffapis", "Meliturgula", "Plesiopanurgus", "Macrotera",       
                              "Perdita", "Clavipanurgus", "Panurginus",        
                              "Camptopoeum", "Melitturga", "Protandrena", "Pseudopanurgus",  
                              "Calliopsis", "Arhysosage", "Callonychium", "Cerceris",        
                              "Eucerceris", "Clypeadon", "Philanthus", "Pulverro",        
                              "Clitemnestra", "Stizoides", "Bembix", "Xerostictia",     
                              "Microbembex", "Bicyrtes", "Ampulex", "Sceliphron",      
                              "Chlorion", "Chalybion", "Isodontia", "Sphex",           
                              "Podalonia", "Prionyx", "Ammophila", "Eremnophila",     
                              "Oxybelus", "Anacrabro", "Plenoculus", "Tachytes",        
                              "Tachysphex", "Samba", "Capicola", "Hesperapis",      
                              "Eremaphanta", "Dasypoda", "Melitta", "Redivivoides",    
                              "Rediviva", "Macropis", "Promelitta", "Meganomia",       
                              "Habropoda", "Deltoptila", "Pachymelus", "Amegilla",        
                              "Sphecodopsis", "Pasites", "Oreopasites",     
                              "Ammobates", "Odyneropsis", "Triepeolus", "Rhinepeolus",     
                              "Doeringiella", "Thalestria", "Epeolus", "Triopasites",     
                              "Brachynomada", "Paranomada", "Holcopasites", "Ammobatoides",    
                              "Nomada", "Hexepeolus", "Neolarra", "Biastes",         
                              "Neopasites", "Townsendiella", "Caenoprosopina", "Caenoprosopis",   
                              "Tetralonioidella", "Zacosmia", "Xeromelecta", "Melecta",         
                              "Thyreus", "Hopliphora", "Mesoplia", "Mesocheira",      
                              "Ctenioschelus", "Epiclopus", "Mesonychium", "Ericrocis",       
                              "Rhathymus", "Nanorhathymus", "Osiris", "Isepeolus",       
                              "Melectoides", "Epeoloides", "Leiopodus", "Coelioxoides",    
                              "Parepeolus", "Ancyla", "Florilegus", "Svastrina",       
                              "Peponapis", "Xenoglossa", "Tetraloniella",   
                              "Tetralonia", "Svastra", "Melissodes", "Martinapis",      
                              "Svastrides", "Thygater", "Melissoptila", "Meliphilopsis",   
                              "Diadasia", "Alepidosceles", "Ptilothrix", "Diadasina",       
                              "Melitoma", "Tapinotaspoides", "Caenonomada", "Tapinotaspidini", 
                              "Arhysoceble", "Paratetrapedia", "Anthophorula", "Exomalopsis",     
                              "Ancyloscelis", "Epicharis", "Exaerete", "Euglossa",        
                              "Aglae", "Eulaema", "Eufriesea",            
                              "Tetragonilla", "Tetragonula", "Platytrigona",    
                              "Heterotrigona", "Sundatrigona", "Geniotrigona", "Lepidotrigona",   
                              "Lophotrigona", "Tetrigona", "Homotrigona", "Odontotrigona",   
                              "Leurotrigona", "Hypotrigona", "Austroplebeia", "Lisotrigona",     
                              "Liotrigona", "Plebeiella", "Axestotrigona", "Meliponula",      
                              "Apotrigona", "Meliplebeia", "Plebeina", "Dactylurina",     
                              "Melipona", "Parapartamona", "Meliwillea", "Partamona",       
                              "Nogueirapis", "Aparatrigona", "Paratrigona", "Nannotrigona",    
                              "Tetragonisca", "Frieseomelitta", "Duckeola", "Trichotrigona",   
                              "Lestrimelitta", "Plebeia", "Friesella", "Mourella",        
                              "Schwarziana", "Oxytrigona", "Scaptotrigona", "Ptilotrigona",    
                              "Tetragona", "Trigona", "Cephalotrigona", "Geotrigona",      
                              "Scaura", "Schwarzula", "Dolichotrigona", "Trigonisca",      
                              "Celetrigona", "Centris", "Manuelia", "Ctenoplectrina",  
                              "Ctenoplectra", "Macrogalea", "Allodapula",      
                              "Exoneuridia", "Exoneurella", "Brevineura", "Exoneura",        
                              "Inquilina",  "Halterapis", "Compsomelissa", "Braunsapis",      
                              "Allodape", "Ceratina", "Fideliopsis", "Fidelia",         
                              "Pararhophites", "Aspidosmia", "Aglaoapis", "Paradioxys",      
                              "Dioxys", "Noteriades", "Radoszkowskiana", 
                              "Coelioxys", "Pseudoheriades", "Afroheriades", "Protosmia",       
                              "Heriades", "Stenoheriades", "Hofferia", "Othinosmia",      
                              "Haetosmia", "Wainia", "Hoplosmia",           
                              "Ashmeadiella", "Atoposmia", "Hoplitis", "Stenosmia",       
                              "Chelostoma", "Ochreriades", "Trachusa", "Afranthidium",    
                              "Anthidium", "Serapista", "Pseudoanthidium", "Bathanthidium",   
                              "Dianthidium", "Anthidiellum", "Paranthidium",  
                              "Icteranthidium", "Pachyanthidium", "Benanthis", "Eoanthidium",     
                              "Hypanthidium", "Duckeanthidium", "Anthodioctes", "Hypanthidioides", 
                              "Notanthidium", "Epanthidium", "Stelis", "Lithurgus",       
                              "Microthurge", "Trichothurgus", "Neofidelia", "Dieunomia",       
                              "Pseudapis", "Lipotriches", "Curvinomia", "Hoplonomia",      
                              "Nomia", "Macronomia", "Nomioides", "Cellariella",     
                              "Corynura", "Neocorynura", "Megommation", "Megalopta",       
                              "Xenochlora", "Megaloptidia", "Augochlora", "Augochlorella",   
                              "Augochloropsis", "Agapostemon", "Dinagapostemon", "Rhinetula",       
                              "Caenohalictus", "Habralictus", "Ruizantheda", "Pseudagapostemon",
                              "Eupetersia", "Sphecodes", "Mexalictus", "Patellapis",      
                              "Thrincohalictus", "Halictus", "Homalictus",   
                              "Parathrincostoma", "Thrinchostoma", "Penapis", "Goeletapis",      
                              "Xeralictus", "Protodufourea", "Dufourea", "Systropha",       
                              "Rophites", "Sphecodosoma", "Conanthalictus", "Mydrosoma",       
                              "Ptiloglossidia", "Willinkapis", "Caupolicana", "Ptiloglossa",     
                              "Zikanapis", "Cadeguala", "Diphaglossa", "Cadegualina", 
                              "Edwyniana", "Belopria", "Nomiocolletes", "Eulonchopria",    
                              "Hoplocolletes",  "Niltonia", "Spinolapis", "Kylopasiphae",    
                              "Hexantheda", "Brachyglossula", "Tetraglossula", "Perditomorpha",   
                              "Halictanthrena", "Phenacolletes", "Euryglossidia", "Excolletes",      
                              "Leioproctus", "Lamprocolletes", "Neopasiphae", "Andrenopsis",     
                              "Colletellus", "Protomorpha", "Goniocolletes", "Odontocolletes",  
                              "Glossurocolletes", "Reedapis", "Cephalocolletes", "Chilicolletes",   
                              "Paracolletes", "Trichocolletes", "Callomelitta", "Xanthocotelles",  
                              "Hemicotelles", "Colletes", "Mourecotelles", "Chilicola"))

B
str(B)
plot(B)







################
