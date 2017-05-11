(**exo1.1
définition de l'arbre ,c'est un arbre de type (int*string ),int:son niveau dans ce arbre ,string : le feuille de string **)
type string_builder =Vide
|Noeud of (int*string) *string_builder*string_builder;;

(**exo1.2
fonction word: prend en argument une chaîne de caractères et qui renvoie le string_builder
correspondant.  l'initiation de niveau est 0
@para :str :une chaîne de caractères
@retour : un feuille de type string_builder
**)
let word str = Noeud(((String.length str),str),Vide,Vide);;                

(**exécution **)word "abc";;

(**exo1.3
fonction concat: prend en argument deux string_builder et qui renvoie le nouveau
string_builder résultant de leur concaténation. 
l'order est pas sûr ,donc on utilise  ran_int2 () donne un int aléatoire entre 0,1 pour indiquer l'ordre de abr1 et abr2
@para :abr1,abr2 :2 arbres à combiner 
@retour : concaténation de abr1 et abr2 de type string_builder
**)
let ran_int2 ()=Random.int 2;;

let concat abr1 abr2=let i=ran_int2 () in if (i=0) then Noeud((0,"node"),abr1,abr2)
else Noeud((0,"node"),abr2,abr1);;   

(**exécution **)
let x=word "a";;  
let y=word "b";;
let z=word "c";; 
  let m=word "d";;
let a=concat x y;;
let b=concat z a;;
let c=concat m b;;




(**exo2**)
(**
fonction auxiliaire 1: to_list
@para abr:un arbre
@retour :char list correspond au argument (repecter l'ordre des caractères) 
**)
let rec to_list abr=match abr with
Vide->[]
|Noeud((a,b),fg,fd)->if((fg=Vide)&&(fd=Vide)) then to_list fg @ b ::to_list fg
else (to_list fg )@(to_list fd);;

(**
fonction auxiliaire 2: ltostr
@para :str：un list de char
@retour :string correspondant  
**)
let ltostr str=String.concat "" str;;

(**
 fonction char_at:  prendre en argument un entier i et un string_builder
représentant le mot [c0 ; . . . ; cn−1], et qui renvoie le caractère ci
para:abr:un arbre . i: ième caractère 
**)
let char_at abr i=String.get (ltostr (to_list abr)) (i-1);; 

(**exécution **)char_at c 3;;


(**exo3**)
(**
fonction sub_string :prend en arguments un entier i, un entier m et un
string_builder sb représentant le mot [c0 ; . . . ; cn−1] et qui renvoie un string_builder représentant
le mot [ci; . . . ; ci+m−1], c’est-à-dire la sous-chaîne de c débutant au caractère i et de longueur m.
@para: i: le début du sous-chaîne   m: la fin du sous-chaîne   abr:un arbre
@retour :
**)

let sub_string i m abr=String.sub (ltostr (to_list abr)) i (m-i);;



(**exo4**)
(**
fonction auxiliaire 1: niveau: set les niveaus d'arbre
@para:abr:un arbre
@retour : un arbre qui est modifie les niveau correspondants
**)
let rec niveau abr l=match abr with Vide->Vide
|Noeud((m,x),fg,fd)->Noeud((l+1,x),niveau fg (l+1),niveau fd (l+1));;
(**
fonction cost: prendre en argument un string_builder et qui renvoie son
coût selon la définition précédente
@para: abr: un arbre 
@retour : coût de type int 
**)
let rec cost abr=match abr with
Vide ->0
|Noeud((l,x),fg,fd)->if ((fg==Vide)&&(fd==Vide)) then l*(String.length x)+cost fg+cost fd
else cost fg + cost fd;;

(**exécution **)niveau c 0;;
cost c;;


(**exo5**)
(**
fonction auxiliaire 1:profondeur:calculer porfondeur
@para:abr:un arbre
@retour :son profondeur de type int  (le plus grand niveau ) 
**)
let rec profondeur abr=match abr with
 Vide->0
|Noeud((l,x),fg,fd)->max l (max (profondeur fg) (profondeur fd));;

(**générer un char aléatoire **)
let ran_char ()=Char.escaped ( Char.chr (97+(Random.int 26)));;
(**générer un int [1,10] aléatoire **)
let ran_int ()=1+(Random.int 10);;
(**combiner les chars dans un liste à un seul string**)
let ltostr str=String.concat "" str;;

(**
fonction auxiliaire 2:ran_string:générer un string aléatoire 
@para:abr: str: l'initiation est [].  ini:compteur ,l'initiation est 0.  i:longuer de string l'initiation est ran_int () ,un int aléatoire 
@retour :un string aléatoire de longuer aléatoire  
**)
let rec ran_string str ini i = 
		if(ini=i)then str
		else (ran_char ())::(ran_string str (ini+1) i);;
(**
fonction random_string: prendre en argument un entier i et qui génère
un arbre de profondeur i. 
@para: i: profondeur.  abr: l'arbre qu'on veut ,l'initiation est Vide
@retour :un arbre de profondeur i. 
**)
let rec random_string i abr=
	let abr=concat abr (word (ltostr (ran_string [] 0 (ran_int ()))))
	in
	if (profondeur(niveau abr 0)=i) then abr else random_string i abr;;


(**exécution **)let f =random_string 11 Vide;;


(**exo6**)
(**
fonction list_of_int:prendre en argument un string_builder et qui
renvoie la liste des chaînes de caractères dans le même ordre que dans l’arbre (parcours infixe).
@para: abr: un arbre
@retour:la liste des chaînes de caractères dans le même ordre que dans l’arbre 
**)
let rec list_of_string abr=match abr with
Vide->[]
|Noeud((l,b),fg,fd)->if((fg=Vide)&&(fd=Vide)) then list_of_string fg @ b ::list_of_string fg
else (list_of_string fg )@(list_of_string fd);;
 

(**exécution **)let l=list_of_string f;;

(**exo7**)
(**
fonction auxiliaire 1:transformer  
@para: e: un list de list_of_string 
@retour : un list de forme[(string*(string.lente string))] 
**)
let rec transformer l= match l with 
|[]->[]
|x::q->((word x),String.length x)::transformer q;;


(**
fonction auxiliaire 1:succ  
@para: e: un élément d'un list .  l:un list de type(string_builder*int) qui est reçu par fonction tranformer 
@retour : l'element(string_builder) succession de e dans ce list 
**)
let rec succ e l=match l with 
|(a1,l1)::(a2,l2)::q->if ((a1,l1)=e) then a2
else succ e ((a2,l2)::q)
|_->Vide;;



(**
fonction auxiliaire 2:min2  
@para: l：un list de type(string_builder*int) qui est reçu par fonction tranformer 
@retour : l'element(string_builder) succession de e dans ce list 
**)

let rec min2 l=match l with   
|[]->0
|e::[]->1000000
|(abr,len)::reste->min (cost(concat abr (succ (abr,len) l))) ( ( min2 reste ));;

(**
fonction auxiliaire 3:min_retirer  
@para: l：un list de type(string_builder*int) qui est reçu par fonction tranformer 
@retour : l'element qui permet de faire la concaténation avec son successeur a le coût le plus faible 
**)
let rec min_retirer l=match l with  
|[]->Vide
|(abr,len)::reste-> if ((cost(concat abr (succ (abr,len) l)))=min2 l) then fst (abr,len)
else  min_retirer reste;;

(**
fonction auxiliaire 4:concat2  
@para: a : un arbre qui est reçu de fonction min_retirer. l: une liste .l2: une liste pour conserver les éléments déjà parcouru ,l'initiation est [] 
@retour : une nouveau liste qui a contacté les deux arbre qui a le coût plus petit 
**)

let rec concat2 a l l2=if((List.length l)=1) then l else match l with
|[]->[]
|[e]->l
|(abr1,len1)::(abr2,len2)::q->
if(abr1=a) then  l2@((concat abr1 abr2),(len1+len2))::q
else (concat2 a ((abr2,len2)::q) (l2@[(abr1,len1)]));;




(**
fonction auxiliaire 5:minimiser  
@para: a : faire fonction concat2 jusqu'à il expose un seul élément
@retour : liste avec le seul élément  
**)
let rec minimiser l=match l with
|[]->[]
|[e]->[e]
|_->if (List.length l=1)then l
else
minimiser (concat2 (min_retirer l) l []);;
 
(**
fonction auxiliaire 6:prendre  
@para: a : une liste avec un seul élément
@retour : le seul élément 
**) 
let prendre l=match l with
|[e]->e
|_->failwith "il exsite plus de 1 element";;


(**
fonction balance :prendre en argument un string_builder et qui renvoie
un nouveau string_builder équilibré 
@para : abr: l'arbre initial 
@retour :un nouveau string_builder équilibré 
**)

let balance abr=fst (prendre (minimiser (transformer (list_of_string abr))));;

(**exéuction
let f =random_string 11 Vide;;
let g=list_of_string f;;
concat2 (min_retirer g) g [];;

**)

(**exécution **)balance f;;

(**exo8**)

(**
fonction auxiliaire 1: pertes :la fonction qui calcule les gains (ou les pertes) en coût de la fonction balance
@para: un arbre
@retour : le différence de coût entre arbre initial et arbre équilibré 
**)
let pertes abr=(cost abr)-(cost (niveau(balance abr) 0));;

pertes f;;
(**
fonction auxiliaire 2:gene_abr  
@para : ini:le compteur ,l'initiation est 0.  i:le nombre d'arbre qu'on veut.  l:une liste vide pour conserver les pertes des es arbres généré 
@retour : une liste des pertes des arbres généré de type int list 
**) 
let rec gene_abrs ini i l= 
		if(ini=i)then l
else  gene_abrs (ini+1) i ((pertes (random_string (ran_int()) Vide))::l);;

(**let l1=gene_abrs 0 5 [];;
**)
(**le plus petit perte **)
let rec min_pertes l=match l with
|[]->100000000
|e::q-> min e (min_pertes q );;

(**le plus grand perte **)
let rec max_pertes l=match l with
|[]->0
|e::q-> max e (max_pertes q );;

(**la somme des pertes**)
let rec somme l=match l with
|[]->0
|e::q->e+(somme q);;
(**la moyenne des pertes**)
let rec moyenne_pertes l=(somme l)/(List.length l);;
(**la index de  médiane des pertes**)
let mediane l=if(((List.length l) mod 2)=0) then (0,((List.length l)/2))
else (1,(((List.length l)-1)/2));;

(**
fonction auxiliaire 7:prendre_i  
@para : ini:le compteur ,l'initiation est 0.  i:le nombre d'arbre qu'on veut.  l:une liste vide pour conserver les pertes des es arbres généré 
@retour : le ième élément d'une liste de type int  
**)
let rec prendre_i  ini i l=match l with
|[]->0
|e::q->
 if(ini=i) then e
else prendre_i (ini+1) i q;;

(**la médiane des pertes**)
let medi_pertes l i=match i with
|(0,a)->((prendre_i 0 a l)+(prendre_i 0 (a-1) l))/2
|(1,a)->prendre_i 0 a l
|_->failwith "error";;
(**
medi_pertes l (mediane l);;
**)


(**
fonction résultat :calculer les gains (ou les pertes) en coût de la fonction balance
sur un grand nombre d’arbres générés aléatoirement.
La fonction peut, entre autre, renvoyer le min, le max, la moyenne et la valeur médiane.
@para: la liste des pertes des arbre généré 
@retour :une liste de type int :le premier élément :le plus petit perte ,2ème : le plus grand perte ,3ème: la moyenne ,4ème: la médiane 
**)
let resultat l=[(min_pertes l);(max_pertes l);(moyenne_pertes l);(medi_pertes l (mediane l))];; 
let l1=gene_abr 0 6 [];;
let l2=gene_abr 0 5 [];;
resultat l1;;
resultat l2;;
