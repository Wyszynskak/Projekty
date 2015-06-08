#' Wyszukiwanie informacji o filmach w bazie IMDb
#'
#' Funkcja \code{PobierzFilm} sciaga informacje o filmie z serisu IMDb
#'
#' @usage
#' \code{PobierzFilm(link)}
#'
#' @param link link do strony danego filmu w bazie IMDb 
#' (struktura linku jak w przykladzie)
#' @details \code{PobierzFilm} przeszukuje strone danego filmu w bazie IMDb 
#' szukajac przydatnych informacji o nim takich jak np. tytul, aktorzy, gatunek.
#'
#' @return Zwraca liste zlozona z ramek danych i wektorow przechowujacych 
#' informacje o filmie
#'
#'@author Krzysztof Rudas
#'
#' @examples
#' PobierzFilm("http://www.imdb.com/title/tt0395699/")
#'



PobierzFilm<-function(link){

   #Tworzymy funkcje pomocnicza czyszczaca napisy

    czysc<-function(napis){
      napis<-stri_replace_all_regex(napis,"\n","")
      napis<-stri_replace_all_regex(napis,"\t","")
      napis<-stri_replace_all_regex(napis,"\b","")
      napis<-stri_replace_all_regex(napis,"\r","")
      napis<-stri_replace_all_regex(napis,"\a","")
      return(napis)
    }

  #Zczytujemy strone
  html0 <- NULL
  try(html0<-html(link), silent = TRUE)
  if(is.null(html0)){print(paste0("Nie mozna odczytac strony: ", link)); return(NULL)}
  

   #Najpierw sprawdzamy czy film nie jest krotkometrazowy
   nodes<-html_nodes(html0,".infobar .itemprop")
   #Gatunki IMBD
   data_gatunki<-html_text(nodes)
   if(length(data_gatunki)==0)
   {
     data_gatunki<-""
   } else { k <- unlist(stri_match_all_regex(data_gatunki, "Short"))
            if(any(!is.na(k))){l<-0; print("Nie chcemy filmow krotkometrazowych."); return(NULL)}
   }

   #Tytuly polskie
   nodes<-html_nodes(html0,".header .itemprop")
   tytul_pol<-html_text(nodes)
   nodes<-html_nodes(html0,".title-extra")
   #Tytuly angielskie
   tytul_ang<-html_text(nodes)
   if(length(tytul_ang)==0)
   {
      tytul_ang<- tytul_pol
   }
   else
   {
      tytul_ang<-czysc(tytul_ang)
      tytul_ang<-unlist(stri_extract_all_regex(tytul_ang,"\".*\""))
      tytul_ang<-stri_replace_all_regex(tytul_ang,"\"","")
   }
   #Rok produkcji
   nodes<-html_nodes(html0,".header a")
   rok_produkcji<-html_text(nodes)
   if(length(rok_produkcji)==0)
   {
      rok_produkcji<-0
   }
   else
   {
      rok_produkcji<-as.numeric(rok_produkcji)
   }
   #Budzet
   nodes<-html_nodes(html0,"#titleDetails.article>div.txt-block")
   budzet<-html_text(nodes)
   budzet<-budzet[which(stri_detect_regex(budzet,"Budget"))]
   if(length(budzet)==0)
   {
      budzet<-0
   }
   else
   {
      budzet<-stri_replace_all_regex(budzet,",","")
      options(scipen=999)
      budzet<-as.numeric(unlist(stri_extract_all_regex(budzet,"[0-9]*[0-9]")))
   }
   #opis IMDB
   nodes<-html_nodes(html0,"#titleStoryLine p")
   opis_IMDB<-html_text(nodes)
   if(length(opis_IMDB)==0)
   {
      opis_IMDB<-""
   }
   else
   {
      opis_IMDB<-stri_replace_all_regex(opis_IMDB,"\n","")
      opis_IMDB<-stri_replace_all_regex(opis_IMDB,"Written by.*","")
      opis_IMDB<-czysc(opis_IMDB)
   }
   #Stworz ramke
   data_filmy<-data.frame(tytul_pol,tytul_ang,rok_produkcji,opis_IMDB,budzet)
  

   #Teraz oceny
   nodes<-html_nodes(html0,"strong span")
   
   #Ocena glowna
   ocena_IMDB<-as.numeric(html_text(nodes))
   if(length(ocena_IMDB)==0)
   {
      ocena_IMDB<--1
   }
   #Oceny plcio/wiekowe
   link_do_ocen<-stri_paste(link,"ratings")
    oceny <- NULL
   try( oceny<-html(link_do_ocen), silent =TRUE)
    if(is.null(oceny)){oceny1 <- NULL}else{
      oceny1<-html_nodes(oceny, "a+ table td~ td+ td")
      oceny1<-html_text(oceny1)[-1]  
      oceny1<-stri_replace_all_regex(oceny1,"Â","")
      oceny1<-as.numeric(unlist(stri_extract_all_regex(oceny1,"[0-9]?[0-9].[0-9]")))
      nazwy<-html_nodes(oceny, "a+ table td:nth-child(1)")
      nazwy<-html_text(nazwy)[-1]
      
      d<-data.frame(ocena_IMDB)
      #Trzeba zlokalizowac ktore sposrod ponizszych sa w statystykach
      wektornazw<-c("Males under 18","Females under 18",
                    "Males Aged 18-29",
                    "Females Aged 18-29",
                    "Males Aged 30-44",
                    "Females Aged 30-44",
                    "Males Aged 45+",
                    "Females Aged 45+")
      for(i in 1:8)
      {
        k<-which(stri_detect_regex(nazwy,wektornazw[i]))
        if(length(k)==0)
        {
          d<-cbind(d,-1)
        }
        else
        {
          d<-cbind(d,oceny1[k])
        }
      }
      #Nazwy
      colnames(d)=c("Ocena_IMBD","M_pon_18","K_pon_18",
                    "M_18-29","K_18-29","M_30-44","K_30-44","M_45+","K_45+")
      data_oceny<-d
    }

   
   #Czas na rezyserow
   html1<-NULL
   try(html1<-html(stri_paste(link,"fullcredits")))
   if(is.null(html1)){data_rez <- NULL; data_scen <- NULL}else{
   nodes<-html_nodes(html1,".simpleCreditsTable:nth-child(2) a")
   rezyser_IMDB<-html_text(nodes)
   rezyser_IMDB<-czysc(rezyser_IMDB)

   data_rez<-data.frame()

   if(length(rezyser_IMDB)!=0)
   {
      for(i in 1:length(rezyser_IMDB))
      {
         p<-unlist(stri_extract_all_words(rezyser_IMDB[i]))
         p<-data.frame(Imie=p[1],Nazwisko=p[length(p)])
         data_rez<-rbind(data_rez,p)
        
      }
   }
   else
   {
      data_rez<-rbind(data_rez,data.frame(Imie="",Nazwisko=""))
   }
 
   nodes<-html_nodes(html1,".simpleCreditsTable:nth-child(4) a")
   scenarzysta_IMDB<-html_text(nodes)
   scenarzysta_IMDB<-czysc(scenarzysta_IMDB)
   data_scen<-data.frame()
   if(length(scenarzysta_IMDB)!=0)
   {
      for(i in 1:length(scenarzysta_IMDB))
      {
         p<-unlist(stri_extract_all_words(scenarzysta_IMDB[i]))
         p<-data.frame(Imie=p[1],Nazwisko=p[length(p)])#Zabezpieczenie przez wieloma imionami
         data_scen<-rbind(data_scen,p)
         
      }
   }
   else
   {
      data_scen<-rbind(data_scen,data.frame(Imie="",Nazwisko=""))
   }
  
   
   }
   #Czas na producentow

   html2<-html(stri_paste(link,"companycredits"))
   nodes<-html_nodes(html2,"#production+ .simpleList a")
   data_producenci<-html_text(nodes)
   if(length(data_producenci)==0)
   {
      data_producenci<-""
   }
   data_producenci <- stri_replace_all_regex(data_producenci, '\"', "")

   #Kraje

   nodes<-html_nodes(html0,"#titleDetails.article>div.txt-block")
   data_kraj<-html_text(nodes)
   if(length(data_kraj)==0)
   {
      data_kraj<-""
   }
   else
   {
      data_kraj<-data_kraj[stri_detect_regex(data_kraj,"Country:")]
      data_kraj<-czysc(data_kraj)
      data_kraj<-stri_replace_all_regex(data_kraj,"Country:","")
      data_kraj<-unlist(strsplit(data_kraj,"\\|"))
      data_kraj<-unlist(stri_extract_all_regex(data_kraj,"[^ ].*[^ ]"))
   }


   #aktorzy lista glowna
   nodes<-html_nodes(html0,"#titleCast .itemprop")
   p1<-html_text(nodes)
   if(length(p1)!=0)
   {
      p1<-p1[1:(length(p1)/2)*2]
   }
   else
   {
      p1<-""
   }
   #gwiazdy
   nodes<-html_nodes(html0,"#overview-top .txt-block :nth-child(1)")
   t<-html_text(nodes)
   if(length(t)!=0)
   {
      t<-t[-length(t)]
      t1<-which(t=="Stars:")
      if(length(t1)!=0)
      {
         stars<-t[(t1+1):length(t)]
      }
      else
      {
         stars<-""
      }
     
   }
   else
   {
      stars<-""
   }
   #pelna lista
   nodes<-html_nodes(html1,".itemprop")
   aktorzy_IMDB<-html_text(nodes)
   if(length(aktorzy_IMDB)!=0)
   {
      aktorzy_IMDB<-aktorzy_IMDB[1:(length(aktorzy_IMDB)/2)*2]

      data_aktorzy<-data.frame()

      n<-length(aktorzy_IMDB)

      czy_star<-rep(FALSE,n)
      czy_gl<-rep(FALSE,n)

      for(i in 1:n)
      {
         czy_star[i]<-any(stars==aktorzy_IMDB[i])
         czy_gl[i]<-any(p1==aktorzy_IMDB[i])
         p3<-unlist(stri_extract_all_words(aktorzy_IMDB[i]))

         p3<-data.frame(Imie=p3[1],Nazwisko=p3[length(p3)])#Zabezpieczenie przez wieloma imionami
         data_aktorzy<-rbind(data_aktorzy,p3)
      }

      data_aktorzy<-cbind(data_aktorzy,Czy_gl=czy_gl,Czy_star=czy_star)
    
   }
   else
   {
      data_aktorzy<-data.frame(Imie="",Nazwisko="",Czy_gl=FALSE,Czy_star=FALSE)
   }



 l<-list(data_filmy,data_oceny,data_gatunki,data_rez,data_scen,
        data_producenci,data_kraj,data_aktorzy)

   return(l)

}