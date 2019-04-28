# webgrep.r
# Das Skript "grep"t die Notenstatistiken aus den von der HU unter 
# https://www.wiwi.hu-berlin.de/de/studium/pa/noten
# veröffentlichten Notenstatistiken und speichert sie schön in einem gängigen
# Datensatzformat.

# **************************************************************************** #

# TODO: - grep'ing dynmisch(er) machen, d.h. abhängig von der Position von
#         bestimmten Strings, werden andere lokalisiert
#       - Wenn es mehrere Prüfende gibt, ALLE extrahieren.
#       - Überprüfen, ob Download/Converting überhaupt funktioniert hat
#         (manchmal scheintcurl nämlich nicht zu gehen: "Error in HTTP2 layer")
#       - Codezeilenlänge/verbose messages
#       - Korrekter Wise/Sose String
#         
# **************************************************************************** #

# Packages & Functions
# --------------------
# Hier werden benötigte Pakete geladen.

library("tidyverse")

library("pdftools")
library("curl")

"%p%" = function(lhs, rhs) paste0(lhs, rhs)

# Read URLS
# ----------------
# Einlesen der URLS, hinter denen die Notenstatistiken hinterlegt sind.

urls = "urls.txt" %>% 
    read_lines()


# Grep
# ----
# df0 ist der Datensatz, in dem die Daten gesammelt werden. In der Schleife wird
# dieser dann mit den entsprechend ge-"grep"'ten Notenstatistiken befüllt. 

df0  = tibble()

for(i in seq(1, length(urls), 1))
{
    "\n[" %p% str_pad(i, str_length(length(urls)), "left", " ") %p% "] Trying to fetch ~" %p% str_sub(pluck(urls, i), -32) %p% " …" %>%
        message()

    # Mit curl werden die PDFs heruntergeladen und als "noten.pdf".
    urls %>%
        pluck(i) %>% 
        curl::curl_download("noten.pdf")
    
    # Gab es Probleme?
    #if()
    #message("[", i, "] Failed ✗\nContinuing with ", i + 1, "...\n\n") 
    #next
       
    "[" %p% str_pad(i, str_length(length(urls)), "left", " ") %p% "] Finished ✓" %>%
        message()
       
    # Die PDF wird in eine Textdatei umgewandelt.
    x = "noten.pdf" %>% 
        pdftools::pdf_text()
    
    "[" %p% str_pad(i, str_length(length(urls)), "left", " ") %p% "] Converting PDF to ASCII text …" %>%
        message()
    
    # Gab es Probleme?
    #if()
    #message("[", i, "] Failed ✗\nContinuing with ", i + 1, "...\n\n") 
    #next
    
    "[" %p% str_pad(i, str_length(length(urls)), "left", " ") %p% "] Converted ✓" %>%
        message()
    
        
    # Die PDF ist jetzt in einem sehr unsauberen String gespeichert. Daher wird
    # im ersten Schritt der String Zeilenweise aufgesplittet und die ganzen
    # trailing whitespaces enfernt.
    x = x %>%
        str_split("\n") %>%
        pluck(1) %>%
        str_squish() %>%
        enframe()
    
    # Die umgewandelten PDFs haben Gemeinsamkeiten. Wir wissen:
    # - Es gibt ein EOF Symbol in der letzten Zeile.
    # - In den drei Zeilen vor der letzten Zeile sind Durchschnitt, Median und
    #   Durchfallquote.
    # - In der fünftletzten Zeile stehen die Teilnehmer·innenzahlen der Prüfung.
    #   Diese Zeile beginnt immer mit "Summe:".
    # - Der Semesterstring ist in der ersten Zeile.
    # - In Zeile 5 und 6 sind Prüfungsnummer bzw. Prüfungsname.
    # - In Zeile 9 stehen die Noten.
    # - Stand der Veröffentlichung ist in Zeile 2.
    # - In Zeile 10 steht, ob es sich um Erst- oder Zweittermin handelt.
    # - In Zeile 10 steht außerdem der Namen der Prüfenden.
   
    y = tibble(    
            id             = i %>% rep(11),
            note           = x %>% slice(9) %>% pull(value),
            teiln          = x %>% slice(nrow(x) - 4) %>% pull(value),
            modulid        = x %>% slice(5) %>% pull(value),
            modul          = x %>% slice(6) %>% pull(value),
            pruefende      = x %>% slice(10) %>% pull(value),
            termin         = x %>% slice(10) %>% pull(value),
            stand          = x %>% slice(2) %>% pull(value),
            semester       = x %>% slice(1) %>% pull(value),
            ngrepd         = x %>% slice(10) %>% pull(value),
            avggrepd       = x %>% slice(nrow(x) - 3) %>% pull(value),
            medgrepd       = x %>% slice(nrow(x) - 2) %>% pull(value),
            failratiogrepd = x %>% slice(nrow(x) - 1) %>% pull(value)
        )
        
    # Die raw Strings müssen jetzt aber noch aufgehübscht werden.
    y = y %>%
        mutate(
            # Der note-String sind schon relativ clean. Es muss nur der String
            # an den Blanks gesplittet werden und Kommas durch Punkte ersetzt
            # werden, damit der gewünschte Datentyp (numeric) möglich ist. 
            note = note %>% 
                str_split(" ") %>% 
                pluck(1) %>% 
                str_replace(",", ".") %>% 
                as.numeric(),
            # Der teiln-String ist auch relativ clean. Wieder wird der String 
            # an den Blanks gesplittet und missing values ("-") durch "0" 
            # ersetzt werden. Da es 11 mögliche Noten gibt und die an letzter
            # Stelle stehen, werden die letzten 10 Elemente extrahiert und 
            # die numerischen Werte auch in numeric umgewandelt.
            teiln = teiln %>%
                str_split(" ") %>%
                pluck(1) %>%
                str_replace("-", "0") %>%
                tail(11) %>%
                as.numeric(),
            # Splitte den modulid-String an "Prüfungsnummer: " in einen leeren
            # String und einen String, der die Modul-ID enthält.
            modulid = modulid %>%
                str_split("Prüfungsnummer: ") %>%
                pluck(1, 2),
            # Der modul-String geht, mutate mutandis, wie der  modulid-String. 
            modul = modul %>%
                str_split("Prüfung: ") %>%
                pluck(1, 2),
            # Die Prüfenden stehen in Zeile 10 an zweiter Stelle (nach dem 
            # Termin). D.h. es wird wieder der String an den Blanks gesplittet
            # und das zweite Element ausgewählt.
            pruefende = pruefende %>%
                str_split(" ") %>%
                pluck(1, 2),
            # Für den termin-String wird wie beim pruefende-String zurerst
            # gesplittet und dann aber das erste Element gewählt und in numeric
            # umgewandelt.
            termin = termin %>%
                str_split(" ") %>%
                pluck(1, 1) %>%
                as.numeric(),
            # Splitte den stand-String an "Stand: " in einen String, der den 
            # Namen der Fakultät enthält und einen mit dem Stand der
            # Veröffentlichung. Wähle diesen aus.
            stand = stand %>%
                str_split("Stand: ") %>%
                pluck(1, 2),
            # Der semester-String wird an "semester" in zwei Strings gesplittet.
            # Der zweite enthält das Semester. Kommt ein "/" im String vor, ist
            # es ein Wintersemester, wenn nicht ein Sommersemester. Das wird
            # ausgenutzt.
            semester = semester %>%
                str_split("semester ") %>%
                pluck(1, 2), #%>%
            #    case_when(
            #        str_detect(., "/") == 1 ~ "WiSe" %p% .,
            #        str_detect(., "/") != 1 ~ "SoSe" %p% .
            #     ),
             # Der ngrepd-String wird wie der pruefende- und der termin-String
             # behandelt. Die Summe steht an zwölfletzter Stelle.
             ngrepd = ngrepd %>%
                str_split(" ") %>%
                pluck(1) %>%
                tail(12) %>%
                pluck(1) %>%
                as.numeric(),
            # Der avggrepd-String wird an den Blanks gesplittet. Der zweite
            # enthält dann den im Dokument angegebenen Durchschnitt. Wähle
            # diesen aus und ersetze noch "," durch "." und passe den Datentyp
            # an.
            avggrepd = avggrepd %>%
                str_split(" ") %>%
                pluck(1, 2) %>%
                str_replace(",", ".") %>%
                as.numeric(),
            # Der medgrepd-String funktioniert, mutatis mutandis wie der 
            # avggrepd-String.
            medgrepd = medgrepd %>%
                str_split(" ") %>%
                pluck(1, 2) %>%
                str_replace(",", ".") %>%
                as.numeric(),
            # Der failratiogrepd-String wird an den Blanks gesplitet. Der zweite
            # enthält dann die angegebene Durchfallquote. Entferne ggf. ein "%"
            # und ersetzt "," durch "." und transformiere den Datentyp in
            # numeric.
            failratiogrepd = failratiogrepd %>%
                str_split(" ") %>%
                pluck(1, 2) %>%
                str_replace("%", "") %>%
                str_replace(",", ".") %>%
                as.numeric()/100
        )
    
    # verbose...
    "[" %p% str_pad(i, str_length(length(urls)), "left", " ") %p% "] Entry:\n" %p% paste(paste(rep(" ", str_length(length(urls)) + 3), collapse = ""), collapse = "") %p% pluck(pull(y, modul), 1) %p% " (" %p% pluck(pull(y, modulid), 1) %p% ")\n" %p% paste(rep(" ", str_length(length(urls)) + 3), collapse = "") %p% "Prüfer·in: " %p% pluck(pull(y, pruefende), 1) %p% " (" %p% pluck(pull(y, semester), 1) %p% ")\n" %p% paste(rep(" ", str_length(length(urls)) + 3), collapse = "") %p% rep(" ", 4) %p% paste(str_pad(pull(y, note), 3, "left", " "), collapse = " ") %p% "\n" %p% paste(rep(" ", str_length(length(urls)) + 3), collapse = "") %p% rep(" ", 4) %p% paste(str_pad(pull(y, teiln), 3, "left", " "), collapse = " ") %p% "\n" %p% paste(rep(" ", str_length(length(urls)) + 3), collapse = "") %p% "Durchschnitt/Median/Durchfallquote\n" %p% paste(rep(" ", str_length(length(urls)) + 3), collapse = "") %p% pluck(pull(y, avggrepd), 1) %p% "/" %p% pluck(pull(y, medgrepd), 1) %p% "/" %p% (pluck(pull(y, failratiogrepd), 1) * 100) %p% " %\n" %p% paste(rep(" ", str_length(length(urls)) + 3), collapse = "") %p% "Eingetragen am: " %p% pluck(pull(y, stand), 1) %p% "\n" %>%
        pluck(1) %>%
        message() 
    
    #Sys.sleep(1)
    
    # Füge die extrahierten Werte zum Datensatz hinzu.
    df0 = df0 %>% 
        bind_rows(y)
}

# Save Data
# ---------
# Die Daten im Wide Format werden abschließend als csv gespeichert.

df0 %>%    
    spread(note, teiln) %>% 
    write_csv("notenstats.csv")

# **************************************************************************** #

# eof
