Aplikacja beatbox.py pozwala na posk�adanie utworu
stworzonego z sampli za pomoc� zadanej kolejno�ci ich odgrywania w plikach trackXX.txt
oraz struktury utworu zawartej w song.txt. Tempo poisenki zosta�o okre�lone w pliku defs.txt.
Wywo�anie skryptu odbywa sie za pomoc� komendy './beatbox.py' do ktorej dodajemy odpowiedni argument:
	- nazwa utworu jesli jest on zawarty w folderze
	- nazwa utworu wraz z rozszerzeniem je�li jest to plik skompresowany
PRZYK�AD
'./beatbox.py utwor1'
'./beatbox.py utwor2.zip'

Aplikacja wykorzystuje pakiet soundCreator.