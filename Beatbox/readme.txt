Aplikacja beatbox.py pozwala na posk³adanie utworu
stworzonego z sampli za pomoc¹ zadanej kolejnoœci ich odgrywania w plikach trackXX.txt
oraz struktury utworu zawartej w song.txt. Tempo piosenki zosta³o okreœlone w pliku defs.txt.
Wywo³anie skryptu odbywa siê przy pomoy¹ komendy './beatbox.py' do której dodajemy odpowiedni argument:
	- nazwa utworu jesli jest on zawarty w folderze
	- nazwa utworu wraz z rozszerzeniem jeœli jest to plik skompresowany
PRZYK£AD
'./beatbox.py utwor1'
'./beatbox.py utwor2.zip'

Aplikacja wykorzystuje pakiet soundCreator.