import urllib.request
import io
from bs4 import BeautifulSoup

url = "http://www.menneske.no/hitori/12x12/eng/"
filename = "Puzzle.txt"
filenameSolution = "Solution.txt"

page = urllib.request.urlopen(url)
soup = BeautifulSoup(page, "html.parser")
text = open(filename, "w")

x = soup.find_all("tr")
solution = soup.find_all("a")

for i in range(0,len(x)):
    hitori = x[i].find_all('td' ,{"class" : "white"})
    for t in hitori:
        text.write(t.contents[0])
        text.write(" ")
        #print(str(str(t.contents[0])))
    text.write("\n")
text.close()

for link in solution:
    if "solution" in link.text:
        print(link.get("href"))
        url += link.get("href")

page = urllib.request.urlopen(url)
soup = BeautifulSoup(page, "html.parser")
text = open(filenameSolution, "w")

x = soup.find_all("tr")
solution = soup.find_all("a")

for i in range(0,len(x)):
    hitori = x[i].find_all('td')
    for h in hitori:
        if h.get("class") != None:
            if "white" in h.get("class"):
                text.write("W")
                text.write(" ")
            else:
                text.write("B")
                text.write(" ")
    text.write("\n")
text.write(url)
text.close()





