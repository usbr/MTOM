cbrfcUrl = "https://www.cbrfc.noaa.gov/outgoing/ucbor/"

Dim fso: Set fso = CreateObject("Scripting.FileSystemObject")
Dim downloadLocation
downloadLocation = fso.GetParentFolderName(Wscript.ScriptFullName) & "\"

Set fso = CreateObject ("Scripting.FileSystemObject")
Set stdout = fso.GetStandardStream (1)
stdout.WriteLine "Downloading files to " & downloadLocation

GetCbrfcFile cbrfcUrl,downloadLocation,"BMDC2.espmvol.5yr.adj.csv"
GetCbrfcFile cbrfcUrl,downloadLocation,"CLSC2.espmvol.5yr.adj.csv"
GetCbrfcFile cbrfcUrl,downloadLocation,"DRGC2.espmvol.5yr.adj.csv"
GetCbrfcFile cbrfcUrl,downloadLocation,"GBRW4.espmvol.5yr.adj.csv"
GetCbrfcFile cbrfcUrl,downloadLocation,"GJNC2LOC.espmvol.5yr.adj.csv"
GetCbrfcFile cbrfcUrl,downloadLocation,"GLDA3.espmvol.5yr.adj.csv"
GetCbrfcFile cbrfcUrl,downloadLocation,"GRNU1.espmvol.5yr.adj.csv"
GetCbrfcFile cbrfcUrl,downloadLocation,"MPSC2.espmvol.5yr.adj.csv"
GetCbrfcFile cbrfcUrl,downloadLocation,"NVRN5.espmvol.5yr.adj.csv"
GetCbrfcFile cbrfcUrl,downloadLocation,"TPIC2.espmvol.5yr.adj.csv"
GetCbrfcFile cbrfcUrl,downloadLocation,"VCRC2.espmvol.5yr.adj.csv"
GetCbrfcFile cbrfcUrl,downloadLocation,"YDLC2.espmvol.5yr.adj.csv"
GetCbrfcFile cbrfcUrl,downloadLocation,"README.txt"

Function GetCbrfcFile(cbrfcUrl,downloadLocation,fileName)
	Set fso = CreateObject ("Scripting.FileSystemObject")
	Set stdout = fso.GetStandardStream (1)
	stdout.WriteLine "     " & fileName
	dim xHttp: Set xHttp = createobject("Msxml2.XMLHttp.6.0")
	dim bStrm: Set bStrm = createobject("Adodb.Stream")
	downloadUrl = cbrfcUrl & fileName
	downloadText = downloadLocation & fileName
	xHttp.Open "GET", downloadUrl, False
	xHttp.Send
	
	with bStrm
		.type = 1 '//binary
		.open
		.write xHttp.responseBody
		.savetofile downloadText, 2 '//overwrite
	end with
End Function