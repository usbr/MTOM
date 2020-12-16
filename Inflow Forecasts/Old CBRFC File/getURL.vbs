cbrfcUrl = "https://www.cbrfc.noaa.gov/outgoing/32month/"

Dim fso: Set fso = CreateObject("Scripting.FileSystemObject")
Dim downloadLocation
downloadLocation = fso.GetParentFolderName(Wscript.ScriptFullName) & "\"

Set fso = CreateObject ("Scripting.FileSystemObject")
Set stdout = fso.GetStandardStream (1)
stdout.WriteLine "Downloading files to " & downloadLocation

GetCbrfcFile cbrfcUrl,downloadLocation,"BMDC2.espmvol.txt"
GetCbrfcFile cbrfcUrl,downloadLocation,"CLSC2.espmvol.txt"
GetCbrfcFile cbrfcUrl,downloadLocation,"DLAC2.espmvol.txt"
GetCbrfcFile cbrfcUrl,downloadLocation,"DRGC2.espmvol.txt"
GetCbrfcFile cbrfcUrl,downloadLocation,"GBRW4.espmvol.txt"
GetCbrfcFile cbrfcUrl,downloadLocation,"GJLOC.espmvol.txt"
GetCbrfcFile cbrfcUrl,downloadLocation,"GLDA3.espmvol.txt"
GetCbrfcFile cbrfcUrl,downloadLocation,"GRNU1.espmvol.txt"
GetCbrfcFile cbrfcUrl,downloadLocation,"LEMC2.espmvol.txt"
GetCbrfcFile cbrfcUrl,downloadLocation,"LNAC2.espmvol.txt"
GetCbrfcFile cbrfcUrl,downloadLocation,"MPSC2.espmvol.txt"
GetCbrfcFile cbrfcUrl,downloadLocation,"NAVC2.espmvol.txt"
GetCbrfcFile cbrfcUrl,downloadLocation,"NVLOC.espmvol.txt"
GetCbrfcFile cbrfcUrl,downloadLocation,"NVRN5.espmvol.txt"
GetCbrfcFile cbrfcUrl,downloadLocation,"RBPC2.espmvol.txt"
GetCbrfcFile cbrfcUrl,downloadLocation,"SOMC2.espmvol.txt"
GetCbrfcFile cbrfcUrl,downloadLocation,"TPIC2.espmvol.txt"
GetCbrfcFile cbrfcUrl,downloadLocation,"VCRC2.espmvol.txt"
GetCbrfcFile cbrfcUrl,downloadLocation,"YDLC2.espmvol.txt"
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