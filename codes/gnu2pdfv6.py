#-*-coding=utf8-*-
#!/use/bin/env python

import os
import sys
import re
import codecs
import subprocess

def Clean( perfix ):
    for i in ["log", "tex", "aux", "rec"]:
        if (os.path.exists(perfix + i)):
            os.remove(perfix+i)

def GenerateTikzTexFile( gnuplotfile ):
    gnuplotfilecontent = "".join(codecs.open( gnuplotfile, 'r', 'utf8').readlines())
    texflist = re.findall('\w*?\.tex', re.sub(re.compile('#.*'),'',gnuplotfilecontent))
    subprocess.call(['gnuplot', gnuplotfile], shell=False)
    return texflist

def GeneratePDFFile( texflist ):
    for tex in texflist:
        oritex_l = codecs.open(tex, 'r', 'utf-8').readlines()
        oritex_l[0] = "\\documentclass[10pt,UTF8]{ctexart}\n"
        oritex_l[4] = ""

        oritex_l[6] = "\\usepackage{gnuplot-lua-tikz}\n\\usetikzlibrary{positioning,fit}\n"

        tex_f = codecs.open( tex, 'w', 'utf-8' )
        for i in oritex_l:
            tex_f.write( i ) 
        tex_f.close()

        subprocess.call(['pdflatex', '-interaction=nonstopmode', tex, ">", tex[0:-3]+"rec"], shell=False)
        subprocess.call(['pdftops', '-eps', tex[0:-3]+"pdf"], shell=False)
        #subprocess.call(['f:\\Program Files\\gs\\gs9.19\\bin\\gswin64c.exe', '-dSAFER', '-dBATCH', '-dNOPAUSE', '-sDEVICE=pngalpha', '-sOutputFile='+tex[0:-3]+"png", '-r300', tex[0:-3]+"pdf"], shell=False)
        #subprocess.call(['f:\\Program Files\\gs\\gs9.19\\bin\\gswin64c.exe', '-dSAFER', '-dBATCH', '-dNOPAUSE', '-sDEVICE=jpeg', '-dJPEGQ=100', '-sOutputFile='+tex[0:-3]+"jpg", '-r600', tex[0:-3]+"pdf"], shell=False)
        Clean(tex[0:-3])



if __name__ == '__main__':
    gnuplotfile = sys.argv[ len(sys.argv) - 1 ]
    des_dir = os.path.split( gnuplotfile )[0]
    des_file = os.path.split( gnuplotfile )[-1]
    if len( des_dir ) == 0:
        des_dir = "."
    os.chdir( des_dir )
    GeneratePDFFile(GenerateTikzTexFile(gnuplotfile))
    

