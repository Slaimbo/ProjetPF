all: lzwCompressArbre.exe lzwCompressDico.exe lzwDecompressDico.exe lzwDecompressArbre.exe
	rm -f *.hi *.o

lzwCompressArbre.exe: lzwCompressArbre.hs
	ghc -o lzwCompressArbre.exe lzwCompressArbre.hs

lzwCompressDico.exe: lzwCompressDico.hs
	ghc -o lzwCompressDico.exe lzwCompressDico.hs

lzwDecompressDico.exe: lzwDecompressDico.hs
	ghc -o lzwDecompressDico.exe lzwDecompressDico.hs

lzwDecompressArbre.exe: lzwDecompressArbre.hs
	ghc -o lzwDecompressArbre.exe lzwDecompressArbre.hs

clean:
	rm -f *.exe *.hi *.o *.decomp *.comp
