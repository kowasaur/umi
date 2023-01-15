# Build and install to /usr/local/bin
mcs umi.cs -out:umi
mv umi /usr/local/bin/umi
cp -TR std /usr/local/bin/std
