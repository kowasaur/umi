# Build and install to /usr/local/bin
mcs umi.cs -out:umi
mv umi /usr/local/bin/umi
cp -R std /usr/local/bin/std
