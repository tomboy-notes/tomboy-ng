#!/usr/bin/bash

rm *.o *.ppu
#  /home/dbannon/bin/FPC/fpc-3.2.3/bin/fpc -MObjFPC -Scaghi -Cg -CirotR -O1 -gw3 -gl -gh -gt -l -vewnhibq -Fu/home/dbannon/Pascal/tomboy-ng/experimental/Misty-Small/ -Fu../../../../bin/Lazarus/lazarus_4_6/lcl/units/x86_64-linux -Fu../../../../bin/Lazarus/lazarus_4_6/components/freetype/lib/x86_64-linux -Fu../../../../bin/Lazarus/lazarus_4_6/components/lazutils/lib/x86_64-linux -Fu../../../../bin/Lazarus/lazarus_4_6/packager/units/x86_64-linux -omisty-server -dLCL -dLCLgtk2 -dMISTY_SMALL webserver.lpr

# /home/dbannon/bin/FPC/fpc-3.2.3/bin/fpc -MObjFPC -Scaghi -Cg -CirotR -O1 -gw3 -gl -gh -gt -l -vewnhibq -Fu/home/dbannon/Pascal/tomboy-ng/experimental/Misty-Small/ -Fu../../../../bin/Lazarus/lazarus_4_6/lcl/units/x86_64-linux -Fu../../../../bin/Lazarus/lazarus_4_6/components/lazutils/lib/x86_64-linux -omisty-server -dLCL -dLCLgtk2 -dMISTY_SMALL webserver.lpr
# -Fu../../../../bin/Lazarus/lazarus_4_6/lcl/units/x86_64-linux

# fpc -vabt -Parm -MObjFPC -Scaghi -Cg -CirotR -O1 -gw3 -gl -gh -gt -l -vewnhibq -Fu/home/dbannon/Pascal/tomboy-ng/experimental/Misty-Small/ -Fu/home/dbannon/bin/Lazarus/lazarus-main/components/lazutils/lib/arm-linux/ -omisty-server-arm -dLCL -dLCLgtk2 -dMISTY_SMALL webserver.lpr

# armhf (32bit Pi)
fpc -Parm -MObjFPC -Scaghi -Cg -CirotR -O1 -gw3 -gl -gh -gt -l -vewnhibq -Fu/home/dbannon/Pascal/tomboy-ng/experimental/Misty-Small/ -Fu/home/dbannon/bin/Lazarus/lazarus-main/components/lazutils/lib/arm-linux/ -Fl/usr/lib/gcc-cross/arm-linux-gnueabihf/14/ -Fl/usr/arm-linux-gnueabihf/lib/  -omisty-server-arm -dLCL  -dMISTY_SMALL webserver.lpr

# These can be added to the -Fl box, Lazarus, Proj Opts, Paths (with semicolon between)
# -Fl/usr/lib/gcc-cross/arm-linux-gnueabihf/14/     libgcc-14-dev-armhf-cross
# -Fl/usr/arm-linux-gnueabihf/lib/                  libc6-dev-armhf-cross
# greybox gcc-12
# hp-07 gcc-15
# u2004 gcc-9
