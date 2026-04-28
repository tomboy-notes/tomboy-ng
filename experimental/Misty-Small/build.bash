#!/usr/bin/bash

rm *.o *.ppu
#  /home/dbannon/bin/FPC/fpc-3.2.3/bin/fpc -MObjFPC -Scaghi -Cg -CirotR -O1 -gw3 -gl -gh -gt -l -vewnhibq -Fu/home/dbannon/Pascal/tomboy-ng/experimental/Misty-Small/ -Fu../../../../bin/Lazarus/lazarus_4_6/lcl/units/x86_64-linux -Fu../../../../bin/Lazarus/lazarus_4_6/components/freetype/lib/x86_64-linux -Fu../../../../bin/Lazarus/lazarus_4_6/components/lazutils/lib/x86_64-linux -Fu../../../../bin/Lazarus/lazarus_4_6/packager/units/x86_64-linux -omisty-server -dLCL -dLCLgtk2 -dMISTY_SMALL webserver.lpr

# /home/dbannon/bin/FPC/fpc-3.2.3/bin/fpc -MObjFPC -Scaghi -Cg -CirotR -O1 -gw3 -gl -gh -gt -l -vewnhibq -Fu/home/dbannon/Pascal/tomboy-ng/experimental/Misty-Small/ -Fu../../../../bin/Lazarus/lazarus_4_6/lcl/units/x86_64-linux -Fu../../../../bin/Lazarus/lazarus_4_6/components/lazutils/lib/x86_64-linux -omisty-server -dLCL -dLCLgtk2 -dMISTY_SMALL webserver.lpr
# -Fu../../../../bin/Lazarus/lazarus_4_6/lcl/units/x86_64-linux

/home/dbannon/bin/FPC/fpc-3.2.3/bin/fpc -MObjFPC -Scaghi -Cg -CirotR -O1 -gw3 -gl -gh -gt -l -vewnhibq -Fu/home/dbannon/Pascal/tomboy-ng/experimental/Misty-Small/ -FuLazFiles -omisty-server -dLCL -dLCLgtk2 -dMISTY_SMALL webserver.lpr
