#include "HsFFI.h"
#include "Rts.h"

int main(int argc, char* argv[]) {
  RtsConfig config = defaultRtsConfig;

  // CAFs in the main executable must be retained, because we
  // don't know whether they will be used by a future DLL that we
  // load.  If we don't do this, the likely result is a crash
  // in the GC at a random time after loading a shared object.
  config.keep_cafs = 1;

  config.rts_opts_enabled = RtsOptsAll;

  extern StgClosure ZCMain_main_closure;
  hs_main(argc, argv, &ZCMain_main_closure, config);
}
