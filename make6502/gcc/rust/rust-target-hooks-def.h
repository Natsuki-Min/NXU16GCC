#ifndef TARGET_RUST_CPU_INFO
#define TARGET_RUST_CPU_INFO hook_void_void
#endif
#ifndef TARGET_RUST_OS_INFO
#define TARGET_RUST_OS_INFO hook_void_void
#endif

#define TARGETRUSTM_INITIALIZER \
  { \
    TARGET_RUST_CPU_INFO, \
    TARGET_RUST_OS_INFO, \
  }
