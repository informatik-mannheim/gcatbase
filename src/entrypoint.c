// We need to forward routine registration from C to Rust
// to avoid the linker removing the static library.

// gcatrustr like in R package name.
// If name is different, compiling fails.
void R_init_gcatbase_extendr(void *dll);

void R_init_gcatbase(void *dll) {
    R_init_gcatbase_extendr(dll);
}
