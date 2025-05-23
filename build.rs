fn main() {
    let parser = "rust";

    if parser == "haskell" {
        use std::process::Command;

        let subfolder = "lib";
        let haskell_file = "ParserFFI.hs";
        let c_file = "parser_api.c";
        let haskell_library = "amethyst_parser";
        let c_api_library = "amethyst_parser_api";

        // Erases last build and prepares the lib folder for the shared objects
        Command::new("rm").args(&["-r", "./lib"]).status().unwrap();
        Command::new("mkdir").args(&[subfolder]).status().unwrap();

        // Creates the Haskell shared library which we call from C
        Command::new("ghc")
            .args(&[
                &format!("./src/{}", haskell_file),
                "-dynamic",
                "-shared",
                "-fPIC",
                "-isrc",
                "-no-keep-hi-files",
                "-no-keep-o-files",
                "-stubdir ./clean",
                "-o",
                &format!("./{}/lib{}.so", subfolder, haskell_library),
            ])
            .status()
            .expect("Unable to create hs shared object");

        // Creates the C API library
        Command::new("gcc")
            .args(&[
                &format!("./src/{}", c_file),
                "-shared",
                // Paths for haskell-c linking
                "-I/usr/lib/ghc/include",
                "-L/usr/lib/ghc/rts",
                "-Wl,-rpath,/usr/lib/ghc/rts",
                // Paths for running from top of the project
                &format!("-L./{}", subfolder),
                &format!("-Wl,-rpath,./{}", subfolder),
                // Paths for running from ./src
                &format!("-L./../{}", subfolder),
                &format!("-Wl,-rpath,./../{}", subfolder),
                // Paths for running from ./subfolder folder
                "-L.",
                "-Wl,-rpath,.",
                "-lHSrts-ghc8.8.4",
                &format!("-l{}", haskell_library),
                "-o",
                &format!("./{}/lib{}.so", subfolder, c_api_library),
            ])
            .status()
            .expect("Unable to create c shared object");

        // Delete the useless files
        Command::new("rm")
            .args(&["-r", "./clean"])
            .status()
            .unwrap();

        // Alternative to "export LD_LIBRARY_PATH=.:./src"
        println!(
            "{}",
            format!(
                "cargo:rustc-link-arg=-Wl,-rpath,$ORIGIN/../../{}",
                subfolder
            )
        );
        // Also the relative path from the tests directory
        println!(
            "{}",
            format!(
                "cargo:rustc-link-arg=-Wl,-rpath,$ORIGIN/../../../{}",
                subfolder
            )
        );
        // Specify the libraries' location
        println!(
            "{}",
            format!("cargo:rustc-link-search=native=./{}/", subfolder)
        );
        // Specify all the shared libraries
        println!(
            "{}",
            format!("cargo:rustc-link-lib=dylib={}", haskell_library)
        );
        println!(
            "{}",
            format!("cargo:rustc-link-lib=dylib={}", c_api_library)
        );
    }
}
