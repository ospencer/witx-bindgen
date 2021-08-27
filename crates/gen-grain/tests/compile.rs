use std::env;
use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    println!("compiled successfully!")
}

mod imports {
    test_codegen::grain_import!(
        // ...
        "*.wit"

        // TODO: these use push/pull buffer in exports which isn't implemented
        // yet
        "!wasi_next.wit"
        "!host.wit"
        "!conventions.wit"
    );
}

mod exports {
    test_codegen::grain_export!(
        "!*.wit"

        // These use preview1 ABI things which are only supported for imports
        "!wasi_snapshot_preview1.wit"

        // TODO: these use push/pull buffer in exports which isn't implemented
        // yet
        "!wasi_next.wit"
        "!host.wit"
    );
}

fn verify(dir: &str, name: &str) {
    let dir = Path::new(dir);
    let mut cmd = Command::new("grain");
    cmd.arg("compile");
    cmd.arg(dir.join("bindings.gr"));

    println!("{:?}", cmd);
    let output = match cmd.output() {
        Ok(output) => output,
        Err(e) => panic!("failed to spawn compiler: {}", e),
    };

    if output.status.success() {
        return;
    }
    println!("status: {}", output.status);
    println!("stdout: ------------------------------------------");
    println!("{}", String::from_utf8_lossy(&output.stdout));
    println!("stderr: ------------------------------------------");
    println!("{}", String::from_utf8_lossy(&output.stderr));
    panic!("failed to compile");
}
