use anyhow::{Context, Result};
use clap::Parser;
use is_executable::IsExecutable;
use std::path::{Path, PathBuf};
use tempfile::NamedTempFile;
use wasm_shrink::{IsInteresting, WasmShrink};

/// Shrink a Wasm file while maintaining a property of interest (such as triggering
/// a compiler bug).
///
/// ## Example
///
/// Given that
///
///  * `crasher.wasm` is a Wasm file that triggers a crash in your Wasm compiler
///    when you try to compile it, and
///
///  * `compile.sh` is a script that exits with code zero if its first
///    command-line argument triggers a crash in your Wasm compiler, and exits
///    with a non-zero code otherwise.
///
/// then you can shrink `crasher.wasm` into a smaller Wasm file that still triggers
/// the crash and save it at `shrunken.wasm` with the following command:
///
/// $ wasm-shrink compile.sh crasher.wasm -o shrunken.wasm
#[derive(Parser)]
pub struct Opts {
    #[clap(flatten)]
    shrink: WasmShrink,

    /// The interestingness predicate script.
    predicate: PathBuf,

    #[clap(flatten)]
    io: wasm_tools::InputOutput,
}

impl Opts {
    pub fn general_opts(&self) -> &wasm_tools::GeneralOpts {
        self.io.general_opts()
    }

    pub fn run(self) -> Result<()> {
        let input = self.io.parse_input_wasm()?;
        let initial_size = input.len();

        // Prerequisites for the predicate.
        anyhow::ensure!(
            self.predicate.is_file(),
            "The predicate script '{}' does not exist.",
            self.predicate.display()
        );
        anyhow::ensure!(
            self.predicate.is_executable(),
            "The predicate script '{}' is not executable.",
            self.predicate.display()
        );

        let output = self
            .io
            .output_path()
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| match self.io.input_path() {
                Some(path) => path.with_extension("shrunken.wasm"),
                None => "shrunken.wasm".into(),
            });
        log::info!("Will write shrunken Wasm file to: {}", output.display());

        let predicate = make_predicate(&self.predicate);

        let shrunken = self
            .shrink
            .on_new_smallest(Some(Box::new({
                let output = output.clone();
                move |new_smallest: &[u8]| {
                    // Write the Wasm to a temp file and then move that to the output
                    // path as a second, atomic step. This ensures that the output is
                    // always a valid, interesting, shrunken Wasm file, even in the
                    // presence of the user doing `Ctrl-C`.
                    //
                    // Note that to have the highest likelihood of the rename to succeed
                    // the temporary file is placed in the same directory as the
                    // destination. This attempts to avoid possibilities where the
                    // system tmp directory is not on the same filesystem as the
                    // destination, which would prevent a rename.
                    let tmp = match output.parent() {
                        Some(parent) => NamedTempFile::new_in(parent),
                        None => NamedTempFile::new(),
                    };
                    let tmp = tmp.context("Failed to create a temporary file")?;
                    std::fs::write(tmp.path(), new_smallest).with_context(|| {
                        format!("Failed to write to file: {}", tmp.path().display())
                    })?;
                    std::fs::rename(tmp.path(), &output).with_context(|| {
                        format!(
                            "Failed to rename {} to {}",
                            tmp.path().display(),
                            output.display()
                        )
                    })?;

                    println!(
                        "{} bytes ({:.02}% smaller)",
                        new_smallest.len(),
                        (100.0 - (new_smallest.len() as f64 / initial_size as f64 * 100.0))
                    );

                    // Now write the WAT disassembly as well.
                    match wasmprinter::print_bytes(new_smallest) {
                        Err(e) => {
                            // Ignore disassembly errors, since this isn't critical for
                            // shrinking.
                            log::warn!("Error disassembling the shrunken Wasm into WAT: {}", e);
                        }
                        Ok(wat) => {
                            let wat_path = output.with_extension("wat");
                            log::info!("Writing WAT disassembly to {}", wat_path.display());
                            std::fs::write(&wat_path, wat).with_context(|| {
                                format!("Failed to write WAT disassembly to {}", wat_path.display())
                            })?;
                        }
                    }

                    Ok(())
                }
            })))
            .run(input, predicate)?;

        let wat = wasmprinter::print_bytes(&shrunken.output)
            .unwrap_or_else(|e| format!("<error disassembling WAT: {}>", e));

        println!(
            "\n\
         {} :: {} bytes ({:.02}% smaller)\n\
         ================================================================================\n\
         {}\n\
         ================================================================================",
            output.display(),
            shrunken.output.len(),
            100.0 - (shrunken.output.len() as f64 / initial_size as f64 * 100.0),
            wat.trim(),
        );

        Ok(())
    }
}

struct OutputIsInteresting(std::process::Output);

impl std::fmt::Display for OutputIsInteresting {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "predicate script exit status: {}\n\
             \n\
             === predicate script stdout ===\n\
             {}\n\
             ===============================\n\
             \n\
             === predicate script stderr ===\n\
             {}\n\
             ===============================",
            self.0.status,
            String::from_utf8_lossy(&self.0.stdout),
            String::from_utf8_lossy(&self.0.stderr),
        )
    }
}

impl IsInteresting for OutputIsInteresting {
    fn is_interesting(&self) -> bool {
        // `code` is `None` when a signal killed the child process on
        // unix. Treat that as a non-zero exit code.
        let code = self.0.status.code().unwrap_or(1);
        code == 0
    }
}

fn make_predicate<'a>(
    predicate_script: &'a Path,
) -> impl FnMut(&[u8]) -> Result<OutputIsInteresting> + 'a {
    move |wasm| {
        let tmp = NamedTempFile::new().context("Failed to create a temporary file.")?;
        std::fs::write(tmp.path(), wasm).with_context(|| {
            format!(
                "Failed to write to temporary file: {}",
                tmp.path().display()
            )
        })?;

        let output = std::process::Command::new(&predicate_script)
            .arg(tmp.path())
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .output()
            .with_context(|| {
                format!(
                    "Failed to run predicate script '{}'",
                    predicate_script.display()
                )
            })?;

        log::trace!("predicate script exit status: {}", output.status);
        log::trace!(
            "predicate script stdout:\n{}",
            String::from_utf8_lossy(&output.stdout)
        );
        log::trace!(
            "predicate script stderr:\n{}",
            String::from_utf8_lossy(&output.stderr)
        );

        Ok(OutputIsInteresting(output))
    }
}
