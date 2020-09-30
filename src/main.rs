use clap::Clap;

#[derive(Clap)]
#[clap(
    version = "0.1.0",
    author = "duskmoon(hkp) <hkp18@mails.tsinghua.edu.cn>"
)]
struct Opts {
    /// minidecaf source code
    source_code: String,

    /// log lexer output
    #[clap(short, long)]
    lexer: bool,

    /// log parser output
    #[clap(short, long)]
    parser: bool,

    /// log ir output
    #[clap(short, long)]
    ir: bool,

    /// log file path (without .log)
    #[clap(long, default_value = "minidecaf_compile")]
    log_file: String,
}

fn main() -> std::io::Result<()> {
    let opts: Opts = Opts::parse();
    let input = std::fs::read_to_string(opts.source_code)?;
    minidecaf::run(
        &input,
        &mut std::io::stdout(),
        minidecaf::LogOpts {
            log_file: opts.log_file,
            lexer: opts.lexer,
            parser: opts.parser,
            ir: opts.ir,
        },
    )
}
