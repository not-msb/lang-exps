use logos::Logos;

#[derive(Logos, Debug)]
#[logos(skip r"\s+")]
pub enum PreProcess {
    #[regex(r"\/\/.*")]
    Comment,
}

impl PreProcess {
    pub fn strip(input: &str) -> String {
        let mut output = input.to_owned();

        let mut spans = vec![];
        let mut lex = PreProcess::lexer(&output);
        while let Some(token) = lex.next() {
            if token.is_ok() { spans.push(lex.span()) }
        }

        let mut removed: usize = 0;
        for span in spans {
            let range = span.start-removed..span.end-removed;
            output.replace_range(range, "");
            removed += span.len();
        }

        output
    }
}
