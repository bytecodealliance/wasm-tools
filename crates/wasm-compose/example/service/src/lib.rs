cargo_component_bindings::generate!();

use bindings::{
    example::service::{
        logging::{get_logger as host_get_logger, Logger as HostLogger},
        types::{Request, Response},
    },
    exports::example::service::{
        handler::{Error, Guest},
        logging::{self, GuestLogger},
    },
};
use cargo_component_bindings::rt::Resource;
use std::str;

struct Component;

impl Guest for Component {
    fn execute(req: Request) -> Result<Response, Error> {
        let headers = req.headers();
        // The content should be plain text
        let content_type = headers
            .iter()
            .find(|(k, _)| k == b"content-type")
            .map(|(_, v)| v)
            .ok_or(Error::BadRequest)?;

        if content_type != b"text/plain" {
            return Err(Error::BadRequest);
        }

        // We assume the body is UTF-8 encoded
        let body = req.body();
        let body = str::from_utf8(&body).map_err(|_| Error::BadRequest)?;

        // Echo the body back in the response
        Ok(Response::new(
            &vec![(b"content-type".to_vec(), b"text/plain".to_vec())],
            &format!("The request body was: {body}").into_bytes(),
        ))
    }
}

impl logging::Guest for Component {
    fn get_logger() -> Resource<Logger> {
        Resource::new(Logger(host_get_logger()))
    }
}

pub struct Logger(HostLogger);

impl GuestLogger for Logger {
    fn log(&self, message: String) {
        self.0.log(&format!("service logger: {message}"))
    }
}
