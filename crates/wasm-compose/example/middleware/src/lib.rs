cargo_component_bindings::generate!();

use bindings::{
    example::service::{
        handler as downstream, logging,
        types::{Error, Request, Response},
    },
    exports::example::service::handler,
};
use flate2::{write::GzEncoder, Compression};
use std::io::Write;

struct Component;

impl handler::Guest for Component {
    fn execute(req: Request) -> Result<Response, Error> {
        logging::get_logger().log("entering middleware execute function");

        // Send the request to the downstream service
        let response = downstream::execute(Request::new(&req.headers(), &req.body()))?;

        let mut headers = response.headers();

        // If the response is already encoded, leave it alone
        if headers.iter().any(|(k, _)| k == b"content-encoding") {
            return Ok(response);
        }

        // Set the `content-encoding` header to `gzip`
        headers.push((b"content-encoding".to_vec(), b"gzip".to_vec()));

        // Compress the response body
        let mut encoder = GzEncoder::new(Vec::new(), Compression::default());
        encoder
            .write_all(&response.body())
            .map_err(|_| Error::BadRequest)?;

        Ok(Response::new(
            &headers,
            &encoder.finish().map_err(|_| Error::BadRequest)?,
        ))
    }
}
